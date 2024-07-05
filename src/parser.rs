use crate::ast::*;
use crate::emitter::Emitter;
use crate::lexer::{Keyword, Lexer, Operator, Token, TokenKind};
use std::{
    collections::HashSet,
    fmt::{self, Display, Formatter},
};

type ParseResult = Result<(), ParserError>;

pub struct Parser<'a> {
    lexer: Lexer,
    emitter: &'a mut Emitter,
    curr_token: Token,
    peek_token: Token,
    symbols: HashSet<String>,
    labels_declared: HashSet<String>,
    labels_gotoed: HashSet<String>,
}

impl<'a> Parser<'a> {
    pub fn new(mut lexer: Lexer, emitter: &'a mut Emitter) -> Parser {
        // NOTE: We assume a caller already validated the lexer.
        // In this case, we can assume that get_token() will always return a valid token.
        assert!(lexer.validate().is_ok());
        let curr_token = lexer.get_token().unwrap();
        let peek_token = lexer.get_token().unwrap();
        Parser {
            lexer,
            emitter,
            curr_token,
            peek_token,
            symbols: HashSet::new(),
            labels_declared: HashSet::new(),
            labels_gotoed: HashSet::new(),
        }
    }

    pub fn program(&mut self) -> ParseResult {
        self.emitter.header_line("#include <stdio.h>");
        self.emitter.header_line("int main(void){");

        while self.check_curr(TokenKind::Newline) {
            self.next();
        }
        while !self.check_curr(TokenKind::EOF) {
            self.statement()?;
        }

        self.emitter.emit_line("return 0;");
        self.emitter.emit_line("}");

        for label in &self.labels_gotoed {
            if !self.labels_declared.contains(label) {
                return Err(ParserError::UndeclaredLabel(label.clone()));
            }
        }

        Ok(())
    }

    fn statement(&mut self) -> ParseResult {
        let curr_kind = self.curr_token.kind;
        match curr_kind {
            TokenKind::Keyword(Keyword::Print) => {
                self.next();

                if self.check_curr(TokenKind::String) {
                    self.emitter
                        .emit_line(&format!("printf(\"{}\\n\");", self.curr_token.value));
                    self.next();
                } else {
                    self.emitter.emit("printf(\"%.2f\\n\", (float)(");
                    self.expression()?;
                    self.emitter.emit_line("));");
                }
            }
            TokenKind::Keyword(Keyword::If) => {
                self.next();
                self.emitter.emit("if (");
                self.comparison()?;
                self.check_match(TokenKind::Keyword(Keyword::Then))?;
                self.emitter.emit_line(") {");
                self.nl()?;
                while !self.check_curr(TokenKind::Keyword(Keyword::Endif)) {
                    self.statement()?;
                }
                self.check_match(TokenKind::Keyword(Keyword::Endif))?;
                self.emitter.emit_line("}");
            }
            TokenKind::Keyword(Keyword::While) => {
                self.next();
                self.emitter.emit("while (");
                self.comparison()?;
                self.check_match(TokenKind::Keyword(Keyword::Repeat))?;
                self.emitter.emit_line(") {");
                self.nl()?;
                while !self.check_curr(TokenKind::Keyword(Keyword::Endwhile)) {
                    self.statement()?;
                }
                self.check_match(TokenKind::Keyword(Keyword::Endwhile))?;
                self.emitter.emit_line("}");
            }
            TokenKind::Keyword(Keyword::Label) => {
                self.next();
                self.emitter
                    .emit_line(&format!("{}:", self.curr_token.value));
                let symbol = self.curr_token.value.clone();
                if self.labels_declared.contains(&symbol) {
                    return Err(ParserError::AmbiguousLabel(symbol));
                }
                self.labels_declared.insert(symbol);
                self.check_match(TokenKind::Identifier)?;
            }
            TokenKind::Keyword(Keyword::Goto) => {
                self.next();
                self.emitter
                    .emit_line(&format!("goto {};", self.curr_token.value));
                let symbol = self.curr_token.value.clone();
                self.labels_gotoed.insert(symbol);
                self.check_match(TokenKind::Identifier)?;
            }
            TokenKind::Keyword(Keyword::Input) => {
                self.next();
                if !self.symbols.contains(&self.curr_token.value) {
                    self.symbols.insert(self.curr_token.value.clone());
                    self.emitter
                        .header_line(&format!("float {};", self.curr_token.value));
                }
                // self.emitter.emit_line("if(0 == scanf(\"%" + "f\", &" + self.curr_token.value + ")) {");
                let formatted_string =
                    format!("if(0 == scanf(\"%f\", &{})) {{", self.curr_token.value);
                self.emitter.emit_line(&formatted_string);

                self.emitter
                    .emit_line(&format!("{} = 0;", self.curr_token.value));
                self.emitter.emit("scanf(\"%");
                self.emitter.emit_line("*s\");");
                self.emitter.emit_line("}");
                self.check_match(TokenKind::Identifier)?;
            }
            TokenKind::Keyword(Keyword::Let) => {
                self.next();
                if !self.symbols.contains(&self.curr_token.value) {
                    self.symbols.insert(self.curr_token.value.clone());
                    self.emitter
                        .header_line(&format!("float {};", self.curr_token.value));
                }
                self.emitter.emit(&format!("{} = ", self.curr_token.value));
                self.check_match(TokenKind::Identifier)?;
                self.check_match(TokenKind::Operator(Operator::Assign))?;
                self.expression()?;
                self.emitter.emit_line(";");
            }
            _ => {
                return Err(ParserError::UnexpectedToken {
                    expected: vec![
                        TokenKind::Keyword(Keyword::Print),
                        TokenKind::Keyword(Keyword::If),
                        TokenKind::Keyword(Keyword::While),
                        TokenKind::Keyword(Keyword::Label),
                        TokenKind::Keyword(Keyword::Goto),
                        TokenKind::Keyword(Keyword::Input),
                        TokenKind::Keyword(Keyword::Let),
                    ],
                    found: Some(curr_kind),
                })
            }
        }
        self.nl()?;
        Ok(())
    }

    fn comparison(&mut self) -> ParseResult {
        self.expression()?;

        if self.curr_token.kind.is_comparator() {
            self.emitter.emit(&self.curr_token.value);
            self.next();
            self.expression()?;
        } else {
            return Err(ParserError::UnexpectedToken {
                expected: vec![
                    TokenKind::Operator(Operator::Equal),
                    TokenKind::Operator(Operator::NotEqual),
                    TokenKind::Operator(Operator::Greater),
                    TokenKind::Operator(Operator::GreaterEqual),
                    TokenKind::Operator(Operator::Less),
                    TokenKind::Operator(Operator::LessEqual),
                ],
                found: Some(self.curr_token.kind),
            });
        }

        while self.curr_token.kind.is_comparator() {
            self.emitter.emit(&self.curr_token.value);
            self.next();
            self.expression()?;
        }

        Ok(())
    }

    fn nl(&mut self) -> ParseResult {
        self.check_match(TokenKind::Newline)?;
        while self.check_curr(TokenKind::Newline) {
            self.next();
        }
        Ok(())
    }

    fn expression(&mut self) -> ParseResult {
        self.term()?;
        if let TokenKind::Operator(Operator::Plus | Operator::Minus) = self.curr_token.kind {
            self.emitter.emit(&self.curr_token.value);
            self.next();
            self.term()?;
        }
        Ok(())
    }

    fn term(&mut self) -> ParseResult {
        self.unary()?;

        while let TokenKind::Operator(Operator::Multiply | Operator::Divide) = self.curr_token.kind
        {
            self.emitter.emit(&self.curr_token.value);
            self.next();
            self.unary()?;
        }
        Ok(())
    }

    fn unary(&mut self) -> ParseResult {
        if let TokenKind::Operator(Operator::Plus | Operator::Minus) = self.curr_token.kind {
            self.emitter.emit(&self.curr_token.value);
            self.next();
        }
        self.primary()
    }

    fn primary(&mut self) -> ParseResult {
        self.emitter.emit(&self.curr_token.value);
        match self.curr_token.kind {
            TokenKind::Number => self.next(),
            TokenKind::Identifier => {
                if !self.symbols.contains(&self.curr_token.value) {
                    return Err(ParserError::UndeclaredSymbol(self.curr_token.value.clone()));
                }
                self.next();
            }
            _ => {
                return Err(ParserError::UnexpectedToken {
                    expected: vec![TokenKind::Number, TokenKind::Identifier],
                    found: Some(self.curr_token.kind),
                })
            }
        }
        Ok(())
    }

    fn check_curr(&self, kind: TokenKind) -> bool {
        self.curr_token.kind == kind
    }

    fn next(&mut self) {
        self.curr_token = self.peek_token.clone();
        self.peek_token = self.lexer.get_token().unwrap();
    }

    fn check_match(&mut self, kind: TokenKind) -> Result<(), ParserError> {
        if !self.check_curr(kind) {
            return Err(ParserError::UnexpectedToken {
                expected: vec![kind],
                found: self.curr_token.kind.into(),
            });
        }
        self.next();
        Ok(())
    }
}

#[derive(Debug)]
pub enum ParserError {
    UndeclaredSymbol(String),
    AmbiguousLabel(String),
    UndeclaredLabel(String),
    UnexpectedToken {
        expected: Vec<TokenKind>,
        found: Option<TokenKind>,
    },
}

impl Display for ParserError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            ParserError::UnexpectedToken { expected, found } => {
                write!(f, "Unexpected token found. Expected one of: ")?;
                for (i, kind) in expected.iter().enumerate() {
                    write!(f, "{:?}", kind)?;
                    if i < expected.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                if let Some(found) = found {
                    write!(f, ". Found: {:?}", found)?;
                }
                Ok(())
            }
            ParserError::UndeclaredSymbol(symbol) => {
                write!(f, "Symbol {} undeclared.", symbol)
            }
            ParserError::AmbiguousLabel(symbol) => {
                write!(f, "Label {} already declared.", symbol)
            }
            ParserError::UndeclaredLabel(symbol) => {
                write!(f, "Label {} undeclared.", symbol)
            }
        }
    }
}
