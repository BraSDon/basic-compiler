use crate::ast::*;
use crate::emitter::Emitter;
use crate::lexer::{Keyword, Lexer, Operator, Token, TokenKind};
use std::{
    collections::HashSet,
    fmt::{self, Display, Formatter},
};

type ParserResult = Result<(), ParserError>;

pub struct Parser<'a> {
    lexer: Lexer,
    emitter: &'a mut Emitter,
    curr_token: Token,
    prev_token: Token,
    peek_token: Token,
    symbols: HashSet<String>,
    labels_declared: HashSet<String>,
    labels_gotoed: HashSet<String>,
}

impl<'a> Parser<'a> {
    pub fn new(mut lexer: Lexer, emitter: &'a mut Emitter) -> Parser {
        assert!(
            lexer.validate().is_ok(),
            "Lexing failed. Ensure that you validate the lexer before creating a parser."
        );
        let curr_token = lexer.get_token().unwrap();
        let peek_token = lexer.get_token().unwrap();
        Parser {
            lexer,
            emitter,
            curr_token: curr_token.clone(),
            prev_token: curr_token.clone(),
            peek_token,
            symbols: HashSet::new(),
            labels_declared: HashSet::new(),
            labels_gotoed: HashSet::new(),
        }
    }

    /// program ::= { statement }
    pub fn program(&mut self) -> Result<Program, ParserError> {
        let mut statements = Vec::new();

        self.emitter.header_line("#include <stdio.h>");
        self.emitter.header_line("int main(void){");

        while self.check_curr(TokenKind::Newline) {
            self.next();
        }
        while !self.check_curr(TokenKind::EOF) {
            let s = self.statement()?;
            statements.push(s);
        }

        self.emitter.emit_line("return 0;");
        self.emitter.emit_line("}");

        for label in &self.labels_gotoed {
            if !self.labels_declared.contains(label) {
                return Err(ParserError::UndeclaredLabel(label.clone()));
            }
        }

        Ok(Program(statements))
    }

    /// statement ::= print nl
    ///     | if nl
    ///     | while nl
    ///     | label nl
    ///     | goto nl
    ///     | input nl
    ///     | let nl
    fn statement(&mut self) -> Result<Statement, ParserError> {
        let curr_kind = self.curr_token.kind;
        let stmt = match curr_kind {
            TokenKind::Keyword(Keyword::Print) => self.print_statement()?,
            TokenKind::Keyword(Keyword::If) => self.if_statement()?,
            TokenKind::Keyword(Keyword::While) => self.while_statement()?,
            TokenKind::Keyword(Keyword::Label) => self.label_statement()?,
            TokenKind::Keyword(Keyword::Goto) => self.goto_statement()?,
            TokenKind::Keyword(Keyword::Input) => self.input_statement()?,
            TokenKind::Keyword(Keyword::Let) => self.let_statement()?,
            _ => {
                return Err(self.unexpected_token_error(vec![
                    TokenKind::Keyword(Keyword::Print),
                    TokenKind::Keyword(Keyword::If),
                    TokenKind::Keyword(Keyword::While),
                    TokenKind::Keyword(Keyword::Label),
                    TokenKind::Keyword(Keyword::Goto),
                    TokenKind::Keyword(Keyword::Input),
                    TokenKind::Keyword(Keyword::Let),
                ]))
            }
        };
        self.nl()?;
        Ok(stmt)
    }

    /// print ::= "PRINT" (expression | string)
    fn print_statement(&mut self) -> Result<Statement, ParserError> {
        self.next();

        let inner = if self.check_curr(TokenKind::String) {
            self.emitter
                .emit_line(&format!("printf(\"{}\\n\");", self.curr_token.value));
            let str = self.curr_token.value.clone();
            self.next();
            Print::String(str)
        } else {
            self.emitter.emit("printf(\"%.2f\\n\", (float)(");
            let expr = self.expression()?;
            self.emitter.emit_line("));");
            Print::Expression(expr)
        };

        Ok(Statement::Print(inner))
    }

    /// if :== "IF" comparison "THEN" nl {statement} "ENDIF"
    fn if_statement(&mut self) -> Result<Statement, ParserError> {
        self.next();
        self.emitter.emit("if (");
        let condition = self.comparison()?;
        self.expect_and_advance(TokenKind::Keyword(Keyword::Then))?;
        self.emitter.emit_line(") {");
        self.nl()?;
        let mut body = Vec::new();
        while !self.check_curr(TokenKind::Keyword(Keyword::Endif)) {
            let stmt = self.statement()?;
            body.push(stmt);
        }
        self.expect_and_advance(TokenKind::Keyword(Keyword::Endif))?;
        self.emitter.emit_line("}");
        Ok(Statement::If(If { condition, body }))
    }

    /// while ::= "WHILE" comparison "REPEAT" nl {statement} "ENDWHILE"
    fn while_statement(&mut self) -> Result<Statement, ParserError> {
        self.next();
        self.emitter.emit("while (");
        let condition = self.comparison()?;
        self.expect_and_advance(TokenKind::Keyword(Keyword::Repeat))?;
        self.emitter.emit_line(") {");
        self.nl()?;
        let mut body = Vec::new();
        while !self.check_curr(TokenKind::Keyword(Keyword::Endwhile)) {
            let stmt = self.statement()?;
            body.push(stmt);
        }
        self.expect_and_advance(TokenKind::Keyword(Keyword::Endwhile))?;
        self.emitter.emit_line("}");
        Ok(Statement::While(While { condition, body }))
    }

    /// label ::= "LABEL" identifier
    fn label_statement(&mut self) -> Result<Statement, ParserError> {
        self.next();
        self.emitter
            .emit_line(&format!("{}:", self.curr_token.value));
        let symbol = self.curr_token.value.clone();
        if self.labels_declared.contains(&symbol) {
            return Err(ParserError::AmbiguousLabel(symbol));
        }
        self.labels_declared.insert(symbol.clone());
        self.expect_and_advance(TokenKind::Identifier)?;
        Ok(Statement::Label(Identifier(symbol)))
    }

    /// goto ::= "GOTO" identifier
    fn goto_statement(&mut self) -> Result<Statement, ParserError> {
        self.next();
        self.emitter
            .emit_line(&format!("goto {};", self.curr_token.value));
        let symbol = self.curr_token.value.clone();
        self.labels_gotoed.insert(symbol.clone());
        self.expect_and_advance(TokenKind::Identifier)?;
        Ok(Statement::Goto(Identifier(symbol)))
    }

    /// input ::= "INPUT" identifier
    fn input_statement(&mut self) -> Result<Statement, ParserError> {
        self.next();
        let symbol = self.curr_token.value.clone();
        if !self.symbols.contains(&self.curr_token.value) {
            self.symbols.insert(self.curr_token.value.clone());
            self.emitter
                .header_line(&format!("float {};", self.curr_token.value));
        }
        let formatted_string = format!("if(0 == scanf(\"%f\", &{})) {{", self.curr_token.value);
        self.emitter.emit_line(&formatted_string);

        self.emitter
            .emit_line(&format!("{} = 0;", self.curr_token.value));
        self.emitter.emit("scanf(\"%");
        self.emitter.emit_line("*s\");");
        self.emitter.emit_line("}");
        self.expect_and_advance(TokenKind::Identifier)?;
        Ok(Statement::Input(Identifier(symbol)))
    }

    /// let ::= "LET" identifier "=" expression
    fn let_statement(&mut self) -> Result<Statement, ParserError> {
        self.next();
        let ident = self.curr_token.value.clone();
        if !self.symbols.contains(&self.curr_token.value) {
            self.symbols.insert(self.curr_token.value.clone());
            self.emitter
                .header_line(&format!("float {};", self.curr_token.value));
        }
        self.emitter.emit(&format!("{} = ", self.curr_token.value));
        self.expect_and_advance(TokenKind::Identifier)?;
        self.expect_and_advance(TokenKind::Operator(Operator::Assign))?;
        let expr = self.expression()?;
        self.emitter.emit_line(";");
        Ok(Statement::Let(Let {
            identifier: Identifier(ident),
            expression: expr,
        }))
    }

    /// comparison ::= expression (("==" | "!=" | ">" | ">=" | "<" | "<=") expression)+
    fn comparison(&mut self) -> Result<Comparison, ParserError> {
        let left = self.expression()?;
        let mut right = Vec::new();

        if self.curr_token.kind.is_comparator() {
            let operator = self.curr_token.kind.try_into().unwrap();
            self.emitter.emit(&self.curr_token.value);
            self.next();
            let expr = self.expression()?;
            right.push(ComparisonListItem {
                operator,
                expression: expr,
            });
        } else {
            return Err(self.unexpected_token_error(vec![
                TokenKind::Operator(Operator::Equal),
                TokenKind::Operator(Operator::NotEqual),
                TokenKind::Operator(Operator::Greater),
                TokenKind::Operator(Operator::GreaterEqual),
                TokenKind::Operator(Operator::Less),
                TokenKind::Operator(Operator::LessEqual),
            ]));
        }

        while self.curr_token.kind.is_comparator() {
            let operator = self.curr_token.kind.try_into().unwrap();
            self.emitter.emit(&self.curr_token.value);
            self.next();
            let expr = self.expression()?;
            right.push(ComparisonListItem {
                operator,
                expression: expr,
            });
        }

        Ok(Comparison { left, right })
    }

    /// nl ::= '\n'+
    fn nl(&mut self) -> ParserResult {
        self.expect_and_advance(TokenKind::Newline)?;
        while self.check_curr(TokenKind::Newline) {
            self.next();
        }
        Ok(())
    }

    /// expression ::= term {( "-" | "+" ) term}
    fn expression(&mut self) -> Result<Expression, ParserError> {
        let left = self.term()?;
        let mut right = Vec::new();
        while let TokenKind::Operator(Operator::Plus | Operator::Minus) = self.curr_token.kind {
            let operator = self.curr_token.kind.try_into().unwrap();
            self.emitter.emit(&self.curr_token.value);
            self.next();
            let term = self.term()?;
            right.push(ExpressionListItem { operator, term });
        }
        let right = right.is_empty().then_some(right);
        Ok(Expression { left, right })
    }

    /// term ::= unary {( "/" | "*" ) unary}
    fn term(&mut self) -> Result<Term, ParserError> {
        let left = self.unary()?;
        let mut right = Vec::new();

        while let TokenKind::Operator(Operator::Multiply | Operator::Divide) = self.curr_token.kind
        {
            let operator = self.curr_token.kind.try_into().unwrap();
            self.emitter.emit(&self.curr_token.value);
            self.next();
            let unary = self.unary()?;
            right.push(TermListItem { operator, unary });
        }
        let right = right.is_empty().then_some(right);
        Ok(Term { left, right })
    }

    /// unary ::= ["+" | "-"] primary
    fn unary(&mut self) -> Result<Unary, ParserError> {
        let operator =
            if let TokenKind::Operator(Operator::Plus | Operator::Minus) = self.curr_token.kind {
                self.emitter.emit(&self.curr_token.value);
                self.next();
                Some(self.prev_token.kind)
            } else {
                None
            }
            .map(|kind| kind.try_into().unwrap());
        let primary = self.primary()?;
        Ok(Unary { operator, primary })
    }

    /// primary ::= number | identifier
    fn primary(&mut self) -> Result<Primary, ParserError> {
        self.emitter.emit(&self.curr_token.value);
        Ok(match self.curr_token.kind {
            TokenKind::Number => {
                self.next();
                Primary::Number(self.prev_token.value.parse().unwrap())
            }
            TokenKind::Identifier => {
                if !self.symbols.contains(&self.curr_token.value) {
                    return Err(ParserError::UndeclaredSymbol(self.curr_token.value.clone()));
                }
                self.next();
                Primary::Identifier(Identifier(self.prev_token.value.clone()))
            }
            _ => {
                return Err(
                    self.unexpected_token_error(vec![TokenKind::Number, TokenKind::Identifier])
                )
            }
        })
    }

    fn check_curr(&self, kind: TokenKind) -> bool {
        self.curr_token.kind == kind
    }

    fn next(&mut self) {
        self.prev_token = self.curr_token.clone();
        self.curr_token = self.peek_token.clone();
        self.peek_token = self.lexer.get_token().unwrap();
    }

    /// Check if the current token is of the expected kind.
    /// If it is, consume it and return Ok(()).
    /// Otherwise, return an error.
    fn expect_and_advance(&mut self, kind: TokenKind) -> Result<(), ParserError> {
        if !self.check_curr(kind) {
            return Err(self.unexpected_token_error(vec![kind]));
        }
        self.next();
        Ok(())
    }

    fn unexpected_token_error(&self, expected: Vec<TokenKind>) -> ParserError {
        ParserError::UnexpectedToken {
            expected,
            found: self.curr_token.kind.into(),
        }
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
                write!(
                    f,
                    "Undeclared symbol: {}. Ensure the symbol is declared before use.",
                    symbol
                )
            }
            ParserError::AmbiguousLabel(symbol) => {
                write!(
                    f,
                    "Label {} already declared. Labels must be unique.",
                    symbol
                )
            }
            ParserError::UndeclaredLabel(symbol) => {
                write!(
                    f,
                    "Undeclared label: {}. Ensure the label is declared before use.",
                    symbol
                )
            }
        }
    }
}
