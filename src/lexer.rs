use crate::ast::{
    AdditiveOperator, ComparisonOperator, MultiplicativeOperator, Operator as AstOperator,
};
use std::fmt::{self, Display, Formatter};
use std::str::FromStr;

pub struct Lexer {
    source: String,
    curr_char: char,
    curr_pos: usize,
}

impl Lexer {
    pub fn new(mut source: String) -> Lexer {
        assert!(!source.is_empty());
        source.push_str("\n");
        let fst_char = source.chars().next().unwrap();

        Lexer {
            source,
            curr_char: fst_char,
            curr_pos: 0,
        }
    }

    fn next_char(&mut self) {
        self.curr_pos += 1;
        self.curr_char = self.source.chars().nth(self.curr_pos).unwrap_or('\0');
    }

    fn peek(&self) -> char {
        self.source.chars().nth(self.curr_pos + 1).unwrap_or('\0')
    }

    /// Skip all whitespace characters except newline
    fn skip_whitespace(&mut self) {
        while self.curr_char.is_whitespace() && self.curr_char != '\n' {
            self.next_char();
        }
    }

    fn skip_comment(&mut self) {
        if self.curr_char == '#' {
            while self.curr_char != '\n' {
                self.next_char();
            }
        }
    }

    pub fn get_token(&mut self) -> Result<Token, LexerError> {
        self.skip_whitespace();
        self.skip_comment();

        let c = &self.curr_char.to_string();
        let token = match self.curr_char {
            '+' | '-' | '*' | '/' | '\n' | '\0' => Token::from_str(c),
            '=' | '>' | '<' | '!' => self.match_eq_combination(self.curr_char),
            '"' => self.process_string(),
            c if c.is_alphabetic() => self.process_ident_or_keyword(),
            c if c.is_numeric() => self.process_number(),
            _ => Err(LexerError::UnexpectedChar(self.curr_char)),
        };
        self.next_char();
        token
    }

    pub fn get_all_tokens(&mut self) -> Result<Vec<Token>, LexerError> {
        let mut tokens = Vec::new();

        loop {
            match self.get_token() {
                Ok(token) => {
                    let exit = token.kind == TokenKind::EOF;
                    tokens.push(token);
                    if exit {
                        break;
                    }
                }
                Err(e) => return Err(e), // Return the error if get_token fails
            }
        }

        Ok(tokens)
    }

    /// Validate if lexing is successful.
    /// If not, return the first error encountered in lexing.
    /// If successful, reset the lexer to the beginning of the source.
    pub fn validate(&mut self) -> Result<(), LexerError> {
        let x = self.get_all_tokens()?;
        self.curr_char = self.source.chars().nth(0).unwrap();
        self.curr_pos = 0;
        Ok(())
    }

    fn match_eq_combination(&mut self, c: char) -> Result<Token, LexerError> {
        let peek = self.peek();
        if peek == '=' {
            self.next_char();
            Token::from_str(&format!("{}=", c))
        } else if c == '!' {
            Err(LexerError::UnexpectedChar(peek))
        } else {
            Token::from_str(&c.to_string())
        }
    }

    fn process_string(&mut self) -> Result<Token, LexerError> {
        self.next_char();
        let start = self.curr_pos;

        while self.curr_char != '"' {
            match self.curr_char {
                '\r' | '\n' | '\t' | '\\' | '%' => {
                    return Err(LexerError::UnexpectedChar(self.curr_char))
                }
                _ => {}
            }
            self.next_char();
        }

        // token_txt without quotes
        let token_txt = &self.source[start..self.curr_pos];
        Ok(Token {
            kind: TokenKind::String,
            value: token_txt.to_string(),
        })
    }

    fn process_ident_or_keyword(&mut self) -> Result<Token, LexerError> {
        let start = self.curr_pos;

        while self.peek().is_alphanumeric() {
            self.next_char();
        }

        let token_txt = &self.source[start..self.curr_pos + 1];
        // this function determines if keyword or identifier
        Token::from_str(token_txt)
    }

    fn process_number(&mut self) -> Result<Token, LexerError> {
        let start = self.curr_pos;

        while self.peek().is_numeric() {
            self.next_char();
        }

        if self.peek() == '.' {
            self.next_char();

            if !self.peek().is_numeric() {
                return Err(LexerError::UnexpectedChar(self.peek()));
            }

            while self.peek().is_numeric() {
                self.next_char();
            }
        }
        let token_txt = &self.source[start..self.curr_pos + 1];
        Ok(Token {
            kind: TokenKind::Number,
            value: token_txt.to_string(),
        })
    }
}

#[derive(Debug)]
pub enum LexerError {
    UnexpectedChar(char),
    UnexpectedEOF,
    KindParseError,
}

impl Display for LexerError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            LexerError::UnexpectedChar(c) => write!(f, "Unexpected character: {}", c),
            LexerError::UnexpectedEOF => write!(f, "Unexpected EOF"),
            LexerError::KindParseError => write!(f, "Failed to parse token kind"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub value: String,
}

impl FromStr for Token {
    type Err = LexerError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let kind = TokenKind::from_str(s)?;
        let value = match kind {
            TokenKind::EOF => "",
            _ => s,
        };
        Ok(Token {
            kind,
            value: value.to_string(),
        })
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum TokenKind {
    Identifier,
    Number,
    String,
    Newline,
    EOF,
    Keyword(Keyword),
    Operator(Operator),
}

impl TokenKind {
    pub fn is_comparator(&self) -> bool {
        match self {
            TokenKind::Operator(op) => op.is_comparator(),
            _ => false,
        }
    }
}

impl TryInto<AstOperator> for TokenKind {
    type Error = ();

    fn try_into(self) -> Result<AstOperator, Self::Error> {
        match self {
            TokenKind::Operator(op) => Ok(op.try_into()?),
            _ => Err(()),
        }
    }
}

impl TryInto<ComparisonOperator> for TokenKind {
    type Error = ();

    fn try_into(self) -> Result<ComparisonOperator, Self::Error> {
        match self {
            TokenKind::Operator(op) => match op {
                Operator::Equal => Ok(ComparisonOperator::Equal),
                Operator::NotEqual => Ok(ComparisonOperator::NotEqual),
                Operator::Greater => Ok(ComparisonOperator::Greater),
                Operator::GreaterEqual => Ok(ComparisonOperator::GreaterEqual),
                Operator::Less => Ok(ComparisonOperator::Less),
                Operator::LessEqual => Ok(ComparisonOperator::LessEqual),
                _ => Err(()),
            },
            _ => Err(()),
        }
    }
}

impl TryInto<AdditiveOperator> for TokenKind {
    type Error = ();

    fn try_into(self) -> Result<AdditiveOperator, Self::Error> {
        match self {
            TokenKind::Operator(op) => match op {
                Operator::Plus => Ok(AdditiveOperator::Plus),
                Operator::Minus => Ok(AdditiveOperator::Minus),
                _ => Err(()),
            },
            _ => Err(()),
        }
    }
}

impl TryInto<MultiplicativeOperator> for TokenKind {
    type Error = ();

    fn try_into(self) -> Result<MultiplicativeOperator, Self::Error> {
        match self {
            TokenKind::Operator(op) => match op {
                Operator::Multiply => Ok(MultiplicativeOperator::Multiply),
                Operator::Divide => Ok(MultiplicativeOperator::Divide),
                _ => Err(()),
            },
            _ => Err(()),
        }
    }
}

impl FromStr for TokenKind {
    type Err = LexerError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Ok(keyword) = Keyword::from_str(s) {
            return Ok(TokenKind::Keyword(keyword));
        }

        if let Ok(operator) = Operator::from_str(s) {
            return Ok(TokenKind::Operator(operator));
        }

        match s {
            "\n" => Ok(TokenKind::Newline),
            "\0" => Ok(TokenKind::EOF),
            a if a.chars().all(char::is_alphabetic) => Ok(TokenKind::Identifier),
            _ => Err(LexerError::KindParseError),
        }
    }
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            TokenKind::Identifier => write!(f, "Identifier"),
            TokenKind::Number => write!(f, "Number"),
            TokenKind::String => write!(f, "String"),
            TokenKind::Newline => write!(f, "Newline"),
            TokenKind::EOF => write!(f, "EOF"),
            TokenKind::Keyword(k) => write!(f, "{}", k),
            TokenKind::Operator(o) => write!(f, "{}", o),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum Keyword {
    Label,
    Goto,
    Print,
    Input,
    Let,
    If,
    Then,
    Endif,
    While,
    Repeat,
    Endwhile,
}

impl FromStr for Keyword {
    type Err = LexerError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // all uppercase
        match s {
            "LABEL" => Ok(Keyword::Label),
            "GOTO" => Ok(Keyword::Goto),
            "PRINT" => Ok(Keyword::Print),
            "INPUT" => Ok(Keyword::Input),
            "LET" => Ok(Keyword::Let),
            "IF" => Ok(Keyword::If),
            "THEN" => Ok(Keyword::Then),
            "ENDIF" => Ok(Keyword::Endif),
            "WHILE" => Ok(Keyword::While),
            "REPEAT" => Ok(Keyword::Repeat),
            "ENDWHILE" => Ok(Keyword::Endwhile),
            _ => Err(LexerError::KindParseError),
        }
    }
}

impl Display for Keyword {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Keyword::Label => write!(f, "LABEL"),
            Keyword::Goto => write!(f, "GOTO"),
            Keyword::Print => write!(f, "PRINT"),
            Keyword::Input => write!(f, "INPUT"),
            Keyword::Let => write!(f, "LET"),
            Keyword::If => write!(f, "IF"),
            Keyword::Then => write!(f, "THEN"),
            Keyword::Endif => write!(f, "ENDIF"),
            Keyword::While => write!(f, "WHILE"),
            Keyword::Repeat => write!(f, "REPEAT"),
            Keyword::Endwhile => write!(f, "ENDWHILE"),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum Operator {
    Plus,
    Minus,
    Multiply,
    Divide,
    Assign,
    Equal,
    NotEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
}

impl Operator {
    pub fn is_comparator(&self) -> bool {
        let comparators = vec![
            Self::Equal,
            Self::NotEqual,
            Self::Greater,
            Self::GreaterEqual,
            Self::Less,
            Self::LessEqual,
        ];
        comparators.contains(self)
    }
}

impl TryInto<AstOperator> for Operator {
    type Error = ();

    fn try_into(self) -> Result<AstOperator, Self::Error> {
        match self {
            Operator::Plus => Ok(AdditiveOperator::Plus.into()),
            Operator::Minus => Ok(AdditiveOperator::Minus.into()),
            Operator::Multiply => Ok(MultiplicativeOperator::Multiply.into()),
            Operator::Divide => Ok(MultiplicativeOperator::Divide.into()),
            Operator::Equal => Ok(ComparisonOperator::Equal.into()),
            Operator::NotEqual => Ok(ComparisonOperator::NotEqual.into()),
            Operator::Greater => Ok(ComparisonOperator::Greater.into()),
            Operator::GreaterEqual => Ok(ComparisonOperator::GreaterEqual.into()),
            Operator::Less => Ok(ComparisonOperator::Less.into()),
            Operator::LessEqual => Ok(ComparisonOperator::LessEqual.into()),
            _ => Err(()),
        }
    }
}

impl FromStr for Operator {
    type Err = LexerError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "+" => Ok(Operator::Plus),
            "-" => Ok(Operator::Minus),
            "*" => Ok(Operator::Multiply),
            "/" => Ok(Operator::Divide),
            "=" => Ok(Operator::Assign),
            "==" => Ok(Operator::Equal),
            "!=" => Ok(Operator::NotEqual),
            ">" => Ok(Operator::Greater),
            ">=" => Ok(Operator::GreaterEqual),
            "<" => Ok(Operator::Less),
            "<=" => Ok(Operator::LessEqual),
            _ => Err(LexerError::KindParseError),
        }
    }
}

impl Display for Operator {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Operator::Plus => write!(f, "+"),
            Operator::Minus => write!(f, "-"),
            Operator::Multiply => write!(f, "*"),
            Operator::Divide => write!(f, "/"),
            Operator::Assign => write!(f, "="),
            Operator::Equal => write!(f, "=="),
            Operator::NotEqual => write!(f, "!="),
            Operator::Greater => write!(f, ">"),
            Operator::GreaterEqual => write!(f, ">="),
            Operator::Less => write!(f, "<"),
            Operator::LessEqual => write!(f, "<="),
        }
    }
}

// tests
#[cfg(test)]
mod tests {}
