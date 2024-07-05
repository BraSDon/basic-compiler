// TODO: Add Span later
#[derive(Debug)]
pub struct Program(pub Vec<Statement>);

#[derive(Debug)]
pub enum Statement {
    Print(Print),
    If(If),
    While(While),
    Label(Identifier),
    Goto(Identifier),
    Let(Let),
    Input(Identifier),
}

#[derive(Debug)]
pub struct If {
    pub condition: Comparison,
    pub body: Vec<Statement>,
}

#[derive(Debug)]
pub struct While {
    pub condition: Comparison,
    pub body: Vec<Statement>,
}

#[derive(Debug)]
pub struct Let {
    pub identifier: Identifier,
    pub expression: Expression,
}

#[derive(Debug)]
pub struct Identifier(pub String);

// Print either expression OR string
#[derive(Debug)]
pub enum Print {
    Expression(Expression),
    String(String),
}

#[derive(Debug)]
pub struct Comparison {
    pub left: Expression,
    pub right: Vec<ComparisonListItem>,
}

#[derive(Debug)]
pub struct ComparisonListItem {
    pub operator: ComparisonOperator,
    pub expression: Expression,
}

#[derive(Debug)]
pub struct Expression {
    pub left: Term,
    pub right: Option<Vec<ExpressionListItem>>,
}

#[derive(Debug)]
pub struct ExpressionListItem {
    pub operator: AdditiveOperator,
    pub term: Term,
}

#[derive(Debug)]
pub struct Term {
    pub left: Unary,
    pub right: Option<Vec<TermListItem>>,
}

#[derive(Debug)]
pub struct TermListItem {
    pub operator: MultiplicativeOperator,
    pub unary: Unary,
}

#[derive(Debug)]
pub struct Unary {
    pub operator: Option<AdditiveOperator>,
    pub primary: Primary,
}

#[derive(Debug)]
pub enum Primary {
    Identifier(Identifier),
    Number(f32),
}

#[derive(Debug)]
pub enum Operator {
    Additive(AdditiveOperator),
    Multiplicative(MultiplicativeOperator),
    Comparison(ComparisonOperator),
}

#[derive(Debug)]
pub enum AdditiveOperator {
    Plus,
    Minus,
}

impl Into<Operator> for AdditiveOperator {
    fn into(self) -> Operator {
        Operator::Additive(self)
    }
}

#[derive(Debug)]
pub enum MultiplicativeOperator {
    Multiply,
    Divide,
}

impl Into<Operator> for MultiplicativeOperator {
    fn into(self) -> Operator {
        Operator::Multiplicative(self)
    }
}

#[derive(Debug)]
pub enum ComparisonOperator {
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Equal,
    NotEqual,
}

impl Into<Operator> for ComparisonOperator {
    fn into(self) -> Operator {
        Operator::Comparison(self)
    }
}
