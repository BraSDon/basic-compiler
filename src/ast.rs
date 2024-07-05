// TODO: Add Span later
pub struct Program(pub Vec<Statement>);

pub enum Statement {
    Print(Print),
    If(If),
    While(While),
    Label(Identifier),
    Goto(Identifier),
    Let(Let),
    Input(Identifier),
}

pub struct If {
    pub condition: Comparison,
    pub body: Vec<Statement>,
}

pub struct While {
    pub condition: Comparison,
    pub body: Vec<Statement>,
}

pub struct Let {
    pub identifier: Identifier,
    pub expression: Expression,
}

pub struct Identifier(pub String);

// Print either expression OR string
pub enum Print {
    Expression(Expression),
    String(String),
}

pub struct Comparison {
    pub left: Expression,
    pub right: Vec<ComparisonListItem>,
}

pub struct ComparisonListItem {
    pub operator: ComparisonOperator,
    pub expression: Expression,
}

pub struct Expression {
    pub left: Term,
    pub right: Option<Vec<ExpressionListItem>>,
}

pub struct ExpressionListItem {
    pub operator: AdditiveOperator,
    pub term: Term,
}

pub struct Term {
    pub left: Unary,
    pub right: Option<Vec<TermListItem>>,
}

pub struct TermListItem {
    pub operator: MultiplicativeOperator,
    pub factor: Unary,
}

pub struct Unary {
    pub operator: Option<AdditiveOperator>,
    pub primary: Primary,
}

pub enum Primary {
    Identifier(Identifier),
    Number(f32),
}

pub enum Operator {
    Additive(AdditiveOperator),
    Multiplicative(MultiplicativeOperator),
    Comparison(ComparisonOperator),
}

pub enum AdditiveOperator {
    Plus,
    Minus,
}

pub enum MultiplicativeOperator {
    Multiply,
    Divide,
}

pub enum ComparisonOperator {
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Equal,
    NotEqual,
}
