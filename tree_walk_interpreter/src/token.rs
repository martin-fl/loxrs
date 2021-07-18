#[derive(Debug, PartialEq, Clone)]
pub(crate) enum TokenType {
    // Length-1 tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // Length-1/2 tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    // Literals
    Identifier,
    String(String),
    Number(f64),

    // Keywords
    And,
    Or,
    Class,
    Super,
    This,
    If,
    Else,
    For,
    While,
    True,
    False,
    Fun,
    Return,
    Var,
    Print,
    Nil,

    EOF,
}

impl TokenType {
    pub(crate) fn try_to_keyword(lexeme: &str) -> Option<TokenType> {
        use TokenType::*;
        match lexeme {
            "and" => Some(And),
            "or" => Some(Or),
            "class" => Some(Class),
            "super" => Some(Super),
            "this" => Some(This),
            "if" => Some(If),
            "else" => Some(Else),
            "for" => Some(For),
            "while" => Some(While),
            "true" => Some(True),
            "false" => Some(False),
            "fun" => Some(Fun),
            "return" => Some(Return),
            "var" => Some(Var),
            "print" => Some(Print),
            "nil" => Some(Nil),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Token<'tok> {
    pub ty: TokenType,
    pub lexeme: &'tok str,
    pub line: usize,
}

impl<'tok> Token<'tok> {
    pub fn new(ty: TokenType, lexeme: &'tok str, line: usize) -> Self {
        Self { ty, lexeme, line }
    }
}
