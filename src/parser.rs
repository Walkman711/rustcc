use crate::{
    lexer::{Keywords, Lexer, Token},
    parser_enums::{AdditiveOp, EqualityOp, MultiplicativeOp, RelationOp, UnaryOp},
    utils::{ParseError, RustCcError, RustCcResult},
};

#[derive(Clone, Debug)]
pub enum Program {
    Func(Function),
}

#[derive(Clone, Debug)]
pub enum Function {
    Fun(String, Statement),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Statement {
    Return(Expression),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Factor {
    Const(u64),
    Unary(UnaryOp, Box<Factor>),
    ParenExp(Box<Expression>),
}

pub type Expression = (LogicalAndExpression, Vec<LogicalAndExpression>);
pub type LogicalAndExpression = (EqualityExpression, Vec<EqualityExpression>);
pub type EqualityExpression = (
    RelationalExpression,
    Vec<(EqualityOp, RelationalExpression)>,
);
pub type RelationalExpression = (AdditiveExpression, Vec<(RelationOp, AdditiveExpression)>);
pub type AdditiveExpression = (Term, Vec<(AdditiveOp, Term)>);
pub type Term = (Factor, Vec<(MultiplicativeOp, Factor)>);

/// <program> ::= <function>
/// <function> ::= "int" <id> "(" ")" "{" <statement> ""
/// <statement> ::= "return" <exp> ";"
/// <exp> ::= <logical-and-exp> { "||" <logical-and-exp> }
/// <logical-and-exp> ::= <equality-exp> { "&&" <equality-exp> }
/// <equality-exp> ::= <relational-exp> { ("!=" | "==") <relational-exp> }
/// <relational-exp> ::= <additive-exp> { ("<" | ">" | "<=" | ">=") <additive-exp> }
/// <additive-exp> ::= <term> { ("+" | "-") <term> }
/// <term> ::= <factor> { ("*" | "/") <factor> }
/// <factor> ::= "(" <exp> ")" | <unary_op> <factor> | <int_value>
/// <unop> ::= "!" | "~" | "-"

pub struct Parser {
    lexer: Lexer,
}

impl Parser {
    pub fn new(program: &str) -> RustCcResult<Self> {
        let lexer = Lexer::try_from(program)?;
        Ok(Self { lexer })
    }
}

impl Parser {
    pub fn parse(&mut self) -> RustCcResult<Program> {
        let function = self.parse_fn()?;
        Ok(Program::Func(function))
    }

    fn parse_fn(&mut self) -> RustCcResult<Function> {
        self.lexer.expect_next(&Token::Keyword(Keywords::Int))?;

        let tok = self
            .lexer
            .next_token()
            .ok_or(RustCcError::ParseError(ParseError::UnexpectedTokenEnd))?;

        let Token::Identifier(identifier) = tok else {
            return Err(RustCcError::ParseError(
                ParseError::ExpectedToken(
                    Token::Identifier("any function name".to_string()), tok)));
        };

        self.lexer.expect_next(&Token::OpenParen)?;
        self.lexer.expect_next(&Token::CloseParen)?;
        self.lexer.expect_next(&Token::OpenBrace)?;

        let statement = self.parse_statement()?;

        Ok(Function::Fun(identifier, statement))
    }

    fn parse_statement(&mut self) -> RustCcResult<Statement> {
        self.lexer.expect_next(&Token::Keyword(Keywords::Return))?;

        let exp = self.parse_exp()?;

        self.lexer.expect_next(&Token::Semicolon)?;
        self.lexer.expect_next(&Token::CloseBrace)?;

        Ok(Statement::Return(exp))
    }

    fn parse_exp(&mut self) -> RustCcResult<Expression> {
        let first_logical_and_exp = self.parse_logical_and_exp()?;

        let mut trailing_logical_and_exps = vec![];

        while let Some(tok) = self.lexer.next_token() {
            match tok {
                Token::Or => {}
                _ => {
                    self.lexer.back();
                    break;
                }
            }
            let logical_and_exp = self.parse_logical_and_exp()?;
            trailing_logical_and_exps.push(logical_and_exp);
        }

        Ok((first_logical_and_exp, trailing_logical_and_exps))
    }

    fn parse_logical_and_exp(&mut self) -> RustCcResult<LogicalAndExpression> {
        let first_equality_exp = self.parse_equality_exp()?;

        let mut trailing_equality_exps = vec![];

        while let Some(tok) = self.lexer.next_token() {
            match tok {
                Token::And => {}
                _ => {
                    self.lexer.back();
                    break;
                }
            }
            let equality_exp = self.parse_equality_exp()?;
            trailing_equality_exps.push(equality_exp);
        }

        Ok((first_equality_exp, trailing_equality_exps))
    }

    fn parse_equality_exp(&mut self) -> RustCcResult<EqualityExpression> {
        let first_relational_exp = self.parse_relational_exp()?;

        let mut trailing_relational_exps = vec![];

        while let Some(tok) = self.lexer.next_token() {
            let op = match tok {
                Token::Equals => EqualityOp::Equals,
                Token::NotEquals => EqualityOp::NotEquals,
                _ => {
                    self.lexer.back();
                    break;
                }
            };
            let relational_exp = self.parse_relational_exp()?;
            trailing_relational_exps.push((op, relational_exp));
        }

        Ok((first_relational_exp, trailing_relational_exps))
    }

    fn parse_relational_exp(&mut self) -> RustCcResult<RelationalExpression> {
        let first_additive_exp = self.parse_additive_exp()?;

        let mut trailing_additive_exps = vec![];

        while let Some(tok) = self.lexer.next_token() {
            let op = match tok {
                Token::LessThan => RelationOp::LessThan,
                Token::LessThanEquals => RelationOp::LessThanEquals,
                Token::GreaterThan => RelationOp::GreaterThan,
                Token::GreaterThanEquals => RelationOp::GreaterThanEquals,
                _ => {
                    self.lexer.back();
                    break;
                }
            };
            let additive_exp = self.parse_additive_exp()?;
            trailing_additive_exps.push((op, additive_exp));
        }

        Ok((first_additive_exp, trailing_additive_exps))
    }

    fn parse_additive_exp(&mut self) -> RustCcResult<AdditiveExpression> {
        let first_term = self.parse_term()?;

        let mut trailing_terms = vec![];

        while let Some(tok) = self.lexer.next_token() {
            let op = match tok {
                Token::Plus => AdditiveOp::Addition,
                Token::Minus => AdditiveOp::Subtraction,
                _ => {
                    self.lexer.back();
                    break;
                }
            };
            let term = self.parse_term()?;
            trailing_terms.push((op, term));
        }

        Ok((first_term, trailing_terms))
    }

    fn parse_term(&mut self) -> RustCcResult<Term> {
        let first_factor = self.parse_factor()?;
        let mut trailing_factors = vec![];
        while let Some(tok) = self.lexer.next_token() {
            let op = match tok {
                Token::Star => MultiplicativeOp::Multiply,
                Token::Slash => MultiplicativeOp::Divide,
                _ => {
                    // go back by one if next token isn't a trailing factor
                    self.lexer.back();
                    break;
                }
            };
            let factor = self.parse_factor()?;
            trailing_factors.push((op, factor));
        }
        Ok((first_factor, trailing_factors))
    }

    fn parse_factor(&mut self) -> RustCcResult<Factor> {
        let tok = self
            .lexer
            .next_token()
            .ok_or(RustCcError::ParseError(ParseError::UnexpectedTokenEnd))?;

        let factor = match tok {
            Token::OpenParen => {
                let f = Factor::ParenExp(Box::new(self.parse_exp()?));
                self.lexer.expect_next(&Token::CloseParen)?;
                f
            }
            Token::Integer(u) => Factor::Const(u),
            Token::Minus => Factor::Unary(UnaryOp::Negation, Box::new(self.parse_factor()?)),
            Token::ExclamationPoint => {
                Factor::Unary(UnaryOp::LogicalNegation, Box::new(self.parse_factor()?))
            }
            Token::Tilde => {
                Factor::Unary(UnaryOp::BitwiseComplement, Box::new(self.parse_factor()?))
            }
            _ => panic!("bad factor"),
        };

        Ok(factor)
    }
}
