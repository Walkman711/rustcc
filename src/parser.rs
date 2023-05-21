use crate::{
    lexer::Lexer,
    lexer_enums::{Keywords, Token},
    ops::*,
    parse_level,
    parser_types::*,
    utils::{ParseError, RustCcError, RustCcResult},
};

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

        let mut block_items = self.parse_block_items()?;

        let need_to_add_return = !block_items.iter().any(|bi| bi.has_return());
        if need_to_add_return {
            block_items.push(BlockItem::Stmt(Statement::Return(None)))
        }

        assert!(self.lexer.peek().is_none());

        Ok(Function::Fun(identifier, block_items))
    }

    fn parse_statement(&mut self) -> RustCcResult<Statement> {
        match self.lexer.next_token() {
            Some(Token::Keyword(Keywords::Return)) => {
                let exp = self.parse_l15_exp()?;

                self.lexer.expect_next(&Token::Semicolon)?;

                Ok(Statement::Return(Some(exp)))
            }
            Some(Token::Keyword(Keywords::If)) => {
                let exp = self.parse_l15_exp()?;

                let pred_stmt = self.parse_statement()?;

                if self.lexer.advance_if_match(&Token::Keyword(Keywords::Else)) {
                    let else_stmt = self.parse_statement()?;
                    let if_stmt =
                        Statement::If(exp, Box::new(pred_stmt), Some(Box::new(else_stmt)));
                    Ok(if_stmt)
                } else {
                    let if_stmt = Statement::If(exp, Box::new(pred_stmt), None);
                    Ok(if_stmt)
                }
            }
            Some(Token::OpenBrace) => {
                self.lexer.back();
                let compound = Statement::Compound(self.parse_block_items()?);
                Ok(compound)
            }
            _ => {
                self.lexer.back();

                let exp = Ok(Statement::Exp(self.parse_l15_exp()?));

                self.lexer.expect_next(&Token::Semicolon)?;

                exp
            }
        }
    }

    fn parse_block_items(&mut self) -> RustCcResult<Vec<BlockItem>> {
        self.lexer.expect_next(&Token::OpenBrace)?;
        let mut block_items = vec![];
        while let Some(tok) = self.lexer.peek() {
            if tok == Token::CloseBrace {
                break;
            }

            let block_item = if self.lexer.advance_if_match(&Token::Keyword(Keywords::Int)) {
                let Some(Token::Identifier(id)) = self.lexer.next_token() else {
                    panic!("assignment statment needs an identifier")
                };

                // Declaration vs initialization
                let exp = if self.lexer.advance_if_match(&Token::Semicolon) {
                    None
                } else {
                    // TODO: change for +=, -=, etc.
                    self.lexer.expect_next(&Token::SingleEquals)?;
                    let exp = self.parse_l15_exp()?;
                    self.lexer.expect_next(&Token::Semicolon)?;
                    Some(exp)
                };

                BlockItem::Declare((id, exp))
            } else {
                BlockItem::Stmt(self.parse_statement()?)
            };
            block_items.push(block_item);
        }

        self.lexer.expect_next(&Token::CloseBrace)?;
        Ok(block_items)
    }

    parse_level!(parse_l15_exp, Level15Exp, Level15Op, parse_l14_exp);
    parse_level!(parse_l12_exp, Level12Exp, Level12Op, parse_l11_exp);
    parse_level!(parse_l11_exp, Level11Exp, Level11Op, parse_l10_exp);
    parse_level!(parse_l10_exp, Level10Exp, Level10Op, parse_l9_exp);
    parse_level!(parse_l9_exp, Level9Exp, Level9Op, parse_l8_exp);
    parse_level!(parse_l8_exp, Level8Exp, Level8Op, parse_l7_exp);
    parse_level!(parse_l7_exp, Level7Exp, Level7Op, parse_l6_exp);
    parse_level!(parse_l6_exp, Level6Exp, Level6Op, parse_l5_exp);
    parse_level!(parse_l5_exp, Level5Exp, Level5Op, parse_l4_exp);
    parse_level!(parse_l4_exp, Level4Exp, Level4Op, parse_l3_exp);
    parse_level!(parse_l3_exp, Level3Exp, Level3Op, parse_l2_exp);

    fn parse_l14_exp(&mut self) -> RustCcResult<Level14Exp> {
        if let Some(Token::Identifier(var_name)) = self.lexer.peek() {
            let _ = self.lexer.next_token();
            if self.lexer.advance_if_match(&Token::SingleEquals) {
                let exp = self.parse_l15_exp()?;
                Ok(Level14Exp::SimpleAssignment(var_name, Box::new(exp)))
            } else {
                self.lexer.back();
                Ok(Level14Exp::NonAssignment(self.parse_l13_exp()?))
            }
        } else {
            let l13_exp = self.parse_l13_exp()?;
            Ok(Level14Exp::NonAssignment(l13_exp))
        }
    }

    fn parse_l13_exp(&mut self) -> RustCcResult<Level13Exp> {
        let first_l12_exp = self.parse_l12_exp()?;
        if self.lexer.advance_if_match(&Token::QuestionMark) {
            let pred_exp = self.parse_l15_exp()?;
            self.lexer.expect_next(&Token::Colon)?;
            // From the C standard
            let else_exp = self.parse_l13_exp()?;
            Ok(Level13Exp::Ternary(
                first_l12_exp,
                Box::new(pred_exp),
                Box::new(else_exp),
            ))
        } else {
            Ok(Level13Exp::NoTernary(first_l12_exp))
        }
    }

    fn parse_l2_exp(&mut self) -> RustCcResult<Level2Exp> {
        let tok = self
            .lexer
            .next_token()
            .ok_or(RustCcError::ParseError(ParseError::UnexpectedTokenEnd))?;

        let l2_exp = match tok {
            Token::OpenParen => {
                let f = Level2Exp::ParenExp(Box::new(self.parse_l15_exp()?));
                self.lexer.expect_next(&Token::CloseParen)?;
                f
            }
            Token::Integer(u) => Level2Exp::Const(u),
            Token::Identifier(id) => Level2Exp::Var(id),
            Token::Plus => Level2Exp::Unary(Level2Op::UnaryPlus, Box::new(self.parse_l2_exp()?)),
            Token::Minus => Level2Exp::Unary(Level2Op::UnaryMinus, Box::new(self.parse_l2_exp()?)),
            Token::ExclamationPoint => {
                Level2Exp::Unary(Level2Op::LogicalNot, Box::new(self.parse_l2_exp()?))
            }
            Token::Tilde => Level2Exp::Unary(Level2Op::BitwiseNot, Box::new(self.parse_l2_exp()?)),
            t => panic!("bad l2 token: {t:?}"),
        };

        Ok(l2_exp)
    }
}

#[macro_export]
macro_rules! parse_level {
    ($fn_name: ident, $exp_type: ident, $op_type: ty, $next_fn: ident) => {
        fn $fn_name(&mut self) -> RustCcResult<$exp_type> {
            let first_exp = self.$next_fn()?;
            let mut trailing_exps = vec![];
            while let Some(tok) = self.lexer.next_token() {
                if let Ok(op) = <$op_type>::try_from(tok) {
                    let trailing_exp = self.$next_fn()?;
                    trailing_exps.push((op, trailing_exp));
                } else {
                    self.lexer.back();
                    break;
                }
            }
            let ret = $exp_type((first_exp, trailing_exps));
            Ok(ret)
        }
    };
}
