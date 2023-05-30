use crate::{
    lexing::{
        lexer::Lexer,
        lexer_enums::{Keywords, Token},
    },
    parse_level,
    utils::error::{ParseError, RustCcError, RustCcResult},
};

use super::{ops::*, parser_types::*};

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
        let mut functions = vec![];
        while self.lexer.peek().is_some() {
            let function = self.parse_fn()?;
            functions.push(function);
        }
        Ok(Program(functions))
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

        let mut params = vec![];
        self.lexer.expect_next(&Token::OpenParen)?;
        while self.lexer.advance_if_match(&Token::Keyword(Keywords::Int)) {
            let Some(Token::Identifier(param)) = self.lexer.next_token() else {
                panic!("bad param");
            };
            params.push(param);
            self.lexer.advance_if_match(&Token::Comma);
        }
        self.lexer.expect_next(&Token::CloseParen)?;
        // println!("{params:?}");

        if self.lexer.advance_if_match(&Token::Semicolon) {
            Ok(Function::Declaration(identifier, params))
        } else {
            let mut block_items = self.parse_block_items()?;

            let need_to_add_return = !block_items.iter().any(|bi| bi.has_return());
            if need_to_add_return {
                block_items.push(BlockItem::Stmt(Statement::Return(None)))
            }

            Ok(Function::Definition(identifier, params, block_items))
        }
    }

    fn parse_statement(&mut self) -> RustCcResult<Statement> {
        match self.lexer.next_token() {
            // RETURN <exp> ";"
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
                    Ok(Statement::If(
                        exp,
                        Box::new(pred_stmt),
                        Some(Box::new(else_stmt)),
                    ))
                } else {
                    Ok(Statement::If(exp, Box::new(pred_stmt), None))
                }
            }
            Some(Token::OpenBrace) => {
                // Move lexer back so that parse_block_items() is the only thing enforcing
                // bracket rules. Otherwise the responsibility for bracket parsing is distributed
                // all over the place and it's a pain to debug if something goes wrong.
                self.lexer.back();
                Ok(Statement::Compound(self.parse_block_items()?))
            }
            Some(Token::Keyword(Keywords::For)) => {
                self.lexer.expect_next(&Token::OpenParen)?;

                // TODO: I really, really don't like this
                let mut decl = None;
                let mut init_exp = None;
                if self.lexer.advance_if_match(&Token::Keyword(Keywords::Int)) {
                    let Some(Token::Identifier(id)) = self.lexer.peek() else {
                        panic!("decl without identifier");
                    };
                    // XXX: can we just declare and not init a var in the init exp?
                    // I think that the Expression type doesn't do empty exps for now.
                    decl = Some((id, Some(self.parse_l15_exp()?)));
                    self.lexer.expect_next(&Token::Semicolon)?;
                } else if !self.lexer.advance_if_match(&Token::Semicolon) {
                    let exp = self.parse_l15_exp()?;
                    self.lexer.expect_next(&Token::Semicolon)?;
                    init_exp = Some(exp);
                }

                let controlling_exp = if self.lexer.advance_if_match(&Token::Semicolon) {
                    None
                } else {
                    let exp = self.parse_l15_exp()?;
                    self.lexer.expect_next(&Token::Semicolon)?;
                    Some(exp)
                };

                let post_exp = if self.lexer.advance_if_match(&Token::CloseParen) {
                    None
                } else {
                    let exp = self.parse_l15_exp()?;
                    self.lexer.expect_next(&Token::CloseParen)?;
                    Some(exp)
                };

                let body = self.parse_statement()?;
                match decl {
                    Some(d) => Ok(Statement::ForDecl(
                        d,
                        controlling_exp,
                        post_exp,
                        Box::new(body),
                    )),
                    None => Ok(Statement::For(
                        init_exp,
                        controlling_exp,
                        post_exp,
                        Box::new(body),
                    )),
                }
            }
            // WHILE "(" <exp> ")" <statement>
            Some(Token::Keyword(Keywords::While)) => {
                self.lexer.expect_next(&Token::OpenParen)?;
                let exp = self.parse_l15_exp()?;
                self.lexer.expect_next(&Token::CloseParen)?;
                let body = self.parse_statement()?;
                Ok(Statement::While(exp, Box::new(body)))
            }
            // BREAK ";"
            Some(Token::Keyword(Keywords::Break)) => {
                self.lexer.expect_next(&Token::Semicolon)?;
                Ok(Statement::Break)
            }
            // CONTINUE ";"
            Some(Token::Keyword(Keywords::Continue)) => {
                self.lexer.expect_next(&Token::Semicolon)?;
                Ok(Statement::Continue)
            }
            // DO <statement> WHILE "(" <exp> ")" ";"
            Some(Token::Keyword(Keywords::Do)) => {
                let body = self.parse_statement()?;
                self.lexer.expect_next(&Token::Keyword(Keywords::While))?;
                self.lexer.expect_next(&Token::OpenParen)?;
                let exp = self.parse_l15_exp()?;
                self.lexer.expect_next(&Token::CloseParen)?;
                self.lexer.expect_next(&Token::Semicolon)?;
                Ok(Statement::DoWhile(Box::new(body), exp))
            }
            Some(Token::Semicolon) => Ok(Statement::Exp(None)),
            _ => {
                self.lexer.back();

                let exp = Ok(Statement::Exp(Some(self.parse_l15_exp()?)));

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

    fn parse_l15_exp(&mut self) -> RustCcResult<Level15Exp> {
        let first_exp = self.parse_l14_exp()?;
        let trailing_exps = vec![];
        // TODO: actually handle commas. This is causing issues with parsing expressions for
        // function calls.
        // while let Some(tok) = self.lexer.next_token() {
        //     if let Ok(op) = Level15Op::try_from(tok) {
        //         let trailing_exp = self.parse_l14_exp()?;
        //         trailing_exps.push((op, trailing_exp));
        //     } else {
        //         self.lexer.back();
        //         break;
        //     }
        // }
        let ret = Level15Exp((first_exp, trailing_exps));
        Ok(ret)
    }

    // parse_level!(parse_l15_exp, Level15Exp, Level15Op, parse_l14_exp);
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
            Token::Identifier(id) => {
                if self.lexer.advance_if_match(&Token::OpenParen) {
                    let mut params = vec![];
                    while let Some(tok) = self.lexer.peek() {
                        if tok == Token::CloseParen {
                            break;
                        }

                        let exp = self.parse_l15_exp()?;
                        params.push(exp);
                        self.lexer.advance_if_match(&Token::Comma);
                    }
                    self.lexer.expect_next(&Token::CloseParen)?;
                    Level2Exp::FunctionCall(id, params)
                } else {
                    Level2Exp::Var(id)
                }
            }
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
