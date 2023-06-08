use crate::{
    lexing::{
        lexer::Lexer,
        lexer_enums::{Keywords, Token},
    },
    parse_level,
    utils::{
        error::{ParseError, RustCcResult},
        types::{BasicType, IntegerType, ReturnType, VariableType},
    },
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
        let mut top_level_items = vec![];
        while self.lexer.peek().is_some() {
            let tli = self.parse_top_level_item()?;
            top_level_items.push(tli);
        }
        Ok(Program(top_level_items))
    }

    fn parse_top_level_item(&mut self) -> RustCcResult<TopLevelItem> {
        let top_level_type = self.parse_fn_ret_type()?;

        let tok = self.lexer.next_token_fallible()?;

        let Token::Identifier(identifier) = tok else {
            return Err(
                ParseError::ExpectedToken(
                    Token::Identifier("any function name".to_string()), tok).into());
        };

        let tok = self.lexer.next_token_fallible()?;

        match tok {
            Token::SingleEquals => {
                if let ReturnType::Void = top_level_type {
                    todo!("Error: tried to declare a global var with type Void");
                }
                if let Some(Token::Integer(i)) = self.lexer.next_token() {
                    self.lexer.expect_next(&Token::Semicolon)?;
                    Ok(TopLevelItem::Var(GlobalVar::Definition(
                        identifier, i as i64,
                    )))
                } else {
                    Err(ParseError::ExpectedToken(Token::Integer(100), tok).into())
                }
            }
            Token::Semicolon => {
                // DRY: pull this up?
                if let ReturnType::Void = top_level_type {
                    todo!("Error: tried to declare a global var with type Void");
                }
                Ok(TopLevelItem::Var(GlobalVar::Declaration(identifier)))
            }
            _ => {
                self.lexer.back();
                let function = self.parse_fn(identifier, top_level_type)?;
                Ok(TopLevelItem::Fun(function))
            }
        }
    }

    fn parse_fn_ret_type(&mut self) -> RustCcResult<ReturnType> {
        if self.lexer.advance_if_match(&Token::Keyword(Keywords::Void)) {
            Ok(ReturnType::Void)
        } else {
            let vt = self.parse_variable_type()?;
            Ok(ReturnType::NonVoid(vt))
        }
    }

    fn parse_variable_type(&mut self) -> RustCcResult<VariableType> {
        let bt = self.parse_basic_type()?;
        Ok(VariableType::Basic(bt))
    }

    fn parse_basic_type(&mut self) -> RustCcResult<BasicType> {
        let ret = match self.lexer.peek() {
            Some(Token::Keyword(Keywords::Int)) => Ok(BasicType::Int(IntegerType::Int)),
            Some(Token::Keyword(Keywords::Char)) => Ok(BasicType::Int(IntegerType::Char)),
            _ => Err(ParseError::CouldNotParseBasicType.into()),
        };

        if ret.is_ok() {
            let _ = self.lexer.next_token();
        }

        ret
    }

    fn parse_fn(&mut self, identifier: String, ret_type: ReturnType) -> RustCcResult<Function> {
        let mut params = vec![];
        self.lexer.expect_next(&Token::OpenParen)?;

        // Parse params. We should either have nothing, void and nothing else, or a list of args.
        if !self.lexer.advance_if_match(&Token::Keyword(Keywords::Void)) {
            while let Ok(var_type) = self.parse_variable_type() {
                let Some(Token::Identifier(id)) = self.lexer.next_token() else {
                    return Err(ParseError::MalformedDeclaration.into());
                };

                params.push(Param { var_type, id });
                self.lexer.advance_if_match(&Token::Comma);
            }
        }
        self.lexer.expect_next(&Token::CloseParen)?;

        if self.lexer.advance_if_match(&Token::Semicolon) {
            Ok(Function::Declaration(identifier, ret_type, params))
        } else {
            let mut block_items = self.parse_block_items()?;

            let need_to_add_return = !block_items.iter().any(|bi| bi.has_return());
            if need_to_add_return {
                block_items.push(BlockItem::Stmt(Statement::Return(None)))
            }

            Ok(Function::Definition(
                identifier,
                ret_type,
                params,
                block_items,
            ))
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
                if let Ok(var_type) = self.parse_variable_type() {
                    let Some(Token::Identifier(id)) = self.lexer.peek() else {
                        return Err(ParseError::MalformedDeclaration.into());
                    };
                    // XXX: can we just declare and not init a var in the init exp?
                    // I think that the Expression type doesn't do empty exps for now.
                    decl = Some((id, var_type, Some(self.parse_l15_exp()?)));
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

            let block_item = if let Ok(var_type) = self.parse_variable_type() {
                let Some(Token::Identifier(id)) = self.lexer.next_token() else {
                    return Err(ParseError::MalformedDeclaration.into());
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

                BlockItem::Declare((id, var_type, exp))
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
        let tok = self.lexer.next_token_fallible()?;

        match tok {
            Token::OpenParen => {
                let f = Level2Exp::ParenExp(Box::new(self.parse_l15_exp()?));
                self.lexer.expect_next(&Token::CloseParen)?;
                Ok(f)
            }
            Token::Integer(u) => Ok(Level2Exp::Const(u)),
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
                    Ok(Level2Exp::FunctionCall(id, params))
                } else {
                    Ok(Level2Exp::Var(id))
                }
            }
            Token::Plus => Ok(Level2Exp::Unary(
                Level2Op::UnaryPlus,
                Box::new(self.parse_l2_exp()?),
            )),
            Token::Minus => Ok(Level2Exp::Unary(
                Level2Op::UnaryMinus,
                Box::new(self.parse_l2_exp()?),
            )),
            Token::ExclamationPoint => Ok(Level2Exp::Unary(
                Level2Op::LogicalNot,
                Box::new(self.parse_l2_exp()?),
            )),
            Token::Tilde => Ok(Level2Exp::Unary(
                Level2Op::BitwiseNot,
                Box::new(self.parse_l2_exp()?),
            )),
            Token::Keyword(Keywords::Sizeof) => Ok(Level2Exp::Unary(
                Level2Op::SizeOf,
                Box::new(self.parse_l2_exp()?),
            )),
            t => Err(ParseError::UnexpectedBottomLevelToken(t).into()),
        }
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
