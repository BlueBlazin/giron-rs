use crate::syntax::Syntax;
use errors::errors::{Error, ErrorType};
use errors::result::Result;
use estree::estree::*;
use scanner::scanner::{LexGoal, Scanner};
use span::span::{Marker, Span};
use std::mem;
use token::token::{Token, TokenType};
use token::value::Value;

// **************************************************************
//   Dev stuff, Remove Me
// **************************************************************

const DEBUG: bool = false;

macro_rules! log {
    ( $name:expr ) => {{
        if DEBUG {
            eprintln!("{}", $name);
        }
    }};
}

// **************************************************************

pub enum ValueOrExpr {
    Value(Value),
    Expr(Node),
}

pub struct Context {
    pub assign_target_type: &'static str,
    pub strict: bool,
    pub in_function: bool,
    pub in_iteration: bool,
}

#[derive(Default)]
pub struct Params {
    pub has_in: bool,
    pub has_await: bool,
    pub has_yield: bool,
    pub has_tagged: bool,
    pub has_return: bool,
    pub has_default: bool,
}

impl Params {
    pub fn new() -> Self {
        Params {
            has_in: false,
            has_await: false,
            has_yield: false,
            has_tagged: false,
            has_return: false,
            has_default: false,
        }
    }
}

pub struct Parser<I> {
    pub scanner: Scanner<I>,
    pub ctx: Context,
    pub params: Params,
    pub has_line_terminator: bool,
    pub current: Token,
    pub lookahead: Option<Token>,
    params_stack: Vec<Params>,
    marker_stack: Vec<Marker>,
}

impl<I> Parser<I>
where
    I: Iterator<Item = char>,
{
    pub fn new(source: I) -> Self {
        let mut scanner = Scanner::new(source);
        let current = scanner.next(LexGoal::RegExp).unwrap();

        Parser {
            scanner,
            ctx: Context {
                assign_target_type: "invalid",
                strict: false,
                in_function: false,
                in_iteration: false,
            },
            params: Params::new(),
            has_line_terminator: false,
            current,
            lookahead: None,
            params_stack: vec![],
            marker_stack: vec![],
        }
    }

    // *******************************************************************
    //      Scripts & Modules
    // *******************************************************************

    pub fn parse_script(&mut self) -> Result<Node> {
        log!("parse_script");
        self.start_span();
        let mut body = self.parse_directive_prologues()?;
        body.extend(self.parse_until_eof(&mut Self::parse_stmt_list_item)?);
        Ok(Node::Program {
            body,
            source_type: "script",
            span: self.end_span(),
        })
    }

    pub fn parse_module(&mut self) -> Result<Node> {
        log!("parse_module");
        self.start_span();
        let mut body = self.with_params(
            Params {
                has_yield: false,
                has_await: false,
                has_return: false,
                ..self.params
            },
            &mut Self::parse_directive_prologues,
        )?;
        body.extend(self.parse_until_eof(&mut Self::parse_module_item)?);
        Ok(Node::Program {
            body,
            source_type: "script",
            span: self.end_span(),
        })
    }

    fn parse_directive_prologues(&mut self) -> Result<Vec<Node>> {
        let mut body = vec![];
        loop {
            match self.current.tokentype {
                TokenType::StringLiteral => {
                    self.start_span();
                    let expr = self.parse_expression()?;
                    match &expr {
                        Node::Literal { value, .. }
                            if value == &Value::Str(String::from("use strict")) =>
                        {
                            self.ctx.strict = true;
                        }
                        _ => (),
                    }
                    self.consume_semicolon(LexGoal::RegExp)?;
                    body.push(Node::ExpressionStatement {
                        expression: Box::from(expr),
                        span: self.end_span(),
                    });
                }
                _ => return Ok(body),
            }
        }
    }

    pub fn parse_module_item(&mut self) -> Result<Node> {
        log!("parse_module_item");
        // ModuleItem:
        //     ImportDeclaration
        //     ExportDeclaration
        //     StatementListItem[~Yield, ~Await, ~Return]
        match self.current.tokentype {
            TokenType::Keyword if self.current.matches_str("import") => self.parse_import_dclr(),
            TokenType::Keyword if self.current.matches_str("export") => self.parse_export_dclr(),
            _ => self.with_params(
                Params {
                    has_yield: false,
                    has_await: false,
                    has_return: false,
                    ..self.params
                },
                &mut Self::parse_stmt_list_item,
            ),
        }
    }

    pub fn parse_import_dclr(&mut self) -> Result<Node> {
        log!("parse_import_dclr");
        self.start_span();
        self.consume_kw("import", LexGoal::RegExp)?;
        match self.current.tokentype {
            TokenType::Punctuator if self.current.matches_punc("*") => {
                let specifiers = self.parse_namespace_import_specifiers()?;
                self.parse_import_from_dclr(specifiers)
            }
            TokenType::Punctuator if self.current.matches_punc("{") => {
                let specifiers = self.parse_named_import_specifiers()?;
                self.parse_import_from_dclr(specifiers)
            }
            TokenType::Identifier | TokenType::Keyword => {
                let local = self
                    .with_params(
                        Params {
                            has_yield: false,
                            has_await: false,
                            ..self.params
                        },
                        &mut Self::parse_binding_id,
                    )?
                    .to_id()?;
                let imported = local.clone();
                let mut specifiers =
                    vec![ImportDeclarationSpecifier::ImportSpecifier { local, imported }];
                match self.current.tokentype {
                    TokenType::Punctuator if self.current.matches_punc(",") => {
                        self.advance(LexGoal::RegExp)?;
                        match self.current.tokentype {
                            TokenType::Punctuator if self.current.matches_punc("*") => {
                                specifiers.extend(self.parse_namespace_import_specifiers()?);
                                self.parse_import_from_dclr(specifiers)
                            }
                            TokenType::Punctuator if self.current.matches_punc("{") => {
                                specifiers.extend(self.parse_named_import_specifiers()?);
                                self.parse_import_from_dclr(specifiers)
                            }
                            _ => self.unexpected_current(),
                        }
                    }
                    _ => self.parse_import_from_dclr(specifiers),
                }
            }
            TokenType::StringLiteral => unimplemented!(),
            _ => self.unexpected_current(),
        }
    }

    fn parse_namespace_import_specifiers(&mut self) -> Result<Vec<ImportDeclarationSpecifier>> {
        self.consume("*", LexGoal::RegExp)?;
        self.consume_id("as", LexGoal::RegExp)?;
        let local = self
            .with_params(
                Params {
                    has_yield: false,
                    has_await: false,
                    ..self.params
                },
                &mut Self::parse_binding_id,
            )?
            .to_id()?;
        Ok(vec![ImportDeclarationSpecifier::ImportNamespaceSpecifier {
            local,
        }])
    }

    fn parse_named_import_specifiers(&mut self) -> Result<Vec<ImportDeclarationSpecifier>> {
        self.consume("{", LexGoal::RegExp)?;
        let specifiers = self.parse_until_punc("}", &mut |this| {
            this.start_span();
            let name = this.parse_id_name(LexGoal::RegExp)?;
            let span = this.end_span();
            match this.current.tokentype {
                TokenType::Identifier if this.current.matches_str("as") => {
                    let imported = Identifier { name, span };
                    this.advance(LexGoal::RegExp)?;
                    let local = this
                        .with_params(
                            Params {
                                has_yield: false,
                                has_await: false,
                                ..this.params
                            },
                            &mut Self::parse_binding_id,
                        )?
                        .to_id()?;
                    if !this.current.matches_punc("}") {
                        this.consume(",", LexGoal::RegExp)?;
                    }
                    Ok(ImportDeclarationSpecifier::ImportSpecifier { local, imported })
                }
                _ => {
                    if this.scanner.is_keyword(&name) {
                        let token = this.advance(LexGoal::RegExp)?;
                        return Err(Error {
                            line: token.line_num,
                            col: token.line_start,
                            errortype: ErrorType::UnexpectedReserved,
                        });
                    }
                    let imported = Identifier {
                        name: name.clone(),
                        span: span.clone(),
                    };
                    let local = Identifier { name, span };
                    if !this.current.matches_punc("}") {
                        this.consume(",", LexGoal::RegExp)?;
                    }
                    Ok(ImportDeclarationSpecifier::ImportSpecifier { local, imported })
                }
            }
        })?;
        self.consume("}", LexGoal::RegExp)?;
        Ok(specifiers)
    }

    fn parse_import_from_dclr(
        &mut self,
        specifiers: Vec<ImportDeclarationSpecifier>,
    ) -> Result<Node> {
        self.consume_id("from", LexGoal::RegExp)?;
        let source = self.parse_module_specifier()?;
        self.consume_semicolon(LexGoal::RegExp)?;
        Ok(Node::ImportDeclaration {
            specifiers,
            source,
            span: self.end_span(),
        })
    }

    pub fn parse_module_specifier(&mut self) -> Result<Literal> {
        match self.current.tokentype {
            TokenType::StringLiteral => {
                self.start_span();
                let value = self.advance(LexGoal::RegExp)?.value;
                let raw = String::from(&value);
                Ok(Literal {
                    value,
                    raw,
                    span: self.end_span(),
                })
            }
            _ => self.unexpected_current(),
        }
    }

    pub fn parse_export_dclr(&mut self) -> Result<Node> {
        log!("parse_export_dclr");
        self.start_span();
        self.consume_kw("export", LexGoal::RegExp)?;
        match self.current.tokentype {
            TokenType::Keyword if self.current.matches_str("default") => {
                self.advance(LexGoal::RegExp)?;
                match self.current.tokentype {
                    TokenType::Identifier | TokenType::Keyword
                        if self.current.is_hoistable_dclr_kw() =>
                    {
                        let declaration = self.with_params(
                            Params {
                                has_yield: false,
                                has_await: false,
                                has_default: true,
                                ..self.params
                            },
                            &mut Self::parse_hoistable_dclr,
                        )?;
                        Ok(Node::ExportDefaultDeclaration {
                            declaration: Box::from(declaration),
                            span: self.end_span(),
                        })
                    }
                    TokenType::Keyword if self.current.matches_str("class") => {
                        let declaration = self.with_params(
                            Params {
                                has_yield: false,
                                has_await: false,
                                has_default: true,
                                ..self.params
                            },
                            &mut Self::parse_class_dclr,
                        )?;
                        Ok(Node::ExportDefaultDeclaration {
                            declaration: Box::from(declaration),
                            span: self.end_span(),
                        })
                    }
                    _ => {
                        let declaration = self.with_params(
                            Params {
                                has_in: true,
                                has_yield: false,
                                has_await: false,
                                ..self.params
                            },
                            &mut Self::parse_assignment_expr,
                        )?;
                        self.consume_semicolon(LexGoal::RegExp)?;
                        Ok(Node::ExportDefaultDeclaration {
                            declaration: Box::from(declaration),
                            span: self.end_span(),
                        })
                    }
                }
            }
            TokenType::Keyword if self.current.matches_str("var") => {
                let declaration = self.with_params(
                    Params {
                        has_yield: false,
                        has_await: false,
                        ..self.params
                    },
                    &mut |this| this.parse_variable_stmt(true),
                )?;
                Ok(Node::ExportNamedDeclaration {
                    declaration: Some(Box::from(declaration)),
                    specifiers: vec![],
                    source: None,
                    span: self.end_span(),
                })
            }
            TokenType::Keyword if self.current.is_dclr_kw() => {
                let declaration = self.with_params(
                    Params {
                        has_yield: false,
                        has_await: false,
                        ..self.params
                    },
                    &mut Self::parse_declaration,
                )?;
                Ok(Node::ExportNamedDeclaration {
                    declaration: Some(Box::from(declaration)),
                    specifiers: vec![],
                    source: None,
                    span: self.end_span(),
                })
            }
            TokenType::Punctuator if self.current.matches_punc("{") => {
                let specifiers = self.parse_named_export_specifiers()?;
                match self.current.tokentype {
                    TokenType::Keyword if self.current.matches_str("from") => {
                        self.advance(LexGoal::RegExp)?;
                        let source = Some(self.parse_module_specifier()?);
                        self.consume_semicolon(LexGoal::RegExp)?;
                        Ok(Node::ExportNamedDeclaration {
                            declaration: None,
                            specifiers,
                            source,
                            span: self.end_span(),
                        })
                    }
                    _ => Ok(Node::ExportNamedDeclaration {
                        declaration: None,
                        specifiers,
                        source: None,
                        span: self.end_span(),
                    }),
                }
            }
            TokenType::Punctuator if self.current.matches_punc("*") => {
                self.advance(LexGoal::RegExp)?;
                self.consume_id("from", LexGoal::RegExp)?;
                let source = self.parse_module_specifier()?;
                self.consume_semicolon(LexGoal::RegExp)?;
                Ok(Node::ExportAllDeclaration {
                    source,
                    span: self.end_span(),
                })
            }
            _ => self.unexpected_current(),
        }
    }

    fn parse_named_export_specifiers(&mut self) -> Result<Vec<ExportSpecifier>> {
        self.consume("{", LexGoal::RegExp)?;
        let specifiers = self.parse_until_punc("}", &mut |this| {
            this.start_span();
            let name = this.parse_id_name(LexGoal::RegExp)?;
            match this.current.tokentype {
                TokenType::Identifier if this.current.matches_str("as") => {
                    let exported = Identifier {
                        name,
                        span: this.end_span(),
                    };
                    this.advance(LexGoal::RegExp)?;
                    this.start_span();
                    let local = Identifier {
                        name: this.parse_id_name(LexGoal::RegExp)?,
                        span: this.end_span(),
                    };
                    if !this.current.matches_punc("}") {
                        this.consume(",", LexGoal::RegExp)?;
                    }
                    Ok(ExportSpecifier { local, exported })
                }
                _ => {
                    let span = this.end_span();
                    let exported = Identifier {
                        name: name.clone(),
                        span: span.clone(),
                    };
                    let local = Identifier { name, span };
                    if !this.current.matches_punc("}") {
                        this.consume(",", LexGoal::RegExp)?;
                    }
                    Ok(ExportSpecifier { local, exported })
                }
            }
        })?;
        self.consume("}", LexGoal::RegExp)?;
        Ok(specifiers)
    }

    // *******************************************************************
    //      Statements & Declarations
    // *******************************************************************

    pub fn parse_declaration(&mut self) -> Result<Node> {
        log!("parse_declaration");
        match self.current.tokentype {
            TokenType::Keyword if self.current.is_hoistable_dclr_kw() => {
                self.with_default(false, &mut Self::parse_hoistable_dclr)
            }
            TokenType::Keyword if self.current.matches_str("class") => {
                self.with_default(false, &mut Self::parse_class_dclr)
            }
            TokenType::Keyword if self.current.is_lexical_dclr_kw() => {
                self.with_in(true, &mut Self::parse_lexical_dclr)
            }
            _ => self.unexpected_current(),
        }
    }

    pub fn parse_statement_list(&mut self) -> Result<Vec<Node>> {
        log!("parse_statement_list");
        self.parse_until_punc("}", &mut Self::parse_stmt_list_item)
    }

    pub fn parse_stmt_list_item(&mut self) -> Result<Node> {
        log!("parse_stmt_list_item");
        match self.current.tokentype {
            // statements
            TokenType::Punctuator if self.current.matches_punc("{") => self.parse_block_stmt(),
            TokenType::Punctuator if self.current.matches_punc(";") => self.parse_empty_stmt(),
            TokenType::Keyword if self.current.matches_str("var") => self.parse_variable_stmt(true),
            TokenType::Keyword if self.current.matches_str("if") => self.parse_if_stmt(),
            TokenType::Keyword if self.current.is_iteration_kw() => self.parse_iteration_stmt(),
            TokenType::Keyword if self.current.matches_str("switch") => self.parse_switch_stmt(),
            TokenType::Keyword if self.current.matches_str("continue") => {
                self.parse_continue_stmt()
            }
            TokenType::Keyword if self.current.matches_str("break") => self.parse_break_stmt(),
            TokenType::Keyword if self.current.matches_str("return") => {
                if self.params.has_return {
                    self.parse_return_stmt()
                } else {
                    let token = self.advance(LexGoal::RegExp)?;
                    self.error(token, ErrorType::IllegalReturn)
                }
            }
            TokenType::Keyword if self.current.matches_str("with") => self.parse_with_stmt(),
            TokenType::Keyword if self.current.matches_str("throw") => self.parse_throw_stmt(),
            TokenType::Keyword if self.current.matches_str("try") => self.parse_try_stmt(),
            TokenType::Keyword if self.current.matches_str("debugger") => {
                self.parse_debugger_stmt()
            }
            // special LL(2) grammar case
            TokenType::Identifier if self.current.matches_str("async") => {
                self.start_span();
                self.scanner.save_state();
                let next_token = self.scanner.next(LexGoal::RegExp)?;
                match &next_token.tokentype {
                    TokenType::Keyword
                        if next_token.matches_str("function")
                            && self.current.line_num == next_token.line_num =>
                    {
                        self.scanner.restore_state(next_token);
                        self.with_default(false, &mut Self::parse_hoistable_dclr)
                    }
                    _ => {
                        self.scanner.restore_state(next_token);
                        self.end_span();
                        self.parse_expression_or_labelled_stmt()
                    }
                }
            }
            // declarations
            TokenType::Identifier | TokenType::Keyword if self.current.is_hoistable_dclr_kw() => {
                self.with_default(false, &mut Self::parse_hoistable_dclr)
            }
            TokenType::Keyword if self.current.matches_str("class") => {
                self.with_default(false, &mut Self::parse_class_dclr)
            }
            TokenType::Identifier | TokenType::Keyword if self.current.is_lexical_dclr_kw() => {
                self.with_in(true, &mut Self::parse_lexical_dclr)
            }
            // ExpressionStatement | LabelledStatement
            _ => self.parse_expression_or_labelled_stmt(),
        }
    }

    pub fn parse_statement(&mut self) -> Result<Node> {
        log!("parse_statement");
        // Statement[Yield, Await, Return]:
        //     BlockStatement[?Yield, ?Await, ?Return]
        //     VariableStatement[?Yield, ?Await]
        //     EmptyStatement
        //     ExpressionStatement[?Yield, ?Await]
        //     IfStatement[?Yield, ?Await, ?Return]
        //     BreakableStatement[?Yield, ?Await, ?Return]
        //     ContinueStatement[?Yield, ?Await]
        //     BreakStatement[?Yield, ?Await]
        //     [+Return] ReturnStatement[?Yield, ?Await]
        //     WithStatement[?Yield, ?Await, ?Return]
        //     LabelledStatement[?Yield, ?Await, ?Return]
        //     ThrowStatement[?Yield, ?Await]
        //     TryStatement[?Yield, ?Await, ?Return]
        //     DebuggerStatement
        match self.current.tokentype {
            TokenType::Punctuator if self.current.matches_punc("{") => self.parse_block_stmt(),
            TokenType::Keyword if self.current.matches_str("var") => self.parse_variable_stmt(true),
            TokenType::Punctuator if self.current.matches_punc(";") => self.parse_empty_stmt(),
            TokenType::Keyword if self.current.matches_str("if") => self.parse_if_stmt(),
            TokenType::Keyword if self.current.is_iteration_kw() => self.parse_iteration_stmt(),
            TokenType::Keyword if self.current.matches_str("switch") => self.parse_switch_stmt(),
            TokenType::Keyword if self.current.matches_str("continue") => {
                self.parse_continue_stmt()
            }
            TokenType::Keyword if self.current.matches_str("break") => self.parse_break_stmt(),
            TokenType::Keyword if self.current.matches_str("return") => {
                if self.params.has_return {
                    self.parse_return_stmt()
                } else {
                    let token = self.advance(LexGoal::RegExp)?;
                    self.error(token, ErrorType::IllegalReturn)
                }
            }
            TokenType::Keyword if self.current.matches_str("with") => self.parse_with_stmt(),
            TokenType::Keyword if self.current.matches_str("throw") => self.parse_throw_stmt(),
            TokenType::Keyword if self.current.matches_str("try") => self.parse_try_stmt(),
            TokenType::Keyword if self.current.matches_str("debugger") => {
                self.parse_debugger_stmt()
            }
            // ExpressionStatement | LabelledStatement
            _ => self.parse_expression_or_labelled_stmt(),
        }
    }

    pub fn parse_expression_or_labelled_stmt(&mut self) -> Result<Node> {
        log!("parse_expression_or_labelled_stmt");
        self.start_span();
        let expr = self.with_in(true, &mut Self::parse_expression)?;
        match self.current.tokentype {
            TokenType::Punctuator if self.current.matches_punc(":") => match expr {
                Node::Identifier { name, span, .. } => {
                    self.advance(LexGoal::RegExp)?;
                    self.parse_labelled_stmt(Some(Identifier { name, span }))
                }
                _ => self.parse_expression_stmt(Box::from(expr)),
            },
            _ => self.parse_expression_stmt(Box::from(expr)),
        }
    }

    pub fn parse_block_stmt(&mut self) -> Result<Node> {
        log!("parse_block_stmt");
        // BlockStatement[Yield, Await, Return]:
        //     Block[?Yield, ?Await, ?Return]
        // Block[Yield, Await, Return]:
        //     "{" StatementList[?Yield, ?Await, ?Return](opt) "}"
        self.start_span();
        self.consume("{", LexGoal::RegExp)?;
        let body = self.parse_statement_list()?;
        self.consume("}", LexGoal::RegExp)?;
        Ok(Node::BlockStatement {
            body,
            span: self.end_span(),
        })
    }

    pub fn parse_variable_stmt(&mut self, consume_var: bool) -> Result<Node> {
        log!("parse_variable_stmt");
        // VariableStatement[Yield, Await]:
        //     "var" VariableDeclarationList[+In, ?Yield, ?Await] ";"
        // VariableDeclarationList[In, Yield, Await]:
        //     VariableDeclaration[?In, ?Yield, ?Await]
        //     VariableDeclarationList[?In, ?Yield, ?Await] "," VariableDeclaration[?In, ?Yield, ?Await]
        // VariableDeclaration[In, Yield, Await]:
        //     BindingIdentifier[?Yield, ?Await] Initializer[?In, ?Yield, ?Await](opt)
        //     BindingPattern[?Yield, ?Await] Initializer[?In, ?Yield, ?Await]
        self.start_span();
        if consume_var {
            self.consume_kw("var", LexGoal::RegExp)?;
        }
        self.save_params();
        self.params.has_in = true;
        let mut declarations = vec![self.parse_variable_dclr()?];
        declarations.extend(self.parse_while_punc(",", &mut |this| {
            this.consume(",", LexGoal::RegExp)?;
            this.parse_variable_dclr()
        })?);
        self.restore_params();
        self.consume_semicolon(LexGoal::RegExp)?;
        Ok(Node::VariableDeclaration {
            declarations,
            kind: "var",
            span: self.end_span(),
        })
    }

    fn parse_variable_dclr(&mut self) -> Result<VariableDeclarator> {
        log!("parse_variable_dclr");
        let id = self.parse_binding_id_or_pat()?;
        let init = match self.current.tokentype {
            TokenType::Punctuator if self.current.matches_punc("=") => {
                self.advance(LexGoal::RegExp)?;
                Some(self.parse_assignment_expr()?)
            }
            _ => match &id {
                Binding::BindingIdentifier { .. } => None,
                _ => {
                    let token = self.advance(LexGoal::RegExp)?;
                    return self.error(token, ErrorType::MissingInitializer);
                }
            },
        };
        Ok(VariableDeclarator { id, init })
    }

    pub fn parse_empty_stmt(&mut self) -> Result<Node> {
        log!("parse_empty_stmt");
        self.start_span();
        self.consume(";", LexGoal::RegExp)?;
        Ok(Node::EmptyStatement {
            span: self.end_span(),
        })
    }

    pub fn parse_expression_stmt(&mut self, expression: Box<Node>) -> Result<Node> {
        log!("parse_expression_stmt");
        let span = self.end_span();
        self.consume_semicolon(LexGoal::RegExp)?;
        Ok(Node::ExpressionStatement { expression, span })
    }

    pub fn parse_if_stmt(&mut self) -> Result<Node> {
        log!("parse_if_stmt");
        // IfStatement[Yield, Await, Return]:
        //     "if" "(" Expression[+In, ?Yield, ?Await] ")"
        //       Statement[?Yield, ?Await, ?Return] "else" Statement[?Yield, ?Await, ?Return]
        //     if "(" Expression[+In, ?Yield, ?Await] ")" Statement[?Yield, ?Await, ?Return]
        // IfStatement {
        //     test: Box<Node>,              // Expression
        //     consequent: Box<Node>,        // Statement
        //     alternate: Option<Box<Node>>, // Statement | null
        // },
        self.start_span();
        self.consume_kw("if", LexGoal::RegExp)?;
        self.consume("(", LexGoal::RegExp)?;
        let test = Box::from(self.with_in(true, &mut Self::parse_expression)?);
        self.consume(")", LexGoal::RegExp)?;
        let consequent = Box::from(self.parse_statement()?);
        let alternate = match self.current.tokentype {
            TokenType::Keyword if self.current.matches_str("else") => {
                self.advance(LexGoal::RegExp)?;
                Some(Box::from(self.parse_statement()?))
            }
            _ => None,
        };
        Ok(Node::IfStatement {
            test,
            consequent,
            alternate,
            span: self.end_span(),
        })
    }

    pub fn parse_iteration_stmt(&mut self) -> Result<Node> {
        log!("parse_iteration_stmt");
        match self.current.tokentype {
            TokenType::Keyword if self.current.matches_str("for") => self.parse_for_stmt(),
            TokenType::Keyword if self.current.matches_str("while") => self.parse_while_stmt(),
            TokenType::Keyword if self.current.matches_str("do") => self.parse_do_while_stmt(),
            _ => unreachable!(),
        }
    }

    pub fn parse_for_stmt(&mut self) -> Result<Node> {
        log!("parse_for_stmt");
        // IterationStatement[Yield, Await, Return]:
        //     "for" "(" "var" VariableDeclarationList[~In, ?Yield, ?Await]
        //       ";" Expression[+In, ?Yield, ?Await](opt)
        //       ";" Expression[+In, ?Yield, ?Await](opt) ")" Statement[?Yield, ?Await, ?Return]
        //     "for" "(" "var" ForBinding[?Yield, ?Await] "in" Expression[+In, ?Yield, ?Await] ")" Statement[?Yield, ?Await, ?Return]
        //     "for" "(" "var" ForBinding[?Yield, ?Await] "of"
        //       AssignmentExpression[+In, ?Yield, ?Await] ")" Statement[?Yield, ?Await, ?Return]

        //     "for" "(" LexicalDeclaration[~In, ?Yield, ?Await] Expression[+In, ?Yield, ?Await](opt)
        //       ";" Expression[+In, ?Yield, ?Await](opt) ")" Statement[?Yield, ?Await, ?Return]
        //     for "(" ForDeclaration[?Yield, ?Await] "in" Expression[+In, ?Yield, ?Await] ")" Statement[?Yield, ?Await, ?Return]
        //     "for" "(" ForDeclaration[?Yield, ?Await] "of" AssignmentExpression[+In, ?Yield, ?Await] ")" Statement[?Yield, ?Await, ?Return]

        //     "for" "(" [lookahead ∉ { let [ }] Expression[~In, ?Yield, ?Await](opt)
        //       ";" Expression[+In, ?Yield, ?Await](opt)
        //       ";" Expression[+In, ?Yield, ?Await](opt) ")" Statement[?Yield, ?Await, ?Return]
        //     "for" "(" [lookahead ∉ { let [ }] LeftHandSideExpression[?Yield, ?Await] "in"
        //       Expression[+In, ?Yield, ?Await] ")" Statement[?Yield, ?Await, ?Return]
        //     for "(" [lookahead ≠ let] LeftHandSideExpression[?Yield, ?Await] "of"
        //       AssignmentExpression[+In, ?Yield, ?Await] ")" Statement[?Yield, ?Await, ?Return]
        // ForBinding[Yield, Await]:
        //     BindingIdentifier[?Yield, ?Await]
        //     BindingPattern[?Yield, ?Await]
        // VariableDeclaration[In, Yield, Await]:
        //     BindingIdentifier[?Yield, ?Await] Initializer[?In, ?Yield, ?Await](opt)
        //     BindingPattern[?Yield, ?Await] Initializer[?In, ?Yield, ?Await]

        // ForStatement {
        //     init: Option<Box<Node>>,   // Expression | null
        //     test: Option<Box<Node>>,   // Expression | null
        //     update: Option<Box<Node>>, // Expression | null
        //     body: Box<Node>,           // Statement
        // },
        self.start_span();
        self.consume_kw("for", LexGoal::RegExp)?;
        self.consume("(", LexGoal::RegExp)?;
        match self.current.tokentype {
            TokenType::Keyword if self.current.matches_str("var") => {
                self.start_span();
                self.advance(LexGoal::RegExp)?;
                let id = self.parse_binding_id_or_pat()?;
                match self.current.tokentype {
                    TokenType::Punctuator if self.current.matches_punc("=") => {
                        // "var" (BindingIdentifier | BindingPattern) "=" Initializer
                        // pub struct VariableDeclarator {
                        //     pub id: Binding,
                        //     pub init: Option<Node>, // Expression | null
                        // }
                        self.advance(LexGoal::RegExp)?;
                        let init = Some(self.with_in(false, &mut Self::parse_assignment_expr)?);
                        let mut declarations = vec![VariableDeclarator { id, init }];
                        declarations.extend(self.parse_while_punc(",", &mut |this| {
                            this.consume(",", LexGoal::RegExp)?;
                            this.save_params();
                            this.params.has_in = true;
                            let res = this.parse_variable_dclr();
                            this.restore_params();
                            res
                        })?);
                        let init = Some(Box::from(Node::VariableDeclaration {
                            declarations,
                            kind: "var",
                            span: self.end_span(),
                        }));
                        self.parse_for_classic_stmt(init)
                    }
                    TokenType::Keyword if self.current.matches_str("of") => {
                        // "var" (BindingIdentifier | BindingPattern) "of"
                        // TODO: end span
                        self.parse_for_of_stmt(Box::from(id.to_node()?))
                    }
                    TokenType::Keyword if self.current.matches_str("in") => {
                        // "var" (BindingIdentifier | BindingPattern) "in"
                        // TODO: end span
                        self.parse_for_in_stmt(Box::from(id.to_node()?))
                    }
                    _ => {
                        match &id {
                            Binding::BindingIdentifier { .. } => {
                                // "var" BindingIdentifier
                                let mut declarations = vec![VariableDeclarator { id, init: None }];
                                declarations.extend(self.parse_while_punc(",", &mut |this| {
                                    this.consume(",", LexGoal::RegExp)?;
                                    this.save_params();
                                    this.params.has_in = true;
                                    let res = this.parse_variable_dclr();
                                    this.restore_params();
                                    res
                                })?);
                                let init = Some(Box::from(Node::VariableDeclaration {
                                    declarations,
                                    kind: "var",
                                    span: self.end_span(),
                                }));
                                self.parse_for_classic_stmt(init)
                            }
                            _ => self.error_current(ErrorType::MissingInitializer), // TODO: check if row col is correct
                        }
                    }
                }
            }
            TokenType::Keyword | TokenType::Identifier if self.current.is_lexical_dclr_kw() => {
                // "for" "(" LexicalDeclaration[~In, ?Yield, ?Await] Expression[+In, ?Yield, ?Await](opt)
                //     ";" Expression[+In, ?Yield, ?Await](opt) ")" Statement[?Yield, ?Await, ?Return]
                // for "(" ForDeclaration[?Yield, ?Await] "in" Expression[+In, ?Yield, ?Await] ")" Statement[?Yield, ?Await, ?Return]
                // "for" "(" ForDeclaration[?Yield, ?Await] "of" AssignmentExpression[+In, ?Yield, ?Await] ")" Statement[?Yield, ?Await, ?Return]
                // LexicalDeclaration[In, Yield, Await]:
                //     LetOrConst BindingList[?In, ?Yield, ?Await];
                // BindingList[In, Yield, Await]:
                //     LexicalBinding[?In, ?Yield, ?Await]
                //     BindingList[?In, ?Yield, ?Await] "," LexicalBinding[?In, ?Yield, ?Await]
                // LexicalBinding[In, Yield, Await]:
                //     BindingIdentifier[?Yield, ?Await] Initializer[?In, ?Yield, ?Await](opt)
                //     BindingPattern[?Yield, ?Await] Initializer[?In, ?Yield, ?Await]
                // ForDeclaration[Yield, Await]:
                //     LetOrConst ForBinding[?Yield, ?Await]
                // ForBinding[Yield, Await]:
                //     BindingIdentifier[?Yield, ?Await]
                //     BindingPattern[?Yield, ?Await]
                // let kind = &(self.advance(LexGoal::RegExp)?.value.string()?.clone())[..];
                self.start_span();
                let kind = match self.current.tokentype {
                    TokenType::Keyword if self.current.matches_str("const") => "const",
                    TokenType::Identifier if self.current.matches_str("let") => "let",
                    _ => return self.unexpected_current(),
                };
                self.advance(LexGoal::RegExp)?;
                let (line, col) = (self.current.line_num, self.current.line_start);
                let id = self.parse_binding_id_or_pat()?;
                match self.current.tokentype {
                    TokenType::Punctuator if self.current.matches_punc("=") => {
                        // LetOrConst Binding "=" "," ...
                        self.advance(LexGoal::RegExp)?;
                        let init = Some(self.with_in(false, &mut Self::parse_assignment_expr)?);
                        let mut declarations = vec![VariableDeclarator { id, init }];
                        declarations.extend(self.parse_while_punc(",", &mut |this| {
                            this.consume(",", LexGoal::RegExp)?;
                            this.save_params();
                            this.params.has_in = true;
                            let res = this.parse_variable_dclr();
                            this.restore_params();
                            res
                        })?);
                        let init = Some(Box::from(Node::VariableDeclaration {
                            declarations,
                            kind,
                            span: self.end_span(),
                        }));
                        self.parse_for_classic_stmt(init)
                    }
                    TokenType::Keyword if self.current.matches_str("in") => {
                        // LetOrConst Binding "in" ...
                        // TODO: end span
                        self.parse_for_in_stmt(Box::from(id.to_node()?))
                    }
                    TokenType::Identifier if self.current.matches_str("of") => {
                        // LetOrConst Binding "of" ...
                        // TODO: end span
                        self.parse_for_of_stmt(Box::from(id.to_node()?))
                    }
                    _ => {
                        match &id {
                            Binding::BindingIdentifier { .. } => {
                                // LetOrConst BindingIdentifier "," ...
                                let mut declarations = vec![VariableDeclarator { id, init: None }];
                                declarations.extend(self.parse_while_punc(",", &mut |this| {
                                    this.consume(",", LexGoal::RegExp)?;
                                    this.save_params();
                                    this.params.has_in = true;
                                    let res = this.parse_variable_dclr();
                                    this.restore_params();
                                    res
                                })?);
                                let init = Some(Box::from(Node::VariableDeclaration {
                                    declarations,
                                    kind,
                                    span: self.end_span(),
                                }));
                                self.parse_for_classic_stmt(init)
                            }
                            _ => Err(Error {
                                line,
                                col,
                                errortype: ErrorType::MissingInitializer,
                            }),
                        }
                    }
                }
            }
            _ => {
                // "for" "(" [lookahead ∉ { let [ }] Expression[~In, ?Yield, ?Await](opt)
                //     ";" Expression[+In, ?Yield, ?Await](opt)
                //     ";" Expression[+In, ?Yield, ?Await](opt) ")" Statement[?Yield, ?Await, ?Return]
                // "for" "(" [lookahead ∉ { let [ }] LeftHandSideExpression[?Yield, ?Await] "in"
                //     Expression[+In, ?Yield, ?Await] ")" Statement[?Yield, ?Await, ?Return]
                // for "(" [lookahead ≠ let] LeftHandSideExpression[?Yield, ?Await] "of"
                //     AssignmentExpression[+In, ?Yield, ?Await] ")" Statement[?Yield, ?Await, ?Return]
                self.start_span();
                let mut expr = self.with_in(false, &mut Self::parse_assignment_expr)?;
                match self.current.tokentype {
                    TokenType::Keyword if self.current.matches_str("in") => {
                        self.end_span(); // not needed
                        expr = match expr {
                            Node::ObjectExpression { .. } | Node::ArrayExpression { .. } => {
                                expr.to_assignment_pattern()
                            }
                            _ => Ok(expr),
                        }?;
                        if expr.assign_target_type() != "simple" {
                            let token = self.advance(LexGoal::RegExp)?;
                            return self.error(token, ErrorType::InvalidLhsInFor);
                        }
                        self.parse_for_in_stmt(Box::from(expr))
                    }
                    TokenType::Keyword if self.current.matches_str("of") => {
                        self.end_span(); // not needed
                        expr = match expr {
                            Node::ObjectExpression { .. } | Node::ArrayExpression { .. } => {
                                expr.to_assignment_pattern()
                            }
                            _ => Ok(expr),
                        }?;
                        if expr.assign_target_type() != "simple" {
                            let token = self.advance(LexGoal::RegExp)?;
                            return self.error(token, ErrorType::InvalidLhsInFor);
                        }
                        self.parse_for_of_stmt(Box::from(expr))
                    }
                    _ => {
                        let mut expressions = vec![expr];
                        expressions.extend(self.parse_while_punc(",", &mut |this| {
                            this.consume(",", LexGoal::RegExp)?;
                            this.with_in(true, &mut Self::parse_assignment_expr)
                        })?);
                        let init = Some(Box::from(Node::SequenceExpression {
                            expressions,
                            assign_target_type: "invalid",
                            span: self.end_span(),
                        }));
                        self.parse_for_classic_stmt(init)
                    }
                }
            }
        }
    }

    fn parse_for_classic_stmt(&mut self, init: Option<Box<Node>>) -> Result<Node> {
        // "for" "(" "var" VariableDeclarationList[~In, ?Yield, ?Await]
        //     ";" Expression[+In, ?Yield, ?Await](opt)
        //     ";" Expression[+In, ?Yield, ?Await](opt) ")" Statement[?Yield, ?Await, ?Return]
        // ForStatement {
        //     init: Option<Box<Node>>,   // Expression | null
        //     test: Option<Box<Node>>,   // Expression | null
        //     update: Option<Box<Node>>, // Expression | null
        //     body: Box<Node>,           // Statement
        // },
        self.consume(";", LexGoal::RegExp)?;
        let test = match self.current.tokentype {
            TokenType::Punctuator if self.current.matches_punc(";") => None,
            _ => Some(Box::from(self.with_in(true, &mut Self::parse_expression)?)),
        };
        self.consume(";", LexGoal::RegExp)?;
        let update = match self.current.tokentype {
            TokenType::Punctuator if self.current.matches_punc(")") => None,
            _ => Some(Box::from(self.with_in(true, &mut Self::parse_expression)?)),
        };
        self.consume(")", LexGoal::RegExp)?;
        let prev_in_iteration = self.ctx.in_iteration;
        self.ctx.in_iteration = true;
        let body = Box::from(self.parse_statement()?);
        self.ctx.in_iteration = prev_in_iteration;
        Ok(Node::ForStatement {
            init,
            test,
            update,
            body,
            span: self.end_span(),
        })
    }

    fn parse_for_in_stmt(&mut self, left: Box<Node>) -> Result<Node> {
        // "for" "(" "var" ForBinding[?Yield, ?Await] "in" Expression[+In, ?Yield, ?Await] ")" Statement[?Yield, ?Await, ?Return]
        // ForInStatement {
        //     left: Box<Node>,  // Expression
        //     right: Box<Node>, // Expression
        //     body: Box<Node>,  // Statement
        //     each: bool,
        // },
        self.consume_kw("in", LexGoal::RegExp)?;
        let right = Box::from(self.with_in(true, &mut Self::parse_expression)?);
        self.consume(")", LexGoal::RegExp)?;
        let prev_in_iteration = self.ctx.in_iteration;
        self.ctx.in_iteration = true;
        let body = Box::from(self.parse_statement()?);
        self.ctx.in_iteration = prev_in_iteration;
        Ok(Node::ForInStatement {
            left,
            right,
            body,
            each: false,
            span: self.end_span(),
        })
    }

    fn parse_for_of_stmt(&mut self, left: Box<Node>) -> Result<Node> {
        // "for" "(" "var" ForBinding[?Yield, ?Await] "of"
        //     AssignmentExpression[+In, ?Yield, ?Await] ")" Statement[?Yield, ?Await, ?Return]
        // ForOfStatement {
        //     left: Box<Node>,  // Expression
        //     right: Box<Node>, // Expression
        //     body: Box<Node>,  // Statement
        // },
        self.consume_id("of", LexGoal::RegExp)?;
        let right = Box::from(self.with_in(true, &mut Self::parse_assignment_expr)?);
        self.consume(")", LexGoal::RegExp)?;
        let prev_in_iteration = self.ctx.in_iteration;
        self.ctx.in_iteration = true;
        let body = Box::from(self.parse_statement()?);
        self.ctx.in_iteration = prev_in_iteration;
        Ok(Node::ForOfStatement {
            left,
            right,
            body,
            span: self.end_span(),
        })
    }

    pub fn parse_while_stmt(&mut self) -> Result<Node> {
        log!("parse_while_stmt");
        // IterationStatement[Yield, Await, Return]:
        //     "while" "(" Expression[+In, ?Yield, ?Await] ")" Statement[?Yield, ?Await, ?Return]
        self.start_span();
        self.consume_kw("while", LexGoal::RegExp)?;
        self.consume("(", LexGoal::RegExp)?;
        let test = Box::from(self.with_in(true, &mut Self::parse_expression)?);
        self.consume(")", LexGoal::RegExp)?;
        let prev_in_iteration = self.ctx.in_iteration;
        self.ctx.in_iteration = true;
        let body = Box::from(self.parse_statement()?);
        self.ctx.in_iteration = prev_in_iteration;
        Ok(Node::WhileStatement {
            test,
            body,
            span: self.end_span(),
        })
    }

    pub fn parse_do_while_stmt(&mut self) -> Result<Node> {
        log!("parse_do_while_stmt");
        // IterationStatement[Yield, Await, Return]:
        //     "do" Statement[?Yield, ?Await, ?Return] "while" "(" Expression[+In, ?Yield, ?Await] ")" ";"
        // DoWhileStatement {
        //     body: Box<Node>, // Statement
        //     test: Box<Node>, // Expression
        // },
        self.start_span();
        self.consume_kw("do", LexGoal::RegExp)?;
        let prev_in_iteration = self.ctx.in_iteration;
        self.ctx.in_iteration = true;
        let body = Box::from(self.parse_statement()?);
        self.ctx.in_iteration = prev_in_iteration;
        self.consume_kw("while", LexGoal::RegExp)?;
        self.consume("(", LexGoal::RegExp)?;
        let test = Box::from(self.with_in(true, &mut Self::parse_expression)?);
        self.consume(")", LexGoal::RegExp)?;
        self.consume_semicolon(LexGoal::RegExp)?;
        Ok(Node::DoWhileStatement {
            body,
            test,
            span: self.end_span(),
        })
    }

    pub fn parse_switch_stmt(&mut self) -> Result<Node> {
        log!("parse_switch_stmt");
        // SwitchStatement[Yield, Await, Return]:
        //     "switch" "(" Expression[+In, ?Yield, ?Await] ")" CaseBlock[?Yield, ?Await, ?Return]
        // CaseBlock[Yield, Await, Return]:
        //     "{" CaseClauses[?Yield, ?Await, ?Return](opt) "}"
        //     "{" CaseClauses[?Yield, ?Await, ?Return](opt) DefaultClause[?Yield, ?Await, ?Return] CaseClauses[?Yield, ?Await, ?Return](opt) "}"
        // CaseClauses[Yield, Await, Return]:
        //     CaseClause[?Yield, ?Await, ?Return]
        //     CaseClauses[?Yield, ?Await, ?Return] CaseClause[?Yield, ?Await, ?Return]
        // CaseClause[Yield, Await, Return]:
        //     "case" Expression[+In, ?Yield, ?Await] ":" StatementList[?Yield, ?Await, ?Return](opt)
        // DefaultClause[Yield, Await, Return]:
        //     "default" ":" StatementList[?Yield, ?Await, ?Return](opt)
        // SwitchStatement {
        //     discriminant: Box<Node>, // Expression
        //     cases: Vec<SwitchCase>,
        // },
        self.start_span();
        self.consume_kw("switch", LexGoal::RegExp)?;
        self.consume("(", LexGoal::RegExp)?;
        let discriminant = Box::from(self.with_in(true, &mut Self::parse_expression)?);
        self.consume(")", LexGoal::RegExp)?;
        self.consume("{", LexGoal::RegExp)?;
        let mut cases = vec![];
        let mut first_default = true;
        loop {
            let test = match self.current.tokentype {
                TokenType::Keyword if self.current.matches_str("case") => {
                    self.advance(LexGoal::RegExp)?;
                    Some(self.with_in(true, &mut Self::parse_expression)?)
                }
                TokenType::Keyword if self.current.matches_str("default") && first_default => {
                    self.advance(LexGoal::RegExp)?;
                    first_default = false;
                    None
                }
                TokenType::Keyword if self.current.matches_str("default") => {
                    return self.error_current(ErrorType::MultipleDefaultsInSwitch);
                }
                _ => return self.unexpected_current(),
            };
            self.consume(":", LexGoal::RegExp)?;
            let consequent = self.parse_switch_case_consequent()?;
            cases.push(SwitchCase { test, consequent });
            match self.current.tokentype {
                TokenType::Punctuator if self.current.matches_punc("}") => break,
                _ => (),
            }
        }
        self.consume("}", LexGoal::RegExp)?;
        Ok(Node::SwitchStatement {
            discriminant,
            cases,
            span: self.end_span(),
        })
    }

    fn parse_switch_case_consequent(&mut self) -> Result<Vec<Node>> {
        let mut consequent = vec![];
        loop {
            match self.current.tokentype {
                TokenType::Punctuator if self.current.matches_punc("}") => break,
                TokenType::Keyword if self.current.matches_str("case") => break,
                TokenType::Keyword if self.current.matches_str("default") => break,
                _ => consequent.push(self.parse_stmt_list_item()?),
            }
        }
        Ok(consequent)
    }

    pub fn parse_continue_stmt(&mut self) -> Result<Node> {
        log!("parse_continue_stmt");
        // ContinueStatement[Yield, Await]:
        //     "continue" ";"
        //     "continue" [no LineTerminator here] LabelIdentifier[?Yield, ?Await] ";"
        self.start_span();
        self.consume_kw("continue", LexGoal::RegExp)?;
        let label = match self.current.tokentype {
            TokenType::Identifier | TokenType::Keyword if !self.has_line_terminator => {
                Some(Box::from(self.parse_id_reference()?))
            }
            _ => None,
        };
        match &label {
            None if !self.ctx.in_iteration => {
                return self.error_current(ErrorType::NoIterationStatement)
            }
            _ => (),
        }
        self.consume_semicolon(LexGoal::RegExp)?;
        Ok(Node::ContinueStatement {
            label,
            span: self.end_span(),
        })
    }

    pub fn parse_break_stmt(&mut self) -> Result<Node> {
        log!("parse_break_stmt");
        // BreakStatement[Yield, Await]:
        //     "break" ";"
        //     "break" [no LineTerminator here] LabelIdentifier[?Yield, ?Await] ";"
        self.start_span();
        self.consume_kw("break", LexGoal::RegExp)?;
        let label = match self.current.tokentype {
            TokenType::Identifier | TokenType::Keyword if !self.has_line_terminator => {
                Some(Box::from(self.parse_id_reference()?))
            }
            _ => None,
        };
        match &label {
            None if !self.ctx.in_iteration => return self.error_current(ErrorType::IllegalBreak),
            _ => (),
        }
        self.consume_semicolon(LexGoal::RegExp)?;
        Ok(Node::BreakStatement {
            label,
            span: self.end_span(),
        })
    }

    pub fn parse_return_stmt(&mut self) -> Result<Node> {
        log!("parse_return_stmt");
        // ReturnStatement[Yield, Await]:
        //     "return" ";"
        //     "return" [no LineTerminator here] Expression[+In, ?Yield, ?Await] ";"
        self.start_span();
        self.consume_kw("return", LexGoal::RegExp)?;
        let argument = match self.current.tokentype {
            TokenType::Punctuator if self.current.matches_punc(";") => None,
            _ if !self.has_line_terminator => {
                Some(Box::from(self.with_in(true, &mut Self::parse_expression)?))
            }
            _ => None,
        };
        self.consume_semicolon(LexGoal::RegExp)?;
        Ok(Node::ReturnStatement {
            argument,
            span: self.end_span(),
        })
    }

    pub fn parse_with_stmt(&mut self) -> Result<Node> {
        log!("parse_with_stmt");
        // WithStatement[Yield, Await, Return]:
        //     "with" "(" Expression[+In, ?Yield, ?Await] ")" Statement[?Yield, ?Await, ?Return]
        if self.ctx.strict {
            return self.error_current(ErrorType::StrictWith);
        }
        self.start_span();
        self.consume_kw("with", LexGoal::RegExp)?;
        self.consume("(", LexGoal::RegExp)?;
        let object = Box::from(self.with_in(true, &mut Self::parse_expression)?);
        self.consume(")", LexGoal::RegExp)?;
        let body = Box::from(self.parse_statement()?);
        Ok(Node::WithStatement {
            object,
            body,
            span: self.end_span(),
        })
    }

    pub fn parse_labelled_stmt(&mut self, label: Option<Identifier>) -> Result<Node> {
        log!("parse_labelled_stmt");
        // LabelledStatement[Yield, Await, Return]:
        //     LabelIdentifier[?Yield, ?Await] ":" LabelledItem[?Yield, ?Await, ?Return]
        // LabelledItem[Yield, Await, Return]:
        //     Statement[?Yield, ?Await, ?Return]
        //     FunctionDeclaration[?Yield, ?Await, ~Default]
        let label = match label {
            Some(label) => Ok(label),
            None => {
                self.start_span();
                match self.parse_id_reference()? {
                    Node::Identifier { name, span, .. } => Ok(Identifier { name, span }),
                    _ => unreachable!(),
                }
            }
        }?;
        self.consume(":", LexGoal::RegExp)?;
        let body =
            match self.current.tokentype {
                TokenType::Keyword if self.current.matches_str("function") => {
                    if self.ctx.strict {
                        return self.error_current(ErrorType::StrictFunction);
                    } else {
                        Box::from(self.with_default(false, &mut |this| {
                            this.parse_function_dclr(false, false)
                        })?)
                    }
                }
                _ => Box::from(self.parse_statement()?),
            };
        Ok(Node::LabeledStatement {
            label,
            body,
            span: self.end_span(),
        })
    }

    pub fn parse_throw_stmt(&mut self) -> Result<Node> {
        log!("parse_throw_stmt");
        // ThrowStatement[Yield, Await]:
        //     "throw" [no LineTerminator here] Expression[+In, ?Yield, ?Await] ";"
        self.start_span();
        self.consume_kw("throw", LexGoal::RegExp)?;
        if !self.has_line_terminator {
            let argument = Box::from(self.with_in(true, &mut Self::parse_expression)?);
            self.consume_semicolon(LexGoal::RegExp)?;
            Ok(Node::ThrowStatement {
                argument,
                span: self.end_span(),
            })
        } else {
            self.error_current(ErrorType::NewlineAfterThrow)
        }
    }

    pub fn parse_try_stmt(&mut self) -> Result<Node> {
        log!("parse_try_stmt");
        // TryStatement[Yield, Await, Return]:
        //     "try" Block[?Yield, ?Await, ?Return] Catch[?Yield, ?Await, ?Return]
        //     "try" Block[?Yield, ?Await, ?Return] Finally[?Yield, ?Await, ?Return]
        //     "try" Block[?Yield, ?Await, ?Return] Catch[?Yield, ?Await, ?Return] Finally[?Yield, ?Await, ?Return]
        // Catch[Yield, Await, Return]:
        //     "catch" "(" CatchParameter[?Yield, ?Await] ")" Block[?Yield, ?Await, ?Return]
        //     "catch" Block[?Yield, ?Await, ?Return]
        // Finally[Yield, Await, Return]:
        //     "finally" Block[?Yield, ?Await, ?Return]
        // CatchParameter[Yield, Await]:
        //     BindingIdentifier[?Yield, ?Await]
        //     BindingPattern[?Yield, ?Await]
        // Block[Yield, Await, Return]:
        //     "{" StatementList[?Yield, ?Await, ?Return](opt) "}"
        self.start_span();
        self.consume_kw("try", LexGoal::RegExp)?;
        let block = Box::from(self.parse_block_stmt()?);
        let handler = self.parse_catch_clause()?;
        let finalizer = self.parse_finally_clause()?;
        Ok(Node::TryStatement {
            block,
            handler,
            finalizer,
            span: self.end_span(),
        })
    }

    fn parse_catch_clause(&mut self) -> Result<Option<CatchClause>> {
        match self.current.tokentype {
            TokenType::Keyword if self.current.matches_str("catch") => {
                self.advance(LexGoal::RegExp)?;
                let param = match self.current.tokentype {
                    TokenType::Punctuator if self.current.matches_punc("(") => {
                        self.advance(LexGoal::RegExp)?;
                        let binding = self.parse_binding_id_or_pat()?;
                        self.consume(")", LexGoal::RegExp)?;
                        Some(binding)
                    }
                    _ => None,
                };
                let body = Box::from(self.parse_block_stmt()?);
                Ok(Some(CatchClause { param, body }))
            }
            TokenType::Keyword if self.current.matches_str("finally") => Ok(None),
            _ => self.unexpected_current(),
        }
    }

    fn parse_finally_clause(&mut self) -> Result<Option<Box<Node>>> {
        // Finally[Yield, Await, Return]:
        //     "finally" Block[?Yield, ?Await, ?Return]
        match self.current.tokentype {
            TokenType::Keyword if self.current.matches_str("finally") => {
                self.advance(LexGoal::RegExp)?;
                Ok(Some(Box::from(self.parse_block_stmt()?)))
            }
            _ => Ok(None),
        }
    }

    pub fn parse_debugger_stmt(&mut self) -> Result<Node> {
        log!("parse_debugger_stmt");
        self.start_span();
        self.consume_kw("debugger", LexGoal::RegExp)?;
        self.consume_semicolon(LexGoal::RegExp)?;
        Ok(Node::DebuggerStatement {
            span: self.end_span(),
        })
    }

    pub fn parse_hoistable_dclr(&mut self) -> Result<Node> {
        log!("parse_hoistable_dclr");
        // HoistableDeclaration[Yield, Await, Default]:
        //     FunctionDeclaration[?Yield, ?Await, ?Default]
        //     GeneratorDeclaration[?Yield, ?Await, ?Default]
        //     AsyncFunctionDeclaration[?Yield, ?Await, ?Default]
        //     AsyncGeneratorDeclaration[?Yield, ?Await, ?Default]
        match self.current.tokentype {
            TokenType::Identifier if self.current.matches_str("async") => {
                self.start_span();
                self.advance(LexGoal::RegExp)?;
                self.parse_function_dclr(true, true)
            }
            TokenType::Keyword if self.current.matches_str("function") => {
                self.parse_function_dclr(false, true)
            }
            _ => self.unexpected_current(),
        }
    }

    pub fn parse_function_dclr(&mut self, r#async: bool, allow_generator: bool) -> Result<Node> {
        log!("parse_function_dclr");
        // FunctionDeclaration[Yield, Await, Default]:
        //     "function" BindingIdentifier[?Yield, ?Await] "(" FormalParameters[~Yield, ~Await] ")" "{" FunctionBody[~Yield, ~Await] "}"
        //     [+Default] "function" "(" FormalParameters[~Yield, ~Await] ")" "{ "FunctionBody[~Yield, ~Await] "}"
        // UniqueFormalParameters[Yield, Await]:
        //     FormalParameters[?Yield, ?Await]
        // FormalParameters[Yield, Await]:
        //     [empty]
        //     FunctionRestParameter[?Yield, ?Await]
        //     FormalParameterList[?Yield, ?Await]
        //     FormalParameterList[?Yield, ?Await] ","
        //     FormalParameterList[?Yield, ?Await] "," FunctionRestParameter[?Yield, ?Await]
        // FormalParameterList[Yield, Await]:
        //     FormalParameter[?Yield, ?Await]
        //     FormalParameterList[?Yield, ?Await] "," FormalParameter[?Yield, ?Await]
        // FunctionRestParameter[Yield, Await]:
        //     BindingRestElement[?Yield, ?Await]
        // FormalParameter[Yield, Await]:
        //     BindingElement[?Yield, ?Await]
        // AsyncFunctionDeclaration[Yield, Await, Default]:
        //     "async" [no LineTerminator here] "function" BindingIdentifier[?Yield, ?Await]
        //       "(" FormalParameters[~Yield, +Await] ")" "{" AsyncFunctionBody "}"
        //     [+Default] "async" [no LineTerminator here] "function" "(" FormalParameters[~Yield, +Await] ")" "{" AsyncFunctionBody "}"
        // GeneratorDeclaration[Yield, Await, Default]:
        //     "function" "*" BindingIdentifier[?Yield, ?Await] "(" FormalParameters[+Yield, ~Await] ")" "{" GeneratorBody "}"
        //     [+Default] "function" "*" "(" FormalParameters[+Yield, ~Await] ")" "{" GeneratorBody "}"
        // AsyncGeneratorDeclaration[Yield, Await, Default]:
        //     "async" [no LineTerminator here] "function" "*" BindingIdentifier[?Yield, ?Await]
        //       "(" FormalParameters[+Yield, +Await] ")" "{" AsyncGeneratorBody "}"
        //     [+Default] "async" [no LineTerminator here] "function" "*" "(" FormalParameters[+Yield, +Await] ")" "{" AsyncGeneratorBody "}"
        if r#async && !self.has_line_terminator {
            return self.unexpected_current();
        }
        if !r#async {
            self.start_span();
        }
        self.consume_kw("function", LexGoal::RegExp)?;
        let generator = match self.current.tokentype {
            TokenType::Punctuator if self.current.matches_punc("*") => {
                if !allow_generator {
                    return self.error_current(ErrorType::GeneratorInSingleStatementContext);
                }
                self.advance(LexGoal::RegExp)?;
                true
            }
            _ => false,
        };
        let id = match self.current.tokentype {
            TokenType::Punctuator if self.current.matches_punc("(") => {
                if !self.params.has_default {
                    self.error_current(ErrorType::MissingFunctionName)
                } else {
                    Ok(None)
                }
            }
            _ => match self.parse_binding_id()? {
                Binding::BindingIdentifier(BindingIdentifier { name, span }) => {
                    Ok(Some(Box::from(Node::BindingIdentifier { name, span })))
                }
                _ => self.unexpected_current(),
            },
        }?;

        let params = self.with_params(
            Params {
                has_yield: generator,
                has_await: r#async,
                ..self.params
            },
            &mut Self::parse_formal_params,
        )?;

        let body = self.with_params(
            Params {
                has_yield: generator,
                has_await: r#async,
                ..self.params
            },
            &mut Self::parse_function_body,
        )?;
        Ok(Node::FunctionDeclaration {
            id,
            params,
            body: Box::from(body),
            generator,
            expression: false,
            r#async,
            span: self.end_span(),
        })
    }

    pub fn parse_function_body(&mut self) -> Result<Node> {
        let prev_in_function = self.ctx.in_function;
        self.ctx.in_function = true;
        let res = self.with_return(true, &mut Self::parse_function_body_block);
        self.ctx.in_function = prev_in_function;
        res
    }

    fn parse_function_body_block(&mut self) -> Result<Node> {
        self.start_span();
        self.consume("{", LexGoal::RegExp)?;
        let prev_strict = self.ctx.strict;
        let mut body = self.parse_directive_prologues()?;

        let prev_in_iteration = self.ctx.in_iteration;
        body.extend(self.parse_statement_list()?);
        self.ctx.in_iteration = prev_in_iteration;

        self.consume("}", LexGoal::RegExp)?;
        self.ctx.strict = prev_strict;
        Ok(Node::BlockStatement {
            body,
            span: self.end_span(),
        })
    }

    pub fn parse_class_dclr(&mut self) -> Result<Node> {
        log!("parse_class_dclr");
        // ClassDeclaration[Yield, Await, Default]:
        //     "class" BindingIdentifier[?Yield, ?Await] ClassTail[?Yield, ?Await]
        //     [+Default] "class" ClassTail[?Yield, ?Await]
        // TODO: strict mode
        self.start_span();
        self.consume_kw("class", LexGoal::RegExp)?;
        let id = match self.parse_binding_id()? {
            Binding::BindingIdentifier(BindingIdentifier { name, span }) => {
                Ok(Node::BindingIdentifier { name, span })
            }
            _ => unreachable!(),
        }?;
        self.parse_class_tail(Some(Box::from(id)))
    }

    fn parse_class_tail(&mut self, id: Option<Box<Node>>) -> Result<Node> {
        // ClassTail[Yield, Await]:
        //     ClassHeritage[?Yield, ?Await](opt) "{" ClassBody[?Yield, ?Await](opt) "}"
        // ClassHeritage[Yield, Await]:
        //     "extends" LeftHandSideExpression[?Yield, ?Await]
        // ClassDeclaration {
        //     id: Option<Box<Node>>, // Identifier | null
        //     #[serde(rename = "superClass")]
        //     super_class: Option<Box<Node>>, // Expression | null
        //     body: ClassBody,
        // }
        let super_class = match self.current.tokentype {
            TokenType::Keyword if self.current.matches_str("extends") => {
                self.advance(LexGoal::RegExp)?;
                Some(Box::from(self.parse_left_hand_side_expr()?))
            }
            _ => None,
        };
        self.consume("{", LexGoal::RegExp)?;
        let body = self.parse_class_body()?;
        self.consume("}", LexGoal::RegExp)?;
        Ok(Node::ClassDeclaration {
            id,
            super_class,
            body,
            span: self.end_span(),
        })
    }

    fn parse_class_body(&mut self) -> Result<ClassBody> {
        // ClassBody[Yield, Await]:
        //     ClassElementList[?Yield, ?Await]
        // ClassElementList[Yield, Await]:
        //     ClassElement[?Yield, ?Await]
        //     ClassElementList[?Yield, ?Await] ClassElement[?Yield, ?Await]
        // ClassElement[Yield, Await]:
        //     MethodDefinition[?Yield, ?Await]
        //     "static" MethodDefinition[?Yield, ?Await]
        //     ";"
        let body_with_empty =
            self.parse_until_punc("}", &mut |this| match this.current.tokentype {
                TokenType::Punctuator if this.current.matches_punc(";") => {
                    this.advance(LexGoal::RegExp)?;
                    Ok(None)
                }
                _ => {
                    let r#static = match this.current.tokentype {
                        TokenType::Keyword if this.current.matches_str("static") => {
                            this.advance(LexGoal::RegExp)?;
                            true
                        }
                        _ => false,
                    };
                    match this.current.tokentype {
                        TokenType::Identifier if this.current.matches_str("get") => {
                            this.advance(LexGoal::RegExp)?;
                            let (key, computed) = this.parse_property_name()?;
                            this.consume("(", LexGoal::RegExp)?;
                            this.consume(")", LexGoal::RegExp)?;
                            let body = Box::from(this.with_params(
                                Params {
                                    has_yield: false,
                                    has_await: false,
                                    ..this.params
                                },
                                &mut Self::parse_function_body,
                            )?);
                            let value = FunctionExpression {
                                id: None,
                                params: vec![],
                                body,
                                generator: false,
                                expression: false,
                                r#async: false,
                            };
                            Ok(Some(MethodDefinition {
                                key: key.as_node(),
                                computed,
                                value: Some(value),
                                kind: "get",
                                r#static,
                            }))
                        }
                        TokenType::Identifier if this.current.matches_str("set") => {
                            this.advance(LexGoal::RegExp)?;
                            let (key, computed) = this.parse_property_name()?;
                            let params = this.with_params(
                                Params {
                                    has_yield: false,
                                    has_await: false,
                                    ..this.params
                                },
                                &mut Self::parse_formal_params,
                            )?;
                            let body = Box::from(this.with_params(
                                Params {
                                    has_yield: false,
                                    has_await: false,
                                    ..this.params
                                },
                                &mut Self::parse_function_body,
                            )?);
                            let value = FunctionExpression {
                                id: None,
                                params,
                                body,
                                generator: false,
                                expression: false,
                                r#async: false,
                            };
                            Ok(Some(MethodDefinition {
                                key: key.as_node(),
                                computed,
                                value: Some(value),
                                kind: "set",
                                r#static,
                            }))
                        }
                        TokenType::Identifier if this.current.matches_str("async") => {
                            this.advance(LexGoal::RegExp)?;
                            let r#async = true;
                            let generator = match this.current.tokentype {
                                TokenType::Punctuator
                                    if !this.has_line_terminator
                                        && this.current.matches_punc("*") =>
                                {
                                    this.advance(LexGoal::RegExp)?;
                                    true
                                }
                                _ => false,
                            };
                            if !generator && !this.has_line_terminator {
                                return this.unexpected_current();
                            }
                            let (key, computed) = this.parse_property_name()?;
                            let params = this.with_params(
                                Params {
                                    has_yield: generator,
                                    has_await: r#async,
                                    ..this.params
                                },
                                &mut Self::parse_formal_params,
                            )?;
                            let body = Box::from(this.with_params(
                                Params {
                                    has_yield: generator,
                                    has_await: r#async,
                                    ..this.params
                                },
                                &mut Self::parse_function_body,
                            )?);
                            let value = FunctionExpression {
                                id: None,
                                params,
                                body,
                                generator,
                                expression: false,
                                r#async,
                            };
                            Ok(Some(MethodDefinition {
                                key: key.as_node(),
                                computed,
                                value: Some(value),
                                kind: "method",
                                r#static,
                            }))
                        }
                        TokenType::Punctuator if this.current.matches_punc("*") => {
                            this.advance(LexGoal::RegExp)?;
                            let r#async = false;
                            let generator = true;
                            let (key, computed) = this.parse_property_name()?;
                            let params = this.with_params(
                                Params {
                                    has_yield: generator,
                                    has_await: r#async,
                                    ..this.params
                                },
                                &mut Self::parse_formal_params,
                            )?;
                            let body = Box::from(this.with_params(
                                Params {
                                    has_yield: generator,
                                    has_await: r#async,
                                    ..this.params
                                },
                                &mut Self::parse_function_body,
                            )?);
                            let value = FunctionExpression {
                                id: None,
                                params,
                                body,
                                generator,
                                expression: false,
                                r#async,
                            };
                            Ok(Some(MethodDefinition {
                                key: key.as_node(),
                                computed,
                                value: Some(value),
                                kind: "method",
                                r#static,
                            }))
                        }
                        _ => {
                            let r#async = false;
                            let generator = false;
                            let (key, computed) = this.parse_property_name()?;
                            let kind = match &key {
                                PropertyKey::Identifier(Identifier { name, .. }) => {
                                    if name == &String::from("constructor") {
                                        "constructor"
                                    } else {
                                        "method"
                                    }
                                }
                                PropertyKey::Literal(Literal { raw, .. }) => {
                                    if raw == &String::from("constructor") {
                                        "constructor"
                                    } else {
                                        "method"
                                    }
                                }
                                _ => "method",
                            };
                            let params = this.with_params(
                                Params {
                                    has_yield: generator,
                                    has_await: r#async,
                                    ..this.params
                                },
                                &mut Self::parse_formal_params,
                            )?;
                            let body = Box::from(this.with_params(
                                Params {
                                    has_yield: generator,
                                    has_await: r#async,
                                    ..this.params
                                },
                                &mut Self::parse_function_body,
                            )?);
                            let value = FunctionExpression {
                                id: None,
                                params,
                                body,
                                generator,
                                expression: false,
                                r#async,
                            };
                            Ok(Some(MethodDefinition {
                                key: key.as_node(),
                                computed,
                                value: Some(value),
                                kind,
                                r#static,
                            }))
                        }
                    }
                }
            })?;
        let mut body = vec![];
        for item in body_with_empty.into_iter() {
            if let Some(method) = item {
                body.push(method);
            }
        }
        Ok(ClassBody { body })
    }

    pub fn parse_lexical_dclr(&mut self) -> Result<Node> {
        log!("parse_lexical_dclr");
        // VariableDeclaration {
        //     declarations: Vec<VariableDeclarator>,
        //     kind: &'static str,
        // },
        self.start_span();
        let kind = match self.current.tokentype {
            TokenType::Identifier => {
                self.consume_id("let", LexGoal::RegExp)?;
                Ok("let")
            }
            TokenType::Keyword => {
                self.consume_kw("const", LexGoal::RegExp)?;
                Ok("const")
            }
            _ => self.unexpected_current(),
        }?;
        let mut declarations = vec![self.parse_variable_dclr()?];
        declarations.extend(self.parse_while_punc(",", &mut |this| {
            this.consume(",", LexGoal::RegExp)?;
            this.parse_variable_dclr()
        })?);
        self.consume_semicolon(LexGoal::RegExp)?;
        Ok(Node::VariableDeclaration {
            declarations,
            kind,
            span: self.end_span(),
        })
    }

    // *******************************************************************
    //      Expressions
    // *******************************************************************

    pub fn parse_expression(&mut self) -> Result<Node> {
        log!("parse_expression");
        // Expression[In, Yield, Await]:
        //     AssignmentExpression[?In, ?Yield, ?Await]
        //     Expression[?In, ?Yield, ?Await],AssignmentExpression[?In, ?Yield, ?Await]
        self.start_span();
        let expr = self.parse_assignment_expr()?;
        if self.current.matches_punc(",") {
            let mut expressions = vec![expr];
            expressions.extend(self.parse_while_punc(",", &mut |this| {
                this.consume(",", LexGoal::RegExp)?;
                this.parse_assignment_expr()
            })?);
            let assign_target_type = if expressions.len() == 1 {
                "simple"
            } else {
                "invalid"
            };
            Ok(Node::SequenceExpression {
                expressions,
                assign_target_type,
                span: self.end_span(),
            })
        } else {
            Ok(expr)
        }
    }

    pub fn parse_assignment_expr(&mut self) -> Result<Node> {
        log!("parse_assignment_expr");
        // AssignmentExpression[In, Yield, Await]:
        //     [+Yield] YieldExpression[?In, ?Await]
        //     ConditionalExpression[?In, ?Yield, ?Await]
        //     ArrowFunction[?In, ?Yield, ?Await]
        //     AsyncArrowFunction[?In, ?Yield, ?Await]
        //     LeftHandSideExpression[?Yield, ?Await] "=" AssignmentExpression[?In, ?Yield, ?Await]
        //     LeftHandSideExpression[?Yield, ?Await] AssignmentOperator AssignmentExpression[?In, ?Yield, ?Await]
        if self.params.has_yield && self.current.matches_str("yield") {
            return self.parse_yield_expr();
        }
        self.start_span();
        let mut lhs = self.parse_conditional_expr()?;
        // TODO: store row and col

        if let Node::Identifier { name, .. } = &lhs {
            match self.current.tokentype {
                TokenType::Punctuator if self.current.matches_punc("=>") => {
                    lhs = self.parse_single_param_arrow_function_expr(name.clone())?;
                }
                _ => (),
            }
        }

        match self.current.tokentype {
            TokenType::Punctuator if self.current.matches_punc("=") => {
                let token = self.advance(LexGoal::RegExp)?;
                lhs = lhs.to_assignment_pattern()?;
                if lhs.assign_target_type() != "simple" {
                    // TODO: use correct row and col
                    return self.error(token, ErrorType::InvalidLhsInAssignment);
                }
                let rhs = self.parse_assignment_expr()?;
                Ok(Node::AssignmentExpression {
                    operator: "=",
                    left: Box::from(lhs),
                    right: Box::from(rhs),
                    assign_target_type: "invalid",
                    span: self.end_span(),
                })
            }
            TokenType::Punctuator if self.current.is_assign_op() => {
                let token = self.advance(LexGoal::RegExp)?;
                let operator = token.value.clone().punc().unwrap().to_str();
                if lhs.assign_target_type() != "simple" {
                    // TODO: use correct row and col
                    return self.error(token, ErrorType::InvalidLhsInAssignment);
                }
                let rhs = self.parse_assignment_expr()?;
                Ok(Node::AssignmentExpression {
                    operator,
                    left: Box::from(lhs),
                    right: Box::from(rhs),
                    assign_target_type: "invalid",
                    span: self.end_span(),
                })
            }
            _ => {
                self.end_span();
                Ok(lhs)
            }
        }
    }

    pub fn parse_yield_expr(&mut self) -> Result<Node> {
        // YieldExpression[In, Await]:
        //     "yield"
        //     "yield" `no LineTerminator here` AssignmentExpression[+Yield]
        //     yield `no LineTerminator here` "*" AssignmentExpression[+Yield]
        // YieldExpression {
        //     argument: Option<Box<Node>>
        //     delegate: bool,
        // },
        log!("parse_yield_expr");
        self.start_span();
        self.consume_kw("yield", LexGoal::RegExp)?;
        if !self.has_line_terminator {
            match self.current.tokentype {
                TokenType::Punctuator if self.current.matches_punc("*") => {
                    self.advance(LexGoal::RegExp)?;
                    Ok(Node::YieldExpression {
                        argument: Some(Box::from(
                            self.with_yield(true, &mut Self::parse_assignment_expr)?,
                        )),
                        delegate: true,
                        assign_target_type: "invalid",
                        span: self.end_span(),
                    })
                }
                TokenType::Punctuator if self.current.matches_punc("=>") => {
                    self.parse_single_param_arrow_function_expr(String::from("yield"))
                }
                _ => Ok(Node::YieldExpression {
                    argument: Some(Box::from(
                        self.with_yield(true, &mut Self::parse_assignment_expr)?,
                    )),
                    delegate: false,
                    assign_target_type: "invalid",
                    span: self.end_span(),
                }),
            }
        } else {
            Ok(Node::YieldExpression {
                argument: None,
                delegate: false,
                assign_target_type: "invalid",
                span: self.end_span(),
            })
        }
    }

    fn parse_single_param_arrow_function_expr(&mut self, name: String) -> Result<Node> {
        log!("parse_single_param_arrow_function_expr");
        self.consume("=>", LexGoal::RegExp)?;
        let (body, expression) = self.parse_arrow_body()?;
        Ok(Node::ArrowFunctionExpression {
            id: None,
            params: vec![FunctionParameter::BindingIdentifier { name }],
            body,
            generator: false,
            expression,
            r#async: false,
            assign_target_type: "invalid",
            span: self.end_span(),
        })
    }

    pub fn parse_conditional_expr(&mut self) -> Result<Node> {
        log!("parse_conditional_expr");
        // ConditionalExpression[In, Yield, Await]:
        //     LogicalORExpression[?In, ?Yield, ?Await]
        //     LogicalORExpression[?In, ?Yield, ?Await] ?
        //      AssignmentExpression[+In, ?Yield, ?Await] :
        //      AssignmentExpression[?In, ?Yield, ?Await]
        self.start_span();
        let expr = self.parse_binary_expr()?;
        if self.current.matches_punc("?") {
            self.consume("?", LexGoal::RegExp)?;
            let consequent = Box::from(self.with_in(true, &mut Self::parse_assignment_expr)?);
            self.consume(":", LexGoal::RegExp)?;
            let alternate = Box::from(self.parse_assignment_expr()?);
            Ok(Node::ConditionalExpression {
                test: Box::from(expr),
                consequent,
                alternate,
                span: self.end_span(),
            })
        } else {
            Ok(expr)
        }
    }

    pub fn parse_binary_expr(&mut self) -> Result<Node> {
        log!("parse_binary_expr");
        // ExponentiationExpression binop ExponentiationExpression
        self.start_span();
        let lhs = self.parse_exponentiation_expr()?;
        self.precedence_climbing(lhs, 0)
    }

    fn precedence_climbing(&mut self, mut lhs: Node, prec: usize) -> Result<Node> {
        while self.current.is_bin_op() && self.current.precedence() >= prec {
            let op = self.advance(LexGoal::RegExp)?;
            let mut rhs = self.parse_exponentiation_expr()?;
            while self.current.is_bin_op() && self.current.precedence() > op.precedence() {
                rhs = self.precedence_climbing(rhs, self.current.precedence())?;
            }
            lhs = Node::BinaryExpression {
                operator: op.to_string(),
                left: Box::from(lhs),
                right: Box::from(rhs),
                assign_target_type: "invalid",
                span: self.end_span(),
            };
            self.start_span();
        }
        self.end_span();
        Ok(lhs)
    }

    pub fn parse_exponentiation_expr(&mut self) -> Result<Node> {
        // ExponentiationExpression[Yield, Await]:
        //     UnaryExpression[?Yield, ?Await]
        //     UpdateExpression[?Yield, ?Await] ** ExponentiationExpression[?Yield, ?Await]
        log!("parse_exponentiation_expr");
        if self.current.is_unary_op() {
            self.parse_unary_expr()
        } else {
            self.start_span();
            let lhs = self.parse_update_expr()?;
            match self.current.tokentype {
                TokenType::Punctuator if self.current.matches_punc("**") => {
                    let operator = self.consume("**", LexGoal::RegExp)?.to_string();
                    let rhs = self.parse_exponentiation_expr()?;
                    Ok(Node::BinaryExpression {
                        operator,
                        left: Box::from(lhs),
                        right: Box::from(rhs),
                        assign_target_type: "invalid",
                        span: self.end_span(),
                    })
                }
                _ => {
                    self.end_span();
                    Ok(lhs)
                }
            }
        }
    }

    pub fn parse_unary_expr(&mut self) -> Result<Node> {
        log!("parse_unary_expr");
        // UnaryExpression[Yield, Await]:
        //     UpdateExpression[?Yield, ?Await]
        //     delete UnaryExpression[?Yield, ?Await]
        //     void UnaryExpression[?Yield, ?Await]
        //     typeof UnaryExpression[?Yield, ?Await]
        //     + UnaryExpression[?Yield, ?Await]
        //     - UnaryExpression[?Yield, ?Await]
        //     ~ UnaryExpression[?Yield, ?Await]
        //     ! UnaryExpression[?Yield, ?Await]
        //     [+Await] AwaitExpression[?Yield]  TODO
        match self.current.tokentype {
            TokenType::Punctuator | TokenType::Keyword if self.current.is_unary_op() => {
                self.start_span();
                Ok(Node::UnaryExpression {
                    operator: self.advance(LexGoal::RegExp)?.to_string(),
                    argument: Box::new(self.parse_unary_expr()?),
                    prefix: true,
                    assign_target_type: "invalid",
                    span: self.end_span(),
                })
            }
            _ => self.parse_update_expr(),
        }
    }

    pub fn parse_update_expr(&mut self) -> Result<Node> {
        log!("parse_update_expr");
        if self.current.is_update_op() {
            self.start_span();
            let operator = self.advance(LexGoal::RegExp)?.to_string();
            let (line, col) = (self.current.line_num, self.current.line_start);
            let expr = self.parse_unary_expr()?;
            if expr.assign_target_type() == "invalid" || expr.assign_target_type() == "strict" {
                return Err(Error {
                    line,
                    col,
                    errortype: ErrorType::InvalidLhsInPrefixOp,
                });
            }
            Ok(Node::UpdateExpression {
                operator,
                argument: Box::from(expr),
                prefix: true,
                assign_target_type: "invalid",
                span: self.end_span(),
            })
        } else {
            self.start_span();
            let (line, col) = (self.current.line_num, self.current.line_start);
            let expr = self.parse_left_hand_side_expr()?;
            if !self.has_line_terminator && self.current.is_update_op() {
                if expr.assign_target_type() == "invalid" || expr.assign_target_type() == "strict" {
                    return Err(Error {
                        line,
                        col,
                        errortype: ErrorType::InvalidLhsInPostfixOp,
                    });
                }
                Ok(Node::UpdateExpression {
                    operator: self.advance(LexGoal::RegExp)?.to_string(),
                    argument: Box::from(expr),
                    prefix: false,
                    assign_target_type: "invalid",
                    span: self.end_span(),
                })
            } else {
                Ok(expr)
            }
        }
    }

    pub fn parse_left_hand_side_expr(&mut self) -> Result<Node> {
        log!("parse_left_hand_side_expr");
        match self.current.tokentype {
            TokenType::Keyword if self.current.matches_str("new") => self.parse_new_expr(),
            _ => self.parse_call_expr(),
        }
    }

    pub fn parse_call_expr(&mut self) -> Result<Node> {
        log!("parse_call_expr");
        // CallExpression:
        //     (CoverCallExpressionAndAsyncArrowHead | "super" Arguments)
        //         (Arguments | "[" Expression[+In] "]" | "." IdentifierName | TemplateLiteral[+Tagged])*
        self.start_span();
        let callee = match self.current.tokentype {
            TokenType::Keyword if self.current.matches_str("super") => {
                let object = self.parse_super()?;
                match self.current.tokentype {
                    TokenType::Punctuator if self.current.matches_punc("(") => {
                        Ok(Node::CallExpression {
                            callee: Box::from(object),
                            arguments: self.parse_arguments()?,
                            assign_target_type: "invalid",
                            span: self.peek_span(),
                        })
                    }
                    TokenType::Punctuator if self.current.matches_punc("[") => {
                        self.parse_computed_member_expr(object)
                    }
                    TokenType::Punctuator if self.current.matches_punc(".") => {
                        self.parse_static_member_expr(object)
                    }
                    _ => self.unexpected_current(),
                }
            }
            _ => self.parse_member_expr(),
        }?;
        self.parse_call_expr_tail(callee)
    }

    pub fn parse_call_expr_tail(&mut self, mut callee: Node) -> Result<Node> {
        loop {
            match self.current.tokentype {
                TokenType::Punctuator if self.current.matches_punc("(") => {
                    callee = Node::CallExpression {
                        callee: Box::from(callee),
                        arguments: self.parse_arguments()?,
                        assign_target_type: "invalid",
                        span: self.peek_span(),
                    };
                }
                TokenType::Punctuator if self.current.matches_punc("[") => {
                    callee = self.parse_computed_member_expr(callee)?;
                }
                TokenType::Punctuator if self.current.matches_punc(".") => {
                    callee = self.parse_static_member_expr(callee)?;
                }
                TokenType::Template if self.current.head == Some(true) => {
                    match self.parse_template_literal()? {
                        Node::TemplateLiteral {
                            quasis,
                            expressions,
                            span,
                            ..
                        } => {
                            callee = Node::TaggedTemplateExpression {
                                tag: Box::from(callee),
                                quasi: TemplateLiteral {
                                    quasis,
                                    expressions,
                                },
                                span,
                            };
                        }
                        _ => unreachable!(),
                    }
                }
                _ => break,
            }
        }
        self.end_span();
        Ok(callee)
    }

    pub fn parse_member_expr(&mut self) -> Result<Node> {
        log!("parse_member_expr");
        // MemberExpression:
        //     (PrimaryExpression | SuperProperty)
        //         ("[" Expression[+In] "]" | "." IdentifierName | TemplateLiteral[+Tagged])*
        //     "new" MemberExpression Arguments
        // SuperProperty[Yield, Await]:
        //     "super" "[" Expression[+In] "]"
        //     super "." IdentifierName
        let mut expr = match self.current.tokentype {
            TokenType::Keyword if self.current.matches_str("super") => {
                let object = self.parse_super()?;
                match self.current.tokentype {
                    TokenType::Punctuator if self.current.matches_punc("[") => {
                        self.parse_computed_member_expr(object)
                    }
                    TokenType::Punctuator if self.current.matches_punc(".") => {
                        self.parse_static_member_expr(object)
                    }
                    _ => self.unexpected_current(),
                }
            }
            _ => self.parse_primary_expr(),
        }?;
        loop {
            match self.current.tokentype {
                TokenType::Punctuator if self.current.matches_punc("[") => {
                    expr = self.parse_computed_member_expr(expr)?
                }
                TokenType::Punctuator if self.current.matches_punc(".") => {
                    expr = self.parse_static_member_expr(expr)?
                }
                TokenType::Template if self.current.head == Some(true) => {
                    match self.parse_template_literal()? {
                        Node::TemplateLiteral {
                            quasis,
                            expressions,
                            span,
                            ..
                        } => {
                            expr = Node::TaggedTemplateExpression {
                                tag: Box::from(expr),
                                quasi: TemplateLiteral {
                                    quasis,
                                    expressions,
                                },
                                span,
                            };
                        }
                        _ => unreachable!(),
                    }
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    fn parse_computed_member_expr(&mut self, object: Node) -> Result<Node> {
        log!("parse_computed_member_expr");
        self.start_span();
        self.consume("[", LexGoal::RegExp)?;
        let member_expr = Node::ComputedMemberExpression {
            computed: true,
            object: Box::from(object),
            property: Box::from(self.with_in(true, &mut Self::parse_expression)?),
            assign_target_type: "simple",
            span: self.end_span(),
        };
        self.consume("]", LexGoal::Div)?;
        Ok(member_expr)
    }

    fn parse_static_member_expr(&mut self, object: Node) -> Result<Node> {
        log!("parse_static_member_expr");
        self.start_span();
        self.consume(".", LexGoal::RegExp)?;
        self.start_span();
        let name = self.parse_id_name(LexGoal::Div)?;
        let property = Box::from(Node::Identifier {
            name,
            assign_target_type: "invalid",
            span: self.end_span(),
        });
        Ok(Node::StaticMemberExpression {
            computed: false,
            object: Box::from(object),
            property,
            assign_target_type: "simple",
            span: self.end_span(),
        })
    }

    pub fn parse_new_expr(&mut self) -> Result<Node> {
        log!("parse_new_expr");
        // LeftHandSideExpression:
        //     ("new")* MemberExpression
        //     (PrimaryExpression | SuperProperty | new "." target)
        //         ("[" Expression[+In] "]" | "." IdentifierName | TemplateLiteral[+Tagged])*
        //     "new" MemberExpression Arguments
        //     (CoverCallExpressionAndAsyncArrowHead | "super" Arguments)
        //         (Arguments | "[" Expression[+In] "]" | "." IdentifierName | TemplateLiteral[+Tagged])*
        // SuperProperty[Yield, Await]:
        //     "super" "[" Expression[+In] "]"
        //     "super" "." IdentifierName
        // NewExpression {
        //     callee: Box<Node>, // Expression
        //     arguments: Vec<ArgumentListElement>,
        // },
        self.start_span();
        let id = self.consume_kw("new", LexGoal::RegExp)?;
        let span = self.peek_span();
        match self.current.tokentype {
            TokenType::Punctuator if self.current.matches_punc(".") => {
                self.advance(LexGoal::RegExp)?;
                match self.current.tokentype {
                    TokenType::Identifier if self.current.matches_str("target") => {
                        let property = Box::from(Node::Identifier {
                            name: self.advance(LexGoal::Div)?.value.string().unwrap(),
                            assign_target_type: "invalid",
                            span: span.clone(),
                        });
                        Ok(Node::MetaProperty {
                            meta: Box::from(Node::Identifier {
                                name: id.value.string().unwrap(),
                                assign_target_type: "invalid",
                                span,
                            }),
                            property,
                            assign_target_type: "invalid",
                            span: self.end_span(),
                        })
                    }
                    _ => self.unexpected_current(),
                }
            }
            TokenType::Keyword if self.current.matches_str("new") => Ok(Node::NewExpression {
                callee: Box::from(self.parse_new_expr()?),
                arguments: vec![],
                assign_target_type: "invalid",
                span: self.end_span(),
            }),
            _ => {
                let callee = Box::from(self.parse_member_expr()?);
                let arguments = match self.current.tokentype {
                    TokenType::Punctuator if self.current.matches_punc("(") => {
                        self.parse_arguments()?
                    }
                    _ => vec![],
                };
                let callee = Node::NewExpression {
                    callee,
                    arguments,
                    assign_target_type: "invalid",
                    span: self.end_span(),
                };
                self.parse_call_expr_tail(callee)
            }
        }
    }

    pub fn parse_super(&mut self) -> Result<Node> {
        log!("parse_super");
        self.start_span();
        self.consume_kw("super", LexGoal::RegExp)?;
        match self.current.tokentype {
            TokenType::Punctuator if self.current.matches_punc("[") => Ok(Node::Super {
                assign_target_type: "simple",
                span: self.end_span(),
            }),
            TokenType::Punctuator if self.current.matches_punc(".") => Ok(Node::Super {
                assign_target_type: "simple",
                span: self.end_span(),
            }),
            TokenType::Punctuator if self.current.matches_punc("(") => Ok(Node::Super {
                assign_target_type: "simple",
                span: self.end_span(),
            }),
            _ => self.unexpected_current(),
        }
    }

    pub fn parse_primary_expr(&mut self) -> Result<Node> {
        log!("parse_primary_expr");
        // PrimaryExpression[Yield, Await]:
        //     FunctionExpression
        //     GeneratorExpression
        //     AsyncFunctionExpression
        //     AsyncGeneratorExpression
        //     ClassExpression[?Yield, ?Await]
        //     this
        //     IdentifierReference[?Yield, ?Await]
        //     Literal
        //     ArrayLiteral[?Yield, ?Await]
        //     ObjectLiteral[?Yield, ?Await]
        //     RegularExpressionLiteral
        //     TemplateLiteral[?Yield, ?Await, ~Tagged]
        //     CoverParenthesizedExpressionAndArrowParameterList[?Yield, ?Await]
        match &self.current.tokentype {
            TokenType::Keyword if self.current.matches_str("function") => {
                self.parse_function_expr(false)
            }
            TokenType::Identifier if self.current.matches_str("async") => {
                self.parse_async_primary_expr()
            }
            TokenType::Keyword if self.current.matches_str("class") => self.parse_class_expr(),
            TokenType::Keyword if self.current.matches_str("this") => {
                self.start_span();
                self.advance(LexGoal::Div)?;
                Ok(Node::ThisExpression {
                    assign_target_type: "invalid",
                    span: self.end_span(),
                })
            }
            TokenType::Identifier | TokenType::Keyword => self.parse_id_reference(),
            TokenType::NumericLiteral
            | TokenType::StringLiteral
            | TokenType::BooleanLiteral
            | TokenType::NullLiteral => {
                self.start_span();
                let value = self.advance(LexGoal::Div)?.value;
                let raw = String::from(&value);
                Ok(Node::Literal {
                    value,
                    raw,
                    assign_target_type: "invalid",
                    span: self.end_span(),
                })
            }
            TokenType::Punctuator if self.current.matches_punc("[") => self.parse_array_lit_expr(),
            TokenType::Punctuator if self.current.matches_punc("{") => self.parse_object_lit_expr(),
            TokenType::RegularExpression => {
                self.start_span();
                let token = self.advance(LexGoal::Div)?;
                Ok(Node::RegexLiteral {
                    value: String::from(&self.current.value),
                    raw: String::from(&self.current.value),
                    regex: Regex {
                        pattern: token.pattern.unwrap(),
                        flags: token.flags.unwrap(),
                    },
                    assign_target_type: "invalid",
                    span: self.end_span(),
                })
            }
            TokenType::Template => self.parse_template_literal(),
            TokenType::Punctuator if self.current.matches_punc("(") => self.parse_seq_or_arrow(),
            _ => self.unexpected_current(),
        }
    }

    pub fn parse_async_call_or_arrow(&mut self, callee: Box<Node>) -> Result<Node> {
        log!("parse_async_call_or_arrow");
        let arguments = self.parse_arguments()?;
        match self.current.tokentype {
            TokenType::Punctuator if self.current.matches_punc("=>") => {
                self.advance(LexGoal::RegExp)?;
                let (body, expression) = self.parse_arrow_body()?;
                let params = self.arguments_to_params(arguments)?;
                Ok(Node::AsyncArrowFunctionExpression {
                    id: None,
                    params,
                    body,
                    generator: false,
                    expression,
                    r#async: true,
                    assign_target_type: "invalid",
                    span: self.end_span(),
                })
            }
            _ => Ok(Node::CallExpression {
                callee,
                arguments,
                assign_target_type: "invalid",
                span: self.end_span(),
            }),
        }
    }

    pub fn arguments_to_params(
        &mut self,
        arguments: Vec<ArgumentListElement>,
    ) -> Result<Vec<FunctionParameter>> {
        log!("arguments_to_params");
        let mut params = vec![];
        if arguments.is_empty() {
            return Ok(params);
        }
        let last_idx = arguments.len() - 1;
        for (i, argument) in arguments.into_iter().enumerate() {
            let param = match argument.as_node()?.to_binding_pattern()? {
                Node::AssignmentPattern { left, right, .. } => {
                    Ok(FunctionParameter::AssignmentPattern {
                        left,
                        right: *right,
                    })
                }
                Node::BindingIdentifier { name, .. } => {
                    Ok(FunctionParameter::BindingIdentifier { name })
                }
                Node::ObjectPattern { properties, .. } => {
                    Ok(FunctionParameter::ObjectPattern { properties })
                }
                Node::ArrayPattern { elements, .. } => {
                    Ok(FunctionParameter::ArrayPattern { elements })
                }
                Node::RestElement { argument, .. } => {
                    if i == last_idx {
                        Ok(FunctionParameter::RestElement { argument })
                    } else {
                        // TODO: fix row and col
                        self.error_current(ErrorType::ParamAfterRest)
                    }
                }
                _ => self.error_current(ErrorType::InvalidDestructuringTarget), // TODO: fix row and col
            }?;
            params.push(param);
        }
        Ok(params)
    }

    pub fn parse_arguments(&mut self) -> Result<Vec<ArgumentListElement>> {
        log!("parse_arguments");
        // Arguments[Yield, Await]:
        //     "(" ")"
        //     "(" ArgumentList[?Yield, ?Await] ")"
        //     "(" ArgumentList[?Yield, ?Await] "," ")"
        // ArgumentList[Yield, Await]:
        //     AssignmentExpression[+In, ?Yield, ?Await]
        //     "..." AssignmentExpression[+In, ?Yield, ?Await]
        //     ArgumentList[?Yield, ?Await] "," AssignmentExpression[+In, ?Yield, ?Await]
        //     ArgumentList[?Yield, ?Await] "," "..." AssignmentExpression[+In, ?Yield, ?Await]
        self.consume("(", LexGoal::RegExp)?;
        let arguments = self.parse_until_punc(")", &mut |this| match this.current.tokentype {
            TokenType::Punctuator if this.current.matches_punc("...") => {
                this.start_span();
                this.advance(LexGoal::RegExp)?;
                let argument = this.with_in(true, &mut Self::parse_assignment_expr)?;
                let res = ArgumentListElement::SpreadElement(SpreadElement {
                    argument,
                    span: this.end_span(),
                });
                if !this.current.matches_punc(")") {
                    this.consume(",", LexGoal::RegExp)?;
                }
                Ok(res)
            }
            _ => {
                let expr = this.with_in(true, &mut Self::parse_assignment_expr)?;
                if !this.current.matches_punc(")") {
                    this.consume(",", LexGoal::RegExp)?;
                }
                Ok(ArgumentListElement::Expression(expr))
            }
        })?;
        self.consume(")", LexGoal::Div)?;
        Ok(arguments)
    }

    pub fn parse_seq_or_arrow(&mut self) -> Result<Node> {
        log!("parse_seq_or_arrow");
        // CoverParenthesizedExpressionAndArrowParameterList[Yield, Await]:
        //     "(" Expression[+In, ?Yield, ?Await] ")"
        //     "(" Expression[+In, ?Yield, ?Await] "," ")"
        //     "(" ")"
        //     "(" "..." BindingIdentifier[?Yield, ?Await] ")"
        //     "(" "..." BindingPattern[?Yield, ?Await] ")"
        //     "(" Expression[+In, ?Yield, ?Await] "," "..." BindingIdentifier[?Yield, ?Await]")"
        //     "(" Expression[+In, ?Yield, ?Await] "," "..." BindingPattern[?Yield, ?Await] ")"
        self.start_span();
        self.consume("(", LexGoal::RegExp)?;
        let mut definitely_arrow = false;
        let expressions =
            self.parse_until_punc_or_rest(")", &mut |this| match this.current.tokentype {
                TokenType::Punctuator if this.current.matches_punc("...") => {
                    this.start_span();
                    this.advance(LexGoal::RegExp)?;
                    definitely_arrow = true;
                    Ok(Node::RestElement {
                        argument: this.parse_binding_id_or_pat()?,
                        span: this.end_span(),
                    })
                }
                _ => {
                    let expr = this.with_in(true, &mut Self::parse_assignment_expr)?;
                    if !this.current.matches_punc(")") {
                        this.consume(",", LexGoal::RegExp)?;
                        if this.current.matches_punc(")") {
                            definitely_arrow = true;
                        }
                    }
                    Ok(expr)
                }
            })?;
        self.consume(")", LexGoal::Div)?;
        let assign_target_type = if expressions.len() == 1 {
            (&expressions[0]).assign_target_type()
        } else {
            "invalid"
        };
        match (definitely_arrow, self.current.matches_punc("=>")) {
            (true, _) => self.seq_to_arrow(expressions),
            (false, true) => self.seq_to_arrow(expressions),
            (false, false) => Ok(Node::SequenceExpression {
                expressions,
                assign_target_type,
                span: self.end_span(),
            }),
        }
    }

    pub fn seq_to_arrow(&mut self, seq: Vec<Node>) -> Result<Node> {
        log!("seq_to_arrow");
        // ArrowParameterPlaceHolder {
        //     params: Vec<FunctionParameter>,
        //     r#async: bool,
        // }
        // pub enum FunctionParameter {
        //     AssignmentPattern { left: Binding, right: Node },
        //     BindingIdentifier { name: String },
        //     BindingPattern(BindingPattern),
        // }
        // pub enum BindingPattern {
        //     ArrayPattern {
        //         elements: Vec<Option<ArrayPatternElement>>,
        //     },
        //     ObjectPattern {
        //         properties: Vec<Option<ObjectPatternProperty>>,
        //     },
        // }
        let params = self.seq_to_arrow_params(seq)?;
        self.consume("=>", LexGoal::RegExp)?;
        let (body, expression) = self.parse_arrow_body()?;
        Ok(Node::ArrowFunctionExpression {
            id: None,
            params,
            body,
            generator: false,
            expression,
            r#async: false,
            assign_target_type: "invalid",
            span: self.end_span(),
        })
    }

    pub fn seq_to_arrow_params(&mut self, seq: Vec<Node>) -> Result<Vec<FunctionParameter>> {
        log!("seq_to_arrow_params");
        let mut params = vec![];
        if seq.is_empty() {
            return Ok(params);
        }
        let last_idx = seq.len() - 1;
        for (i, expr) in seq.into_iter().enumerate() {
            let param = match expr.to_binding_pattern()? {
                Node::AssignmentPattern { left, right, .. } => {
                    Ok(FunctionParameter::AssignmentPattern {
                        left,
                        right: *right,
                    })
                }
                Node::BindingIdentifier { name, .. } => {
                    Ok(FunctionParameter::BindingIdentifier { name })
                }
                Node::ObjectPattern { properties, .. } => {
                    Ok(FunctionParameter::ObjectPattern { properties })
                }
                Node::ArrayPattern { elements, .. } => {
                    Ok(FunctionParameter::ArrayPattern { elements })
                }
                Node::RestElement { argument, .. } => {
                    if i == last_idx {
                        Ok(FunctionParameter::RestElement { argument })
                    } else {
                        // TODO: fix row and col
                        self.error_current(ErrorType::ParamAfterRest)
                    }
                }
                _ => self.error_current(ErrorType::InvalidDestructuringTarget), // TODO: fix row and col
            }?;
            params.push(param);
        }
        Ok(params)
    }

    pub fn parse_binding_id_or_pat(&mut self) -> Result<Binding> {
        log!("parse_binding_id_or_pat");
        match self.current.tokentype {
            TokenType::Identifier | TokenType::Keyword => self.parse_binding_id(),
            TokenType::Punctuator if self.current.matches_punc("{") => self.parse_obj_binding_pat(),
            TokenType::Punctuator if self.current.matches_punc("[") => self.parse_arr_binding_pat(),
            _ => self.unexpected_current(),
        }
    }

    pub fn parse_obj_binding_pat(&mut self) -> Result<Binding> {
        log!("parse_obj_binding_pat");
        // ObjectBindingPattern[Yield, Await]:
        //     "{" "}"
        //     "{" BindingRestProperty "}"
        //     "{" BindingPropertyList "}"
        //     "{" BindingPropertyList "," BindingRestProperty(opt) "}"
        // BindingPropertyList[Yield, Await]:
        //     BindingProperty[?Yield, ?Await]
        //     BindingPropertyList[?Yield, ?Await] "," BindingProperty[?Yield, ?Await]
        // BindingRestProperty[Yield, Await]:
        //     "..." BindingIdentifier[?Yield, ?Await]
        // BindingProperty[Yield, Await]:
        //     SingleNameBinding[?Yield, ?Await]
        //     PropertyName[?Yield, ?Await] ":" BindingElement[?Yield, ?Await]
        // SingleNameBinding[Yield, Await]:
        //     BindingIdentifier[?Yield, ?Await] Initializer[+In, ?Yield, ?Await](opt)
        // PropertyName[Yield, Await]:
        //     LiteralPropertyName
        //     ComputedPropertyName[?Yield, ?Await]
        // Initializer[In, Yield, Await]:
        //     "=" AssignmentExpression[?In, ?Yield, ?Await]
        self.start_span();
        self.consume("{", LexGoal::RegExp)?;
        let properties = self.parse_until_punc_or_rest("}", &mut |this| match this
            .current
            .tokentype
        {
            TokenType::Punctuator if this.current.matches_punc("...") => {
                this.advance(LexGoal::RegExp)?;
                Ok(ObjectPatternProperty::RestElement {
                    argument: this.parse_binding_id_or_pat()?,
                })
            }
            _ => {
                this.start_span();
                let value = match this.current.tokentype {
                    TokenType::Identifier
                    | TokenType::Keyword
                    | TokenType::StringLiteral
                    | TokenType::NumericLiteral => {
                        Ok(ValueOrExpr::Value(this.advance(LexGoal::RegExp)?.value))
                    }
                    TokenType::Punctuator if this.current.matches_punc("[") => {
                        this.consume("[", LexGoal::RegExp)?;
                        let expr = this.with_in(true, &mut Self::parse_assignment_expr)?;
                        this.consume("]", LexGoal::RegExp)?;
                        Ok(ValueOrExpr::Expr(expr))
                    }
                    _ => this.unexpected_current(),
                }?;
                let span = this.end_span();
                let property = match this.current.tokentype {
                    TokenType::Punctuator if this.current.matches_punc("=") => {
                        this.advance(LexGoal::RegExp)?;
                        let name = match &value {
                            ValueOrExpr::Value(Value::Str(s)) => Ok(s.clone()),
                            _ => this.unexpected_current(),
                        }?;
                        let key = this.value_to_key(value, false, span.clone())?;
                        let obj_value = PropertyValue::AssignmentPattern(AssignmentPattern {
                            left: Binding::BindingIdentifier(BindingIdentifier { name, span }),
                            right: this.with_in(true, &mut Self::parse_assignment_expr)?,
                        });
                        Ok(ObjectPatternProperty::Property {
                            key,
                            computed: false,
                            value: Some(obj_value),
                            kind: "init",
                            method: false,
                            shorthand: true,
                        })
                    }
                    TokenType::Punctuator if this.current.matches_punc(":") => {
                        this.advance(LexGoal::RegExp)?;
                        let key = this.value_to_key(value, true, span)?;
                        let computed = match &key {
                            PropertyKey::Expression(_) => true,
                            _ => false,
                        };
                        let (line, col) = (this.current.line_num, this.current.line_start);
                        let pattern = this.parse_binding_id_or_pat()?;
                        let obj_value = match this.current.tokentype {
                            TokenType::Punctuator if this.current.matches_punc("=") => {
                                this.advance(LexGoal::RegExp)?;
                                Ok(PropertyValue::AssignmentPattern(AssignmentPattern {
                                    left: pattern,
                                    right: this.with_in(true, &mut Self::parse_assignment_expr)?,
                                }))
                            }
                            _ => match pattern {
                                Binding::BindingIdentifier(BindingIdentifier { name, span }) => {
                                    Ok(PropertyValue::BindingIdentifier(BindingIdentifier {
                                        name,
                                        span,
                                    }))
                                }
                                Binding::ArrayPattern(ArrayPattern { elements, .. }) => {
                                    Ok(PropertyValue::BindingPattern(
                                        BindingPattern::ArrayPattern { elements },
                                    ))
                                }
                                Binding::ObjectPattern(ObjectPattern { properties, .. }) => {
                                    Ok(PropertyValue::BindingPattern(
                                        BindingPattern::ObjectPattern { properties },
                                    ))
                                }
                                _ => {
                                    return Err(Error {
                                        line,
                                        col,
                                        errortype: ErrorType::InvalidDestructuringTarget,
                                    })
                                }
                            },
                        }?;
                        Ok(ObjectPatternProperty::Property {
                            key,
                            computed,
                            value: Some(obj_value),
                            kind: "init",
                            method: false,
                            shorthand: false,
                        })
                    }
                    _ => {
                        let key = this.value_to_key(value, false, span)?;
                        Ok(ObjectPatternProperty::Property {
                            key,
                            computed: false,
                            value: None,
                            kind: "init",
                            method: false,
                            shorthand: true,
                        })
                    }
                }?;
                if !this.current.matches_punc("}") {
                    this.consume(",", LexGoal::RegExp)?;
                }
                Ok(property)
            }
        })?;
        self.consume("}", LexGoal::RegExp)?;
        Ok(Binding::ObjectPattern(ObjectPattern {
            properties,
            span: self.end_span(),
        }))
    }

    pub fn value_to_key(
        &mut self,
        value: ValueOrExpr,
        literal: bool,
        span: Span,
    ) -> Result<PropertyKey> {
        match value {
            ValueOrExpr::Expr(expr) if literal => Ok(PropertyKey::Expression(expr)),
            ValueOrExpr::Value(value) => {
                if literal {
                    let raw = String::from(&value);
                    Ok(PropertyKey::Literal(Literal { value, raw, span }))
                } else {
                    let val = &String::from(&value)[..];
                    if self.scanner.is_keyword(val) {
                        if (!self.params.has_yield && val == "yield")
                            | (!self.params.has_await && val == "await")
                        {
                            Ok(PropertyKey::Identifier(Identifier {
                                name: String::from(val),
                                span,
                            }))
                        } else {
                            self.unexpected_current()
                        }
                    } else {
                        Ok(PropertyKey::Identifier(Identifier {
                            name: String::from(val),
                            span,
                        }))
                    }
                }
            }
            _ => panic!(),
        }
    }

    pub fn parse_arr_binding_pat(&mut self) -> Result<Binding> {
        log!("parse_arr_binding_pat");
        // ArrayBindingPattern[Yield, Await]:
        //     "[" Elision(opt) BindingRestElement(opt) "]"
        //     "[" BindingElementList "]"
        //     "[" BindingElementList "," Elision(opt) BindingRestElement(opt) "]"
        // BindingElementList[Yield, Await]:
        //     BindingElisionElement
        //     BindingElementList "," BindingElisionElement
        // BindingElement[Yield, Await]:
        //     SingleNameBinding
        //     BindingPattern Initializer[+In](opt)
        // SingleNameBinding[Yield, Await]:
        //     BindingIdentifier Initializer[+In](opt)
        // BindingElisionElement[Yield, Await]:
        //     Elision(opt) BindingElement
        // BindingRestElement[Yield, Await]:
        //     "..." BindingIdentifier
        //     "..." BindingPattern

        // ArrayBindingPattern:
        //     "[" (BindingElement ",")* (BindingElement | "..." BindingIdentifier | "..." BindingPattern)? "]"
        // BindingElement:
        //     (BindingIdentifier | BindingPattern) ("=" AssignmentExpression[+In])?
        //     ","
        //     EMPTY
        // BindingPattern:
        //     ObjectBindingPattern
        //     ArrayBindingPattern
        self.start_span();
        self.consume("[", LexGoal::RegExp)?;
        let elements =
            self.parse_until_punc_or_rest("]", &mut |this| match this.current.tokentype {
                TokenType::Punctuator if this.current.matches_punc(",") => {
                    this.advance(LexGoal::RegExp)?;
                    Ok(None)
                }
                TokenType::Punctuator if this.current.matches_punc("...") => {
                    this.advance(LexGoal::RegExp)?;
                    Ok(Some(ArrayPatternElement::RestElement {
                        argument: this.parse_binding_id_or_pat()?,
                    }))
                }
                _ => {
                    let (line, col) = (this.current.line_num, this.current.line_start);
                    let pattern = this.parse_binding_id_or_pat()?;
                    let element = match this.current.tokentype {
                        TokenType::Punctuator if this.current.matches_punc("=") => {
                            this.advance(LexGoal::RegExp)?;
                            ArrayPatternElement::AssignmentPattern {
                                left: pattern,
                                right: this.with_in(true, &mut Self::parse_assignment_expr)?,
                            }
                        }
                        _ => match pattern {
                            Binding::BindingIdentifier(BindingIdentifier { name, .. }) => {
                                ArrayPatternElement::BindingIdentifier { name }
                            }
                            Binding::ArrayPattern(ArrayPattern { elements, .. }) => {
                                ArrayPatternElement::ArrayPattern { elements }
                            }
                            Binding::ObjectPattern(ObjectPattern { properties, .. }) => {
                                ArrayPatternElement::ObjectPattern { properties }
                            }
                            _ => {
                                return Err(Error {
                                    line,
                                    col,
                                    errortype: ErrorType::InvalidDestructuringTarget,
                                })
                            }
                        },
                    };
                    if !this.current.matches_punc("]") {
                        this.consume(",", LexGoal::RegExp)?;
                    }
                    Ok(Some(element))
                }
            })?;
        self.consume("]", LexGoal::RegExp)?;
        Ok(Binding::ArrayPattern(ArrayPattern {
            elements,
            span: self.end_span(),
        }))
    }

    pub fn parse_template_literal(&mut self) -> Result<Node> {
        log!("parse_template_literal");
        let mut expressions = vec![];
        let mut quasis = vec![];
        self.start_span();
        let quasi = self.parse_template_head()?;
        let mut is_tail = quasi.tail;
        quasis.push(quasi);
        while !is_tail {
            expressions.push(self.with_in(true, &mut Self::parse_expression)?);
            let quasi = self.parse_template_element()?;
            is_tail = quasi.tail;
            quasis.push(quasi);
        }
        Ok(Node::TemplateLiteral {
            quasis,
            expressions,
            assign_target_type: "invalid",
            span: self.end_span(),
        })
    }

    fn parse_template_head(&mut self) -> Result<TemplateElement> {
        log!("parse_template_head");
        match self.current.tokentype {
            TokenType::Template => match &self.current.head {
                Some(true) => {
                    let token = self.advance(LexGoal::Div)?;
                    Ok(TemplateElement {
                        value: TemplateElementValue {
                            cooked: token.cooked.unwrap(),
                            raw: String::from(&token.value),
                        },
                        tail: token.tail.unwrap(),
                    })
                }
                _ => self.error_current(ErrorType::UnexpectedTemplateString),
            },
            _ => self.unexpected_current(),
        }
    }

    fn parse_template_element(&mut self) -> Result<TemplateElement> {
        log!("parse_template_element");
        match self.current.tokentype {
            TokenType::Template => {
                let token = self.advance(LexGoal::Div)?;
                Ok(TemplateElement {
                    value: TemplateElementValue {
                        cooked: token.cooked.unwrap(),
                        raw: String::from(&token.value),
                    },
                    tail: token.tail.unwrap(),
                })
            }
            _ => self.unexpected_current(),
        }
    }

    pub fn parse_class_expr(&mut self) -> Result<Node> {
        // ClassExpression[Yield, Await]:
        //     "class" BindingIdentifier[?Yield, ?Await](opt) ClassTail[?Yield, ?Await]
        // ClassTail[Yield, Await]:
        //     ClassHeritage[?Yield, ?Await](opt) "{" ClassBody[?Yield, ?Await](opt) "}"
        // ClassHeritage[Yield, Await]:
        //     "extends" LeftHandSideExpression[?Yield, ?Await]
        // ClassBody[Yield, Await]:
        //     ClassElementList[?Yield, ?Await]
        // ClassElementList[Yield, Await]:
        //     ClassElement[?Yield, ?Await]
        //     ClassElementList[?Yield, ?Await] ClassElement[?Yield, ?Await]
        // ClassElement[Yield, Await]:
        //     MethodDefinition[?Yield, ?Await]
        //     "static" MethodDefinition[?Yield, ?Await]
        //     ";"
        log!("parse_class_expr");
        self.start_span();
        self.consume_kw("class", LexGoal::RegExp)?;
        let id = match self.current.tokentype {
            TokenType::Identifier | TokenType::Keyword => match self.parse_binding_id()? {
                Binding::BindingIdentifier(BindingIdentifier { name, span }) => {
                    Some(Box::from(Node::BindingIdentifier { name, span }))
                }
                _ => unreachable!(),
            },
            _ => None,
        };
        self.parse_class_tail(id)
    }

    pub fn parse_function_expr(&mut self, r#async: bool) -> Result<Node> {
        log!("parse_function_expr");
        // FunctionExpression:
        //     "function" BindingIdentifier(opt)
        //         "(" FormalParameters ")" "{" FunctionBody "}"
        // GeneratorExpression:
        //     "function" "*" BindingIdentifier(opt)[+Yield]
        //         "(" FormalParameters[+Yield] ")" "{" GeneratorBody "}"
        // AsyncFunctionExpression:
        //     "async" `no LineTerminator here` "function"
        //         "(" FormalParameters[+Await] ")" "{" AsyncFunctionBody "}"
        //     "async" `no LineTerminator here` "function" BindingIdentifier[+Await]
        //         "(" FormalParameters[+Await] ")" "{" AsyncFunctionBody "}"
        // AsyncGeneratorExpression:
        //     "async" `no LineTerminator here` "function" "*" BindingIdentifier(opt)[+Yield, +Await]
        //         "(" FormalParameters[+Yield, +Await] ")" "{" AsyncGeneratorBody "}"

        // IMPORTANT: We want to maintain an invariant where if async = true, we assume we are
        // parsing something like `async function` and the span began at `async`. Maintain
        // this invariant at all costs!
        if !r#async {
            self.start_span();
        }
        self.consume_kw("function", LexGoal::RegExp)?;
        let generator = match self.current.tokentype {
            TokenType::Punctuator if self.current.matches_str("*") => true,
            _ => false,
        };
        let id = match self.current.tokentype {
            TokenType::Punctuator if self.current.matches_punc("(") => None,
            _ => Some(Box::from(self.parse_id_reference()?)),
        };
        Ok(Node::FunctionExpression {
            id,
            params: self.parse_formal_params()?,
            body: Box::from(self.parse_function_body()?),
            generator,
            expression: false,
            r#async,
            assign_target_type: "invalid",
            span: self.end_span(),
        })
    }

    pub fn parse_formal_params(&mut self) -> Result<Vec<FunctionParameter>> {
        log!("parse_formal_params");
        // FormalParameters[Yield, Await]:
        //     [empty]
        //     FunctionRestParameter[?Yield, ?Await]
        //     FormalParameterList[?Yield, ?Await]
        //     FormalParameterList[?Yield, ?Await] ","
        //     FormalParameterList[?Yield, ?Await] "," FunctionRestParameter[?Yield, ?Await]
        // FormalParameterList[Yield, Await]:
        //     FormalParameter[?Yield, ?Await]
        //     FormalParameterList[?Yield, ?Await] "," FormalParameter[?Yield, ?Await]
        // FunctionRestParameter[Yield, Await]:
        //     BindingRestElement[?Yield, ?Await]
        // FormalParameter[Yield, Await]:
        //     BindingElement[?Yield, ?Await]
        // BindingElement[Yield, Await]:
        //     SingleNameBinding[?Yield, ?Await]
        //     BindingPattern[?Yield, ?Await] Initializer[+In, ?Yield, ?Await](opt)
        // SingleNameBinding[Yield, Await]:
        //     BindingIdentifier[?Yield, ?Await] Initializer[+In, ?Yield, ?Await](opt)
        // BindingRestElement[Yield, Await]:
        //     "..." BindingIdentifier[?Yield, ?Await]
        //     "..." BindingPattern[?Yield, ?Await]
        // BindingPattern[Yield, Await]:
        //     ObjectBindingPattern[?Yield, ?Await]
        //     ArrayBindingPattern[?Yield, ?Await]
        self.consume("(", LexGoal::RegExp)?;
        let parameters =
            self.parse_until_punc_or_rest(")", &mut |this| match this.current.tokentype {
                TokenType::Punctuator if this.current.matches_punc("...") => {
                    this.advance(LexGoal::RegExp)?;
                    Ok(FunctionParameter::RestElement {
                        argument: this.parse_binding_id_or_pat()?,
                    })
                }
                _ => {
                    let (line, col) = (this.current.line_num, this.current.line_start);
                    let left = this.parse_binding_id_or_pat()?;
                    let param = match this.current.tokentype {
                        TokenType::Punctuator if this.current.matches_punc("=") => {
                            this.advance(LexGoal::RegExp)?;
                            FunctionParameter::AssignmentPattern {
                                left,
                                right: this.with_in(true, &mut Self::parse_assignment_expr)?,
                            }
                        }
                        _ => match left {
                            Binding::BindingIdentifier(BindingIdentifier { name, .. }) => {
                                FunctionParameter::BindingIdentifier { name }
                            }
                            Binding::ArrayPattern(ArrayPattern { elements, .. }) => {
                                FunctionParameter::ArrayPattern { elements }
                            }
                            Binding::ObjectPattern(ObjectPattern { properties, .. }) => {
                                FunctionParameter::ObjectPattern { properties }
                            }
                            _ => {
                                return Err(Error {
                                    line,
                                    col,
                                    errortype: ErrorType::InvalidDestructuringTarget,
                                })
                            }
                        },
                    };
                    if !this.current.matches_punc(")") {
                        this.consume(",", LexGoal::RegExp)?;
                    }
                    Ok(param)
                }
            })?;
        self.consume(")", LexGoal::RegExp)?;
        Ok(parameters)
    }

    pub fn parse_object_lit_expr(&mut self) -> Result<Node> {
        log!("parse_object_lit_expr");
        // ObjectLiteral[Yield, Await]:
        //     "{" "}"
        //     "{" PropertyDefinitionList[?Yield, ?Await] "}"
        //     "{" PropertyDefinitionList[?Yield, ?Await] "," "}"
        // PropertyDefinitionList[Yield, Await]:
        //     PropertyDefinition[?Yield, ?Await]
        //     PropertyDefinitionList[?Yield, ?Await] "," PropertyDefinition[?Yield, ?Await]
        // PropertyDefinition[Yield, Await]:
        //     IdentifierReference[?Yield, ?Await]
        //     CoverInitializedName[?Yield, ?Await]
        //     PropertyName[?Yield, ?Await] ":" AssignmentExpression[+In, ?Yield, ?Await]
        //     MethodDefinition[?Yield, ?Await]
        //     "..." AssignmentExpression[+In, ?Yield, ?Await]
        // PropertyName[Yield, Await]:
        //     LiteralPropertyName
        //     ComputedPropertyName[?Yield, ?Await]
        // LiteralPropertyName:
        //     IdentifierName
        //     StringLiteral
        //     NumericLiteral
        // ComputedPropertyName[Yield, Await]:
        //     "[" AssignmentExpression[+In, ?Yield, ?Await] "]"
        // CoverInitializedName[Yield, Await]:
        //     IdentifierReference[?Yield, ?Await] Initializer[+In, ?Yield, ?Await]
        // Initializer[In, Yield, Await]:
        //     "=" AssignmentExpression[?In, ?Yield, ?Await]
        // MethodDefinition[Yield, Await]:
        //     PropertyName[?Yield, ?Await] "(" UniqueFormalParameters[~Yield, ~Await] ")" "{" FunctionBody[~Yield, ~Await] "}"
        //     GeneratorMethod[?Yield, ?Await]
        //     AsyncMethod[?Yield, ?Await]
        //     AsyncGeneratorMethod[?Yield, ?Await]
        //     "get" PropertyName[?Yield, ?Await] "(" ")" "{" FunctionBody[~Yield, ~Await] "}"
        //     "set" PropertyName[?Yield, ?Await] "(" FormalParameter[~Yield, ~Await] ")" "{" FunctionBody[~Yield, ~Await] "}"
        self.start_span();
        self.consume("{", LexGoal::RegExp)?;
        let properties = self.parse_until_punc("}", &mut |this| {
            this.start_span();
            if this.current.matches_punc("...") {
                this.advance(LexGoal::RegExp)?;
                let argument = this.with_in(true, &mut Self::parse_assignment_expr)?;
                let res = ObjectExpressionProperty::SpreadElement {
                    argument,
                    span: this.end_span(),
                };
                if !this.current.matches_punc("}") {
                    this.consume(",", LexGoal::RegExp)?;
                }
                return Ok(res);
            }

            let mut computed = false;
            let mut r#async = false;
            let mut generator = false;
            let mut kind = "init";

            match this.current.tokentype {
                TokenType::Identifier if this.current.matches_str("get") => {
                    this.scanner.save_state();
                    let next_token = this.scanner.next(LexGoal::RegExp)?;
                    match &next_token.tokentype {
                        TokenType::Punctuator => {
                            this.scanner.restore_state(next_token);
                        }
                        _ => {
                            this.scanner.restore_state(next_token);
                            this.start_span();
                            this.advance(LexGoal::RegExp)?;
                            let span = this.end_span();
                            if this.current.matches_punc("(") {
                                return this.parse_method_definition(
                                    PropertyKey::Identifier(Identifier {
                                        name: String::from("get"),
                                        span,
                                    }),
                                    kind,
                                    r#async,
                                    generator,
                                    computed,
                                );
                            }
                            kind = "get";
                        }
                    }
                }
                TokenType::Identifier if this.current.matches_str("set") => {
                    this.scanner.save_state();
                    let next_token = this.scanner.next(LexGoal::RegExp)?;
                    match &next_token.tokentype {
                        TokenType::Punctuator => {
                            this.scanner.restore_state(next_token);
                        }
                        _ => {
                            this.scanner.restore_state(next_token);
                            this.start_span();
                            this.advance(LexGoal::RegExp)?;
                            let span = this.end_span();
                            if this.current.matches_punc("(") {
                                return this.parse_method_definition(
                                    PropertyKey::Identifier(Identifier {
                                        name: String::from("set"),
                                        span,
                                    }),
                                    kind,
                                    r#async,
                                    generator,
                                    computed,
                                );
                            }
                            kind = "set";
                        }
                    }
                }
                TokenType::Identifier if this.current.matches_str("async") => {
                    this.start_span();
                    this.advance(LexGoal::RegExp)?;
                    let span = this.end_span();
                    if this.current.matches_punc("*") {
                        this.advance(LexGoal::RegExp)?;
                        generator = true;
                    }
                    if this.current.matches_punc("(") {
                        return this.parse_method_definition(
                            PropertyKey::Identifier(Identifier {
                                name: String::from("async"),
                                span,
                            }),
                            kind,
                            r#async,
                            generator,
                            computed,
                        );
                    } else {
                        r#async = true;
                    }
                }
                TokenType::Punctuator if this.current.matches_punc("*") => {
                    this.advance(LexGoal::RegExp)?;
                    generator = true;
                    let (key, computed) = this.parse_property_name()?;
                    return this.parse_method_definition(key, kind, r#async, generator, computed);
                }
                _ => (),
            }

            let key = match this.current.tokentype {
                TokenType::Identifier | TokenType::Keyword => {
                    this.start_span();
                    let name = this.parse_id_name(LexGoal::Div)?;
                    Ok(PropertyKey::Identifier(Identifier {
                        name,
                        span: this.end_span(),
                    }))
                }
                TokenType::StringLiteral | TokenType::NumericLiteral => {
                    this.start_span();
                    let value = this.advance(LexGoal::RegExp)?.value;
                    Ok(PropertyKey::Literal(Literal {
                        // value: this.current.value.clone(),
                        value,
                        raw: String::from(&this.current.value),
                        span: this.end_span(),
                    }))
                }
                TokenType::Punctuator if this.current.matches_punc("[") => {
                    computed = true;
                    this.parse_computed_property_name()
                }
                _ => this.unexpected_current(),
            }?;

            let property = match this.current.tokentype {
                TokenType::Punctuator if this.current.matches_punc(":") => {
                    this.advance(LexGoal::RegExp)?;
                    let value = PropertyValue::Expression(
                        this.with_in(true, &mut Self::parse_assignment_expr)?,
                    );
                    ObjectExpressionProperty::Property {
                        key,
                        computed,
                        value: Some(value),
                        kind,
                        method: false,
                        shorthand: false,
                        span: this.end_span(),
                    }
                }
                TokenType::Punctuator if this.current.matches_punc("=") => {
                    this.advance(LexGoal::RegExp)?;
                    let value = PropertyValue::Expression(
                        this.with_in(true, &mut Self::parse_assignment_expr)?,
                    );
                    ObjectExpressionProperty::Property {
                        key,
                        computed,
                        value: Some(value),
                        kind,
                        method: false,
                        shorthand: true,
                        span: this.end_span(),
                    }
                }
                TokenType::Punctuator if this.current.matches_punc("(") => {
                    this.parse_method_definition(key, kind, r#async, generator, computed)?
                }
                _ => ObjectExpressionProperty::Property {
                    key,
                    computed,
                    value: None,
                    kind,
                    method: false,
                    shorthand: true,
                    span: this.end_span(),
                },
            };
            if !this.current.matches_punc("}") {
                this.consume(",", LexGoal::RegExp)?;
            }
            Ok(property)
        })?;
        self.consume("}", LexGoal::RegExp)?;
        Ok(Node::ObjectExpression {
            properties,
            assign_target_type: "invalid",
            span: self.end_span(),
        })
    }

    pub fn parse_property_name(&mut self) -> Result<(PropertyKey, bool)> {
        log!("parse_property_name");
        match self.current.tokentype {
            TokenType::Identifier | TokenType::Keyword => {
                self.start_span();
                Ok((
                    PropertyKey::Identifier(Identifier {
                        name: self.parse_id_name(LexGoal::Div)?,
                        span: self.end_span(),
                    }),
                    false,
                ))
            }
            TokenType::StringLiteral | TokenType::NumericLiteral => {
                self.start_span();
                Ok((
                    PropertyKey::Literal(Literal {
                        value: self.current.value.clone(),
                        raw: String::from(&self.current.value),
                        span: self.end_span(),
                    }),
                    false,
                ))
            }
            TokenType::Punctuator if self.current.matches_punc("[") => {
                Ok((self.parse_computed_property_name()?, true))
            }
            _ => self.unexpected_current(),
        }
    }

    pub fn parse_computed_property_name(&mut self) -> Result<PropertyKey> {
        log!("parse_computed_property_name");
        self.consume("[", LexGoal::RegExp)?;
        let name = PropertyKey::Expression(self.with_in(true, &mut Self::parse_assignment_expr)?);
        self.consume("]", LexGoal::RegExp)?;
        Ok(name)
    }

    pub fn parse_method_definition(
        &mut self,
        key: PropertyKey,
        kind: &'static str,
        r#async: bool,
        generator: bool,
        computed: bool,
    ) -> Result<ObjectExpressionProperty> {
        log!("parse_method_definition");
        // pub enum ObjectExpressionProperty {
        //     Property {
        //         key: PropertyKey,
        //         computed: bool,
        //         value: Option<PropertyValue>,
        //         kind: &'static str,
        //         method: bool,
        //         shorthand: bool,
        //     },
        //     SpreadElement {
        //         argument: Node,
        //     },
        // }
        // FunctionExpression {
        //     id: Option<Box<Node>>, // Identifier | null
        //     params: Vec<FunctionParameter>,
        //     body: Box<Node>, // BlockStatement
        //     generator: bool,
        //     expression: bool,
        //     r#async: bool,
        // },
        match kind {
            "get" => {
                self.consume("(", LexGoal::RegExp)?;
                self.consume(")", LexGoal::RegExp)?;
                let value = Some(PropertyValue::FunctionExpression(FunctionExpression {
                    id: None,
                    params: vec![],
                    body: Box::from(self.with_params(
                        Params {
                            has_yield: false,
                            has_await: false,
                            ..self.params
                        },
                        &mut Self::parse_function_body,
                    )?),
                    generator,
                    expression: false,
                    r#async,
                }));
                Ok(ObjectExpressionProperty::Property {
                    key,
                    computed,
                    value,
                    kind,
                    method: true,
                    shorthand: false,
                    span: self.end_span(),
                })
            }
            "set" => {
                let value = Some(PropertyValue::FunctionExpression(FunctionExpression {
                    id: None,
                    params: self.with_params(
                        Params {
                            has_yield: false,
                            has_await: false,
                            ..self.params
                        },
                        &mut Self::parse_formal_params,
                    )?,
                    body: Box::from(self.with_params(
                        Params {
                            has_yield: false,
                            has_await: false,
                            ..self.params
                        },
                        &mut Self::parse_function_body,
                    )?),
                    generator,
                    expression: false,
                    r#async,
                }));
                Ok(ObjectExpressionProperty::Property {
                    key,
                    computed,
                    value,
                    kind,
                    method: true,
                    shorthand: false,
                    span: self.end_span(),
                })
            }
            "init" => {
                let params = self.with_params(
                    Params {
                        has_await: r#async,
                        has_yield: generator,
                        ..self.params
                    },
                    &mut Self::parse_formal_params,
                )?;
                let body = Box::from(self.with_params(
                    Params {
                        has_await: r#async,
                        has_yield: generator,
                        ..self.params
                    },
                    &mut Self::parse_function_body,
                )?);
                let value = if r#async {
                    Some(PropertyValue::AsyncFunctionExpression(
                        AsyncFunctionExpression {
                            id: None,
                            params,
                            body,
                            generator,
                            expression: false,
                            r#async,
                        },
                    ))
                } else {
                    Some(PropertyValue::FunctionExpression(FunctionExpression {
                        id: None,
                        params,
                        body,
                        generator,
                        expression: false,
                        r#async,
                    }))
                };
                Ok(ObjectExpressionProperty::Property {
                    key,
                    computed,
                    value,
                    kind,
                    method: true,
                    shorthand: false,
                    span: self.end_span(),
                })
            }
            _ => self.unexpected_current(), // TODO: fix row and col
        }
    }

    pub fn parse_array_lit_expr(&mut self) -> Result<Node> {
        log!("parse_array_lit_expr");
        // ArrayLiteral[Yield, Await]:
        //     "[" Elision(opt) "]"
        //     "[" ElementList[?Yield, ?Await] "]"
        //     "[" ElementList[?Yield, ?Await] "," Elision(opt) "]"
        // ElementList[Yield, Await]:
        //     Elision(opt) AssignmentExpression[+In, ?Yield, ?Await]
        //     Elision(opt) SpreadElement[?Yield, ?Await]
        //     ElementList[?Yield, ?Await] "," Elision(opt) AssignmentExpression[+In, ?Yield, ?Await]
        //     ElementList[?Yield, ?Await] "," Elision(opt) SpreadElement[?Yield, ?Await]
        // Elision:
        //     ","
        //     Elision ","
        // SpreadElement[Yield, Await]:
        //     "..." AssignmentExpression[+In, ?Yield, ?Await]
        self.start_span();
        self.consume("[", LexGoal::RegExp)?;
        let elements = self.parse_until_punc("]", &mut |this| match this.current.tokentype {
            TokenType::Punctuator if this.current.matches_punc(",") => {
                this.advance(LexGoal::RegExp)?;
                Ok(None)
            }
            TokenType::Punctuator if this.current.matches_punc("...") => {
                this.start_span();
                this.advance(LexGoal::RegExp)?;
                let argument = this.with_in(true, &mut Self::parse_assignment_expr)?;
                if !this.current.matches_punc("]") {
                    this.consume(",", LexGoal::RegExp)?;
                }
                Ok(Some(ArrayExpressionElement::SpreadElement(SpreadElement {
                    argument,
                    span: this.end_span(),
                })))
            }
            _ => {
                let element = this.with_in(true, &mut Self::parse_assignment_expr)?;
                if !this.current.matches_punc("]") {
                    this.consume(",", LexGoal::RegExp)?;
                }
                Ok(Some(ArrayExpressionElement::Expression(element)))
            }
        })?;
        self.consume("]", LexGoal::Div)?;
        Ok(Node::ArrayExpression {
            elements,
            assign_target_type: "invalid",
            span: self.end_span(),
        })
    }

    pub fn parse_async_primary_expr(&mut self) -> Result<Node> {
        log!("parse_async_primary_expr");
        self.start_span();
        let id = self.parse_id_reference()?;
        if !self.has_line_terminator {
            match self.current.tokentype {
                TokenType::Punctuator if self.current.matches_punc(";") => Ok(id),
                TokenType::Punctuator if self.current.matches_punc(".") => Ok(id),
                TokenType::Punctuator if self.current.matches_punc("[") => Ok(id),
                TokenType::Punctuator if self.current.matches_punc("`") => Ok(id),
                TokenType::Punctuator if self.current.matches_punc("(") => {
                    self.parse_async_call_or_arrow(Box::from(id))
                }
                TokenType::Keyword if self.current.matches_str("function") => {
                    self.parse_function_expr(true)
                }
                _ => {
                    let param = match self.with_await(true, &mut Self::parse_id_reference)? {
                        Node::Identifier { name, .. } => {
                            Ok(FunctionParameter::BindingIdentifier { name })
                        }
                        _ => unreachable!(),
                    }?;
                    if !self.has_line_terminator && self.current.matches_punc("=>") {
                        self.advance(LexGoal::RegExp)?;
                        let (body, expression) = self.parse_arrow_body()?;
                        Ok(Node::AsyncArrowFunctionExpression {
                            id: None,
                            params: vec![param],
                            body,
                            generator: false,
                            expression,
                            r#async: true,
                            assign_target_type: "invalid",
                            span: self.end_span(),
                        })
                    } else {
                        self.unexpected_current()
                    }
                }
            }
        } else {
            self.end_span();
            Ok(id)
        }
    }

    pub fn parse_arrow_body(&mut self) -> Result<(Box<Node>, bool)> {
        log!("parse_arrow_body");
        let body;
        let mut expression = false;
        match self.current.tokentype {
            TokenType::Punctuator if self.current.matches_punc("{") => {
                body = Box::from(self.with_params(
                    Params {
                        has_await: true,
                        has_yield: false,
                        ..self.params
                    },
                    &mut Self::parse_function_body,
                )?);
            }
            _ => {
                body = Box::from(self.with_params(
                    Params {
                        has_await: true,
                        has_yield: false,
                        ..self.params
                    },
                    &mut Self::parse_assignment_expr,
                )?);
                expression = true;
            }
        }
        Ok((body, expression))
    }

    pub fn parse_id_reference(&mut self) -> Result<Node> {
        log!("parse_id_reference");
        // IdentifierReference[Yield, Await]:
        //     Identifier
        //     [~Yield] yield
        //     [~Await] await
        self.start_span();
        match self.current.tokentype {
            TokenType::Identifier => {
                let name = self.advance(LexGoal::Div)?.to_string();
                let assign_target_type = match &name[..] {
                    "eval" | "arguments" if self.ctx.strict => "strict",
                    _ => "simple",
                };
                Ok(Node::Identifier {
                    name,
                    assign_target_type,
                    span: self.end_span(),
                })
            }
            TokenType::Keyword if self.current.matches_str("yield") => {
                if self.params.has_yield {
                    self.error_current(ErrorType::YieldInParameter)
                } else {
                    Ok(Node::Identifier {
                        name: self.advance(LexGoal::Div)?.to_string(),
                        assign_target_type: "simple",
                        span: self.end_span(),
                    })
                }
            }
            TokenType::Keyword if self.current.matches_str("await") => {
                if self.params.has_await {
                    self.error_current(ErrorType::AwaitExpressionFormalParameter)
                } else {
                    Ok(Node::Identifier {
                        name: self.advance(LexGoal::Div)?.to_string(),
                        assign_target_type: "simple",
                        span: self.end_span(),
                    })
                }
            }
            _ => self.unexpected_current(),
        }
    }

    pub fn parse_binding_id(&mut self) -> Result<Binding> {
        log!("parse_binding_id");
        self.start_span();
        match self.current.tokentype {
            TokenType::Identifier => Ok(Binding::BindingIdentifier(BindingIdentifier {
                name: self.advance(LexGoal::Div)?.to_string(),
                span: self.end_span(),
            })),
            TokenType::Keyword if self.current.matches_str("yield") => {
                Ok(Binding::BindingIdentifier(BindingIdentifier {
                    name: self.advance(LexGoal::Div)?.to_string(),
                    span: self.end_span(),
                }))
            }
            TokenType::Keyword if self.current.matches_str("await") => {
                Ok(Binding::BindingIdentifier(BindingIdentifier {
                    name: self.advance(LexGoal::Div)?.to_string(),
                    span: self.end_span(),
                }))
            }
            _ => self.unexpected_current(),
        }
    }

    fn parse_id_name(&mut self, goal: LexGoal) -> Result<String> {
        log!("parse_id_name");
        match self.current.tokentype {
            TokenType::Identifier | TokenType::Keyword => {
                Ok(self.advance(goal)?.value.string().unwrap())
            }
            _ => self.unexpected_current(),
        }
    }

    // *******************************************************************
    //      Utility
    // *******************************************************************

    fn parse_while_punc<F, T>(&mut self, punc: &str, parse_fn: &mut F) -> Result<Vec<T>>
    where
        F: FnMut(&mut Self) -> Result<T>,
    {
        let mut res = vec![];
        loop {
            match &self.current.tokentype {
                TokenType::Punctuator if self.current.matches_punc(punc) => {
                    res.push(parse_fn(self)?);
                }
                _ => return Ok(res),
            }
        }
    }

    fn parse_until_punc<F, T>(&mut self, punc: &str, parse_fn: &mut F) -> Result<Vec<T>>
    where
        F: FnMut(&mut Self) -> Result<T>,
    {
        let mut res = vec![];
        loop {
            match &self.current.tokentype {
                TokenType::Punctuator if self.current.matches_punc(punc) => return Ok(res),
                _ => {
                    res.push(parse_fn(self)?);
                }
            }
        }
    }

    fn parse_until_punc_or_rest<F, T>(&mut self, punc: &str, parse_fn: &mut F) -> Result<Vec<T>>
    where
        F: FnMut(&mut Self) -> Result<T>,
    {
        let mut res = vec![];
        loop {
            match &self.current.tokentype {
                TokenType::Punctuator if self.current.matches_punc(punc) => return Ok(res),
                TokenType::Punctuator if self.current.matches_punc("...") => {
                    res.push(parse_fn(self)?);
                    return Ok(res);
                }
                _ => {
                    res.push(parse_fn(self)?);
                }
            }
        }
    }

    fn parse_until_eof<F>(&mut self, parse_fn: &mut F) -> Result<Vec<Node>>
    where
        F: FnMut(&mut Self) -> Result<Node>,
    {
        let mut res = vec![];
        loop {
            match &self.current.tokentype {
                TokenType::Eof => return Ok(res),
                _ => res.push(parse_fn(self)?),
            }
        }
    }

    fn with_params<F, T>(&mut self, params: Params, parse_fn: &mut F) -> Result<T>
    where
        F: FnMut(&mut Self) -> Result<T>,
    {
        self.save_params();
        self.params = params;
        let expr = parse_fn(self);
        self.restore_params();
        expr
    }

    fn with_in<F>(&mut self, has_in: bool, parse_fn: &mut F) -> Result<Node>
    where
        F: FnMut(&mut Self) -> Result<Node>,
    {
        self.save_params();
        self.params.has_in = has_in;
        let expr = parse_fn(self);
        self.restore_params();
        expr
    }

    fn with_yield<F>(&mut self, has_yield: bool, parse_fn: &mut F) -> Result<Node>
    where
        F: FnMut(&mut Self) -> Result<Node>,
    {
        self.save_params();
        self.params.has_yield = has_yield;
        let expr = parse_fn(self);
        self.restore_params();
        expr
    }

    fn with_await<F>(&mut self, has_await: bool, parse_fn: &mut F) -> Result<Node>
    where
        F: FnMut(&mut Self) -> Result<Node>,
    {
        self.save_params();
        self.params.has_await = has_await;
        let expr = parse_fn(self);
        self.restore_params();
        expr
    }

    fn with_default<F>(&mut self, has_default: bool, parse_fn: &mut F) -> Result<Node>
    where
        F: FnMut(&mut Self) -> Result<Node>,
    {
        self.save_params();
        self.params.has_default = has_default;
        let expr = parse_fn(self);
        self.restore_params();
        expr
    }

    fn with_return<F>(&mut self, has_return: bool, parse_fn: &mut F) -> Result<Node>
    where
        F: FnMut(&mut Self) -> Result<Node>,
    {
        self.save_params();
        self.params.has_return = has_return;
        let expr = parse_fn(self);
        self.restore_params();
        expr
    }

    fn advance(&mut self, goal: LexGoal) -> Result<Token> {
        let prev_line_num = self.current.line_num;
        let token = mem::replace(&mut self.current, self.scanner.next(goal)?);
        self.has_line_terminator = self.current.line_num > prev_line_num;
        Ok(token)
    }

    fn consume(&mut self, punc: &str, goal: LexGoal) -> Result<Token> {
        if self.current.matches_punc(punc) {
            self.advance(goal)
        } else {
            self.unexpected_current()
        }
    }

    fn consume_kw(&mut self, kw: &str, goal: LexGoal) -> Result<Token> {
        match self.current.tokentype {
            TokenType::Keyword if self.current.matches_str(kw) => self.advance(goal),
            _ => self.unexpected_current(),
        }
    }

    fn consume_id(&mut self, id: &str, goal: LexGoal) -> Result<Token> {
        match self.current.tokentype {
            TokenType::Identifier if self.current.matches_str(id) => self.advance(goal),
            _ => self.unexpected_current(),
        }
    }

    fn consume_semicolon(&mut self, goal: LexGoal) -> Result<()> {
        match self.current.tokentype {
            TokenType::Punctuator if self.current.matches_punc(";") => {
                self.advance(goal)?;
                Ok(())
            }
            _ if !self.has_line_terminator => match self.current.tokentype {
                TokenType::Punctuator if self.current.matches_punc("}") => Ok(()),
                TokenType::Eof => Ok(()),
                _ => self.unexpected_current(),
            },
            _ => Ok(()),
        }
    }

    fn save_params(&mut self) {
        self.params_stack.push(Params { ..self.params });
    }

    fn restore_params(&mut self) {
        self.params = self.params_stack.pop().unwrap();
    }

    fn start_span(&mut self) {
        self.marker_stack.push(Marker {
            line: self.current.line_num,
            col: self.current.line_start,
            idx: self.current.start,
        });
    }

    fn end_span(&mut self) -> Span {
        let start = match self.marker_stack.pop() {
            Some(marker) => marker,
            _ => panic!("Tried to pop marker from an empty stack. This probably means a marker was failed to be pushed somewhere.")
        };
        Span {
            start,
            end: Marker {
                line: self.scanner.line,
                col: self.scanner.line_start,
                idx: self.scanner.current,
            },
        }
    }

    fn peek_span(&mut self) -> Span {
        let start = match self.marker_stack.last() {
            Some(marker) => marker.clone(),
            _ => panic!("Tried to pop marker from an empty stack. This probably means a marker was failed to be pushed somewhere.")
        };
        Span {
            start,
            end: Marker {
                line: self.scanner.line,
                col: self.scanner.line_start,
                idx: self.scanner.current,
            },
        }
    }

    // *******************************************************************
    //      Errors
    // *******************************************************************

    fn error<T>(&mut self, token: Token, errortype: ErrorType) -> Result<T> {
        Err(Error {
            line: token.line_num,
            col: token.line_start,
            errortype,
        })
    }

    fn error_current<T>(&mut self, errortype: ErrorType) -> Result<T> {
        let token = self.advance(LexGoal::RegExp)?;
        self.error(token, errortype)
    }

    fn unexpected<T>(&self, token: Token) -> Result<T> {
        match &token.tokentype {
            _ => Err(Error {
                line: token.line_num,
                col: token.line_start,
                errortype: ErrorType::UnexpectedToken(String::from(&token.value)),
            }),
        }
    }

    fn unexpected_current<T>(&mut self) -> Result<T> {
        let token = self.advance(LexGoal::RegExp)?;
        self.unexpected(token)
    }
}
