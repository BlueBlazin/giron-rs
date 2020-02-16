use crate::reader::Reader;
use codepoint::codepoint::CodePoint;
use errors::errors::{Error, ErrorType};
use errors::result::Result;
use std::char;
use std::mem;
use token::punctuators::Punctuator;
use token::token::{Token, TokenType};
use token::value::Value;

#[derive(PartialEq)]
pub enum LexGoal {
    Div,
    RegExp,
}

#[derive(PartialEq, Clone)]
pub enum Brace {
    Template,
    LBrace,
}

struct ScannerState {
    start: usize,
    current: usize,
    line: usize,
    line_start: usize,
    newline: bool,
    raw_string: String,
    curly_stack: Vec<Brace>,
}

pub struct Scanner<I> {
    pub start: usize,
    pub current: usize,
    pub line: usize,
    pub line_start: usize,
    pub newline: bool,
    reader: Reader<I>,
    record_raw: bool,
    raw_string: String,
    curly_stack: Vec<Brace>,
    unscanned: Option<Token>,
    state: Option<ScannerState>,
}

impl<I> Scanner<I>
where
    I: Iterator<Item = char>,
{
    pub fn new(source: I) -> Self {
        Scanner {
            start: 0,
            current: 0,
            line: 0,
            line_start: 0,
            newline: false,
            reader: Reader::new(source),
            record_raw: false,
            raw_string: String::with_capacity(8),
            curly_stack: Vec::new(),
            unscanned: None,
            state: None,
        }
    }

    pub fn next(&mut self, goal: LexGoal) -> Result<Token> {
        if let Some(_) = self.unscanned {
            let token = mem::replace(&mut self.unscanned, None);
            return Ok(token.unwrap());
        }

        self.newline = false;
        self.scan_comments();
        self.start_new_token();

        match self.peek2() {
            (Some(cp), _) if cp.is_identifier_start() => self.scan_identifier(),
            (Some('"'), _) | (Some('\''), _) => self.scan_string_literal(),
            (Some('.'), Some(cp)) if cp.is_decimal_digit() => self.scan_numeric_literal(),
            (Some(cp), _) if cp.is_decimal_digit() => self.scan_numeric_literal(),
            (Some('`'), _) => self.scan_template(),
            (Some('}'), _) if self.curly_stack.last() == Some(&Brace::Template) => {
                self.curly_stack.pop();
                self.scan_template_substitution_tail()
            }
            (Some('/'), _) if goal == LexGoal::Div => self.scan_div_punctuator(),
            (Some('/'), _) if goal == LexGoal::RegExp => self.scan_regexp_literal(),
            (Some(_), _) => self.scan_punctuator(),
            (None, _) => self.token_simple(TokenType::Eof, Value::Null),
        }
    }

    fn scan_comments(&mut self) {
        loop {
            match self.peek2() {
                (Some(cp), _) if cp.is_whitespace() => {
                    self.consume();
                }
                (Some('\r'), Some('\n')) => {
                    self.consume();
                    self.consume();
                }
                (Some(cp), _) if cp.is_line_terminator() => {
                    self.consume();
                }
                (Some('/'), Some('/')) => self.scan_singleline_comment(),
                (Some('/'), Some('*')) => self.scan_multiline_comment(),
                (_, _) => break,
            }
        }
    }

    fn scan_singleline_comment(&mut self) {
        self.consume();
        self.consume();
        loop {
            match self.peek() {
                Some(cp) if cp.is_line_terminator() => break,
                Some(_) => {
                    self.consume();
                }
                None => break,
            }
        }
    }

    fn scan_multiline_comment(&mut self) {
        self.consume();
        self.consume();
        loop {
            match self.peek2() {
                (Some('*'), Some('/')) => {
                    self.consume();
                    self.consume();
                    break;
                }
                (Some(_), _) => {
                    self.consume();
                }
                (None, _) => break,
            }
        }
    }

    // *******************************************************************
    //      Identifiers
    // *******************************************************************

    pub fn scan_identifier(&mut self) -> Result<Token> {
        let mut value = String::new();
        value.push(self.scan_identifier_start()?);
        let part = self.scan_until_with(
            |cp| cp != '\\' && !cp.is_identifier_continue(),
            &mut |this| {
                let cp = this.consume().unwrap();
                if cp == '\\' {
                    this.scan_unicode_esc_seq()
                } else {
                    Ok(cp)
                }
            },
        )?;
        value.push_str(&part);

        match value.as_str() {
            "null" => self.token_simple(TokenType::NullLiteral, Value::Null),
            "true" | "false" => self.token_simple(TokenType::BooleanLiteral, Value::Str(value)),
            val => {
                if self.is_keyword(val) {
                    self.token_simple(TokenType::Keyword, Value::Str(value))
                } else {
                    self.token_simple(TokenType::Identifier, Value::Str(value))
                }
            }
        }
    }

    fn scan_identifier_start(&mut self) -> Result<char> {
        let cp = match self.peek() {
            Some('\\') => self.scan_unicode_esc_seq()?,
            Some(_) => self.consume().unwrap(),
            None => return self.error(ErrorType::UnexpectedEOS),
        };

        if cp.is_identifier_start() {
            Ok(cp)
        } else {
            self.error(ErrorType::InvalidOrUnexpectedToken)
        }
    }

    /// Scans a legal codepoint following one of the patterns:
    /// "\" "u" HexDigit HexDigit HexDigit HexDigit
    /// "\" "u" "{" HexDigits "}"
    fn scan_unicode_esc_seq(&mut self) -> Result<char> {
        self.expect('\\')?;
        self.expect('u')?;

        if self.matches('{') {
            self.consume();
            // reduce scanned chars to u32
            let value = self
                .scan_until_with(|cp| !cp.is_hex_digit(), &mut |this| {
                    this.consume().ok_or(Error {
                        line: this.line,
                        col: this.line_start,
                        errortype: ErrorType::UnexpectedEOS,
                    })
                })?
                .chars()
                .fold(0, |value, cp| value * 16 + cp.to_digit(16).unwrap());
            // consume "}"
            self.expect('}')?;
            char::from_u32(value).ok_or(Error {
                line: self.line,
                col: self.line_start,
                errortype: ErrorType::InvalidUnicodeEscapeSequence,
            })
        } else {
            let mut value = 0;
            for _ in 0..4 {
                match self.consume() {
                    Some(cp) if cp.is_hex_digit() => {
                        value = value * 16 + cp.to_digit(16).unwrap();
                    }
                    _ => return self.error(ErrorType::InvalidUnicodeEscapeSequence),
                }
            }
            char::from_u32(value).ok_or(Error {
                line: self.line,
                col: self.line_start,
                errortype: ErrorType::InvalidOrUnexpectedToken,
            })
        }
    }

    /// Check whether value is a keyword or future reserved word
    pub fn is_keyword(&self, value: &str) -> bool {
        match value.len() {
            2 => value == "do" || value == "if" || value == "in",
            3 => value == "for" || value == "new" || value == "try" || value == "var",
            4 => {
                value == "case"
                    || value == "else"
                    || value == "this"
                    || value == "void"
                    || value == "with"
                    || value == "enum"
            }
            5 => {
                value == "await"
                    || value == "break"
                    || value == "catch"
                    || value == "class"
                    || value == "const"
                    || value == "super"
                    || value == "throw"
                    || value == "while"
                    || value == "yield"
            }
            6 => {
                value == "delete"
                    || value == "export"
                    || value == "import"
                    || value == "return"
                    || value == "switch"
                    || value == "typeof"
            }
            7 => value == "default" || value == "extends" || value == "finally",
            8 => value == "continue" || value == "debugger" || value == "function",
            10 => value == "instanceof",
            _ => false,
        }
    }

    // *******************************************************************
    //      Numeric Literals
    // *******************************************************************

    pub fn scan_numeric_literal(&mut self) -> Result<Token> {
        let value = match self.peek2() {
            (Some('0'), Some('b')) | (Some('0'), Some('B')) => self.scan_radix_literal(2),
            (Some('0'), Some('o')) | (Some('0'), Some('O')) => self.scan_radix_literal(8),
            (Some('0'), Some('x')) | (Some('0'), Some('X')) => self.scan_radix_literal(16),
            (Some('0'), Some(cp)) if cp != '.' => {
                let val = self.scan_until_with(|cp| !cp.is_decimal_digit(), &mut |this| {
                    this.consume().ok_or(Error {
                        line: this.line,
                        col: this.line_start,
                        errortype: ErrorType::UnexpectedEOS,
                    })
                })?;
                // A number starting with 0 can either be octal or decimal.
                i64::from_str_radix(&val, 8)
                    .or_else(|_| {
                        i64::from_str_radix(&val, 10)
                        // TODO: Scan dot and exponent
                    })
                    .map(|i| i as f64)
                    .map_err(|_| Error {
                        line: self.line,
                        col: self.line_start,
                        errortype: ErrorType::InvalidOrUnexpectedToken,
                    })
            }
            (Some(cp), _) if cp.is_decimal_digit() => {
                // scan decimal digit
                let mut val = self.scan_until_with(|cp| !cp.is_decimal_digit(), &mut |this| {
                    this.consume().ok_or(Error {
                        line: this.line,
                        col: this.line_start,
                        errortype: ErrorType::UnexpectedEOS,
                    })
                })?;
                // scan potential dot
                if self.matches('.') {
                    self.scan_fractional_part(val)
                } else {
                    val.push_str(&self.scan_exponent()?);
                    val.parse().map_err(|_| Error {
                        line: self.line,
                        col: self.line_start,
                        errortype: ErrorType::InvalidOrUnexpectedToken,
                    })
                }
            }
            (Some('.'), _) => {
                let val = String::new();
                self.scan_fractional_part(val)
            }
            (_, _) => self.error(ErrorType::InvalidOrUnexpectedToken),
        }?;

        self.token_simple(TokenType::NumericLiteral, Value::Num(value))
    }

    fn scan_fractional_part(&mut self, mut value: String) -> Result<f64> {
        self.expect('.')?;
        value.push('.');

        // scan decimal digits
        let fractional = self.scan_until_with(|cp| !cp.is_decimal_digit(), &mut |this| {
            this.consume().ok_or(Error {
                line: this.line,
                col: this.line_start,
                errortype: ErrorType::InvalidOrUnexpectedToken,
            })
        })?;

        value.push_str(&fractional);
        value.push_str(&self.scan_exponent()?);
        // We use parse here instead of from_str_radix for convenience
        value.parse().map_err(|_| Error {
            line: self.line,
            col: self.line_start,
            errortype: ErrorType::InvalidOrUnexpectedToken,
        })
    }

    fn scan_exponent(&mut self) -> Result<String> {
        let mut value = String::new();
        match self.peek() {
            Some('e') | Some('E') => {
                value.push(self.consume().unwrap());
                // scan potential sign
                match self.peek() {
                    Some('+') | Some('-') => value.push(self.consume().unwrap()),
                    _ => (),
                };
                let exponent = self.scan_until_with(|cp| !cp.is_decimal_digit(), &mut |this| {
                    this.consume().ok_or(Error {
                        line: this.line,
                        col: this.line_start,
                        errortype: ErrorType::InvalidOrUnexpectedToken,
                    })
                })?;
                value.push_str(&exponent);
                Ok(value)
            }
            _ => Ok(value),
        }
    }

    fn scan_radix_literal(&mut self, radix: u32) -> Result<f64> {
        // NOTE: This function consumes as many hex digits as possible.
        // Can this lead to Err for valid ECMAScript?
        self.expect('0')?;
        self.consume(); // b | B | o | O | x | X
        let value = self.scan_until_with(|cp| !cp.is_hex_digit(), &mut |this| {
            this.consume().ok_or(Error {
                line: this.line,
                col: this.line_start,
                errortype: ErrorType::InvalidOrUnexpectedToken,
            })
        })?;

        i64::from_str_radix(&value, radix)
            .map(|i| i as f64)
            .map_err(|_| Error {
                line: self.line,
                col: self.line_start,
                errortype: ErrorType::InvalidOrUnexpectedToken,
            })
    }

    // *******************************************************************
    //      String Literals
    // *******************************************************************

    pub fn scan_string_literal(&mut self) -> Result<Token> {
        let quote;
        let mut octal = false;
        match self.peek() {
            Some('\'') | Some('"') => quote = self.consume().unwrap(),
            _ => return self.error(ErrorType::InvalidOrUnexpectedToken),
        }

        let value = self.scan_until_with(|cp| cp == quote, &mut |this| {
            let (res, is_octal) = this.scan_string_literal_cp();
            octal = octal || is_octal;
            res
        })?;

        self.expect(quote)?;
        self.token(
            TokenType::StringLiteral,
            Value::Str(value),
            None,
            None,
            Some(octal),
            None,
            None,
            None,
        )
    }

    fn scan_string_literal_cp(&mut self) -> (Result<char>, bool) {
        loop {
            match self.peek2() {
                (Some('\\'), Some(cp)) if cp.is_single_escape_char() => {
                    self.consume();
                    let val = self.consume().map(|esc| esc.unescape()).ok_or(Error {
                        line: self.line,
                        col: self.line_start,
                        errortype: ErrorType::UnexpectedEOS,
                    });
                    return (val, false);
                }
                (Some('\\'), Some(cp)) if cp.is_line_terminator() => {
                    self.consume(); // consume "\"
                    if let (Some('\r'), Some('\n')) = self.peek2() {
                        self.consume(); // consume "\r" if <CR><LF>
                    }
                    self.consume(); // consume line terminator
                }
                (Some('\\'), Some('x')) => return (self.scan_hex_esc_seq(), false),
                (Some('\\'), Some('u')) => return (self.scan_unicode_esc_seq(), false),
                (Some('\\'), Some(cp)) if cp.is_octal_digit() => {
                    return (self.scan_octal_esc_seq(), true)
                }
                (Some('\\'), Some(_)) => {
                    return (
                        self.consume().ok_or(Error {
                            line: self.line,
                            col: self.line_start,
                            errortype: ErrorType::UnexpectedEOS,
                        }),
                        false,
                    )
                }
                (Some(cp), _) if cp.is_line_terminator() => {
                    return (
                        Err(Error {
                            line: self.line,
                            col: self.line_start,
                            errortype: ErrorType::InvalidOrUnexpectedToken,
                        }),
                        false,
                    )
                }
                (Some(_), _) => {
                    return (
                        self.consume().ok_or(Error {
                            line: self.line,
                            col: self.line_start,
                            errortype: ErrorType::UnexpectedEOS,
                        }),
                        false,
                    )
                }
                (_, _) => {
                    return (
                        Err(Error {
                            line: self.line,
                            col: self.line_start,
                            errortype: ErrorType::InvalidOrUnexpectedToken,
                        }),
                        false,
                    )
                }
            }
        }
    }

    fn scan_octal_esc_seq(&mut self) -> Result<char> {
        self.consume();

        let value = self.scan_until_with(|cp| !cp.is_octal_digit(), &mut |this| {
            this.consume().ok_or(Error {
                line: this.line,
                col: this.line_start,
                errortype: ErrorType::UnexpectedEOS,
            })
        })?;

        let mv = i64::from_str_radix(&value, 8)
            .map(|i| i as u32)
            .map_err(|_| Error {
                line: self.line,
                col: self.line_start,
                errortype: ErrorType::InvalidOrUnexpectedToken,
            })?;

        char::from_u32(mv).ok_or(Error {
            line: self.line,
            col: self.line_start,
            errortype: ErrorType::InvalidOrUnexpectedToken,
        })
    }

    fn scan_hex_esc_seq(&mut self) -> Result<char> {
        self.expect('\\')?;
        self.expect('x')?;
        let mut value = 0;
        for _ in 0..2 {
            match self.consume() {
                Some(cp) if cp.is_hex_digit() => {
                    value = value * 16 + cp.to_digit(16).unwrap();
                }
                _ => return self.error(ErrorType::InvalidOrUnexpectedToken),
            }
        }
        char::from_u32(value).ok_or(Error {
            line: self.line,
            col: self.line_start,
            errortype: ErrorType::InvalidOrUnexpectedToken,
        })
    }

    // *******************************************************************
    //      Regular Expression Literals
    // *******************************************************************

    pub fn scan_regexp_literal(&mut self) -> Result<Token> {
        let pattern = self.scan_regexp_body()?;
        let flags = self.scan_regexp_flags()?;

        self.token(
            TokenType::RegularExpression,
            Value::Str(String::from("")),
            Some(pattern),
            Some(flags),
            None,
            None,
            None,
            None,
        )
    }

    fn scan_regexp_body(&mut self) -> Result<String> {
        // TODO: Do a better scan and report early errors.
        self.expect('/')?;
        if let Some('*') = self.peek() {
            return self.error(ErrorType::InvalidOrUnexpectedToken);
        }
        let mut value = String::from("");
        loop {
            match self.peek() {
                Some('\\') => {
                    value.push(self.consume().ok_or(Error {
                        line: self.line,
                        col: self.line_start,
                        errortype: ErrorType::UnexpectedEOS,
                    })?);
                    value.push(self.consume().ok_or(Error {
                        line: self.line,
                        col: self.line_start,
                        errortype: ErrorType::UnexpectedEOS,
                    })?);
                }
                Some(cp) if cp == '/' => break,
                Some(_) => {
                    let res = self.consume().ok_or(Error {
                        line: self.line,
                        col: self.line_start,
                        errortype: ErrorType::UnexpectedEOS,
                    })?;
                    value.push(res);
                }
                None => break,
            }
        }

        let value = self.scan_until_with(|cp| cp == '/', &mut |this| {
            this.consume().ok_or(Error {
                line: this.line,
                col: this.line_start,
                errortype: ErrorType::UnexpectedEOS,
            })
        })?;
        self.expect('/')?;
        Ok(value)
    }

    fn scan_regexp_flags(&mut self) -> Result<Vec<char>> {
        self.scan_until_with(
            |cp| cp != '\\' && !cp.is_identifier_continue(),
            &mut |this| {
                let cp = this.consume().unwrap();
                if cp == '\\' {
                    this.scan_unicode_esc_seq()
                } else {
                    Ok(cp)
                }
            },
        )
        .map(|value| value.chars().collect())
    }

    // *******************************************************************
    //      Template Literal Lexical Components
    // *******************************************************************

    pub fn scan_template(&mut self) -> Result<Token> {
        let mut value = String::new();
        let mut tail = false;
        let octal = false;
        self.expect('`')?;
        self.start_recording();
        let cooked = self.scan2_until_with(
            |(cp1, cp2)| cp1 == Some('`') || (cp1 == Some('$') && cp2 == Some('{')),
            &mut |this| this.scan_template_char(),
        )?;
        value.push_str(&self.stop_recording());
        match self.peek2() {
            (Some('$'), Some('{')) => {
                self.consume();
                self.consume();
                self.curly_stack.push(Brace::Template);
            }
            (Some('`'), _) => {
                self.consume();
                tail = true;
            }
            (_, _) => unreachable!(),
        }
        self.token(
            TokenType::Template,
            Value::Str(value),
            None,
            None,
            Some(octal),
            Some(cooked),
            Some(true),
            Some(tail),
        )
    }

    pub fn scan_template_substitution_tail(&mut self) -> Result<Token> {
        let mut value = String::new();
        let mut tail = false;
        let octal = false;
        self.expect('}')?;
        self.start_recording();
        let cooked = self.scan2_until_with(
            |(cp1, cp2)| cp1 == Some('`') || (cp1 == Some('$') && cp2 == Some('{')),
            &mut |this| this.scan_template_char(),
        )?;
        value.push_str(&self.stop_recording());
        match self.peek2() {
            (Some('$'), Some('{')) => {
                self.consume();
                self.consume();
                self.curly_stack.push(Brace::Template);
            }
            (Some('`'), _) => {
                self.consume();
                tail = true;
            }
            (_, _) => unreachable!(),
        }
        self.token(
            TokenType::Template,
            Value::Str(value),
            None,
            None,
            Some(octal),
            Some(cooked),
            Some(false),
            Some(tail),
        )
    }

    fn scan_template_char(&mut self) -> Result<char> {
        match self.peek2() {
            (Some('\\'), Some(cp)) if cp.is_line_terminator() => {
                self.consume();
                match self.peek2() {
                    (Some('\r'), Some('\n')) => {
                        self.consume();
                        self.consume();
                        Ok('\0')
                    }
                    (_, _) => {
                        self.consume();
                        Ok('\0')
                    }
                }
            }
            (Some('\\'), Some('b'))
            | (Some('\\'), Some('f'))
            | (Some('\\'), Some('v'))
            | (Some('\\'), Some('n'))
            | (Some('\\'), Some('r'))
            | (Some('\\'), Some('t')) => {
                self.consume();
                self.consume().map(|cp| cp.unescape()).ok_or(Error {
                    line: self.line,
                    col: self.line_start,
                    errortype: ErrorType::UnexpectedEOS,
                })
            }
            (Some('\\'), Some('x')) => self.scan_hex_esc_seq(),
            (Some('\\'), Some('u')) => self.scan_unicode_esc_seq(),
            // TODO: handle octal escape
            (Some('\\'), Some(_)) => {
                self.consume();
                self.consume().ok_or(Error {
                    line: self.line,
                    col: self.line_start,
                    errortype: ErrorType::UnexpectedEOS,
                })
            }
            (Some(cp), _) if cp.is_line_terminator() => {
                self.consume();
                Ok('\n')
            }
            (Some(_), _) => self.consume().ok_or(Error {
                line: self.line,
                col: self.line_start,
                errortype: ErrorType::UnexpectedEOS,
            }),
            (_, _) => self.error(ErrorType::InvalidOrUnexpectedToken),
        }
    }

    // *******************************************************************
    //      Punctuators
    // *******************************************************************

    pub fn scan_punctuator(&mut self) -> Result<Token> {
        // Single: "{" "(" "[" ";" "," "]" ":" "?" "~"
        // Double: ("+", "+=", "++") ("-", "-=", "--") ("&", "&=", "&&") ("|", "|=", "||") ("%", "%=")
        // Double: ("^", "^=")
        // Triple: ("<", "<<", "<<=", "<=") (".", "...") ("!", "!=", "!==") ("*", "**", "*=", "**=")
        // Triple: ("=", "==", "===", "=>")
        // Quadruple: (">", ">=", ">>", ">>>", ">>=", ">>>=")
        match self.peek2() {
            (Some('{'), _) => {
                self.curly_stack.push(Brace::LBrace);
                self.punc1("{")
            }
            (Some('}'), _) => {
                self.curly_stack.pop();
                self.punc1("}")
            }
            (Some(cp), _) if cp.is_single_char_punctuator() => self.punc1(&cp.to_string()[..]),
            (Some('+'), Some(cp)) => match cp {
                '=' => self.punc2("+="),
                '+' => self.punc2("++"),
                _ => self.punc2("+"),
            },
            (Some('-'), Some(cp)) => match cp {
                '=' => self.punc2("-="),
                '-' => self.punc2("--"),
                _ => self.punc1("-"),
            },
            (Some('&'), Some(cp)) => match cp {
                '=' => self.punc2("&="),
                '&' => self.punc2("&&"),
                _ => self.punc2("&"),
            },
            (Some('|'), Some(cp)) => match cp {
                '=' => self.punc2("|="),
                '|' => self.punc2("||"),
                _ => self.punc2("|"),
            },
            (Some('%'), Some(cp)) => match cp {
                '=' => self.punc2("%="),
                _ => self.punc2("%"),
            },
            (Some('^'), Some(cp)) => match cp {
                '=' => self.punc2("^="),
                _ => self.punc2("^"),
            },
            (Some('.'), _) => {
                self.consume();
                match self.peek2() {
                    (Some('.'), Some('.')) => self.punc2("..."),
                    _ => self.punc0("."),
                }
            }
            (Some('<'), _) => {
                self.consume();
                match self.peek2() {
                    (Some('<'), Some('=')) => self.punc2("<<="),
                    (Some('<'), _) => self.punc1("<<"),
                    (Some('='), _) => self.punc1("<="),
                    (_, _) => self.punc0("<"),
                }
            }
            (Some('!'), _) => {
                self.consume();
                match self.peek2() {
                    (Some('='), Some('=')) => self.punc2("!=="),
                    (Some('='), _) => self.punc1("!="),
                    (_, _) => self.punc0("!"),
                }
            }
            (Some('*'), _) => {
                self.consume();
                match self.peek2() {
                    (Some('*'), Some('=')) => self.punc2("**="),
                    (Some('*'), _) => self.punc1("**"),
                    (Some('='), _) => self.punc1("*="),
                    (_, _) => self.punc0("*"),
                }
            }
            (Some('='), _) => {
                self.consume();
                match self.peek2() {
                    (Some('='), Some('=')) => self.punc2("==="),
                    (Some('='), _) => self.punc1("=="),
                    (Some('>'), _) => self.punc1("=>"),
                    (_, _) => self.punc0("="),
                }
            }
            (Some('>'), _) => {
                self.consume();
                match self.peek2() {
                    (Some('='), _) => self.punc1(">="),
                    (Some('>'), Some('>')) => {
                        self.consume();
                        match self.peek2() {
                            (Some('>'), Some('=')) => self.punc2(">>>="),
                            (_, _) => self.punc1(">>>"),
                        }
                    }
                    (Some('>'), _) => self.punc1(">>"),
                    (_, _) => self.punc0(">"),
                }
            }
            (_, _) => self.error(ErrorType::InvalidOrUnexpectedToken),
        }
    }

    pub fn scan_div_punctuator(&mut self) -> Result<Token> {
        match self.peek2() {
            (Some('/'), Some('=')) => self.punc2("/="),
            (Some('/'), _) => self.punc1("/"),
            (_, _) => self.error(ErrorType::InvalidOrUnexpectedToken),
        }
    }

    fn punc0(&mut self, value: &str) -> Result<Token> {
        self.token_simple(TokenType::Punctuator, Value::Punc(Punctuator::from(value)))
    }

    fn punc1(&mut self, value: &str) -> Result<Token> {
        self.consume();
        self.token_simple(TokenType::Punctuator, Value::Punc(Punctuator::from(value)))
    }

    fn punc2(&mut self, value: &str) -> Result<Token> {
        self.consume();
        self.consume();
        self.token_simple(TokenType::Punctuator, Value::Punc(Punctuator::from(value)))
    }

    // *******************************************************************
    //      Utility
    // *******************************************************************

    fn scan_until_with<F, G>(&mut self, pred: F, read: &mut G) -> Result<String>
    where
        F: Fn(char) -> bool,
        G: FnMut(&mut Self) -> Result<char>,
    {
        let mut res = String::new();
        loop {
            match self.peek() {
                Some(cp) if pred(cp) => return Ok(res),
                Some(_) => res.push(read(self)?),
                None => return Ok(res),
            }
        }
    }

    pub fn scan2_until_with<F, G>(&mut self, pred: F, read: &mut G) -> Result<String>
    where
        F: Fn((Option<char>, Option<char>)) -> bool,
        G: FnMut(&mut Self) -> Result<char>,
    {
        let mut res = String::new();
        loop {
            match self.peek2() {
                (Some(cp1), Some(cp2)) if pred((Some(cp1), Some(cp2))) => return Ok(res),
                (Some(cp1), None) if pred((Some(cp1), None)) => return Ok(res),
                (Some(_), _) => res.push(read(self)?),
                (None, _) => return Ok(res),
            }
        }
    }

    fn peek(&mut self) -> Option<char> {
        self.reader.lookahead(0)
    }

    fn peek2(&mut self) -> (Option<char>, Option<char>) {
        (self.reader.lookahead(0), self.reader.lookahead(1))
    }

    fn consume(&mut self) -> Option<char> {
        let cp = self.reader.next();
        if self.record_raw {
            if let Some(cp) = cp {
                self.raw_string.push(cp);
            }
        }
        self.current += 1;
        match (cp, self.peek()) {
            (Some('\r'), Some('\n')) => (),
            (Some(cp), _) if cp.is_line_terminator() => {
                self.line += 1;
                self.newline = true;
            }
            (_, _) => (),
        }
        cp
    }

    fn start_new_token(&mut self) {
        self.line_start = self.line;
        self.start = self.current;
    }

    fn start_recording(&mut self) {
        self.record_raw = true;
    }

    fn stop_recording(&mut self) -> String {
        self.record_raw = false;
        mem::replace(&mut self.raw_string, String::with_capacity(8))
    }

    fn token(
        &mut self,
        tokentype: TokenType,
        value: Value,
        pattern: Option<String>,
        flags: Option<Vec<char>>,
        octal: Option<bool>,
        cooked: Option<String>,
        head: Option<bool>,
        tail: Option<bool>,
    ) -> Result<Token> {
        Ok(Token {
            tokentype,
            value,
            pattern,
            flags,
            octal,
            cooked,
            head,
            tail,
            line_num: self.line,
            line_start: self.line_start,
            start: self.start,
            end: self.current - 1,
        })
    }

    fn token_simple(&mut self, tokentype: TokenType, value: Value) -> Result<Token> {
        self.token(tokentype, value, None, None, None, None, None, None)
    }

    fn expect(&mut self, expected: char) -> Result<()> {
        match self.consume() {
            Some(cp) if cp == expected => Ok(()),
            _ => self.error(ErrorType::InvalidOrUnexpectedToken),
        }
    }

    fn matches(&mut self, expected: char) -> bool {
        match self.peek() {
            Some(cp) => cp == expected,
            _ => false,
        }
    }

    pub fn save_state(&mut self) {
        // struct ScannerState {
        //     start: usize,
        //     current: usize,
        //     line: usize,
        //     line_start: usize,
        //     newline: bool,
        //     raw_string: String,
        //     curly_stack: Vec<Brace>,
        // }
        self.state = Some(ScannerState {
            start: self.start,
            current: self.current,
            line: self.line,
            line_start: self.line_start,
            newline: self.newline,
            raw_string: self.raw_string.clone(),
            curly_stack: self.curly_stack.clone(),
        });
    }

    pub fn restore_state(&mut self, token: Token) {
        let state = mem::replace(&mut self.state, None);
        match state {
            Some(state) => {
                self.start = state.start;
                self.current = state.current;
                self.line = state.line;
                self.line_start = state.line_start;
                self.newline = state.newline;
                self.raw_string = state.raw_string;
                self.curly_stack = state.curly_stack;
                self.unscanned = Some(token);
            }
            None => panic!("Trying to restore an unsaved state."),
        }
    }

    fn error<T>(&self, errortype: ErrorType) -> Result<T> {
        Err(Error {
            line: self.line,
            col: self.line_start,
            errortype,
        })
    }
}
