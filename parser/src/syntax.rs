use token::punctuators::Punctuator;
use token::token::{Token, TokenType};
use token::value::Value;

pub trait Syntax {
    fn to_keyword(self) -> Self;
    fn to_string(self) -> String;
    fn to_str(&self) -> &str;
    fn matches_str(&self, val: &str) -> bool;
    fn matches_punc(&self, val: &str) -> bool;
    fn matches_null(&self) -> bool;
    fn is_assign_op(&self) -> bool;
    fn is_bin_op(&self) -> bool;
    fn is_unary_op(&self) -> bool;
    fn is_update_op(&self) -> bool;
    fn precedence(&self, has_in: bool) -> usize;
    fn is_iteration_kw(&self) -> bool;
    fn is_hoistable_dclr_kw(&self) -> bool;
    fn is_lexical_dclr_kw(&self) -> bool;
    fn is_lexical_dclr_or_var_kw(&self) -> bool;
    fn is_dclr_kw(&self) -> bool;
}

impl Syntax for Token {
    fn to_keyword(mut self) -> Self {
        if self.tokentype == TokenType::Identifier {
            self.tokentype = TokenType::Keyword
        }
        self
    }

    fn to_string(self) -> String {
        match self.value {
            Value::Str(s) => s,
            Value::Punc(p) => p.to_string(),
            _ => panic!("Not a string value."),
        }
    }

    fn to_str(&self) -> &str {
        match &self.value {
            Value::Str(s) => &s[..],
            Value::Punc(p) => p.to_str(),
            _ => panic!("Not possible to convert value to &str."),
        }
    }

    fn matches_str(&self, val: &str) -> bool {
        match &self.value {
            Value::Str(s) if &s[..] == val => true,
            _ => false,
        }
    }

    fn matches_punc(&self, val: &str) -> bool {
        match &self.value {
            Value::Punc(p) if p == &Punctuator::from(val) => true,
            _ => false,
        }
    }

    fn matches_null(&self) -> bool {
        match self.value {
            Value::Null => true,
            _ => false,
        }
    }

    fn is_assign_op(&self) -> bool {
        match self.value {
            Value::Punc(Punctuator::PlusAssign)
            | Value::Punc(Punctuator::MinusAssign)
            | Value::Punc(Punctuator::StarAssign)
            | Value::Punc(Punctuator::ModAssign)
            | Value::Punc(Punctuator::PowAssign)
            | Value::Punc(Punctuator::LShiftAssign)
            | Value::Punc(Punctuator::RShiftAssign)
            | Value::Punc(Punctuator::ZRShiftAssign)
            | Value::Punc(Punctuator::BitAndAssign)
            | Value::Punc(Punctuator::BitOrAssign)
            | Value::Punc(Punctuator::BitXorAssign)
            | Value::Punc(Punctuator::SlashAssign) => true,
            Value::Punc(Punctuator::Equal) => true,
            _ => false,
        }
    }

    fn is_bin_op(&self) -> bool {
        match self.tokentype {
            TokenType::Punctuator if self.matches_punc("<") => true,
            TokenType::Punctuator if self.matches_punc(">") => true,
            TokenType::Punctuator if self.matches_punc("<=") => true,
            TokenType::Punctuator if self.matches_punc(">=") => true,
            TokenType::Punctuator if self.matches_punc("==") => true,
            TokenType::Punctuator if self.matches_punc("!=") => true,
            TokenType::Punctuator if self.matches_punc("===") => true,
            TokenType::Punctuator if self.matches_punc("!==") => true,
            TokenType::Punctuator if self.matches_punc("+") => true,
            TokenType::Punctuator if self.matches_punc("-") => true,
            TokenType::Punctuator if self.matches_punc("*") => true,
            TokenType::Punctuator if self.matches_punc("/") => true,
            TokenType::Punctuator if self.matches_punc("%") => true,
            TokenType::Punctuator if self.matches_punc("<<") => true,
            TokenType::Punctuator if self.matches_punc(">>") => true,
            TokenType::Punctuator if self.matches_punc(">>>") => true,
            TokenType::Punctuator if self.matches_punc("&") => true,
            TokenType::Punctuator if self.matches_punc("|") => true,
            TokenType::Punctuator if self.matches_punc("^") => true,
            TokenType::Punctuator if self.matches_punc("&&") => true,
            TokenType::Punctuator if self.matches_punc("||") => true,
            TokenType::Keyword if self.matches_str("in") => true,
            TokenType::Keyword if self.matches_str("instanceof") => true,
            _ => false,
        }
        // match self.value {
        //     Value::Punc(Punctuator::LAngle)
        //     | Value::Punc(Punctuator::RAngle)
        //     | Value::Punc(Punctuator::LEqual)
        //     | Value::Punc(Punctuator::GEqual)
        //     | Value::Punc(Punctuator::Equal)
        //     | Value::Punc(Punctuator::NEqual)
        //     | Value::Punc(Punctuator::StrictEqual)
        //     | Value::Punc(Punctuator::StrictNEqual)
        //     | Value::Punc(Punctuator::Plus)
        //     | Value::Punc(Punctuator::Minus)
        //     | Value::Punc(Punctuator::Star)
        //     | Value::Punc(Punctuator::Slash)
        //     | Value::Punc(Punctuator::Mod)
        //     | Value::Punc(Punctuator::LShift)
        //     | Value::Punc(Punctuator::RShift)
        //     | Value::Punc(Punctuator::ZRShift)
        //     | Value::Punc(Punctuator::BitAnd)
        //     | Value::Punc(Punctuator::BitOr)
        //     | Value::Punc(Punctuator::BitXor)
        //     | Value::Punc(Punctuator::LogicalAnd)
        //     | Value::Punc(Punctuator::LogicalOr) => true,
        //     // | Value::Punc(Punctuator::Instanceof)
        //     _ => false,
        // }
    }

    fn is_unary_op(&self) -> bool {
        let unary_punc = match self.value {
            Value::Punc(Punctuator::Plus)
            | Value::Punc(Punctuator::Minus)
            | Value::Punc(Punctuator::Tilde)
            | Value::Punc(Punctuator::Bang) => true,
            _ => false,
        };
        let unary_keyword = match self.tokentype {
            TokenType::Keyword => match self.to_str() {
                "delete" | "void" | "typeof" => true,
                _ => false,
            },
            _ => false,
        };
        unary_punc || unary_keyword
    }
    fn is_update_op(&self) -> bool {
        match self.value {
            Value::Punc(Punctuator::Inc) | Value::Punc(Punctuator::Dec) => true,
            _ => false,
        }
    }

    fn precedence(&self, has_in: bool) -> usize {
        match self.tokentype {
            TokenType::Punctuator if self.matches_punc("||") => 1,
            TokenType::Punctuator if self.matches_punc("&&") => 2,
            TokenType::Punctuator if self.matches_punc("|") => 3,
            TokenType::Punctuator if self.matches_punc("^") => 4,
            TokenType::Punctuator if self.matches_punc("&") => 5,
            TokenType::Punctuator if self.matches_punc("==") => 6,
            TokenType::Punctuator if self.matches_punc("!=") => 6,
            TokenType::Punctuator if self.matches_punc("===") => 6,
            TokenType::Punctuator if self.matches_punc("!==") => 6,
            TokenType::Punctuator if self.matches_punc("<") => 7,
            TokenType::Punctuator if self.matches_punc(">") => 7,
            TokenType::Punctuator if self.matches_punc("<=") => 7,
            TokenType::Punctuator if self.matches_punc(">=") => 7,
            TokenType::Punctuator if self.matches_punc("<<") => 8,
            TokenType::Punctuator if self.matches_punc(">>") => 8,
            TokenType::Punctuator if self.matches_punc(">>>") => 8,
            TokenType::Punctuator if self.matches_punc("+") => 9,
            TokenType::Punctuator if self.matches_punc("-") => 9,
            TokenType::Punctuator if self.matches_punc("*") => 10,
            TokenType::Punctuator if self.matches_punc("/") => 10,
            TokenType::Punctuator if self.matches_punc("%") => 11,
            TokenType::Keyword if self.matches_str("instanceof") => 7,
            TokenType::Keyword if (self.matches_str("in") && has_in) => 7,
            _ => 0,
        }

        // match self.value {
        //     Value::Punc(Punctuator::LogicalOr) => 1,
        //     Value::Punc(Punctuator::LogicalAnd) => 2,
        //     Value::Punc(Punctuator::BitOr) => 3,
        //     Value::Punc(Punctuator::BitXor) => 4,
        //     Value::Punc(Punctuator::BitAnd) => 5,
        //     Value::Punc(Punctuator::Equal) => 6,
        //     Value::Punc(Punctuator::NEqual) => 6,
        //     Value::Punc(Punctuator::StrictEqual) => 6,
        //     Value::Punc(Punctuator::StrictNEqual) => 6,
        //     Value::Punc(Punctuator::LAngle) => 7,
        //     Value::Punc(Punctuator::RAngle) => 7,
        //     Value::Punc(Punctuator::LEqual) => 7,
        //     Value::Punc(Punctuator::GEqual) => 7,
        //     // Value::Punc(Punctuator::Instanceof) => 7,
        //     Value::Punc(Punctuator::LShift) => 8,
        //     Value::Punc(Punctuator::RShift) => 8,
        //     Value::Punc(Punctuator::ZRShift) => 8,
        //     Value::Punc(Punctuator::Plus) => 9,
        //     Value::Punc(Punctuator::Minus) => 9,
        //     Value::Punc(Punctuator::Star) => 10,
        //     Value::Punc(Punctuator::Slash) => 10,
        //     Value::Punc(Punctuator::Mod) => 11,
        //     _ => 0,
        // }
    }

    fn is_iteration_kw(&self) -> bool {
        match self.tokentype {
            TokenType::Keyword => {
                self.matches_str("do") | self.matches_str("while") | self.matches_str("for")
            }
            _ => false,
        }
    }

    fn is_hoistable_dclr_kw(&self) -> bool {
        match self.tokentype {
            TokenType::Identifier | TokenType::Keyword => {
                self.matches_str("async") | self.matches_str("function")
            }
            _ => false,
        }
    }

    fn is_lexical_dclr_kw(&self) -> bool {
        match self.tokentype {
            TokenType::Identifier | TokenType::Keyword => {
                self.matches_str("let") | self.matches_str("const")
            }
            _ => false,
        }
    }

    fn is_lexical_dclr_or_var_kw(&self) -> bool {
        match self.tokentype {
            TokenType::Identifier | TokenType::Keyword => {
                self.matches_str("let") | self.matches_str("const") | self.matches_str("var")
            }
            _ => false,
        }
    }

    fn is_dclr_kw(&self) -> bool {
        if self.is_hoistable_dclr_kw() || self.is_lexical_dclr_kw() {
            true
        } else {
            match self.tokentype {
                TokenType::Keyword if self.matches_str("class") => true,
                _ => false,
            }
        }
    }
}
