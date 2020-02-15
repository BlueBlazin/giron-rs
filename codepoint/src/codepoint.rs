use ucd::Codepoint;

pub trait CodePoint {
    fn is_whitespace(self) -> bool;
    fn is_line_terminator(self) -> bool;
    fn is_identifier_start(self) -> bool;
    fn is_identifier_continue(self) -> bool;
    fn is_decimal_digit(self) -> bool;
    fn is_nonzero_digit(self) -> bool;
    fn is_octal_digit(self) -> bool;
    fn is_hex_digit(self) -> bool;
    fn is_single_escape_char(self) -> bool;
    fn unescape(self) -> Self;
    fn is_single_char_punctuator(self) -> bool;
}

impl CodePoint for char {
    fn is_whitespace(self) -> bool {
        match self {
            '\u{0009}' | '\u{000B}' | '\u{000C}' | '\u{0020}' | '\u{00A0}' | '\u{FEFF}'
            | '\u{1680}' | '\u{2000}' | '\u{2001}' | '\u{2002}' | '\u{2003}' | '\u{2004}'
            | '\u{2005}' | '\u{2006}' | '\u{2007}' | '\u{2008}' | '\u{2009}' | '\u{200A}'
            | '\u{202F}' | '\u{205F}' | '\u{3000}' => true,
            _ => false,
        }
    }

    fn is_line_terminator(self) -> bool {
        match self {
            '\n' | '\r' | '\u{2028}' | '\u{2029}' => true,
            _ => false,
        }
    }

    fn is_identifier_start(self) -> bool {
        // $ | _ | \ | A..Z | a..z | other ID_START
        match self {
            '$' | '_' | 'A'..='Z' | 'a'..='z' => true,
            _ => self.is_id_start(),
        }
    }

    fn is_identifier_continue(self) -> bool {
        match self {
            '$' | '_' | 'A'..='Z' | 'a'..='z' | '0'..='9' | '\u{005C}' => true,
            _ => self.is_id_continue(),
        }
    }

    fn is_decimal_digit(self) -> bool {
        match self {
            '0'..='9' => true,
            _ => false,
        }
    }

    fn is_nonzero_digit(self) -> bool {
        match self {
            '1'..='9' => true,
            _ => false,
        }
    }

    fn is_octal_digit(self) -> bool {
        match self {
            '0'..='7' => true,
            _ => false,
        }
    }

    fn is_hex_digit(self) -> bool {
        match self {
            '0'..='9' | 'a'..='f' | 'A'..='F' => true,
            _ => false,
        }
    }

    fn is_single_escape_char(self) -> bool {
        match self {
            '\'' | '"' | '\\' | 'b' | 'f' | 'n' | 'r' | 't' | 'v' => true,
            _ => false,
        }
    }

    fn unescape(self) -> Self {
        match self {
            'b' => '\x08',
            'f' => '\x0C',
            'n' => '\n',
            'r' => '\r',
            't' => '\t',
            'v' => '\x0B',
            _ => self,
        }
    }

    fn is_single_char_punctuator(self) -> bool {
        match self {
            '{' | '(' | '[' | ';' | ',' | ']' | ':' | '?' | '~' | ')' => true,
            _ => false,
        }
    }
}
