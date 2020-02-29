use crate::token::punctuators::Punctuator;
use serde::Serialize;

#[derive(Serialize, Debug, PartialEq, Clone)]
#[serde(untagged)]
pub enum Value {
    Str(String),
    Num(f64),
    Punc(Punctuator),
    Null,
}

impl From<&Value> for String {
    fn from(value: &Value) -> Self {
        match value {
            Value::Str(s) => s.clone(),
            Value::Num(x) => x.to_string(),
            Value::Punc(p) => p.to_string(),
            Value::Null => String::from("null"),
        }
    }
}

impl Value {
    pub fn string(self) -> Result<String, ()> {
        match self {
            Value::Str(val) => Ok(val),
            _ => Err(()),
        }
    }

    pub fn punc(self) -> Result<Punctuator, ()> {
        match self {
            Value::Punc(val) => Ok(val),
            _ => Err(()),
        }
    }
}
