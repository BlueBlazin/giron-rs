use crate::errors::errors::Error;

pub type Result<T> = std::result::Result<T, Error>;
