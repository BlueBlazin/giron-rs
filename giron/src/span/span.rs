use serde::Serialize;

#[derive(Serialize, Debug, Clone)]
#[serde(tag = "type")]
pub struct Marker {
    pub line: usize,
    pub col: usize,
    pub idx: usize,
}

#[derive(Serialize, Debug, Clone)]
#[serde(tag = "type")]
pub struct Span {
    pub start: Marker,
    pub end: Marker,
}
