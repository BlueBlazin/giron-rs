use std::collections::VecDeque;

pub struct Reader<I> {
    pub source: I,
    pub buf: VecDeque<char>,
}

impl<I> Reader<I>
where
    I: Iterator<Item = char>,
{
    pub fn new(source: I) -> Self {
        Reader {
            source,
            buf: VecDeque::with_capacity(3),
        }
    }

    pub fn lookahead(&mut self, n: usize) -> Option<char> {
        for _ in self.buf.len()..(n + 1) {
            match self.source.next() {
                Some(item) => self.buf.push_back(item),
                None => return None,
            }
        }

        self.buf.get(n).map(|&x| x)
    }
}

impl<I> Iterator for Reader<I>
where
    I: Iterator<Item = char>,
{
    type Item = char;

    fn next(&mut self) -> Option<char> {
        self.buf.pop_front().or_else(|| self.source.next())
    }
}
