use nom;
use nom::types::CompleteStr;
use std::num;
use std::num::NonZeroU8;
use std::u8;

pub type Cell = Option<NonZeroU8>;
pub type Row = [Cell; 9];
pub type MutRow<'a> = [&'a mut Cell; 9];
pub type Column = [Cell; 3];
pub type MutColumn<'a> = [&'a mut Cell; 3];

#[derive(Debug, PartialEq, Eq)]
#[repr(C)]
pub struct Bankoplade {
    cells: [Row; 3],
}
assert_eq_size!(same_as_c; Bankoplade, [u8; 27]);

#[inline]
fn is_felt_char(c: char) -> bool {
    c.is_digit(10)
}

#[inline]
fn from_felt_str(s: CompleteStr) -> Result<Cell, num::ParseIntError> {
    let n = s.parse::<u8>()?;
    if n <= 90 {
        Ok(NonZeroU8::new(n))
    } else {
        Ok(None)
    }
}

named!(felt<CompleteStr, Cell >,
       map_res!(take_while_m_n!(2, 2, is_felt_char), from_felt_str)
);

named!(bankopladelinje<CompleteStr, Row>,
       do_parse!(
           felt0: terminated!(felt, char!(' ')) >>
           felt1: terminated!(felt, char!(' ')) >>
           felt2: terminated!(felt, char!(' ')) >>
           felt3: terminated!(felt, char!(' ')) >>
           felt4: terminated!(felt, char!(' ')) >>
           felt5: terminated!(felt, char!(' ')) >>
           felt6: terminated!(felt, char!(' ')) >>
           felt7: terminated!(felt, char!(' ')) >>
           felt8: terminated!(felt, char!('\n')) >>
           ([felt0, felt1, felt2, felt3, felt4, felt5, felt6, felt7, felt8])
       )
);

named!(bankoplade<CompleteStr, Bankoplade>,
       do_parse!(
           linje0: bankopladelinje >>
           linje1: bankopladelinje >>
           linje2: bankopladelinje >>
           char!('\n') >>
           (Bankoplade { cells: [linje0, linje1, linje2] })
       )
);

named!(bankoplade1<CompleteStr, Bankoplade>, terminated!(bankoplade, eof!()));
named!(bankoplader<CompleteStr, Vec<Bankoplade>>, terminated!(many1!(bankoplade), eof!()));

impl Bankoplade {
    #[inline]
    pub fn zero_plade() -> Bankoplade {
        Bankoplade {
            cells: [[None; 9]; 3],
        }
    }

    #[inline]
    pub fn print(&self) {
        for row in self.rows() {
            let mut first = true;
            for value in row.iter() {
                if first {
                    first = false;
                } else {
                    print!(" ");
                }
                print!(
                    "{}",
                    value
                        .map(|n| format!("{:02}", n))
                        .unwrap_or("00".to_string())
                );
            }
            println!();
        }
        println!();
    }

    #[inline]
    pub fn rows<'a>(&'a self) -> impl Iterator<Item = Row> + 'a {
        self.cells.iter().cloned()
    }

    #[inline]
    pub fn rows_mut<'a>(&'a mut self) -> impl Iterator<Item = MutRow<'a>> + 'a {
        self.cells.iter_mut().map(|row| {
            let &mut [ref mut row0, ref mut row1, ref mut row2, ref mut row3, ref mut row4, ref mut row5, ref mut row6, ref mut row7, ref mut row8] =
                &mut *row;
            [row0, row1, row2, row3, row4, row5, row6, row7, row8]
        })
    }

    #[inline]
    pub fn columns<'a>(&'a self) -> impl Iterator<Item = Column> + 'a {
        self.cells[0]
            .iter()
            .zip(self.cells[1].iter())
            .zip(self.cells[2].iter())
            .map(|((a, b), c)| [*a, *b, *c])
    }

    #[inline]
    pub fn columns_mut<'a>(&'a mut self) -> impl Iterator<Item = MutColumn<'a>> + 'a {
        let &mut [ref mut row0, ref mut row1, ref mut row2] = &mut self.cells;
        row0.iter_mut()
            .zip(row1.iter_mut())
            .zip(row2.iter_mut())
            .map(|((a, b), c)| [a, b, c])
    }

    #[inline]
    pub fn parse1(s: &str) -> Result<Bankoplade, nom::Err<CompleteStr>> {
        Ok(bankoplade1(CompleteStr(s))?.1)
    }

    #[inline]
    pub fn parse_many(s: &str) -> Result<Vec<Bankoplade>, nom::Err<CompleteStr>> {
        Ok(bankoplader(CompleteStr(s))?.1)
    }

    #[inline]
    pub fn is_valid(&self) -> bool {
        for row in self.rows() {
            // Check that all rows have the proper amount of numbers
            if row.iter().filter_map(|&value| value).count() != 5 {
                return false;
            }

            // Check that all numbers are within the ranges required
            for (col, &value) in row.iter().enumerate() {
                let col = col as u8;
                if let Some(value) = value {
                    if col == 0 {
                        if !(value.get() < 10) {
                            return false;
                        }
                    } else if 1 <= col && col < 8 {
                        if !(10 * col <= value.get() && value.get() < 10 * (col + 1)) {
                            return false;
                        }
                    } else {
                        if !(80 <= value.get() && value.get() <= 90) {
                            return false;
                        }
                    }
                }
            }
        }

        for column in self.columns() {
            let mut last = None;

            for value in column.iter().cloned().filter(Option::is_some) {
                // Check that all columns are increasing
                if last <= value {
                    last = value;
                } else {
                    return false;
                }
            }

            // Check that all columns contain at least one value
            if last.is_none() {
                return false;
            }
        }

        true
    }
}
