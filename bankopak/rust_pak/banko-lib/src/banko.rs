use nom;
use nom::types::CompleteStr;
use std::num;
use std::num::NonZeroU8;
use std::u8;

#[derive(Debug, PartialEq, Eq)]
#[repr(C)]
pub struct Bankoplade {
    pub rows: [[Option<NonZeroU8>; 9]; 3],
}
assert_eq_size!(same_as_c; Bankoplade, [u8; 27]);

#[inline]
fn is_felt_char(c: char) -> bool {
    c.is_digit(10)
}

#[inline]
fn from_felt_str(s: CompleteStr) -> Result<Option<NonZeroU8>, num::ParseIntError> {
    let n = s.parse::<u8>()?;
    if n <= 90 {
        Ok(NonZeroU8::new(n))
    } else {
        Ok(None)
    }
}

named!(felt<CompleteStr, Option<NonZeroU8> >,
       map_res!(take_while_m_n!(2, 2, is_felt_char), from_felt_str)
);

named!(bankopladelinje<CompleteStr, [Option<NonZeroU8>; 9]>,
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
           (Bankoplade { rows: [linje0, linje1, linje2] })
       )
);

named!(bankoplade1<CompleteStr, Bankoplade>, terminated!(bankoplade, eof!()));
named!(bankoplader<CompleteStr, Vec<Bankoplade>>, terminated!(many1!(bankoplade), eof!()));

impl Bankoplade {
    pub fn zero_plade() -> Bankoplade {
        Bankoplade {
            rows: [[None; 9]; 3],
        }
    }

    #[inline]
    pub fn print(&self) {
        for row in &self.rows {
            let mut first = true;
            for value in row {
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
    pub fn parse1(s: &str) -> Result<Bankoplade, nom::Err<CompleteStr>> {
        Ok(bankoplade1(CompleteStr(s))?.1)
    }

    #[inline]
    pub fn parse_many(s: &str) -> Result<Vec<Bankoplade>, nom::Err<CompleteStr>> {
        Ok(bankoplader(CompleteStr(s))?.1)
    }

    #[inline]
    pub fn is_valid(&self) -> bool {
        for row in &self.rows {
            if row.iter().filter_map(|&value| value).count() != 5 {
                return false;
            }

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

        for ((&col1, &col2), &col3) in self.rows[0]
            .iter()
            .zip(self.rows[1].iter())
            .zip(self.rows[2].iter())
        {
            let mut last = 0;

            for &value in &[col1, col2, col3] {
                if let Some(value) = value {
                    if value.get() <= last {
                        return false;
                    }
                    last = value.get();
                }
            }

            if last == 0 {
                return false;
            }
        }

        true
    }
}
