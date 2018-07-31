use combinations::Combination;
use std::u8;
use typenum::consts::*;

struct ColumnTable0 {
    table: [[[(u8, u8); 10]; 10]; 10],
}

struct ColumnTableN {
    table: [[[(u8, u8); 11]; 11]; 11],
}

struct ColumnTable8 {
    table: [[[(u8, u8); 12]; 12]; 12],
}

#[derive(Default)]
pub struct ColumnTable {
    table0: ColumnTable0,
    tablen: ColumnTableN,
    table8: ColumnTable8,
}

impl ColumnTable0 {
    #[inline]
    fn get(&self, row0: u8, row1: u8, row2: u8) -> Option<(u8, u8)> {
        if row0 >= 10 || row1 >= 10 || row2 >= 10 {
            None
        } else {
            let result = self.table[row0 as usize][row1 as usize][row2 as usize];

            if result.0 == u8::MAX {
                None
            } else {
                Some(result)
            }
        }
    }
}

impl ColumnTableN {
    #[inline]
    fn get(&self, row0: u8, row1: u8, row2: u8) -> Option<(u8, u8)> {
        if row0 >= 11 || row1 >= 11 || row2 >= 11 {
            None
        } else {
            let result = self.table[row0 as usize][row1 as usize][row2 as usize];

            if result.0 == u8::MAX {
                None
            } else {
                Some(result)
            }
        }
    }
}

impl ColumnTable8 {
    #[inline]
    fn get(&self, row0: u8, row1: u8, row2: u8) -> Option<(u8, u8)> {
        if row0 >= 12 || row1 >= 12 || row2 >= 12 {
            None
        } else {
            let result = self.table[row0 as usize][row1 as usize][row2 as usize];

            if result.0 == u8::MAX {
                None
            } else {
                Some(result)
            }
        }
    }
}

impl ColumnTable {
    #[inline]
    pub fn get(
        &self,
        col: usize,
        row0: Option<u8>,
        row1: Option<u8>,
        row2: Option<u8>,
    ) -> Option<(u8, u8)> {
        let sub = match col {
            0 => 0,
            n if n <= 8 => 10 * n as u8 - 1,
            _ => panic!("Invalid index {:?}", (col, row0, row1, row2)),
        };

        let row0 = if let Some(row0) = row0 {
            let row0 = row0.checked_sub(sub)?;
            if row0 == 0 {
                return None;
            }
            row0
        } else {
            0
        };

        let row1 = if let Some(row1) = row1 {
            let row1 = row1.checked_sub(sub)?;
            if row1 == 0 {
                return None;
            }
            row1
        } else {
            0
        };

        let row2 = if let Some(row2) = row2 {
            let row2 = row2.checked_sub(sub)?;
            if row2 == 0 {
                return None;
            }
            row2
        } else {
            0
        };

        match col {
            0 => self.table0.get(row0, row1, row2),
            8 => self.table8.get(row0, row1, row2),
            _ => self.tablen.get(row0, row1, row2),
        }
    }
}

impl Default for ColumnTable0 {
    #[inline]
    fn default() -> ColumnTable0 {
        let mut table = [[[(u8::MAX, u8::MAX); 10]; 10]; 10];

        for (comb_ndx, comb) in Combination::<U8, U1>::new().enumerate() {
            let comb_ndx = comb_ndx as u8;
            let index0 = [comb[0] as usize + 1, 0, 0];
            let index1 = [0, comb[0] as usize + 1, 0];
            let index2 = [0, 0, comb[0] as usize + 1];

            table[index0[0]][index0[1]][index0[2]] =
                (comb_ndx, Combination::<U8, U1>::total_count() as u8);
            table[index1[0]][index1[1]][index1[2]] =
                (comb_ndx, Combination::<U8, U1>::total_count() as u8);
            table[index2[0]][index2[1]][index2[2]] =
                (comb_ndx, Combination::<U8, U1>::total_count() as u8);
        }

        for (comb_ndx, comb) in Combination::<U8, U2>::new().enumerate() {
            let comb_ndx = comb_ndx as u8;
            let index0 = [0, comb[0] as usize + 1, comb[1] as usize + 1];
            let index1 = [comb[0] as usize + 1, 0, comb[1] as usize + 1];
            let index2 = [comb[0] as usize + 1, comb[1] as usize + 1, 0];

            table[index0[0]][index0[1]][index0[2]] =
                (comb_ndx, Combination::<U8, U2>::total_count() as u8);
            table[index1[0]][index1[1]][index1[2]] =
                (comb_ndx, Combination::<U8, U2>::total_count() as u8);
            table[index2[0]][index2[1]][index2[2]] =
                (comb_ndx, Combination::<U8, U2>::total_count() as u8);
        }

        for (comb_ndx, comb) in Combination::<U8, U3>::new().enumerate() {
            let comb_ndx = comb_ndx as u8;
            let index = [
                comb[0] as usize + 1,
                comb[1] as usize + 1,
                comb[2] as usize + 1,
            ];

            table[index[0]][index[1]][index[2]] =
                (comb_ndx, Combination::<U8, U3>::total_count() as u8);
        }

        ColumnTable0 { table }
    }
}

impl Default for ColumnTableN {
    #[inline]
    fn default() -> ColumnTableN {
        let mut table = [[[(u8::MAX, u8::MAX); 11]; 11]; 11];

        for (comb_ndx, comb) in Combination::<U9, U1>::new().enumerate() {
            let comb_ndx = comb_ndx as u8;
            let index0 = [comb[0] as usize + 1, 0, 0];
            let index1 = [0, comb[0] as usize + 1, 0];
            let index2 = [0, 0, comb[0] as usize + 1];

            table[index0[0]][index0[1]][index0[2]] =
                (comb_ndx, Combination::<U9, U1>::total_count() as u8);
            table[index1[0]][index1[1]][index1[2]] =
                (comb_ndx, Combination::<U9, U1>::total_count() as u8);
            table[index2[0]][index2[1]][index2[2]] =
                (comb_ndx, Combination::<U9, U1>::total_count() as u8);
        }

        for (comb_ndx, comb) in Combination::<U9, U2>::new().enumerate() {
            let comb_ndx = comb_ndx as u8;
            let index0 = [0, comb[0] as usize + 1, comb[1] as usize + 1];
            let index1 = [comb[0] as usize + 1, 0, comb[1] as usize + 1];
            let index2 = [comb[0] as usize + 1, comb[1] as usize + 1, 0];

            table[index0[0]][index0[1]][index0[2]] =
                (comb_ndx, Combination::<U9, U2>::total_count() as u8);
            table[index1[0]][index1[1]][index1[2]] =
                (comb_ndx, Combination::<U9, U2>::total_count() as u8);
            table[index2[0]][index2[1]][index2[2]] =
                (comb_ndx, Combination::<U9, U2>::total_count() as u8);
        }

        for (comb_ndx, comb) in Combination::<U9, U3>::new().enumerate() {
            let comb_ndx = comb_ndx as u8;
            let index = [
                comb[0] as usize + 1,
                comb[1] as usize + 1,
                comb[2] as usize + 1,
            ];

            table[index[0]][index[1]][index[2]] =
                (comb_ndx, Combination::<U9, U3>::total_count() as u8);
        }

        ColumnTableN { table }
    }
}

impl Default for ColumnTable8 {
    #[inline]
    fn default() -> ColumnTable8 {
        let mut table = [[[(u8::MAX, u8::MAX); 12]; 12]; 12];

        for (comb_ndx, comb) in Combination::<U10, U1>::new().enumerate() {
            let comb_ndx = comb_ndx as u8;
            let index0 = [comb[0] as usize + 1, 0, 0];
            let index1 = [0, comb[0] as usize + 1, 0];
            let index2 = [0, 0, comb[0] as usize + 1];

            table[index0[0]][index0[1]][index0[2]] =
                (comb_ndx, Combination::<U10, U1>::total_count() as u8);
            table[index1[0]][index1[1]][index1[2]] =
                (comb_ndx, Combination::<U10, U1>::total_count() as u8);
            table[index2[0]][index2[1]][index2[2]] =
                (comb_ndx, Combination::<U10, U1>::total_count() as u8);
        }

        for (comb_ndx, comb) in Combination::<U10, U2>::new().enumerate() {
            let comb_ndx = comb_ndx as u8;
            let index0 = [0, comb[0] as usize + 1, comb[1] as usize + 1];
            let index1 = [comb[0] as usize + 1, 0, comb[1] as usize + 1];
            let index2 = [comb[0] as usize + 1, comb[1] as usize + 1, 0];

            table[index0[0]][index0[1]][index0[2]] =
                (comb_ndx, Combination::<U10, U2>::total_count() as u8);
            table[index1[0]][index1[1]][index1[2]] =
                (comb_ndx, Combination::<U10, U2>::total_count() as u8);
            table[index2[0]][index2[1]][index2[2]] =
                (comb_ndx, Combination::<U10, U2>::total_count() as u8);
        }

        for (comb_ndx, comb) in Combination::<U10, U3>::new().enumerate() {
            let comb_ndx = comb_ndx as u8;
            let index = [
                comb[0] as usize + 1,
                comb[1] as usize + 1,
                comb[2] as usize + 1,
            ];

            table[index[0]][index[1]][index[2]] =
                (comb_ndx, Combination::<U10, U3>::total_count() as u8);
        }

        ColumnTable8 { table }
    }
}
