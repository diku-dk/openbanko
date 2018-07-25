use combinations::Combination;
use std::ops;
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

impl ops::Index<[u8; 3]> for ColumnTable0 {
    type Output = (u8, u8);
    fn index(&self, index: [u8; 3]) -> &(u8, u8) {
        let result = &self.table[index[0] as usize][index[1] as usize][index[2] as usize];

        if result.0 == u8::MAX {
            panic!("Invalid index {:?}", index);
        }

        result
    }
}

impl ops::Index<[u8; 3]> for ColumnTableN {
    type Output = (u8, u8);
    fn index(&self, index: [u8; 3]) -> &(u8, u8) {
        let result = &self.table[index[0] as usize][index[1] as usize][index[2] as usize];

        if result.0 == u8::MAX {
            panic!("Invalid index {:?}", index);
        }

        result
    }
}

impl ops::Index<[u8; 3]> for ColumnTable8 {
    type Output = (u8, u8);
    fn index(&self, index: [u8; 3]) -> &(u8, u8) {
        let result = &self.table[index[0] as usize][index[1] as usize][index[2] as usize];

        if result.0 == u8::MAX {
            panic!("Invalid index {:?}", index);
        }

        result
    }
}

impl ops::Index<(u8, [Option<u8>; 3])> for ColumnTable {
    type Output = (u8, u8);
    #[inline]
    fn index(&self, index: (u8, [Option<u8>; 3])) -> &(u8, u8) {
        let (table, index) = index;

        let sub = match table {
            0 => 0,
            n if n <= 8 => 10 * n - 1,
            _ => panic!("Invalid index {:?}", (table, index)),
        };

        let index = [
            index[0]
                .map(|value| {
                    let value = value.checked_sub(sub).expect("Invalid index");
                    assert!(value > 0, "Invalid index");
                    value
                })
                .unwrap_or(0),
            index[1]
                .map(|value| {
                    let value = value.checked_sub(sub).expect("Invalid index");
                    assert!(value > 0, "Invalid index");
                    value
                })
                .unwrap_or(0),
            index[2]
                .map(|value| {
                    let value = value.checked_sub(sub).expect("Invalid index");
                    assert!(value > 0, "Invalid index");
                    value
                })
                .unwrap_or(0),
        ];

        match table {
            0 => &self.table0[index],
            8 => &self.table8[index],
            _ => &self.tablen[index],
        }
    }
}

impl Default for ColumnTable0 {
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
