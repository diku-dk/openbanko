use combinations::Combination;
use std::ops;
use std::u64;
use typenum::consts::*;

pub struct LayoutTable {
    table: Box<[[[u64; 126]; 126]; 126]>,
}

impl ops::Index<[u8; 3]> for LayoutTable {
    type Output = u64;
    fn index(&self, index: [u8; 3]) -> &u64 {
        let result = &self.table[index[0] as usize][index[1] as usize][index[2] as usize];

        if *result == u64::MAX {
            panic!("Invalid index {:?}", index);
        }

        result
    }
}

impl Default for LayoutTable {
    fn default() -> LayoutTable {
        let mut table = box [[[u64::MAX; 126]; 126]; 126];

        let mut total_count = 0;

        let counts_col0 = [
            Combination::<U8, U0>::total_count(),
            Combination::<U8, U1>::total_count(),
            Combination::<U8, U2>::total_count(),
            Combination::<U8, U3>::total_count(),
        ];

        let counts_col_most = [
            Combination::<U9, U0>::total_count(),
            Combination::<U9, U1>::total_count(),
            Combination::<U9, U2>::total_count(),
            Combination::<U9, U3>::total_count(),
        ];

        let counts_col8 = [
            Combination::<U10, U0>::total_count(),
            Combination::<U10, U1>::total_count(),
            Combination::<U10, U2>::total_count(),
            Combination::<U10, U3>::total_count(),
        ];

        for (ndx0, row0) in Combination::<U8, U5>::new().enumerate() {
            for (ndx1, row1) in Combination::<U8, U5>::new().enumerate() {
                for (ndx2, row2) in Combination::<U8, U5>::new().enumerate() {
                    let mut count = [0; 9];

                    for &value in row0.iter().chain(row1.iter()).chain(row2.iter()) {
                        count[value as usize] += 1;
                    }

                    if count.iter().any(|&n| n == 0) {
                        continue;
                    }

                    table[ndx0][ndx1][ndx2] = total_count;
                    total_count += counts_col0[count[0]]
                        * (1..8).map(|n| counts_col_most[count[n]]).product::<u64>()
                        * counts_col8[count[8]];
                }
            }
        }

        LayoutTable { table }
    }
}
