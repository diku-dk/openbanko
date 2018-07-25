use combinations::Combination;
use std::collections::BTreeMap;
use std::u64;
use typenum::consts::*;

struct CompactIndexes(u32);

impl CompactIndexes {
    #[inline]
    fn expand(&self) -> [u8; 9] {
        let mut value = self.0;
        let mut result = [0u8; 9];

        for result_value in result.iter_mut() {
            *result_value = (value & 7) as u8;
            value = value >> 3;
        }

        result
    }
}

pub struct OffsetTable {
    table: BTreeMap<u64, CompactIndexes>,
}

impl OffsetTable {
    #[inline]
    pub fn get(&self, offset: u64) -> (u64, [u8; 9]) {
        let (&result_offset, indexes) = self
            .table
            .range(..=offset)
            .next_back()
            .expect("Invalid index");
        (offset - result_offset, indexes.expand())
    }
}

impl Default for OffsetTable {
    #[inline]
    fn default() -> OffsetTable {
        let mut total_count = 0;
        let mut table = BTreeMap::new();

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

        for row0 in Combination::<U8, U5>::new() {
            for row1 in Combination::<U8, U5>::new() {
                for row2 in Combination::<U8, U5>::new() {
                    let mut count = [0; 9];
                    let mut compact_index = 0;
                    let mut values = [[false; 9]; 3];

                    for &value in &row0 {
                        count[value as usize] += 1;
                        values[0][value as usize] = true;
                    }

                    for &value in &row1 {
                        count[value as usize] += 1;
                        values[1][value as usize] = true;
                    }

                    for &value in &row2 {
                        count[value as usize] += 1;
                        values[2][value as usize] = true;
                    }

                    if count.iter().any(|&n| n == 0) {
                        continue;
                    }

                    for (row_ndx, row) in values.iter().enumerate() {
                        for (col_ndx, &value) in row.iter().enumerate() {
                            if value {
                                compact_index |= 1 << (row_ndx + 3 * col_ndx);
                            }
                        }
                    }

                    table.insert(total_count, CompactIndexes(compact_index));

                    total_count += counts_col0[count[0]]
                        * (1..8).map(|n| counts_col_most[count[n]]).product::<u64>()
                        * counts_col8[count[8]];
                }
            }
        }

        OffsetTable { table }
    }
}
