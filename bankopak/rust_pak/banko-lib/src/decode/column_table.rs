use combinations::Combination;
use generic_array::sequence::GenericSequence;
use generic_array::GenericArray;
use typenum::consts::*;

pub struct ColumnTable {
    table: GenericArray<GenericArray<Box<[(u8, u8, u8)]>, U7>, U9>,
}

impl ColumnTable {
    #[inline]
    pub fn get(&self, col: usize, bit_index: u8) -> Option<&[(u8, u8, u8)]> {
        if col >= 9 || bit_index == 0 || bit_index >= 8 {
            None
        } else {
            Some(&self.table[col][bit_index as usize - 1])
        }
    }
}

impl Default for ColumnTable {
    #[inline]
    fn default() -> ColumnTable {
        fn gen(col: usize, bit_index: usize) -> Box<[(u8, u8, u8)]> {
            match (col, bit_index) {
                (0, 0) => Combination::<U8, U1>::new()
                    .map(|v| (v[0] + 1, 0, 0))
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
                (0, 1) => Combination::<U8, U1>::new()
                    .map(|v| (0, v[0] + 1, 0))
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
                (0, 2) => Combination::<U8, U2>::new()
                    .map(|v| (v[0] + 1, v[1] + 1, 0))
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
                (0, 3) => Combination::<U8, U1>::new()
                    .map(|v| (0, 0, v[0] + 1))
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
                (0, 4) => Combination::<U8, U2>::new()
                    .map(|v| (v[0] + 1, 0, v[1] + 1))
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
                (0, 5) => Combination::<U8, U2>::new()
                    .map(|v| (0, v[0] + 1, v[1] + 1))
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
                (0, 6) => Combination::<U8, U3>::new()
                    .map(|v| (v[0] + 1, v[1] + 1, v[2] + 1))
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),

                (8, 0) => Combination::<U10, U1>::new()
                    .map(|v| (v[0] + 80, 0, 0))
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
                (8, 1) => Combination::<U10, U1>::new()
                    .map(|v| (0, v[0] + 80, 0))
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
                (8, 2) => Combination::<U10, U2>::new()
                    .map(|v| (v[0] + 80, v[1] + 80, 0))
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
                (8, 3) => Combination::<U10, U1>::new()
                    .map(|v| (0, 0, v[0] + 80))
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
                (8, 4) => Combination::<U10, U2>::new()
                    .map(|v| (v[0] + 80, 0, v[1] + 80))
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
                (8, 5) => Combination::<U10, U2>::new()
                    .map(|v| (0, v[0] + 80, v[1] + 80))
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
                (8, 6) => Combination::<U10, U3>::new()
                    .map(|v| (v[0] + 80, v[1] + 80, v[2] + 80))
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),

                (n, 0) => Combination::<U9, U1>::new()
                    .map(|v| (v[0] + 10 * n as u8, 0, 0))
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
                (n, 1) => Combination::<U9, U1>::new()
                    .map(|v| (0, v[0] + 10 * n as u8, 0))
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
                (n, 2) => Combination::<U9, U2>::new()
                    .map(|v| (v[0] + 10 * n as u8, v[1] + 10 * n as u8, 0))
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
                (n, 3) => Combination::<U9, U1>::new()
                    .map(|v| (0, 0, v[0] + 10 * n as u8))
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
                (n, 4) => Combination::<U9, U2>::new()
                    .map(|v| (v[0] + 10 * n as u8, 0, v[1] + 10 * n as u8))
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
                (n, 5) => Combination::<U9, U2>::new()
                    .map(|v| (0, v[0] + 10 * n as u8, v[1] + 10 * n as u8))
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
                (n, 6) => Combination::<U9, U3>::new()
                    .map(|v| {
                        (
                            v[0] + 10 * n as u8,
                            v[1] + 10 * n as u8,
                            v[2] + 10 * n as u8,
                        )
                    })
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
                _ => panic!("impossible {} {}", col, bit_index),
            }
        }

        let table: GenericArray<GenericArray<Box<[(u8, u8, u8)]>, U7>, U9> =
            GenericArray::generate(|col| GenericArray::generate(|bit_index| gen(col, bit_index)));

        ColumnTable { table }
    }
}
