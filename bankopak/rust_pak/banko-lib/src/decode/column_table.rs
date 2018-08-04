use combinations::Combination;
use generic_array::sequence::GenericSequence;
use generic_array::GenericArray;
use std::num::NonZeroU8;
use typenum::consts::*;

pub struct ColumnTable {
    table: GenericArray<GenericArray<Box<[[Option<NonZeroU8>; 3]]>, U7>, U9>,
}

impl ColumnTable {
    #[inline]
    pub fn get(&self, col: usize, bit_index: u8) -> Option<&[[Option<NonZeroU8>; 3]]> {
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
        fn gen(col: usize, bit_index: usize) -> Box<[[Option<NonZeroU8>; 3]]> {
            fn fixup((a, b, c): (u8, u8, u8)) -> [Option<NonZeroU8>; 3] {
                [NonZeroU8::new(a), NonZeroU8::new(b), NonZeroU8::new(c)]
            }

            match (col, bit_index + 1) {
                (0, 0b001) => Combination::<U8, U1>::new()
                    .map(|v| (v[0] + 1, 0, 0))
                    .map(fixup)
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
                (0, 0b010) => Combination::<U8, U1>::new()
                    .map(|v| (0, v[0] + 1, 0))
                    .map(fixup)
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
                (0, 0b011) => Combination::<U8, U2>::new()
                    .map(|v| (v[0] + 1, v[1] + 1, 0))
                    .map(fixup)
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
                (0, 0b100) => Combination::<U8, U1>::new()
                    .map(|v| (0, 0, v[0] + 1))
                    .map(fixup)
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
                (0, 0b101) => Combination::<U8, U2>::new()
                    .map(|v| (v[0] + 1, 0, v[1] + 1))
                    .map(fixup)
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
                (0, 0b110) => Combination::<U8, U2>::new()
                    .map(|v| (0, v[0] + 1, v[1] + 1))
                    .map(fixup)
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
                (0, 0b111) => Combination::<U8, U3>::new()
                    .map(|v| (v[0] + 1, v[1] + 1, v[2] + 1))
                    .map(fixup)
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),

                (8, 0b001) => Combination::<U10, U1>::new()
                    .map(|v| (v[0] + 80, 0, 0))
                    .map(fixup)
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
                (8, 0b010) => Combination::<U10, U1>::new()
                    .map(|v| (0, v[0] + 80, 0))
                    .map(fixup)
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
                (8, 0b011) => Combination::<U10, U2>::new()
                    .map(|v| (v[0] + 80, v[1] + 80, 0))
                    .map(fixup)
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
                (8, 0b100) => Combination::<U10, U1>::new()
                    .map(|v| (0, 0, v[0] + 80))
                    .map(fixup)
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
                (8, 0b101) => Combination::<U10, U2>::new()
                    .map(|v| (v[0] + 80, 0, v[1] + 80))
                    .map(fixup)
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
                (8, 0b110) => Combination::<U10, U2>::new()
                    .map(|v| (0, v[0] + 80, v[1] + 80))
                    .map(fixup)
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
                (8, 0b111) => Combination::<U10, U3>::new()
                    .map(|v| (v[0] + 80, v[1] + 80, v[2] + 80))
                    .map(fixup)
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),

                (n, 0b001) => Combination::<U9, U1>::new()
                    .map(|v| (v[0] + 10 * n as u8, 0, 0))
                    .map(fixup)
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
                (n, 0b010) => Combination::<U9, U1>::new()
                    .map(|v| (0, v[0] + 10 * n as u8, 0))
                    .map(fixup)
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
                (n, 0b011) => Combination::<U9, U2>::new()
                    .map(|v| (v[0] + 10 * n as u8, v[1] + 10 * n as u8, 0))
                    .map(fixup)
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
                (n, 0b100) => Combination::<U9, U1>::new()
                    .map(|v| (0, 0, v[0] + 10 * n as u8))
                    .map(fixup)
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
                (n, 0b101) => Combination::<U9, U2>::new()
                    .map(|v| (v[0] + 10 * n as u8, 0, v[1] + 10 * n as u8))
                    .map(fixup)
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
                (n, 0b110) => Combination::<U9, U2>::new()
                    .map(|v| (0, v[0] + 10 * n as u8, v[1] + 10 * n as u8))
                    .map(fixup)
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
                (n, 0b111) => Combination::<U9, U3>::new()
                    .map(|v| {
                        (
                            v[0] + 10 * n as u8,
                            v[1] + 10 * n as u8,
                            v[2] + 10 * n as u8,
                        )
                    })
                    .map(fixup)
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
                _ => panic!("impossible {} {}", col, bit_index),
            }
        }

        let table: GenericArray<GenericArray<Box<[[Option<NonZeroU8>; 3]]>, U7>, U9> =
            GenericArray::generate(|col| GenericArray::generate(|bit_index| gen(col, bit_index)));

        ColumnTable { table }
    }
}
