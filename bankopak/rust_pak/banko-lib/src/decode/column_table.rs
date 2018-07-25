use combinations::Combination;
use generic_array::sequence::GenericSequence;
use generic_array::GenericArray;
use std::ops;
use typenum::consts::*;

pub struct ColumnTable {
    table: GenericArray<GenericArray<Option<Box<[(u8, u8, u8)]>>, U8>, U9>,
}

impl ops::Index<[u8; 2]> for ColumnTable {
    type Output = [(u8, u8, u8)];
    #[inline]
    fn index(&self, index: [u8; 2]) -> &[(u8, u8, u8)] {
        if index[1] == 0 {
            panic!("Invalid index {:?}", index);
        }
        self.table[index[0] as usize][index[1] as usize]
            .as_ref()
            .unwrap()
    }
}

impl Default for ColumnTable {
    #[inline]
    fn default() -> ColumnTable {
        let mut table: GenericArray<GenericArray<Option<Box<[(u8, u8, u8)]>>, U8>, U9> =
            GenericArray::generate(|_| GenericArray::generate(|_| None));

        table[0][0] = None;
        table[0][1] = Some(
            Combination::<U8, U1>::new()
                .map(|v| (v[0] + 1, 0, 0))
                .collect::<Vec<_>>()
                .into_boxed_slice(),
        );
        table[0][2] = Some(
            Combination::<U8, U1>::new()
                .map(|v| (0, v[0] + 1, 0))
                .collect::<Vec<_>>()
                .into_boxed_slice(),
        );
        table[0][3] = Some(
            Combination::<U8, U2>::new()
                .map(|v| (v[0] + 1, v[1] + 1, 0))
                .collect::<Vec<_>>()
                .into_boxed_slice(),
        );
        table[0][4] = Some(
            Combination::<U8, U1>::new()
                .map(|v| (0, 0, v[0] + 1))
                .collect::<Vec<_>>()
                .into_boxed_slice(),
        );
        table[0][5] = Some(
            Combination::<U8, U2>::new()
                .map(|v| (v[0] + 1, 0, v[1] + 1))
                .collect::<Vec<_>>()
                .into_boxed_slice(),
        );
        table[0][6] = Some(
            Combination::<U8, U2>::new()
                .map(|v| (0, v[0] + 1, v[1] + 1))
                .collect::<Vec<_>>()
                .into_boxed_slice(),
        );
        table[0][7] = Some(
            Combination::<U8, U3>::new()
                .map(|v| (v[0] + 1, v[1] + 1, v[2] + 1))
                .collect::<Vec<_>>()
                .into_boxed_slice(),
        );

        for n in 1..8 {
            table[n][0] = None;
            table[n][1] = Some(
                Combination::<U9, U1>::new()
                    .map(|v| (v[0] + 10 * n as u8, 0, 0))
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
            );
            table[n][2] = Some(
                Combination::<U9, U1>::new()
                    .map(|v| (0, v[0] + 10 * n as u8, 0))
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
            );
            table[n][3] = Some(
                Combination::<U9, U2>::new()
                    .map(|v| (v[0] + 10 * n as u8, v[1] + 10 * n as u8, 0))
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
            );
            table[n][4] = Some(
                Combination::<U9, U1>::new()
                    .map(|v| (0, 0, v[0] + 10 * n as u8))
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
            );
            table[n][5] = Some(
                Combination::<U9, U2>::new()
                    .map(|v| (v[0] + 10 * n as u8, 0, v[1] + 10 * n as u8))
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
            );
            table[n][6] = Some(
                Combination::<U9, U2>::new()
                    .map(|v| (0, v[0] + 10 * n as u8, v[1] + 10 * n as u8))
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
            );
            table[n][7] = Some(
                Combination::<U9, U3>::new()
                    .map(|v| {
                        (
                            v[0] + 10 * n as u8,
                            v[1] + 10 * n as u8,
                            v[2] + 10 * n as u8,
                        )
                    })
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
            );
        }

        table[8][0] = None;
        table[8][1] = Some(
            Combination::<U10, U1>::new()
                .map(|v| (v[0] + 80, 0, 0))
                .collect::<Vec<_>>()
                .into_boxed_slice(),
        );
        table[8][2] = Some(
            Combination::<U10, U1>::new()
                .map(|v| (0, v[0] + 80, 0))
                .collect::<Vec<_>>()
                .into_boxed_slice(),
        );
        table[8][3] = Some(
            Combination::<U10, U2>::new()
                .map(|v| (v[0] + 80, v[1] + 80, 0))
                .collect::<Vec<_>>()
                .into_boxed_slice(),
        );
        table[8][4] = Some(
            Combination::<U10, U1>::new()
                .map(|v| (0, 0, v[0] + 80))
                .collect::<Vec<_>>()
                .into_boxed_slice(),
        );
        table[8][5] = Some(
            Combination::<U10, U2>::new()
                .map(|v| (v[0] + 80, 0, v[1] + 80))
                .collect::<Vec<_>>()
                .into_boxed_slice(),
        );
        table[8][6] = Some(
            Combination::<U10, U2>::new()
                .map(|v| (0, v[0] + 80, v[1] + 80))
                .collect::<Vec<_>>()
                .into_boxed_slice(),
        );
        table[8][7] = Some(
            Combination::<U10, U3>::new()
                .map(|v| (v[0] + 80, v[1] + 80, v[2] + 80))
                .collect::<Vec<_>>()
                .into_boxed_slice(),
        );

        ColumnTable { table }
    }
}
