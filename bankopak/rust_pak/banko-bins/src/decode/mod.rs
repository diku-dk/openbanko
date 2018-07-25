use banko::Bankoplade;

mod column_table;
mod offset_table;

#[derive(Default)]
pub struct Decoder {
    offset_table: offset_table::OffsetTable,
    column_table: column_table::ColumnTable,
}

impl Decoder {
    pub fn decode(&self, value: u64) -> Bankoplade {
        let (mut offset, indexes) = self.offset_table.get(value);

        let mut rows = [[None; 9]; 3];

        {
            let &mut [ref mut row0, ref mut row1, ref mut row2] = &mut rows;

            for (col_ndx, ((out_value0, out_value1), out_value2)) in row0
                .iter_mut()
                .zip(row1.iter_mut())
                .zip(row2.iter_mut())
                .enumerate()
            {
                let values: &[(u8, u8, u8)] = &self.column_table[[col_ndx as u8, indexes[col_ndx]]];
                let cur_value = values[(offset % (values.len() as u64)) as usize];
                offset /= values.len() as u64;

                *out_value0 = if cur_value.0 == 0 {
                    None
                } else {
                    Some(cur_value.0)
                };

                *out_value1 = if cur_value.1 == 0 {
                    None
                } else {
                    Some(cur_value.1)
                };

                *out_value2 = if cur_value.2 == 0 {
                    None
                } else {
                    Some(cur_value.2)
                };
            }
        }

        Bankoplade { rows }
    }
}
