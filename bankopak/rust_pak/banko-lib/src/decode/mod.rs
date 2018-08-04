use banko::{Bankoplade, Column};

mod column_table;
mod offset_table;

#[derive(Default)]
pub struct Decoder {
    offset_table: offset_table::OffsetTable,
    column_table: column_table::ColumnTable,
}

impl Decoder {
    #[inline]
    pub fn decode(&self, value: u64) -> Option<Bankoplade> {
        let (mut offset, indexes) = self.offset_table.get(value)?;

        let mut plade = Bankoplade::zero_plade();

        for (col_ndx, (column, index)) in
            plade.columns_mut().zip(indexes.iter().cloned()).enumerate()
        {
            let column_values_for_index: &[Column] = self.column_table.get(col_ndx, index)?;

            if column_values_for_index.is_empty() {
                return None;
            }

            let len = column_values_for_index.len() as u64;
            let cur_value = column_values_for_index[(offset % len) as usize];
            offset /= len;

            *column[0] = cur_value[0];
            *column[1] = cur_value[1];
            *column[2] = cur_value[2];
        }

        if offset == 0 {
            Some(plade)
        } else {
            None
        }
    }
}
