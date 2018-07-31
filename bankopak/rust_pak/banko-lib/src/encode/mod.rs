use banko::Bankoplade;

mod column_table;
mod layout_table;
mod row_table;

#[derive(Default)]
pub struct Encoder {
    row_table: row_table::RowTable,
    layout_table: layout_table::LayoutTable,
    column_table: column_table::ColumnTable,
}

impl Encoder {
    #[inline]
    pub fn encode(&self, plade: &Bankoplade) -> Option<u64> {
        let row0 = self.row_table.get(plade.rows[0])?;
        let row1 = self.row_table.get(plade.rows[1])?;
        let row2 = self.row_table.get(plade.rows[2])?;
        let mut result = self.layout_table.get(row0, row1, row2)?;
        let mut multiply = 1;

        for (col_ndx, ((&val0, &val1), &val2)) in plade.rows[0]
            .iter()
            .zip(plade.rows[1].iter())
            .zip(plade.rows[2].iter())
            .enumerate()
        {
            let (value, limit) = self.column_table.get(col_ndx, val0, val1, val2)?;
            result += (value as u64) * multiply;
            multiply *= limit as u64;
        }

        Some(result)
    }
}
