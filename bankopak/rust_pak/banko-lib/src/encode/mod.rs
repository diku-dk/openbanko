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
        let mut rows = plade.rows().map(|row| self.row_table.get(row));
        let mut result = self
            .layout_table
            .get(rows.next()??, rows.next()??, rows.next()??)?;
        let mut multiply = 1;

        for (col_ndx, column) in plade.columns().enumerate() {
            let (value, limit) = self
                .column_table
                .get(col_ndx, column[0], column[1], column[2])?;
            result += (value as u64) * multiply;
            multiply *= limit as u64;
        }

        Some(result)
    }
}
