extern crate banko_lib;
extern crate byteorder;

use byteorder::{LittleEndian, WriteBytesExt};
use std::io::Read;

fn main() {
    let stdout = std::io::stdout();
    let stdin = std::io::stdin();

    let mut writer = std::io::BufWriter::new(stdout.lock());

    let mut s = String::new();
    stdin
        .lock()
        .read_to_string(&mut s)
        .expect("Unable to read file");

    let encoder = banko_lib::Encoder::default();
    let plader = banko_lib::Bankoplade::parse_many(&s).unwrap();
    for plade in plader.iter() {
        writer
            .write_u64::<LittleEndian>(encoder.encode(plade))
            .expect("Could not write");
    }
}
