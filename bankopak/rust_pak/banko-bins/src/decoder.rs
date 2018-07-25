extern crate banko_lib;
extern crate byteorder;

use byteorder::{LittleEndian, ReadBytesExt};
use std::io::ErrorKind;

fn main() {
    let stdin = std::io::stdin();
    let mut reader = std::io::BufReader::new(stdin);

    let decoder = banko_lib::Decoder::default();

    loop {
        match reader.read_u64::<LittleEndian>() {
            Ok(n) => {
                let plade = decoder.decode(n);
                plade.print();
            }
            Err(ref e) if e.kind() == ErrorKind::UnexpectedEof => {
                break;
            }
            e => {
                e.expect("Could not read from file");
            }
        }
    }
}
