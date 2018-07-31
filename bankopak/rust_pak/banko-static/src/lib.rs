extern crate banko_lib;

use std::u64;

#[repr(C)]
#[derive(Default)]
pub struct CBankoplade {
    cells: [[u8; 9]; 3],
}

impl CBankoplade {
    fn to_rust(&self) -> banko_lib::Bankoplade {
        let mut rust_plade = banko_lib::Bankoplade {
            rows: [[None; 9]; 3],
        };
        for (rust_row, row) in rust_plade.rows.iter_mut().zip(self.cells.iter()) {
            for (rust_field, &field) in rust_row.iter_mut().zip(row.iter()) {
                if field != 0 {
                    *rust_field = Some(field);
                }
            }
        }
        rust_plade
    }

    fn from_rust(rust_plade: &banko_lib::Bankoplade) -> CBankoplade {
        let mut c_plade = CBankoplade { cells: [[0; 9]; 3] };

        for (rust_row, row) in rust_plade.rows.iter().zip(c_plade.cells.iter_mut()) {
            for (rust_field, field) in rust_row.iter().zip(row.iter_mut()) {
                *field = rust_field.unwrap_or(0);
            }
        }

        c_plade
    }
}

#[no_mangle]
pub extern "C" fn rust_encoder_init() -> *mut banko_lib::Encoder {
    let result: Box<banko_lib::Encoder> = Default::default();
    Box::into_raw(result)
}

#[no_mangle]
pub unsafe extern "C" fn rust_encoder_free(ptr: *mut banko_lib::Encoder) {
    Box::from_raw(ptr);
}

#[no_mangle]
pub extern "C" fn rust_encoder_run(
    encoder: *const banko_lib::Encoder,
    in_plade: *const CBankoplade,
    out_u64: *mut u64,
) -> bool {
    match unsafe { (encoder.as_ref(), in_plade.as_ref(), out_u64.as_mut()) } {
        (Some(encoder), Some(in_plade), Some(out_u64)) => {
            if let Some(out) = encoder.encode(&in_plade.to_rust()) {
                *out_u64 = out;
                true
            } else {
                *out_u64 = u64::MAX;
                false
            }
        }
        _ => false,
    }
}

#[no_mangle]
pub extern "C" fn rust_decoder_init() -> *mut banko_lib::Decoder {
    let result: Box<banko_lib::Decoder> = Default::default();
    Box::into_raw(result)
}

#[no_mangle]
pub unsafe extern "C" fn rust_decoder_free(ptr: *mut banko_lib::Decoder) {
    Box::from_raw(ptr);
}

#[no_mangle]
pub extern "C" fn rust_decoder_run(
    decoder: *mut banko_lib::Decoder,
    in_u64: u64,
    out_plade: *mut CBankoplade,
) -> bool {
    match unsafe { (decoder.as_ref(), out_plade.as_mut()) } {
        (Some(decoder), Some(out_plade)) => {
            let plade = decoder.decode(in_u64).as_ref().map(CBankoplade::from_rust);

            if let Some(plade) = plade {
                *out_plade = plade;
                true
            } else {
                *out_plade = Default::default();
                false
            }
        }
        _ => false,
    }
}
