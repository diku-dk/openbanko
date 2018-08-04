extern crate banko_lib;

use banko_lib::Bankoplade;
use std::u64;

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
    in_plade: *const Bankoplade,
    out_u64: *mut u64,
) -> bool {
    match unsafe { (encoder.as_ref(), in_plade.as_ref(), out_u64.as_mut()) } {
        (Some(encoder), Some(in_plade), Some(out_u64)) => {
            if let Some(out) = encoder.encode(in_plade) {
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
    out_plade: *mut Bankoplade,
) -> bool {
    match unsafe { (decoder.as_ref(), out_plade.as_mut()) } {
        (Some(decoder), Some(out_plade)) => {
            let plade = decoder.decode(in_u64);
            let result = plade.is_some();
            *out_plade = plade.unwrap_or_else(Bankoplade::zero_plade);
            result
        }
        _ => false,
    }
}
