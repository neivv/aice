#[macro_export]
macro_rules! debug_fatal {
    ($($toks:tt)*) => {
        if cfg!(debug_assertions) {
            panic!($($toks)*);
        } else {
            error!($($toks)*);
        }
    }
}

#[macro_export]
macro_rules! bw_print {
    ($lit:expr $(,)*) => {{
        crate::samase::print_text(concat!($lit, "\0").as_ptr());
    }};

    ($lit:expr, $($toks:tt)*) => {{
        crate::macros::print_and_drop(format_args!($lit, $($toks)*));
    }};
}


#[macro_export]
macro_rules! bw_print_error {
    ($lit:expr $(,)*) => {{
        crate::macros::print_text_error(concat!($lit, "\0").as_ptr());
    }};

    ($lit:expr, $($toks:tt)*) => {{
        crate::macros::print_and_drop_error(format_args!($lit, $($toks)*));
    }};
}

// For keeping binsize low with bw_print
#[inline(never)]
pub fn print_and_drop(args: std::fmt::Arguments) {
    let mut text = String::new();
    let _ = std::fmt::write(&mut text, args);
    text.push('\0');
    crate::samase::print_text(text.as_ptr());
}

// For keeping binsize low with bw_print
#[inline(never)]
pub fn print_and_drop_error(args: std::fmt::Arguments) {
    let mut text = String::new();
    let _ = std::fmt::write(&mut text, args);
    text.push('\0');
    let len = text.len();
    error!("{}", &text[..len - 1]);
    crate::samase::print_text(text.as_ptr());
}

#[expect(dead_code)]
pub fn print_text_error(ptr: *const u8) {
    unsafe {
        let len = (0..).position(|x| *ptr.add(x) == 0).unwrap();
        let text = std::slice::from_raw_parts(ptr, len);
        let text = String::from_utf8_lossy(text);
        error!("{}", text);
        crate::samase::print_text(ptr);
    }
}
