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

// For keeping binsize low with bw_print
#[inline(never)]
pub fn print_and_drop(args: std::fmt::Arguments) {
    let mut text = String::new();
    let _ = std::fmt::write(&mut text, args);
    text.push('\0');
    crate::samase::print_text(text.as_ptr());
}
