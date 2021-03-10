#[macro_use]
extern crate log;

#[macro_use] mod macros;

pub mod mpqdraft;
pub mod samase;

mod bw;
mod globals;
mod iscript;
mod parse;
mod recurse_checked_mutex;
mod windows;

use std::sync::atomic::{AtomicBool, Ordering};

fn init() {
    if cfg!(debug_assertions) {
        let _ = fern::Dispatch::new()
            .format(|out, message, record| {
                out.finish(format_args!(
                    "{}[{}:{}][{}] {}",
                    chrono::Local::now().format("[%Y-%m-%d][%H:%M:%S]"),
                    record.file().unwrap_or(""),
                    record.line().unwrap_or(0),
                    record.level(),
                    message
                ))
            })
            .level(log::LevelFilter::Trace)
            .chain(fern::log_file("aice.log").unwrap())
            .apply();
    }

    std::panic::set_hook(Box::new(|info| {
        use std::fmt::Write;

        let mut msg = String::with_capacity(256);
        writeln!(msg, "Aice {} panic", env!("CARGO_PKG_VERSION")).unwrap();
        match info.location() {
            Some(s) => writeln!(msg, "Panic at {}:{}", s.file(), s.line()).unwrap(),
            None => writeln!(msg, "Panic at unknown location").unwrap(),
        }
        let payload = info.payload();
        let panic_msg = match payload.downcast_ref::<&str>() {
            Some(s) => s,
            None => match payload.downcast_ref::<String>() {
                Some(s) => &s[..],
                None => "(???)",
            },
        };
        writeln!(msg, "{}", panic_msg).unwrap();
        error!("{}", msg);
        samase::crash_with_message(&msg);
    }));

    unsafe {
        iscript::load_iscript(false);
    }
}

static IS_1161: AtomicBool = AtomicBool::new(false);

fn is_scr() -> bool {
    IS_1161.load(Ordering::Relaxed) == false
}

#[no_mangle]
#[allow(non_snake_case)]
pub extern fn Initialize() {
    IS_1161.store(true, Ordering::Relaxed);
    // 1.16.1 init
    unsafe {
        let f: fn() = || {
            let ctx = samase_shim::init_1161();
            samase::samase_plugin_init(ctx.api());
        };
        samase_shim::on_win_main(f);
    }
}
