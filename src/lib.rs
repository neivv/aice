#[macro_use]
extern crate log;
#[macro_use]
extern crate whack;

#[macro_use] mod macros;

pub mod mpqdraft;
pub mod samase;

mod bw;
mod globals;
mod iscript;
mod parse;
mod recurse_checked_mutex;
mod unit;
mod windows;

use std::sync::atomic::{AtomicBool, Ordering};

use lazy_static::lazy_static;
use parking_lot::Mutex;

use winapi::um::processthreadsapi::{GetCurrentProcess, TerminateProcess};

lazy_static! {
    static ref PATCHER: Mutex<whack::Patcher> = Mutex::new(whack::Patcher::new());
}

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

        #[cfg(debug_assertions)]
        fn backtrace() -> String {
            use std::path::Path;

            let mut backtrace = String::new();
            backtrace::trace(|frame| {
                let ip = frame.ip();
                let symbol_address = frame.symbol_address();

                backtrace::resolve(ip, |symbol| {
                    let mut line = format!("    {:p}", symbol_address);
                    if symbol_address != ip {
                        write!(line, " ({:p})", symbol_address).unwrap();
                    }
                    let module = windows::module_from_address(symbol_address as *mut _);
                    if let Some((name, base)) = module {
                        if let Some(fname) = Path::new(&name).file_name() {
                            write!(line, " {:?} {:p}", fname, base).unwrap();
                        } else {
                            write!(line, " {:?} {:p}", name, base).unwrap();
                        }
                    }
                    if let Some(name) = symbol.name() {
                        write!(line, " -- {}", name).unwrap();
                    }
                    if let Some(filename) = symbol.filename() {
                        if let Some(lineno) = symbol.lineno() {
                            write!(line, " -- {:?}:{}", filename, lineno).unwrap();
                        } else {
                            write!(line, " -- {:?}:???", filename).unwrap();
                        }
                    }
                    writeln!(backtrace, "{}", line).unwrap();
                });
                true // keep going to the next frame
            });
            backtrace
        }

        #[cfg(not(debug_assertions))]
        fn backtrace() -> String {
            "".into()
        }

        let mut msg = String::new();
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
        if cfg!(debug_assertions) {
            write!(msg, "Backtrace:\n{}", backtrace()).unwrap();
        }
        error!("{}", msg);
        windows::message_box("Aice panic", &msg);
        unsafe {
            TerminateProcess(GetCurrentProcess(), 0x4230daef);
        }
    }));

    unsafe {
        iscript::load_iscript(false);
    }
}

static IS_1161: AtomicBool = AtomicBool::new(false);

#[no_mangle]
#[allow(non_snake_case)]
pub extern fn Initialize() {
    IS_1161.store(true, Ordering::Relaxed);
    // 1.16.1 init
    unsafe {
        let f: fn() = || {
            let ctx = samase_shim::init_1161();
            samase::samase_plugin_init(ctx.api());

            let mut active_patcher = PATCHER.lock();

            let mut exe = active_patcher.patch_exe(0x00400000);
            bw::init_funcs(&mut exe);
            bw::init_vars(&mut exe);
            //exe.hook_opt(bw::increment_death_scores, aiscript::increment_deaths);
        };
        samase_shim::on_win_main(f);
    }
}
