#[allow(unused_imports)]
use std::ffi::OsStr;
use std::ffi::OsString;
use std::io::{BufWriter, Write};
#[allow(unused_imports)]
use std::panic::Location;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};
use std::sync::{Arc, Mutex};

#[macro_export]
macro_rules! env_option {
    ($name:ident = $tag:expr) => {
        pub struct $name;
        impl $name {
            pub const KEY: &str = concat!("NOBUILD_", $tag);

            pub fn set() {
                std::env::set_var(Self::KEY, "1");
            }

            pub fn set_value(value: impl Into<String>) {
                std::env::set_var(Self::KEY, value.into());
            }

            pub fn unset() {
                std::env::remove_var(Self::KEY);
            }

            pub fn is_set() -> bool {
                std::env::var(Self::KEY).is_ok_and(|v| v != "NOBUILD_UNSET")
            }

            pub fn is_not_set() -> bool {
                std::env::var(Self::KEY).map_or_else(|_| true, |v| v == "NOBUILD_UNSET")
            }
        }
    };
}

env_option!(IgnoreUnknownFSEntity = "IGNORE_UNKNOWN_FS_ENTITY");

#[macro_export]
macro_rules! go_rebuild_urself {
    ($($flags:expr)?) => {{
        let source_path = file!();
        let args = args();
        let binary_path = Path::new(&args[0]);
        if is_path1_modified_after_path2(&source_path, binary_path) {
            let replace_path = PathBuf::from(format!("{}.old", binary_path.display()));
            os_replace(binary_path, &replace_path);

            // compile the new binary
            let mut command = Command::new("rustc");
            command
                $(.args($flags))?
                .arg("-o").arg(binary_path)
                .arg(source_path);
            eprintln!("RECOMPILING: {:?}", command);
            let mut child = unwrap!(command.spawn(); e => fatal!("failed to spawn command: {}", e));
            let status = unwrap!(child.wait(); e => fatal!("failed to wait for command: {}", e));
            if !status.success() {
                os_replace(&replace_path, binary_path);
                fatal!("command failed");
            }
            if exists("target") {
                rmdir_all("target");
            }

            // run the new binary
            let mut command = Command::new(binary_path);
            command.args(&args[1..]);
            spawn(&mut command);

            std::process::exit(0);
        }
    }};
}

#[macro_export]
macro_rules! info {
    ($($arg:tt)+) => {
        eprintln!("INFO: {}", format_args!($($arg)+))
    };
}

#[macro_export]
macro_rules! warn {
    ($($arg:tt)+) => {
        eprintln!("WARN: {}", format_args!($($arg)+))
    };
}

#[macro_export]
macro_rules! error {
    ($($arg:tt)+) => {
        eprintln!("ERROR {}: {}", location(), format_args!($($arg)+))
    };
}

#[macro_export]
macro_rules! fatal {
    ($($arg:tt)+) => {{
        eprintln!("FATAL {}: {}", location(), format_args!($($arg)+));
        std::process::exit(1);
    }};
}

#[macro_export]
macro_rules! cmd {
    (
        $(in $path:expr;)?
        $cmd:expr $(, $($e:expr),*)? $(,)?
        $(; fn $name:ident => $body:expr)?

    ) => {{
        let mut command = Command::new(AsRef::<OsStr>::as_ref(&$cmd));
        $(command.current_dir($path);)?
        $(command.args(&[ $(AsRef::<OsStr>::as_ref(&$e)),* ]);)?
        $({
            let $name = &mut command;
            $body;
        })?
        eprintln!("CMD: {:?}", command);
        cmd(&mut command)
    }};
    () => {
        compile_error!("cmd! macro requires at least a command name")
    }
}

#[macro_export]
macro_rules! unwrap {
    ($e:expr; $err:expr) => {
        match $e {
            Some(v) => v,
            None => $err,
        }
    };
    ($e:expr; $o:pat => $err:expr) => {
        match $e {
            Ok(v) => v,
            Err($o) => $err,
        }
    };
}

#[track_caller]
pub fn cmd(command: &mut Command) -> Output {
    let output = unwrap!(command.output(); e => fatal!("failed to spawn command: {}", e));
    if !output.status.success() {
        unwrap!(
            BufWriter::new(std::io::stderr()).write_all(&output.stderr);
            e => fatal!("failed to write stderr: {}", e)
        );

        fatal!("command failed");
    } else {
        output
    }
}

#[track_caller]
pub fn spawn(command: &mut Command) {
    let mut child = unwrap!(command.spawn(); e => fatal!("failed to spawn command: {}", e));
    let status = unwrap!(child.wait(); e => fatal!("failed to wait for command: {}", e));
    if !status.success() {
        fatal!("command failed");
    }
}

pub fn args() -> Vec<String> {
    std::env::args().collect()
}

#[track_caller]
pub fn location() -> &'static Location<'static> {
    Location::caller()
}

#[track_caller]
pub fn metadata(path: impl AsRef<Path>) -> std::fs::Metadata {
    let path = path.as_ref();
    unwrap!(std::fs::metadata(path); e => fatal!("failed to get metadata for {}: {}", path.display(), e))
}

#[track_caller]
pub fn modified(path: impl AsRef<Path>) -> std::time::SystemTime {
    let path = path.as_ref();
    unwrap!(metadata(path).modified(); e => fatal!("failed to get modified time for {}: {}", path.display(), e))
}

#[track_caller]
pub fn is_path1_modified_after_path2(path1: impl AsRef<Path>, path2: impl AsRef<Path>) -> bool {
    let file1_time = modified(path1);
    let file2_time = modified(path2);
    file1_time > file2_time
}

#[track_caller]
pub fn ends_with(path: impl AsRef<Path>, suffix: impl AsRef<Path>) -> bool {
    path.as_ref().ends_with(suffix.as_ref())
}

pub fn has_ext(path: impl AsRef<Path>, ext: impl AsRef<OsStr>) -> bool {
    path.as_ref()
        .extension()
        .is_some_and(|c| c.eq_ignore_ascii_case(ext.as_ref()))
}

#[track_caller]
pub fn has_any_ext(path: impl AsRef<Path>, exts: &[impl AsRef<OsStr>]) -> bool {
    path.as_ref()
        .extension()
        .is_some_and(|c| exts.iter().any(|e| c.eq_ignore_ascii_case(e.as_ref())))
}

#[track_caller]
pub fn has_an_ext(path: impl AsRef<Path>) -> bool {
    path.as_ref().extension().is_some()
}

#[track_caller]
pub fn ext(path: impl AsRef<Path>) -> Option<String> {
    unwrap!(path.as_ref().extension(); fatal!("failed to get file extension"))
        .to_str()
        .map(|s| s.to_lowercase())
}

#[track_caller]
pub fn no_ext(path: impl AsRef<Path>) -> PathBuf {
    let path = path.as_ref();
    let stem = unwrap!(path.file_stem(); fatal!("failed to get file stem"));
    let buf = PathBuf::from(stem);
    match path.parent() {
        Some(parent) => parent.join(buf),
        None => buf,
    }
}

#[track_caller]
pub fn parent(path: impl AsRef<Path>) -> PathBuf {
    unwrap!(path.as_ref().parent(); fatal!("failed to get parent path")).to_path_buf()
}

#[track_caller]
pub fn rm(path: impl AsRef<Path>) {
    info!("removing {}", path.as_ref().display());
    unwrap!(std::fs::remove_file(path); e => fatal!("failed to remove file: {}", e))
}

#[track_caller]
pub fn exists(path: impl AsRef<Path>) -> bool {
    std::fs::metadata(path).is_ok()
}

#[track_caller]
pub fn is_dir(path: impl AsRef<Path>) -> bool {
    metadata(path).is_dir()
}

#[track_caller]
pub fn is_file(path: impl AsRef<Path>) -> bool {
    metadata(path).is_file()
}

#[track_caller]
pub fn rename(from: impl AsRef<Path>, to: impl AsRef<Path>) {
    info!("renaming {} to {}", from.as_ref().display(), to.as_ref().display());
    unwrap!(std::fs::rename(from, to); e => fatal!("failed to rename file: {}", e));
}

#[track_caller]
pub fn rmdir_all(path: impl AsRef<Path>) {
    info!("removing {}", path.as_ref().display());
    unwrap!(std::fs::remove_dir_all(path); e => fatal!("failed to remove dir: {}", e));
}

#[track_caller]
pub fn rmdir(path: impl AsRef<Path>) {
    info!("removing {}", path.as_ref().display());
    unwrap!(std::fs::remove_dir(path); e => fatal!("failed to remove dir: {}", e));
}

#[track_caller]
pub fn mkdir_all(path: impl AsRef<Path>) {
    info!("creating {}", path.as_ref().display());
    unwrap!(std::fs::create_dir_all(path); e => fatal!("failed to create dir: {}", e));
}

#[track_caller]
pub fn mkdir(path: impl AsRef<Path>) {
    info!("creating {}", path.as_ref().display());
    unwrap!(std::fs::create_dir(path); e => fatal!("failed to create dir: {}", e));
}

#[track_caller]
pub fn copy(from: impl AsRef<Path>, to: impl AsRef<Path>) {
    info!("copying {} to {}", from.as_ref().display(), to.as_ref().display());
    unwrap!(std::fs::copy(from, to); e => fatal!("failed to copy file: {}", e));
}

#[track_caller]
pub fn read(path: impl AsRef<Path>) -> String {
    unwrap!(std::fs::read_to_string(path); e => fatal!("failed to read file: {}", e))
}

pub fn read_dir(path: impl Into<PathBuf>) -> impl Iterator<Item=(String, PathBuf)> {
    let dir = unwrap!(std::fs::read_dir(path.into()); e => fatal!("failed to read dir: {}", e));
    dir
        .map(|entry| unwrap!(entry; e => fatal!("failed to read dir entry: {}", e)))
        .map(|entry| {
            let file_name = unwrap!(entry.file_name().into_string(); e => fatal!("failed to convert file name to string: {:?}", e));
            let path = entry.path();
            (file_name, path)
        })
}

pub fn read_dir_recursive(path: impl Into<PathBuf>) -> impl Iterator<Item=(String, PathBuf)> {
    let path = path.into();
    let mut stack = vec![path];
    std::iter::from_fn(move || {
        while let Some(path) = stack.pop() {
            if is_dir(&path) {
                let mut dirs = read_dir(&path).collect::<Vec<_>>();
                dirs.reverse();
                stack.extend(dirs.into_iter().map(|(_, path)| path));
            } else if is_file(&path) {
                let file_name = unwrap!(path.file_name(); fatal!("failed to get file name"));
                let file_name = unwrap!(
                    OsString::from(file_name).into_string();
                    e => fatal!("failed to convert file name to string: {:?}", e)
                );

                return Some((file_name.to_string(), path));
            } else if IgnoreUnknownFSEntity::is_not_set() {
                warn!("ignoring {}", path.display());
            }
        }
        None
    })
}

// os-stuff

fn os_replace(from: impl AsRef<Path>, to: impl AsRef<Path>) {
    #[cfg(unix)] let result = unix::replace(from, to);
    #[cfg(windows)] let result = windows::replace(from, to);

    unwrap!(result; e => fatal!("failed to replace executable: {}", e));
}

#[track_caller]
fn os_remove(path: impl AsRef<Path>) {
    let path = path.as_ref();
    info!("removing {}", path.display());
    #[cfg(unix)] let result = unix::remove(path);
    #[cfg(windows)] let result = windows::remove(path);
    unwrap!(result; e => fatal!("failed to remove file: {}", e));
}

fn strip_prefix(prefix: impl AsRef<Path>, path: impl AsRef<Path>) -> PathBuf {
    let prefix = prefix.as_ref();
    let path = path.as_ref();
    path
        .strip_prefix(prefix)
        .map(Path::to_path_buf)
        .unwrap_or_else(|_| path.to_path_buf())
}

#[cfg(windows)]
mod windows {
    use std::ffi::c_uint;
    use std::path::Path;

    extern "C" {
        fn MoveFileExW(lpExistingFileName: *const u16, lpNewFileName: *const u16, dwFlags: c_uint) -> i32;
        fn DeleteFileW(lpFileName: *const u16) -> i32;
    }

    const MOVEFILE_REPLACE_EXISTING: c_uint = 1;

    fn win_str(path: &Path) -> Vec<u16> {
        use std::os::windows::ffi::OsStrExt;
        path.as_os_str()
            .encode_wide()
            .chain(Some(0))
            .collect()
    }

    pub fn replace(from: impl AsRef<Path>, to: impl AsRef<Path>) -> std::io::Result<()> {
        let from = win_str(from.as_ref());
        let to = win_str(to.as_ref());
        let result = unsafe {
            MoveFileExW(from.as_ptr(), to.as_ptr(), MOVEFILE_REPLACE_EXISTING)
        };
        if result == 0 {
            Err(std::io::Error::last_os_error())
        } else {
            Ok(())
        }
    }

    pub fn remove(path: impl AsRef<Path>) -> std::io::Result<()> {
        let path = win_str(path.as_ref());
        let result = unsafe {
            DeleteFileW(path.as_ptr())
        };
        if result == 0 {
            Err(std::io::Error::last_os_error())
        } else {
            Ok(())
        }
    }
}

pub fn buffer_collect<T, I>(count: usize, iter: I, f: impl (Fn(I::Item) -> T) + Copy + Send + 'static) -> Vec<T>
    where T: Send + 'static,
          I: IntoIterator<Item=T>,
          I::Item: Send + 'static,
          I::IntoIter: Send + 'static
{
    let iter = Arc::new(Mutex::new(iter.into_iter()));

    let handles =
        std::iter::repeat(iter)
            .take(count)
            .map(|iter| {
                std::thread::spawn(move || {
                    let mut items = Vec::new();
                    loop {
                        let item = { iter.lock().unwrap().next() };
                        match item {
                            Some(item) => items.push(f(item)),
                            None => break,
                        }
                    }
                    items
                })
            })
            .collect::<Vec<_>>();

    let mut items = Vec::new();
    for handle in handles {
        let set = handle.join().unwrap();
        items.extend(set);
    }
    items
}

pub fn buffer_for_each<I>(count: usize, iter: I, f: impl (Fn(I::Item)) + Copy + Send + 'static)
    where
        I: IntoIterator,
        I::IntoIter: Send + 'static
{
    let iter = Arc::new(Mutex::new(iter.into_iter()));

    let handles =
        std::iter::repeat(iter)
            .take(count)
            .map(|iter| {
                std::thread::spawn(move || {
                    loop {
                        let item = { iter.lock().unwrap().next() };
                        match item {
                            Some(item) => f(item),
                            None => break,
                        };
                    }
                })
            })
            .collect::<Vec<_>>();

    for handle in handles {
        handle.join().unwrap();
    }
}

#[cfg(unix)]
mod unix {
    use std::ffi::{c_char, c_int};
    use std::ffi::CString;
    use std::path::Path;

    extern "C" {
        fn rename(oldpath: *const c_char, newpath: *const c_char) -> c_int;

        fn remove(path: *const c_char) -> c_int;
    }

    fn c_str(path: &Path) -> io::Result<Vec<u8>> {
        use std::os::unix::ffi::OsStrExt;
        Ok(CString::new(path.as_os_str().as_bytes())?.into_bytes_with_nul())
    }

    pub fn replace(from: impl AsRef<Path>, to: impl AsRef<Path>) -> std::io::Result<()> {
        let result = unsafe {
            rename(from.as_ptr() as *const c_char, to.as_ptr() as *const c_char)
        };
        if result == -1 {
            Err(std::io::Error::last_os_error())
        } else {
            Ok(())
        }
    }

    pub fn remove(path: impl AsRef<Path>) -> std::io::Result<()> {
        let result = unsafe {
            remove(path.as_ptr() as *const c_char)
        };
        if result == -1 {
            Err(std::io::Error::last_os_error())
        } else {
            Ok(())
        }
    }
}
