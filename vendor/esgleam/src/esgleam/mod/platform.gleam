// #TODO: externalize this to a seperate package

pub type OsName {
  Win32
  Android
  Linux
  Darwin
  Solaris
  Sunos
  Freebsd
  Openbsd
}

pub type Arch {
  X64
  Ia32
  Arm64
  Arm
  Ppc64
}

@external(erlang, "ffi_esgleam", "get_os")
@external(javascript, "../../ffi_esgleam.mjs", "get_os")
pub fn get_os() -> Result(OsName, Nil)

@external(erlang, "ffi_esgleam", "get_arch")
@external(javascript, "../../ffi_esgleam.mjs", "get_arch")
pub fn get_arch() -> Result(Arch, Nil)

pub fn get_package_name() {
  let assert Ok(os) = get_os()
  let assert Ok(arch) = get_arch()

  let os_str = case os {
    Win32 -> "win32"
    Android -> "android"
    Linux -> "linux"
    Darwin -> "darwin"
    Solaris | Sunos -> "sunos"
    Freebsd -> "freebsd"
    Openbsd -> "openbsd"
  }

  let arch_str = case arch {
    X64 -> "x64"
    Ia32 -> "ia32"
    Arm64 -> "arm64"
    Arm -> "arm"
    Ppc64 -> "ppc64"
  }

  os_str <> "-" <> arch_str
}

pub fn get_exe_name() {
  let assert Ok(os) = get_os()

  case os {
    Win32 -> "esbuild.exe"
    _ -> "esbuild"
  }
}
