// @ts-check
import { spawnSync } from 'node:child_process';
import { chmod, mkdir, writeFile } from 'node:fs/promises';
import { watch } from 'node:fs';
import { resolve } from 'node:path';
import { Buffer } from 'node:buffer';
import { default as process } from 'node:process';
import {
  Android,
  Win32,
  Linux,
  Darwin,
  Solaris,
  Freebsd,
  Openbsd,
  Arm,
  Arm64,
  Ia32,
  Ppc64,
  X64,
  // @ts-expect-error
} from './esgleam/mod/platform.mjs';
// @ts-expect-error
import { Ok, Error } from './gleam.mjs';

import { entries } from './streaming_tar.mjs';

const install_dir = "./build/dev/bin"
const pkg_dir = `${install_dir}/package/bin`;

export function exec_shell(command) {
  command = command.toArray();
  spawnSync(command[0], command.slice(1), { cwd: '.', stdio: 'inherit' });
}

/** @type {Partial<Record<NodeJS.Platform, () => import('../build/dev/javascript/esgleam/esgleam/mod/platform.d.mts').OsName$>>} */
const platform_map = {
  android: () => new Android(),
  darwin: () => new Darwin(),
  freebsd: () => new Freebsd(),
  linux: () => new Linux(),
  openbsd: () => new Openbsd(),
  sunos: () => new Solaris(),
  win32: () => new Win32(),
};

export function get_os() {
  const platform = process.platform;
  const res = platform_map[platform]?.();
  if (res) return new Ok(res);
  return new Error(res);
}

/** @type {Partial<Record<NodeJS.Architecture, () => import('../build/dev/javascript/esgleam/esgleam/mod/platform.d.mts').Arch$>>} */
const arch_map = {
  arm: () => new Arm(),
  arm64: () => new Arm64(),
  ia32: () => new Ia32(),
  ppc64: () => new Ppc64(),
  x64: () => new X64(),
};

export function get_arch() {
  const arch = process.arch;
  const res = arch_map[arch]?.();
  if (res) return new Ok(res);
  return new Error(res);
}

/**
 * @param {string} tarball_url 
 * @param {VoidFunction} then 
 */
export async function do_fetch(tarball_url, then) {
  console.log(`Fetching tarball from: ${tarball_url}`);
  const tarResp = await fetch(tarball_url);
  const tarStream = tarResp.body?.pipeThrough(new DecompressionStream('gzip'));

  for await (const entry of entries(tarStream)) {
    if (/(^|\/)esbuild(\.exe)?$/.test(entry.name)) {
      try {
        await mkdir(pkg_dir, { recursive: true });
      } catch {}
      const exePath = entry.name.endsWith('.exe') ? `${pkg_dir}/esbuild.exe` : `${pkg_dir}/esbuild`;
      await writeFile(
        exePath,
        Buffer.from(await entry.arrayBuffer()),
        { encoding: 'binary' }
      );
      await chmod(exePath, 0o777);
      break;
    }
  }
  then()
}

export function install() {
  spawnSync('gleam', ['run', '--target=erlang', '-m', 'esgleam/install'], {stdio: 'inherit'});
}

export function watch_gleam() {
  const _path = resolve('./src');
  console.log(_path, process.cwd());
  /** @type {import("node:fs").FSWatcher} */
  const _watcher = watch(_path, { recursive: true }, (event, filename) => {
    console.log(`[${event}]: ${filename}`);
    spawnSync('gleam', ['build', '--target=javascript'], {stdio: 'inherit'});
  });
  return () => {
    _watcher.close();
  };
}
