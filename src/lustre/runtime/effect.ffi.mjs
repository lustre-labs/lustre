export function makeKey() {
  return globalThis.crypto.randomUUID();
}
