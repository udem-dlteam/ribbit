import { closeSync, openSync, readSync, writeSync } from "node:fs";
import { exit, stdout } from "node:process";
import { argv } from "node:process";
import { StringDecoder } from "node:string_decoder";

const logInteger = function(integer32) {
  /** @type {string[]} **/
  const cs = integer32.toString().split('');
  const s = [];
  const l = cs.length;
  for (let i = 3; i < l; i += 3) {
    s.push(cs.pop(), cs.pop(), cs.pop(), "_")
  }
  for (const c of cs.reverse()) {
    s.push(c);
  }
  stdout.write(s.reverse().join(''));
};

const exitPrim = function(integer32) {
  exit(integer32);
};

const logChar = function(integer32) {
  stdout.write(String.fromCharCode(integer32));
};

const getChar = function() {
  const buff = new Uint8Array(1);
  const x = Deno.stdin.readSync(buff);
  return x === null ? -1 : buff.at(0);
};

const openFile = function(filenamePtr, writeMode) {
  const arr = new Uint8Array(wasm.instance.exports.memory.buffer);
  const bytes = [];
  let i = filenamePtr;
  let next;
  while ((next = arr.at(i++)) != 0) {
    bytes.push(next);
  }

  const filename = new StringDecoder().write(new Uint8Array(bytes));
  return openSync(filename, writeMode ? "w" : "r");
};

const readChar = function(fd) {
  const buff = new Uint8Array(1);
  const x = readSync(fd, buff);
  return x === 0 ? -1 : buff.at(0);
};

const writeChar = function(fd, char) {
  const buff = new Uint8Array([char]);
  const x = writeSync(fd, buff);
  if (x === 0) {
    throw new Error("Could not write to file");
  }
};

const closeFd = function(fd) {
  closeSync(fd);
};

const options = {
  import: {
    logInteger,
    exit: exitPrim,
    logChar,
    getChar,
    openFile,
    readChar,
    writeChar,
    closeFd,
  },
};

const wasm = await WebAssembly.instantiateStreaming(
  fetch(new URL(`../../${argv[2]}`, import.meta.url)),
  options,
);

wasm.instance.exports._start();
