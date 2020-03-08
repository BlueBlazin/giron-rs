# giron-wasm

A WebAssembly module for parsing Javascript.

## Usage

The easiest way to see how you can use `giron-wasm` is with the wasm-app template.

Just run `npm init wasm-app` and this will create a boilerplate wasm project for you.

Now you can install `giron-wasm` and import it like any normal npm package.

## API

`function parseScript(source: string)` - Parses a string as a javascript script.

`function parseModule(source: string)` - Parses a string as a javascript module. Modules in javascript are parsed as strict mode code and allow import/export declarations.
