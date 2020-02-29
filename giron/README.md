[![Build Status](https://travis-ci.com/BlueBlazin/giron.svg?branch=master)](https://travis-ci.com/BlueBlazin/giron)

# Giron

v0.1.0

## Table of Contents

- [Introduction](#introduction)
- [Contribution](#contribution)
- [Requirements](#requirements)
- [Installation](#installation)
- [Usage](#usage)

## Introduction

Giron is an ECMAScript parser written in Rust which outputs Rust strucs or JSON in the ESTree specification format.

The `giron-wasm` provides the compiled .wasm binary and javascript interface for using the giron parser on the web.

**Note:** giron is a work in progress.

## Contribution

This repository is looking for contributors. There's still a lot of work to be done, but some of the priorities right now are:

1. Add more tests, ensure parity with output from esprima and/or acorn.
2. Update parser and ESTree to be ECMAScript 2020 compliant.
3. Add all early errors from the ECMAScript specification to the parser.
4. Refactor the codebase to increase readability and add more documentation.
5. Improve performance.
6. Add JSX support.

## Requirements

1. You need to have Rust and Cargo installed. https://www.rust-lang.org/
2. For giron-wasm you need a browser capable of executing WebAssembly.

## Installation

1. Clone the repository.
2. Build the binary using cargo build.

```sh
git clone https://github.com/BlueBlazin/giron.git
cd giron
cargo build
```

## Usage

To parse a javascript file using the built binary, run giron using cargo with a single argument providing the path of the file.

By default the result is output to stdout. To redirect it to a file:

```sh
cargo run myscript.js > myscript.json
```
