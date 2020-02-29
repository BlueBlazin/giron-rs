[![Build Status](https://travis-ci.com/BlueBlazin/giron.svg?branch=master)](https://travis-ci.com/BlueBlazin/giron)

# Giron

## Table of Contents

- [Introduction](#introduction)
- [Contribution](#contribution)
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

## Installation

Get from crates.io: https://crates.io/crates/giron

## Usage

Once you add `giron` to your Cargo.toml,

**Basic Usage:**

```rs
use giron::{parse_module, parse_script};

fn main() {
    let source = String::from("const PI = 3.14;");
    parse_script(source).unwrap();
}
```

**Giron Errors:**

```rs
use giron::{parse_module, parse_script, GironError, EstreeNode};

fn analyze_ast() -> Result<EstreeNode, GironError> {
    let source = String::from("const PI = 3.14;");
    parse_script(source)
}
```

**Parse contents of a javascript file:**

```rs
use giron::{parse_module, parse_script};
use std::fs;

fn main() {
    let source = fs::read_to_string("example-file.js").unwrap();
    parse_script(source).unwrap();
}
```
