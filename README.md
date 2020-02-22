# Giron

v0.1.0

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

TODO

## Usage

TODO

Author: Sanjeet N. Dasharath
