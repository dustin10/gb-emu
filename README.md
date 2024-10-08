# Game Boy Emulator

A Game Boy emulator written in the [Rust](https://rustlang.org) programming language.

## Executing JSON Tests

The JSON-based [test suite](https://github.com/SingleStepTests/sm83) for testing the CPU instruction implementations
can be executed by running ignored tests. The `JSON_TESTS_BASE_DIR` environment variable must be set. If it is not
set then the base directory defaults to `./`.

The ignored tests can be run with the following `cargo` command.

```sh 
# assumes the test repo is cloned to ~
$ JSON_TESTS_BASE_DIR=~/sm83/v1 cargo test -- --ignored
```

All tests can be run at once as well.

```sh 
$ JSON_TESTS_BASE_DIR=~/sm83/v1 cargo test -- --include-ignored
```

## Resources

* CPU Op Codes - https://izik1.github.io/gbops
* JSON CPU Instruction Test Suite - https://github.com/SingleStepTests/sm83
