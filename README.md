# Darkroom

<img src="darkroomlogo_mini.svg?sanitize=true" width="150"/>
---
[![Crates.io](https://img.shields.io/crates/v/darkroom.svg)](https://crates.io/crates/darkroom)
[![Docs.rs](https://docs.rs/darkroom/badge.svg)](https://docs.rs/darkroom/)

A contract testing tool built in Rust using the [filmReel format](https://github.com/Bestowinc/filmReel).


`dark`:

<!-- dark start -->
```
Usage: dark [<address>] [-v] [--tls] [--proto <proto>] [-H <header>] [-C <cut-out>] <command> [<args>]

Top-level command.

Options:
  -v, --verbose     enable verbose output
  --tls             enable TLS (automatically inferred HTTP/S)
  --proto           pass proto files used for payload forming
  -H, --header      fallback header passed to the specified protocol
  -C, --cut-out     output of final cut file
  --help            display usage information

Commands:
  version           returns CARGO_PKG_VERSION
  take              Takes a single frame, emitting the request then validating
                    the returned response
  record            Attempts to play through an entire Reel sequence running a
                    take for every frame in the sequence

```
<!-- dark stop -->


`dark take`:

<!-- dark take start -->
```
Usage: dark take <frame> -c <cut> [-o <output>]

Takes a single frame, emitting the request then validating the returned response

Options:
  -c, --cut         filepath of input cut file
  -o, --output      output of take file
  --help            display usage information

```
<!-- dark take stop -->

`dark record`:

<!-- dark record start -->
```
Usage: dark record <reel_path> <reel_name> [<merge_cuts...>] [-c <cut>] [-o <output>] [-i]

Attempts to play through an entire Reel sequence running a take for every frame in the sequence

Options:
  -c, --cut         filepath of input cut file
  -o, --output      output directory for successful takes
  -i, --interactive interactive frame sequence transitions
  --help            display usage information

```
<!-- dark record stop -->

### New in `0.2`:

* HTTP support
* Full json object storage and retrieval, the cut register is no longer a flat associative array, strings are still used to map to JSON objects for templating
* Variable discarding: `${lowercase}` variables will only be kept around for the duration of the frame
* Headers and entrypoints can be stored and read on a per JSON frame basis
* SOPS/json secrets support

### New in `0.2.1`:

* Added hidden variable support, hidden variables are defined with a leading underscore: `${_HIDDEN}`
* Added `dark version` command
* moved common parameters into the main `dark` command to be shared across subcommands


#### SOPS example:

```sh
# destructively merge FIFO sops "KEY_NAME" value into the in-memory cut register
dark record ./reel_path reel_name -c ./reel_name.cut.json \
    <(sops -d --extract '["KEY_NAME"]' path/to/myfile.enc.json)

# multiple merge cuts can be used, with values being overridden left to right (right will have newer values)
dark -v record -i ./test_data post -c ./test_data/post.cut.json \
    <(echo '{"new":"value"}') <(echo '{"newer": "value", "new":"overridden"}')
```
#### Cut output example:

```sh
# echo the origin "${IP}" that gets written to the cut register from the httpbin.org POST request
dark -C >(jq .IP) take ./test_data/post.01s.body.fr.json --cut ./test_data/post.cut.json
```

<!--
cargo build --release
tar czf darkroom-0.1.2-x86_64-apple-darwin.tar.gz target/release/dark
alias rust-musl-builder='docker run --rm -it -v "$(pwd)":/home/rust/src ekidd/rust-musl-builder'
rust-musl-builder cargo build --release
tar czf darkroom-0.1.2-x86_64-unknown-linux-musl.tar.gz ./target/x86_64-unknown-linux-musl/release/dark
-->
