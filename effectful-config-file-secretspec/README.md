# effectful-config-file-secretspec

SecretSpec-backed Secrets effect handler for effectful-config-file.

## Installing libsecretspec

The `secretspec` Haskell package links against the libsecretspec C library,
found through `pkg-config`. Build it from the
[secretspec](https://github.com/cachix/secretspec) tag matching the
`secretspec` extra-dep in `stack.yaml`.

Requirements: [rustup](https://rustup.rs), `pkg-config` and
[cargo-c](https://github.com/lu-zero/cargo-c) (`cargo install cargo-c`).

```bash
git clone --depth 1 --branch v0.21.1 https://github.com/cachix/secretspec.git
cd secretspec
cargo cinstall -p libsecretspec --manifest-path Cargo.toml \
  --library-type staticlib \
  --prefix "$HOME/.libsecretspec" \
  --bindir lib \
  --libdir lib \
  --pkgconfigdir lib/pkgconfig
export PKG_CONFIG_PATH="$HOME/.libsecretspec/lib/pkgconfig${PKG_CONFIG_PATH:+:$PKG_CONFIG_PATH}"
```

Check it with `pkg-config --libs libsecretspec`, then build with `stack build`.

## Releasing to Hackage

Hackage's doc builder has no libsecretspec, so documentation must be built
and uploaded by a maintainer, with libsecretspec installed:

```bash
scripts/hackage-upload.sh effectful-config-file-secretspec            # candidate
scripts/hackage-upload.sh effectful-config-file-secretspec --publish  # release
```

Credentials come from `HACKAGE_KEY` or an interactive prompt.
