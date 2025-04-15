#!/bin/sh
set -eu

need() {
    command -v "$1" >/dev/null || {
        echo "$1 is missing. Install it and retry."
        echo "  brew install $2"
        exit 1
    }
}

need pkg-config pkg-config

# macOS usually has necessary SSL development tools with Xcode Command Line Tools
# You might need to install them if you haven't already:
# xcode-select --install
# If you specifically need openssl, you can install it with brew:
# brew install openssl
# However, directly checking for 'libssl-dev' doesn't apply to macOS.
# We'll skip this check for now, assuming the necessary tools are present.

rustup component add rustfmt -- --quiet 2>/dev/null || true
command -v cargo-tarpaulin >/dev/null || cargo install cargo-tarpaulin

cargo fmt --all
cargo check --all-targets
cargo clippy --all-targets
cargo build --all-targets
cargo test --doc
cargo test --all-targets -- "$@"
cargo doc --no-deps
[ "$(command -v cargo-tarpaulin)" ] && cargo tarpaulin