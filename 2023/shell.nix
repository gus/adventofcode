let
  moz_overlay = import (builtins.fetchTarball https://github.com/mozilla/nixpkgs-mozilla/archive/master.tar.gz);
  pkgs = import <nixpkgs> { overlays = [ moz_overlay ]; };
in
  with pkgs;
  stdenv.mkDerivation {
    name = "moz_overlay_shell";
    buildInputs = [
      pkgs.glibcLocales
      pkgs.bashInteractive
      # # to use the latest nightly:
      pkgs.latest.rustChannels.nightly.rust
      pkgs.latest.rustChannels.nightly.rust-src
      # # to use the project's rust-toolchain file:
      # (pkgs.rustChannelOf { rustToolchain = ./.rust-toolchain; }).rust
    ];
    RUST_SRC_PATH="${pkgs.latest.rustChannels.nightly.rust-src}/lib/rustlib/src/rust/library";
  }

# {}:
# let
#   rust-overlay = (import (builtins.fetchTarball "https://github.com/oxalica/rust-overlay/archive/master.tar.gz"));
#   pkgs = (import (fetchTarball http://nixos.org/channels/nixos-23.11/nixexprs.tar.xz) {
#     overlays = [ rust-overlay ];
#   });
# in
# pkgs.mkShell {
#   buildInputs = [
#     (pkgs.rust-bin.stable.latest.rust.override {
#       extensions = ["rust-src"];
#     })

#     pkgs.glibcLocales
#     pkgs.bashInteractive

#   ];
#   RUST_SRC_PATH = "${pkgs.rust.packages.stable.rustPlatform.rustLibSrc}/lib/rustlib/src/rust/library";
# }


# { pkgs ? import (fetchTarball http://nixos.org/channels/nixos-22.11/nixexprs.tar.xz) { } }:

# pkgs.mkShell {
#   buildInputs = [
#     pkgs.latest.rustChannels.stable.rust
#     pkgs.latest.rustChannels.stable.rust-src
#   ];
#   RUST_SRC_PATH="${pkgs.latest.rustChannels.stable.rust-src}/lib/rustlib/src/rust/library/";
# }


# { pkgs ? import (fetchTarball http://nixos.org/channels/nixos-23.11/nixexprs.tar.xz) { } }:
# # { pkgs ? import <nixpkgs> {} }:

# with pkgs;

# mkShell {
#   buildInputs = [
#     glibcLocales
#     bashInteractive
#     rustc
#     rust-src
#   ];
#   RUST_SRC_PATH="${pkgs.rust.packages.stable.rustPlatform.rustLibSrc}/lib/rustlib/src/rust/library/";
# }
