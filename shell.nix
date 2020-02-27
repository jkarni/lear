{ pkgs ? import <nixos> {}
}:

with pkgs;

stdenv.mkDerivation {
  name = "lear";
  buildInputs = [
    ghc
    stack
    stylish-haskell
    zlib
];
}
