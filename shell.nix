{ pkgs ? import <nixos> {}
}:

with pkgs;

stdenv.mkDerivation {
  name = "lear";
  buildInputs = [
    ghc
    stack
    zlib
    blas
    gfortran
];
}
