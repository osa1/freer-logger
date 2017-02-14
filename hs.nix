{ mkDerivation, base, bytestring, fast-logger, freer, monad-logger
, mtl, stdenv, template-haskell, text
}:
mkDerivation {
  pname = "freer-logger";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring fast-logger freer monad-logger template-haskell
    text
  ];
  testHaskellDepends = [ base freer mtl text ];
  homepage = "https://github.com/osa1/freer-logger";
  license = stdenv.lib.licenses.bsd3;
}
