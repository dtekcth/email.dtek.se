{ mkDerivation, base, digestive-functors, digestive-functors-lucid
, lucid, mysql-simple, scotty, stdenv, text
}:
mkDerivation {
  pname = "email-dtek-se";
  version = "0.0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base digestive-functors digestive-functors-lucid lucid mysql-simple
    scotty text
  ];
  executableHaskellDepends = [ base mysql-simple scotty text ];
  homepage = "https://github.com/dtekcth/email.dtek.se";
  license = stdenv.lib.licenses.agpl3;
}
