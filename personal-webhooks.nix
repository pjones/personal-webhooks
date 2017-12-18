{ mkDerivation, aeson, base, bytestring, containers, cryptonite
, data-default, directory, filepath, opaleye, optparse-applicative
, postgresql-simple, postgresql-simple-migration
, product-profunctors, profunctors, resource-pool, sandi, snap
, snap-core, snap-server, stdenv, table-layout, template-haskell
, text, time, transformers, yaml
}:
mkDerivation {
  pname = "personal-webhooks";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson base bytestring containers cryptonite data-default directory
    filepath opaleye optparse-applicative postgresql-simple
    postgresql-simple-migration product-profunctors profunctors
    resource-pool sandi snap snap-core snap-server template-haskell
    text time transformers yaml
  ];
  executableHaskellDepends = [
    aeson base bytestring data-default opaleye optparse-applicative
    table-layout text
  ];
  description = "Trigger personal scripts from incoming HTTP requests";
  license = stdenv.lib.licenses.bsd2;
}
