{ cabal, aeson, base16Bytestring, base64Bytestring, blazeBuilder
, byteable, caseInsensitive, cereal, conduit, conduitExtra
, cryptohash, dataDefault, filepath, httpConduit, httpTypes
, liftedBase, monadControl, mtl, network, resourcet, text, time
, transformers, unorderedContainers, utf8String, vector, xmlConduit
}:

cabal.mkDerivation (self: {
  pname = "aws";
  version = "0.9.4";
  sha256 = "1039db933612e3eb51f232b3875eb555c688530227349a9b50c4805b6cf3376f";
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    aeson base16Bytestring base64Bytestring blazeBuilder byteable
    caseInsensitive cereal conduit conduitExtra cryptohash dataDefault
    filepath httpConduit httpTypes liftedBase monadControl mtl network
    resourcet text time transformers unorderedContainers utf8String
    vector xmlConduit
  ];
  meta = {
    homepage = "http://github.com/aristidb/aws";
    description = "Amazon Web Services (AWS) for Haskell";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
    maintainers = with self.stdenv.lib.maintainers; [ ocharles ];
  };
})
