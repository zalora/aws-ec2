let
  unsafeDerivation = args: derivation ({
    PATH = builtins.getEnv "PATH";
    system = builtins.currentSystem;
    builder = ./bootstrap/proxy-bash.sh;
    preferLocalBuild = true;
    __noChroot = true;
  } // args);

  shell = name: command: unsafeDerivation {
    inherit name;
    args = ["-c" command];
  };

  # working git must be in $PATH.
  # increase (change) `clock' to trigger updates
  shallow-fetchgit =
    {url, branch ? "master", clock ? 1}:
      shell "${baseNameOf (toString url)}-${toString clock}" ''
        git clone --depth 1 -b ${branch} --recursive ${url} $out
        cd $out
      '';

  nixpkgs = shallow-fetchgit {
    url = "git://github.com/zalora/nixpkgs.git";
    branch = "upcast";
    clock = 1;
  };
in
{ system ? builtins.currentSystem
, pkgs ? import nixpkgs { inherit system; }
, name ? "aws-ec2"
, src ? builtins.filterSource (path: type: let base = baseNameOf path; in
    type != "unknown" &&
    base != ".git" && base != "result" && base != "dist" && base != ".cabal-sandbox"
    ) ./.
, haskellPackages ? pkgs.haskellPackages_ghc783
}:

let
  aws-ec2-expr = 
    { cabal, aeson, aws, base16Bytestring, base64Bytestring
    , blazeBuilder, byteable, conduitExtra, cryptohash, httpConduit
    , httpTypes, mtl, optparseApplicative, resourcet, scientific, text
    , time, unorderedContainers, vector, xmlConduit, yaml
    }:

    cabal.mkDerivation (self: {
      pname = "aws-ec2";
      inherit src;
      version = "0.3.2";
      isLibrary = true;
      isExecutable = true;
      jailbreak = true;
      buildDepends = [
        aeson aws base16Bytestring base64Bytestring blazeBuilder byteable
        conduitExtra cryptohash httpConduit httpTypes mtl
        optparseApplicative resourcet scientific text time
        unorderedContainers vector xmlConduit yaml
      ];
      meta = {
        homepage = "https://github.com/zalora/aws-ec2";
        description = "AWS EC2/VPC, ELB and CloudWatch client library for Haskell";
        license = self.stdenv.lib.licenses.bsd3;
        platforms = self.ghc.meta.platforms;
      };
    });

  aws = haskellPackages.callPackage ./aws.nix {};
  aws-ec2 = haskellPackages.callPackage aws-ec2-expr ({ inherit aws; });
in rec {
  library = aws-ec2;

  put-metric = pkgs.runCommand "${name}-put-metric" {} ''
    mkdir -p $out/bin
    cp ${library}/bin/put-metric $out/bin
  '';

  create-alarm = pkgs.runCommand "${name}-create-alarm" {} ''
    mkdir -p $out/bin
    cp ${library}/bin/create-alarm $out/bin
  '';
}
