{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
    flags = { demo = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "Win32-network"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "2019 Input Output (Hong Kong) Ltd.";
      maintainer = "duncan@well-typed.com, marcin.szamotulski@iohk.io";
      author = "Duncan Coutts, Marcin Szamotulski";
      homepage = "";
      url = "";
      synopsis = "Win32 network API";
      description = "";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = if system.isWindows
          then [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"))
            ]
          else [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
        libs = (pkgs.lib).optional (system.isWindows) (pkgs."ws2_32" or (errorHandler.sysDepError "ws2_32"));
        buildable = true;
        };
      exes = {
        "named-pipe-demo" = {
          depends = if system.isWindows
            then [
              (hsPkgs."base" or (errorHandler.buildDepError "base"))
              (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
              (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
              (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"))
              (hsPkgs."Win32-network" or (errorHandler.buildDepError "Win32-network"))
              ]
            else [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
          buildable = true;
          };
        };
      tests = {
        "test-Win32-network" = {
          depends = (pkgs.lib).optionals (system.isWindows) [
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"))
            (hsPkgs."Win32-network" or (errorHandler.buildDepError "Win32-network"))
            ];
          buildable = if system.isWindows then true else false;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/ouroboros-network";
      rev = "f0eb6e439e7c0121476ded5e88d2f638e8aa36ac";
      sha256 = "0pii9myzb5ckf2vagd9b7fsmk78w72z8lw3zyyfzw58dyapy5c70";
      });
    postUnpack = "sourceRoot+=/Win32-network; echo source root reset to \$sourceRoot";
    }