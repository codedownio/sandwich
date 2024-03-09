
module Test.Sandwich.Contexts.FakeSmtpServer.Derivation (
  fakeSmtpServerDerivation
  ) where

import Data.String.Interpolate
import Relude


fakeSmtpServerDerivation :: Text
fakeSmtpServerDerivation = [i|
{ callPackage
, fetchFromGitHub
, node2nix
, nodejs_18
, stdenv
}:

let

  nixified = stdenv.mkDerivation {
    pname = "fake-smtp-server";
    version = "0.8.1";

    src = fetchFromGitHub {
      owner = "codedownio";
      repo = "fake-smtp-server";
      rev = "1adbffb35d6c90bcb2ad9fac3049fa2028a34d2f";
      sha256 = "sha256-zXaNM7sp2c3IEvmoZ81M+7LrcC1I0JhlqG0A+gOA38E=";
    };

    dontConfigure = true;

    buildInputs = [node2nix];

    buildPhase = ''
      node2nix -- --nodejs-18 --lock package-lock.json
    '';

    installPhase = ''
      cp -r ./. $out
    '';
  };

in

(callPackage nixified { nodejs = nodejs_18; }).package
|]
