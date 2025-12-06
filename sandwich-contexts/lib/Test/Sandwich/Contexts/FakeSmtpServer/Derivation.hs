
module Test.Sandwich.Contexts.FakeSmtpServer.Derivation (
  fakeSmtpServerDerivation
  ) where

import Data.String.Interpolate
import Relude


-- | A Nix derivation to build a fake Node.js SMTP server, based on
-- https://github.com/ReachFive/fake-smtp-server.
fakeSmtpServerDerivation :: Text
fakeSmtpServerDerivation = [i|
{ callPackage
, fetchFromGitHub
, buildNpmPackage
, stdenv
}:

buildNpmPackage rec {
  pname = "fake-smtp-server";
  version = "0.8.1";

  src = fetchFromGitHub {
    owner = "codedownio";
    repo = "fake-smtp-server";
    rev = "1adbffb35d6c90bcb2ad9fac3049fa2028a34d2f";
    sha256 = "sha256-zXaNM7sp2c3IEvmoZ81M+7LrcC1I0JhlqG0A+gOA38E=";
  };

  npmDepsHash = "sha256-CffyLKMJ4OYGFxxnaM4bFbu2OReekpKr2dTCSo8/Ei8=";

  dontNpmBuild = true;
}
|]
