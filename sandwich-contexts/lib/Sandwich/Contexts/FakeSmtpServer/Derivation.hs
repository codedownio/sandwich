
module Sandwich.Contexts.FakeSmtpServer.Derivation where

import Data.String.Interpolate
import Relude


expr :: Text
expr = [iii|
{ callPackage
, fetchFromGitHub
, node2nix
, nodejs
, stdenv
}:

let

  nixified = stdenv.mkDerivation {
    pname = "fake-smtp-server";
    version = "0.8.1";

    src = fetchFromGitHub {
      owner = "codedownio";
      repo = "fake-smtp-server";
      rev = "102b72c1ec852d88309b290b6b68ff5b4f50a431";
      sha256 = "sha256-uTcYGs5OOQ/uKfKYdmgnGYvBPfTALDoZsytqJEDTwHA=";
    };

    dontConfigure = true;

    buildInputs = [node2nix];

    buildPhase = ''
      node2nix -- --nodejs-14 --lock package-lock.json
    '';

    installPhase = ''
      cp -r ./. $out
    '';
  };

in

(callPackage nixified { inherit nodejs; }).package
|]
