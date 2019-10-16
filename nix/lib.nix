let
  # iohk-nix can be overridden for debugging purposes by setting
  # NIX_PATH=iohk_nix=/path/to/iohk-nix
  iohkNix = import (
    let try = builtins.tryEval <iohk_nix>;
    in if try.success
    then builtins.trace "using host <iohk_nix>" try.value
    else
      let
        spec = builtins.fromJSON (builtins.readFile ./iohk-nix-src.json);
      in builtins.fetchTarball {
        url = "${spec.url}/archive/${spec.rev}.tar.gz";
        inherit (spec) sha256;
      }) {};

  pkgs19_10 = import (builtins.fetchGit {
      name = "nixpkgs-19.09";
      url = https://github.com/nixos/nixpkgs/;
      rev = "3a440874c75a667432e4cd0934db5e06297e3533";
  }) {};

  pkgs = iohkNix.pkgs;
  lib = pkgs.lib;
in lib // { inherit iohkNix pkgs pkgs19_10; inherit (iohkNix) nix-tools; }
