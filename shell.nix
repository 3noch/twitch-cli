((import <nixpkgs> {}).haskellPackages.callCabal2nix "twitch-cli" ./. {}).env
