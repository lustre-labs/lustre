{
  description = "A Nix-flake-based development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forEachSupportedSystem = f: nixpkgs.lib.genAttrs supportedSystems (system: f rec {
        pkgs = import nixpkgs { inherit system; config.allowUnfree = true; };
      });
      localPackages = pkgs:
        if builtins.pathExists ./flake.local.nix
        then import ./flake.local.nix { inherit pkgs; }
        else [];
    in
    {
      devShells = forEachSupportedSystem ({ pkgs }: {
        default = pkgs.mkShell {
          packages = (with pkgs; [
            # Editors
            typescript-language-server
            taplo
            typescript

            # FE
            nodejs_24
            bun

            gleam
            beam28Packages.erlang
            beam28Packages.rebar3
          ]) ++ (localPackages pkgs);
        };
      });
    };
}
