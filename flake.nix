{
  description = "Basic haskell flake";

  inputs.nixpkgs.url = github:NixOS/nixpkgs/nixos-unstable;

  outputs = { self, nixpkgs }: {
    defaultPackage.x86_64-linux =
       with import nixpkgs { system = "x86_64-linux"; };
       let
          haskellDeps = ps: with ps; [
            reflex-dom
            ghcide
            categories
            cookie
            either
            ref-tf
            snap
            snap-server
            modern-uri
            websockets
            warp
            warp-tls
            http-reverse-proxy
            jsaddle
            jsaddle-warp
            (pkgs.haskell.lib.appendPatch snap-core ./check-range.patch)
            monad-control
            universe
            universe-dependent-sum
          ];
          ghc = pkgs.haskellPackages.ghcWithPackages haskellDeps;
       in 
       stdenv.mkDerivation {
          name = "rf";
          buildInputs = [ ghc pkgs.ghcid ];
          src = self;
          buildPhase = "ghc -o $name Main.hs";
          installPhase = "mkdir -p $out/bin; install -t $out/bin $name";
    };
  };
}
