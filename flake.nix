{
  description = "Polysemy Effects for Concurrency";

  inputs = {
    hix.url = git+https://git.tryp.io/tek/hix;
    polysemy-time.url = git+https://git.tryp.io/tek/polysemy-time?tag=v0.6.0.0;
  };

  outputs = { hix, polysemy-time, ... }:
  let

    dev = { hackage, source, ... }: {
      polysemy = hackage "1.9.0.0" "1af07cppnjpv5v56wanya1mhkvbfnyynf5447mnkcf4zc4k23pyk";
      polysemy-plugin = hackage "0.4.4.0" "08ry72bw78fis9iallzw6wsrzxnlmayq2k2yy0j79hpw4sp8knmg";
    };

    all = { hackage, ... }: {
      incipit-base = hackage "0.4.1.0" "17579j3hzsh3ic0272h8ly8k7gz4zm1hv5jqimdam9gcq8alahkl";
      incipit-core = hackage "0.4.1.0" "1fm6bf1w8mvpa9qlgxqv3ngf0lyb3057cwv5ajibgbljjaznxpxc";
      polysemy = hackage "1.9.0.0" "1af07cppnjpv5v56wanya1mhkvbfnyynf5447mnkcf4zc4k23pyk";
      polysemy-plugin = hackage "0.4.4.0" "08ry72bw78fis9iallzw6wsrzxnlmayq2k2yy0j79hpw4sp8knmg";
      polysemy-resume = hackage "0.7.0.0" "1b9agh2qd0nrbd7cc5iabkzjb7g9lnzzy3pprvn33hr54va9p928";
      polysemy-time = hackage "0.6.0.0" "1ay0ym01wznk98km2ksw8slj52gc7rav6n16z4sndzsw7cdwdq2y";
    };

  in hix.lib.pro ({ config, lib, ...}: {
    packages = {
      polysemy-conc = ./packages/conc;
      polysemy-process = ./packages/process;
    };
    main = "polysemy-process";
    devGhc.compiler = "ghc902";
    overrides = { inherit dev all; };
    deps = [polysemy-time];
    hpack.packages = import ./ops/hpack.nix { inherit config lib; };
    hackage.versionFile = "ops/version.nix";
    ghci = {
      preludePackage = "incipit-core";
      preludeModule = "IncipitCore";
    };
  });
}
