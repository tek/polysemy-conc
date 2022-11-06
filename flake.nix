{
  description = "Polysemy Effects for Concurrency";

  inputs = {
    hix.url = git+https://git.tryp.io/tek/hix;
    polysemy-time.url = github:tek/polysemy-time;
  };

  outputs = { hix, polysemy-time, ... }:
  let

    ghc924 = { hackage, jailbreak, notest, ... }: {
      polysemy = hackage "1.7.1.0" "0qwli1kx3hk68hqsgw65mk81bx0djw1wlk17v8ggym7mf3lailyc";
      polysemy-plugin = hackage "0.4.3.0" "1r7j1ffsd6z2q2fgpg78brl2gb0dg8r5ywfiwdrsjd2fxkinjcg1";
      type-errors = notest;
    };

    all = { hackage, source, ... }: {
      incipit-base = hackage "0.3.0.0" "1078yyl5k94c9pr16rqd1i1g1fj8zx4iswhk7rcxb8f10fjqzapg";
      incipit-core = hackage "0.3.0.0" "0q11zmxlpdb72p8c8zvr5hd7qca9c37crm70lm16jxlzw1qxk51b";
      polysemy = hackage "1.6.0.0" "15k51ysrfcbkww1562g8zvrlzymlk2rxhcsz9ipsb0q6h571qgvf";
      polysemy-plugin = hackage "0.4.1.0" "117g92l1ppsqd3w0rqjrxfk0lx6yndd54rpymgxljilnv43zg29s";
      polysemy-resume = hackage "0.5.0.0" "1yavr2h31ffxj861vscm2hddrwi977ddx0sn0hh47zn78pqafz77";
      polysemy-time = hackage "0.5.0.0" "1y7zqzvpah019hh9dqxhv9vh690b7nqchq613hlfsc4xgrvzq3d4";
      polysemy-test = hackage "0.6.0.0" "07pi549ral22sxhja67k5b9v787q0b32ysp0bq9szhwjqgxsab46";
    };

  in hix.lib.pro ({ config, lib, ...}: {
    packages = {
      polysemy-conc = ./packages/conc;
      polysemy-process = ./packages/process;
    };
    main = "polysemy-process";
    devGhc.compiler = "ghc902";
    overrides = { inherit all ghc924; };
    deps = [polysemy-time];
    hpack.packages = import ./ops/hpack.nix { inherit config lib; };
    hackage.versionFile = "ops/version.nix";
    ghci = {
      preludePackage = "incipit-core";
      preludeModule = "IncipitCore";
    };
  });
}
