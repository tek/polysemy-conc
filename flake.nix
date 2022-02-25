{
  description = "Polysemy Effects for Concurrency";

  inputs.hix.url = github:tek/hix;
  inputs.incipit-core.url = github:tek/incipit-core;
  inputs.polysemy-resume.url = github:tek/polysemy-resume;
  inputs.polysemy-time.url = github:tek/polysemy-time;

  outputs = { hix, incipit-core, polysemy-resume, polysemy-time, ... }:
  let

    ghc921 = { hackage, jailbreak, notest, ... }: {
      polysemy = hackage "1.7.1.0" "0qwli1kx3hk68hqsgw65mk81bx0djw1wlk17v8ggym7mf3lailyc";
      polysemy-plugin = hackage "0.4.3.0" "1r7j1ffsd6z2q2fgpg78brl2gb0dg8r5ywfiwdrsjd2fxkinjcg1";
      type-errors = notest;
    };

    all = { hackage, unbreak, source, ... }: {
      polysemy = hackage "1.6.0.0" "15k51ysrfcbkww1562g8zvrlzymlk2rxhcsz9ipsb0q6h571qgvf";
      polysemy-plugin = hackage "0.4.1.0" "117g92l1ppsqd3w0rqjrxfk0lx6yndd54rpymgxljilnv43zg29s";
      polysemy-resume = source.package polysemy-resume "resume";
    };

  in hix.lib.flake {
    base = ./.;
    packages = {
      polysemy-conc = ./packages/conc;
      polysemy-process = ./packages/process;
    };
    main = "polysemy-process";
    overrides = { inherit all ghc921; };
    deps = [incipit-core polysemy-time];
    hackage.versionFile = "ops/hpack/shared/meta.yaml";
    ghci.preludePackage = "incipit-core";
  };
}
