{
  description = "Polysemy Effects for Concurrency";

  inputs.hix.url = github:tek/hix;
  inputs.polysemy-resume.url = github:tek/polysemy-resume;
  inputs.polysemy-time.url = github:tek/polysemy-time;

  outputs = { hix, polysemy-resume, polysemy-time, ... }:
  let

    ghc921 = { hackage, jailbreak, notest, ... }: {
      polysemy = hackage "1.7.1.0" "0qwli1kx3hk68hqsgw65mk81bx0djw1wlk17v8ggym7mf3lailyc";
      polysemy-plugin = hackage "0.4.3.0" "1r7j1ffsd6z2q2fgpg78brl2gb0dg8r5ywfiwdrsjd2fxkinjcg1";
    };

    all = { hackage, unbreak, source, ... }: {
      incipit-base = hackage "0.2.0.0" "12979prkjk1kr1556mwsgf1v04rzd67xg68x6q9pnvm41pxbvk5w";
      incipit-core = hackage "0.2.0.0" "1v4xrqwcylbk32b6hzl6i7k0964varw2iy73s7mkjxpxpdg432ci";
      polysemy = hackage "1.6.0.0" "15k51ysrfcbkww1562g8zvrlzymlk2rxhcsz9ipsb0q6h571qgvf";
      polysemy-plugin = hackage "0.4.1.0" "117g92l1ppsqd3w0rqjrxfk0lx6yndd54rpymgxljilnv43zg29s";
      polysemy-resume = source.package polysemy-resume "resume";
      polysemy-time = hackage "0.4.0.0" "1dddg61d8djfwlc85bz99vwm23621cdjwxd1llcc4ng3afgx5bg9";
      polysemy-test = hackage "0.5.0.0" "0lzbf7bfmcima8ib4hv68bjciy2n5s4x493g0a5cmdjj6pcg2d2k";
    };

  in hix.lib.flake {
    base = ./.;
    packages = {
      polysemy-conc = ./packages/conc;
      polysemy-process = ./packages/process;
    };
    main = "polysemy-process";
    overrides = { inherit all ghc921; };
    deps = [polysemy-time];
    hackage.versionFile = "ops/hpack/shared/meta.yaml";
    ghci.preludePackage = "incipit-core";
  };
}
