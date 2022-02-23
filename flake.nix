{
  description = "Polysemy Effects for Concurrency";

  inputs.hix.url = github:tek/hix;

  outputs = { hix, ... }:
  let

    ghc921 = { hackage, jailbreak, notest, ... }: {
      polysemy = hackage "1.7.1.0" "0qwli1kx3hk68hqsgw65mk81bx0djw1wlk17v8ggym7mf3lailyc";
      polysemy-plugin = hackage "0.4.3.0" "1r7j1ffsd6z2q2fgpg78brl2gb0dg8r5ywfiwdrsjd2fxkinjcg1";
      type-errors = notest;
    };

    # ghc884 = { hackage, ... }: {
    #   polysemy-test = hackage "0.3.1.7" "0j33f5zh6gyhl86w8kqh6nm02915b4n32xikxc4hwcy7p5l7cl34";
    #   polysemy-time = hackage "0.1.4.0" "0hwx89cilmsdjs3gb5w6by87ysy24scgj5zg77vbfnqpzr3ifrwh";
    # };

    all = { hackage, unbreak, ... }: {
      polysemy = hackage "1.6.0.0" "15k51ysrfcbkww1562g8zvrlzymlk2rxhcsz9ipsb0q6h571qgvf";
      polysemy-plugin = hackage "0.4.1.0" "117g92l1ppsqd3w0rqjrxfk0lx6yndd54rpymgxljilnv43zg29s";
      polysemy-resume = hackage "0.3.0.0" "0kv8x41cxrdwxh7xw8vrywl7sgjkigl84xl7gv038gijh7pvd358";
      polysemy-test = hackage "0.4.0.1" "038n31xxid72vrckr3afgkvbsvqhf9q4b912agg24ppjzckq2s15";
      polysemy-time = hackage "0.3.0.0" "0mgiq70b35q7ymfwvb8fv291l3f8v7k0z7w6909h922d6jgl4jgp";
      incipit-base = hackage "0.1.0.1" "0bcygln28zhrp0jqsm1z8p45k7faas5yamwddz2narsgpkzirx4y";
      incipit-core = hackage "0.1.0.1" "1bdkw0q4db3k73i3jjhil96p3rz3gw7mq9jcpcphamld72f4f5ni";
    };

  in hix.lib.flake {
    base = ./.;
    overrides = { inherit all ghc921; };
    packages = {
      polysemy-conc = ./packages/conc;
      polysemy-process = ./packages/process;
    };
    main = "polysemy-process";
    hackage.versionFile = "ops/hpack/shared/meta.yaml";
    ghci.preludePackage = "incipit-core";
  };
}
