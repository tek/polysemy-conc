{
  description = "Polysemy Effects for Concurrency";

  inputs.hix.url = github:tek/hix;

  outputs = { hix, ... }:
  let

    compat901 = { jailbreak, ... }: {
      type-errors-pretty = jailbreak;
    };

    compat = { hackage, ... }: {
      polysemy-resume = hackage "0.2.0.0" "0kh7cwqkr5w69zkm68l6q4d8nkai7fc29n48p3f8skqw638x4w9p";
      polysemy-test = hackage "0.3.1.7" "0j33f5zh6gyhl86w8kqh6nm02915b4n32xikxc4hwcy7p5l7cl34";
      polysemy-time = hackage "0.1.3.2" "1s06c1jwsq9ckfcq2cwwpiy5a2a0lj8j63zg4jr2kidpd2lkk6cd";
    };

    all = { hackage, only, unbreak, jailbreak, super, ... }: {
      polysemy = hackage "1.6.0.0" "15k51ysrfcbkww1562g8zvrlzymlk2rxhcsz9ipsb0q6h571qgvf";
      polysemy-plugin = hackage "0.4.1.0" "117g92l1ppsqd3w0rqjrxfk0lx6yndd54rpymgxljilnv43zg29s";
      relude = hackage "1.0.0.1" "164p21334c3pyfzs839cv90438naxq9pmpyvy87113mwy51gm6xn";
    };

  in hix.flake {
    base = ./.;
    overrides = all;
    compatOverrides = { all = all; ghc901 = compat901; ghc884 = compat; ghc8107 = compat; };
    packages = {
      polysemy-conc = ./packages/conc;
      polysemy-process = ./packages/process;
    };
    main = "polysemy-process";
    ghcid.easy-hls = false;
    versionFile = "ops/hpack/shared/meta.yaml";
  };
}
