{
  description = "Polysemy Effects for Concurrency";

  inputs.hix.url = github:tek/hix;

  outputs = { hix, ... }:
  let

    ghc884 = { hackage, ... }: {
      polysemy-test = hackage "0.3.1.7" "0j33f5zh6gyhl86w8kqh6nm02915b4n32xikxc4hwcy7p5l7cl34";
      polysemy-time = hackage "0.1.4.0" "0hwx89cilmsdjs3gb5w6by87ysy24scgj5zg77vbfnqpzr3ifrwh";
    };

    ghc902 = { hackage, ... }: {
      relude = hackage "1.0.0.1" "164p21334c3pyfzs839cv90438naxq9pmpyvy87113mwy51gm6xn";
    };

    all = { hackage, unbreak, ... }: {
      exon = hackage "0.2.0.1" "0hs0xrh1v64l1n4zqx3rqfjdh6czxm7av85kj1awya9zxcfcy5cl";
      flatparse = hackage "0.3.2.0" "01w71985b9ndg4wkfxqxjj7f1cynji6vp71akr7ivpmxn2drxspa";
      polysemy = hackage "1.6.0.0" "15k51ysrfcbkww1562g8zvrlzymlk2rxhcsz9ipsb0q6h571qgvf";
      polysemy-plugin = hackage "0.4.1.0" "117g92l1ppsqd3w0rqjrxfk0lx6yndd54rpymgxljilnv43zg29s";
      polysemy-resume = hackage "0.2.0.0" "0kh7cwqkr5w69zkm68l6q4d8nkai7fc29n48p3f8skqw638x4w9p";
    };

  in hix.lib.flake {
    base = ./.;
    overrides = { inherit ghc884 ghc902 all; };
    packages = {
      polysemy-conc = ./packages/conc;
      polysemy-process = ./packages/process;
    };
    main = "polysemy-process";
    versionFile = "ops/hpack/shared/meta.yaml";
  };
}
