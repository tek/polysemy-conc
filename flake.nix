{
  description = "Polysemy Effects for Concurrency";

  inputs.hix.url = github:tek/hix;

  outputs = { hix, ... }:
  let
    common = { hackage, source, jailbreak, ... }: {
      path = hackage "0.9.0" "14symzl1rszvk5zivv85k79anz7xyl5gaxy0sm4vhhzsgxc59msv";
      path-io = jailbreak (hackage "1.6.3" "05hcxgyf6kkz36mazd0fqwb6mjy2049gx3vh8qq9h93gfjkpp2vc");
      polysemy = hackage "1.6.0.0" "15k51ysrfcbkww1562g8zvrlzymlk2rxhcsz9ipsb0q6h571qgvf";
      polysemy-plugin = hackage "0.4.0.0" "0pah1a8h8ckbv2fq20hrikrd1p5a3bdxr03npkyixc6mv5k1rmck";
      relude = hackage "1.0.0.1" "164p21334c3pyfzs839cv90438naxq9pmpyvy87113mwy51gm6xn";
    };

    compat901 = { hackage, source, jailbreak, minimal, noHpack, ... }: {
      type-errors-pretty = jailbreak;
    };

    compat8104 = { hackage, only, unbreak, jailbreak, super, ... }: {
      polysemy = jailbreak (unbreak super.polysemy);
    };

    compat = { hackage, only, unbreak, jailbreak, super, ... }: {
      polysemy-test = hackage "0.3.1.6" "0bfh37l68a5chhjfr7gqcffsmvdgg5hqclxi0fc5xnqni2mg81ak";
      polysemy-time = hackage "0.1.3.1" "1ldg92dmy1nyjhkbmh5k32q94pn2c7qcfjc4yhl4lc1wnfp6r59m";
    };

    main = { hackage, ... }: {
      tasty-hedgehog = hackage "1.1.0.0" "0cs96s7z5csrlwj334v8zl459j5s4ws6gmjh59cv01wwvvrrjwd9";
    };
  in
  hix.flake {
    base = ./.;
    overrides = [common compat main];
    compatOverrides = { all = compat; ghc901 = [common compat901]; ghc8104 = compat8104; };
    packages.polysemy-conc = ./packages/conc;
    ghci.extraArgs = ["-fplugin=Polysemy.Plugin"];
    ghcid.easy-hls = false;
    versionFile = "ops/hpack/shared/meta.yaml";
  };
}
