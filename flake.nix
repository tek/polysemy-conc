{
  description = "Polysemy Effects for Concurrency";

  inputs.tryp-hs.url = github:tek/tryp-hs;

  outputs = { tryp-hs, ... }:
  let
    common = { hackage, only, ... }: {
      polysemy-test = hackage "0.3.1.1" "0x0zg1kljr7a1mwmm3zrmha5inz3l2pkldnq65fvsig8f3x8rsar";
      polysemy-time = hackage "0.1.2.1" "09l8r5fx0vnapdn8p0cwiwprgg3i67m58dd4j5hcdhw34gfqnnsr";
      polysemy-chronos = hackage "0.1.2.1" "0layan6jxg857n2dmxwnylichnk2ynlpxih5iya3q8x2nbndpbl2";
    };

    main = { hackage, ... }: {
      path = hackage "0.8.0" "0isldidz2gypw2pz399g6rn77x9mppd1mvj5h6ify4pj4mpla0pb";
      tasty-hedgehog = hackage "1.1.0.0" "0cs96s7z5csrlwj334v8zl459j5s4ws6gmjh59cv01wwvvrrjwd9";
      polysemy = hackage "1.5.0.0" "1xl472xqdxnp4ysyqnackpfn6wbx03rlgwmy9907bklrh557il6d";
      polysemy-plugin = hackage "0.3.0.0" "1frz0iksmg8bpm7ybnpz9h75hp6hajd20vpdvmi04aspklmr6hj0";
      relude = hackage "1.0.0.1" "164p21334c3pyfzs839cv90438naxq9pmpyvy87113mwy51gm6xn";
    };

    compat = { hackage, source, only, ... }: {
      polysemy = only "865" (hackage "1.4.0.0" "04bl0w7z35jh63jpy87sa1rrbgqhwn7c0pxsm5l3ww0pjnswkhjj");
    };
  in
  tryp-hs.flake {
    base = ./.;
    overrides = [common main];
    compatOverrides = [common compat];
    packages.polysemy-conc = "packages/conc";
    ghci.extraArgs = ["-fplugin=Polysemy.Plugin"];
    versionFile = "ops/hpack/shared/meta.yaml";
  };
}
