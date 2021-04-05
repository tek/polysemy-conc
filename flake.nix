{
  description = "Polysemy Effects for Concurrency";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/c0e881852006b132236cbf0301bd1939bb50867e;
    flake-utils.url = github:numtide/flake-utils;
    tryp-hs.url = github:tek/tryp-hs;
    tryp-hs.inputs.nixpkgs.follows = "nixpkgs";
    polysemy-time.url = github:tek/polysemy-time;
    polysemy-test.follows = "polysemy-time/polysemy-test";
    polysemy.follows = "polysemy-time/polysemy";
  };

  outputs = { tryp-hs, polysemy-time, polysemy-test, polysemy, ... }@inputs:
  let
    overrides = { hackage, source, ... }: {
      path = hackage "0.8.0" "0isldidz2gypw2pz399g6rn77x9mppd1mvj5h6ify4pj4mpla0pb";
      tasty-hedgehog = hackage "1.1.0.0" "0cs96s7z5csrlwj334v8zl459j5s4ws6gmjh59cv01wwvvrrjwd9";
      polysemy = hackage "1.5.0.0" "1xl472xqdxnp4ysyqnackpfn6wbx03rlgwmy9907bklrh557il6d";
      polysemy-plugin = hackage "0.3.0.0" "1frz0iksmg8bpm7ybnpz9h75hp6hajd20vpdvmi04aspklmr6hj0";
      relude = hackage "1.0.0.1" "164p21334c3pyfzs839cv90438naxq9pmpyvy87113mwy51gm6xn";
      polysemy-test = source.sub polysemy-test "packages/polysemy-test";
      polysemy-time = source.sub polysemy-time "packages/time";
      polysemy-chronos = source.sub polysemy-time "packages/chronos";
    };

    compatOverrides = { hackage, source, only, ... }: {
      polysemy = only "865" (hackage "1.4.0.0" "04bl0w7z35jh63jpy87sa1rrbgqhwn7c0pxsm5l3ww0pjnswkhjj");
      polysemy-test = hackage "0.3.1.1" "0x0zg1kljr7a1mwmm3zrmha5inz3l2pkldnq65fvsig8f3x8rsar";
      polysemy-time = source.sub polysemy-time "packages/time";
      polysemy-chronos = source.sub polysemy-time "packages/chronos";
      # TODO wait for metadata on hackage to update
      # polysemy-time = hackage "0.1.2.0" "1v7lxmz36pkdx8rdgy2dm5cplvfrp4p65b1mncwmhrvvr26zb5bj";
      # polysemy-chronos = hackage "0.1.2.0" "192h42wx6j2a9612agv08z36h2za9m7havwh1d7vd5vpmkgyb8a3";
    };
  in
  tryp-hs.flake {
    base = ./.;
    compiler = "ghc8104";
    main = "polysemy-conc";
    overrides = tryp-hs.overrides overrides;
    compatOverrides = tryp-hs.overrides compatOverrides;
    packages.polysemy-conc = "packages/conc";
    ghci.extraArgs = ["-fplugin=Polysemy.Plugin"];
    ghcid.prelude = "packages/conc/lib/Prelude.hs";
  };
}
