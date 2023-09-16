{
  description = "Polysemy effects for concurrency";

  inputs = {
    hix.url = "git+https://git.tryp.io/tek/hix";
    polysemy-time.url = "git+https://git.tryp.io/tek/polysemy-time";
  };

  outputs = { hix, polysemy-time, ... }: hix.lib.pro ({config, ...}: {
    ghcVersions = ["ghc90" "ghc92" "ghc94"];
    hackage.versionFile = "ops/version.nix";
    main = "polysemy-process";
    deps = [polysemy-time];
    gen-overrides.enable = true;
    compiler = "ghc94";

    envs.dev.overrides = { hackage, ... }: {
      incipit-base = hackage "0.5.1.0" "0hkqnqpdw8rvg4xzslw9sp3684ggyk9n4hr0lczwm8b0pzakzs0l";
      incipit-core = hackage "0.5.1.0" "04lyzycvqxyjqcd703cd33lnlk5va9wj3czpsybah0ybydnrwabd";
      polysemy = hackage "1.9.1.0" "05mhzjz6hz0dnxsn3cc0l6yyj5ch35gn8xfnx0a1gn3q8yljfg2a";
      polysemy-plugin = hackage "0.4.5.0" "0v2k0l42zaangwv050xfv5jdqfrbvdxfr533291ndsxalv8n3xi8";
      polysemy-resume = hackage "0.7.0.0" "1b9agh2qd0nrbd7cc5iabkzjb7g9lnzzy3pprvn33hr54va9p928";
      polysemy-time = hackage "0.6.0.0" "1ay0ym01wznk98km2ksw8slj52gc7rav6n16z4sndzsw7cdwdq2y";
    };

    cabal = {
      license = "BSD-2-Clause-Patent";
      license-file = "LICENSE";
      author = "Torsten Schmits";
      prelude = {
        enable = true;
        package = {
          name = "incipit-core";
          version = "^>= 0.5";
        };
        module = "IncipitCore";
      };
      meta = {
        maintainer = "hackage@tryp.io";
        github = "tek/polysemy-conc";
        extra-source-files = ["readme.md" "changelog.md"];
      };
    };

    packages.polysemy-conc = {
      src = ./packages/conc;

      cabal.meta.synopsis = "Polysemy effects for concurrency";
      cabal.meta.category = "Concurrency";

      library = {
        enable = true;
        dependencies = [
          "async"
          "containers"
          "polysemy ^>= 1.9"
          "polysemy-resume ^>= 0.7"
          "polysemy-time ^>= 0.6"
          "stm"
          "stm-chans ^>= 3"
          "torsor ^>= 0.1"
          "unagi-chan ^>= 0.4"
          "unix"
        ];
      };

      test = {
        enable = true;
        dependencies = [
          "async"
          "hedgehog >= 1.1 && < 1.3"
          "polysemy ^>= 1.9"
          "polysemy-conc"
          "polysemy-plugin ^>= 0.4.4"
          "polysemy-test >= 0.6 && < 0.8"
          "polysemy-time ^>= 0.6"
          "stm"
          "tasty ^>= 1.4"
          "tasty-hedgehog >= 1.3 && < 1.5"
          "time"
          "unix"
        ];
      };

    };

    packages.polysemy-process = {
      src = ./packages/process;

      cabal.meta.synopsis = "Polysemy effects for system processes";
      cabal.meta.category = "Process";

      library = {
        enable = true;
        dependencies = [
          "path ^>= 0.9"
          "path-io >= 1.7 && < 1.9"
          "polysemy ^>= 1.9"
          "polysemy-conc ^>= 0.12"
          "polysemy-resume ^>= 0.7"
          "polysemy-time ^>= 0.6"
          "posix-pty ^>= 0.2"
          "process"
          "stm-chans ^>= 3"
          "typed-process ^>= 0.2.6"
          "unix"
        ];
      };

      test = {
        enable = true;
        dependencies = [
          "polysemy"
          "polysemy-conc"
          "polysemy-plugin ^>= 0.4.4"
          "polysemy-process"
          "polysemy-resume"
          "polysemy-test >= 0.6 && < 0.8"
          "polysemy-time ^>= 0.6"
          "tasty ^>= 1.4"
          "tasty-expected-failure ^>= 0.12"
          "typed-process"
        ];
      };

    };

  });
}
