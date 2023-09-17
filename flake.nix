{
  description = "Polysemy effects for concurrency";

  inputs = {
    hix.url = "git+https://git.tryp.io/tek/hix";
    polysemy-time.url = "git+https://git.tryp.io/tek/polysemy-time";
  };

  outputs = { hix, polysemy-time, ... }: hix.lib.pro ({config, ...}: {
    ghcVersions = ["ghc92" "ghc94" "ghc96"];
    hackage.versionFile = "ops/version.nix";
    main = "polysemy-process";
    deps = [polysemy-time];
    gen-overrides.enable = true;
    compiler = "ghc94";

    envs.ghc96.overrides = { hackage, ... }: {
      polysemy-resume = hackage "0.7.0.0" "1b9agh2qd0nrbd7cc5iabkzjb7g9lnzzy3pprvn33hr54va9p928";
    };

    envs.ghc94.overrides = { hackage, ... }: {
      polysemy-resume = hackage "0.7.0.0" "1b9agh2qd0nrbd7cc5iabkzjb7g9lnzzy3pprvn33hr54va9p928";
    };

    envs.ghc92.overrides = { hackage, ... }: {
      polysemy-resume = hackage "0.7.0.0" "1b9agh2qd0nrbd7cc5iabkzjb7g9lnzzy3pprvn33hr54va9p928";
    };

    envs.dev.overrides = { hackage, ... }: {
      polysemy = hackage "1.9.1.2" "01vkiqxcjvvihgg8dvws76sfg0d98z8xyvpnj3g3nz02i078xf8j";
      polysemy-plugin = hackage "0.4.5.1" "0afmx1vdgmvggk4sb4av91qnm8b3hr2kb4adcj9fhzq2w50393bc";
      polysemy-resume = hackage "0.8.0.0" "1mh050fxlkvhdd8knf9dlakf3zqij3rxh8ac1zb6mwhp4j6y1dqn";
      polysemy-time = hackage "0.6.0.1" "1rkpjgx1jrdc50ma6y32mv77516qz9py80h97z3qijl0qi10hw10";
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
          "polysemy-resume >= 0.7 && < 0.9"
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
          "polysemy-plugin ^>= 0.4.4"
          "polysemy-test >= 0.6 && < 0.9"
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
          config.packages.polysemy-conc.dep.minor
          "polysemy-resume >= 0.7 && < 0.9"
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
          config.packages.polysemy-conc.dep.minor
          "polysemy-plugin ^>= 0.4.4"
          "polysemy-process"
          "polysemy-resume >= 0.7 && < 0.9"
          "polysemy-test >= 0.6 && < 0.9"
          "polysemy-time ^>= 0.6"
          "tasty ^>= 1.4"
          "tasty-expected-failure ^>= 0.12"
          "typed-process"
        ];
      };

    };

  });
}
