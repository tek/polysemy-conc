{
  description = "Polysemy effects for concurrency";

  inputs.hix.url = "git+https://git.tryp.io/tek/hix";

  outputs = { hix, ... }: hix.lib.pro ({config, ...}: let
    overrides = {jailbreak, unbreak, ...}: {
      polysemy-test = jailbreak unbreak;
    };
  in {
    ghcVersions = ["ghc94" "ghc96" "ghc98"];
    compat.versions = ["ghc96"];
    hackage.versionFile = "ops/version.nix";
    main = "polysemy-process";
    gen-overrides.enable = true;
    managed = {
      enable = true;
      lower.enable = true;
      sets = "each";
      envs.solverOverrides = overrides;
      latest.compiler = "ghc98";
    };

    inherit overrides;

    cabal = {
      license = "BSD-2-Clause-Patent";
      license-file = "LICENSE";
      author = "Torsten Schmits";
      prelude = {
        enable = true;
        package.name = "incipit-core";
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

      cabal.meta = {
        synopsis = "Polysemy effects for concurrency";
        category = "Concurrency";
      };

      library = {
        enable = true;
        dependencies = [
          "async"
          "polysemy"
          "polysemy-resume"
          "polysemy-time"
          "stm"
          "stm-chans"
          "torsor"
          "unagi-chan"
        ];
      };

      test = {
        enable = true;
        dependencies = [
          "async"
          "hedgehog"
          "polysemy"
          "polysemy-plugin"
          "polysemy-test"
          "polysemy-time"
          "tasty"
          "tasty-hedgehog"
          "time"
        ];
      };

    };

    packages.polysemy-process = {
      src = ./packages/process;

      cabal.meta = {
        synopsis = "Polysemy effects for system processes";
        category = "Process";
      };

      library = {
        enable = true;
        dependencies = [
          "async"
          "path"
          "path-io"
          "polysemy"
          config.packages.polysemy-conc.dep.minor
          "polysemy-resume"
          "polysemy-time"
          "posix-pty"
          "process"
          "stm-chans"
          "typed-process"
          "unix"
        ];
      };

      test = {
        enable = true;
        dependencies = [
          "async"
          "hedgehog"
          "polysemy"
          config.packages.polysemy-conc.dep.minor
          "polysemy-plugin"
          "polysemy-process"
          "polysemy-resume"
          "polysemy-test"
          "polysemy-time"
          "tasty"
          "tasty-expected-failure"
          "tasty-hedgehog"
          "typed-process"
          "unix"
        ];
      };

    };

  });
}
