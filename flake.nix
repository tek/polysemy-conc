{
  description = "Polysemy effects for concurrency";

  inputs.hix.url = "git+https://git.tryp.io/tek/hix";

  outputs = {hix, ...}: hix.lib.pro ({config, ...}: {
    ghcVersions = ["ghc98" "ghc910" "ghc912"];
    main = "polysemy-process";
    gen-overrides.enable = true;

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
          "torsor"
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

    cabal = {
      license = "BSD-2-Clause-Patent";
      license-file = "LICENSE";
      author = "Torsten Schmits";
      language = "GHC2021";
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

    managed = {
      enable = true;
      lower.enable = true;
      sets = "each";
      latest.compiler = "ghc912";
      lower.compiler = "ghc94";
    };

    package-sets.ghc912.overrides = {jailbreak, ...}: {
      polysemy-resume = jailbreak;
      polysemy-test = jailbreak;
      polysemy-time = jailbreak;
    };

    hackage.repos."hackage.haskell.org".user = "tek";

    internal.hixCli.dev = true;

  });
}
