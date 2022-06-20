{ config, lib, ... }:
with builtins;
with lib;
let

  mergeAttr = a: b:
  if isAttrs a
  then merge a b
  else if isList a
  then a ++ b
  else b;

  merge = l: r:
  let
    f = name:
    if hasAttr name l && hasAttr name r
    then mergeAttr l.${name} r.${name}
    else l.${name} or r.${name};
  in genAttrs (concatMap attrNames [l r]) f;

  paths = name: {
    when = {
      condition = false;
      generated-other-modules = ["Paths_${replaceStrings ["-"] ["_"] name}"];
    };
  };

  meta = {
    version = import ./version.nix;
    license = "BSD-2-Clause-Patent";
    license-file = "LICENSE";
    author = "Torsten Schmits";
    maintainer = "hackage@tryp.io";
    copyright = "2022 Torsten Schmits";
    category = "Concurrency";
    build-type = "Simple";
    github = "tek/polysemy-conc";
  };

  options.ghc-options = [
    "-Wall"
    "-Wredundant-constraints"
    "-Wincomplete-uni-patterns"
    "-Wmissing-deriving-strategies"
    "-Widentities"
    "-Wunused-packages"
  ];

  dependencies = [
      { name = "base"; version = ">= 4.12 && < 5"; mixin = "hiding (Prelude)"; }
      { name = "incipit-core"; version = ">= 0.3"; mixin = ["(IncipitCore as Prelude)" "hiding (IncipitCore)"]; }
    ];

  project = name: doc: merge (meta // { library = paths name; } // options) {
    inherit name;
    description = "See https://hackage.haskell.org/package/${name}/docs/${doc}.html";
    library = {
      source-dirs = "lib";
      inherit dependencies;
    };
    default-extensions = config.ghci.extensions;
    extra-source-files = ["changelog.md" "readme.md"];
  };

  exe = name: dir: merge (paths name // {
    main = "Main.hs";
    source-dirs = dir;
    inherit dependencies;
    ghc-options = [
      "-threaded"
      "-rtsopts"
      "-with-rtsopts=-N"
    ];
  });

in {

  polysemy-conc = merge (project "polysemy-conc" "Polysemy-Conc") {
    synopsis = "Polysemy effects for concurrency";
    library.dependencies = [
      "async"
      "containers"
      "polysemy >= 1.6"
      "polysemy-resume >= 0.5"
      "polysemy-time >= 0.3"
      "stm"
      "stm-chans >= 2"
      "torsor >= 0.1"
      "unagi-chan >= 0.4"
      "unix"
    ];
    tests.polysemy-conc-unit = exe "polysemy-conc" "test" {
      dependencies = [
        "async"
        "hedgehog"
        "polysemy"
        "polysemy-conc"
        "polysemy-plugin"
        "polysemy-resume"
        "polysemy-test"
        "polysemy-time"
        "stm"
        "tasty"
        "tasty-hedgehog"
        "time"
        "unagi-chan"
        "unix"
      ];
    };
  };

  polysemy-process = merge (project "polysemy-process" "Polysemy-Process") {
    synopsis = "Polysemy effects for system processes";
    library.dependencies = [
      "path >= 0.7"
      "path-io >= 1.6.2"
      "polysemy >= 1.6"
      "polysemy-conc >= 0.9"
      "polysemy-resume >= 0.5"
      "polysemy-time >= 0.5"
      "posix-pty >= 0.2"
      "process"
      "stm-chans >= 2"
      "typed-process >= 0.2.6"
      "unix"
    ];
    tests.polysemy-process-unit = exe "polysemy-process" "test" {
      dependencies = [
        "polysemy"
        "polysemy-conc"
        "polysemy-plugin"
        "polysemy-process"
        "polysemy-resume"
        "polysemy-test >= 0.6"
        "polysemy-time"
        "tasty"
        "tasty-expected-failure"
        "typed-process"
      ];
    };
  };

}
