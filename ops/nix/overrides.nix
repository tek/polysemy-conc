inputs:
{
  hackage,
  ...
}:
self: super:
let
  inherit (hackage) pack cabal2nix cabal2nixNoHpack subPkg subPkgNoHpack github;

  versions = [
    (pack "path" "0.8.0" "0isldidz2gypw2pz399g6rn77x9mppd1mvj5h6ify4pj4mpla0pb")
    (pack "path-io" "0.3.1" "07m7q36pdkqk18bmf0lkafjc9npksym7dhn2am1m9c1rvj3b26qf")
  ];
  versionOverrides = builtins.listToAttrs versions;

  custom = {
    polysemy = cabal2nixNoHpack "polysemy" inputs.polysemy;
    polysemy-test = subPkg "packages/polysemy-test" "polysemy-test" inputs.polysemy-test;
    polysemy-time = subPkg "packages/time" "polysemy-time" inputs.polysemy-time;
  };
in
  versionOverrides // custom
