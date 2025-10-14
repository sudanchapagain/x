{
  depot,
  lib,
  pkgs,
  ...
}:

depot.nix.readTree.drvTargets rec {
  terraform = pkgs.terraform.withPlugins (p: [
    p.minio
  ]);

  validate = depot.tools.checks.validateTerraform {
    inherit terraform;
    name = "hetzner-s3";
    src = lib.cleanSource ./.;
  };
}
