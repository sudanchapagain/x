{
    inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    outputs =
        { self, nixpkgs }:
        {
            devShells.x86_64-linux.default =
                let
                    pkgs = nixpkgs.legacyPackages.x86_64-linux;
                in
                pkgs.mkShell {
                    packages = [
                        pkgs.gst_all_1.gstreamer
                        pkgs.gst_all_1.gst-plugins-base
                        pkgs.gst_all_1.gst-plugins-good
                        pkgs.gst_all_1.gst-plugins-bad
                        pkgs.pkg-config
                    ];
                };
        };
}
