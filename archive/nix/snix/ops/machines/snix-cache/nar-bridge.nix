{ config, pkgs, ... }:
{
  imports = [ ./nar-bridge-module.nix ];

  # Microbenchmark
  # hyperfine --warmup 1 'rm -rf /tmp/cache; nix copy --from https://nixos.snix.store/ --to "file:///tmp/cache?compression=none" /nix/store/jlkypcf54nrh4n6r0l62ryx93z752hb2-firefox-132.0'
  services.nginx = {
    package = pkgs.nginxStable;
    virtualHosts.${config.machine.domain} = {
      locations."=/" = {
        tryFiles = "$uri $uri/index.html =404";
        root =
          pkgs.runCommand "index"
            {
              nativeBuildInputs = [ pkgs.markdown2html-converter ];
            }
            ''
              mkdir -p $out
              markdown2html-converter ${./README.md} -o $out/index.html
            '';
      };
      locations."/" = {
        proxyPass = "http://unix:/run/nar-bridge.sock:/";
        extraConfig = ''
          # Sometimes it takes a while to download and unpack from upstream.
          proxy_read_timeout 180s;

          # Restrict allowed HTTP methods
          limit_except GET HEAD {
            # nar bridge allows to upload nars via PUT
            deny all;
          }

          # Propagate content-encoding to the backend
          proxy_set_header Accept-Encoding $http_accept_encoding;

          # Enable proxy cache
          proxy_cache nar-bridge;
          proxy_cache_key "$scheme$proxy_host$request_uri";
          proxy_cache_valid 200 301 302 10m;  # Cache responses for 10 minutes
          proxy_cache_valid 404 1m;  # Cache 404 responses for 1 minute
          proxy_cache_min_uses 2;  # Cache only if the object is requested at least twice
          proxy_cache_use_stale error timeout updating;
        '';
      };
      # Rewrite old nar/tvix-castore/…?narsize=… requests to the new URL
      # Clients keep these paths in their narinfo cache annoyingly long.
      locations."/nar/tvix-castore".extraConfig = ''
        rewrite ^/nar/tvix-castore/(.*)$ /nar/snix-castore/$1 redirect;
      '';
    };
    virtualHosts."nixos.tvix.store" = {
      forceSSL = true;
      enableACME = true;
      locations."/".return = "301 https://nixos.snix.store$request_uri";
    };

    # use more cores for compression
    appendConfig = ''
      worker_processes auto;
    '';

    proxyCachePath."nar-bridge" = {
      enable = true;
      levels = "1:2";
      keysZoneName = "nar-bridge";
      # Put our 1TB NVME to good use
      maxSize = "200G";
      inactive = "10d";
      useTempPath = false;
    };
  };

  services.nar-bridge = {
    enable = true;

    settings = {
      blobservices = {
        root = {
          type = "objectstore";
          object_store_url = "file:///tank/nar-bridge/blobs.object_store";
          object_store_options = { };
        };
      };

      directoryservices = {
        root = {
          type = "redb";
          is_temporary = false;
          path = "/var/lib/nar-bridge/directories.redb";
        };
      };

      pathinfoservices = {
        root = {
          type = "cache";
          near = "redb";
          far = "cache-nixos-org";
        };

        redb = {
          type = "redb";
          is_temporary = false;
          path = "/var/lib/nar-bridge/pathinfo.redb";
        };

        "cache-nixos-org" = {
          type = "nix";
          base_url = "https://cache.nixos.org";
          blob_service = "root";
          directory_service = "root";
          public_keys = [
            "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
          ];
        };
      };
    };
  };

  systemd.tmpfiles.rules = [
    # Put the blobs on the big disk
    "d /tank/nar-bridge 0755 nar-bridge nar-bridge -"
    "d /tank/nar-bridge/blobs.object_store 0755 nar-bridge nar-bridge -"
    # Cache responses on NVME
    "d /var/cache/nginx 0755 ${config.services.nginx.user} ${config.services.nginx.group} -"
  ];

  systemd.services.nar-bridge = {
    unitConfig = {
      # Keep most data on the SSD which is at /var/lib/nar-bridge, but bind-mount the blobs in
      RequiresMountsFor = "/tank";
    };

    # twice the normal allowed limit, same as nix-daemon
    serviceConfig.LimitNOFILE = "1048576";
  };
}
