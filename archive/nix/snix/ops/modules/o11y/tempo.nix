{
  config,
  lib,
  ...
}:
let
  cfg = config.services.depot.tempo;
  inherit (lib) mkEnableOption mkIf;
in
{
  options.services.depot.tempo.enable = mkEnableOption "Tempo trace store";

  config = mkIf cfg.enable {
    services.tempo = {
      enable = true;
      extraFlags = [ "--config.expand-env=true" ];
      settings = {
        multitenancy_enabled = false;
        stream_over_http_enabled = true;

        server = {
          http_listen_port = 9190;
          grpc_listen_port = 9195;
        };
        distributor.receivers.otlp.protocols.http.endpoint = "127.0.0.1:4138";

        # TODO: S3
        storage.trace = {
          backend = "s3";
          s3 = {
            endpoint = "fsn1.your-objectstorage.com";
            bucket = "snix-tempo";
            secret_key = "\${S3_KEY}"; # This is a secret injected via an environment variable
            access_key = "\${S3_KEY_ID}";
          };
          wal.path = "/var/lib/tempo/traces-wal";
        };

        metrics_generator.storage = {
          path = "/var/lib/tempo/metrics-wal";
          remote_write = [
            {
              url = "http://127.0.0.1:9009/api/v1/push";
            }
          ];
        };

        overrides.defaults.metrics_generator.processors = [ "span-metrics" ];
      };
    };

    systemd.services.tempo.serviceConfig.EnvironmentFile = [
      config.age.secrets.tempo-environment.path
    ];

    services.nginx = {
      upstreams.tempo = {
        servers."${config.services.tempo.settings.distributor.receivers.otlp.protocols.http.endpoint}" =
          { };
        extraConfig = "keepalive 16;";
      };

      virtualHosts."tempo.snix.dev" = {
        enableACME = true;
        forceSSL = true;
        locations."/" = {
          proxyPass = "http://tempo";
          basicAuthFile = config.age.secrets.metrics-push-htpasswd.path;
        };
      };
    };

    infra.monitoring.alloy.exporters.tempo.port = 9190;
  };
}
