# A NixOS module that you can import to create hooks.
{ config, lib, pkgs, ...}: with lib;

let
  cfg = config.services.personal-webhooks;
  package = import ../default.nix { inherit pkgs; };
  workingDir = "/var/lib/webhooks";

  hookOptions = {
    options = {
      name = mkOption {
        type = types.str;
        description = "The name of this hook.";
      };

      script = mkOption {
        type = types.path;
        description = ''
          The script to run on incoming JSON data.  The data will be
          given to its STDIN.
        '';
      };

      path = mkOption {
        type = types.listOf types.package;
        default = [ ];
        description = "List of packages to put in PATH.";
      };

      user = mkOption {
        type = types.str;
        description = "The user to run the script as.";
      };
    };
  };

  hookToService = hook: {
    description = "${hook.name} Webhook";
    wantedBy = [ "multi-user.target" ];
    after = [ "network.service" "personal-webhooks.service" ];
    path = hook.path ++ (with pkgs; [ bash coreutils ]);

    serviceConfig = {
      WorkingDirectory = "~";
      Restart = "always";
      Type = "simple";
      TimeoutStopSec = "10s";

      PermissionsStartOnly = true;
      User = hook.user;
      UMask = "0007";
    };

    preStart = ''
      mkdir -p ${cfg.pipesDirectory}/${hook.user}
      chown -R ${hook.user}:${cfg.group} ${cfg.pipesDirectory}/${hook.user}
      chmod -R 0711 ${cfg.pipesDirectory}
      chmod -R 0755 ${cfg.pipesDirectory}/${hook.user}
    '';

    script = ''
      export examples=$(find "${package.data}" -type d -name examples)

      bash "$examples"/watchfifo.sh \
      -f "${cfg.pipesDirectory}/${hook.user}/${hook.name}" \
      -g "${cfg.group}" \
      -- ${hook.script}
    '';
  };

  mainService = {
    personal-webhooks = {
      description = "Personal Webhooks Server";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.service" "postgresql.service" ];

      serviceConfig = {
        WorkingDirectory = "-${workingDir}";
        ExecStart = "${cfg.package}/bin/webhooks -c ${cfg.configFile} server -p ${toString cfg.port}";
        Restart = "always";
        Type = "simple";

        PermissionsStartOnly = true;
        User = cfg.user;
        Group = cfg.group;
        UMask = "0077";
      };

      preStart = ''
        ${pkgs.coreutils}/bin/mkdir -p ${workingDir} ${cfg.pipesDirectory}
        ${pkgs.coreutils}/bin/chown -R ${cfg.user}:${cfg.group} ${workingDir} ${cfg.pipesDirectory}
        ${pkgs.coreutils}/bin/chmod -R 0700 ${workingDir}
        ${pkgs.coreutils}/bin/chmod -R 0770 ${cfg.pipesDirectory}
      '';
    };
};
in
{
  #### Interface:
  options.services.personal-webhooks = {
    enable = mkEnableOption "Personal Webhooks";

    package = mkOption {
      type = types.package;
      default = package;
      description = "The personal-webhooks package to use.";
    };

    port = mkOption {
      type = types.int;
      default = 8082;
      description = "Port number for the webhooks server.";
    };

    user = mkOption {
      type = types.str;
      default = "webhooks";
      description = "Webhooks server user.";
    };

    group = mkOption {
      type = types.str;
      default = "webhooks";
      description = "Webhooks server group.";
    };

    pipesDirectory = mkOption {
      type = types.path;
      default = "/run/webhooks";
      description = "Where FIFO pipes will live.";
    };

    configFile = mkOption {
      type = types.path;
      default = "${../examples/config.yml}";
      description = "The configuration file to use.";
    };

    hooks = mkOption {
      type = types.listOf (types.submodule hookOptions);
      default = [ ];
      description = "Hooks to configure.";
    };
  };

  #### Implementation:
  config = mkIf cfg.enable {
    environment.systemPackages = [ cfg.package ];

    users.extraUsers = optional (cfg.user == "webhooks")
      { name = cfg.user;
        description = "Webhooks user";
        group = cfg.group;
        isSystemUser = true;
      };

    users.extraGroups = optional (cfg.group == "webhooks")
      { name = cfg.group;
      };

    systemd.services =
      foldl' (x: y: x // {"webhook-${y.name}" = hookToService y;})
             mainService cfg.hooks;
  };
}
