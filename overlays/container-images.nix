final: prev:

# Building on macOS, assuming you have the `nix-darwin` Linux builder set up:
#
# ```
# $ nix build .#containerImages.<name> --system aarch64-linux --max-jobs 0
# ```

let
  guestSystem =
    final.lib.replaceString
      "-darwin"
      "-linux"
      final.stdenv.hostPlatform.system;

  pkgsLinux =
    import final.inputs.nixpkgs {
      system = guestSystem;
      config = { allowUnfree = true; };
      overlays = [ final.inputs.self.overlays.default ];
    };

  claude-code-yolo =
    pkgsLinux.symlinkJoin {
      name = "claude-code-yolo";
      paths = [ pkgsLinux.llm-agents.claude-code ];
      buildInputs = [ pkgsLinux.makeWrapper ];
      postBuild = ''
        wrapProgram $out/bin/claude \
          --add-flags "--allow-dangerously-skip-permissions"
      '';
    };

in
{
  containerImages = {
    debian =
      final.dockerTools.pullImage ({
        imageName = "docker.io/debian";
        finalImageTag = "trixie";
        os = "linux";
      } // {
        "aarch64-linux" = {
          imageDigest = "sha256:2c91e484d93f0830a7e05a2b9d92a7b102be7cab562198b984a84fdbc7806d91";
          hash = "sha256-G0n+yNa0TmJ0dvhOIyck9gaZJRDqCJsPBTGhn761p40=";
          arch = "arm64";
        };
        "x86_64-linux" = {
          imageDigest = "sha256:2c91e484d93f0830a7e05a2b9d92a7b102be7cab562198b984a84fdbc7806d91";
          hash = "sha256-JNPUag2l/prrT//uzlNpHe0f1Faf5c4UHVM2eHLmneU=";
          arch = "amd64";
        };
      }.${guestSystem});

    # TODO: Figure out how to pass host's Claude Code credentials to container.
    # They're stored in Keychain on macOS.
    #
    # `docker container run --rm --interactive --tty --mount type=bind,source=$HOME/.claude,target=/home/somebody/.claude agents:latest`
    agents =
      # https://nixos.org/manual/nixpkgs/stable/#ssec-pkgs-dockerTools-buildLayeredImage
      final.dockerTools.buildLayeredImage {
        name = "agents";
        tag = "latest";
        fromImage = final.containerImages.debian;
        contents = with pkgsLinux; [
          # Agents
          claude-code-yolo
          llm-agents.amp

          # Tools
          bashInteractive
          coreutils
          curl
          fd
          file
          findutils
          gawkInteractive
          git
          gnugrep
          gnused
          jq
          jujutsu
          kakoune
          less
          nodejs
          python3
          ripgrep
          sd
          shellcheck
          wget
        ];
        # https://github.com/opencontainers/image-spec/blob/v1.1.1/config.md#properties
        config = {
          User = "somebody";
          WorkingDir = [ "/home/somebody" ];
          Cmd = [ "/bin/bash" ];
        };
        fakeRootCommands = ''
          ${pkgsLinux.dockerTools.shadowSetup}
          useradd somebody --create-home
        '';
        enableFakechroot = true;
      };
  };
}
