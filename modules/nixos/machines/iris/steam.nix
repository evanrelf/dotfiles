{ ... }:

{
  hardware.graphics.enable = true;
  hardware.graphics.enable32Bit = true;

  security.rtkit.enable = true;

  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true;
    gamescopeSession = {
      enable = true;
      args = [
        "--backend" "headless"
        "--output-width" "1280"
        "--output-height" "800"
        "--nested-refresh" "60"
        "--prefer-vk-device" "1002:73ff" # AMD Radeon RX 6600 XT
        "--xwayland-count" "2"
      ];
      steamArgs = [
        "-pipewire-dmabuf"
        "-gamepadui"
        "-steamdeck"
        "-steamos3"
      ];
    };
  };

  services.greetd = {
    enable = true;
    settings = rec {
      initial_session = {
        user = "evanrelf";
        command = "steam-gamescope > /dev/null 2>&1";
      };
      # Relaunch Steam if it exits
      default_session = initial_session;
    };
  };
}
