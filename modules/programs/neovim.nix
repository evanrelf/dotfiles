{ lib, pkgs, ... }:

{
  home.packages = [ pkgs.neovim ];

  xdg.configFile."nvim" = {
    source = ../../configs/neovim/.config/nvim;
    recursive = true;
  };

  home.activation.installVimPluginManager =
    lib.hm.dag.entryAfter [ "writeBarrier" ] ''
      install_vim_plug() {
        if [ ! -f "$HOME/.local/share/nvim/site/autoload/plug.vim" ]; then
          echo "Installing vim-plug"
          $DRY_RUN_CMD curl --location --fail --create-dirs \
            "https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim" \
            -o "$HOME/.local/share/nvim/site/autoload/plug.vim"
        fi
      }

      install_packer() {
        if [ ! -d "$HOME/.local/share/nvim/site/pack/packer/opt/packer.nvim" ]; then
          echo "Installing packer"
          $DRY_RUN_CMD git clone "https://github.com/wbthomason/packer.nvim" \
            "$HOME/.local/share/nvim/site/pack/packer/opt/packer.nvim"
        fi
      }

      install_paq() {
        if [ ! -d "$HOME/.local/share/nvim/site/pack/paqs/start/paq-nvim" ]; then
          echo "Installing paq"
          $DRY_RUN_CMD git clone --depth=1 "https://github.com/savq/paq-nvim.git" \
            "$HOME/.local/share/nvim/site/pack/paqs/start/paq-nvim"
        fi
      }

      install_vim_plug
    '';
}
