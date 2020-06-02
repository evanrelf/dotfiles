# Core
source "$HOME/.config/fish/prelude.fish"
source "$HOME/.config/fish/nix.fish"
source "$HOME/.config/fish/path.fish"

# Customization
source "$HOME/.config/fish/editor.fish"
source "$HOME/.config/fish/colors.fish"
source "$HOME/.config/fish/cursor-shape.fish"
source "$HOME/.config/fish/prompt.fish"
source "$HOME/.config/fish/aliases.fish"

# Plugins and integrations
source "$HOME/.config/fish/install-fisher.fish"
source "$HOME/.config/fish/fzf.fish"
source "$HOME/.config/fish/zoxide.fish"
source "$HOME/.config/fish/direnv.fish"

# Functions
source "$HOME/.config/fish/rc.fish"
source "$HOME/.config/fish/update.fish"
source "$HOME/.config/fish/edit-matches.fish"
source "$HOME/.config/fish/darwin.fish"

set --global fish_features "stderr-nocaret"

if test -f "$HOME/.config/fish/local.fish"
    source "$HOME/.config/fish/local.fish"
end
