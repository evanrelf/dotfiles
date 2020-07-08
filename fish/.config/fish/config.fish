set --local config "$HOME/.config/fish"

# Core
source "$config/prelude.fish"
source "$config/nix.fish"
source "$config/xdg.fish"
source "$config/path.fish"

# Customization
source "$config/editor.fish"
source "$config/colors.fish"
source "$config/cursor-shape.fish"
source "$config/prompt.fish"
source "$config/aliases.fish"

# Plugins and integrations
source "$config/install-fisher.fish"
source "$config/fzf.fish"
source "$config/zoxide.fish"
source "$config/direnv.fish"

# Functions
source "$config/rc.fish"
source "$config/update.fish"
source "$config/edit-matches.fish"
source "$config/darwin.fish"

set --global fish_features "stderr-nocaret"

if test -f "$config/local.fish"
    source "$config/local.fish"
end
