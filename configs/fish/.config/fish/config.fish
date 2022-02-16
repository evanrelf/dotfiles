# Core
source "$__fish_config_dir/prelude.fish"
source "$__fish_config_dir/nix.fish"
source "$__fish_config_dir/xdg.fish"
source "$__fish_config_dir/path.fish"

# Customization
source "$__fish_config_dir/aliases.fish"
source "$__fish_config_dir/editor.fish"
source "$__fish_config_dir/colors.fish"
source "$__fish_config_dir/cursor-shape.fish"
source "$__fish_config_dir/prompt.fish"

# Plugins and integrations
source "$__fish_config_dir/fzf.fish"
source "$__fish_config_dir/zoxide.fish"
source "$__fish_config_dir/direnv.fish"
source "$__fish_config_dir/brew.fish"

# Functions
source "$__fish_config_dir/update.fish"
source "$__fish_config_dir/edit-matches.fish"
source "$__fish_config_dir/darwin.fish"

set --global fish_features stderr-nocaret

if test -f "$__fish_config_dir/local.fish"
    source "$__fish_config_dir/local.fish"
end
