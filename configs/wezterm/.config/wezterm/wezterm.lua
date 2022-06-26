local wezterm = require("wezterm")

local font = wezterm.font_with_fallback({
  "PragmataPro Liga",
  "Iosevka SS08",
  "Iosevka Nerd Font",
})

local font_rules = function()
  local rules = {}
  local italics = { true, false }
  local intensities = { "Bold", "Normal", "Half" }
  for _, italic in ipairs(italics) do
    for _, intensity in ipairs(intensities) do
      table.insert(rules, {
        italic = italic,
        intensity = intensity,
        font = font,
      })
    end
  end
  return rules
end

local key = function(mods, key, action)
  return {
    mods = mods,
    key = key,
    action = wezterm.action(action),
  }
end

return {
  font = font,
  font_size = 17.0,
  font_rules = font_rules(),
  color_scheme = "primer-light",
  freetype_load_flags = "NO_HINTING",
  colors = {
    tab_bar = {
      background = "#ffffff",
      active_tab = {
        fg_color = "#000000",
        bg_color = "#ffffff",
      },
      inactive_tab = {
        fg_color = "#959da5",
        bg_color = "#ffffff",
      },
      inactive_tab_edge = "#ffffff",
      new_tab = {
        fg_color = "#ffffff",
        bg_color = "#ffffff",
      },
      new_tab_hover = {
        fg_color = "#ffffff",
        bg_color = "#ffffff",
      },
    },
  },
  inactive_pane_hsb = {
    brightness = 1.0,
    saturation = 1.0,
  },
  window_frame = {
    font = wezterm.font("PragmataPro Liga"),
    active_titlebar_bg = "#ffffff",
    inactive_titlebar_bg = "#ffffff",
  },
  window_padding = {
    left = 4,
    right = 4,
    top = 4,
    bottom = 4,
  },
  use_fancy_tab_bar = false,
  tab_bar_at_bottom = true,
  hide_tab_bar_if_only_one_tab = true,
  show_tab_index_in_tab_bar = false,
  audible_bell = "Disabled",
  leader = { mods = "SUPER", key = "s" },
  keys = {
    key("LEADER", "-", { SplitVertical = {} }),
    key("LEADER", "\\", { SplitHorizontal = {} }),
    key("LEADER", "h", { ActivatePaneDirection = "Left" }),
    key("LEADER", "j", { ActivatePaneDirection = "Down" }),
    key("LEADER", "k", { ActivatePaneDirection = "Up" }),
    key("LEADER", "l", { ActivatePaneDirection = "Right" }),
    key("LEADER", "[", { AdjustPaneSize = { "Left", 5 } }),
    key("LEADER", "]", { AdjustPaneSize = { "Right", 5 } }),
    key("LEADER", "{", { AdjustPaneSize = { "Down", 5 } }),
    key("LEADER", "}", { AdjustPaneSize = { "Up", 5 } }),
    key("LEADER", "<", { MoveTabRelative = -1 }),
    key("LEADER", ">", { MoveTabRelative = 1 }),
    -- TODO: Focusing tabs
    -- TODO: Renaming tabs
    -- TODO: Moving panes
  },
  enable_csi_u_key_encoding = true,
}
