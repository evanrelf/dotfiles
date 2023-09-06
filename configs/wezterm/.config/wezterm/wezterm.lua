local wezterm = require("wezterm")

local font = wezterm.font_with_fallback({
  { family = "PragmataPro" },
  { family = "Iosevka SS08", harfbuzz_features = { "calt=0" } },
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

local color_scheme = (function()
  if wezterm.gui.get_appearance():find("Light") then
    return "primer-light"
  else
    return "primer-dark"
  end
end)()

local colors = (function()
  if wezterm.gui.get_appearance():find("Light") then
    return {
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
        inactive_tab_hover = {
          fg_color = "#959da5",
          bg_color = "#ffffff",
        },
        new_tab = {
          fg_color = "#ffffff",
          bg_color = "#ffffff",
        },
        new_tab_hover = {
          fg_color = "#ffffff",
          bg_color = "#ffffff",
        },
      },
    }
  else
    return {
      tab_bar = {
        background = "#010409",
        active_tab = {
          fg_color = "#ffffff",
          bg_color = "#010409",
        },
        inactive_tab = {
          fg_color = "#959da5", -- TODO
          bg_color = "#010409",
        },
        inactive_tab_hover = {
          fg_color = "#959da5", -- TODO
          bg_color = "#010409",
        },
        new_tab = {
          fg_color = "#010409",
          bg_color = "#010409",
        },
        new_tab_hover = {
          fg_color = "#010409",
          bg_color = "#010409",
        },
      },
    }
  end
end)()

return {
  font = font,
  font_size = 17.0,
  font_rules = font_rules(),
  freetype_load_flags = "NO_HINTING",
  color_scheme = color_scheme,
  colors = colors,
  inactive_pane_hsb = {
    brightness = 1.0,
    saturation = 1.0,
  },
  window_padding = {
    left = 4,
    right = 4,
    top = 4,
    bottom = 4,
  },
  window_decorations = "RESIZE",
  use_fancy_tab_bar = false,
  tab_bar_at_bottom = true,
  hide_tab_bar_if_only_one_tab = true,
  show_tab_index_in_tab_bar = false,
  audible_bell = "Disabled",
  leader = { mods = "SUPER", key = "s" },
  keys = {
    { mods = "CMD", key = "w", action = wezterm.action.CloseCurrentPane { confirm = true } },
    { mods = "LEADER", key = "-", action = wezterm.action.SplitPane { direction = "Down" } },
    { mods = "LEADER", key = "\\", action = wezterm.action.SplitPane { direction = "Right" } },
    { mods = "LEADER", key = "h", action = wezterm.action.ActivatePaneDirection "Left" },
    { mods = "LEADER", key = "j", action = wezterm.action.ActivatePaneDirection "Down" },
    { mods = "LEADER", key = "k", action = wezterm.action.ActivatePaneDirection "Up" },
    { mods = "LEADER", key = "l", action = wezterm.action.ActivatePaneDirection "Right" },
    { mods = "LEADER", key = "[", action = wezterm.action.AdjustPaneSize { "Left", 5 } },
    { mods = "LEADER", key = "]", action = wezterm.action.AdjustPaneSize { "Right", 5 } },
    { mods = "LEADER", key = "{", action = wezterm.action.AdjustPaneSize { "Down", 5 } },
    { mods = "LEADER", key = "}", action = wezterm.action.AdjustPaneSize { "Up", 5 } },
    { mods = "LEADER", key = "z", action = wezterm.action.TogglePaneZoomState },
    { mods = "LEADER", key = "<", action = wezterm.action.MoveTabRelative(-1) },
    { mods = "LEADER", key = ">", action = wezterm.action.MoveTabRelative(1) },
    { mods = "LEADER", key = ",", action = wezterm.action.PromptInputLine {
        description = "Rename tab",
        action = wezterm.action_callback(function(window, pane, line)
          if line then
            window:active_tab():set_title(line)
          end
        end),
      },
    },
    -- TODO: Focusing tabs
    -- TODO: Moving panes
  },
  enable_csi_u_key_encoding = true,
  max_fps = 120,
  check_for_updates = false,
}
