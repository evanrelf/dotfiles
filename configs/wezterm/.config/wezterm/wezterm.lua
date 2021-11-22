local wezterm = require "wezterm"

local font = wezterm.font_with_fallback({
  "PragmataPro Liga",
  "Iosevka",
  "Monaco",
})

return {
  -- Fonts
  font = font,
  font_rules = {
    { italic = true, font = font },
    { intensity = "Normal", font = font },
    { intensity = "Half", font = font },
    { intensity = "Bold", font = font },
  },
  font_size = 17.0,
  -- TODO: These don't work? Fonts don't seem to smooth out no matter what I set
  -- these to.
  -- font_antialias = "Subpixel",
  -- font_hinting = "Full",

  -- Colors
  bold_brightens_ansi_colors = false,
  color_scheme = "primer",
  color_schemes = {
    ["primer"] = {
      foreground = "#000000",
      background = "#ffffff",
      -- TODO: See if these can be ported from kitty
      -- selection_background #fff5b1
      -- active_border_color #d1d5da
      -- inactive_border_color #d1d5da
      ansi = {
        "#24292e",
        "#cb2431",
        "#28a745",
        "#b08800",
        "#0366d6",
        "#6f42c1",
        "#ea4aaa",
        "#959da5",
      },
      brights = {
        "#444d56",
        "#ea4a5a",
        "#85e89d",
        "#f9c513",
        "#79b8ff",
        "#b392f0",
        "#f692ce",
        "#d1d5da",
      },
      -- TODO: This doesn't work? It still uses default tab_bar colors.
      tab_bar = {
        background = "#ffffff",
        active_tab = {
          fg_color = "#000000",
          bg_color = "#ffffff",
        },
        inactive_tab = {
          fg_color = "#f692ce",
          bg_color = "#ffffff",
        },
      },
    },
  },

  -- Window padding
  window_padding = {
    top = 5,
    bottom = 5,
    left = 5,
    right = 5,
  },

  -- Tab bar
  hide_tab_bar_if_only_one_tab = true,
}

