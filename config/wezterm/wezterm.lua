local function merge(...)
  local result = {}
  for _, t in ipairs({ ... }) do
    for k, v in pairs(t) do
      result[k] = v
    end
  end
  return result
end

---

local wezterm = require('wezterm')
local c = wezterm.config_builder()

c.color_scheme = 'OneDark (base16)'
c.font_size = 10

c.hide_tab_bar_if_only_one_tab = true
c.use_fancy_tab_bar = false

local tab_bg = '#21242b'
local tab_bg_active = '#282c34'
local tab_fg = '#c8ccd4'

local active_tab = {
  bg_color = tab_bg_active,
  fg_color = tab_fg
}
local inactive_tab = {
  bg_color = tab_bg,
  fg_color = tab_fg
}
local italic = { italic = true }

c.colors = {
  tab_bar = {
    background = tab_bg,
    active_tab = active_tab,
    inactive_tab = inactive_tab,
    inactive_tab_hover = merge(active_tab, italic),
    new_tab = inactive_tab,
    new_tab_hover = merge(active_tab, italic),
  },
}

c.command_palette_bg_color = tab_bg
c.command_palette_font = wezterm.font('JetBrains Mono')
c.command_palette_font_size = 10

return c
