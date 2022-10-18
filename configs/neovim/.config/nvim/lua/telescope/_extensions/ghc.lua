local telescope = require("telescope")
local action_state = require("telescope.actions.state")
local actions = require("telescope.actions")
local config = require("telescope.config").values
local finders = require("telescope.finders")
local pickers = require("telescope.pickers")

local line_picker = function(title, command)
  return function(options)
    options = options or {}

    local handle = io.popen(command)
    local result = handle:read("*a")
    handle:close()

    local choices = {}
    for token in string.gmatch(result, "[^%s]+") do
      table.insert(choices, token)
    end

    pickers.new(options, {
      prompt_title = title,
      finder = finders.new_table({ results = choices }),
      sorter = config.generic_sorter(options),
      attach_mappings = function(prompt_bufnr, map)
        actions.select_default:replace(function()
          actions.close(prompt_bufnr)
          local selection = action_state.get_selected_entry()
          vim.api.nvim_put({ selection[1] }, "", false, true)
        end)
        return true
      end,
    }):find()
  end
end

local language_extensions = line_picker(
  "GHC Language Extensions",
  [[ghc --supported-extensions | grep --invert-match --extended-regexp '(GeneralisedNewtypeDeriving|Rank2Types|AutoDeriveTypeable|TypeInType|NullaryTypeClasses)']]
)

local options = line_picker(
  "GHC Options",
  [[ghc --show-options | grep --invert-match --extended-regexp '(^-X|-Wwarn=|-Werror=|-Wno-error=)']]
)

local packages = line_picker(
  "GHC Packages",
  [[ghc-pkg field '*' name | cut -d ' ' -f 2 | sort -u]]
)

local modules = line_picker(
  "GHC Package Modules",
  [[ghc-pkg field '*' exposed-modules | tr -d ',' | tr ' ' '\n' | grep --only-matching --extended-regexp '\b[A-Z]\w+\b(\.\w+)*\b' | sort -u]]
)

return telescope.register_extension({
  exports = {
    language_extensions = language_extensions,
    options = options,
    packages = packages,
    modules = modules,
  },
})
