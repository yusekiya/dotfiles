local ls = require("luasnip")
local s = ls.snippet
local i = ls.insert_node
local fmta = require("luasnip.extras.fmt").fmta

local tex_utils = require("luasnip-utils.tex")

return {
  s(
    {
      trig = "dm",
      dscr = "Display math",
      condition = tex_utils.in_text,
      snippetType = "autosnippet",
      priority = 1001,
    },
    fmta(
      [[
        $$
        \begin{align}
            <>
        \end{align}
        $$
      ]],
      { i(0) }
    )
  ),
}
