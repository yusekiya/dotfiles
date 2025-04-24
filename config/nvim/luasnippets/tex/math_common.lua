local ls = require("luasnip")
local s = ls.snippet
local i = ls.insert_node
local fmt = require("luasnip.extras.fmt").fmt
local fmta = require("luasnip.extras.fmt").fmta

local tex_utils = require("luasnip-utils.tex")

return {
  s(
    { trig = ";;", dscr = "Inline math", condition = tex_utils.in_text, snippetType = "autosnippet" },
    fmt("${}$", { i(0) })
  ),
  s(
    { trig = "dm", dscr = "Display math", condition = tex_utils.in_text, snippetType = "autosnippet" },
    fmta(
      [[
        \begin{align}
            <>
        \end{align}
      ]],
      { i(0) }
    )
  ),
}
