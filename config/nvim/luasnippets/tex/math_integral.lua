local ls = require("luasnip")
local s = ls.snippet
local i = ls.insert_node
local fmta = require("luasnip.extras.fmt").fmta

local tex_utils = require("luasnip-utils.tex")

return {
  s(
    {
      trig = "int",
      dscr = "integral",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \int d<> \, 
      ]],
      {
        i(1, "x"),
      }
    )
  ),
  s(
    {
      trig = "dint",
      dscr = "definite integral",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \int_{<>}^{<>} d<> \, 
      ]],
      {
        i(1, "-\\infty"),
        i(2, "\\infty"),
        i(3, "x"),
      }
    )
  ),
  s(
    {
      trig = "oint",
      dscr = "closed loop integral",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \oint
      ]],
      {}
    )
  ),
  s(
    {
      trig = "\\mathrm{i}nt",
      dscr = "2-fold integral",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \iint
      ]],
      {}
    )
  ),
  s(
    {
      trig = "\\mathrm{i}int",
      dscr = "3-fold integral",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \iiint
      ]],
      {}
    )
  ),
  s(
    {
      trig = "i%.int",
      dscr = "multiple integral",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
      priority = 1001,
    },
    fmta(
      [[
        \idotsint
      ]],
      {}
    )
  ),
}
