local ls = require("luasnip")
local s = ls.snippet
local i = ls.insert_node
local f = ls.function_node
local fmta = require("luasnip.extras.fmt").fmta

local tex_utils = require("luasnip-utils.tex")

return {
  s(
    {
      trig = "([^%a])int",
      dscr = "integral",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\int d<> \, 
      ]],
      {
        f(function(_, snip)
          return snip.captures[1]
        end),
        i(1, "x"),
      }
    )
  ),
  s(
    {
      trig = "([^%a])dint",
      dscr = "definite integral",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\int_{<>}^{<>} d<> \, 
      ]],
      {
        f(function(_, snip)
          return snip.captures[1]
        end),
        i(1, "-\\infty"),
        i(2, "\\infty"),
        i(3, "x"),
      }
    )
  ),
  s(
    {
      trig = "([^%a])oint",
      dscr = "closed loop integral",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\oint
      ]],
      {
        f(function(_, snip)
          return snip.captures[1]
        end),
      }
    )
  ),
  s(
    {
      trig = "([^%a])\\mathrm{i}nt",
      dscr = "2-fold integral",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\iint
      ]],
      {
        f(function(_, snip)
          return snip.captures[1]
        end),
      }
    )
  ),
  s(
    {
      trig = "([^%a])\\mathrm{i}int",
      dscr = "3-fold integral",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\iiint
      ]],
      {
        f(function(_, snip)
          return snip.captures[1]
        end),
      }
    )
  ),
  s(
    {
      trig = "([^%a])i%.int",
      dscr = "multiple integral",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
      priority = 1001,
    },
    fmta(
      [[
        <>\idotsint
      ]],
      {
        f(function(_, snip)
          return snip.captures[1]
        end),
      }
    )
  ),
}
