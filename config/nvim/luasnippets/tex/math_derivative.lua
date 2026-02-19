local ls = require("luasnip")
local s = ls.snippet
local i = ls.insert_node
local f = ls.function_node
local fmta = require("luasnip.extras.fmt").fmta

local tex_utils = require("luasnip-utils.tex")

return {
  s(
    {
      trig = "([^%a])par",
      dscr = "partial derivative",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
    },
    fmta(
      [[
        <>\frac{\partial <>}{\partial <>}
      ]],
      {
        f(function(_, snip)
          return snip.captures[1]
        end),
        i(1),
        i(2),
      }
    )
  ),
  s(
    {
      trig = "pa2",
      dscr = "second partial derivative",
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
      priority = 1001,
    },
    fmta(
      [[
        \frac{\partial^2 <>}{\partial <>^2}
      ]],
      {
        i(1),
        i(2),
      }
    )
  ),
  s(
    {
      trig = "pa3",
      dscr = "third partial derivative",
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
      priority = 1001,
    },
    fmta(
      [[
        \frac{\partial^3 <>}{\partial <>^3}
      ]],
      {
        i(1),
        i(2),
      }
    )
  ),
  s(
    {
      trig = "([^%a])d(%a)d(%a)",
      dscr = "derivative of univariate function",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\frac{d <>}{d <>}
      ]],
      {
        f(function(_, snip)
          return snip.captures[1]
        end),
        f(function(_, snip)
          return snip.captures[2]
        end),
        f(function(_, snip)
          return snip.captures[3]
        end),
      }
    )
  ),
  s(
    {
      trig = "([^%a])d_2(%a)d(%a)",
      dscr = "second derivative of univariate function",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\frac{d^2 <>}{d <>^2}
      ]],
      {
        f(function(_, snip)
          return snip.captures[1]
        end),
        f(function(_, snip)
          return snip.captures[2]
        end),
        f(function(_, snip)
          return snip.captures[3]
        end),
      }
    )
  ),
  s(
    {
      trig = "([^%a])ddt",
      dscr = "time differential operator",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\frac{d}{dt}
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
      trig = "([^%a])ddx",
      dscr = "space differential operator",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\frac{d}{dx}
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
      trig = "([^%a])par(%a)",
      dscr = "partial derivative operator",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\partial_<>
      ]],
      {
        f(function(_, snip)
          return snip.captures[1]
        end),
        f(function(_, snip)
          return snip.captures[2]
        end),
      }
    )
  ),
  s(
    {
      trig = "([^%a])Del(%a)",
      dscr = "capital del operator",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>D_<>
      ]],
      {
        f(function(_, snip)
          return snip.captures[1]
        end),
        f(function(_, snip)
          return snip.captures[2]
        end),
      }
    )
  ),
}
