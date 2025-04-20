local ls = require("luasnip")
local s = ls.snippet
local i = ls.insert_node
local fmta = require("luasnip.extras.fmt").fmta

local tex_utils = require("luasnip-utils.tex")

return {
  s(
    {
      trig = "pmat",
      dscr = "parentheses matrix",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \begin{pmatrix}
            <>
        \end{pmatrix}
      ]],
      {
        i(1),
      }
    )
  ),
  s(
    {
      trig = "bmat",
      dscr = "bracket matrix",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \begin{bmatrix}
            <>
        \end{bmatrix}
      ]],
      {
        i(1),
      }
    )
  ),
  s(
    {
      trig = "Bmat",
      dscr = "brace matrix",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \begin{Bmatrix}
            <>
        \end{Bmatrix}
      ]],
      {
        i(1),
      }
    )
  ),
  s(
    {
      trig = "vmat",
      dscr = "vertical line matrix",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \begin{vmatrix}
            <>
        \end{vmatrix}
      ]],
      {
        i(1),
      }
    )
  ),
  s(
    {
      trig = "Vmat",
      dscr = "double vertical line matrix",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \begin{Vmatrix}
            <>
        \end{Vmatrix}
      ]],
      {
        i(1),
      }
    )
  ),
  s(
    {
      trig = "mat",
      dscr = "general matrix",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
      priority = 900,
    },
    fmta(
      [[
        \begin{matrix}
            <>
        \end{matrix}
      ]],
      {
        i(1),
      }
    )
  ),
  s(
    {
      trig = "case",
      dscr = "cases",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \begin{cases}
            <>
        \end{cases}
      ]],
      {
        i(1),
      }
    )
  ),
  s(
    {
      trig = "arr",
      dscr = "array",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \begin{array}{|<>|}
            <>
        \end{array}
      ]],
      {
        i(1, "c"),
        i(2),
      }
    )
  ),
}
