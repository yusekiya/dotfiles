local ls = require("luasnip")
local s = ls.snippet
local i = ls.insert_node
local d = ls.dynamic_node
local fmt = require("luasnip.extras.fmt").fmt
local fmta = require("luasnip.extras.fmt").fmta

local ls_common = require("luasnip-utils.common")
local get_visual = ls_common.get_visual
local tex_utils = require("luasnip-utils.tex")

return {
  s(
    {
      trig = "avg",
      dscr = "average",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \langle <> \rangle
      ]],
      {
        d(1, get_visual),
      }
    )
  ),
  s(
    {
      trig = "mod",
      dscr = "mod",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        | <> |
      ]],
      {
        d(1, get_visual),
      }
    )
  ),
  s(
    {
      trig = "lrp",
      dscr = "large parentheses",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \left( <> \right)
      ]],
      {
        d(1, get_visual),
      }
    )
  ),
  s(
    {
      trig = "lrc",
      dscr = "large curly braces",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \left\{ <> \right\}
      ]],
      {
        d(1, get_visual),
      }
    )
  ),
  s(
    {
      trig = "lrb",
      dscr = "large brackets",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \left[ <> \right]
      ]],
      {
        d(1, get_visual),
      }
    )
  ),
  s(
    {
      trig = "lr<",
      dscr = "large angle brackets",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmt(
      [[
        \left< {} \right>
      ]],
      {
        d(1, get_visual),
      }
    )
  ),
  s(
    {
      trig = "lr|",
      dscr = "large bars",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \left| <> \right|
      ]],
      {
        d(1, get_visual),
      }
    )
  ),
}
