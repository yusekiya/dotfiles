local ls = require("luasnip")
local s = ls.snippet
local f = ls.function_node
local fmta = require("luasnip.extras.fmt").fmta

local tex_utils = require("luasnip-utils.tex")

return {
  s(
    {
      trig = ":i",
      dscr = "i-th unit vector",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \mathbf{i}
      ]],
      {}
    )
  ),
  s(
    {
      trig = ":j",
      dscr = "j-th unit vector",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \mathbf{j}
      ]],
      {}
    )
  ),
  s(
    {
      trig = ":k",
      dscr = "k-th unit vector",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \mathbf{k}
      ]],
      {}
    )
  ),
  s(
    {
      trig = ":x",
      dscr = "x unit",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \hat{\mathbf{x}}
      ]],
      {}
    )
  ),
  s(
    {
      trig = ":y",
      dscr = "y unit",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \hat{\mathbf{y}}
      ]],
      {}
    )
  ),
  s(
    {
      trig = ":z",
      dscr = "z unit",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \hat{\mathbf{z}}
      ]],
      {}
    )
  ),
  s(
    {
      trig = "([^%a]):e(%w)",
      dscr = "e unit",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
      priority = 1001, -- giving priority to this snippet over sub digit snippet
    },
    fmta(
      [[
        <>\mathbf{e}_<> 
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
