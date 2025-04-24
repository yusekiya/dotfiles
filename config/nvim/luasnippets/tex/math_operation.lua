local ls = require("luasnip")
local s = ls.snippet
local sn = ls.snippet_node
local t = ls.text_node
local i = ls.insert_node
local f = ls.function_node
local d = ls.dynamic_node
local fmta = require("luasnip.extras.fmt").fmta

local ls_common = require("luasnip-utils.common")
local get_visual = ls_common.get_visual
local tex_utils = require("luasnip-utils.tex")

local generate_fraction = function(_, snip)
  local stripped = snip.captures[1]
  local depth = 0
  local j = #stripped
  while true do
    local c = stripped:sub(j, j)
    if c == "(" then
      depth = depth + 1
    elseif c == ")" then
      depth = depth - 1
    end
    if depth == 0 then
      break
    end
    j = j - 1
  end
  return sn(
    nil,
    fmta(
      [[
        <>\frac{<>}{<>}
        ]],
      { t(stripped:sub(1, j - 1)), t(stripped:sub(j + 1, -2)), i(1) }
    )
  )
end

local add_trigonometric = function(snippets)
  local fnames = { "arcsin", "arccos", "arctan", "arccot", "arccsc", "arcsec", "sin", "cos", "tan", "cot", "csc" }
  for _, fname in pairs(fnames) do
    table.insert(
      snippets,
      s(
        {
          trig = "([^%a])" .. fname .. "([A-Za-gi-z; ])",
          dscr = fname .. " function",
          regTrig = true,
          wordTrig = false,
          condition = tex_utils.in_mathzone,
          snippetType = "autosnippet",
        },
        fmta("<>\\" .. fname .. " <>", {
          f(function(_, snip)
            return snip.captures[1]
          end),
          f(function(_, snip)
            return string.gsub(snip.captures[2], "%s+", "")
          end),
        })
      )
    )
  end
end

local add_hyperbolic = function(snippets)
  local fnames =
    { "arcsinh", "arccosh", "arctanh", "arccoth", "arccsch", "arcsech", "sinh", "cosh", "tanh", "coth", "csch" }
  for _, fname in pairs(fnames) do
    table.insert(
      snippets,
      s(
        {
          trig = "([^%a])" .. fname .. "([A-Za-z; ])",
          dscr = fname .. " function",
          regTrig = true,
          wordTrig = false,
          condition = tex_utils.in_mathzone,
          snippetType = "autosnippet",
        },
        fmta("<>\\" .. fname .. " <>", {
          f(function(_, snip)
            return snip.captures[1]
          end),
          f(function(_, snip)
            return string.gsub(snip.captures[2], "%s+", "")
          end),
        })
      )
    )
  end
end

local snippets = {
  s(
    {
      trig = "([^%a])te",
      dscr = "text",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\text{<>}
      ]],
      {
        f(function(_, snip)
          return snip.captures[1]
        end),
        i(0),
      }
    )
  ),
  s(
    {
      trig = "([^%a])bf",
      dscr = "math bold",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\mathbf{<>}
      ]],
      {
        f(function(_, snip)
          return snip.captures[1]
        end),
        d(1, get_visual),
      }
    )
  ),
  s(
    {
      trig = "([^%a])bb",
      dscr = "blackboard bold",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\mathbb{<>}
      ]],
      {
        f(function(_, snip)
          return snip.captures[1]
        end),
        d(1, get_visual),
      }
    )
  ),
  s(
    {
      trig = "([^%a])rm",
      dscr = "roman style",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\mathrm{<>}
      ]],
      {
        f(function(_, snip)
          return snip.captures[1]
        end),
        d(1, get_visual),
      }
    )
  ),
  s(
    {
      trig = "([^%a])cal",
      dscr = "calligraphic style",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\mathcal{<>}
      ]],
      {
        f(function(_, snip)
          return snip.captures[1]
        end),
        d(1, get_visual),
      }
    )
  ),
  s(
    {
      trig = "[%s]*sr",
      dscr = "squared",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        ^2<>
      ]],
      { i(0) }
    )
  ),
  s(
    {
      trig = "[%s]*cb",
      dscr = "cubed",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        ^3<>
      ]],
      { i(0) }
    )
  ),
  s(
    {
      trig = "[%s]*invs",
      dscr = "inverse",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        ^{-1}<>
      ]],
      { i(0) }
    )
  ),
  s(
    {
      trig = "[%s]*sp",
      dscr = "supescript",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        ^{<>}
      ]],
      {
        d(1, get_visual),
      }
    )
  ),
  s(
    {
      trig = "[%s]*sb",
      dscr = "subescript",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        _{<>}
      ]],
      {
        d(1, get_visual),
      }
    )
  ),
  s(
    {
      trig = "(%a)no",
      dscr = "subscript nough",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>_{\mathrm{0}}<>
      ]],
      {
        f(function(_, snip)
          return snip.captures[1]
        end),
        i(0),
      }
    )
  ),
  s(
    {
      trig = "([^%a])sq",
      dscr = "square root",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\sqrt{<>}
      ]],
      {
        f(function(_, snip)
          return snip.captures[1]
        end),
        d(1, get_visual),
      }
    )
  ),
  s(
    {
      trig = "([^%a])ee",
      dscr = "exp (superscript)",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>e^{<>}
      ]],
      {
        f(function(_, snip)
          return snip.captures[1]
        end),
        d(1, get_visual),
      }
    )
  ),
  s(
    {
      trig = "([^%a])exp",
      dscr = "exp function",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\exp <>
      ]],
      {
        f(function(_, snip)
          return snip.captures[1]
        end),
        d(1, get_visual),
      }
    )
  ),
  s(
    {
      trig = "([^%a])log",
      dscr = "log function",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\log <>
      ]],
      {
        f(function(_, snip)
          return snip.captures[1]
        end),
        d(1, get_visual),
      }
    )
  ),
  s(
    {
      trig = "([^%a])ln",
      dscr = "natural log function",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\ln <>
      ]],
      {
        f(function(_, snip)
          return snip.captures[1]
        end),
        d(1, get_visual),
      }
    )
  ),
  s(
    {
      trig = "[%s]*trans",
      dscr = "transpose",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        ^\top<>
      ]],
      { i(0) }
    )
  ),
  s(
    {
      trig = "[%s]*conj",
      dscr = "conjugate",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        ^*<>
      ]],
      { i(0) }
    )
  ),
  s(
    {
      trig = "([^%a])Tr",
      dscr = "trace",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\Tr
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
      trig = "([^%a])det",
      dscr = "determinant",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\det
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
      trig = "([^%a])Re",
      dscr = "real part",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\Re
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
      trig = "([^%a])Im",
      dscr = "imaginary part",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\Im
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
      trig = "(%a),%.",
      dscr = "upright bold style",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \mathbf{<>}<>
      ]],
      {
        f(function(_, snip)
          return snip.captures[1]
        end),
        i(0),
      }
    )
  ),
  s(
    {
      trig = "(%a)%.,",
      dscr = "upright bold style (alt)",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \mathbf{<>}<>
      ]],
      {
        f(function(_, snip)
          return snip.captures[1]
        end),
        i(0),
      }
    )
  ),
  s(
    {
      trig = "(%a),s",
      dscr = "slant bold style",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \boldsymbol{<>}<>
      ]],
      {
        f(function(_, snip)
          return snip.captures[1]
        end),
        i(0),
      }
    )
  ),
  s(
    {
      trig = "(%a)(%d)",
      dscr = "sub digit",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>_<>
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
      trig = "_(%d%d)",
      dscr = "sub two digits",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        _{<>}
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
      trig = "(})(%d)",
      dscr = "add sub digit to block",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>_<>
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
      trig = "([^%a])bar",
      dscr = "bar",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\bar{<>}
      ]],
      {
        f(function(_, snip)
          return snip.captures[1]
        end),
        d(1, get_visual),
      }
    )
  ),
  s(
    {
      trig = "([^%a])([a-gi-zA-Z])bar",
      dscr = "add bar",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\bar{<>}
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
      trig = "([^%a])hat",
      dscr = "hat",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\hat{<>}
      ]],
      {
        f(function(_, snip)
          return snip.captures[1]
        end),
        d(1, get_visual),
      }
    )
  ),
  s(
    {
      trig = "([^%a])(%a)hat",
      dscr = "add hat",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\hat{<>}
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
      trig = "([^%a])dot",
      dscr = "dot",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\dot{<>}
      ]],
      {
        f(function(_, snip)
          return snip.captures[1]
        end),
        d(1, get_visual),
      }
    )
  ),
  s(
    {
      trig = "([^%a])(%a)dot",
      dscr = "add dot",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\dot{<>}
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
      trig = "([^%a])(%a)ddot",
      dscr = "add ddot",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\ddot{<>}
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
      trig = "([^%a])vec",
      dscr = "vec",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\vec{<>}
      ]],
      {
        f(function(_, snip)
          return snip.captures[1]
        end),
        d(1, get_visual),
      }
    )
  ),
  s(
    {
      trig = "([^%a])(%a)vec",
      dscr = "add vec arrow",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\vec{<>}
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
      trig = "([^%a])Vec",
      dscr = "wide vec arrow",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\overrightarrow{<>}
      ]],
      {
        f(function(_, snip)
          return snip.captures[1]
        end),
        d(1, get_visual),
      }
    )
  ),
  s(
    {
      trig = "([^%a])(%a)Vec",
      dscr = "add wide vec arrow",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\overrightarrow{<>}
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
      trig = "([^%a])tilde",
      dscr = "tilde",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\tilde{<>}
      ]],
      {
        f(function(_, snip)
          return snip.captures[1]
        end),
        d(1, get_visual),
      }
    )
  ),
  s(
    {
      trig = "([^%a])(%a)tilde",
      dscr = "add tilde",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\tilde{<>}
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
      trig = "([^%a])Tilde",
      dscr = "wide tilde",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\widetilde{<>}
      ]],
      {
        f(function(_, snip)
          return snip.captures[1]
        end),
        d(1, get_visual),
      }
    )
  ),
  s(
    {
      trig = "([^%a])(%a)Tilde",
      dscr = "add wide tilde",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\widetilde{<>}
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
      trig = "([^%a])und",
      dscr = "underline",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\underline{<>}
      ]],
      {
        f(function(_, snip)
          return snip.captures[1]
        end),
        d(1, get_visual),
      }
    )
  ),
  s(
    {
      trig = "([^%a])(%a)und",
      dscr = "add underline",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\underline{<>}
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
      trig = "([^%a])hat",
      dscr = "hat",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\hat{<>}
      ]],
      {
        f(function(_, snip)
          return snip.captures[1]
        end),
        d(1, get_visual),
      }
    )
  ),
  s(
    {
      trig = "([^%a])(%a)hat",
      dscr = "add hat",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\hat{<>}
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
      trig = "([^%a])Hat",
      dscr = "wide hat",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\widehat{<>}
      ]],
      {
        f(function(_, snip)
          return snip.captures[1]
        end),
        d(1, get_visual),
      }
    )
  ),
  s(
    {
      trig = "([^%a])(%a)Hat",
      dscr = "add wide hat",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\widehat{<>}
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
      trig = "([^%a])norm",
      dscr = "norm",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\lVert <> \rVert
      ]],
      {
        f(function(_, snip)
          return snip.captures[1]
        end),
        d(1, get_visual),
      }
    )
  ),
  s(
    {
      trig = "([^%a])abs",
      dscr = "abs",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\lvert <> \rvert 
      ]],
      {
        f(function(_, snip)
          return snip.captures[1]
        end),
        d(1, get_visual),
      }
    )
  ),
  s(
    {
      trig = "([^%a])set",
      dscr = "set",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\set{<>}
      ]],
      {
        f(function(_, snip)
          return snip.captures[1]
        end),
        d(1, get_visual),
      }
    )
  ),
  s(
    {
      trig = "([^%a])Set",
      dscr = "large set",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\Set{<>}
      ]],
      {
        f(function(_, snip)
          return snip.captures[1]
        end),
        d(1, get_visual),
      }
    )
  ),
  s(
    {
      trig = "_(%a)[%s]*%+%+",
      dscr = "increment subscript",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        _{<>+1}
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
      trig = "_(%a)[%s]*%-%-",
      dscr = "decrement subscript",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        _{<>-1}
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
      trig = "//",
      dscr = "fraction",
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \frac{<>}{<>}
      ]],
      {
        d(1, get_visual),
        i(2),
      }
    )
  ),
  s(
    {
      trig = "((\\d+)|(\\d*)(\\\\)?([A-Za-z]+)((\\^|_)(\\{\\d+\\}|\\d))*)\\/",
      dscr = "fraction without parentheses",
      regTrig = true,
      wordTrig = false,
      trigEngine = "ecma",
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \frac{<>}{<>}
      ]],
      {
        f(function(_, snip)
          return snip.captures[1]
        end),
        i(2),
      }
    )
  ),
  s(
    {
      trig = "(^.*\\))/",
      dscr = "fraction with parentheses",
      regTrig = true,
      wordTrig = false,
      trigEngine = "ecma",
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta("<>", {
      d(1, generate_fraction),
    })
  ),
}

add_trigonometric(snippets)
add_hyperbolic(snippets)

return snippets
