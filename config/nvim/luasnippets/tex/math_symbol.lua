local ls = require("luasnip")
local s = ls.snippet
local i = ls.insert_node
local f = ls.function_node
local fmta = require("luasnip.extras.fmt").fmta

local tex_utils = require("luasnip-utils.tex")

return {
  s(
    {
      trig = "ooo",
      dscr = "infinity",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \infty
      ]],
      {}
    )
  ),
  s(
    {
      trig = "tfr",
      dscr = "therefore",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \therefore
      ]],
      {}
    )
  ),
  s(
    {
      trig = "bcs",
      dscr = "because",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \because
      ]],
      {}
    )
  ),
  s(
    {
      trig = "OO",
      dscr = "emptyset",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \emptyset
      ]],
      {}
    )
  ),
  s(
    {
      trig = "sum",
      dscr = "summation",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \sum
      ]],
      {}
    )
  ),
  s(
    {
      trig = "prod",
      dscr = "production",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \prod
      ]],
      {}
    )
  ),
  s(
    {
      trig = "lim",
      dscr = "limit",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \lim_{<> \to <>}
      ]],
      {
        i(1),
        i(2),
      }
    )
  ),
  s(
    {
      trig = "%.%.%.",
      dscr = "dots",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \dots
      ]],
      {}
    )
  ),
  s(
    {
      trig = "%.%.c",
      dscr = "ellipsis for commas",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \dotsc
      ]],
      {}
    )
  ),
  s(
    {
      trig = "%.%.m",
      dscr = "ellipsis for multiplications",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \dotsm
      ]],
      {}
    )
  ),
  s(
    {
      trig = "%.%.b",
      dscr = "ellipsis for binary operations",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \dotsb
      ]],
      {}
    )
  ),
  s(
    {
      trig = "%.%.i",
      dscr = "ellipsis for integrals",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \dotsi
      ]],
      {}
    )
  ),
  s(
    {
      trig = "%.%.o",
      dscr = "other ellipsis",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \dotso
      ]],
      {}
    )
  ),
  s(
    {
      trig = "c%.%.",
      dscr = "centered dots",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \cdots
      ]],
      {}
    )
  ),
  s(
    {
      trig = "v%.%.",
      dscr = "vertical dots",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \vdots
      ]],
      {}
    )
  ),
  s(
    {
      trig = "d%.%.",
      dscr = "diagonal dots",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \ddots
      ]],
      {}
    )
  ),
  s(
    {
      trig = "<%->",
      dscr = "left right arrow",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \leftrightarrow
      ]],
      {}
    )
  ),
  s(
    {
      trig = "<%-%->",
      dscr = "long left right arrow",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \longleftrightarrow
      ]],
      {}
    )
  ),
  s(
    {
      trig = "<=>",
      dscr = "thick left right arrow",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \Leftrightarrow
      ]],
      {}
    )
  ),
  s(
    {
      trig = "<==>",
      dscr = "long thick left right arrow",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \Longleftrightarrow
      ]],
      {}
    )
  ),
  s(
    {
      trig = "%->",
      dscr = "to",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \to
      ]],
      {}
    )
  ),
  s(
    {
      trig = "to",
      dscr = "to",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \to
      ]],
      {}
    )
  ),
  s(
    {
      trig = "([^%a])<%-([^->])",
      dscr = "get",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\gets <>
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
      trig = "|>",
      dscr = "maps to",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \mapsto
      ]],
      {}
    )
  ),
  s(
    {
      trig = "\\\\\\",
      dscr = "set minus",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \setminus
      ]],
      {}
    )
  ),
  s(
    {
      trig = "||",
      dscr = "set condition",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \mid
      ]],
      {}
    )
  ),
  s(
    {
      trig = "\\mid|",
      dscr = "parallel",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \parallel
      ]],
      {}
    )
  ),
  s(
    {
      trig = "and",
      dscr = "logical and",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \cap
      ]],
      {}
    )
  ),
  s(
    {
      trig = "orr",
      dscr = "logical or",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \cup
      ]],
      {}
    )
  ),
  s(
    {
      trig = "wed",
      dscr = "wedge symbol",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \wedge
      ]],
      {}
    )
  ),
  s(
    {
      trig = "Wed",
      dscr = "wedge operator",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \bigwedge
      ]],
      {}
    )
  ),
  s(
    {
      trig = "vel",
      dscr = "descending wedge symbol",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \vee
      ]],
      {}
    )
  ),
  s(
    {
      trig = "Vel",
      dscr = "descending wedge operator",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \bigvee
      ]],
      {}
    )
  ),
  s(
    {
      trig = "cap",
      dscr = "cap",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \cap
      ]],
      {}
    )
  ),
  s(
    {
      trig = "Cap",
      dscr = "cap operator",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \bigcap
      ]],
      {}
    )
  ),
  s(
    {
      trig = "cup",
      dscr = "cup",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \cup
      ]],
      {}
    )
  ),
  s(
    {
      trig = "Cup",
      dscr = "cup operator",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \bigcup
      ]],
      {}
    )
  ),
  s(
    {
      trig = "inn",
      dscr = "set in",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \in
      ]],
      {}
    )
  ),
  s(
    {
      trig = "subset",
      dscr = "subset",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \subset
      ]],
      {}
    )
  ),
  s(
    {
      trig = "\\subset eq",
      dscr = "subset or equal",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \subseteq
      ]],
      {}
    )
  ),
  s(
    {
      trig = "=>",
      dscr = "implies",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \implies
      ]],
      {}
    )
  ),
  s(
    {
      trig = "=<",
      dscr = "implied by",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \impliedby
      ]],
      {}
    )
  ),
  s(
    {
      trig = "iff",
      dscr = "if and onl if",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \iff
      ]],
      {}
    )
  ),
  s(
    {
      trig = "exists",
      dscr = "exists",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \exists
      ]],
      {}
    )
  ),
  s(
    {
      trig = "forall",
      dscr = "for all",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \forall
      ]],
      {}
    )
  ),
  s(
    {
      trig = "s%.t%.",
      dscr = "such that or subject to",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \text{s.t.}
      ]],
      {}
    )
  ),
  s(
    {
      trig = "===",
      dscr = "equivalent to",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \equiv
      ]],
      {}
    )
  ),
  s(
    {
      trig = ":=",
      dscr = "defined as",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \coloneq
      ]],
      {}
    )
  ),
  s(
    {
      trig = "=:",
      dscr = "defines",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \eqcolon
      ]],
      {}
    )
  ),
  s(
    {
      trig = "qed",
      dscr = "end of proof",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \square
      ]],
      {}
    )
  ),
  s(
    {
      trig = "!=",
      dscr = "not equal",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \neq
      ]],
      {}
    )
  ),
  s(
    {
      trig = "([^%a])>=(.)",
      dscr = "greater than or equal to",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\geq <>
      ]],
      {
        f(function(_, snip)
          return snip.captures[1]
        end),
        f(function(_, snip)
          return string.gsub(snip.captures[2], "[ \t]+%f[\r\n%z]", "")
        end),
      }
    )
  ),
  s(
    {
      trig = "([^%a])<=([^=])",
      dscr = "less than or equal to",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\leq <>
      ]],
      {
        f(function(_, snip)
          return snip.captures[1]
        end),
        f(function(_, snip)
          return string.gsub(snip.captures[2], "[ \t]+%f[\r\n%z]", "")
        end),
      }
    )
  ),
  s(
    {
      trig = "([^%a])>~(.)",
      dscr = "greater than or similar to",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\gtrsim <>
      ]],
      {
        f(function(_, snip)
          return snip.captures[1]
        end),
        f(function(_, snip)
          return string.gsub(snip.captures[2], "[ \t]+%f[\r\n%z]", "")
        end),
      }
    )
  ),
  s(
    {
      trig = "([^%a])<~(.)",
      dscr = "less than or similar to",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\lesssim <>
      ]],
      {
        f(function(_, snip)
          return snip.captures[1]
        end),
        f(function(_, snip)
          return string.gsub(snip.captures[2], "[ \t]+%f[\r\n%z]", "")
        end),
      }
    )
  ),
  s(
    {
      trig = ">>",
      dscr = "much greater than",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \gg
      ]],
      {}
    )
  ),
  s(
    {
      trig = "<<",
      dscr = "much less than",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \ll
      ]],
      {}
    )
  ),
  s(
    {
      trig = "~~",
      dscr = "similar to",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \sim
      ]],
      {}
    )
  ),
  s(
    {
      trig = "~=",
      dscr = "similarly equal to",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \simeq
      ]],
      {}
    )
  ),
  s(
    {
      trig = "\\sim ~",
      dscr = "approximated as",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \approx
      ]],
      {}
    )
  ),
  s(
    {
      trig = "prop",
      dscr = "proportional to",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \propto
      ]],
      {}
    )
  ),
  s(
    {
      trig = "par ",
      dscr = "partial",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \partial 
      ]],
      {}
    )
  ),
  s(
    {
      trig = "nabl",
      dscr = "nabla",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \nabla
      ]],
      {}
    )
  ),
  s(
    {
      trig = "del",
      dscr = "nabla",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \nabla
      ]],
      {}
    )
  ),
  s(
    {
      trig = "div",
      dscr = "divergence",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \div
      ]],
      {}
    )
  ),
  s(
    {
      trig = "rot",
      dscr = "rotation",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \rot
      ]],
      {}
    )
  ),
  s(
    {
      trig = "grad",
      dscr = "gradient",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \grad
      ]],
      {}
    )
  ),
  s(
    {
      trig = "xx",
      dscr = "times",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \times
      ]],
      {}
    )
  ),
  s(
    {
      trig = "%*%*",
      dscr = "cdot",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \cdot
      ]],
      {}
    )
  ),
  s(
    {
      trig = "xnn",
      dscr = "x sub n",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        x_n
      ]],
      {}
    )
  ),
  s(
    {
      trig = "\\xii",
      dscr = "x sub i",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
      priority = 1001,
    },
    fmta(
      [[
        x_i
      ]],
      {}
    )
  ),
  s(
    {
      trig = "xjj",
      dscr = "x sub j",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        x_j
      ]],
      {}
    )
  ),
  s(
    {
      trig = "ynn",
      dscr = "y sub n",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        y_n
      ]],
      {}
    )
  ),
  s(
    {
      trig = "yii",
      dscr = "y sub i",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        y_i
      ]],
      {}
    )
  ),
  s(
    {
      trig = "yjj",
      dscr = "y sub j",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        y_j
      ]],
      {}
    )
  ),
  s(
    {
      trig = "ell",
      dscr = "ell",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \ell
      ]],
      {}
    )
  ),
  s(
    {
      trig = "lll",
      dscr = "ell",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \ell
      ]],
      {}
    )
  ),
  s(
    {
      trig = "LL",
      dscr = "calligraphic L",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \mathcal{L}
      ]],
      {}
    )
  ),
  s(
    {
      trig = "HH",
      dscr = "calligraphic H",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \mathcal{H}
      ]],
      {}
    )
  ),
  s(
    {
      trig = "RR",
      dscr = "real field",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \mathbb{R}
      ]],
      {}
    )
  ),
  s(
    {
      trig = "CC",
      dscr = "complex field",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \mathbb{C}
      ]],
      {}
    )
  ),
  s(
    {
      trig = "ZZ",
      dscr = "integer group",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \mathbb{Z}
      ]],
      {}
    )
  ),
  s(
    {
      trig = "NN",
      dscr = "natural number group",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \mathbb{N}
      ]],
      {}
    )
  ),
  s(
    {
      trig = "II",
      dscr = "unit",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \mathbb{1}
      ]],
      {}
    )
  ),
  s(
    {
      trig = "\\mathbb{1}I",
      dscr = "unit (alt)",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \hat{\mathbb{1}}
      ]],
      {}
    )
  ),
  s(
    {
      trig = "AA",
      dscr = "calligraphic A",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \mathcal{A}
      ]],
      {}
    )
  ),
  s(
    {
      trig = "BB",
      dscr = "magnetic field",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \bm{B}
      ]],
      {}
    )
  ),
  s(
    {
      trig = "EE",
      dscr = "electric field",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \bm{E}
      ]],
      {}
    )
  ),
  s(
    {
      trig = "ii",
      dscr = "imaginary unit",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \mathrm{i}
      ]],
      {}
    )
  ),
}
