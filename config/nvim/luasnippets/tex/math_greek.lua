local ls = require("luasnip")
local s = ls.snippet
local f = ls.function_node
local fmta = require("luasnip.extras.fmt").fmta

local tex_utils = require("luasnip-utils.tex")

return {
  s(
    {
      trig = ";a",
      dscr = "α",
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \alpha
      ]],
      {}
    )
  ),
  s(
    {
      trig = ";b",
      dscr = "β",
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \beta
      ]],
      {}
    )
  ),
  s(
    {
      trig = ";g",
      dscr = "γ",
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \gamma
      ]],
      {}
    )
  ),
  s(
    {
      trig = ";G",
      dscr = "Γ",
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \Gamma
      ]],
      {}
    )
  ),
  s(
    {
      trig = ";d",
      dscr = "δ",
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \delta
      ]],
      {}
    )
  ),
  s(
    {
      trig = ";D",
      dscr = "Δ",
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \Delta
      ]],
      {}
    )
  ),
  s(
    {
      trig = ";e",
      dscr = "ε",
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \epsilon
      ]],
      {}
    )
  ),
  s(
    {
      trig = ";ve",
      dscr = "variant of ε",
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \varepsilon
      ]],
      {}
    )
  ),
  s(
    {
      trig = ";z",
      dscr = "ζ",
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \zeta
      ]],
      {}
    )
  ),
  s(
    {
      trig = "eta",
      dscr = "η",
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \eta
      ]],
      {}
    )
  ),
  s(
    {
      trig = ";t",
      dscr = "θ",
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \theta
      ]],
      {}
    )
  ),
  s(
    {
      trig = ";vt",
      dscr = "variant of θ",
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \vartheta
      ]],
      {}
    )
  ),
  s(
    {
      trig = ";T",
      dscr = "Θ",
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \Theta
      ]],
      {}
    )
  ),
  s(
    {
      trig = ";i",
      dscr = "ι",
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \iota
      ]],
      {}
    )
  ),
  s(
    {
      trig = ";k",
      dscr = "κ",
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \kappa
      ]],
      {}
    )
  ),
  s(
    {
      trig = ";l",
      dscr = "λ",
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \lambda
      ]],
      {}
    )
  ),
  s(
    {
      trig = ";L",
      dscr = "Λ",
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \Lambda
      ]],
      {}
    )
  ),
  s(
    {
      trig = "([^%a])mu",
      dscr = "μ",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\mu
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
      trig = "([^%a])nu",
      dscr = "ν",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\nu
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
      trig = "([^%a])xi",
      dscr = "ξ",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\xi
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
      trig = "([^%a])Xi",
      dscr = "Ξ",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\Xi
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
      trig = "([^%a])pi",
      dscr = "π",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\pi
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
      trig = "([^%a])Pi",
      dscr = "Π",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\Pi
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
      trig = "([^%a])rho",
      dscr = "ρ",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\rho
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
      trig = ";vr",
      dscr = "variant of ρ",
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \varrho
      ]],
      {}
    )
  ),
  s(
    {
      trig = ";s",
      dscr = "σ",
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \sigma
      ]],
      {}
    )
  ),
  s(
    {
      trig = ";vs",
      dscr = "variant of σ",
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \varsigma
      ]],
      {}
    )
  ),
  s(
    {
      trig = ";S",
      dscr = "Σ",
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \Sigma
      ]],
      {}
    )
  ),
  s(
    {
      trig = "tau",
      dscr = "τ",
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \tau
      ]],
      {}
    )
  ),
  s(
    {
      trig = ";u",
      dscr = "υ",
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \upsilon
      ]],
      {}
    )
  ),
  s(
    {
      trig = ";U",
      dscr = "Υ",
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \Upsilon
      ]],
      {}
    )
  ),
  s(
    {
      trig = "([^%a])phi",
      dscr = "φ",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\phi
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
      trig = ";vp",
      dscr = "variant of φ",
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \varphi
      ]],
      {}
    )
  ),
  s(
    {
      trig = "([^%a])Phi",
      dscr = "Φ",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\Phi
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
      trig = "([^%a])chi",
      dscr = "χ",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\chi
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
      trig = "([^%a])psi",
      dscr = "ψ",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\psi
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
      trig = "([^%a])Psi",
      dscr = "Ψ",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\Psi
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
      trig = ";o",
      dscr = "ω",
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \omega
      ]],
      {}
    )
  ),
  s(
    {
      trig = ";O",
      dscr = "ω",
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \Omega
      ]],
      {}
    )
  ),
}
