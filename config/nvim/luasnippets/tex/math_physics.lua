local ls = require("luasnip")
local s = ls.snippet
local i = ls.insert_node
local f = ls.function_node
local d = ls.dynamic_node
local fmta = require("luasnip.extras.fmt").fmta
local rep = require("luasnip.extras").rep

local ls_common = require("luasnip-utils.common")
local get_visual = ls_common.get_visual
local tex_utils = require("luasnip-utils.tex")

return {
  s(
    {
      trig = "kbt",
      dscr = "Boltzmann constant times temperature",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        k_\mathrm{B}T
      ]],
      {}
    )
  ),
  s(
    {
      trig = "hbar",
      dscr = "reduced Planck constant",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \hbar
      ]],
      {}
    )
  ),
  s(
    {
      trig = "([%s]*)dag",
      dscr = "Hermitian conjugate",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        ^\dagger<>
      ]],
      {
        i(0),
      }
    )
  ),
  s(
    {
      trig = "o%+",
      dscr = "direct sum",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \oplus
      ]],
      {}
    )
  ),
  s(
    {
      trig = "O%+",
      dscr = "direct sum operator",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \bigoplus
      ]],
      {}
    )
  ),
  s(
    {
      trig = "ox",
      dscr = "direct product",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \otimes
      ]],
      {}
    )
  ),
  s(
    {
      trig = "Ox",
      dscr = "direct product operator",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \bigotimes
      ]],
      {}
    )
  ),
  s(
    {
      trig = "qty",
      dscr = "quantity with unit",
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta([[\qty{<>}{<>}]], {
      i(1),
      i(2),
    })
  ),
  s(
    {
      trig = "bra",
      dscr = "bra state",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \bra{<>}
      ]],
      {
        d(1, get_visual),
      }
    )
  ),
  s(
    {
      trig = "Bra",
      dscr = "large bra state",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \Bra{<>}
      ]],
      {
        d(1, get_visual),
      }
    )
  ),
  s(
    {
      trig = "ket",
      dscr = "ket state",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \ket{<>}
      ]],
      {
        d(1, get_visual),
      }
    )
  ),
  s(
    {
      trig = "Ket",
      dscr = "large ket state",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \Ket{<>}
      ]],
      {
        d(1, get_visual),
      }
    )
  ),
  s(
    {
      trig = "brk",
      dscr = "braket",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \braket{<>}
      ]],
      {
        d(1, get_visual),
      }
    )
  ),
  s(
    {
      trig = "Brk",
      dscr = "large braket",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \Braket{<>}
      ]],
      {
        d(1, get_visual),
      }
    )
  ),
  s(
    {
      trig = "outp",
      dscr = "outer product",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \ket{<>}\!\bra{<>}
      ]],
      {
        d(1, get_visual),
        rep(1),
      }
    )
  ),
  s(
    {
      trig = "Outp",
      dscr = "large outer product",
      regTrig = true,
      wordTrig = true,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \Ket{<>}\!\Bra{<>}
      ]],
      {
        d(1, get_visual),
        rep(1),
      }
    )
  ),
  s(
    {
      trig = "\\bra{([^|]+)|",
      dscr = "change bra into braket",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \braket{<>|<>
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
      trig = "\\Bra{([^|]+)|",
      dscr = "change large bra into braket",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        \Braket{<>|<>
      ]],
      {
        f(function(_, snip)
          return snip.captures[1]
        end),
        i(0),
      }
    )
  ),
}
