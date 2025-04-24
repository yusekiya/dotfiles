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
      trig = "([^%a])kbt",
      dscr = "Boltzmann constant times temperature",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>k_\mathrm{B}T
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
      trig = "([^%a])hbar",
      dscr = "reduced Planck constant",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\hbar
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
      trig = "([^%a])o%+",
      dscr = "direct sum",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\oplus
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
      trig = "([^%a])O%+",
      dscr = "direct sum operator",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\bigoplus
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
      trig = "([^%a])ox",
      dscr = "direct product",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\otimes
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
      trig = "([^%a])Ox",
      dscr = "direct product operator",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\bigotimes
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
      trig = "([^%a])bra",
      dscr = "bra state",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\bra{<>}
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
      trig = "([^%a])Bra",
      dscr = "large bra state",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\Bra{<>}
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
      trig = "([^%a])ket",
      dscr = "ket state",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\ket{<>}
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
      trig = "([^%a])Ket",
      dscr = "large ket state",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\Ket{<>}
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
      trig = "([^%a])brk",
      dscr = "braket",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\braket{<>}
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
      trig = "([^%a])Brk",
      dscr = "large braket",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\Braket{<>}
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
      trig = "([^%a])outp",
      dscr = "outer product",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\ket{<>}\!\bra{<>}
      ]],
      {
        f(function(_, snip)
          return snip.captures[1]
        end),
        d(1, get_visual),
        rep(1),
      }
    )
  ),
  s(
    {
      trig = "([^%a])Outp",
      dscr = "large outer product",
      regTrig = true,
      wordTrig = false,
      condition = tex_utils.in_mathzone,
      snippetType = "autosnippet",
    },
    fmta(
      [[
        <>\Ket{<>}\!\Bra{<>}
      ]],
      {
        f(function(_, snip)
          return snip.captures[1]
        end),
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
