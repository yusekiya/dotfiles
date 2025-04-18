local ls = require("luasnip")
local s = ls.snippet
local i = ls.insert_node
local fmt = require("luasnip.extras.fmt").fmt

return {
  s(
    "snippet",
    fmt(
      [=[
        s(
          { trig = "<trigger>", dscr = "<description>" },
          fmt(
            [[
              <text>
            ]],
            { i(1, "<placeholder>") }
          )
        ),
      ]=],
      {
        trigger = i(1, "trigger"),
        description = i(2, "description"),
        text = i(3, "text"),
        placeholder = i(0, "placeholder"),
      },
      { delimiters = "<>" }
    )
  ),
  s(
    { trig = "snippet-require", dscr = "Expand LuaSnip abbreviations" },
    fmt(
      [[
        local ls = require("luasnip")
        local s = ls.snippet
        local sn = ls.snippet_node
        local t = ls.text_node
        local i = ls.insert_node
        local f = ls.function_node
        local d = ls.dynamic_node
        local fmt = require("luasnip.extras.fmt").fmt
        local fmta = require("luasnip.extras.fmt").fmta
        local rep = require("luasnip.extras").rep
        {}
      ]],
      { i(0) }
    )
  ),
}
