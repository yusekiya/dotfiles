-- Run inside `nvim --headless` with the user's config loaded.
-- Writes, for every lazy.nvim plugin, the installed commit and the commit
-- `:Lazy update` would move it to (resolved by lazy.nvim itself, so
-- `version`/`tag`/`branch`/`commit` specs are honored).
local out = assert(os.getenv("NVIM_AUDIT_OUT"), "NVIM_AUDIT_OUT not set")

local function main()
  local Config = require("lazy.core.config")
  local Git = require("lazy.manage.git")

  if os.getenv("NVIM_AUDIT_FETCH") == "1" then
    require("lazy").check({ wait = true, show = false })
  end

  local plugins = {}
  for name, p in pairs(Config.plugins) do
    local e = {
      name = name,
      dir = p.dir,
      url = p.url,
      installed = p._.installed and true or false,
      is_local = p._.is_local and true or false,
    }
    if e.installed and not e.is_local and p.url then
      local info = Git.info(p.dir)
      local target = Git.get_target(p)
      e.from = info and info.commit or nil
      e.to = target and target.commit or nil
      e.target_branch = target and target.branch or nil
      e.target_tag = target and target.tag or nil
    end
    table.insert(plugins, e)
  end
  return { lockfile = Config.options.lockfile, plugins = plugins }
end

local ok, res = xpcall(main, debug.traceback)
local f = assert(io.open(out, "w"))
f:write(vim.json.encode(ok and res or { error = res }))
f:close()
