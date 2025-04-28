local M = {}

local MATH_NODES = {
  displayed_equation = true,
  inline_formula = true,
  math_environment = true,
}

local TEXT_NODES = {
  text_mode = true,
  label_definition = true,
  label_reference = true,
}

local CODE_BLOCK_NODES = {    -- Add this to define code block node types
  fenced_code_block = true,
  indented_code_block = true, -- Optional: include indented code blocks as well if needed
}

function M.in_text(check_parent)
  local node = vim.treesitter.get_node({ ignore_injections = false })

  -- Check for code blocks in any filetype
  local block_node = node
  while block_node do
    if CODE_BLOCK_NODES[block_node:type()] then
      return true -- If in a code block, always consider it text
    end
    block_node = block_node:parent()
  end

  while node do
    if node:type() == "text_mode" then
      if check_parent then
        -- For \text{}
        local parent = node:parent()
        if parent and MATH_NODES[parent:type()] then
          return false
        end
      end
      return true
    elseif MATH_NODES[node:type()] then
      return false
    end
    node = node:parent()
  end
  return true
end

function M.in_mathzone()
  local node = vim.treesitter.get_node({ ignore_injections = false })

  while node do
    if TEXT_NODES[node:type()] then
      return false
    elseif MATH_NODES[node:type()] then
      return true
    end
    node = node:parent()
  end
  return false
end

M.comment = function()
  return vim.fn["vimtex#syntax#in_comment"]() == 1
end

M.env = function(name)
  local x, y = table.unpack(vim.fn["vimtex#env#is_inside"](name))
  return x ~= "0" and y ~= "0"
end

return M
