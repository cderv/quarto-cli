-- shortcodes.lua
-- Copyright (C) 2020-2022 Posit Software, PBC

local shortcode_lpeg = require("lpegshortcode")

_quarto.ast.add_handler({
  class_name = { "quarto-shortcode__" },

  ast_name = "Shortcode",

  kind = "Inline",

  parse = function(span)
    local inner_content = pandoc.Inlines({})

    span.content = span.content:filter(function(el)
      return el.t == "Span"
    end)
    local shortcode_content = span.content:map(function(el)
      if not el.classes:includes("quarto-shortcode__-param") then
        -- luacov: disable
        quarto.log.output(el)
        fatal("Unexpected span in a shortcode parse")
        -- luacov: enable
      end

      -- is it a recursive shortcode?
      local custom_data, t, kind = _quarto.ast.resolve_custom_data(el)
      if custom_data ~= nil then
        local inner_index = #inner_content+1
        inner_content:insert(custom_data)
        return {
          type = "shortcode",
          value = inner_index
        }
      end

      -- is it a plain value?
      if el.attributes["data-key"] == nil and el.attributes["data-value"] then
        return {
          type = "param",
          value = el.attributes["data-value"]
        }
      end

      -- it is a key value.
      if el.attributes["data-key"] then
        local key = el.attributes["data-key"]
        local value = el.attributes["data-value"]
        if value == nil then
          -- it's a recursive value
          value = el.content[1]
          local inner_index = #inner_content+1
          inner_content:insert(value)
          return {
            type = "key-value-shortcode",
            key = key,
            value = inner_index
          }
        else
          -- it's a plain value
          return {
            type = "key-value",
            key = key,
            value = value
          }
        end
      else
        -- luacov: disable
        quarto.log.output(el)
        fatal("Unexpected span in a shortcode parse")
        -- luacov: enable
      end
    end)
    local name = shortcode_content:remove(1)
    if name.type == "param" then
      name = name.value
    end

    local node = _quarto.ast.create_custom_node_scaffold("Shortcode", "Inline")
    node.content = pandoc.Inlines(inner_content:map(function(el)
      return pandoc.Span({el})
    end))
    local tbl = {
      __quarto_custom_node = node,
      name = name,
      unparsed_content = span.attributes["data-raw"],
      params = shortcode_content
    }
    
    return quarto.Shortcode(tbl)
  end,

  render = function(node)
    quarto.log.output(node)
    -- luacov: disable
    internal_error()
    -- luacov: enable
  end,

  constructor = function(tbl)
    return tbl, false
  end,
})

local function handle_shortcode(shortcode_tbl, node, context)
  local name
  if type(shortcode_tbl.name) ~= "string" then
    -- this is a recursive shortcode call,
    -- name is a number that indexes into the node's content
    -- to get the shortcode node to call.

    -- typically, shortcodes are resolved in a typewise traversal
    -- which is bottom up, so we should be seeing resolved shortcode
    -- content here. But in unusual cases, we might be calling
    -- this function outside of a filter, in which case
    -- we need to handle this explicitly

    if type(shortcode_tbl.name) ~= "number" then
      -- luacov: disable
      quarto.log.output(shortcode_tbl.name)
      fatal("Unexpected shortcode name type " .. type(shortcode_tbl.name))
      -- luacov: enable
    end

    local shortcode_node = node.content[shortcode_tbl.name]
    -- are we already resolved?
    for i, v in ipairs(shortcode_node.content) do
      local custom_data, t, kind = _quarto.ast.resolve_custom_data(v)
      if custom_data ~= nil then
        if t ~= "Shortcode" then
          -- luacov: disable
          quarto.log.output(t)
          fatal("Unexpected shortcode content type " .. tostring(t))
          -- luacov: enable
        end
        -- we are not resolved, so resolve
        shortcode_node.content[i] = handle_shortcode(custom_data, v, context)
      end
    end

    name = pandoc.utils.stringify(shortcode_node)
    -- TODO check that this returns a string as it should
  else 
    name = shortcode_tbl.name
  end

  local args = {}
  local raw_args = {}

  for _, v in ipairs(shortcode_tbl.params) do
    if v.type == "key-value" then
      table.insert(args, { name = v.key, value = v.value })
      table.insert(raw_args, v.value)
    elseif v.type == "key-value-shortcode" then
      local result = handle_shortcode(v.value, node, context)
      table.insert(args, { name = v.key, value = result })
      table.insert(raw_args, result)
    elseif v.type == "shortcode" then
      local shortcode_node = node.content[v.value]
      local custom_data, t, kind = _quarto.ast.resolve_custom_data(shortcode_node)
      local result
      if custom_data == nil then
        result = pandoc.utils.stringify(shortcode_node)
      elseif t ~= "Shortcode" then
        -- luacov: disable
        quarto.log.output(custom_data)
        quarto.log.output(t)
        fatal("Unexpected shortcode content type " .. tostring(t))
        -- luacov: enable
      else
        local result = handle_shortcode(custom_data, shortcode_node, context)
        result = pandoc.utils.stringify(result)
      end
      table.insert(args, { value = result })
      table.insert(raw_args, result)
    elseif v.type == "param" then
      table.insert(args, { value = v.value })
      table.insert(raw_args, v.value)
    else
      -- luacov: disable
      quarto.log.output(v)
      fatal("Unexpected shortcode param type " .. tostring(v.type))
      -- luacov: enable
    end
  end

  local shortcode_struct = {
    args = args,
    raw_args = raw_args,
    name = name,
    unparsed_content = shortcode_tbl.unparsed_content
  }

  local handler = handlerForShortcode(shortcode_struct)
  if handler == nil then
    return nil, shortcode_struct
  end

  return callShortcodeHandler(handler, shortcode_struct, context), shortcode_struct
end

local _shortcodes_filter = nil
function process_shortcodes(content)
  return _quarto.ast.walk(content, _shortcodes_filter)
end

function shortcodes_filter()

  local code_shortcode = shortcode_lpeg.make_shortcode_parser({
    escaped = function(s) return "{{<" .. s .. ">}}" end,
    string = function(s) return { value = s } end,
    keyvalue = function(k, r, v) 
      return { name = k, value = v } 
    end,
    shortcode = function(open, space, lst, close)
      local name = table.remove(lst, 1).value
      local raw_args = {}
      for _, v in ipairs(lst) do
        table.insert(raw_args, v.value)
      end
      local shortcode_struct = {
        args = lst,
        raw_args = raw_args,
        name = name
      }
      local handler = handlerForShortcode(shortcode_struct)
      if handler == nil then
        local strs = {}
        table.insert(strs, open)
        table.insert(strs, space)
        table.insert(strs, name)
        for _, v in ipairs(lst) do
          if type(v) == "string" then
            table.insert(strs, v)
          else
            if v.name then
              table.insert(strs, v.name .. "=" .. v.value)
            else
              table.insert(strs, v.value)
            end
          end
        end
        table.insert(strs, close)
        return table.concat(strs, "")
      end
      local result = callShortcodeHandler(handler, shortcode_struct, "text")
      return pandoc.utils.stringify(result) 
    end, 
  })
  local function apply_code_shortcode(text)
    return shortcode_lpeg.wrap_lpeg_match(code_shortcode, text) or text
  end

  local filter

  local block_handler = function(node)
    if (node.t == "Para" or node.t == "Plain") and #node.content == 1 then
      node = node.content[1]
    end
    local custom_data, t, kind = _quarto.ast.resolve_custom_data(node)
    if t ~= "Shortcode" then
      return nil
    end
    local result, struct = handle_shortcode(custom_data, node, "block")
    return _quarto.ast.walk(shortcodeResultAsBlocks(result, struct.name, custom_data), filter)
  end

  local inline_handler = function(custom_data, node)
    local result, struct = handle_shortcode(custom_data, node, "inline")
    local r1 = shortcodeResultAsInlines(result, struct.name, custom_data)
    local r2 = _quarto.ast.walk(r1, filter)
    return r2
  end

  local code_handler = function(el)
    -- don't process shortcodes in code output from engines
    -- (anything in an engine processed code block was actually
    --  proccessed by the engine, so should be printed as is)
    if el.attr and el.attr.classes:includes("cell-code") then
      return
    end

    -- don't process shortcodes if they are explicitly turned off
    if el.attr and el.attr.attributes["shortcodes"] == "false" then
      return
    end

    el.text = apply_code_shortcode(el.text)
    return el
  end

  local attr_handler = function(el)
    for k,v in pairs(el.attributes) do
      if type(v) == "string" then
        el.attributes[k] = apply_code_shortcode(v)
      end
    end
    return el
  end

  filter = {
    Pandoc = function(doc)
      -- first walk them in block context
      doc = _quarto.ast.walk(doc, {
        Para = block_handler,
        Plain = block_handler,
        Code = code_handler,
        RawBlock = code_handler,
        CodeBlock = code_handler,
        Header = attr_handler,
        Div = function(el)
          if el.classes:includes("quarto-markdown-envelope-contents") then
            return nil
          end
          if el.classes:includes("quarto-shortcode__-escaped") then
            return pandoc.Plain(pandoc.Str(el.attributes["data-value"]))
          else
            el = attr_handler(el)
            return el
          end
        end,
      })

      doc = _quarto.ast.walk(doc, {
        Shortcode = inline_handler,
        RawInline = code_handler,
        Image = function(el)
          el = attr_handler(el)
          el.src = apply_code_shortcode(el.src)
          return el
        end,
        Link = function(el)
          el = attr_handler(el)
          el.target = apply_code_shortcode(el.target)
          return el
        end,
        Span = function(el)
          if el.classes:includes("quarto-markdown-envelope-contents") then
            return nil
          end
          if el.classes:includes("quarto-shortcode__-escaped") then
            return pandoc.Str(el.attributes["data-value"])
          else
            el = attr_handler(el)
            return el
          end
        end,
       })
      return doc
    end
  }

  _shortcodes_filter = filter
  return filter
end

-- helper function to read metadata options
local function readMetadata(value)
  -- We were previously coercing everything to lists of inlines when possible
  -- which made for some simpler treatment of values in meta, but it also
  -- meant that reading meta here was different than reading meta in filters
  -- 
  -- This is now just returning the raw meta value and not coercing it, so 
  -- users will have to be more thoughtful (or just use pandoc.utils.stringify)
  --
  -- Additionally, this used to return an empty list of inlines but now
  -- it returns nil for an unset value
  return option(value, nil)
end

-- call a handler w/ args & kwargs
function callShortcodeHandler(handler, shortCode, context)
  local args = pandoc.List()
  local kwargs = setmetatable({}, { __index = function () return pandoc.Inlines({}) end })
  for _,arg in ipairs(shortCode.args) do
    if arg.name then
      kwargs[arg.name] = arg.value
    else
      args:insert(arg.value)
    end
  end
  local meta = setmetatable({}, { __index = function(t, i) 
    return readMetadata(i)
  end})
  local callback = function()
    return handler.handle(args, kwargs, meta, shortCode.raw_args, context)
  end
  -- set the script file path, if present
  if handler.file ~= nil then
    return _quarto.withScriptFile(handler.file, callback)
  else
    return callback()
  end
end

function shortcodeResultAsInlines(result, name, shortcode_tbl)
  if result == nil then
    warn("Shortcode '" .. name .. "' not found")
    local result = pandoc.Inlines({pandoc.RawInline(FORMAT, shortcode_tbl.unparsed_content)})
    return result
  end
  local type = quarto.utils.type(result)
  if type == "Inlines" then
    return result
  elseif type == "Blocks" then
    return pandoc.utils.blocks_to_inlines(result, { pandoc.Space() })
  elseif type == "string" then
    return pandoc.Inlines( { pandoc.Str(result) })
  elseif tisarray(result) then
    local items = pandoc.List(result)
    local inlines = items:filter(isInlineEl)
    if #inlines > 0 then
      return pandoc.Inlines(inlines)
    else
      local blocks = items:filter(isBlockEl)
      return pandoc.utils.blocks_to_inlines(blocks, { pandoc.Space() })
    end
  elseif isInlineEl(result) then
    return pandoc.Inlines( { result })
  elseif isBlockEl(result) then
    return pandoc.utils.blocks_to_inlines( { result }, { pandoc.Space() })
  else
    -- luacov: disable
    error("Unexpected result from shortcode " .. name .. "")
    quarto.log.output(result)
    fatal("This is a bug in the shortcode. If this is a quarto shortcode, please report it at https://github.com/quarto-dev/quarto-cli")
    -- luacov: enable
  end
end
  
function shortcodeResultAsBlocks(result, name, shortcode_tbl)
  if result == nil then
    if name ~= 'include' then
      warn("Shortcode '" .. name .. "' not found")
    end
    return pandoc.Blocks({pandoc.RawBlock(FORMAT, shortcode_tbl.unparsed_content)})
  end
  local type = quarto.utils.type(result)
  if type == "Blocks" then
    return result
  elseif type == "Inlines" then
    return pandoc.Blocks( {pandoc.Para(result) }) -- why not a plain?
  elseif type == "string" then
    return pandoc.Blocks( {pandoc.Para({pandoc.Str(result)})} ) -- why not a plain?
  elseif tisarray(result) then
    local items = pandoc.List(result)
    local blocks = items:filter(isBlockEl)
    if #blocks > 0 then
      return pandoc.Blocks(blocks)
    else
      local inlines = items:filter(isInlineEl)
      return pandoc.Blocks({pandoc.Para(inlines)}) -- why not a plain?
    end
  elseif isBlockEl(result) then
    return pandoc.Blocks( { result } )
  elseif isInlineEl(result) then
    return pandoc.Blocks( {pandoc.Para( {result} ) }) -- why not a plain?
  else
    -- luacov: disable
    error("Unexpected result from shortcode " .. name .. "")
    quarto.log.output(result)
    fatal("This is a bug in the shortcode. If this is a quarto shortcode, please report it at https://github.com/quarto-dev/quarto-cli")
    -- luacov: enable
  end
end
