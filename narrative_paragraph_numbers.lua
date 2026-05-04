local para_counter = 0
local in_appendix = false

local function is_excluded_div(div)
  for _, class in ipairs(div.classes) do
    if class == "content-visible" or class == "no-para-number" or class:match("^callout") then
      return true
    end
  end
  return false
end

local function number_paragraph(para)
  para_counter = para_counter + 1

  local content = pandoc.List:new()
  content:insert(pandoc.Strong({ pandoc.Str(tostring(para_counter) .. ".") }))
  content:insert(pandoc.Space())

  for _, inline in ipairs(para.content) do
    content:insert(inline)
  end

  return pandoc.Para(content)
end

local function process_blocks(blocks, excluded)
  local out = pandoc.List:new()

  for _, block in ipairs(blocks) do
    if block.t == "Header" then
      if block.level == 1 then
        local header_text = pandoc.utils.stringify(block.content)
        if header_text:match("^Appendix") then
          in_appendix = true
        end
      end
      out:insert(block)
    elseif block.t == "Para" then
      if excluded or in_appendix then
        out:insert(block)
      else
        out:insert(number_paragraph(block))
      end
    elseif block.t == "Div" then
      block.content = process_blocks(block.content, excluded or is_excluded_div(block))
      out:insert(block)
    elseif block.t == "BlockQuote" then
      out:insert(block)
    elseif block.t == "BulletList" or block.t == "OrderedList" or block.t == "DefinitionList" or block.t == "Table" then
      out:insert(block)
    else
      out:insert(block)
    end
  end

  return out
end

function Pandoc(doc)
  para_counter = 0
  in_appendix = false
  doc.blocks = process_blocks(doc.blocks, false)
  return doc
end
