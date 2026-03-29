-- pagebreak-h1.lua
-- Inserts an OpenXML page break before every level-1 heading when
-- rendering to DOCX. Has no effect on HTML or other formats.

if FORMAT == "docx" then
  function Header(el)
    if el.level == 1 then
      local pagebreak = pandoc.RawBlock(
        'openxml',
        '<w:p><w:r><w:br w:type="page"/></w:r></w:p>'
      )
      return { pagebreak, el }
    end
  end
end
