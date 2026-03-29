# embed_if_html.R
# Wrapper around xfun::embed_file() that renders a download link in HTML output
# only. In DOCX/PDF output the call is silently skipped.
#
# Usage: embed_if_html("path/to/file.csv", text = "Download label")
# Drop-in replacement for xfun::embed_file().

embed_if_html <- function(path, text = basename(path)) {
  if (knitr::is_html_output()) {
    xfun::embed_file(path, text = text)
  } else {
    invisible(NULL)
  }
}
