
# SYNCH/PUBLISH BOOK TO BOOKDOWN.ORG (created this becuase the 'publish' button in corner of html interface window after render does not work)

# clean out old versions
bookdown::clean_book(TRUE)

# render new version
bookdown:: render_book()

# publish book to https://bookdown.org/kwfwqx/Kenai_River_Water_Quality_Assessment/
bookdown::publish_book(name = "Kenai_River_Water_Quality_Assessment", account = "kwfwqx")

# note 12/15/2022 -- the command blogdown:::serve_site() appears to no longer function for most
# bookdown renders; including this one.


# run this function manually to create auto-refreshing preview in web browser.
# https://bookdown.org/yihui/bookdown/serve-the-book.html

# load packages
library(bookdown)
library(servr)

# serve book
serve_book(dir = ".", output_dir = "_book", preview = TRUE,
           in_session = TRUE, quiet = FALSE)

# as of 12/15/2022, "Error: pandoc document conversion failed with error 64"
# maybe more efficient to just convert to Quarto to proceed????

