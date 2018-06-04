# Notes on PDF and web scraping presentations
# 27 Feb 2018
# Michelle Kelly

# Outline:
#     1. extract text and metadata from websites
#         a. CSS selectors
#         b. regex
#         c. xml2 and httr
#     2. extract text and metadata from PDFs
#         a. regex
#         b. troubleshooting package install issues

# 1.  Extract text and metadata from websites ----
#     Extraction by CSS type
#       HTML docs are built in CSS
#       CSS describes how the HTML elements should be displayed

#     Can use 'rvest' package to select CSS style that you want to scrape
#       but... if the website changes formatting this could be a problem

#     Using Regex to select for only what you want
#       take the CSS scrape, trim for useful info using CSS

# 2.  Extract text and metadata from PDFs ----
#       PDFs aren't meant for data pulling :(

library(pdftools)
PDFurl <- "https://www.dropbox.com/s/py1kmj21ysbkuqk/cnps.pdf?dl=1"
PDFgg <- pdftools::pdf_text(PDFurl)
str(PDFgg) #outputs a gross list, one string for each PDF page
    PDFgg[1] #first page
    cat(PDFgg[1]) # displays line-by-line a little more human readably
    nchar(PDFgg[1]) # number of characters on first page
    strsplit(PDFgg[1], split = "\r\n") # re-lists by "line ends" (may or may not be complete sentences)

#       PDF table scraping
install.packages("tabulizer") # gives warning for our current R version... there is probably a more recent, applicable package

library(devtools)
install_github("ropensci/tabulizer")
library(tabulizer)
# pull tabulizer from github instead

# could be more useful... extract_metadata
pdftools::pdf_info(pdf = PDFgg)
