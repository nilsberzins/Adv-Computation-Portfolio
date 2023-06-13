
library(rvest)
library(magrittr)

wiki <- read_html("https://en.wikipedia.org/wiki/Web_scraping")

headlines <- wiki %>%
  html_nodes(".mw-headline") %>%
  html_text()

verification_boxes <- wiki %>% 
  html_nodes(".ambox-content") %>%
  html_text()

verification_boxes

intext_links <- wiki %>%
  html_nodes(".navigation-not-searchable a , #mw-content-text li a , p a") %>%
  html_text()

#Used Selector Gadget to find node pathings