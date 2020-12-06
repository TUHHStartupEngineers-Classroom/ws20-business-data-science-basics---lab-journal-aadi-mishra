# Import Libraries
library(fs)
library(tidyverse)
library(RSQLite)
library(httr)
library(glue)
library(jsonlite) 
library(stringr)
library(rvest)
library(purrr)

#https://covid-api.mmediagroup.fr/v1/cases?country=Afghanistan , params = glue("={params}"
#/history?country=France&status=Confirmed

########################################################################################
itunes_search_api <- function(path, query) {
  url <- modify_url(url = "https://itunes.apple.com/", 
                    path = glue("/{path}"), 
                    query = glue("{query}"))
  response <- GET(url)
  stop_for_status(response) # automatically throws an error if a request did not succeed
}
response <- itunes_search_api("search", paste("term=dire+straits","limit=25",sep="&"))

# Convert JSON as text into a nested list object and convert to tibble
response_tbl <- fromJSON(content(response, as = "text")) %>%
  map_if(is.data.frame, list) %>%
  as_tibble() %>%
  unnest(cols = c(results))


response_tbl

#######
#Web Scraping 
url_home <- "https://www.radon-bikes.de/"
html_home <- read_html(url_home)
radonbike_category_tbl <- html_home%>% 
  html_nodes("div.megamenu__item > a") %>%
  html_attr('href')%>%
  discard(.p =~ str_detect(.x,"/wear/*"))%>%
  enframe(name = "position", value = "Category_Path")%>%
  separate(col = Category_Path, into = c("garbage1","Model", "Variant", "garbage2"), sep = "/")%>%
  select(Model, Variant)
radonbike_category_tbl
#https://www.radon-bikes.de/mountainbike/hardtail/

mountain_bike_hardtail_url <- str_c(url_home, radonbike_category_tbl$Model[1],"/",radonbike_category_tbl$Variant[1])

html_home <- read_html(mountain_bike_hardtail_url)
radonbike_hardtail_prices_tbl <- html_home%>%
  html_nodes("div.button-label > span")%>%
  html_text(trim = TRUE)%>% 
  strsplit(split = "\n") %>%
  unlist() %>%
  .[. != ""]%>%
  parse_number()%>%
  enframe(name = "position", value = "Hardtail_Prices")%>%
  select("Hardtail_Prices")
radonbike_hardtail_prices_tbl

new_hardtail <- radonbike_hardtail_prices_tbl%>% join("hardtail", by = character)


mountain_bike_fullsuspension_url <- str_c(url_home, radonbike_category_tbl$Model[1],"/",radonbike_category_tbl$Variant[2])

html_home <- read_html(mountain_bike_fullsuspension_url)
radonbike_fullsuspension_prices_tbl <- html_home%>%
  html_nodes("div.button-label > span")%>%
  html_text(trim = TRUE)%>% 
  strsplit(split = "\n") %>%
  unlist() %>%
  .[. != ""]%>%
  parse_number()%>%
  enframe(name = "position", value = "Fullsuspension_Prices")%>%
  select("Fullsuspension_Prices")
radonbike_fullsuspension_prices_tbl

prices_joined_tbl <- rbind.fill(mtcars[c(radonbike_hardtail_prices_tbl, radonbike_fullsuspension_prices_tbl)])
prices_joined_tbl 