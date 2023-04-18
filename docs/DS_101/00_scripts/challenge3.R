# 1.0 libraries
library(vroom)
library(readxl)
library("writexl")
library(tidyr)
library(purrr)
library("stringr") 
library(dplyr)
library(data.table)
# Tidyverse
library(tidyverse)
# 2.0 import data
col_assignee <- list(
  id = col_character(),
  type = col_character(),
  organization = col_character()
)
col_patent_assignee <- list(
  patent_id = col_character(),
  assignee_id = col_character()
)

col_patent <- list(
  id = col_character(),
  date = col_date("%Y-%m-%d"),
  num_claims = col_double()
)

col_uspc <- list(
  patent_id = col_character(),
  mainclass_id = col_character(),
  sequence = col_character()
)

patent_tbl <- vroom(
  file       = "DS_101/00_data/03_patent_data/patent.tsv", 
  delim      = "\t", 
  col_types  = col_patent,
  na         = c("", "NA", "NULL")
)
assignee_tbl <- vroom(
  
  file = "DS_101/00_data/03_patent_data/assignee.tsv",
  delim      = "\t", 
  col_types  = col_assignee,
  na         = c("", "NA", "NULL")
)
patent_assignee_tbl <- vroom(
  
  file = "DS_101/00_data/03_patent_data/patent_assignee.tsv",
  delim      = "\t", 
  col_types  =  col_patent_assignee,
  na         = c("", "NA", "NULL")
  
)
uspc_tbl <- vroom(
  
  file = "DS_101/00_data/03_patent_data/uspc.tsv",
  delim      = "\t", 
  col_types  =  col_uspc,
  na         = c("", "NA", "NULL")
  
)

patent_tbl <- patent_tbl %>% 
  mutate(year = lubridate::year(date))

assignee_tbl <- assignee_tbl %>% rename(assignee_id = id)
patent_tbl <- patent_tbl %>% rename(patent_id = id)

combined_tbl <- patent_assignee_tbl %>%
  left_join(assignee_tbl, by = "assignee_id")

combined_tbl$patent_count <- ifelse(!is.na(combined_tbl$patent_id), 1, 0)

result10 <- combined_tbl %>%
  filter(type == 2) %>%
  group_by("US Company / Organization" = organization) %>%
  summarise(Total_Patents = sum(patent_count)) %>%
  ungroup() %>%
  arrange(desc(Total_Patents)) %>%
  # Output the top 10 US companies/organizations with most assigned/granted patents
  slice(1:10)
result10

# By the Year
combined_tbl <- patent_assignee_tbl %>%
  right_join(patent_tbl, by = "patent_id")%>%
  right_join(assignee_tbl, by = 'assignee_id')

combined_tbl$patent_count <- ifelse(!is.na(combined_tbl$patent_id), 1, 0)

result11 <- combined_tbl %>%
  filter(type == 2 & year == 2014) %>%
  group_by(organization) %>%
  summarise(Total_Patents = sum(patent_count)) %>%
  ungroup() %>%
  arrange(desc(Total_Patents))%>%
  slice(1:10)
result11

# Most Innovative Company
combined_tbl <- patent_assignee_tbl %>%
  right_join(uspc_tbl, by = "patent_id")%>%
  right_join(assignee_tbl, by = 'assignee_id')

combined_tbl$patent_count <- ifelse(!is.na(combined_tbl$patent_id), 1, 0)

result12 <- combined_tbl %>%
  #filter(type == 2 | type == 3) %>%
  group_by('USPTO Tech Main Class' = mainclass_id) %>%
  summarise(Total_Patents = sum(patent_count)) %>%
  ungroup() %>%
  arrange(desc(Total_Patents))%>%
  slice(1:5)
result12