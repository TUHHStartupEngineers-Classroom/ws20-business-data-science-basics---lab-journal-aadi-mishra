# SALES ANALYSIS ----

# 1.0 Load libraries ----
library(tidyverse)
library(readxl)

# 2.0 Importing Files ----
bikes_tbl <- read_excel("00_data/01_bike_sales/01_raw_data/bikes.xlsx")
bikeshops_tbl <-  read_excel("00_data/01_bike_sales/01_raw_data/bikeshops.xlsx")
orderlines_tbl <-  read_excel("00_data/01_bike_sales/01_raw_data/orderlines.xlsx")

# 3.0 Examining Data ----
glimpse(orderlines)

# 4.0 Joining Data ----
bike_orderlines_joined_tbl <- orderlines %>%
  left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
  left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))

bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>%
  separate(col = location,into   = c("state", "city"), sep    = ",", convert = T)

