# SALES ANALYSIS ----

# 1.0 Load libraries ----
library(tidyverse)
library(readxl)
library(lubridate)
library(writexl)

# 2.0 Import data ----
bikes_tbl <- read_excel(path = "DS_101/00_data/01_bike_sales/01_raw_data/bikes.xlsx")
orderlines_tbl <- read_excel("DS_101/00_data/01_bike_sales/01_raw_data/orderlines.xlsx")
bikeshops_tbl  <- read_excel("DS_101/00_data/01_bike_sales/01_raw_data/bikeshops.xlsx")

# 3.0 Examining Data ----
glimpse(orderlines_tbl)

# 4.0 Joining Data ----
bike_orderlines_joined_tbl <- orderlines_tbl %>%
  left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
  left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))

# 5.0 Wrangling Data ----

bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>%
  separate(col    = location,
           into   = c("city", "state"),
           sep    = ", ") %>%
  
  mutate(total.price = price * quantity) %>%
  select(-...1, -gender) %>%
  select(-ends_with(".id")) %>%
  bind_cols(bike_orderlines_joined_tbl %>% select(order.id)) %>% 
  
  select(order.id, contains("order"), contains("model"), contains("state"),
         contains("city"), price, quantity, total.price,
         everything()) %>%
  
  rename(bikeshop = name) %>%
  set_names(names(.) %>% str_replace_all("\\.", "_"))

# 6.0 Business Insights ----
# 6.1 Sales by Year ----
# Manipulate the data and store result
sales_by_loc_tbl <- bike_orderlines_wrangled_tbl %>%
  
  # Select columns
  select(contains("state"), total_price) %>%
  
  # Grouping by year and summarizing sales
  group_by(state) %>% 
  summarize(sales = sum(total_price)) %>%
  arrange(desc(sales)) %>%
  # Optional: Add a column that turns the numbers into a currency format 
  # (makes it in the plot optically more appealing)
  # mutate(sales_text = scales::dollar(sales)) <- Works for dollar values
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))

sales_by_loc_tbl

sales_by_loc_tbl %>%
  
  # Step 2 - Visualize
  # Setup canvas with the columns state (x-axis) and sales (y-axis)
  # States are reordered in decreasing sales for a better visual (e.g. similar to Pareto chart)
  ggplot(aes(x = reorder(state,-sales), y = sales)) +
  # Rotate the x-axis labels
  theme(axis.title.x = element_text(), axis.text.x = element_text(angle = 45, hjust = 1)) +
  
  # Geometries
  geom_col(fill = "#2D303E") + # Use geom_col for a bar plot and fill with color
  # Adding labels to the bars along with formatting for better presentation
  geom_text(aes(label = sales_text), position = position_dodge(width = 0.9), 
            hjust = -0.1, size = 2.5, show.legend = FALSE, angle = 90) +
  
  # Formatting and re-scaling the y-axis
  # Again, we have to adjust it for euro values
  scale_y_continuous(expand = c(0,0), limits = c(0,25000000),
                     labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  
  # Final touches to the plot to ensure titles/subtitles are present
  labs(title    = "Revenue by State",
       subtitle = "Ordered from most to least total revenue",
       x = "State", # Changes the x-axis name
       y = "Revenue")

# 6.2 Sales by Year and location ----
# Manipulate the data and store result
sales_by_loc_year_tbl <- bike_orderlines_wrangled_tbl %>%
  
  # Select columns
  select(order_date, contains("state"), total_price) %>%
  
  # Add year column
  # Note the year() function runs if "lubridate" package was run via library() function
  mutate(year = year(order_date)) %>%
  
  # Grouping by state and year and summarizing sales
  group_by(state, year) %>% 
  summarize(sales = sum(total_price)) %>%
  arrange(year) %>% 
  ungroup() %>%
  
  # Format $ Text
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))

sales_by_loc_year_tbl

sales_by_loc_year_tbl %>%
  # Setup canvas with the columns year (x-axis), sales (y-axis) and state (fill)
  ggplot(aes(x = year, y = sales, fill = state)) +
  # Rotate the x-axis labels
  theme(axis.title.x = element_text(), axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="top") +
  
  # Geometries
  geom_col() + # Use geom_col for a bar plot
  
  # Facet
  facet_wrap(~ state, nrow = 2) +
  
  # Adding labels to the bars along with formatting for better presentation
  geom_text(aes(label = sales_text), position = position_dodge(width = 0.9), 
            hjust = -0.1, size = 2.5, show.legend = FALSE, angle=90) +
  
  # Formatting and re-scaling the y-axis
  # Again, we have to adjust it for euro values
  scale_y_continuous(expand = c(0,0), limits = c(0,7500000),
                     labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  
  # Final touches to the plot to ensure titles/subtitles are present
  labs(title = "Revenue by State and Year",
       x = "Year",
       y = "Revenue",
       # Changes the legend name
       fill = "State")