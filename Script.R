
# 1. Load the required pacakges ----------------------------------------------
if(!require(shiny)) install.packages("shiny")
if(!require(plotly)) install.packages("plotly")
if(!require(tidyverse)) install.packages("tidyverse")


# 2. Create database -----------------------------------------------------
### set seed for reproducible results
set.seed(2001)

df_temp <- tibble(the_transaction_id = paste("trans", seq(1,200000), sep = "_"),
             but_idr_business_unit = as.factor(sample(c("Evere", "Wavre", "Anderlecht", "Antwerp", "Charleroi", 
                                              "Namur", "Liège"), 200000, replace = T,
                                            prob = c(0.35, 0.05, 0.10, 0.15, 0.10, 0.05, 0.20))),
             tdt_type_detail = sample(c("sale", "return"), 200000, replace = T, prob = c(0.9, 0.1)),
             the_date_transaction = sample(seq(as.Date("2019/01/01"), as.Date("2019/12/31"), by = "day"),
                                           200000, replace = T),
             sku_idr_sku = as.factor(paste(sample(c("tennis_item", "football_item", "running_item", "biking_item", 
                                                    "other_item"), 200000, replace = T),
                                           sample(c(1:500), replace = T), sep = "_")))

new_prices <- df_temp %>%
  distinct(but_idr_business_unit, sku_idr_sku) %>%
  mutate(unit_price = sample(seq(0.50, 1000, 0.01), n(), replace = T))

df <- df_temp %>%
  left_join(new_prices, by = c("but_idr_business_unit", "sku_idr_sku")) %>%
  mutate(quantity = sample(seq(1:5), 200000, replace = T, 
                                     prob = c(0.70,0.15, 0.05, 0.03, 0.02)),
         turnover = quantity * unit_price)



   

  