
# 1. Load the required pacakges ----------------------------------------------
if(!require(shiny)) install.packages("shiny")
if(!require(plotly)) install.packages("plotly")
if(!require(tidyverse)) install.packages("tidyverse")


# 2. Create database -----------------------------------------------------
### set seed for reproducible results
set.seed(2001)

df_store <- tibble(the_transaction_id = paste("trans", seq(1,200000), sep = "_"),
                   but_idr_business_unit = sample(c("Evere", "Wavre", "Anderlecht", "Antwerp", "Charleroi", 
                                                    "Namur", "Liège"), 200000, replace = T,
                                                  prob = c(0.35, 0.05, 0.10, 0.15, 0.10, 0.05, 0.20)),
                   tdt_type_detail = sample(c("sale", "return"), 200000, replace = T, prob = c(0.9, 0.1)),
                   the_date_transaction = sample(seq(as.Date("2019/01/01"), as.Date("2019/12/31"), by = "day"),
                                                 200000, replace = T),
                   sku_idr_sku = paste(sample(c("tennis_item", "football_item", "running_item", "biking_item", 
                                                "other_item"), 200000, replace = T),
                                       sample(c(1:500), replace = T), sep = "_"),
                   ) 



