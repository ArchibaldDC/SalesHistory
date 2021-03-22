
# 1. Load the required pacakges ----------------------------------------------
if(!require(shiny)) install.packages("shiny")
if(!require(plotly)) install.packages("plotly")
if(!require(lubridate)) install.packages("lubridate")
if(!require(tidyverse)) install.packages("tidyverse")


# 2. Set locale (for weekdays) --------------------------------------------
Sys.setlocale("LC_TIME", "French_Belgium.1252")


# 2. Create database -----------------------------------------------------
### set seed for reproducible results
set.seed(2001)

df_temp <- tibble(the_transaction_id = paste("trans", seq(1,250000), sep = "_"),
             but_idr_business_unit = as.factor(sample(c("Evere", "Wavre", "Anderlecht", "Antwerp", "Charleroi", 
                                              "Liège"), 250000, replace = T,
                                            prob = c(0.25, 0.15, 0.15, 0.15, 0.15, 0.15))),
             tdt_type_detail = sample(c("sale", "return"), 250000, replace = T, prob = c(0.80, 0.20)),
             the_date_transaction = sample(seq(as.Date("2019/01/01"), as.Date("2019/12/31"), by = "day"),
                                           250000, replace = T),
             sku_idr_sku = as.factor(paste(sample(c("Tennis", "Football", "Running", "Biking", 
                                                    "Other"), 250000, replace = T,
                                                  prob = c(0.15, 0.30, 0.20, 0.20, 0.15)),
                                           sample(c(1:500), replace = T), sep = "_")))

new_prices <- df_temp %>%
  distinct(but_idr_business_unit, sku_idr_sku) %>%
  mutate(unit_price = abs(round(rnorm(n(), mean = 30, sd = 90), 2)))

df <- df_temp %>%
  left_join(new_prices, by = c("but_idr_business_unit", "sku_idr_sku")) %>%
  mutate(quantity = sample(seq(1:5), 250000, replace = T),
         turnover = quantity * unit_price) %>%
  select(-unit_price) %>%
  mutate(weekdays = weekdays(the_date_transaction)) %>%
  filter(weekdays != c("dimanche")) %>%
  select(-weekdays)



  

# 3. Plotly ------------------------------------------------------------------

store_colors <- c(Anderlecht = "purple", Antwerp = "blue", Charleroi = "black", Evere = "orange",
                  Liège = "red", Wavre = "green")

#plot of turnovers per week

df_plotly_1 <- df %>%
  filter(tdt_type_detail == "sale") %>%
  group_by(the_date_transaction, but_idr_business_unit) %>%
  summarize(turnover_date = sum(turnover)) %>%
  ungroup()

df_plotly_1 %>%
  plot_ly(x = ~the_date_transaction, y= ~turnover_date) %>%
  add_lines(color = ~but_idr_business_unit, colors = store_colors) %>%
  layout(xaxis = 
           list(
             rangeselector = list(
               buttons = list(
                 list(
                   count = 7,
                   label = "1 week",
                   step = "day",
                   stepmode = "backward"),
                 list(
                   count = 10,
                   label = "10 days",
                   step = "day",
                   stepmode = "backward"),
                 list(
                   count = 14,
                   label = "2 weeks",
                   step = "day",
                   stepmode = "backward"),
                 list(
                   count = 1,
                   label = "1 month",
                   step = "month",
                   stepmode = "backward"),
                 list(step = "all"))),
             rangeslider = list(type = "date")),
         yaxis = list(title = "Turnover (sales only)"))


#Plot 2
df_plotly_2 <- df %>%
  filter(tdt_type_detail == "sale") %>%
  mutate(sport = factor(str_replace_all(sku_idr_sku, "_\\d+", ""), levels = c("Biking", "Football",
                                                                              "Running", "Tennis", 
                                                                              "Other"))) %>%
  count(sport, but_idr_business_unit) %>%
  pivot_wider(names_from = but_idr_business_unit, values_from = n)

df_plotly_2 %>%
  plot_ly(x = ~sport) %>% 
  add_trace(y = ~Anderlecht, name = "Anderlecht", type = "bar", visible = T, 
            color = I("purple"), text = ~Anderlecht, textposition = "outside") %>%
  add_trace(y = ~Antwerp, name = "Antwerp", type = "bar", visible = F, 
            color = I("darkblue"), text = ~Antwerp, textposition = "outside") %>%
  add_trace(y = ~Charleroi, name = "Charleroi", type = "bar", visible = F, 
            color = I("black"), text = ~Charleroi, textposition = "outside") %>%
  add_trace(y = ~Evere, name = "Evere", type = "bar", visible = F, 
            color = I("orange"), text = ~Evere, textposition = "outside") %>%
  add_trace(y = ~Liège, name = "Liège", type = "bar", visible = F, 
            color = I("red"), text = ~Liège, textposition = "outside") %>%
  add_trace(y = ~Wavre, name = "Wavre", type = "bar", visible = F, 
            color = I("darkgreen"), text = ~Wavre, textposition = "outside") %>%
  layout(
    yaxis = list(title = "Items Sold"),
    xaxis = list(title = "Sport"),
    updatemenus = list(
      list(
        type = "list", 
        label = "Store",
        buttons = list(
          list(method = "restyle",
               args = list("visible", c(T,F,F,F,F,F)),
               label = "Anderlecht"),
          list(method = "restyle",
               args = list("visible", c(F,T,F,F,F,F)),
               label = "Antwerp"),
          list(method = "restyle",
               args = list("visible", c(F,F,T,F,F,F)),
               label = "Charleroi"),
          list(method = "restyle",
               args = list("visible", c(F,F,F,T,F,F)),
               label = "Evere"),
          list(method = "restyle",
               args = list("visible", c(F,F,F,F,T,F)),
               label = "Liège"),
          list(method = "restyle",
               args = list("visible", c(F,F,F,F,F,T)),
               label = "Wavre"))))) #auto argument for text doesn't work see
#https://github.com/plotly/plotly.js/issues/3115

#plot 3



  