
# 1. Load the required pacakges ----------------------------------------------
if(!require(shiny)) install.packages("shiny")
if(!require(plotly)) install.packages("plotly")
if(!require(lubridate)) install.packages("lubridate")
if(!require(tidyverse)) install.packages("tidyverse")

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

plot_ly(df_plotly_1, x = ~the_date_transaction, y= ~turnover_date) %>%
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

#plot with number of items sold per sport per store
df_plotly_2 <- df %>%
  filter(tdt_type_detail == "sale") %>%
  mutate(sport = factor(str_replace_all(sku_idr_sku, "_\\d+", ""), levels = c("Biking", "Football",
                                                                              "Running", "Tennis", 
                                                                              "Other"))) %>%
  count(sport, but_idr_business_unit)

df_plotly_2 %>%
  plot_ly(x = ~sport, y = ~n, text = ~~but_idr_business_unit,
        type = "bar",
        transforms = list(
          list(
            type = "filter", 
            target = ~but_idr_business_unit,
            operation = "=",
            value = unique(df_plotly_2$but_idr_business_unit)[1]
          )
        )) %>%
  layout(
    updatemenus = list(
      list(
        type = "dropdown",
        active = 0,
        buttons = list(
          list(method = "update",
               args = list("transform[0].value", unique(df_plotly_2$but_idr_business_unit)[1]),
               label = unique(df_plotly_2$but_idr_business_unit)[1],
               value = unique(df_plotly_2$but_idr_business_unit)[1]),
          list(method = "update",
               args = list("transform[0].value", unique(df_plotly_2$but_idr_business_unit)[2]),
               label = unique(df_plotly_2$but_idr_business_unit)[2],
               value = unique(df_plotly_2$but_idr_business_unit)[2]),
          list(method = "update",
               args = list("transform[0].value", unique(df_plotly_2$but_idr_business_unit)[3]),
               label = unique(df_plotly_2$but_idr_business_unit)[3],
               value = unique(df_plotly_2$but_idr_business_unit)[3]),
          list(method = "update",
               args = list("transform[0].value", unique(df_plotly_2$but_idr_business_unit)[4]),
               label = unique(df_plotly_2$but_idr_business_unit)[4],
               value = unique(df_plotly_2$but_idr_business_unit)[4]),
          list(method = "update",
               args = list("transform[0].value", unique(df_plotly_2$but_idr_business_unit)[5]),
               label = unique(df_plotly_2$but_idr_business_unit)[5],
               value = unique(df_plotly_2$but_idr_business_unit)[5]),
          list(method = "update",
               args = list("transform[0].value", unique(df_plotly_2$but_idr_business_unit)[6]),
               label = unique(df_plotly_2$but_idr_business_unit)[6],
               value = unique(df_plotly_2$but_idr_business_unit)[6])
        )
      )
    )
  )

#violin plot per day, per store

df_plotly_3 <- df %>%
  mutate(days = weekdays(the_date_transaction))

df_plotly_3 %>%
  plot_ly(type = "violin") %>%
  add_trace(x = ~days[df_plotly_3$tdt_type_detail == "sale"], 
            y = ~turnover[df_plotly_3$tdt_type_detail == "sale"],
            legendgroup = "Sale",
            scalegroup = "Sale",
            name = "Sale",
            side = "positive",
            meanline = list(visible = T),
            color = I("green")) %>%
  add_trace(x = ~days[df_plotly_3$tdt_type_detail == "return"], 
            y = ~turnover[df_plotly_3$tdt_type_detail == "return"], 
            legendgroup = "Return",
            scalegroup = "Return",
            name = "Return",
            side = "negative",
            meanline = list(visible = T),
            color = I("red"))
  

stores <- split(df_plotly_3, df_plotly_3$but_idr_business_unit)

plots <- lapply(stores, function(l){
  l %>%
    plot_ly(type = "violin") %>%
    add_trace(x = ~days[l$tdt_type_detail == "sale"], 
              y = ~turnover[l$tdt_type_detail == "sale"],
              legendgroup = "sale",
              scalegroup = "sale",
              name = "sale",
              side = "positive",
              box = list(visible = T), meanline = list(visible = T),
              color = I("green")) %>%
    add_trace(x = ~days[l$tdt_type_detail == "return"], 
              y = ~turnover[l$tdt_type_detail == "return"], 
              legendgroup = "return",
              scalegroup = "return",
              name = "return",
              side = "negative",
              box = list(visible = T), meanline = list(visible = T),
              color = I("red"))
})


subplot(plots, nrows = 1, shareY = T)


# 4. Shiny Set Up ---------------------------------------------------------

   





  