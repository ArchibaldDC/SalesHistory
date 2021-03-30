# 1. Load the required pacakges ----------------------------------------------
if (!require(shiny))
  install.packages("shiny") #for shiny app
if (!require(writexl))
  install.packages("writexl") # to write xlsx file
if (!require(plotly))
  install.packages("plotly") #for plots
if (!require(lubridate))
  install.packages("lubridate") #for date formats
if (!require(thematic))
  install.packages("thematic") # themes in shiny
if (!require(bslib))
  install.packages("bslib") #themes in shiny
if (!require(tidyverse))
  install.packages("tidyverse") #tidyverse for data manipulation



# 2. Create database -----------------------------------------------------
### set seed for reproducible results
set.seed(2001)
### Create database in different steps, sample() is used to provide randomness

df_temp <-
  tibble(
    the_transaction_id = paste("trans", seq(1, 250000), sep = "_"),
    but_idr_business_unit = as.factor(sample(
      c("Evere", "Wavre", "Anderlecht", "Antwerp", "Charleroi",
        "Liege"),
      250000,
      replace = T,
      prob = sample(seq(0.05, 0.95, by = 0.05), 6)
    )),
    tdt_type_detail = as.factor(sample(
      c("Sale", "Return"),
      250000,
      replace = T,
      prob = c(0.80, 0.20)
    )),
    the_date_transaction = sample(seq(
      as.Date("2020/01/01"), as.Date("2020/12/31"), by = "day"
    ),
    250000, replace = T),
    sku_idr_sku = as.factor(paste(
      sample(
        c("Tennis", "Football", "Running", "Biking",
          "Other"),
        250000,
        replace = T,
        prob = sample(seq(0.10, 0.90, by = 0.10), 5)
      ),
      sample(c(1:500), replace = T),
      sep = "_"
    ))
  )

###rnorm is used to provide a normal distribution of unit prices
new_prices <- df_temp %>%
  distinct(but_idr_business_unit, sku_idr_sku) %>%
  mutate(unit_price = abs(round(rnorm(
    #create unit price variable but removed afterwards
    n(), mean = 30, sd = 90
  ), 2)))

### final dataframe with sundays removed as stores are closed & unit price removed
df <- df_temp %>%
  left_join(new_prices, by = c("but_idr_business_unit", "sku_idr_sku")) %>%
  mutate(quantity = sample(seq(1:5), 250000, replace = T),
         turnover = round((quantity * unit_price), 2)) %>%
  select(-unit_price) %>%
  mutate(
    weekdays = wday(
      the_date_transaction,
      week_start = 1,
      locale = Sys.getlocale("LC_TIME"),
      #set locale to filter out sundays (dimanche), possibly need admin authorization to change locale. LC_Time locale is set to French_Belgium.1252
      label = T,
      abbr = F
    )
  ) %>%
  filter(weekdays != "dimanche") %>% #day of the week might be different depending on your locale settings
  select(-weekdays)



# 3. Plotly ------------------------------------------------------------------
### create custom colors
store_colors <-
  c(
    Anderlecht = "#59C7EB",
    Antwerp = "#FEA090",
    Charleroi = "#9AA0A7",
    Evere = "#077187",
    Liege = "#8E2043",
    Wavre = "#EFDC60"
  )

sport_colors <-
  c(
    Biking = "#696969",
    Football = "#32CD32",
    Running = "#1E90FF",
    Tennis = "#B22222",
    Other = "#DAA520"
  )


#plot 1
df_plotly_1 <- df %>%
  filter(tdt_type_detail == "Sale") %>%
  group_by(the_date_transaction, but_idr_business_unit) %>%
  summarize(turnover_date = sum(turnover)) %>%
  ungroup()


plot_1 <- df_plotly_1 %>%
  plot_ly(x = ~ the_date_transaction, y = ~ turnover_date) %>%
  add_lines(color = ~ but_idr_business_unit, colors = store_colors) %>%
  layout(
    xaxis =
      list(
        title = "Date (Move slider to explore ranges)",
        rangeselector = list(buttons = list(
          list(
            count = 7,
            label = "1 week",
            step = "day",
            stepmode = "backward"
          ),
          list(
            count = 10,
            label = "10 days",
            step = "day",
            stepmode = "backward"
          ),
          list(
            count = 14,
            label = "2 weeks",
            step = "day",
            stepmode = "backward"
          ),
          list(
            count = 1,
            label = "1 month",
            step = "month",
            stepmode = "backward"
          ),
          list(step = "all")
        )),
        rangeslider = list(type = "date")
      ),
    yaxis = list(title = "Turnover (Sales only)")
  )

#Plot 2
df_plotly_2 <- df %>%
  filter(tdt_type_detail == "Sale") %>%
  mutate(sport = factor(
    str_replace_all(sku_idr_sku, "_\\d+", ""),
    levels = c("Biking",
               "Football",
               "Running",
               "Tennis",
               "Other")
  ))

df_plotly_2 <-
  df_plotly_2[rep(1:nrow(df_plotly_2), df_plotly_2$quantity), ]

df_plotly_2 <- df_plotly_2 %>%
  group_by(but_idr_business_unit, sport) %>%
  mutate(items_sold = sum(quantity)) %>%
  ungroup()

df_plotly_2 <-
  df_plotly_2[rep(1:nrow(df_plotly_2), df_plotly_2$quantity), ]


df_plotly_2 <- df_plotly_2 %>%
  count(sport, but_idr_business_unit) %>%
  pivot_wider(names_from = but_idr_business_unit, values_from = n)

plot_2 <- df_plotly_2 %>%
  plot_ly(x = ~ sport) %>%
  add_trace(
    y = ~ Anderlecht,
    name = "Anderlecht",
    type = "bar",
    visible = T,
    color = I("purple"),
    text = ~ Anderlecht,
    textposition = "outside"
  ) %>%
  add_trace(
    y = ~ Antwerp,
    name = "Antwerp",
    type = "bar",
    visible = F,
    color = I("darkblue"),
    text = ~ Antwerp,
    textposition = "outside"
  ) %>%
  add_trace(
    y = ~ Charleroi,
    name = "Charleroi",
    type = "bar",
    visible = F,
    color = I("black"),
    text = ~ Charleroi,
    textposition = "outside"
  ) %>%
  add_trace(
    y = ~ Evere,
    name = "Evere",
    type = "bar",
    visible = F,
    color = I("orange"),
    text = ~ Evere,
    textposition = "outside"
  ) %>%
  add_trace(
    y = ~ Liege,
    name = "Liege",
    type = "bar",
    visible = F,
    color = I("red"),
    text = ~ Liege,
    textposition = "outside"
  ) %>%
  add_trace(
    y = ~ Wavre,
    name = "Wavre",
    type = "bar",
    visible = F,
    color = I("darkgreen"),
    text = ~ Wavre,
    textposition = "outside"
  ) %>%
  layout(
    yaxis = list(title = "Items Sold"),
    xaxis = list(title = "Sport",
                 tickformat = "digits"),
    updatemenus = list(list(
      type = "list",
      label = "Store",
      buttons = list(
        list(
          method = "restyle",
          args = list("visible", c(T, F, F, F, F, F)),
          label = "Anderlecht"
        ),
        list(
          method = "restyle",
          args = list("visible", c(F, T, F, F, F, F)),
          label = "Antwerp"
        ),
        list(
          method = "restyle",
          args = list("visible", c(F, F, T, F, F, F)),
          label = "Charleroi"
        ),
        list(
          method = "restyle",
          args = list("visible", c(F, F, F, T, F, F)),
          label = "Evere"
        ),
        list(
          method = "restyle",
          args = list("visible", c(F, F, F, F, T, F)),
          label = "Liege"
        ),
        list(
          method = "restyle",
          args = list("visible", c(F, F, F, F, F, T)),
          label = "Wavre"
        )
      )
    ))
  ) #auto argument for text doesn't work see
#https://github.com/plotly/plotly.js/issues/3115

#plot3
df_plotly_3 <- df %>%
  mutate(months = month(the_date_transaction, abbr = F, label = T)) %>%
  group_by(months, but_idr_business_unit, tdt_type_detail) %>%
  summarize(total_turnover_month_store = round(sum(turnover), 0)) %>%
  ungroup() %>%
  group_by(but_idr_business_unit, tdt_type_detail) %>%
  mutate(total_turnover_store = round(sum(total_turnover_month_store), 0)) %>%
  ungroup() %>%
  mutate(Sale_Return_proportion = (total_turnover_month_store / total_turnover_store) *
           100)


plot_3 <- df_plotly_3 %>%
  plot_ly() %>%
  add_trace(
    x = ~ months[df_plotly_3$tdt_type_detail == "Sale" &
                   df_plotly_3$but_idr_business_unit == "Anderlecht"],
    y = ~ total_turnover_month_store[df_plotly_3$tdt_type_detail == "Sale" &
                                       df_plotly_3$but_idr_business_unit == "Anderlecht"],
    type = "scatter",
    mode = "lines+markers",
    name = "Sales Turnover",
    color = I("darkgreen"),
    visible = T
  ) %>%
  add_trace(
    x = ~ months[df_plotly_3$tdt_type_detail == "Return" &
                   df_plotly_3$but_idr_business_unit == "Anderlecht"],
    y = ~ total_turnover_month_store[df_plotly_3$tdt_type_detail == "Return" &
                                       df_plotly_3$but_idr_business_unit == "Anderlecht"],
    type = "scatter",
    mode = "lines+markers",
    name = "Return Turnover",
    color = I("darkred"),
    visible = T
  ) %>%
  add_trace(
    x = ~ months[df_plotly_3$tdt_type_detail == "Sale" &
                   df_plotly_3$but_idr_business_unit == "Antwerp"],
    y = ~ total_turnover_month_store[df_plotly_3$tdt_type_detail == "Sale" &
                                       df_plotly_3$but_idr_business_unit == "Antwerp"],
    type = "scatter",
    mode = "lines+markers",
    name = "Sales Turnover",
    color = I("darkgreen"),
    visible = F
  ) %>%
  add_trace(
    x = ~ months[df_plotly_3$tdt_type_detail == "Return" &
                   df_plotly_3$but_idr_business_unit == "Antwerp"],
    y = ~ total_turnover_month_store[df_plotly_3$tdt_type_detail == "Return" &
                                       df_plotly_3$but_idr_business_unit == "Antwerp"],
    type = "scatter",
    mode = "lines+markers",
    name = "Return Turnover",
    color = I("darkred"),
    visible = F
  ) %>%
  add_trace(
    x = ~ months[df_plotly_3$tdt_type_detail == "Sale" &
                   df_plotly_3$but_idr_business_unit == "Charleroi"],
    y = ~ total_turnover_month_store[df_plotly_3$tdt_type_detail == "Sale" &
                                       df_plotly_3$but_idr_business_unit == "Charleroi"],
    type = "scatter",
    mode = "lines+markers",
    name = "Sales Turnover",
    color = I("darkgreen"),
    visible = F
  ) %>%
  add_trace(
    x = ~ months[df_plotly_3$tdt_type_detail == "Return" &
                   df_plotly_3$but_idr_business_unit == "Charleroi"],
    y = ~ total_turnover_month_store[df_plotly_3$tdt_type_detail == "Return" &
                                       df_plotly_3$but_idr_business_unit == "Charleroi"],
    type = "scatter",
    mode = "lines+markers",
    name = "Return Turnover",
    color = I("darkred"),
    visible = F
  ) %>%
  add_trace(
    x = ~ months[df_plotly_3$tdt_type_detail == "Sale" &
                   df_plotly_3$but_idr_business_unit == "Evere"],
    y = ~ total_turnover_month_store[df_plotly_3$tdt_type_detail == "Sale" &
                                       df_plotly_3$but_idr_business_unit == "Evere"],
    type = "scatter",
    mode = "lines+markers",
    name = "Sales Turnover",
    color = I("darkgreen"),
    visible = F
  ) %>%
  add_trace(
    x = ~ months[df_plotly_3$tdt_type_detail == "Return" &
                   df_plotly_3$but_idr_business_unit == "Evere"],
    y = ~ total_turnover_month_store[df_plotly_3$tdt_type_detail == "Return" &
                                       df_plotly_3$but_idr_business_unit == "Evere"],
    type = "scatter",
    mode = "lines+markers",
    name = "Return Turnover",
    color = I("darkred"),
    visible = F
  ) %>%
  add_trace(
    x = ~ months[df_plotly_3$tdt_type_detail == "Sale" &
                   df_plotly_3$but_idr_business_unit == "Liege"],
    y = ~ total_turnover_month_store[df_plotly_3$tdt_type_detail == "Sale" &
                                       df_plotly_3$but_idr_business_unit == "Liege"],
    type = "scatter",
    mode = "lines+markers",
    name = "Sales Turnover",
    color = I("darkgreen"),
    visible = F
  ) %>%
  add_trace(
    x = ~ months[df_plotly_3$tdt_type_detail == "Return" &
                   df_plotly_3$but_idr_business_unit == "Liege"],
    y = ~ total_turnover_month_store[df_plotly_3$tdt_type_detail == "Return" &
                                       df_plotly_3$but_idr_business_unit == "Liege"],
    type = "scatter",
    mode = "lines+markers",
    name = "Return Turnover",
    color = I("darkred"),
    visible = F
  ) %>%
  add_trace(
    x = ~ months[df_plotly_3$tdt_type_detail == "Sale" &
                   df_plotly_3$but_idr_business_unit == "Wavre"],
    y = ~ total_turnover_month_store[df_plotly_3$tdt_type_detail == "Sale" &
                                       df_plotly_3$but_idr_business_unit == "Wavre"],
    type = "scatter",
    mode = "lines+markers",
    name = "Sales Turnover",
    color = I("darkgreen"),
    visible = F
  ) %>%
  add_trace(
    x = ~ months[df_plotly_3$tdt_type_detail == "Return" &
                   df_plotly_3$but_idr_business_unit == "Wavre"],
    y = ~ total_turnover_month_store[df_plotly_3$tdt_type_detail == "Return" &
                                       df_plotly_3$but_idr_business_unit == "Wavre"],
    type = "scatter",
    mode = "lines+markers",
    name = "Return Turnover",
    color = I("darkred"),
    visible = F
  ) %>%
  layout(
    xaxis = list(title = "Month"),
    yaxis = list(title = "Turnover (€)"),
    updatemenus = list(list(
      type = "list",
      label = "Store",
      buttons = list(
        list(
          method = "restyle",
          args = list("visible", c(T, T, F, F, F, F, F, F, F, F, F, F)),
          label = "Anderlecht"
        ),
        list(
          method = "restyle",
          args = list("visible", c(F, F, T, T, F, F, F, F, F, F, F, F)),
          label = "Antwerp"
        ),
        list(
          method = "restyle",
          args = list("visible", c(F, F, F, F, T, T, F, F, F, F, F, F)),
          label = "Charleroi"
        ),
        list(
          method = "restyle",
          args = list("visible", c(F, F, F, F, F, F, T, T, F, F, F, F)),
          label = "Evere"
        ),
        list(
          method = "restyle",
          args = list("visible", c(F, F, F, F, F, F, F, F, T, T, F, F)),
          label = "Liege"
        ),
        list(
          method = "restyle",
          args = list("visible", c(F, F, F, F, F, F, F, F, F, F, T, T)),
          label = "Wavre"
        )
      )
    ))
  )

#extra plot
df_plotly_4 <- df %>%
  mutate(weekdays = weekdays(the_date_transaction))

plot_4 <- df_plotly_4 %>%
  plot_ly() %>%
  add_trace(
    x = ~ weekdays[df_plotly_4$tdt_type_detail == "Sale" &
                     df_plotly_4$but_idr_business_unit == "Anderlecht"],
    y = ~ turnover[df_plotly_4$tdt_type_detail == "Sale" &
                     df_plotly_4$but_idr_business_unit == "Anderlecht"],
    type = "violin",
    side = "negative",
    name = "Sales Turnover",
    color = I("darkgreen"),
    visible = T,
    box = list(visible = T),
    meanline = list(visible = T)
  ) %>%
  add_trace(
    x = ~ weekdays[df_plotly_4$tdt_type_detail == "Return" &
                     df_plotly_4$but_idr_business_unit == "Anderlecht"],
    y = ~ turnover[df_plotly_4$tdt_type_detail == "Return" &
                     df_plotly_4$but_idr_business_unit == "Anderlecht"],
    type = "violin",
    side = "positive",
    name = "Return Turnover",
    color = I("darkred"),
    visible = T,
    box = list(visible = T),
    meanline = list(visible = T)
  ) %>%
  add_trace(
    x = ~ weekdays[df_plotly_4$tdt_type_detail == "Sale" &
                     df_plotly_4$but_idr_business_unit == "Antwerp"],
    y = ~ turnover[df_plotly_4$tdt_type_detail == "Sale" &
                     df_plotly_4$but_idr_business_unit == "Antwerp"],
    type = "violin",
    side = "negative",
    name = "Sales Turnover",
    color = I("darkgreen"),
    visible = F,
    box = list(visible = T),
    meanline = list(visible = T)
  ) %>%
  add_trace(
    x = ~ weekdays[df_plotly_4$tdt_type_detail == "Return" &
                     df_plotly_4$but_idr_business_unit == "Antwerp"],
    y = ~ turnover[df_plotly_4$tdt_type_detail == "Return" &
                     df_plotly_4$but_idr_business_unit == "Antwerp"],
    type = "violin",
    side = "positive",
    name = "Return Turnover",
    color = I("darkred"),
    visible = F,
    box = list(visible = T),
    meanline = list(visible = T)
  ) %>%
  add_trace(
    x = ~ weekdays[df_plotly_4$tdt_type_detail == "Sale" &
                     df_plotly_4$but_idr_business_unit == "Charleroi"],
    y = ~ turnover[df_plotly_4$tdt_type_detail == "Sale" &
                     df_plotly_4$but_idr_business_unit == "Charleroi"],
    type = "violin",
    side = "negative",
    name = "Sales Turnover",
    color = I("darkgreen"),
    visible = F,
    box = list(visible = T),
    meanline = list(visible = T)
  ) %>%
  add_trace(
    x = ~ weekdays[df_plotly_4$tdt_type_detail == "Return" &
                     df_plotly_4$but_idr_business_unit == "Charleroi"],
    y = ~ turnover[df_plotly_4$tdt_type_detail == "Return" &
                     df_plotly_4$but_idr_business_unit == "Charleroi"],
    type = "violin",
    side = "positive",
    name = "Return Turnover",
    color = I("darkred"),
    visible = F,
    box = list(visible = T),
    meanline = list(visible = T)
  ) %>%
  add_trace(
    x = ~ weekdays[df_plotly_4$tdt_type_detail == "Sale" &
                     df_plotly_4$but_idr_business_unit == "Evere"],
    y = ~ turnover[df_plotly_4$tdt_type_detail == "Sale" &
                     df_plotly_4$but_idr_business_unit == "Evere"],
    type = "violin",
    side = "negative",
    name = "Sales Turnover",
    color = I("darkgreen"),
    visible = F,
    box = list(visible = T),
    meanline = list(visible = T)
  ) %>%
  add_trace(
    x = ~ weekdays[df_plotly_4$tdt_type_detail == "Return" &
                     df_plotly_4$but_idr_business_unit == "Evere"],
    y = ~ turnover[df_plotly_4$tdt_type_detail == "Return" &
                     df_plotly_4$but_idr_business_unit == "Evere"],
    type = "violin",
    side = "positive",
    name = "Return Turnover",
    color = I("darkred"),
    visible = F,
    box = list(visible = T),
    meanline = list(visible = T)
  ) %>%
  add_trace(
    x = ~ weekdays[df_plotly_4$tdt_type_detail == "Sale" &
                     df_plotly_4$but_idr_business_unit == "Liege"],
    y = ~ turnover[df_plotly_4$tdt_type_detail == "Sale" &
                     df_plotly_4$but_idr_business_unit == "Liege"],
    type = "violin",
    side = "negative",
    name = "Sales Turnover",
    color = I("darkgreen"),
    visible = F,
    box = list(visible = T),
    meanline = list(visible = T)
  ) %>%
  add_trace(
    x = ~ weekdays[df_plotly_4$tdt_type_detail == "Return" &
                     df_plotly_4$but_idr_business_unit == "Liege"],
    y = ~ turnover[df_plotly_4$tdt_type_detail == "Return" &
                     df_plotly_4$but_idr_business_unit == "Liege"],
    type = "violin",
    side = "positive",
    name = "Return Turnover",
    color = I("darkred"),
    visible = F,
    box = list(visible = T),
    meanline = list(visible = T)
  ) %>%
  add_trace(
    x = ~ weekdays[df_plotly_4$tdt_type_detail == "Sale" &
                     df_plotly_4$but_idr_business_unit == "Wavre"],
    y = ~ turnover[df_plotly_4$tdt_type_detail == "Sale" &
                     df_plotly_4$but_idr_business_unit == "Wavre"],
    type = "violin",
    side = "negative",
    name = "Sales Turnover",
    color = I("darkgreen"),
    visible = F,
    box = list(visible = T),
    meanline = list(visible = T)
  ) %>%
  add_trace(
    x = ~ weekdays[df_plotly_4$tdt_type_detail == "Return" &
                     df_plotly_4$but_idr_business_unit == "Wavre"],
    y = ~ turnover[df_plotly_4$tdt_type_detail == "Return" &
                     df_plotly_4$but_idr_business_unit == "Wavre"],
    type = "violin",
    side = "positive",
    name = "Return Turnover",
    color = I("darkred"),
    visible = F,
    box = list(visible = T),
    meanline = list(visible = T)
  ) %>%
  layout(
    xaxis = list(title = "Month"),
    yaxis = list(title = "Turnover (€)"),
    updatemenus = list(list(
      type = "list",
      label = "Store",
      buttons = list(
        list(
          method = "restyle",
          args = list("visible", c(T, T, F, F, F, F, F, F, F, F, F, F)),
          label = "Anderlecht"
        ),
        list(
          method = "restyle",
          args = list("visible", c(F, F, T, T, F, F, F, F, F, F, F, F)),
          label = "Antwerp"
        ),
        list(
          method = "restyle",
          args = list("visible", c(F, F, F, F, T, T, F, F, F, F, F, F)),
          label = "Charleroi"
        ),
        list(
          method = "restyle",
          args = list("visible", c(F, F, F, F, F, F, T, T, F, F, F, F)),
          label = "Evere"
        ),
        list(
          method = "restyle",
          args = list("visible", c(F, F, F, F, F, F, F, F, T, T, F, F)),
          label = "Liege"
        ),
        list(
          method = "restyle",
          args = list("visible", c(F, F, F, F, F, F, F, F, F, F, T, T)),
          label = "Wavre"
        )
      )
    ))
  )
