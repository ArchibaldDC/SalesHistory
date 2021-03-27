# 1. Load the required pacakges ----------------------------------------------
if (!require(shiny))
  install.packages("shiny") 
if (!require(writexl))
  install.packages("writexl") 
if (!require(plotly))
  install.packages("plotly")
if (!require(lubridate))
  install.packages("lubridate")
if (!require(thematic))
  install.packages("thematic")
if (!require(bslib))
  install.packages("bslib")
if (!require(unikn))
  install.packages("unikn")
if (!require(tidyverse))
  install.packages("tidyverse")



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
      prob = c(0.30, 0.15, 0.20, 0.10, 0.25, 0.20)
    )),
    tdt_type_detail = as.factor(sample(
      c("sale", "return"),
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
        replace = T
      ),
      sample(c(1:500), replace = T), sep = "_"
    ))
  )

###rnorm is used to provide a normal distribution of the prices
new_prices <- df_temp %>%
  distinct(but_idr_business_unit, sku_idr_sku) %>%
  mutate(unit_price = abs(round(rnorm(
    n(), mean = 30, sd = 90
  ), 2)))

### final dataframe with sundays removed as stores are closed
df <- df_temp %>%
  left_join(new_prices, by = c("but_idr_business_unit", "sku_idr_sku")) %>%
  mutate(quantity = sample(seq(1:5), 250000, replace = T),
         turnover = quantity * unit_price) %>%
  select(-unit_price) %>%
  mutate(
    weekdays = wday(
      the_date_transaction,
      week_start = 1,
      locale = Sys.getlocale("LC_TIME"),
      label = T,
      abbr = F
    )
  ) %>%
  filter(weekdays != c("dimanche")) %>%
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
    Tennis = "#B22222",
    Football = "#32CD32",
    Running = "#1E90FF",
    Biking = "#696969",
    Other = "#DAA520"
  )


### create plotly dataframes


###
df2 <- df[rep(1:nrow(df), df$quantity),]



###

# 4. Shiny App ------------------------------------------------------------




# Define UI = Lets users interact with the APP
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "cerulean"),
  img(
    src = "logo.png",
    height = 140,
    width = 400
  ),
  titlePanel(strong("Sales History App")),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      p("You can set your own filters with the parameters below. The plots and dataset will automatically be adjusted."),
      checkboxGroupInput(
        "store",
        label = "Choose a store",
        choices = c("Anderlecht", "Antwerp", "Charleroi", "Evere", "Liege", "Wavre"),
        selected = c("Anderlecht", "Antwerp", "Charleroi", "Evere", "Liege", "Wavre")
      ),
      dateRangeInput(
        "date_range",
        "Select a date range (yyyy-mm-dd)",
        start = "2020/01/01",
        end = "2020/12/31",
        format = "yyyy-mm-dd",
        weekstart = 1,
        language = "fr",
        min = "2020/01/01",
        max = "2020/12/31"
      ),
      radioButtons(
        "file_type",
        "Format",
        choices = c("excel (.xslx)" = "xlsx",
                    "csv (.csv)" = "csv"),
        selected = "csv"
      ),
      downloadButton("download_filtered", "Download"),
      h1("Original Dataset of Sales History"),
      h2("Overview"),
      h3("Dataset variables/columns"),
      p(
        "The dataset simulates the sales and returns turnover numbers for a sport-supply store for the year 2020. the dataset contains 214266 rows (observations) and 7 columns (variables).",
        em(
          "It should be noted that Sundays have not been included in the dataset, as stores are generally closed that day."
        )
      ),
      br(),
      p(
        "- ",
        strong("transaction_id:"),
        "the name of the transaction and serves as",
        em("the Primary Key"),
        "for this dataset, it is assumed that for every transaction, a customer buys only one item,
                   but maybe more than once as the other variables/columns of the dataset will show."
      ),
      br(),
      p("- ",
        strong("but_idr_business_unit:"),
        "the name of the store."),
      br(),
      p(
        "- ",
        strong("tdt_type_detail:"),
        "Shows if the transaction was a sale or a return."
      ),
      br(),
      p(
        "- ",
        strong("the_date_transaction:"),
        "the date of the transaction."
      ),
      br(),
      p(
        "- ",
        strong("sku_idr_sku:"),
        "the ID of the item that was sold, also shows in which category it belongs."
      ),
      br(),
      p("- ",
        strong("quantity:"),
        "amount of items sold or returned."),
      br(),
      p(
        "- ",
        strong("turnover:"),
        "the total turnover of the transaction/sale.",
        strong("IMPORTANT:"),
        "the turnover variable is the unit price x quantity, per request,
      the unit price is not displayed in the dataset",
      strong("but"),
      "can be found in the code that created the dataset."
      )
    ),
    mainPanel(
      h1("Introduction"),
      br(),
      p(
        strong(
          "Below you'll find the plots with a brief explanation above each one, the dataset is provided at the bottom of the page."
        )
      ),
      br(),
      h2("Plots"),
      br(),
      p(
        "This app is accompanied by the required three plots, plus one additional plot, you can browse with your mouse over the plot to reveal additional information.",
        strong(
          "All plots are based on the original dataframe provided below the plots,
               the dataset was tranformed/mutated visualize."
        ),
        "The plots can also be filtered by clicking on the legend labels."
      ),
      br(),
      h3("Daily Sales"),
      p(
        "The first plot shows the daily sale history throughout the year, only sales, not returns,
        though it might appear messy at first, you can play with the date range on the x-axis to visualize smaller time periods, there's also an extra menu provided above.
        Additionally, browsing over the plot also allows you to compare the turnover numbers for all the stores on the same day."
      ),
      plotlyOutput("plot1"),
      
      br(),
      h3("Items Sold Per Store"),
      p(
        "The second plot displays how many items per sport have been sold, the number of transaction is included in the calculations
        (if the item named Tennis_13 was sold four times, it will be counted as four separate entities, not one).
        The piechart below shows the proportion of items sold."
      ),
      
      plotlyOutput("plot2"),
      br(),
      plotlyOutput("plot3"),
      
      
      br(),
      h3("Violin Plots of Sales vs. Returns for weekdays"),
      p(
        "A violin plot shows the entire distribution of the data, the advantage of the violin plot is that
        Wider sections of the violin plot represent a higher probability of a given sales/return turnover amount to occur; the skinnier sections represent a lower probability.
        The dots represent extreme, exceptional values, you can browse with your mouse through the plot to show
        additional information such as minimum values, maximum values, mean, median,..."
      ),
      br(),
      p(
        "In this plot, you can see the probability distribution of the different months."
      ),
      
      plotlyOutput("plot4"),
      
      br(),
      h2("Dataset"),
      
      dataTableOutput("trans_data"),
    )
  )
)



# Define server logic = Part of the APP that is reactive and DOES something
server <- function(input, output) {
  thematic::thematic_shiny()
  
  data_filtered <- reactive({ #make df reactove to inputs from user
    df %>%
      filter(
        .,
        between(
          the_date_transaction,
          input$date_range[1],
          input$date_range[2]
        ),
        but_idr_business_unit %in% input$store
      )
  })
  
  
  output$trans_data <- renderDataTable({
    data_filtered()
  })
  
  output$download_filtered <- downloadHandler( #download in csv or excel format
    filename = function() {
      str_c(Sys.Date(),
            "_transaction_history_2020.",
            input$file_type)
    },
    
    content = function(file) {
      if (input$file_type == "xlsx") {
        writexl::write_xlsx(data_filtered(), file)
      }
      else if (input$file_type == "csv") {
        write_csv(data_filtered(), file)
      }
      
    }
  )
  output$plot1 <- renderPlotly({ #plot1 (daily sales)
    df %>%
      filter(tdt_type_detail == "sale") %>%
      group_by(the_date_transaction, but_idr_business_unit) %>%
      summarize(turnover_date = sum(turnover)) %>%
      ungroup() %>%
      filter(
        between(
          the_date_transaction,
          input$date_range[1],
          input$date_range[2]
        ),
        but_idr_business_unit %in% input$store
      ) %>%
      plot_ly(x = ~ the_date_transaction,
              y = ~ turnover_date) %>%
      add_lines(color = ~ but_idr_business_unit,
                colors = store_colors) %>%
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
              list(
                count = 3,
                label = "3 months",
                step = "month",
                stepmode = "backward"
              ),
              list(
                count = 6,
                label = "6 months",
                step = "month",
                stepmode = "backward"
              ),
              list(step = "all")
            )),
            rangeslider = list(type = "date")
          ),
        yaxis = list(title = "Turnover (sales only)"),
        hovermode = "x unified"
      )
  })
  
  output$plot2 <- renderPlotly({ #plot 2 (barchart)
    df2 <- df[rep(1:nrow(df), df$quantity),]
    
    
    df2 %>%
      filter(
        .,
        between(
          the_date_transaction,
          input$date_range[1],
          input$date_range[2]
        ),
        but_idr_business_unit %in% input$store
      ) %>%
      filter(tdt_type_detail == "sale") %>%
      mutate(sport = factor(
        str_replace_all(sku_idr_sku, "_\\d+", ""),
        levels = c("Biking",
                   "Football",
                   "Running",
                   "Tennis",
                   "Other")
      )) %>%
      group_by(but_idr_business_unit, sport) %>%
      summarize(items_sold = sum(quantity)) %>%
      ungroup() %>%
      distinct(.) %>%
      plot_ly(
        x = ~ but_idr_business_unit,
        y = ~ items_sold,
        color = ~ sport,
        colors = sport_colors,
        type = "bar",
        text = ~ items_sold,
        textposition = "outside",
        legendgroup = ~ sport
      ) %>%
      layout(
        title = "Items Sold per Store & Sport",
        barmode = "group",
        yaxis = list(title = "Items Sold",
                     tickformat = "digits"),
        xaxis = list(title = "Sport",
                     tickformat = "digits"),
        uniformtext = list(minsize = 8, mode = "show")
      )
    
    
    
  })
  
  output$plot3 <- renderPlotly({ #plot3 (piechart)
    df2 <- df[rep(1:nrow(df), df$quantity),]
    
    df2 %>%
      filter(
        .,
        between(
          the_date_transaction,
          input$date_range[1],
          input$date_range[2]
        ),
        but_idr_business_unit %in% input$store
      ) %>%
      filter(tdt_type_detail == "sale") %>%
      mutate(sport = factor(
        str_replace_all(sku_idr_sku, "_\\d+", ""),
        levels = c("Biking",
                   "Football",
                   "Running",
                   "Tennis",
                   "Other")
      )) %>%
      group_by(but_idr_business_unit, sport) %>%
      summarize(items_sold = sum(quantity)) %>%
      ungroup() %>%
      distinct(.) %>%
      plot_ly(
        type = "pie",
        labels = ~ sport,
        values = ~ items_sold,
        textinfo = "label+percent",
        insidetextorientation = "radial",
        legendgroup = ~ sport,
        showlegend = F,
        marker = list(colors = sport_colors)
      ) %>%
      layout(title = "Proportion of Items Sold")
    
    
  })
  
  
  output$plot4 <- renderPlotly({ #plot4 (Violin plot)
    df %>%
      filter(
        .,
        between(
          the_date_transaction,
          input$date_range[1],
          input$date_range[2]
        ),
        but_idr_business_unit %in% input$store
      ) %>%
      mutate(
        weekdays = wday(
          the_date_transaction,
          week_start = 1,
          locale = Sys.getlocale("LC_TIME"),
          label = T,
          abbr = F
        )
      ) %>%
      plot_ly() %>%
      add_trace(
        x = ~ weekdays[df$tdt_type_detail == "sale" &
                         df$but_idr_business_unit == input$store],
        y = ~ turnover[df$tdt_type_detail == "sale" &
                         df$but_idr_business_unit == input$store],
        type = "violin",
        side = "negative",
        color = I("darkgreen"),
        name = "Sale Turnover",
        box = list(visible = T),
        meanline = list(visible = T)
      ) %>%
      add_trace(
        x = ~ weekdays[df$tdt_type_detail == "return" &
                         df$but_idr_business_unit == input$store],
        y = ~ turnover[df$tdt_type_detail == "return" &
                         df$but_idr_business_unit == input$store],
        type = "violin",
        side = "positive",
        color = I("darkred"),
        name = "Return Turnover",
        box = list(visible = T),
        meanline = list(visible = T)
      ) %>%
      layout(
        xaxis = list(title = "Day"),
        yaxis = list(title = "Turnover"),
        showlegend = T,
        violinmode = "overlay"
      )
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
