# 1. Load the required pacakges ----------------------------------------------
if(!require(shiny)) install.packages("shiny")
if(!require(writexl)) install.packages("writexl")
if(!require(plotly)) install.packages("plotly")
if(!require(lubridate)) install.packages("lubridate")
if(!require(thematic)) install.packages("thematic")
if(!require(bslib)) install.packages("bslib")
if(!require(tidyverse)) install.packages("tidyverse")



# 2. Create database -----------------------------------------------------
### set seed for reproducible results
set.seed(2001)

df_temp <- tibble(the_transaction_id = paste("trans", seq(1,250000), sep = "_"),
                  but_idr_business_unit = as.factor(sample(c("Evere", "Wavre", "Anderlecht", "Antwerp", "Charleroi", 
                                                             "Liege"), 250000, replace = T,
                                                           prob = c(0.30, 0.15, 0.20, 0.10, 0.25, 0.20))),
                  tdt_type_detail = as.factor(sample(c("sale", "return"), 250000, replace = T, prob = c(0.80, 0.20))),
                  the_date_transaction = sample(seq(as.Date("2020/01/01"), as.Date("2020/12/31"), by = "day"),
                                                250000, replace = T),
                  sku_idr_sku = as.factor(paste(sample(c("Tennis", "Football", "Running", "Biking", 
                                                         "Other"), 250000, replace = T),
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
                  Liege = "red", Wavre = "green")


###

df_plotly_1 <- df %>%
  filter(tdt_type_detail == "sale") %>%
  group_by(the_date_transaction, but_idr_business_unit) %>%
  summarize(turnover_date = sum(turnover)) %>%
  ungroup()

###
df2 <- df[rep(1:nrow(df), df$quantity), ]



###
df_plotly_4 <- df %>%
  mutate(weekdays = weekdays(the_date_transaction)) %>%
  mutate(weekdays = factor(weekdays, levels = c("lundi", "mardi", "mercredi", "jeudi", "vendredi",
                                                "samedi")))

#app is in new directory

# Define UI = Lets users interact with the APP
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "cerulean"),
  img(src = "logo.png", height = 140, width = 400),
  titlePanel(strong("Sales History App")),
  sidebarLayout(
    sidebarPanel(width = 3,
      checkboxGroupInput("store", label = "Choose a store",
                         choices = c("Anderlecht", "Antwerp", "Charleroi", "Evere", "Liege", "Wavre"),
                         selected = c("Anderlecht", "Antwerp", "Charleroi", "Evere", "Liege", "Wavre")),
      dateRangeInput("date_range", "Select a date range (yyy-mm-dd)", 
                     start = "2020/01/01", end = "2020/12/31", 
                     format = "yyyy-mm-dd", weekstart = 1, language = "fr", min = "2020/01/01",
                     max = "2020/12/31"),
      radioButtons("file_type", "Format", choices = c(excel = "xlsx",
                                                      csv = "csv"),
                   selected = "csv"),
      downloadButton("download_filtered", "Download"),
      h1("Original Dataset of Sales History"),
      h2("Overview"),
      h3("Dataset varabiables/columns"),
      p("The dataset simulates the sales and return turnover numbers for a sport-suypply store for the year 2020. the dataset contains 214266 rows (observations) and 7 columns (variables).",
        em("It should be noted that Sundays have not been included in the dataset, as stores are generally closed that day."),
      ),
      p("- ",
        strong("transaction_id:"), 
        "the name of the transaction and serves as.",
        em("the Primary Key"),
        "for this dataset, it is assumed that for every transaction, a customer buyes only one item, 
      but maybe more than once as the other variables/columns of the dataset will show."),
      br(),
      p("- ",
        strong("but_idr_business_unit:"),
        "the name of the store."),
      br(),
      p("- ", 
        strong("tdt_type_detail:"),
        "Shows if the transaction was a sale or a return."),
      br(),
      p("- ",
        strong("the_date_transaction:"),
        "the date of the transaction."),
      br(),
      p("- ",
        strong("sku_idr_sku:"),
        "the ID of the item that was sold, also shows in which category it belongs."),
      br(),
      p("- ",
        strong("quantity:"),
        "amount of items sold or returned."),
      br(),
      p("- ",
        strong("turnover:"),
        "the total turnover of the transaction/sale.", 
        strong("IMPORTANT:"), "the turnover variable is the unity price x quantity, per request, 
      the unity price is not displayed in the dataset", em("but"), "can be found in the code that created the dataset.")
      ),
    mainPanel(
      h1("Plots"),
      br(),
      p("This app is accompanied by the required three plots, plus one additional plot, you can browse with your mouse over the plot to reveal additional information.",
        strong("All plots are based on the original dataframe provided below the plots,
               the dataset was tranformed/mutated to provide these plots, however, no datapoint(s) was/were changed.")),
      br(),
      h2("Plot 1"),
      p("The first plot shows the daily sale history throughout the year, only sales, not returns,
        though it might appear messy at first, you can play with the date range on the x-axis to visualize smaller time periods, there's also an extra menu provided above.
        Additionally, browsing over the plot also alows you to compare the turnover numbers for all the stores on the same day."),
      plotlyOutput("plot1"),
      
      br(),
      h2("Plot 2"),
      p("The second plot displays how many items per sport have been sold, the quantity of the transaction is included in the calculations
        (if the item named Tennis_13 was sold four times, it will be counted as four separate entities, not one)."),
      
      plotlyOutput("plot2"),
      
      br(),
      h2("Plot 3 (Violin Plots)"),
      p("A violin plot shows the entire distribution of the data, the advantage of the violin plot is that
        Wider sections of the violin plot represent a higher probability of a given sales/return turnover amount to occur; the skinnier sections represent a lower probability.
        The dots represent extreme, exceptional values, you can browse with your mouse through the plot to show
        additional information such as minimum, maximum values, mean, median,..."),
      br(),
      p("In this plot, you can see the probability distribution of the different months."),
      plotlyOutput("plot3"),
  
      dataTableOutput("trans_data"),
      )
  )
)

  
  
# Define server logic = Part of the APP that is reactive and DOES something
server <- function(input, output) {
  thematic::thematic_shiny()

  data_filtered <- reactive({
  df %>%
      filter(., between(the_date_transaction, input$date_range[1], input$date_range[2]), but_idr_business_unit %in% input$store)
  })
  
  
  output$trans_data <- renderDataTable({
    data_filtered()
  })
  
  output$download_filtered <- downloadHandler(
  filename = function(){
    str_c(Sys.Date(),"_transaction_history_2020.", input$file_type)
  },
  
  content = function(file){
    if (input$file_type == "xlsx") {
      writexl::write_xlsx(data_filtered(), file)
    }
    else if (input$file_type == "csv"){
      write_csv(data_filtered(), file)
    }
    
  })
  output$plot1 <- renderPlotly({
    
    df %>%
      filter(tdt_type_detail == "sale") %>%
      group_by(the_date_transaction, but_idr_business_unit) %>%
      dplyr::summarize(turnover_date = sum(turnover)) %>%
      ungroup() %>%
      filter(between(the_date_transaction, input$date_range[1], input$date_range[2]), but_idr_business_unit %in% input$store) %>%
      plot_ly(x = ~the_date_transaction, y= ~turnover_date) %>%
      add_lines(color = ~but_idr_business_unit, colors = store_colors) %>%
      layout(xaxis = 
               list(title = "Date (Move slider to explore ranges)",
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
             yaxis = list(title = "Turnover (sales only)"),
             hovermode = "x unified")
  })
  
  output$plot2 <- renderPlotly({
    
    df2 %>%
      filter(., between(the_date_transaction, input$date_range[1], input$date_range[2]), but_idr_business_unit %in% input$store) %>%
      filter(tdt_type_detail == "sale") %>%
      mutate(sport = factor(str_replace_all(sku_idr_sku, "_\\d+", ""), levels = c("Biking", 
                                                                                    "Football",
                                                                                    "Running", 
                                                                                    "Tennis", 
                                                                                    "Other"))) %>%
      group_by(but_idr_business_unit, sport) %>%
      dplyr::summarize(items_sold = sum(quantity)) %>%
      ungroup() %>%
      distinct(.) %>% 
      plot_ly(x = ~sport, y = ~items_sold, color = ~but_idr_business_unit, colors = store_colors,
              type = "bar", text = ~items_sold, textposition = "outside") %>%
      layout(title = "Items Sold per Store & Sport",
             barmode = "group",
             yaxis = list(title = "Items Sold"),
             xaxis = list(title = "Sport"))
    
  })
  
  output$plot3 <- renderPlotly({
    
    df %>%
      filter(., between(the_date_transaction, input$date_range[1], input$date_range[2]), but_idr_business_unit %in% input$store) %>%
      mutate(weekdays = weekdays(the_date_transaction)) %>%
      mutate(weekdays = factor(weekdays, levels = c("lundi", "mardi", "mercredi", "jeudi", "vendredi",
                                                    "samedi"))) %>%
      plot_ly() %>%
      add_trace(x = ~weekdays[df_plotly_4$tdt_type_detail == "sale" &  df_plotly_4$but_idr_business_unit == "Anderlecht"], 
                y = ~turnover[df_plotly_4$tdt_type_detail == "sale" & df_plotly_4$but_idr_business_unit == "Anderlecht"],
                type = "violin", side = "negative", name = "Sales Turnover", color = I("darkgreen"), visible = T,
                box = list(visible = T), meanline = list(visible = T)) %>%
      add_trace(x = ~weekdays[df_plotly_4$tdt_type_detail == "return" & df_plotly_4$but_idr_business_unit == "Anderlecht"],
                y = ~turnover[df_plotly_4$tdt_type_detail == "return" & df_plotly_4$but_idr_business_unit == "Anderlecht"],
                type = "violin", side = "positive", name = "Return Turnover", color = I("darkred"), visible = T,
                box = list(visible = T), meanline = list(visible = T)) %>%
      add_trace(x = ~weekdays[df_plotly_4$tdt_type_detail == "sale" &  df_plotly_4$but_idr_business_unit == "Antwerp"], 
                y = ~turnover[df_plotly_4$tdt_type_detail == "sale" & df_plotly_4$but_idr_business_unit == "Antwerp"],
                type = "violin", side = "negative", name = "Sales Turnover", color = I("darkgreen"), visible = F,
                box = list(visible = T), meanline = list(visible = T)) %>%
      add_trace(x = ~weekdays[df_plotly_4$tdt_type_detail == "return" & df_plotly_4$but_idr_business_unit == "Antwerp"],
                y = ~turnover[df_plotly_4$tdt_type_detail == "return" & df_plotly_4$but_idr_business_unit == "Antwerp"],
                type = "violin", side = "positive", name = "Return Turnover", color = I("darkred"), visible = F,
                box = list(visible = T), meanline = list(visible = T)) %>%
      add_trace(x = ~weekdays[df_plotly_4$tdt_type_detail == "sale" &  df_plotly_4$but_idr_business_unit == "Charleroi"], 
                y = ~turnover[df_plotly_4$tdt_type_detail == "sale" & df_plotly_4$but_idr_business_unit == "Charleroi"],
                type = "violin", side = "negative", name = "Sales Turnover", color = I("darkgreen"), visible = F,
                box = list(visible = T), meanline = list(visible = T)) %>%
      add_trace(x = ~weekdays[df_plotly_4$tdt_type_detail == "return" & df_plotly_4$but_idr_business_unit == "Charleroi"],
                y = ~turnover[df_plotly_4$tdt_type_detail == "return" & df_plotly_4$but_idr_business_unit == "Charleroi"],
                type = "violin", side = "positive", name = "Return Turnover", color = I("darkred"), visible = F,
                box = list(visible = T), meanline = list(visible = T)) %>%
      add_trace(x = ~weekdays[df_plotly_4$tdt_type_detail == "sale" &  df_plotly_4$but_idr_business_unit == "Evere"], 
                y = ~turnover[df_plotly_4$tdt_type_detail == "sale" & df_plotly_4$but_idr_business_unit == "Evere"],
                type = "violin", side = "negative", name = "Sales Turnover", color = I("darkgreen"), visible = F,
                box = list(visible = T), meanline = list(visible = T)) %>%
      add_trace(x = ~weekdays[df_plotly_4$tdt_type_detail == "return" & df_plotly_4$but_idr_business_unit == "Evere"],
                y = ~turnover[df_plotly_4$tdt_type_detail == "return" & df_plotly_4$but_idr_business_unit == "Evere"],
                type = "violin", side = "positive", name = "Return Turnover", color = I("darkred"), visible = F,
                box = list(visible = T), meanline = list(visible = T)) %>%
      add_trace(x = ~weekdays[df_plotly_4$tdt_type_detail == "sale" &  df_plotly_4$but_idr_business_unit == "Liege"], 
                y = ~turnover[df_plotly_4$tdt_type_detail == "sale" & df_plotly_4$but_idr_business_unit == "Liege"],
                type = "violin", side = "negative", name = "Sales Turnover", color = I("darkgreen"), visible = F,
                box = list(visible = T), meanline = list(visible = T)) %>%
      add_trace(x = ~weekdays[df_plotly_4$tdt_type_detail == "return" & df_plotly_4$but_idr_business_unit == "Liege"],
                y = ~turnover[df_plotly_4$tdt_type_detail == "return" & df_plotly_4$but_idr_business_unit == "Liege"],
                type = "violin", side = "positive", name = "Return Turnover", color = I("darkred"), visible = F,
                box = list(visible = T), meanline = list(visible = T)) %>%
      add_trace(x = ~weekdays[df_plotly_4$tdt_type_detail == "sale" &  df_plotly_4$but_idr_business_unit == "Wavre"], 
                y = ~turnover[df_plotly_4$tdt_type_detail == "sale" & df_plotly_4$but_idr_business_unit == "Wavre"],
                type = "violin", side = "negative", name = "Sales Turnover", color = I("darkgreen"), visible = F,
                box = list(visible = T), meanline = list(visible = T)) %>%
      add_trace(x = ~weekdays[df_plotly_4$tdt_type_detail == "return" & df_plotly_4$but_idr_business_unit == "Wavre"],
                y = ~turnover[df_plotly_4$tdt_type_detail == "return" & df_plotly_4$but_idr_business_unit == "Wavre"],
                type = "violin", side = "positive", name = "Return Turnover", color = I("darkred"), visible = F,
                box = list(visible = T), meanline = list(visible = T)) %>%
      layout(xaxis = list(title = "Day"),
             yaxis = list(title = "Turnover (€)"))
  })
  
  }

# Run the app ----
shinyApp(ui = ui, server = server)