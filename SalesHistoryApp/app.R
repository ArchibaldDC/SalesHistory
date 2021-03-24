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

#app is in new directory

# Define UI = Lets users interact with the APP
ui <- fluidPage(
  titlePanel(strong("Sales History App")),
  sidebarLayout(
    sidebarPanel(
      h1("Original Dataset of Sales History"),
      h2("Overview"),
      h3("Dataset varabiables/columns"),
      p("The dataset simulates the sales and return turnover numbers for a sport-suypply store for the year 2020. the dataset contains 214330 rows (observations) and 7 columns (variables)",
        em("it should be noted that Sundays have not been included in the dataset, as stores are generally closed that day"),
      ),
      p("- ",
        strong("transaction_id:"), 
        "the name of the transaction and serves as",
        em("the Primary Key"),
        "for this dataset, it is assumed that for every transaction, a customer buyes only one item, 
      but maybe more than once as the other variables/columns of the dataset will show."),
      br(),
      p("- ",
        strong("but_idr_business_unit:"),
        "is the name of the store"),
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
      the unity price is not displayed in the dataset", em("but"), "can be found in the code that created the dataset."),
      
      checkboxGroupInput("store", label = "Choose a store",
                  choices = c("Anderlecht", "Antwerp", "Charleroi", "Evere", "Liège", "Wavre")),
      dateRangeInput("daterange", "Select a date range (yyy-mm-dd)", 
                     start = "2020/01/01", end = "2020/12/31", 
                     format = "yyyy-mm-dd", weekstart = 1, language = "fr", min = "2020/01/01",
                     max = "2020/12/31"),
      tableOutput("df"),
      ),
    mainPanel(
      h1("Plots"),
      br(),
      p("This app is accompanied by the required three plots, plus one additional plot, you can browse with your mouse over the plot to reveal additional information",
        strong("all plots are based on the original dataframe provided in the sidebar,
               the dataset was tranformed/mutated to provide these plots, however, no datapoint(s) was changed")),
      br(),
      h2("Plot 1"),
      p("The first plot shows the sale history throughout the year, only sales, not returns,
        though it might appear messy at first, you can play with the date range on the x-axis to visualize smaller time perioods, there's also an extra menu provided above."),
      br(),
      h2("Plot 2"),
      p("The second plot displays how many items per sport have been sold, the quantity of the transaction is included in the calculations
        (if the item named Tennis_13 was sold 4 times, it will be counted as 4 separate entities, not 1). A menu is provided to browse through the different stores."),
      br(),
      h2("Plot 3"),
      p("Plot 3 shows the change in the turnover amount in sales and return over the course of the year, a menu is provided to select the store."),
      br(),
      h2("Extra Plot (Violin Plots)"),
      p("A violin plot shows the entire distribution of the data, the advantage of the violin plot is that
        Wider sections of the violin plot represent a higher probability of a given sales/return turnover amount to occur; the skinnier sections represent a lower probability.
        The dots represent extreme, exceptional values, you can browse with your mouse through the plot to show
        additional information such as minimum, maximum values, mean, median,..."),
      br(),
      p("In this plot, you can see the probability distribution of the different months, a menu is provided to look at the data for every store.")
    )
  )
)

  
  
# Define server logic = Part of the APP that is reactive and DOES something
server <- function(input, output) {
  filtered_data <- reactive({
    req(input$daterange)
    df[df$the_date_transaction >= input$daterange[1] & df$the_date_transaction <= input$daterange[2],]
  })
  output$df <- renderTable(filtered_data())
  }


# Run the app ----
shinyApp(ui = ui, server = server)

runApp()

