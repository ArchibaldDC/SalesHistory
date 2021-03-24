#app is in new directory

if(!require(shiny)) install.packages("shiny")
if(!require(reactable)) install.packages("reactable")



# Define UI = Lets users interact with the APP
ui <- fluidPage(
  sliderInput("x", label = "If x is", min = 1, max = 50, value = 30),
  sliderInput("y", label = "and y is", min = 1, max = 50, value = 5),
  "then x times y is",
  textOutput("product")
)

# Define server logic = Part of the APP that is reactive and DOES something
server <- function(input, output, session) {
  output$product <- renderText({ 
    input$x * input$y
  })
  }

# Run the app ----
shinyApp(ui = ui, server = server)

runApp()

