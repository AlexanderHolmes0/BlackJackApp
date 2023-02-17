#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(cookies)
# Define UI for application that draws a histogram
ui <- add_cookie_handlers(fluidPage(

  # Application title
  titlePanel("Old Faithful Geyser Data"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
        "Number of bins:",
        min = 1,
        max = 50,
        value = 30
      )
    ),

    # Show a plot of the generated distribution
    mainPanel(
      textOutput("distPlot"),
      actionButton("refresh", "REFRESH")
    )
  )
))

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  point <- eventReactive(isolate(input$refresh), {print(input$refresh)
    as.numeric(get_cookie("points", missing = 100))
  }, ignoreNULL=F)

  output$distPlot <- renderText({
    point()
  })

  eventReactive(isolate(input$refresh), {
print("test")
    point() <- point() - 1
    cookies::set_cookie(
      cookie_name = "points",
      cookie_value = point()
    )
    refresh()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
