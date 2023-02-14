library(shiny)
library(dplyr)
library(shinyjs)
library(shinythemes)
library(spsComps)
# Define UI for application that draws a histogram
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "https://unpkg.com/cardsJS/dist/cards.min.css"),
    tags$script(src = "https://unpkg.com/cardsJS/dist/cards.min.js", type = "text/javascript"),
    tags$style(type = "text/css", "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"),
    tags$link(rel = "shortcut icon", href = "diamond-solid.svg"),
    #tags$link(rel="stylesheet",href="https://cdnjs.cloudflare.com/ajax/libs/animate.css/4.1.1/animate.min.css"),
    HTML('<!-- Primary Meta Tags -->
           <title>Blackjack App</title>
           <meta name="title" content="Blackjack App">
           <meta name="description" content="Ever wanted to waste even more time? Well look no further, this blackjack app is designed to waste the maximum amount of your time possible.
           This is second most balling Blackjack App in the SEC">

           <!-- Open Graph / Facebook -->
           <meta property="og:type" content="website">
           <meta property="og:url" content="https://aholmes23.shinyapps.io/Blackjack/">
           <meta property="og:title" content="Blackjack App">
           <meta property="og:description" content="Ever wanted to waste even more time? Well look no further, this blackjack app is designed to waste the maximum amount of your time possible.">
           <meta property="og:image" href="https://image.winudf.com/v2/image/Y29tLmpvZ2F0aW5hLmJsYWNramFja3JveWFsX3NjcmVlbl8wXzE1MzQ4MTc1ODJfMDc0/screen-0.jpg?fakeurl=1&type=.webp">

           <!-- Twitter -->
           <meta property="twitter:card" content="summary_large_image">
           <meta property="twitter:url" content="https://aholmes23.shinyapps.io/Blackjack/">
           <meta property="twitter:title" content="Blackjack App">
           <meta property="twitter:description" content="Ever wanted to waste even more time? Well look no further, this blackjack app is designed to waste the maximum amount of your time possible.">
           <meta property="twitter:image" content="https://image.winudf.com/v2/image/Y29tLmpvZ2F0aW5hLmJsYWNramFja3JveWFsX3NjcmVlbl8wXzE1MzQ4MTc1ODJfMDc0/screen-0.jpg?fakeurl=1&type=.webp">')
  ),

  # Application title
  h1("Blackjack", align = "center"),
  theme = shinytheme("darkly"),
  uiOutput("dealer", align = "center"),
  animateUI('dealer', animation = 'float', hover =TRUE, speed='fast'),
  br(),
  br(),
  br(),
  br(),
  uiOutput("user", align = "center"),
  animateUI('user', animation = 'float', hover =TRUE, speed='fast'),
  br(),
  fluidRow(
    column(6,
      align = "center", offset = 3,
      actionButton("hit", "Hit!"),
      actionButton("stay", "Stay.."),
      textOutput("play"),
      textOutput("deal"),
      textOutput("win"),
      br(),
      actionButton("refresh", "Start a new one!")
    )
  ),
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  deck <- expand.grid(values = c(2:10, "A", "Q", "K", "J"), suits = c("S", "C", "D", "H"))
  deck$lookup <- paste0(deck$values, deck$suits)
  deck$point <- suppressWarnings(ifelse(!is.na(as.numeric(as.character(deck$values))), as.numeric(as.character(deck$values)),
    ifelse(deck$values %in% c("Q", "K", "J"), 10, 1)
  ))

  win <- 21

  dealer <- deck[sample(nrow(deck), 2, replace = F), ]

  deck <- deck %>% filter(!(lookup %in% dealer$lookup))

  player <- deck[sample(nrow(deck), 2, replace = F), ]

  deck <- deck %>% filter(!(lookup %in% player$lookup))

  output$dealer <- renderUI({
    if (!input$stay) {
      HTML(paste0("<div class='hhand-compact active-hand'>
                <img class='card' src='cards/", sample(c("BLUE_BACK.svg", "RED_BACK.svg"), 1), "' style = width:120px>
                <img class='card' src='cards/", dealer$lookup[1], ".svg' style = width:120px>"))
    } else if (input$stay) {
      shinyjs::disable("hit")
      while (sum(dealer$point) < 17) {
        dealer <<- rbind(dealer, deck[sample(nrow(deck), 1, replace = F), ])
        deck <<- deck %>% filter(!(lookup %in% player$lookup))
      }
      dealerbase <- "<div class='hhand active-hand'>"
      for (i in 1:length(dealer$lookup)) {
        dealerbase <- paste0(dealerbase, "<img class='card' src='cards/", dealer$lookup[i], ".svg' style = width:120px>")
      }
      output$deal <- renderText({
        paste("Dealer Score", sum(dealer$point))
      })
      output$win <- renderText({
        if (((sum(dealer$point) > sum(player$point)) & sum(dealer$point) < 21) | (sum(player$point) > win)) {
          "Dealer Wins"
        } else if (sum(dealer$point) < sum(player$point) & (sum(player$point <= win)) |
          (sum(dealer$point) > 21 & sum(player$point <= win)) |
          ((any(player$value %in% c("A")) & sum(player$point) < 11))) {
          "Player Wins"
        } else if (sum(dealer$point) == sum(player$point)) {
          "This is awkward"
        }
      })
      #tags$div(class="animate__animated animate__bounce animate__faster")
      HTML(dealerbase)
    }
  })

  output$user <- renderUI({
    if (input$hit) {
      player <<- rbind(player, deck[sample(nrow(deck), 1, replace = F), ])
      deck <<- deck %>% filter(!(lookup %in% player$lookup))
    }

    output$play <- renderText({
      if (sum(player$point) == 21 | (any(player$value %in% c("A")) & sum(player$point) - 1 == 10)) {
        shinyjs::disable("hit")
        "YOU WON"
      } else if ((any(player$value %in% c("A")) & sum(player$point) < 11)) {
        paste("Player Score:", sum(player$point) + 10)
      } else if (sum(player$point) > 21) {
        shinyjs::disable("hit")
        paste("Bust", sum(player$point))
      } else if (sum(player$point) < 21) {
        paste("Player Score:", sum(player$point))
      }
    })
    base <- "<div class='hhand-compact active-hand'>"
    for (i in 1:length(player$lookup)) {
      base <- paste0(base, "<img class='card' src='cards/", player$lookup[i], ".svg' style = width:120px>")
    }


    HTML(base)
  })

  observeEvent(input$refresh, {
    refresh()
  })
}


# Run the application
shinyApp(ui = ui, server = server)
