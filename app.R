library(shiny)
library(shinyWidgets)
library(dplyr)
library(spsComps)
library(cookies)
library(shinyjs)
library(shinydashboard)
library(gitlink)
library(googleAuthR)
library(ggplot2)

options(googleAuthR.webapp.client_id = "1043541324641-po8v7b7s0rh3c33vm4erpaot8rfu0s4c.apps.googleusercontent.com")
options(shiny.port=1221)
source("helpers/helpers.R")

# Define UI for application that draws a histogram
ui <- add_cookie_handlers(fluidPage(
  useSweetAlert(),
  useShinyjs(),
  useShinydashboard(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "https://unpkg.com/cardsJS/dist/cards.min.css"),
    tags$script(src = "https://unpkg.com/cardsJS/dist/cards.min.js", type = "text/javascript"),
    tags$style(type = "text/css", "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"),
    tags$link(rel = "shortcut icon", href = "favicon.ico"),
    includeHTML("www/meta-tags.html"),
    tags$style(src = "banner.css"),
    HTML('<div class="cookies-eu-banner hidden">
       By clicking ”OK”, you agree to the storing of your Dollaz on your device. Blackjack is not liable for computer damages or addictive behavior changes.
       <button>OK</button>
     </div>
 <link rel="stylesheet" href="banner.css" />
     <script src="banner.js"></script>'),
    tags$script(src = "banner.js")
  ),

  # Application title
  h1(icon("diamond"), "Blackjack", icon("diamond"), id = "title", align = "center"),
  animateUI("title", animation = "float"),
  setBackgroundImage(src = "table.jpg"),
  uiOutput("dealer", align = "center"),
  animateUI("dealer", animation = "float", hover = TRUE, speed = "fast"),
  ribbon_css("https://github.com/AlexanderHolmes0/BlackJackApp", text = "Github Repo Link", position = "right"),
  br(),
  br(),
  uiOutput("user", align = "center"),
  animateUI("user", animation = "float", hover = TRUE, speed = "fast"),
  br(),
  fluidRow(
    id = "some",
    column(
      width = 6,
      align = "center", offset = 3,
      actionBttn("confirm", "Confirm Bet", icon = icon("sack-dollar"), style = "minimal", color = "default"),
      actionBttn("hit", "Hit", style = "minimal", icon = icon("gavel"), color = "default", no_outline = T),
      actionBttn("stay", "Stand", style = "minimal", icon = icon("person"), color = "default"),
      # actionBttn("split", "Split", style = "minimal", icon = icon("arrows-split-up-and-left"), color = "default"),
      br(),
      br(),
      span(uiOutput("play"), style = "font-size:20px; font-family:arial; font-style:italic; color:white"),
      span(textOutput("deal"), style = "font-size:20px; font-family:arial; font-style:italic; color:white"),
      span(textOutput("win"), style = "font-size:20px; font-family:arial; font-style:italic; color:white"),
      span(textOutput("points"), style = "font-size:20px; font-family:arial; font-style:italic; color:white"),
      br(),
      actionBttn("refresh", "New Hand", icon = icon("hands"), style = "minimal", color = "default"),
      actionBttn("reset", "Reset Wallet", icon = icon("rotate-right"), style = "minimal", color = "default"),
      br(),
      chooseSliderSkin(skin = "Square"),
      HTML('<p><span style="font-size:20px; font-family:arial; font-style:italic; color:white"</span>Gamble Amount</p>'),
      sliderInput("gamble", "", min = 0, max = 100, value = 1),
      # actionBttn("hub", "GitHub", onclick = paste0("window.open('https://github.com/AlexanderHolmes0/BlackJackApp')"), style = "minimal", icon = icon("github"), color = "default")
      #googleSignInUI('demo'),
      br(),
      column(box(plotOutput("point_history"),collapsible = TRUE, collapsed = TRUE,solidHeader = TRUE, status = "success", title = "Wallet History"),offset = 3, width = 12)
    )
  ),
))

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  point_history <- c()
  points <- reactiveVal(isolate(as.numeric(get_cookie("points",missing=100))))
  
  gamble <- reactive({
    input$gamble
  })

  # create deck
  deck <- create_deck(4)

  user <- reactiveVal(player(deck))
  dealer <- reactiveVal(dealer1(deck, isolate(user())))

  output$point_history <- renderPlot({
    points()
    point_history <<- c(point_history, points())
    ggplot(tibble(x = 1:length(point_history), y = point_history), aes(x, y)) +
      geom_line(aes(group=1)) +
      geom_point()+
      theme_bw()+ 
      labs(x = "Hand", y = "Wallet")+
      scale_y_continuous(labels = scales::dollar)+
      scale_x_continuous(breaks = seq(1, length(point_history), 1))
    
  })

  output$points <- renderText({
    paste0("Wallet Left: ", "$", points())
  })

  
  
  observeEvent(input$reset, {
    points(100)
  })

  observeEvent(input$confirm, {
    disable("confirm")
    disable("gamble")
    enable("hit")
    enable("stay")
    z(1)
    if (did_user_win(user())) {
      
      points(update_points(as.numeric(points()), gamble(), "win"))

      enable("refresh")
      disable("hit")
      disable(("stay"))
      # disable("split")
      sendSweetAlert(
        session = session,
        title = "You WON",
        text = paste0("You Won:$ ", gamble(), " Keep the Streak!"),
        type = "success"
      )
      while (keep_going_dealer(dealer())) {
        dealer(hit_dealer(dealer(), deck))
        deck <<- update_deck_dealer(deck, dealer())
      }
      v(1)
    }
  })



  observeEvent(input$hit, {
    if (keep_going_user(user())) {
      shinyjs::disable("refresh")

      user(hit_player(user(), deck))
      deck <<- update_deck_user(deck, user())
    }
    
    if (sum(user()$point) > 21) {
      points(update_points(as.numeric(points()), gamble(), "lost"))

      enable("refresh")
      disable("hit")
      disable("stay")

      sendSweetAlert(
        session = session,
        title = "BUST",
        text = paste0("You lost:$ ", gamble(), " -- Start a new one Buster!"),
        type = "error"
      )
      while (keep_going_dealer(dealer())) {
        dealer(hit_dealer(dealer(), deck))
        deck <<- update_deck_dealer(deck, dealer())
      }
      v(1)
    }
    
    if (did_user_win(user())) {
      points(update_points(points(), gamble(), "win"))
      enable("refresh")
      disable("hit")
      disable(("stay"))

      sendSweetAlert(
        session = session,
        title = "You WON",
        text = paste0("You Won:$ ", gamble(), " Keep the Streak!"),
        type = "success"
      )

      while (keep_going_dealer(dealer())) {
        dealer(hit_dealer(dealer(), deck))
        deck <<- update_deck_dealer(deck, dealer())
      }
      v(1)
    }
  })

  # define win logic when player hasnt won and is under 21

  observeEvent(input$stay, {
    while (keep_going_dealer(dealer())) {
      dealer(hit_dealer(dealer(), deck))
      deck <<- update_deck_dealer(deck, dealer())
    }
    
    v(1)
    
    if (user_dealer_comparison(user(), dealer())) {
      points(update_points(points(), gamble(), "win"))

      enable("refresh")
      disable("hit")
      disable(("stay"))
      sendSweetAlert(
        session = session,
        title = "You WON",
        text = paste0("You Won:$ ", gamble(), " Keep the Streak!"),
        type = "success"
      )
    } else if (did_dealer_win(dealer()) | !(user_dealer_comparison(user(), dealer()))) {
      
      points(update_points(points(), gamble(), "lost"))

      enable("refresh")
      disable("hit")
      disable(("stay"))
      sendSweetAlert(
        session = session,
        title = "You Lost",
        text = paste0("You lost:$ ", gamble(), " -- Start a new one Buster!"),
        type = "error"
      )
    } else if (sum(dealer()$point) == sum(user()$point)) {
      shinyjs::enable("refresh")
      shinyjs::disable("hit")
      shinyjs::disable(("stay"))
      sendSweetAlert(
        session = session,
        title = "Tie",
        text = paste0("Tie! -- Start a new one!"),
        type = "info"
      )
    }
  })


  observeEvent(input$refresh, {
    disable("hit")
    enable("stay")
    enable("refresh")
    enable("confirm")
    enable("gamble")
    #take out cards from user and dealer
    deck <<- update_deck_dealer(deck, dealer())
    deck <<- update_deck_user(deck, user())
    
    if (nrow(deck) <= 10) {
      deck <<- create_deck(4)
    }
    #redeal cards
    user(player(deck))
    dealer(dealer1(deck,user()))
    #take out cards just dealt
    deck <<- update_deck_dealer(deck, dealer())
    deck <<- update_deck_user(deck, user())
    output$deal <- renderText({
      ""
    })
    # terribly written switches cuz Idk
    v(0) # flips dealer cards around
    x(0) # split indicator
    z(0) # confirmation of bet switch
  })


observeEvent(input$hit,{
  cookies::set_cookie(
    cookie_name = "points",
    cookie_value = as.character(points()))
}
)
observeEvent(input$stay,{
  cookies::set_cookie(
    cookie_name = "points",
    cookie_value = as.character(points()))
}
)



  output$user <- renderUI({
    if (x() == 0) {
      return(victim())
    } else {
      return(split()[[1]])
    }
  })

  victim <- reactive({
    base <- "<div class='hhand-compact active-hand'>"
    if (z() == 0) {
      HTML(paste0(
        "<div class='hhand-compact active-hand'>
                   <img class='card' src='cards/", sample(c("BLUE_BACK.svg", "RED_BACK.svg"), 1), "' style = width:150px>",
        "<img class='card' src='cards/", sample(c("BLUE_BACK.svg", "RED_BACK.svg"), 1), "' style = width:150px>"
      ))
    } else {
      for (i in 1:nrow(user())) {
        base <- paste0(base, "<img class='card' src='cards/", user()$lookup[i], ".svg' style = width:150px>")
      }
      HTML(base)
    }
  })


  split <- eventReactive(input$split, {
    user(list(user()[1, ], user()[2, ]))

    base <- "<div class='hhand-compact active-hand'>"
    cards <- ""
    for (i in 1:length(user())) {
      cards <- paste0(cards, base)
      for (j in 1:nrow(user()[[i]])) {
        cards <- paste0(cards, "<img class='card' src='cards/", user()[[i]]$lookup[j], ".svg' style = width:150px>")
      }
    }

    list(HTML(cards))
  })


  z <- reactiveVal(0) # turns players cards around
  x <- reactiveVal(0)

  # observeEvent(input$split, {
  #   x(1)
  # })




  output$play <- renderUI({
    if (z() == 1) {
      if ((any(user()$value %in% c("A")) & sum(user()$point) - 1 <= 10)) {
        paste("Player Score:", sum(user()$point) + 10)
      } else {
        paste0("Player Score: ", sum(user()$point))
      }
    }
  })



  output$dealer <- renderUI({
    if (z() == 0) {
      HTML(paste0("<div class='hhand-compact active-hand'>
                   <img class='card' src='cards/", sample(c("BLUE_BACK.svg", "RED_BACK.svg"), 1), "' style = width:150px>
                 <img class='card' src='cards/", sample(c("BLUE_BACK.svg", "RED_BACK.svg"), 1), "' style = width:150px>"))
    } else if (v() == 0 & z() == 1) {
      HTML(paste0("<div class='hhand-compact active-hand'>
                   <img class='card' src='cards/", sample(c("BLUE_BACK.svg", "RED_BACK.svg"), 1), "' style = width:150px>
                 <img class='card' src='cards/", as.character(dealer()$lookup[1]), ".svg' style = width:150px>"))
    } else if (v() == 1) {
      dealerbase <- "<div class='hhand active-hand'>"
      for (i in 1:nrow(dealer())) {
        dealerbase <- paste0(dealerbase, "<img class='card' src='cards/", as.character(dealer()$lookup[i]), ".svg' style = width:150px>")

        output$deal <- renderText({
          if ((any(dealer()$value %in% c("A")) & sum(dealer()$point) - 1 == 10)) {
            paste("Dealer Score:", sum(dealer()$point) + 10)
          } else {
            paste0("Dealer Score: ", sum(dealer()$point))
          }
        })
      }
      HTML(dealerbase)
    }
  })

  v <- reactiveVal(0) # turns the dealer HTML on for real cards
}


# Run the application
shinyApp(ui = ui, server = server)
