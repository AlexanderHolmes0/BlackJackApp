library(shiny)
library(shinyWidgets)
library(dplyr)
library(shinythemes)
library(spsComps)
library(cookies)
library(data.table)
library(shinyjs)
library(ggplot2)
library(plotly)
# Define UI for application that draws a histogram
ui <- add_cookie_handlers(fluidPage(
  useSweetAlert(),
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "https://unpkg.com/cardsJS/dist/cards.min.css"),
    tags$script(src = "https://unpkg.com/cardsJS/dist/cards.min.js", type = "text/javascript"),
    tags$style(type = "text/css", "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"),
    tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {
                                                  background: #000069;
                                                  border-top: 1px solid #000039 ;
                                                  border-bottom: 1px solid #000039 ;}
                           .irs-from, .irs-to, .irs-single { background: #000069 }")),
    tags$link(rel = "shortcut icon", href = "diamond-solid.svg"),
    HTML('<!-- Primary Meta Tags -->
<title>Blackjack App</title>
<meta name="title" content="Blackjack App">
<meta name="description" content="Ever wanted to waste even more time? Well look no further, this blackjack app is designed to waste the maximum amount of your time possible. This is the second most balling Blackjack App in the SEC.">

<!-- Open Graph / Facebook -->
<meta property="og:type" content="website">
<meta property="og:url" content="https://aholmes23.shinyapps.io/Blackjack/">
<meta property="og:title" content="Blackjack App">
<meta property="og:description" content="Ever wanted to waste even more time? Well look no further, this blackjack app is designed to waste the maximum amount of your time possible. This is the second most balling Blackjack App in the SEC.">
<meta property="og:image" content="https://i.imgur.com/cQuw6UM.jpg">

<!-- Twitter -->
<meta property="twitter:card" content="summary_large_image">
<meta property="twitter:url" content="https://aholmes23.shinyapps.io/Blackjack/">
<meta property="twitter:title" content="Blackjack App">
<meta property="twitter:description" content="Ever wanted to waste even more time? Well look no further, this blackjack app is designed to waste the maximum amount of your time possible. This is the second most balling Blackjack App in the SEC.">
<meta property="twitter:image" content="https://i.imgur.com/cQuw6UM.jpg">'),
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
  theme = shinytheme("darkly"),
  setBackgroundColor(
    color = "darkgreen",
    gradient = c("radial"),
    direction = c("bottom"),
    shinydashboard = FALSE
  ),
  uiOutput("dealer", align = "center"),
  animateUI("dealer", animation = "float", hover = TRUE, speed = "fast"),
  br(),
  br(),
  br(),
  br(),
  uiOutput("user", align = "center"),
  animateUI("user", animation = "float", hover = TRUE, speed = "fast"),
  br(),
  fluidRow(
    id = "some",
    column(6,
      align = "center", offset = 3,
      actionButton("hit", "Hit"),
      animateUI("hit", animation = "shake", hover = T),
      actionButton("stay", "Stand"),
      animateUI("stay", animation = "shake", hover = T),
      actionButton("split", "Split"),
      animateUI("split", animation = "shake", hover = T),
      textOutput("play"),
      textOutput("deal"),
      textOutput("win"),
      textOutput("points"),
      br(),
      actionButton("refresh", "New Hand"),
      actionButton("reset", "Reset Points"),
      br(),
      br(),
      sliderInput("gamble", "Gamble Amount:", min = 0, max = 100, value = 1)
    )
  ),
#plotlyOutput('pointhist')
))

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
#   output$pointhist <- renderPlotly({
# if(length(hist()) > 0){
#   
#     ggplotly(ggplot(data.frame(hist()),aes(x = x, y=points, group =1))+
#       geom_point()+
#       labs(x='Hand Number', y='Dollars')+
#       geom_line()+
#       theme_bw())}
#   })
#   
# hist <- reactiveVal()
  
  create_deck <- function(num = 1) {
    deck <- expand.grid(values = c(2:10, "A", "Q", "K", "J"), suits = c("S", "C", "D", "H"), stringsAsFactors = F)
    single <- deck
    if (num > 1) {
      for (i in 1:(num - 1)) {
        deck <- rbind(deck, single)
      }
    }
    deck$lookup <- paste0(deck$values, deck$suits)
    deck$point <- suppressWarnings(ifelse(!is.na(as.numeric(as.character(deck$values))), as.numeric(as.character(deck$values)),
      ifelse(deck$values %in% c("Q", "K", "J"), 10, 1)
    ))

    deck
  }

  dealer1 <- function(deck) {
    dealer <- deck[sample(nrow(deck), 2, replace = F), ]
    return(dealer)
  }

  player <- function(deck) { #' dealer deck passed'
    player <- deck[sample(nrow(deck), 2, replace = F), ]
    return(player)
  }

  hit_player <- function(player, deck) {
    hit <- deck[sample(nrow(deck), 1, replace = F), ]
    return(rbind(player, hit))
  }

  hit_dealer <- function(dealer, deck) {
    hit <- deck[sample(nrow(deck), 1, replace = F), ]
    return(rbind(dealer, hit))
  }

  update_deck_dealer <- function(x, dealer) {
    deck[!(row.names(deck) %in% row.names(dealer)), ]
  }
  update_deck_user <- function(deck, player) {
    deck[!(row.names(deck) %in% row.names(player)), ]
  }

  update_points <- function(points, gamble, todo) {
    if (todo == "win") {
      points <- points + gamble
    }
    if (todo == "lost") {
      points <- points - gamble
    }
    points
  }



  points <- reactiveVal()

  gamble <- reactive({
    input$gamble
  })

  # create deck
  deck <- create_deck(4)

  user <- reactiveVal(player(deck))
  dealer <- reactiveVal(dealer1(deck))

  observeEvent(points(), { 
    output$points <- renderText({
      paste0("Wallet Left: ", "$", points())
    })
    #hist(data.frame(points = rbind(hist(), points())))
  })

  observeEvent(input$reset, {
    points(100)
  })

  observeEvent(input$hit, {
    disable('split')
    if (sum(user()$point) <= 21) {
      shinyjs::disable("refresh")
      
      user(hit_player(user(), deck))
      deck <<- update_deck_user(deck, user())
    }
    if (sum(user()$point) > 21) {
      points(update_points(points(), gamble(), "lost"))
      print(points())
      shinyjs::enable("refresh")
      shinyjs::disable("hit")
      shinyjs::disable("stay")

      

      sendSweetAlert(
        session = session,
        title = "BUST",
        text = paste0("You lost:$ ", gamble(), " -- Start a new one Buster!"),
        type = "error"
      )
      while (sum(dealer()$point) < 17) {
        dealer(hit_dealer(dealer(), deck))
        deck <<- update_deck_dealer(deck, dealer())
      }
      v(1)
    }
    if (sum(user()$point) == 21 | ((any(user()$value %in% c("A")) & (sum(user()$point) - 1) == 10))) {
      points(update_points(points(), gamble(), "win"))
      shinyjs::enable("refresh")
      shinyjs::disable("hit")
      shinyjs::disable(("stay"))

      sendSweetAlert(
        session = session,
        title = "You WON",
        text = paste0("You Won:$ ", gamble(), " Keep the Streak!"),
        type = "success"
      )

      
      while (sum(dealer()$point) < 17) {
        dealer(hit_dealer(dealer(), deck))
        deck <<- update_deck_dealer(deck, dealer())
      }
      v(1)
    }
  })

  # define win logic when player hasnt won and is under 21

  observeEvent(input$stay, {
    while (sum(dealer()$point) < 17) {
      dealer(hit_dealer(dealer(), deck))
      deck <<- update_deck_dealer(deck, dealer())
    }
    v(1)
    if (sum(user()$point) == 21 | (any(user()$value %in% c("A")) & (sum(user()$point) - 1) == 10) |
      (sum(user()$point) > sum(dealer()$point) & sum(user()$point) < 21) | (sum(dealer()$point) > 21)) {
      points(update_points(points(), gamble(), "win"))

      shinyjs::enable("refresh")
      shinyjs::disable("hit")
      shinyjs::disable(("stay"))
      sendSweetAlert(
        session = session,
        title = "You WON",
        text = paste0("You Won:$ ", gamble(), " Keep the Streak!"),
        type = "success"
      )
    } else if ((any(dealer()$value %in% c("A")) & (sum(dealer()$point) - 1 == 10)) |
      (sum(user()$point > 21)) |
      ((sum(user()$point) < sum(dealer()$point)) & (sum(dealer()$point) != sum(user()$point)) & (sum(dealer()$point) <= 21))) {
      
      points(update_points(points(), gamble(), "lost"))

      shinyjs::enable("refresh")
      shinyjs::disable("hit")
      shinyjs::disable(("stay"))
      sendSweetAlert(
        session = session,
        title = "You Lost",
        text = paste0("You lost:$ ", gamble(), " -- Start a new one Buster!"),
        type = "error"
      )
    } else if (sum(dealer()$point) == sum(user()$point)) {
      sendSweetAlert(
        session = session,
        title = "Tie",
        text = paste0("Tie! -- Start a new one!"),
        type = "info"
      )
    }
  })


  observeEvent(input$refresh, {
    shinyjs::enable("hit")
    shinyjs::enable("stay")
    shinyjs::enable("refresh")
    #disable('split')
    if (nrow(deck) <= 10) {
      deck <<- create_deck(4)
    }
    user(player(deck))
    dealer(dealer1(deck))
    deck <<- update_deck_dealer(deck, dealer())
    deck <<- update_deck_user(deck, user())
    output$deal <- renderText({''})
    v(0)
    x(0)
print(nrow(deck))
  })

  observeEvent(get_cookie("points"), {
  
    if (is.na(as.numeric(get_cookie("points")))) {
      points(100)
      cookies::set_cookie(
        cookie_name = "points",
        cookie_value = points()
      )
      output$points <- renderText({
        "Setup Point Cookie"
      })
    } else {
      points(as.numeric(get_cookie("points")))
    }
    
  })

  observeEvent(points(), {
    cookies::set_cookie(
      cookie_name = "points",
      cookie_value = points()
    )
  })
  
  observeEvent(user(),{
    shinyjs::disable('split')
    if(is.data.frame(user())) {
       if(user()$point[1] == user()$point[2] & x() == 0 & nrow(user())==2){
      shinyjs::enable("split")
      }
    }
    if((sum(user()$point) == 21 | ((any(user()$value %in% c("A")) & (sum(user()$point) - 1) == 10)))){
      points(update_points(points(), gamble(), "win"))
      
      shinyjs::enable("refresh")
      shinyjs::disable("hit")
      shinyjs::disable(("stay"))
      sendSweetAlert(
        session = session,
        title = "You WON",
        text = paste0("You Won:$ ", gamble(), " Keep the Streak!"),
        type = "success"
      )
    }
  })
  
  
  output$user <- renderUI({
    
  if(x()==0){ 
    return(victim())}
    else{
      return(split()[[1]])}
    
    })
  
  victim <- reactive({
    base <- "<div class='hhand-compact active-hand'>"
    
    for (i in 1:nrow(user())) {
      base <- paste0(base, "<img class='card' src='cards/", user()$lookup[i], ".svg' style = width:150px>")
    }
    HTML(base)
  })
  

 split <-  eventReactive(input$split,{
    user(list(user()[1,],user()[2,]))
    
    base <-"<div class='hhand-compact active-hand'>"
    cards <- ""
    for (i in 1:length(user())) {
      cards <- paste0(cards,base)
      for(j in 1:nrow(user()[[i]])){
        cards <- paste0(cards, "<img class='card' src='cards/", user()[[i]]$lookup[j], ".svg' style = width:150px>")
      }
    }
    
   list( HTML(cards))
    
  })
 
 
 
  x <- reactiveVal(0)
observeEvent(input$split,{
  x(1)
})
  
  
  
  
  output$play <- renderText({
    if ((any(user()$value %in% c("A")) & sum(user()$point) - 1 == 10)) {
      paste("Player Score:", sum(user()$point) + 10)
    } else {
      paste0("Player Score: ", sum(user()$point))
    }
  })



  output$dealer <- renderUI({
    if (v() == 0) {
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

  v <- reactiveVal(0)
}


# Run the application
shinyApp(ui = ui, server = server)
