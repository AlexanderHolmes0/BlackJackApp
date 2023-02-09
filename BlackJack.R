deck <- expand.grid(Values=c(2:10,'A','Q','K','J'), suits = c('S','C','D','H'))
win <- 21

dealer <- deck[sample(1:52,2, replace=F),]

newdeck<- deck[-as.numeric(rownames(dealer))]

player <- deck[sample(nrow(newdeck),2, replace=F),]

newdeck <- deck[-as.numeric(rownames(player))]


dealer$point <- ifelse(!is.na(as.numeric(as.character(dealer$Values))), as.numeric(as.character(dealer$Values)),
                       ifelse(dealer$Values %in% c('Q','K','J'), 10, 1 ))

player$point <- ifelse(!is.na(as.numeric(as.character(player$Values))), as.numeric(as.character(player$Values)),
                       ifelse(player$Values %in% c('Q','K','J'), 10, 1 ))

if(sum(dealer$point) > sum(player$point)){
  "Dealer Wins"
} else if (sum(dealer$point) == sum(player$point)) {
  "Tie!"
} 
