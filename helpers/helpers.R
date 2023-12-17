create_deck <- function(num = 1) {
  deck <- expand.grid(values = c(2:10, "A", "Q", "K", "J"), suits = c("S", "C", "D", "H"), stringsAsFactors = FALSE)
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

dealer1 <- function(deck,player) {
  deck <- update_deck_user(deck, player)
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

update_deck_dealer <- function(deck, dealer) {
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

keep_going_dealer <- function(dealer) {
  if ((sum(dealer$point) < 17) | (any(dealer$value %in% c("A")) & (sum(dealer$point) - 1 < 10))) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

did_dealer_win <- function(dealer) {
  if ((sum(dealer$point) == 21) | (any(dealer$value %in% c("A")) & (sum(dealer$point) - 1) == 10)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

keep_going_user <- function(user) {
  if ((sum(user$point) < 21) | (any(user$value %in% c("A")) & (sum(user$point) - 1 < 10))) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

did_user_win <- function(user) {
  if ((sum(user$point) == 21) | (any(user$value %in% c("A")) & (sum(user$point) - 1) == 10)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

user_dealer_comparison <- function(user, dealer) {
  if (did_user_win(user)){
    return(TRUE)
  }
  else if (any(user$value %in% c("A")) & sum(user$point) - 1 <= 10 & sum(dealer$point) <= 21){
    return(sum(user$point) + 10  > sum(dealer$point))
  }
  else if (sum(user$point) > sum(dealer$point) & sum(user$point) <= 21) {
    return(TRUE)
  }
  else if (sum(user$point) <= 21 & sum(dealer$point) > 21){
    return(TRUE)
  }
  else {
    return(FALSE)
  }
}
