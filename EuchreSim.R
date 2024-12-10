

# Build the deck

initialize_deck <- function() {
  suits <- c("Clubs", "Diamonds", "Hearts", "Spades")
  values <- c("9", "10", "J", "Q", "K", "A")
  deck <- expand.grid(Value = values, Suit = suits)
  return(deck)
}

deck <- initialize_deck()

# Shuffle and dealing

deal_hand <- function(deck) {
  shuffled <- deck[sample(nrow(deck)), ]
  hands <- list(
    player1 = shuffled[1:5, ],
    player2 = shuffled[6:10, ],
    player3 = shuffled[11:15, ],
    player4 = shuffled[16:20, ]
  )
  kitty <- shuffled[21:24, ]
  return(list(hands = hands, kitty = kitty))
}

# Start building out a round

assign_dealer <- function(players, round_number) {
  dealer <- (round_number - 1) %% length(players) + 1
  return(dealer)
}

# Need to assign ranks to the cards for trump decision making

rank_card <- function(card, trump_suit) {
  suit <- card$Suit
  value <- card$Value
  
  if (suit == trump_suit) {
    if (value == "J") return(13)
    if (value == "A") return(11)
    if (value == "K") return(10)
    if (value == "Q") return(9)
    if (value == "10") return(8)
    if (value == "9") return(7)
  }
  
  if (suit != trump_suit) {
    if (value == "A") return(6)
    if (value == "K") return(5)
    if (value == "Q") return(4)
    if (value == "J") return(3)
    if (value == "10") return(2)
    if (value == "9") return(1)
    
    if (trump_suit == "Diamonds" && suit == "Hearts" && value == "J") return(12)
    if (trump_suit == "Hearts" && suit == "Hearts" && value == "J") return(12)
    if (trump_suit == "Spades" && suit == "Clubs" && value == "J") return(12)
    if (trump_suit == "Clubs" && suit == "Spades" && value == "J") return(12)
  }
}

assign_card_ranks <- function(hands, trump_suit) {
  
  for (player in seq_along(hands)) {
    hand <- hands[[player]]
    hand$Rank <- sapply(1:nrow(hand), function(i) rank_card(hand[i, ], trump_suit))
  }
  return(hands)
}
  

# Logic for which trump to pick

pick_trump <- function(player, available_suits, threshold = 3) {
  hand <- hands[[player]]
  suit_counts <- table(hand$Suit)
  
  # Check if any available suit meets the threshold, then they will be picked
  for (suit in available_suits) {
    if (suit %in% names(suit_counts) && suit_counts[suit] >= threshold) {
      return(suit)
    }
  }
  return(NULL)
}

call_trump <- function(players, dealer, remaining_deck, hands) {
  order <- c((dealer %% 4 + 1):4, 1:dealer)[1:length(players)]
  
  turn_up <- kitty[1,]
  turn_up_suit <- turn_up$Suit
  
  for (i in seq_along(order)) {
    is_dealer_team <- (i %% 2 == 0)
    player <- order[i]
    
    # Now we need the logic for anyone ordering up the turn up -- this needs to be tested
    
    threshold <- ifelse(is_dealer_team, 2, 3) # Initial cutoff can be 2 if you are dealer's team and 3 ow
    
    trump_suit <- pick_trump(player, c(turn_up_suit), threshold)
    
    if (!is.null(trump_suit)) {
      message(paste("Player", player, "chooses", turn_up_suit, "as trump."))
      return(list(trump_suit = turn_up_suit, trump_set = TRUE))
    } else {
      message(paste("Player", player, "passes on", turn_up_suit, "as trump."))
    }
  }
  
  # When the turn up is flipped down
  for (i in seq_along(order)) {
    player <- order[i]
    is_dealer <- (player == dealer)
    available_suits <- setdiff(c("Clubs", "Diamonds", "Hearts", "Spades"), turn_up_suit)
    
    # Dealer must pick if it gets back to them
    if (is_dealer) {
      hand <- hands[[player]]
      suit_counts <- table(hand$Suit)
      dealer_suit <- names(which.max(suit_counts))
      if (dealer_suit == turn_up_suit) {
        available_suits <- setdiff(deck$Suit, turn_up_suit)  # Remove turn-up suit from options
        dealer_suit <- available_suits[1]
      }
      message(paste("Player", player, "is forced to choose", dealer_suit, "as trump."))
      return(list(trump_suit = dealer_suit, trump_set = TRUE))
    }
    
    # Non-dealer players choose based on available suits
    chosen_suit <- pick_trump(player, available_suits, 3)
    if (!is.null(chosen_suit)) {
      message(paste("Player", player, "chooses", chosen_suit, "as trump."))
      return(list(trump_suit = chosen_suit, trump_set = TRUE))
    } else {
      message(paste("Player", player, "passes on second round."))
    }
  }
  stop("Error: Dealer did not pick a trump suit.") #GPT says to include this for debug
}
    

players <- c("player1", "player2", "player3", "player4")
round_number <- 1

dealer <- assign_dealer(players, round_number)

dealt <- deal_hand(deck)
dealt

hands <- dealt$hands
kitty <- dealt$kitty

trump_decision <- call_trump(players, dealer, kitty, hands)

