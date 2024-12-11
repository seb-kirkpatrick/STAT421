library(tidyverse)

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
  
  # All there values are arbitrary, but cooked up by me and my buddies
  
  # Starting with left bar works instead of re-writing off-Jack's value
  if (trump_suit == "Diamonds" && suit == "Hearts" && value == "J") return(20)
  if (trump_suit == "Hearts" && suit == "Diamonds" && value == "J") return(20)
  if (trump_suit == "Spades" && suit == "Clubs" && value == "J") return(20)
  if (trump_suit == "Clubs" && suit == "Spades" && value == "J") return(20)
  
  if (suit == trump_suit) {
    if (value == "J") return(22)
    if (value == "A") return(18)
    if (value == "K") return(16)
    if (value == "Q") return(15)
    if (value == "10") return(13)
    if (value == "9") return(12)
  }
  
  if (suit != trump_suit) {
    if (value == "A") return(9)
    if (value == "K") return(6)
    if (value == "Q") return(4)
    if (value == "J") return(3)
    if (value == "10") return(2)
    if (value == "9") return(1)
  }
}

assign_card_ranks <- function(hands, kitty = NULL, trump_suit) {
  
  for (player in seq_along(hands)) {
    hand <- hands[[player]]
    
    hand$Rank <- sapply(1:nrow(hand), function(i) rank_card(hand[i, ], trump_suit))
    hand <- hand[order(-hand$Rank),]
    hands[[player]] <- hand
  }
  
  ranked_kitty <- NULL
  if (!is.null(kitty)) {
    kitty$Rank <- sapply(1:nrow(kitty), function(i) rank_card(kitty[i, ], trump_suit))
    ranked_kitty <- kitty
  }
  
  return(list(hands = hands, ranked_kitty = ranked_kitty))
}
  

# Logic for which trump to pick

pick_trump <- function(player, available_suits, hands, threshold) {
  hand <- hands[[player]]
  
  best_suit <- NA
  best_rank_sum <- 0
  
  # Check if any available suit meets the threshold, then they will be picked
  for (suit in available_suits) {
    
    ranked_data <- assign_card_ranks(list(hands[[player]]), suit, kitty = NULL)
    ranked_hand <- ranked_data$hands[[1]]
    
    rank_sum <- sum(ranked_hand$Rank)
    
    if (rank_sum >= threshold && rank_sum > best_rank_sum) {
      best_suit <- suit
      best_rank_sum <- rank_sum
    }
  }
  return(best_suit)
}

dealer_hand <- function(dealer, hands, turn_up, turn_up_suit) {
  
  dealer_hand <- hands[[dealer]]
  dealer_hand <- rbind(dealer_hand, turn_up) # Add the turn up to the dealer's hand
  
  dealer_hand$Rank <- sapply(1:nrow(dealer_hand), function(i) rank_card(dealer_hand[i, ], turn_up_suit))
  # Goal will be to drop a suit here, and, if not possible, then just the lowest ranked card
  rank_threshold <- 9 # Set the lowest for an ace, do not want to drop a suit if it is an ace
  
  low_rank_indices <- which(dealer_hand$Rank < rank_threshold)
  
  if (length(low_rank_indices) > 0) {
    suits_to_remove <- unique(dealer_hand$Suit[low_rank_indices])
    suit_counts <- table(dealer_hand$Suit)
    
    for (suit in suits_to_remove) {
      if (suit_counts[suit] == 1) {
        card_index <- which(dealer_hand$Suit == suit & dealer_hand$Rank < rank_threshold)[1] # Remove the single, non-ace card in the suit
        dealer_hand <- dealer_hand[-card_index, ]
        hands[[dealer]] <- dealer_hand
        #message("Dealer picks up the turn-up card and removes a low-ranking card to eliminate a suit.")
        return(hands)
      }
    }
  }
  
  lowest_rank <- min(dealer_hand$Rank) # If dealer can;t drop a suit, then just drop lowest
  lowest_indices <- which(dealer_hand$Rank == lowest_rank)
  dealer_hand <- dealer_hand[-lowest_indices[1], ]
  
  hands[[dealer]] <- dealer_hand  # Update the dealer's hand
  #message("Dealer picks up the turn-up card and removes the overall lowest-ranking card.")
  return(hands)
}

call_trump <- function(players, dealer, kitty, hands, threshold = 55) {
  order <- c((dealer %% 4 + 1):4, 1:dealer)[1:length(players)]
  trump_decisions <- data.frame(player = integer(0), round = character(0), trump_suit = character(0), rank_sum = numeric(0))
  
  turn_up <- kitty[1,]
  turn_up_suit <- turn_up$Suit
  turn_up_rank <- rank_card(turn_up, turn_up_suit)
  
  for (i in seq_along(order)) {
    player <- order[i]
    is_dealer <- (player == dealer)
    is_dealer_team <- (!is_dealer && (i %% 2 == 0))
    
    # Now we need the logic for anyone ordering up the turn up -- this needs to be tested
    
    hand <- hands[[player]]
    hand_ranks <- sapply(1:nrow(hand), function(i) rank_card(hand[i, ], turn_up_suit))
    lowest_rank <- min(hand_ranks)
    
    adjusted_threshold <- if (is_dealer_team) {
      threshold - turn_up_rank + 7 # 7 is a random penalty, player_3 called too much w/o it
    } else if (is_dealer) {
      threshold - turn_up_rank + lowest_rank
    } else {
      threshold
    } # This will only be used for the turn-up decision making
    
    trump_suit <- pick_trump(player, c(turn_up_suit), hands, adjusted_threshold)
    
    if (!is.na(trump_suit)) {
      trump_decisions <- rbind(trump_decisions, data.frame(
        player = player,
        round = "turn-up",
        trump_suit = turn_up_suit,
        rank_sum = sum(hand_ranks)
      ))
      
      #message(paste("Player", player, "chooses", turn_up_suit, "as trump."))
      
      hands <- dealer_hand(dealer, hands, turn_up, turn_up_suit) # Dealer discards a card
      
      ranked_hands <- lapply(hands, function(hand) {
        hand$Rank <- sapply(1:nrow(hand), function(i) rank_card(hand[i, ], turn_up_suit))
        hand <- hand[order(hand$Rank, decreasing = TRUE), ]
        hand
      }) # Re-rank hands for round-sim
      
      return(list(decision = trump_decisions, hands = ranked_hands, trump_card = turn_up))
    } else {
      #message(paste("Player", player, "passes on", turn_up_suit, "as trump."))
    }
  }
  
  # When the turn up is flipped down
  for (i in seq_along(order)) {
    player <- order[i]
    is_dealer <- (player == dealer)
    available_suits <- setdiff(c("Clubs", "Diamonds", "Hearts", "Spades"), turn_up_suit)
    
    # Dealer must pick if it gets back to them
    if (is_dealer) {
      
      ranked_data <- lapply(available_suits, function(suit) assign_card_ranks(list(hands[[player]]), suit, kitty = NULL))
      ranked_hands <- lapply(ranked_data, function(data) data$hands[[1]])
      suit_rank_sums <- sapply(ranked_hands, function(hand) sum(hand$Rank))
      
      dealer_suit <- available_suits[which.max(suit_rank_sums)] # Dealer is forced to pick a trump
      
      trump_decisions <- rbind(trump_decisions, data.frame(
        player = player,
        round = "flip-down",
        trump_suit = dealer_suit,
        rank_sum = max(suit_rank_sums)
      ))
      
      ranked_hands <- lapply(hands, function(hand) {
        hand$Rank <- sapply(1:nrow(hand), function(i) rank_card(hand[i, ], dealer_suit))
        hand <- hand[order(hand$Rank, decreasing = TRUE), ]
        hand
      })
      
      #message(paste("Player", player, "is forced to choose", dealer_suit, "as trump."))
      return(list(decision = trump_decisions, hands = ranked_hands, trump_card = NA))
    }
    
    # Non-dealer players choose based on available suits and regular threshold
    chosen_suit <- pick_trump(player, available_suits, hands, threshold)
    if (!is.na(chosen_suit)) {
      
      hand_ranks <- sapply(1:nrow(hands[[player]]), function(i) rank_card(hands[[player]][i, ], chosen_suit))
      trump_decisions <- rbind(trump_decisions, data.frame(
        player = player,
        round = "flip-down",
        trump_suit = chosen_suit,
        rank_sum = sum(hand_ranks)
      ))
      
      ranked_hands <- lapply(hands, function(hand) {
        hand$Rank <- sapply(1:nrow(hand), function(i) rank_card(hand[i, ], chosen_suit))
        hand <- hand[order(hand$Rank, decreasing = TRUE), ]
        hand
      })
      
      #message(paste("Player", player, "chooses", chosen_suit, "as trump."))
      return(list(decision = trump_decisions, hands = ranked_hands, trump_card = NA))
    } else {
      #message(paste("Player", player, "passes on second round."))
    }
  }
  stop("Error: Dealer did not pick a trump suit.") # GPT says to include this for debug
}

update_off_jack <- function(hands, trump_suit) {
  
  for (player in 1:length(hands)) {
    hand <- hands[[player]]
    off_jack <- hand[hand$Rank == 20]
    
    if (nrow(off_jack) > 0) { # If the player has the off-Jack
      hands[[player]]$Suit[hands[[player]]$Rank == 20] <- trump_suit
    }
    
  }
  return(hands)
}

simulate_round <- function(players, dealer, kitty, hands, threshold = 55) {
  trump_info <- call_trump(players, dealer, kitty, hands, threshold)
  trump_suit <- trump_info$decision$trump_suit
  call <- trump_info$decision
  team_call <- ifelse(call$player %in% c(1, 3), 1, 2)
  
  hands <- trump_info$hands
  hands <- update_off_jack(hands, trump_suit)
  
  lead_player <- (dealer %% length(players)) + 1
  played_cards <- vector("list", length(players))
  
  trick_data <- data.frame(Trick = integer(), Trump = character(), Team_Call = integer(), Player_Call = integer(), Dealer = integer(), 
                           Lead_Player = integer(), Trick_Winner = integer(), 
                           Player = integer(), Card = character(), Suit = character(), 
                           Rank = integer())
  
  trick_summary <- data.frame(Team_Call = integer(), Player_Call = integer(), Trick = integer(), Trick_Winner = integer())
  
  # Options for lead
  
  play_highest_non_trump <- function(hand, trump_suit) {
    non_trump_cards <- hand[hand$Suit != trump_suit, ]
    if (nrow(non_trump_cards) > 0) {
      return(non_trump_cards[which.max(non_trump_cards$Rank), ])
    }
    return(NULL)
  }
  
  # Options for response card
  
  follow_suit <- function(hand, suit) {
    suit_cards <- hand[hand$Suit == suit, ]
    if (nrow(suit_cards) > 0) {
      return(suit_cards[which.min(suit_cards$Rank), ])  # Play the lowest card of the same suit
    }
    return(NULL)
  }
  
  play_lowest_card <- function(hand) {
    return(hand[which.min(hand$Rank), ])
  }
  
  can_beat_card <- function(hand, trick_cards, trump_suit) {
    lead_suit <- trick_cards[1, "Suit"]
    card_to_beat <- trick_cards[1, "Rank"]
    
    suit_cards <- hand[hand$Suit == lead_suit, ]
    winning_cards <- suit_cards[suit_cards$Rank > card_to_beat, ]
    if (nrow(winning_cards) > 0) {
      return(winning_cards[which.min(winning_cards$Rank), ])
    }
    return(NULL)
  }
  
  determine_trick_winner <- function(trick_cards, trump_suit) {
    trump_cards <- trick_cards[trick_cards$Suit == trump_suit, ]
    if (nrow(trump_cards) > 0) { # If trump was played
      winning_card <- trump_cards[which.max(trump_cards$Rank), ]
    } else {
      # We need to make it so a led king is not beat by an ace of a different off-suit
      lead_suit <- trick_cards[1, "Suit"]
      lead_suit_cards <- trick_cards[trick_cards$Suit == lead_suit, ]
      winning_card <- lead_suit_cards[which.max(lead_suit_cards$Rank), ]
    }
    return(winning_card$Player)
  }
  
  for (trick in 1:5) {
    played_cards <- integer(0)
    
    trick_cards <- data.frame(Player = integer(), Card = character(), Suit = character(), Rank = integer())
    
    player_order <- ((lead_player - 1 + 0:(length(players) - 1)) %% length(players)) + 1
    
    for (i in 1:length(players)) {
      player <- player_order[i]
      hand <- hands[[player]]
      team_called_trump <- (ifelse(player %in% c(1, 3), 1, 2) == team_call)
      
      #print(player)
      #print(hand)
      #print(team_called_trump)
      
      #print(played_cards)
      #print(trick_cards)
      
      card_to_play <- NULL
      
      if (i == 1) { # Logic for the lead
        
        if (team_called_trump) { # Lead had the right
          
          jack_of_trump <- hand[hand$Rank == 22 & hand$Suit == trump_suit, ]
          if (nrow(jack_of_trump) > 0) {
            card_to_play <- jack_of_trump
            #message(paste("Player", player, "leads with the Jack of", trump_suit))
            
          } else { # Lead has trump
            
            trump_cards <- hand[hand$Rank >= 10, ]
            if (nrow(trump_cards) > 0) {
              card_to_play <- trump_cards[which.max(trump_cards$Rank), ]
              #message(paste("Player", player, "plays highest trump:", card_to_play$Value, "of", trump_suit))
              
            } else { # Lead does not have trump
              
              card_to_play <- play_highest_non_trump(hand, trump_suit)
              #message(paste("Player", player, "plays highest non-trump:", card_to_play$Value, "of", card_to_play$Suit))
              
            }
          }
        }
        
        # Now if the team that did not call has to lead
        
        else {
          non_trump_cards <- hand[hand$Rank <= 9, ]
          
          if (nrow(non_trump_cards) > 0) {
          card_to_play <- non_trump_cards[which.max(non_trump_cards$Rank), ]
          #message(paste("Player", player, "leads highest card <= 9:", card_to_play$Value, "of", card_to_play$Suit))
          } else {
            # If no non-trump cards, play the lowest card left in the hand
            card_to_play <- hand[which.min(hand$Rank), ]
            #message(paste("Player", player, "leads lowest card:", card_to_play$Value, "of", card_to_play$Suit))
          }
        }
        
      } else {
        
        lead_card <- trick_cards[1,]
        
        if (lead_card$Suit %in% hand$Suit) {
          
          card_to_beat <- can_beat_card(hand, trick_cards, trump_suit)
          
          if (!is.null(card_to_beat)) {
            #message(paste("Player", player, "beats the lead card with", card_to_beat$Value, "of", card_to_beat$Suit))
            card_to_play <- card_to_beat
          } else {
            card_to_play <- follow_suit(hand, lead_card$Suit)
            #message(paste("Player", player, "plays lowest card of the lead suit:", card_to_play$Value, "of", card_to_play$Suit))
          }
        } else {
          # Check if the player has already played
          if (!player %in% played_cards) {
            trump_cards <- hand[hand$Suit == trump_suit, ]
            if (nrow(trump_cards) > 0) {
              card_to_play <- trump_cards[which.min(trump_cards$Rank), ]
              #message(paste("Player", player, "trumps in with", card_to_play$Value, "of", trump_suit))
            } else {
              card_to_play <- play_lowest_card(hand)
              #message(paste("Player", player, "plays lowest card:", card_to_play$Value, "of", card_to_play$Suit))
            }
          } else {
            card_to_play <- play_lowest_card(hand)
            #message(paste("Player", player, "plays lowest card:", card_to_play$Value, "of", card_to_play$Suit))
          }
        }
      }
      
      trick_cards <- rbind(trick_cards, data.frame(Player = player, 
                                                   Card = card_to_play$Value, 
                                                   Suit = card_to_play$Suit, 
                                                   Rank = card_to_play$Rank, 
                                                   Trump = card_to_play$Suit == trump_suit))
      
      hands[[player]] <- hands[[player]][!(hands[[player]]$Rank == card_to_play$Rank & hands[[player]]$Suit == card_to_play$Suit), ]
      
      played_cards <- c(played_cards, player)
    }
      
    trick_winner <- determine_trick_winner(trick_cards, trump_suit)
    trick_data <- rbind(trick_data, data.frame(Trick = trick, 
                                               Trump = trump_suit, 
                                               Team_Call = team_call,
                                               Player_Call = call$player,
                                               Dealer = dealer, 
                                               Lead_Player = lead_player, 
                                               Trick_Winner = trick_winner, 
                                               Player = trick_cards$Player, 
                                               Card = trick_cards$Card, 
                                               Suit = trick_cards$Suit, 
                                               Rank = trick_cards$Rank))
    
    trick_summary <- rbind(trick_summary, data.frame(Team_Call = team_call, Player_Call = call$player,Trick = trick, Trick_Winner = trick_winner))
    
    # Push the lead to who won the trick
    lead_player <- trick_winner
  }
  
  return(list(call = call, trick_data = trick_data, trick_summary = trick_summary))
}

round_summary <- function(trick_summary) {
  team_called_trump <- trick_summary$Team_Call[1]
  team_tricks_won <- 0
  
  for (i in 1:nrow(trick_summary)) {
    if (team_called_trump == 1 && trick_summary$Trick_Winner[i] %in% c(1, 3)) {
      team_tricks_won <- team_tricks_won + 1
    } else if (team_called_trump == 2 && trick_summary$Trick_Winner[i] %in% c(2, 4)) {
      team_tricks_won <- team_tricks_won + 1
    }
  }
  
  if (team_tricks_won == 5) {
    team_points <- 2
  } else if (team_tricks_won >= 3) {
    team_points <- 1
  } else {
    team_points <- -2
  }
  
  summary_row <- data.frame(
    TeamCalled = team_called_trump,
    PlayerCalled = trick_summary$Player_Call[1],
    TricksWon = team_tricks_won,
    Points = team_points
  )
  
  return(summary_row)
}

# Testing

players <- c("player1", "player2", "player3", "player4")
round_number <- 1

dealer <- assign_dealer(players, round_number)

set.seed(740)
dealt <- deal_hand(deck)
dealt

hands <- dealt$hands
kitty <- dealt$kitty

decision <- call_trump(players, dealer, kitty, hands, threshold = 55)
decision$decision

results <- simulate_round(players, dealer, kitty, hands, threshold = 55)
results$call
results$trick_data
sum <- results$trick_summary
sum

round_summary(sum)

# Now for the sim

call_results <- data.frame()
players <- c("player1", "player2", "player3", "player4")
round_number <- 1 # Not going to change the round number here because I more care about the spot respective to the dealer

thresholds <- c(35, 40, 45, 50, 55, 60, 65, 70, 75)

set.seed(740)
for (j in thresholds) {
  for (i in 1:1000) {
    dealt <- deal_hand(deck)
    hands <- dealt$hands
    kitty <- dealt$kitty
    
    decision <- call_trump(players, dealer, kitty, hands, threshold = j)
    
    res <- decision$decision
    res1 <- res |>
      mutate(threshold = j)
    
    call_results <- rbind(call_results, res1)
  }
}

game_results <- data.frame()

set.seed(740)
for (j in thresholds) {
  for (i in 1:1000) {
    dealt <- deal_hand(deck)
    hands <- dealt$hands
    kitty <- dealt$kitty
    
    results <- simulate_round(players, dealer, kitty, hands, threshold = j)
    
    res <- results$trick_summary
    res1 <- round_summary(res)
    res2 <- res1 |>
      mutate(threshold = j)
    
    game_results <- rbind(game_results, res2)
  }
}


head(call_results)

call_results |>
  mutate(Team = ifelse(player %in% c(1,3),1,2)) |>
  group_by(round, Team, threshold) |>
  summarize(
    count = n()
  ) |>
  ungroup() |>
  mutate(
    Round = case_when(
      round == "turn-up" ~ "First Round",
      round == "flip-down" ~ "Second Round"
    )
  ) |>
  ggplot(aes(x = as.factor(threshold), y = count, color = as.factor(Team), group = as.factor(Team))) +
  geom_line(size = 2) +
  facet_wrap(~Round) + 
  labs(x = "Threshold",
       y = "Count",
       color = "Team")

call_results |>
  group_by(round, player, threshold) |>
  summarize(
    count = n()
  ) |>
  ungroup() |>
  mutate(
    Round = case_when(
      round == "turn-up" ~ "First Round",
      round == "flip-down" ~ "Second Round"
    )
  ) |>
  ggplot(aes(x = as.factor(threshold), y = count, color = as.factor(player), group = as.factor(player))) +
  geom_line(size = 2) +
  facet_wrap(~Round) + 
  labs(x = "Threshold",
       y = "Count",
       color = "Player")

call_results |>
  group_by(player, threshold) |>
  summarize(avg_rank = mean(rank_sum)) |>
  ggplot(aes(x = as.factor(threshold), y = avg_rank, color = as.factor(player), group = as.factor(player))) + 
  geom_line(size=2) + 
  labs(x = "Threshold",
       y = "Average Hand Rank",
       color = "Player")

call_results |>
  group_by(player, threshold, round) |>
  summarize(avg_rank = mean(rank_sum)) |>
  ungroup() |>
  mutate(
    Round = case_when(
      round == "turn-up" ~ "First Round",
      round == "flip-down" ~ "Second Round"
    )
  ) |>
  ggplot(aes(x = as.factor(threshold), y = avg_rank, color = as.factor(player), group = as.factor(player))) + 
  geom_line(size=2) + 
  labs(x = "Threshold",
       y = "Average Hand Rank",
       color = "Player") +
  facet_wrap(~Round)

head(game_results)

game_results |>
  group_by(TeamCalled, threshold) |>
  summarize(avg_points = mean(Points)) |>
  ungroup() |>
  ggplot(aes(x = as.factor(threshold), y = avg_points, color = as.factor(TeamCalled), group = as.factor(TeamCalled))) + 
  geom_line(size=2) + 
  labs(x = "Threshold",
       y = "Average Points",
       color = "Team")

game_results |>
  group_by(PlayerCalled, threshold) |>
  summarize(avg_points = mean(Points)) |>
  ungroup() |>
  ggplot(aes(x = as.factor(threshold), y = avg_points, color = as.factor(PlayerCalled), group = as.factor(PlayerCalled))) + 
  geom_line(size=2) + 
  labs(x = "Threshold",
       y = "Average Points",
       color = "Player")

game_results |>
  group_by(TeamCalled, threshold) |>
  summarize(
    count = n(),
    mean_euch = sum(Points == -2) / count
  )