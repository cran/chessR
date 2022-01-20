## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message=FALSE,
  warning=FALSE
)

## ----gh-installation----------------------------------------------------------
# devtools::install_github("JaseZiv/chessR")
library(chessR)

## ----packages_for_eda, include=FALSE------------------------------------------
library(ggplot2)
library(dplyr)
library(stringr)
library(lubridate)

## ----get_raw_chessdotcom------------------------------------------------------
# function to extract chess.com game data
chessdotcom_game_data_all_months <- get_raw_chessdotcom(usernames = "JaseZiv")
glimpse(chessdotcom_game_data_all_months)

## ----get_raw_chessdotcom_months-----------------------------------------------
# function to extract chess.com game data
chessdotcom_hikaru_recent <- get_raw_chessdotcom(usernames = "Hikaru", year_month = c(202104:202105))
glimpse(chessdotcom_hikaru_recent)

## ----inspect_raw_licjess------------------------------------------------------
# function to extract lichess game data
lichess_game_data <- get_raw_lichess("Georges")
glimpse(lichess_game_data)

## ----get_analysis-------------------------------------------------------------
chess_analysis_single <- get_game_data("JaseZiv")

## ----inspect_analysis---------------------------------------------------------
glimpse(chess_analysis_single)

## ----get_chessdotcom_leaders--------------------------------------------------
daily_leaders <- chessdotcom_leaderboard(game_type = "daily")
glimpse(daily_leaders)

## ----get_lichess_leaders, eval=FALSE------------------------------------------
#  lichess_leaders <- lichess_leaderboard(top_n_players = 10, speed_variant = "blitz")
#  glimpse(lichess_leaders)

## ----num_moves----------------------------------------------------------------
# function to extract the number of moves in each game
chessdotcom_game_data_all_months$nMoves <- return_num_moves(moves_string = chessdotcom_game_data_all_months$Moves)

# inspect output
head(chessdotcom_game_data_all_months[, c("Moves", "nMoves")])

## ----game_ending--------------------------------------------------------------
# function to extract the ending of chess.com data
chessdotcom_game_data_all_months$Ending <- mapply(get_game_ending,
                                                  termination_string = chessdotcom_game_data_all_months$Termination,
                                                  white = chessdotcom_game_data_all_months$White,
                                                  black = chessdotcom_game_data_all_months$Black)

# inspect output
head(chessdotcom_game_data_all_months[, c("Termination", "White", "Black", "Ending")])

## ----get_winner---------------------------------------------------------------
# function to extract the winner of each game
chessdotcom_game_data_all_months$Winner <- get_winner(result_column = chessdotcom_game_data_all_months$Result, 
                                     white = chessdotcom_game_data_all_months$White, 
                                     black = chessdotcom_game_data_all_months$Black)

# inspect output
head(chessdotcom_game_data_all_months[, c("White", "Black", "Result", "Winner")])

## ----popular_times------------------------------------------------------------
chessdotcom_game_data_all_months %>% 
  count(TimeClass) %>% 
  ggplot(aes(x= reorder(TimeClass,n), y= n)) +
  geom_col(fill = "steelblue", colour = "grey40", alpha = 0.7) +
  labs(x= "Game Style", y= "Number of Games") +
  ggtitle("WHICH TIME CLASSES ARE PLAYED MOST BY USER") +
  coord_flip() +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank())

## ----user_result--------------------------------------------------------------
chessdotcom_game_data_all_months %>%
  mutate(MonthEnd = paste(year(EndDate), str_pad(lubridate::month(ymd(EndDate)), 2, side = "left", pad = "0"), sep = "-")) %>% 
  mutate(UserResult = ifelse(Winner == Username, "Win", ifelse(Winner == "Draw", "Draw", "Loss"))) %>% 
  group_by(MonthEnd, UserResult) %>% 
  summarise(n = n()) %>% 
  mutate(WinPercentage = n / sum(n)) %>% 
  filter(UserResult == "Win") %>% 
  ggplot(aes(x= MonthEnd, y= WinPercentage, group=1)) +
  geom_line(colour= "steelblue", size=1) +
  geom_hline(yintercept = 0.5, linetype = 2, colour = "grey40") +
  scale_y_continuous(limits = c(0,1)) +
  labs(x= "Month Game Ended", y= "Win %") +
  ggtitle("MONTHLY WINNING %") +
  theme_minimal()

## ----monthly_elo, fig.width=9-------------------------------------------------
chessdotcom_game_data_all_months %>%
  filter(TimeClass %in% c("blitz", "daily")) %>% 
  mutate(UserELO = as.numeric(ifelse(Username == White, WhiteElo, BlackElo))) %>% 
  mutate(MonthEnd = paste(year(EndDate), str_pad(lubridate::month(ymd(EndDate)), 2, side = "left", pad = "0"), sep = "-")) %>% 
  group_by(MonthEnd, TimeClass) %>% 
  summarise(AverageELO = mean(UserELO, na.rm = T)) %>% 
  ggplot(aes(x= MonthEnd, y= AverageELO, group=1)) +
  geom_line(colour= "steelblue", size=1) +
  labs(x= "Month Game Ended", y= "Average ELO") +
  ggtitle("MONTHLY AVERAGE ELO RATING") +
  facet_wrap(~ TimeClass, scales = "free_y", ncol = 1) +
  theme_minimal()

## ----opponet_elo_results------------------------------------------------------
chessdotcom_game_data_all_months %>% 
  mutate(OpponentELO = as.numeric(ifelse(Username == White, BlackElo, WhiteElo)),
         UserResult = ifelse(Winner == Username, "Win", ifelse(Winner == "Draw", "Draw", "Loss"))) %>% 
  filter(TimeClass %in% c("blitz", "daily")) %>% 
  ggplot(aes(x= OpponentELO, fill = UserResult)) +
  geom_density(alpha = 0.3) +
  ggtitle("HOW DO WE FARE AGAINST DIFFERENT ELOs?") +
  facet_wrap(~ TimeClass, scales = "free", ncol = 1) +
  theme_minimal()


