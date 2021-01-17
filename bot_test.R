library(cfbscrapR)
library(gt)
library(MASS)
library(rtweet)
library(tidyverse)
source("R/bot_functions.R")
source("R/helpers.R")

options(dplyr.summarise.inform = FALSE)
#source("bot.R")
team_info <- cfb_team_info()
all_games <- cfb_game_info(2020,week = 1, season_type = "postseason") #week 15 done
all_games <- all_games %>%
  filter(home_team %in% team_info$school,
         away_team %in% team_info$school) %>%
  filter(!is.na(home_post_win_prob))# %>%
# no pbp info for week 3 Duke. Week 6 USF.
  #filter(home_team != "Florida Atlantic")
old_plays <- readRDS("data/old_plays_all.RDS")
for (game_row in 1:nrow(all_games)) {
  game <- all_games[game_row,]
  plays <- get_data(game)
  message(glue::glue("{game$game_id} \nHome: {game$home_team}"))
  for (i in 1:nrow(plays)) {
    temp <- plays %>%
      slice(i) %>%
      make_tidy_data(punt_df)
    old_plays <- bind_rows(old_plays,temp)
  }
}
saveRDS(old_plays,"data/old_plays_all.RDS")

# df <- cfb_game_info(2020,week = 16, team = "Rutgers")
# plays <- get_data(df)
# for (i in 1:nrow(plays)) {
#
#   plays %>%
#     slice(i) %>%
#     tweet_play()
#   print(Sys.time())
#   Sys.sleep(120)
# }
#
# old_plays <- tibble()
# for (i in 1:nrow(plays)) {
#   temp <- plays %>%
#     slice(i) %>%
#     make_tidy_data(punt_df)
#   old_plays <- bind_rows(old_plays,temp)
# }
# view(old_plays)
#
# old_plays2 <- map_df(1:nrow(plays),function(x) {
#   plays[x,] %>% make_tidy_data(punt_df)
# })
# saveRDS(old_plays,"old_plays.RDS")
