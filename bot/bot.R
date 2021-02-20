library(cfbscrapR)
library(gt)
library(MASS)
library(rtweet)
library(tidyverse)


week <- 1
games<- cfb_game_info(2020,week = week,season_type = "postseason") %>%
  filter(away_team != "Iowa",home_team != "TCU",home_team != "Georgia")


options(dplyr.summarise.inform = FALSE)



source("R/bot_functions.R")
source("R/helpers.R")


live_games <- games %>%
  # Had some trouble working with the times, this was super hacky but works
  mutate(start_time = lubridate::as_datetime(start_date),
         start_time2 = lubridate::force_tz(start_time,tzone = "GMT"),
         start_time3 = lubridate::with_tz(start_time2,tzone = "MST"),
         test = as.character(lubridate::as_date(start_time3)) == as.character(lubridate::today())
         ) %>%
   dplyr::filter(

    # this probably isn't necessary but whatever
    season == 2020,

    # hasn't finished yet
    is.na(away_post_win_prob),

    # happening today (REMOVE THE DAY ADJUSTMENT)
    as.character(lubridate::as_date(start_time3)) == as.character(lubridate::today())

  ) %>%
  dplyr::mutate(
    # there's probably a better way to do this but it seems to work
    current_hour = lubridate::hour(lubridate::now()),
    current_minute = lubridate::minute(lubridate::now()),
    game_hour = lubridate::hour(start_time3),
    game_minute = lubridate::minute(start_time3),
    # has already started
    started = dplyr::case_when(
      current_hour > game_hour ~ 1,
      current_hour == game_hour & current_minute >= game_minute + 10 ~ 1,
      TRUE ~ 0
    )
  ) %>%
  dplyr::filter(started == 1) %>%
  dplyr::select(game_id, home_team, away_team, week)

#While Games are playing
while(nrow(live_games) != 0) {
  plays <- tibble()
  to_tweet <- tibble()
  for (i in 1:nrow(live_games)) {
    Sys.sleep(2)
    plays <- rbind(plays,get_data(live_games[i,]))
  }
  if (nrow(plays != 0)) {
    old_plays <- readRDS("data/old_plays.RDS")
    plays <- plays %>% mutate(old = ifelse(play_id %in% old_plays$play_id,1,0))
    to_tweet <- plays %>% filter(old == 0)
  }
  if (nrow(to_tweet) != 0) {
    for (i in 1:nrow(to_tweet)) {

      play <- to_tweet %>% slice(i)
      message(play$play_id)

      play %>%
        tweet_play()
      old_plays <- old_plays %>%
        bind_rows(play %>% make_tidy_data())
      saveRDS(old_plays,"data/old_plays.RDS")
      message(paste(Sys.time(),play$desc))
      Sys.sleep(60)
    }
  }
  message("No Plays To Tweet")
  # games<- cfb_game_info(2020,week = week,season_type = "postseason")
  # live_games <- games %>%
  #   mutate(start_time = lubridate::as_datetime(start_date),
  #          start_time2 = lubridate::force_tz(start_time,tzone = "GMT"),
  #          start_time3 = lubridate::with_tz(start_time2,tzone = "MST")) %>%
  #   #select(start_time,start_time2,start_time3) %>% arrange(start_time3)
  #   dplyr::filter(
  #
  #     # this probably isn't necessary but whatever
  #     season == 2020,
  #
  #     # hasn't finished yet
  #     is.na(away_post_win_prob),
  #
  #     # happening today
  #     as.character(lubridate::as_date(start_time3)) == as.character(lubridate::today())
  #
  #   ) %>%
  #   dplyr::mutate(
  #     # there's probably a better way to do this but it seems to work
  #     current_hour = lubridate::hour(lubridate::now()),
  #     current_minute = lubridate::minute(lubridate::now()),
  #     game_hour = lubridate::hour(start_time3),#as.integer(substr(gametime, 1, 2)),
  #     game_minute = lubridate::minute(start_time3),#as.integer(substr(gametime, 4, 5)),
  #     # has already started
  #     started = dplyr::case_when(
  #       current_hour > game_hour ~ 1,
  #       current_hour == game_hour & current_minute >= game_minute + 10 ~ 1,
  #       TRUE ~ 0
  #     )
  #   ) %>%
  #   dplyr::filter(started == 1) %>%
  #   dplyr::select(game_id, home_team, away_team, week)

    Sys.sleep(120)
}


message("No Live Games")



