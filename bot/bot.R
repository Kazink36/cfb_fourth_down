library(cfbfastR)
library(gt)
library(MASS, exclude = "select")
library(rtweet)
library(tidyverse)
setwd("~/Documents/cfb_fourth_down")
options(dplyr.summarise.inform = FALSE)
source("R/bot_functions.R")
source("R/helpers.R")

week <- 2
season <- 2021
games<- cfbfastR::cfbd_game_info(season,week = week,season_type = "regular") %>%
  filter(home_team %in% team_info$school,away_team %in% team_info$school)

# Get game_id's of nationally broadcasted games
media<-cfbfastR::cfbd_game_media(season,week = week)
national_games <- media %>%
  filter(season_type == "regular") %>%
  unnest_longer(tv) %>% filter(tv %in% c("ABC","CBS","CBSSN","ESPN","ESPN2","FOX","FS1")) %>%
  distinct(game_id)









# Filter for live games
live_games <- games %>%
  # Had some trouble working with the times, this was super hacky but works
  mutate(start_time = lubridate::as_datetime(start_date),
         start_time2 = lubridate::force_tz(start_time,tzone = "GMT"),
         start_time3 = lubridate::with_tz(start_time2,tzone = "MST"),
         test = as.character(lubridate::as_date(start_time3)) == as.character(lubridate::today())
         ) %>%
   dplyr::filter(

    # this probably isn't necessary but whatever
    season == season,

    # hasn't finished yet
    is.na(away_post_win_prob),

    # happening today
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
    plays <- rbind(plays,
                   get_data(live_games[i,]) %>%
                     mutate(season = season,
                            week = week)
                   )
  }
   if (nrow(plays != 0)) {
    old_plays <- readRDS(glue::glue("data/fd_pbp_{season}.RDS"))
    plays <- plays %>% mutate(play_id = as.numeric(play_id),old = ifelse(play_id %in% old_plays$play_id,1,0))
    to_tweet <- plays %>% filter(old == 0)
  }
  if (nrow(to_tweet) != 0) {
    for (i in 1:nrow(to_tweet)) {

      play <- to_tweet %>% slice(i)
      message(play$play_id)
      tidy_play <- make_tidy_data(play) %>% mutate(game_id = as.numeric(game_id),
                                                   play_id = as.numeric(play_id))
      old_plays <- old_plays %>%
        bind_rows(tidy_play)
      saveRDS(old_plays,glue::glue("data/fd_pbp_{season}.RDS"))
    score_diff <- abs(tidy_play$home_score - tidy_play$away_score)
      if(
        !(tidy_play$strength> 0.3 & tidy_play$recommendation == "Punt" & tidy_play$choice == "Punt") |
        (as.integer(tidy_play$game_id) %in% national_games$game_id & tidy_play$qtr>2) |
        (score_diff <= 14 & tidy_play$qtr == 4)
         ) {
      tidy_play %>%
        tweet_play(tidy = TRUE)
        message(paste(Sys.time(),play$desc))
        Sys.sleep(60)
      }



    }
  }
  message("No Plays To Tweet")

  # Update live games
  games <- cfbfastR::cfbd_game_info(season,week = week,season_type = "regular") %>%
    filter(home_team %in% team_info$school,away_team %in% team_info$school)
  live_games <- games %>%
    # Had some trouble working with the times, this was super hacky but works
    mutate(start_time = lubridate::as_datetime(start_date),
           start_time2 = lubridate::force_tz(start_time,tzone = "GMT"),
           start_time3 = lubridate::with_tz(start_time2,tzone = "MST"),
           test = as.character(lubridate::as_date(start_time3)) == as.character(lubridate::today())
    ) %>%
    dplyr::filter(

      # this probably isn't necessary but whatever
      season == season,

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

    Sys.sleep(60)
}


message("No Live Games")



