library(MASS)
library(rtweet)
source("bot_functions.R")
source("helpers.R")
#source("bot.R")

df <- cfb_game_info(2020,week = 14, team = "Utah")
plays <- get_data(df)
for (i in 1:nrow(plays)) {

  plays %>%
    slice(i) %>%
    tweet_play()
  print(Sys.time())
  Sys.sleep(120)
}

old_plays <- tibble()
for (i in 1:nrow(plays)) {
  temp <- plays %>%
    slice(i) %>%
    make_tidy_data(punt_df)
  old_plays <- bind_rows(old_plays,temp)
}
view(old_plays)
