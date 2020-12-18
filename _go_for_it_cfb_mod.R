library(tidyverse)
library(tidymodels)

seasons <- 2014:2019
pbp <- purrr::map_df(seasons, function(x) {
  print(x)
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/saiemgilani/cfbscrapR-data/master/data/rds/pbp_players_pos_{x}.rds")
    )
  )
})

pbp_test <- pbp %>% sample_n(1000)
pbp_test <- pbp_test %>% as_tibble() %>%
  filter(
    down %in% c(3,4),
    rush == 1 | pass == 1,
    !is.na(offense_play),
    !is.na(yards_to_goal),
    !is.na(score_diff)
  ) %>%
  mutate(first_down_penalty = firstD_by_penalty) #%>% select(contains("penalty")) %>% view()
pbp %>% count(penalty_detail) %>% view()
model_vars <-pbp %>%
  filter(
    down %in% c(3,4),
    rush == 1 | pass == 1,
    !is.na(offense_play),
    !is.na(yards_to_goal),
    !is.na(score_diff)
  ) %>%
  mutate(first_down_penalty = firstD_by_penalty) %>%
  mutate(#yards_gained =

           # we need a way to account for defensive penalties that give auto first downs
           # hacky "solution" is saying here that a penalty that gives a first down goes for the yards to go
           # unless the actual penalty yardage is higher

           # the drawback is that a defensive holding on eg 4th and 8 is coded as an 8 yard gain
           # the alternative is to estimate a separate model for penalties or have their own category
           # but first down penalties on 4th and long are very rare:
           # https://twitter.com/benbbaldwin/status/1322530446371074050
           # case_when(
           #   first_down_penalty == 1 & penalty_yards < ydstogo ~ ydstogo,
           #   first_down_penalty == 1 & penalty_yards >= ydstogo ~ penalty_yards,
           #   TRUE ~ yards_gained
           # ),
         # truncate to make model training easier
         yards_gained = ifelse(yards_gained < -10, -10, yards_gained),
         yards_gained = ifelse(yards_gained > 65, 65, yards_gained),
         # home_total = (spread_line + total_line) / 2,
         # away_total = (total_line - spread_line) / 2,
         # posteam_total = if_else(posteam == home_team, home_total, away_total),
         # posteam_spread = dplyr::if_else(posteam == home_team, spread_line, -1 * spread_line)
  ) %>%
  # look at when an actual play is run or a defensive penalty gives a first down
  filter(rush+pass == 1 | first_down_penalty == 1) %>%
  mutate(label = yards_gained) %>%
  select(
    label,
    down,
    distance,
    yards_to_goal
    #era3, era4,
    #outdoors, retractable, dome,
    #posteam_spread, total_line, posteam_total
  ) %>%
  # 0 = 10 yard loss
  mutate(label = label + 10)
set.seed(2013)

# full_train = xgboost::xgb.DMatrix(model.matrix(~.+0, data = model_vars %>% dplyr::select(-label)), label = as.integer(model_vars$label))
#
# nrounds = 5000

yard_model = MASS::polr(as.factor(label) ~ .,data = model_vars)
saveRDS(yard_model,"yard_model.RDS")








