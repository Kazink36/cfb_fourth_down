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
    yards_to_goal
    #yardline_100,
    #era3, era4,
    #outdoors, retractable, dome,
    #posteam_spread, total_line, posteam_total
  ) %>%
  # 0 = 10 yard loss
  mutate(label = label + 10)
set.seed(2013)

full_train = xgboost::xgb.DMatrix(model.matrix(~.+0, data = model_vars %>% dplyr::select(-label)), label = as.integer(model_vars$label))

nrounds = 5000

grid <- grid_latin_hypercube(
  finalize(mtry(), model_vars),
  min_n(),
  tree_depth(),
  learn_rate(),
  loss_reduction(),
  sample_size = sample_prop(),
  size = 20
)
grid <- grid %>%
  mutate(
    # it was making dumb learn rates
    learn_rate = .025 + .1 * ((1 : nrow(grid)) / nrow(grid)),
    # has to be between 0 and 1
    mtry = mtry / length(model_vars)
  )
grid


get_metrics <- function(df, row = 1) {

  # testing only
  # df <- grid %>% dplyr::slice(1)

  params <-
    list(
      booster = "gbtree",
      objective = "multi:softprob",
      eval_metric = c("mlogloss"),
      num_class = 76,
      eta = df$learn_rate,
      gamma = df$loss_reduction,
      subsample= df$sample_size,
      colsample_bytree= df$mtry,
      max_depth = df$tree_depth,
      min_child_weight = df$min_n
    )

  # tuning with cv
  fd_model <- xgboost::xgb.cv(data = full_train, params = params, nrounds = nrounds,
                              nfold = 5, metrics = list("mlogloss"),
                              early_stopping_rounds = 10, print_every_n = 10)

  output <- params
  output$iter = fd_model$best_iteration
  output$logloss = fd_model$evaluation_log[output$iter]$test_mlogloss_mean
  output$error = fd_model$evaluation_log[output$iter]$test_merror_mean

  this_param <- bind_rows(output)

  if (row == 1) {
    saveRDS(this_param, "data/modeling.rds")
  } else {
    prev <- readRDS("data/modeling.rds")
    for_save <- bind_rows(prev, this_param)
    saveRDS(for_save, "data/modeling.rds")
  }

  return(this_param)

}

results <- map_df(1 : nrow(grid), function(x) {

  message(glue::glue("Row {x}"))
  get_metrics(grid %>% dplyr::slice(x), row = x)

})
