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

get_go_wp <- function(game_state) {

  game_state_long <- game_state %>%
    slice(rep(1,76)) %>%
    #add_row(down = rep(game_state$down,75),distance = game_state$distance, yards_to_goal = game_state$yards_to_goal) %>%
    mutate(pred = predict(yard_model,newdata = game_state,type = "probs"),
           yards_gained = row_number()-11,
           yards_to_goal = yards_to_goal-yards_gained,
           success = yards_gained >= distance,
           td = yards_to_goal <= 0
           ) %>%
    update_game_state()


  game_state_long$ep <- predict(object = cfbscrapR:::ep_model,newdata = game_state_long,type = "probs") %>%
    as_tibble() %>%
    mutate(ep =V1*0+V2*3+V3*-3+V4*2+V5*-7+V6*-2+V7*7) %>%
    pull(ep)
  game_state_long <- game_state_long %>%
    mutate(ExpScoreDiff = pos_score_diff_start + ep,
           ExpScoreDiff_Time_Ratio = ExpScoreDiff/TimeSecsRem,
           wp = NA)
  game_state_long$wp <- predict(object = cfbscrapR:::wp_model,newdata = game_state_long,type = "response")
  game_state_long <- game_state_long %>%
    mutate(wp = ifelse(td|turnover,1-wp,wp))

  report <- game_state_long %>%
    mutate(success_tag = ifelse(success,"Convert","Fail")) %>%
    group_by(success_tag) %>%
    summarize(success = sum(pred),
              wp = sum(wp*pred)/success)# %>%
  wp_go <- report %>%
    mutate(exp_wp = sum(success*wp)) %>% slice(1) %>%
    pull(exp_wp)
  first_down_prob <- report %>% filter(success_tag == "Convert") %>% pull(success)
  wp_succeed <- report %>% filter(success_tag == "Convert") %>% pull(wp)
  wp_fail <- report %>% filter(success_tag == "Fail") %>% pull(wp)


  results <- list(
    wp_go,
    first_down_prob,
    wp_fail,
    wp_succeed
  )
  return(results)
}
update_game_state <- function(df) {
  df %>%
    mutate(down = factor(1,levels = c(1,2,3,4)),
           distance = 10,
           #TD
           pos_score = ifelse(td,pos_score+7,pos_score),
           pos_score_temp = ifelse(td,def_pos_score,pos_score),
           def_pos_score = ifelse(td,pos_score,def_pos_score),
           pos_score = ifelse(td,pos_score_temp,pos_score),
           pos_to_temp = ifelse(td,def_pos_team_timeouts_rem_before,pos_team_timeouts_rem_before),
           def_pos_team_timeouts_rem_before = ifelse(td,pos_team_timeouts_rem_before,def_pos_team_timeouts_rem_before),
           pos_team_timeouts_rem_before = ifelse(td,pos_to_temp,pos_team_timeouts_rem_before),
           pos_score = ifelse(td,pos_score_temp,pos_score),
           yards_to_goal = ifelse(td,75,yards_to_goal),
           # Turnover
           turnover = !success,
           pos_score_temp = ifelse(turnover,def_pos_score,pos_score),
           def_pos_score = ifelse(turnover,pos_score,def_pos_score),
           pos_score = ifelse(turnover,pos_score_temp,pos_score),
           pos_to_temp = ifelse(turnover,def_pos_team_timeouts_rem_before,pos_team_timeouts_rem_before),
           def_pos_team_timeouts_rem_before = ifelse(turnover,pos_team_timeouts_rem_before,def_pos_team_timeouts_rem_before),
           pos_team_timeouts_rem_before = ifelse(turnover,pos_to_temp,pos_team_timeouts_rem_before),
           pos_score = ifelse(turnover,pos_score_temp,pos_score),
           yards_to_goal = ifelse(turnover,100-yards_to_goal,yards_to_goal),
           distance = ifelse(yards_to_goal < 10, yards_to_goal,distance)
           ) %>%
    mutate(Under_two = TimeSecsRem < 120,
           log_ydstogo = log(yards_to_goal),
           Goal_To_Go = distance == yards_to_goal,
           pos_score_diff_start = pos_score-def_pos_score,ep = NA,
           )
}






