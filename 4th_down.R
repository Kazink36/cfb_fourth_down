cfbscrapR:::wp_model


tibble(ExpScoreDiff = 0,TimeSecsRem = 1800,half = 2,
       ExpScoreDiff_Time_Ratio = 0,Under_two = FALSE, pos_team_timeouts_rem_before = 3,
       def_pos_team_timeouts_rem_before = 3) %>%
  predict(object = cfbscrapR:::wp_model,newdata = .,type = "response")
cfbscrapR:::ep_model
tibble(TimeSecsRem = 1800,yards_to_goal = 75,down = "1",log_ydstogo = log(yards_to_goal),
       Goal_To_Go = FALSE, pos_score_diff_start = 0,Under_two = FALSE) %>%
  predict(object = cfbscrapR:::ep_model,newdata = .,type = "probs")
cfbscrapR:::fg_model


game_state <- tibble(yards_to_goal = 47, down = "4",distance = 3,pos_score = 7,def_pos_score = 14,
                     TimeSecsRem = 23,half = 1,pos_team_timeouts_rem_before = 3,
                     def_pos_team_timeouts_rem_before = 3) %>%
  mutate(Under_two = TimeSecsRem <= 120,
         log_ydstogo = log(yards_to_goal),
         adj_TimeSecsRem = TimeSecsRem + ifelse(half == 1,1800,0),
         Goal_To_Go = distance == yards_to_goal,
         pos_score_diff_start = pos_score-def_pos_score,ep = NA)

game_state$ep <- sum(predict(object = cfbscrapR:::ep_model,newdata = game_state,type = "probs")*c(0,3,-3,2,-7,-2,7) )

game_state_ep <- game_state %>%
  mutate(ExpScoreDiff = pos_score_diff_start + ep,
         ExpScoreDiff_Time_Ratio = ExpScoreDiff/(adj_TimeSecsRem+1),
         wp = NA)
game_state_ep$wp <- predict(object = cfbscrapR:::wp_model,newdata = game_state_ep,type = "response")

seasons <- 2014:2020
pbp_all <- readRDS("pbp_all.RDS")
pbp_4th_go <- pbp_4th %>% filter(rush+pass == 1)

df <- cfb_game_info(2020,week = 16, team = "Rutgers")


plays <- get_data(df)
plays <- plays %>%
  mutate(id_play = play_id)
test <- cfbscrapR::create_epa(plays)


pbp$boxscore
