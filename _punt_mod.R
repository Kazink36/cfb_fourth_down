punt_df <- pbp %>%
  filter(str_detect(play_text,"punt"))

flip_team <- function(df) {
  turnover <- TRUE
  df %>%
    mutate(
      down = 1,
      distance = 10,
      pos_score_temp = ifelse(turnover,def_pos_score,pos_score),
      def_pos_score = ifelse(turnover,pos_score,def_pos_score),
      pos_score = ifelse(turnover,pos_score_temp,pos_score),
      pos_to_temp = ifelse(turnover,def_pos_team_timeouts_rem_before,pos_team_timeouts_rem_before),
      def_pos_team_timeouts_rem_before = ifelse(turnover,pos_team_timeouts_rem_before,def_pos_team_timeouts_rem_before),
      pos_team_timeouts_rem_before = ifelse(turnover,pos_to_temp,pos_team_timeouts_rem_before),
      pos_score = ifelse(turnover,pos_score_temp,pos_score),
      yards_to_goal = ifelse(turnover,100-yards_to_goal,yards_to_goal)
    )
}

get_density <- function(x, y, ...) {
  density_out <- MASS::kde2d(x, y, ...)
  int_x <- findInterval(x, density_out$x)
  int_y <- findInterval(y, density_out$y)
  comb_int <- cbind(int_x, int_y)
  return(density_out$z[comb_int])
}

points <- pbp %>%
  select(play_text, yards_to_goal,punt, kick_distance=yds_punted, return_yards=yds_punt_return,punt_blocked) %>%
  mutate(
    # give return yards too
    yards_to_goal_end = yards_to_goal - kick_distance + return_yards,
    yards_to_goal_end =
      ifelse(
        stringr::str_detect(play_text, "end zone") & is.na(kick_distance), 20, yards_to_goal_end
      ),
    # for blocked punts, just give them the ball there
    yards_to_goal_end = ifelse(punt_blocked == 1 & is.na(yards_to_goal_end), yards_to_goal, yards_to_goal_end),
    # make it in the actual field of play
    yards_to_goal_end = ifelse(yards_to_goal_end > 100, 100, yards_to_goal_end),
    # there's 2 safeties that are too annoying to deal with
    yards_to_goal_end = ifelse(yards_to_goal_end <= 0, 1, yards_to_goal_end),
    blocked = ifelse(punt_blocked == 1, 1, 0),
    return_td = ifelse(yards_to_goal_end == 100, 1, 0),
    # there's 1 play where there was a fumble lost after a blocked punt
    # this isn't a muffed punt
  ) %>%
  # there's like 10 of these for some reason
  filter(!is.na(yards_to_goal_end)) %>%
  select(play_text, yards_to_goal, yards_to_goal_end, blocked, return_td)

points

outliers <- points %>%
  group_by(yards_to_goal) %>%
  summarize(
    #muffed = sum(fumble_lost),
    blocked = sum(blocked),
    return_td = sum(return_td),
    n = n()
  ) %>%
  ungroup() %>%
  mutate(
    bin = case_when(
      yards_to_goal < 40 ~ 0,
      between(yards_to_goal, 40, 49) ~ 1,
      between(yards_to_goal, 50, 59) ~ 2,
      between(yards_to_goal, 60, 69) ~ 3,
      between(yards_to_goal, 70, 79) ~ 4,
      between(yards_to_goal, 80, 89) ~ 5,
      between(yards_to_goal, 90, 99) ~ 6
    )
  ) %>%
  group_by(bin) %>%
  mutate(
   # muffed = sum(muffed),
    blocked = sum(blocked),
    return_td = sum(return_td),
    n = sum(n),
    #bin_muffed_pct = muffed / n,
    bin_blocked_pct = blocked / n,
    bin_td_pct = return_td / n,
  ) %>%
  ungroup()

return_tds <- outliers %>%
  mutate(
    yards_to_goal_end = 100,
    density = bin_blocked_pct
  ) %>%
  select(yards_to_goal, yards_to_goal_end, density) %>%
  filter(density > 0)

blocks <- outliers %>%
  mutate(
    # not used for anything except to pick these out later
    yards_to_goal_end = 999,
    density = bin_td_pct
  ) %>%
  select(yards_to_goal, yards_to_goal_end, density) %>%
  filter(density > 0)

# get density excluding blocks and returns. will add those later
density_map_normal <- points %>%
  filter(blocked == 0 & return_td == 0) %>%
  select(yards_to_goal, yards_to_goal_end) %>%
  mutate(density = get_density(yards_to_goal, yards_to_goal_end, n = 100))

# get final percentages
df <- density_map_normal %>%
  group_by(yards_to_goal, yards_to_goal_end) %>%
  dplyr::slice(1) %>%
  ungroup() %>%
  arrange(yards_to_goal, yards_to_goal_end) %>%
  group_by(yards_to_goal) %>%
  mutate(
    tot_dens = sum(density),
    pct = density / tot_dens
  ) %>%
  ungroup() %>%
  bind_rows(blocks) %>%
  bind_rows(return_tds) %>%
  arrange(yards_to_goal, yards_to_goal_end) %>%
  group_by(yards_to_goal) %>%
  mutate(
    outlier_pct = sum(density * (yards_to_goal_end == 100)) + sum(density * (yards_to_goal_end == 999)),
    non_outlier_pct = 1 - outlier_pct,
    pct = pct * non_outlier_pct,
    pct = ifelse(is.na(pct), density, pct),
    yards_to_goal_end = ifelse(yards_to_goal_end == 999, yards_to_goal, yards_to_goal_end)
  ) %>%
  ungroup() %>%
  # left_join(
  #   outliers %>% select(yards_to_goal, bin_muffed_pct), by = "yards_to_goal"
  # ) %>%
  arrange(yards_to_goal, yards_to_goal_end) %>%
  select(yards_to_goal, yards_to_goal_end, pct) %>% #, bin_muffed_pct) %>%
  filter(yards_to_goal > 30)

punt_df <- bind_rows(
  # get a df without the return and blocked probs
  df %>%
    filter(yards_to_goal_end != 100 & yards_to_goal != yards_to_goal_end),
  df
) %>%
  arrange(yards_to_goal, yards_to_goal_end) %>%
  group_by(yards_to_goal, yards_to_goal_end) %>%
  # mutate(
  #   muff = 1 : n() - 1,
  #   #pct = ifelse(muff == 1, bin_muffed_pct * pct, pct),
  #   pct = ifelse(
  #     muff == 0 & yards_to_goal_end != 100 & yards_to_goal != yards_to_goal_end, (1 - bin_muffed_pct) * pct, pct
  #   )
  # ) %>%
  # one last making sure all the pct add up to 1
  group_by(yards_to_goal) %>%
  mutate(tot_pct = sum(pct), pct = pct / tot_pct) %>%
  ungroup() %>%
  select(
    yards_to_goal, yards_to_goal_end, pct#, muff
  )

saveRDS(punt_df,"punt_df.RDS")




get_punt_wp <- function(df, punt_df) {

  # get the distribution at a yard line from punt data
  punt_probs <- punt_df %>%
    filter(yards_to_goal == df$yards_to_goal) %>%
    select(yards_to_goal_end, pct)#, muff)

  if (nrow(punt_probs) > 0) {

    # get punt df
    probs <- punt_probs %>%
      bind_cols(df[rep(1, nrow(punt_probs)), ]) %>%
      flip_team() %>%
      mutate(
        yards_to_goal = 100 - yards_to_goal_end,

        # deal with punt return TD (yards_to_goal_end == 100)
        # we want punting team to be receiving a kickoff so have to flip everything back
        #posteam = ifelse(yards_to_goal_end == 100, df$posteam, df$),
        yards_to_goal = ifelse(yards_to_goal_end == 100, as.integer(75), as.integer(yards_to_goal)),
        pos_team_timeouts_rem_before = ifelse(yards_to_goal_end == 100,
                                                    df$pos_team_timeouts_rem_before,
                                                    pos_team_timeouts_rem_before),
        def_pos_team_timeouts_rem_before = ifelse(yards_to_goal_end == 100,
                                                    df$def_pos_team_timeouts_rem_before,
                                                    def_pos_team_timeouts_rem_before),
        # score_differential = ifelse(yards_to_goal_end == 100, as.integer(-score_differential - 7), as.integer(score_differential)),
        # receive_2h_ko = case_when(
        #   qtr <= 2 & receive_2h_ko == 0 & (yards_to_goal_end == 100) ~ 1,
        #   qtr <= 2 & receive_2h_ko == 1 & (yards_to_goal_end == 100) ~ 0,
        #   TRUE ~ receive_2h_ko
        # ),

        # now deal with muffed punts (fumble lost)
        # again we need to flip everything back
        #posteam = ifelse(muff == 1, df$posteam, posteam),
        #yards_to_goal = ifelse(muff == 1, as.integer(100 - yards_to_goal), yards_to_goal),
        # posteam_timeouts_remaining = dplyr::ifelse(muff == 1,
        #                                             df$posteam_timeouts_remaining,
        #                                             posteam_timeouts_remaining),
        # defteam_timeouts_remaining = dplyr::ifelse(muff == 1,
        #                                             df$defteam_timeouts_remaining,
        #                                             defteam_timeouts_remaining),
        #score_differential = ifelse(muff == 1, as.integer(-score_differential), as.integer(score_differential)),
        # receive_2h_ko = case_when(
        #   qtr <= 2 & receive_2h_ko == 0 & (muff == 1) ~ 1,
        #   qtr <= 2 & receive_2h_ko == 1 & (muff == 1) ~ 0,
        #   TRUE ~ receive_2h_ko
        # ),
        distance = ifelse(yards_to_goal < 10, yards_to_goal, as.integer(distance)),
        down = as.factor(down)
      )

    probs$ep <- predict(object = cfbscrapR:::ep_model,newdata = probs,type = "probs") %>%
      as_tibble() %>%
      mutate(ep =V1*0+V2*3+V3*-3+V4*2+V5*-7+V6*-2+V7*7) %>%
      pull(ep)
    probs <- probs %>%
      mutate(ExpScoreDiff = pos_score_diff_start + ep,
             ExpScoreDiff_Time_Ratio = ExpScoreDiff/TimeSecsRem,
             wp = NA)
    probs$wp <- predict(object = cfbscrapR:::wp_model,newdata = probs,type = "response")

    # have to flip bc other team
    #1 - probs %>%
    probs %>%
      # nflfastR::calculate_expected_points() %>%
      # nflfastR::calculate_win_probability() %>%
      mutate(
        # for the punt return TD case
        wp = ifelse(yards_to_goal_end == 100, 1 - wp, wp),
        score_differential = pos_score - def_pos_score,
        # fill in end of game situation when team can kneel out clock
        # discourages punting when the other team can end the game
        wp = case_when(
          score_differential > 0 & TimeSecsRem < 120 & def_pos_team_timeouts_rem_before == 0 ~ 1,
          score_differential > 0 & TimeSecsRem < 80 & def_pos_team_timeouts_rem_before == 1 ~ 1,
          score_differential > 0 & TimeSecsRem < 40 & def_pos_team_timeouts_rem_before == 2 ~ 1,
          TRUE ~ wp
        ),

        wt_wp = pct * wp
      ) %>%
      summarize(wp = sum(wt_wp)) %>%
      pull(wp) %>%
      return()
  } else {
    # message("Too close for punting")
    return(NA_real_)
  }

}

