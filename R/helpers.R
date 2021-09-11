

get_fg_wp <- function(df) {

  # probability field goal is made
  fg_prob <- as.numeric(mgcv::predict.bam(fg_model, newdata = df, type="response"))

  # don't recommend kicking when fg is over 60 yards
  fg_prob <- if_else(df$yards_to_goal > 42, 0, fg_prob)

  # hacky way to not have crazy high probs for long kicks
  # because the bot should be conservative about recommending kicks in this region
  # for 53 through 60 yards
  fg_prob <- if_else(df$yards_to_goal >= 35 & df$yards_to_goal <= 42, fg_prob * .9, fg_prob)

  # win probability of kicking team if field goal is made
  probs <-
    df %>%
    flip_team() %>%
    # win prob after receiving kickoff for touchback and other team has 3 more points
    mutate(
      yards_to_goal = 75,
      score_differential = (pos_score_diff_start - 3),
      pos_score_diff_start = (pos_score_diff_start - 3),
      down = as.factor(down),
      log_ydstogo = log(yards_to_goal),
      adj_TimeSecsRem = TimeSecsRem + ifelse(half == 1,1800,0),
      Goal_To_Go = distance == yards_to_goal,
    ) #%>%
  probs$ep <- sum(predict(object = cfbfastR:::ep_model,newdata = probs,type = "probs")*c(0,3,-3,-2,-7,2,7) )
  probs <- probs %>%
    mutate(ExpScoreDiff = pos_score_diff_start + ep,
           ExpScoreDiff_Time_Ratio = ExpScoreDiff/(adj_TimeSecsRem+1),
           elapsed_share = ((3600 - adj_TimeSecsRem) / 3600),
           spread_time = (-1 * posteam_spread) * exp(-4 * elapsed_share),
           is_home = ifelse(pos_team == home_team, TRUE, FALSE),
           wp = NA)


  wp_vars <- probs %>%
    select(
      "pos_team_receives_2H_kickoff",
      "spread_time",
      "TimeSecsRem",
      "adj_TimeSecsRem",
      "ExpScoreDiff_Time_Ratio",
      "pos_score_diff_start",
      "down",
      "distance",
      "yards_to_goal",
      "is_home",
      "pos_team_timeouts_rem_before",
      "def_pos_team_timeouts_rem_before",
      "period"
    ) %>%
    # Down is encoded as a factor, this prevents creating a character matrix
    mutate(down = as.numeric(down)) %>%
    as.matrix()

  probs$wp <- predict(wp_model, newdata = wp_vars)


  # for end of 1st half stuff
  #flip_half() %>%
  fg_make_wp<- 1-probs %>%
    mutate(

      # fill in end of game situation when team can kneel out clock
      # discourages punting when the other team can end the game
      wp = case_when(
        score_differential > 0 & adj_TimeSecsRem < 120 & def_pos_team_timeouts_rem_before == 0 ~ 1,
        score_differential > 0 & adj_TimeSecsRem < 80 & def_pos_team_timeouts_rem_before == 1 ~ 1,
        score_differential > 0 & adj_TimeSecsRem < 40 & def_pos_team_timeouts_rem_before == 2 ~ 1,
        TRUE ~ wp
      )

    ) %>%
    pull(wp)

  # win probability of kicking team if field goal is missed
  probs <-
    df %>%
    flip_team() %>%
    mutate(
      yards_to_goal = (100 - yards_to_goal),
      score_differential = (pos_score_diff_start),
      pos_score_diff_start = (pos_score_diff_start),
      # yards_to_goal can't be bigger than 80 due to some weird nfl rule
      yards_to_goal = if_else(yards_to_goal > 80, 80, yards_to_goal),
      yards_to_goal = if_else(yards_to_goal == 0, 1, yards_to_goal),
      down = as.factor(down),
      adj_TimeSecsRem = TimeSecsRem + ifelse(half == 1,1800,0),
      log_ydstogo = log(yards_to_goal),
      Goal_To_Go = distance == yards_to_goal
    )

  probs$ep <- sum(predict(object = cfbfastR:::ep_model,newdata = probs,type = "probs")*c(0,3,-3,-2,-7,2,7) )
  probs <- probs %>%
    mutate(ExpScoreDiff = pos_score_diff_start + ep,
           ExpScoreDiff_Time_Ratio = ExpScoreDiff/(adj_TimeSecsRem+1),
           elapsed_share = ((3600 - adj_TimeSecsRem) / 3600),
           spread_time = (-1 * posteam_spread) * exp(-4 * elapsed_share),
           is_home = ifelse(pos_team == home_team, TRUE, FALSE),
           wp = NA)

  wp_vars <- probs %>%
    select(
      "pos_team_receives_2H_kickoff",
      "spread_time",
      "TimeSecsRem",
      "adj_TimeSecsRem",
      "ExpScoreDiff_Time_Ratio",
      "pos_score_diff_start",
      "down",
      "distance",
      "yards_to_goal",
      "is_home",
      "pos_team_timeouts_rem_before",
      "def_pos_team_timeouts_rem_before",
      "period"
    ) %>%
    # Down is encoded as a factor, this prevents creating a character matrix
    mutate(down = as.numeric(down)) %>%
    as.matrix()

  probs$wp <- predict(wp_model, newdata = wp_vars)
  # for end of 1st half stuff
  #flip_half() %>%
  fg_miss_wp <- 1-probs %>%
    mutate(

      # fill in end of game situation when team can kneel out clock
      # discourages punting when the other team can end the game
      wp = case_when(
        score_differential > 0 & adj_TimeSecsRem < 120 & def_pos_team_timeouts_rem_before == 0 ~ 1,
        score_differential > 0 & adj_TimeSecsRem < 80 & def_pos_team_timeouts_rem_before == 1 ~ 1,
        score_differential > 0 & adj_TimeSecsRem < 40 & def_pos_team_timeouts_rem_before == 2 ~ 1,
        TRUE ~ wp
      )

    ) %>%
    pull(wp)

  # for end of half situations
  # when team gets ball again after halftime
  # if (df %>% flip_team() %>% pull(half_seconds_remaining) == 0 & df$qtr == 2 &
  #     df %>% flip_team() %>% flip_half() %>% pull(pos_team) == df$pos_team) {
  #   # message("test")
  #   fg_make_wp <- 1 - fg_make_wp
  #   fg_miss_wp <- 1 - fg_miss_wp
  # }

  # FG win prob is weighted avg of make and miss WPs
  fg_wp <- fg_prob * fg_make_wp + (1 - fg_prob) * fg_miss_wp

  # bind up the probs to return for table
  results <- list(fg_wp, fg_prob, fg_miss_wp, fg_make_wp)

  return(results)
}

get_punt_wp <- function(df) {

  # get the distribution at a yard line from punt data
  punt_probs <- punt_df %>%
    filter(yards_to_goal == df$yards_to_goal) %>%
    select(yards_to_goal_end, pct)

  if (nrow(punt_probs) > 0) {

    # get punt df
    probs <- punt_probs %>%
      bind_cols(df[rep(1, nrow(punt_probs)), ])#, "minimal")

    probs <- probs %>%
      flip_team()

    probs <- probs %>%
      mutate(
        yards_to_goal = 100 - yards_to_goal_end,

        # deal with punt return TD (yards_to_goal_end == 100)
        # we want punting team to be receiving a kickoff so have to flip everything back
        #pos_team = ifelse(yards_to_goal_end == 100, df$pos_team, df$),
        yards_to_goal = ifelse(yards_to_goal_end == 100, as.integer(75), as.integer(yards_to_goal)),
        pos_team_timeouts_rem_before = ifelse(yards_to_goal_end == 100,
                                              df$pos_team_timeouts_rem_before,
                                              pos_team_timeouts_rem_before),
        def_pos_team_timeouts_rem_before = ifelse(yards_to_goal_end == 100,
                                                  df$def_pos_team_timeouts_rem_before,
                                                  def_pos_team_timeouts_rem_before),
        score_differential = ifelse(yards_to_goal_end == 100, as.integer(-score_differential - 7), as.integer(score_differential)),
        log_ydstogo = log(yards_to_goal),
        pos_score_diff_start = ifelse(yards_to_goal_end == 100, as.integer(-pos_score_diff_start - 7), as.integer(pos_score_diff_start)),
        # receive_2h_ko = case_when(
        #   qtr <= 2 & receive_2h_ko == 0 & (yards_to_goal_end == 100) ~ 1,
        #   qtr <= 2 & receive_2h_ko == 1 & (yards_to_goal_end == 100) ~ 0,
        #   TRUE ~ receive_2h_ko
        # ),

        # now deal with muffed punts (fumble lost)
        # again we need to flip everything back
        #pos_team = ifelse(muff == 1, df$pos_team, pos_team),
        #yards_to_goal = ifelse(muff == 1, as.integer(100 - yards_to_goal), yards_to_goal),
        # pos_team_timeouts_remaining = dplyr::ifelse(muff == 1,
        #                                             df$pos_team_timeouts_remaining,
        #                                             pos_team_timeouts_remaining),
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
        Goal_To_Go = distance == yards_to_goal,
        adj_TimeSecsRem = TimeSecsRem + ifelse(half == 1,1800,0),
        down = as.factor(down),
        elapsed_share = ((3600 - adj_TimeSecsRem) / 3600),
        spread_time = (-1 * posteam_spread) * exp(-4 * elapsed_share),
        is_home = ifelse(pos_team == home_team, TRUE, FALSE),
      )

    probs$ep <- predict(object = cfbfastR:::ep_model,newdata = probs,type = "probs") %>%
      as_tibble() %>%
      mutate(ep =V1*0+V2*3+V3*-3+V4*-2+V5*-7+V6*2+V7*7) %>%
      pull(ep)
    probs <- probs %>%
      mutate(ExpScoreDiff = pos_score_diff_start + ep,
             ExpScoreDiff_Time_Ratio = ExpScoreDiff/(adj_TimeSecsRem+1),
             elapsed_share = ((3600 - adj_TimeSecsRem) / 3600),
             spread_time = (-1 * posteam_spread) * exp(-4 * elapsed_share),
             is_home = ifelse(pos_team == home_team, TRUE, FALSE),
             wp = NA)

    wp_vars <- probs %>%
      select(
        "pos_team_receives_2H_kickoff",
        "spread_time",
        "TimeSecsRem",
        "adj_TimeSecsRem",
        "ExpScoreDiff_Time_Ratio",
        "pos_score_diff_start",
        "down",
        "distance",
        "yards_to_goal",
        "is_home",
        "pos_team_timeouts_rem_before",
        "def_pos_team_timeouts_rem_before",
        "period"
      ) %>%
      # Down is encoded as a factor, this prevents creating a character matrix
      mutate(down = as.numeric(down)) %>%
      as.matrix()

    probs$wp <- predict(wp_model, newdata = wp_vars)

    # have to flip bc other team
    #1 - probs %>%
    1-probs %>%
      # nflfastR::calculate_expected_points() %>%
      # nflfastR::calculate_win_probability() %>%
      mutate(
        # for the punt return TD case
        wp = ifelse(yards_to_goal_end == 100, 1 - wp, wp),
        score_differential = pos_score - def_pos_score,
        # fill in end of game situation when team can kneel out clock
        # discourages punting when the other team can end the game
        wp = case_when(
          score_differential > 0 & adj_TimeSecsRem < 120 & def_pos_team_timeouts_rem_before == 0 ~ 1,
          score_differential > 0 & adj_TimeSecsRem < 80 & def_pos_team_timeouts_rem_before == 1 ~ 1,
          score_differential > 0 & adj_TimeSecsRem < 40 & def_pos_team_timeouts_rem_before == 2 ~ 1,
          TRUE ~ wp
        ),

        wt_wp = pct * wp
      ) %>%
      select(yards_to_goal_end,yards_to_goal,wp,pct,wt_wp) %>%
      summarize(wp = sum(wt_wp)) %>%
      pull(wp) %>%
      return()
  } else {
    # message("Too close for punting")
    return(NA_real_)
  }

}
get_go_wp <- function(game_state) {
  # print("Check go WP")
  not.character <- function(x){
    !is.character(x)
  }
  game_state_matrix <- game_state %>%
    select_if(not.character) %>%
    #Debugging
    # mutate(posteam_spread = -7,
    #        posteam_total = 50) %>%
    transmute(down = as.numeric(down),
           distance,
           yards_to_goal,
           posteam_spread,
           posteam_total) %>%
    as.matrix()
  # print("testerB")
  game_state_long <- stats::predict(fd_model, game_state_matrix) %>%
    as_tibble() %>%
    rename(pred = value) %>%
    bind_cols(game_state[rep(1,76),]) %>%
    mutate(yards_gained = row_number()-11,
           yards_to_goal = yards_to_goal - yards_gained,
           success = yards_gained >= distance,
           td = yards_to_goal <= 0) %>% filter(yards_to_goal < 100) %>%
    update_game_state()

  # print("testerA")
  game_state_long$ep <- predict(object = cfbfastR:::ep_model,newdata = game_state_long,type = "probs") %>%
    data.frame() %>%
    mutate(ep =X1*0+X2*3+X3*-3+X4*-2+X5*-7+X6*2+X7*7) %>%
    pull(ep)
  game_state_long <- game_state_long %>%
    mutate(ExpScoreDiff = pos_score_diff_start + ep,
           adj_TimeSecsRem = TimeSecsRem + ifelse(half == 1,1800,0),
           ExpScoreDiff_Time_Ratio = ExpScoreDiff/(adj_TimeSecsRem+1),
           elapsed_share = ((3600 - adj_TimeSecsRem) / 3600),
           spread_time = (-1 * posteam_spread) * exp(-4 * elapsed_share),
           is_home = ifelse(pos_team == home_team, TRUE, FALSE),
           wp = NA)
  # print("tester")
  wp_vars <- game_state_long %>%
    select(
      "pos_team_receives_2H_kickoff",
      "spread_time",
      "TimeSecsRem",
      "adj_TimeSecsRem",
      "ExpScoreDiff_Time_Ratio",
      "pos_score_diff_start",
      "down",
      "distance",
      "yards_to_goal",
      "is_home",
      "pos_team_timeouts_rem_before",
      "def_pos_team_timeouts_rem_before",
      "period"
    ) %>%
    # Down is encoded as a factor, this prevents creating a character matrix
    mutate(down = as.numeric(down)) %>%
    as.matrix()

  game_state_long$wp <- predict(wp_model, newdata = wp_vars)
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
           pos_team = case_when(home_team == pos_team & td ~ away_team,
                                away_team == pos_team & td ~ home_team,
                                TRUE ~ pos_team),
           posteam_spread = ifelse(td,-1*posteam_spread,posteam_spread),
           # Turnover
           turnover = !success,
           pos_team = case_when(home_team == pos_team & turnover ~ away_team,
                                away_team == pos_team & turnover ~ home_team,
                                TRUE ~ pos_team),
           pos_score_temp = ifelse(turnover,def_pos_score,pos_score),
           def_pos_score = ifelse(turnover,pos_score,def_pos_score),
           pos_score = ifelse(turnover,pos_score_temp,pos_score),
           pos_to_temp = ifelse(turnover,def_pos_team_timeouts_rem_before,pos_team_timeouts_rem_before),
           def_pos_team_timeouts_rem_before = ifelse(turnover,pos_team_timeouts_rem_before,def_pos_team_timeouts_rem_before),
           pos_team_timeouts_rem_before = ifelse(turnover,pos_to_temp,pos_team_timeouts_rem_before),
           pos_score = ifelse(turnover,pos_score_temp,pos_score),
           yards_to_goal = ifelse(turnover,100-yards_to_goal,yards_to_goal),
           distance = ifelse(yards_to_goal < 10, yards_to_goal,distance),
           posteam_spread = ifelse(turnover,-1*posteam_spread,posteam_spread)
    ) %>%
    mutate(Under_two = TimeSecsRem < 120,
           log_ydstogo = log(yards_to_goal),
           Goal_To_Go = distance == yards_to_goal,
           pos_score_diff_start = pos_score-def_pos_score,
           ep = NA#,
           #posteam_spread = if_else(pos_team == home_team,spread_line,-1*spread_line)
    )
}
flip_team <- function(df) {
  df %>%
    mutate(
      down = 1,
      distance = 10,
      TimeSecsRem = ifelse(TimeSecsRem<=6,0,TimeSecsRem - 6),
      pos_score_temp = def_pos_score,
      def_pos_score = pos_score,
      pos_score = pos_score_temp,
      pos_to_temp = def_pos_team_timeouts_rem_before,
      def_pos_team_timeouts_rem_before = pos_team_timeouts_rem_before,
      pos_team_timeouts_rem_before = pos_to_temp,
      #pos_score = ifelse(turnover,pos_score_temp,pos_score),
      score_differential = pos_score - def_pos_score,
      pos_score_diff_start = pos_score - def_pos_score,
      pos_team = if_else(home_team == posteam, away_team, home_team),
      posteam = if_else(home_team == posteam, away_team, home_team),
      posteam_spread = -1*posteam_spread
      #posteam_spread = if_else(pos_team == home_team,spread_line,-1*spread_line)
    )%>%
    mutate(Under_two = TimeSecsRem < 120,
           log_ydstogo = log(yards_to_goal),
           Goal_To_Go = distance == yards_to_goal,
           #pos_score_diff_start = pos_score-def_pos_score,
           ep = NA#,
           #posteam_spread = if_else(pos_team == home_team,spread_line,-1*spread_line)
    )
}
