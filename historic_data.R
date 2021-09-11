library(xgboost)
library(tidymodels)
library(tidyverse)
setwd("~/Documents/cfb_fourth_down")
fd_model <- readRDS("data/fd_model.RDS")
punt_df <- readRDS("data/punt_df.RDS")
fg_model <- readRDS("data/fg_model.RDS")
team_info <- readRDS("data/team_info.RDS")

wp_model <- xgb.load("data/wp_model.model")

#print(fd_model$feature_names)

seasons <- 2021
pbp <- purrr::map_df(seasons, function(x) {
  download.file(glue::glue("https://raw.githubusercontent.com/saiemgilani/cfbscrapR-data/master/data/parquet/pbp_players_pos_{x}.parquet"),"tmp.parquet")
  df <- arrow::read_parquet("tmp.parquet") %>%
    mutate(season = x)
  return(df)
})

lines <- purrr::map_df(seasons, function(x) {
  df <- read_csv(glue::glue("data/lines/{x}.csv"))
  return(df)
})

lines <- lines %>%
  group_by(id) %>%
    slice(1) %>%
    ungroup() %>%
  select(game_id = id,home_team = homeTeam,away_team = awayTeam,spread_line = spread,total_line = overUnder) %>%
  replace_na(list(total_line = 55.5))

filter_plays <- function(df) {
  tmp <- df %>%
    filter(
      down %in% c(4),
      #rush == 1 | pass == 1,
      !is.na(offense_play),
      !is.na(yards_to_goal),
      !is.na(score_diff)
    ) %>%
    left_join(lines,by = c("game_id")) %>%
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
      home_total = (spread_line + total_line) / 2,
      away_total = (total_line - spread_line) / 2,
      posteam_total = if_else(offense_play == home_team, home_total, away_total),
      posteam_spread = dplyr::if_else(offense_play == home_team, spread_line, -1 * spread_line),
      play_id = id_play,
      pos_score = pos_team_score,
      def_pos_score = def_pos_team_score,
      pos_team_receives_2H_kickoff = receives_2H_kickoff
    ) %>%
    # look at when an actual play is run or a defensive penalty gives a first down
    filter(#rush+pass == 1 | first_down_penalty == 1,
           distance > 0,
           yards_to_goal > 0,
           distance <= yards_to_goal,
           !is.na(posteam_spread),
           !is.na(posteam_total)) %>%
    mutate(label = as.double(yards_gained)) %>%
    mutate(label = label + 10) %>%
    # To prevent sim errors
    rename(yards_to_goal_end_old = yards_to_goal_end)
  return(tmp)
}

cleaned_pbp <- data.frame()
cleaned_pbp <- filter_plays(pbp)

# function to get WP for field goal attempt
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
  probs$ep <- sum(predict(object = cfbscrapR:::ep_model,newdata = probs,type = "probs")*c(0,3,-3,2,-7,-2,7) )
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

  probs$ep <- sum(predict(object = cfbscrapR:::ep_model,newdata = probs,type = "probs")*c(0,3,-3,2,-7,-2,7) )
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

    probs$ep <- predict(object = cfbscrapR:::ep_model,newdata = probs,type = "probs") %>%
      as_tibble() %>%
      mutate(ep =V1*0+V2*3+V3*-3+V4*2+V5*-7+V6*-2+V7*7) %>%
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
      #select(yards_to_goal_end,yards_to_goal,wp,pct,wt_wp) %>% view()
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
    select(down,
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
  game_state_long$ep <- predict(object = cfbscrapR:::ep_model,newdata = game_state_long,type = "probs") %>%
    as_tibble() %>%
    mutate(ep =V1*0+V2*3+V3*-3+V4*2+V5*-7+V6*-2+V7*7) %>%
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
           distance = ifelse(yards_to_goal < 10, yards_to_goal,distance)
    ) %>%
    mutate(Under_two = TimeSecsRem < 120,
           log_ydstogo = log(yards_to_goal),
           Goal_To_Go = distance == yards_to_goal,
           pos_score_diff_start = pos_score-def_pos_score,
           ep = NA,
           posteam_spread = if_else(pos_team == home_team,spread_line,-1*spread_line)
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
      pos_team = if_else(home_team == pos_team, away_team, home_team),
      posteam_spread = if_else(pos_team == home_team,spread_line,-1*spread_line)
    )%>%
    mutate(Under_two = TimeSecsRem < 120,
           log_ydstogo = log(yards_to_goal),
           Goal_To_Go = distance == yards_to_goal,
           #pos_score_diff_start = pos_score-def_pos_score,
           ep = NA,
           posteam_spread = if_else(pos_team == home_team,spread_line,-1*spread_line)
    )
}

make_tidy_data <- function(current_situation){
  x <- get_punt_wp(current_situation)
  y <- get_fg_wp(current_situation)
  z <- get_go_wp(current_situation)
  df <- current_situation
  fullInput <- df

  go <- tibble::tibble(
    "choice_prob" = z[[1]],
    "choice" = "Go for it",
    "success_prob" = z[[2]],
    "fail_wp" = z[[3]],
    "success_wp" = z[[4]]
  ) %>%
    select(choice, choice_prob, success_prob, fail_wp, success_wp)

  punt <- tibble::tibble(
    "choice_prob" = if_else(is.na(x), NA_real_, x),
    "choice" = "Punt",
    "success_prob" = NA_real_,
    "fail_wp" = NA_real_,
    "success_wp" = NA_real_
  ) %>%
    select(choice, choice_prob, success_prob, fail_wp, success_wp)

  fg <- tibble::tibble(
    "choice_prob" = y[[1]],
    "choice" = "Field goal attempt",
    "success_prob" = y[[2]],
    "fail_wp" = y[[3]],
    "success_wp" = y[[4]]
  ) %>%
    select(choice, choice_prob, success_prob, fail_wp, success_wp)

  tableData <- bind_rows(
    go, fg, punt
  ) %>%
    mutate(
      choice_prob = 100 * choice_prob,
      success_prob = 100 * success_prob,
      fail_wp = 100 * fail_wp,
      success_wp = 100 * success_wp
    ) %>%
    arrange(-choice_prob)



  choice <- dplyr::case_when(
    # football to punt
    fullInput$play_type %in% c("Blocked Punt", "Punt") ~ "Punt",
    # field goal
    fullInput$play_type %in% c("Field Goal Good", "Field Goal Missed") ~ "Field goal attempt",
    # go for it
    fullInput$play_type %in% c("Pass Incompletion", "Pass Reception", "Passing Touchdown", "Rush", "Rushing Touchdown", "Sack") ~ "Go for it",
    # penalty
    fullInput$play_type %in% c("Penalty") ~ "Penalty",
    TRUE ~ ""
  )
  wp1 <- tableData %>% dplyr::slice(1) %>% pull(choice_prob)
  wp2 <- tableData %>% dplyr::slice(2) %>% pull(choice_prob)

  diff <- wp1 - wp2
  recommend <- tableData %>% dplyr::slice(1) %>% pull(choice)

  tibble(
    "game_id" = current_situation$game_id,
    "play_id" = current_situation$play_id,
    "season" = current_situation$season,
    "week" = current_situation$week,
    "desc" = current_situation$play_text,
    "qtr" = current_situation$period,
    "TimeSecsRem" = current_situation$TimeSecsRem,
    "distance" = current_situation$distance,
    "yards_to_goal" = current_situation$yards_to_goal,
    "pos_team" = current_situation$pos_team,
    "home_team" = current_situation$home_team,
    "home_score" = ifelse(current_situation$home_team == current_situation$pos_team, current_situation$pos_team_score, current_situation$def_pos_team_score),
    "away_team" = current_situation$away_team,
    "away_score" = ifelse(current_situation$home_team == current_situation$pos_team, current_situation$def_pos_team_score, current_situation$pos_team_score),
    "fg_wp" = y[[1]],
    "fg_make_prob" = y[[2]],
    "fg_make_wp" = y[[4]],
    "fg_fail_wp" = y[[3]],
    "punt_wp" = x,
    "go_wp" = z[[1]],
    "go_success_prob" = z[[2]],
    "go_success_wp" = z[[4]],
    "go_fail_wp" = z[[3]],
    "choice" = choice,
    "recommendation" = recommend,
    "strength" = diff/100
  )
}

run_data_check <- function() {
  final_pbp <- data.frame()

  for (i in 1:5) {

    play <- cleaned_pbp %>% slice(i)
    message(play$id_play)

    final_pbp <- final_pbp %>%
      bind_rows(play %>% make_tidy_data())
  }
  return(final_pbp)
}
#run_data_check()


run_model <- function() {
  final_pbp <- data.frame()

  for ( i in 1:nrow(cleaned_pbp)) {
    play <- cleaned_pbp %>% slice(i)
    message(play$id_play)
    if (i %% 100 == 0) {
      message(glue::glue("Play {i} out of {nrow(cleaned_pbp)}"))
    }
    final_pbp <- final_pbp %>%
      bind_rows(play %>% make_tidy_data())
  }
  return(final_pbp)
}
tictoc::tic()
final_pbp <- run_model()
tictoc::toc()
saveRDS(final_pbp,"data/fd_pbp_{seasons}_new.RDS")





# cleaned_pbp %>%
#   anti_join(final_pbp,by = c("play_id","play_text" = "desc") ) %>% view()
#
# final_pbp %>%
#   left_join(cleaned_pbp %>% group_by(play_id,play_text) %>% slice(1) %>% ungroup() %>% select(play_id,play_text,EPA),
#             by = c("play_id","desc"="play_text")) %>% summary()


# seasons <- 2014:2020
# purrr::map(seasons, function(x) {
#   download.file(glue::glue("https://raw.githubusercontent.com/saiemgilani/cfbscrapR-data/master/data/parquet/pbp_players_pos_{x}.parquet"),"tmp.parquet")
#   df <- arrow::read_parquet("tmp.parquet")
#
#   cleaned_pbp <- data.frame()
#   cleaned_pbp <- filter_plays(df) %>%
#     group_by(play_id,play_text) %>%
#     slice(1) %>%
#     ungroup() %>%
#     select(play_id,play_text,EPA)
#
#   fd_file <- readRDS(glue::glue("data/fd_pbp_{x}.RDS"))%>%
#     left_join(cleaned_pbp,
#               by = c("play_id","desc"="play_text"))
#   #fd_file$EPA <- cleaned_pbp$EPA # mismatch is here
#   write.csv(fd_file, glue::glue("data/{x}.csv"))
# })
