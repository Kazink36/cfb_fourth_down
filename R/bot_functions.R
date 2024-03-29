# get all the 4th downs for a game
# with thanks to espn for the api
punt_df <- readRDS("data/punt_df.RDS")
fg_model <- readRDS("data/fg_model.RDS")
yard_model <- readRDS("data/fd_model.RDS")
team_info <- readRDS("data/team_info.RDS")
fd_model <- readRDS("data/fd_model.RDS")

wp_model <- xgboost::xgb.load("data/wp_model.model")
get_data <- function(df) {

  espn_game_id <- df$game_id
  home <- df$home_team
  away <- df$away_team
  week <- df$week

  plays <- data.frame()

  tryCatch(
    expr = {

      warn <- 0
      pbp <- httr::GET(url = glue::glue("http://site.api.espn.com/apis/site/v2/sports/football/college-football/summary?event={espn_game_id}")) %>%
        httr::content(as = "text", encoding = "UTF-8") %>%
        jsonlite::fromJSON(flatten = TRUE)

      if ("code" %in% names(pbp)) {
        warning(warn <- 1)
      }

      # get plays out of the drives lists
      # i think current drive duplicates a drive in previous drive so might be ok to cut this
      if ("current" %in% names(pbp$drives) & "previous" %in% names(pbp$drives)) {
        current_drive <- pbp$drives$current
        current_drive <- current_drive[['plays']] %>% bind_rows() %>% as_tibble() %>% mutate(team.abbreviation = current_drive$team$abbreviation)

        previous_drives <- pbp$drives$previous

        drives <- bind_rows(
          previous_drives %>% dplyr::select(team.abbreviation, plays) %>% unnest(plays),
          current_drive
        )
      } else if ("current" %in% names(pbp$drives)) {
        current_drive <- pbp$drives$current
        drives <- current_drive[['plays']] %>% bind_rows() %>% as_tibble() %>% mutate(team.abbreviation = current_drive$team$abbreviation)
      } else {
        previous_drives <- pbp$drives$previous
        drives <- previous_drives %>% dplyr::select(team.abbreviation, plays) %>% unnest(plays)
      }

      line <- pbp$pickcenter %>%
        mutate(provider = factor(provider.name,
                                      c(
                                        "consensus",
                                        "teamrankings",
                                        "numberfire",
                                        "Caesars",
                                        "Caesars (Pennsylvania)",
                                        "William Hill (New Jersey)",
                                        "SugarHouse",
                                        "Bovada"
                                      ))) %>%
        arrange(provider) %>%
        slice(1)

      suppressWarnings(

        plays <- drives %>%
          as_tibble() %>%
          group_by(id) %>%
          dplyr::slice(1) %>%
          ungroup() %>%
          janitor::clean_names() %>%
          dplyr::rename(
            abbreviation = team_abbreviation,
            qtr = period_number,
            yardline_100 = start_yards_to_endzone,
            yardline = start_possession_text,
            down = start_down,
            ydstogo = start_distance,
            desc = text,
            time = clock_display_value
          ) %>%
          left_join(team_info %>%
                      select(abbreviation,posteam = school) %>%
                      mutate(abbreviation = case_when(posteam == "Louisiana" ~ "UL",
                                                      posteam == "Wisconsin" ~ "WISC",
                                                      posteam == "Oklahoma" ~ "OU",
                                                      posteam == "Indiana" ~ "IU",
                                                      posteam == "Charlotte" ~ "CLT",
                                                      posteam == "UMass" ~ "MASS",
                                                      posteam == "Northwestern" ~ "NU",
                                                      posteam == "Miami" ~ "MIA",
                                                      # NOT FBS Teams
                                                      posteam == "Wagner" ~ "WAG",
                                                      posteam == "The Citadel" ~ "CIT",
                                                      #TEMPORARY FOR 2019
                                                      # posteam == "Wisconsin" ~ "WIS",
                                                      # posteam == "Louisiana Tech" ~ "LT",
                                                      # #TEMPORARY FOR 2018
                                                      # posteam == "Connecticut" ~ "UCONN",
                                                      # posteam == "Arkansas State" ~ "ARKST",
                                                      # posteam == "Akron" ~ "AKRON",
                                                     TRUE ~ abbreviation)),
                    by = "abbreviation") %>%
          dplyr::filter(qtr <= 4) %>%
          dplyr::mutate(
            # time column is wacky so extract it from play description when possible
            play_time = stringr::str_extract(desc, "\\([^()]+(?=\\)\\s)"),
            play_time = substr(play_time, 2, nchar(play_time)),
            play_min = stringr::str_extract(play_time, "[^()]+(?=\\:)") %>% as.integer(),
            play_min = if_else(is.na(play_min) & !is.na(play_time), as.integer(0), play_min),
            play_sec = substr(play_time, nchar(play_time) - 1, nchar(play_time)) %>% as.integer(),
            mins = if_else(nchar(time) == 5, substr(time, 1, 2), substr(time, 1, 1)) %>% as.integer(),
            secs = if_else(nchar(time) == 5, substr(time, 4, 5), substr(time, 3, 4)) %>% as.integer(),
            mins = if_else(is.na(play_min), mins, play_min),
            secs = if_else(is.na(play_sec), secs, play_sec)
          ) %>%
          arrange(qtr, desc(mins), desc(secs), id) %>%
          dplyr::mutate(
            home_team = home,
            away_team = away,
            defteam = if_else(posteam == home_team, away_team, home_team),
            half = if_else(qtr <= 2, 1, 2),
            challenge_team = stringr::str_extract(desc, "[:alpha:]*\\s*[:alpha:]*\\s*[:alpha:]*[:alpha:]+(?=\\schallenged)"),
            challenge_team = stringr::str_replace_all(challenge_team, "[\r\n]" , ""),
            challenge_team = stringr::str_trim(challenge_team, side = c("both")),
            desc_timeout = if_else(stringr::str_detect(desc, "Timeout #[:digit:]"), 1, 0),
            timeout_team = stringr::str_extract(desc, "(?<=Timeout #[:digit:] by )[:upper:]{2,3}"),

            home_timeout_used = case_when(
              timeout_team == home_team ~ 1,
              timeout_team != home_team ~ 0,
              is.na(timeout_team) ~ 0
            ),
            away_timeout_used = case_when(
              timeout_team == away_team ~ 1,
              timeout_team != away_team ~ 0,
              is.na(timeout_team) ~ 0
            ),
            home_timeouts_remaining = 3,
            away_timeouts_remaining = 3
          ) %>%
          dplyr::group_by(half) %>%
          arrange(qtr, desc(mins), desc(secs), id) %>%
          dplyr::mutate(
            total_home_timeouts_used = dplyr::if_else(cumsum(home_timeout_used) > 3, 3, cumsum(home_timeout_used)),
            total_away_timeouts_used = dplyr::if_else(cumsum(away_timeout_used) > 3, 3, cumsum(away_timeout_used))
          ) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(
            home_timeouts_remaining = home_timeouts_remaining - total_home_timeouts_used,
            away_timeouts_remaining = away_timeouts_remaining - total_away_timeouts_used,
            posteam_timeouts_remaining = dplyr::if_else(
              posteam == home_team,
              home_timeouts_remaining,
              away_timeouts_remaining
            ),
            defteam_timeouts_remaining = dplyr::if_else(
              defteam == home_team,
              home_timeouts_remaining,
              away_timeouts_remaining
            ),
            time = 60 * as.integer(mins) + as.integer(secs),
            home_score = dplyr::lag(home_score),
            away_score = dplyr::lag(away_score),
            score_differential = if_else(posteam == home_team, home_score - away_score, away_score - home_score),
            runoff = 0,
            yr = 2020,
            home_opening_kickoff = if_else(dplyr::first(na.omit(posteam)) == home_team, 1, 0),
            week = week,
            type = if_else(week <= 17, "reg", "post")
          ) %>%
          filter(
            down == 4,
            !str_detect(desc,"kickoff"),
            !str_detect(desc,"on-side"),
            !(time < 30 & qtr %in% c(4)),
            !str_detect(desc,"Timeout"),
            is.na(timeout_team),
            type_text != "Two-minute warning",
            type_text != "End Period"
          ) %>%
          group_by(qtr, time, ydstogo) %>%
          dplyr::slice(1) %>%
          ungroup() %>%
          arrange(qtr, desc(time), ydstogo) %>%
          mutate(
            game_id = df$game_id,
            yardline_side = purrr::map_chr(
              stringr::str_split(yardline, " "),
              function(x) x[1]
            ),
            yardline_side = case_when(
              yardline_side == "WSH" ~ "WAS",
              yardline_side == "LAR" ~ "LA",
              TRUE ~ yardline_side
            ),
            yardline_number = as.numeric(purrr::map_chr(
              stringr::str_split(yardline, " "),
              function(x) x[2]
            )),
            temp_yardline = dplyr::if_else(
              yardline_side == abbreviation | yardline_100 == 50,
              100 - yardline_number,
              yardline_number
            ),
            yardline_100 = if_else(
              !is.na(temp_yardline), as.integer(temp_yardline), yardline_100
            ),
            TimeSecsRem = time + ifelse(qtr == 1 | qtr == 3,900,0),
            half = ifelse(qtr<3,1,2),
            pos_score = ifelse(posteam == home_team,home_score,away_score),
            def_pos_score = ifelse(posteam == home_team,away_score,home_score),
            score_differential = pos_score - def_pos_score,
            period = qtr,
            total_line = line$overUnder,
            spread_line = line$spread,
            home_total = (spread_line + total_line) / 2,
            away_total = (total_line - spread_line) / 2,
            posteam_total = if_else(posteam == home_team, home_total, away_total),
            posteam_spread = dplyr::if_else(posteam == home_team, spread_line, -1 * spread_line)
          ) %>%
          select(
            game_id,
            play_id = id,
            yr,
            desc,
            type,
            qtr,
            period,
            half,
            TimeSecsRem,
            time,
            posteam,
            abbreviation,
            # yardline_side,
            away_team,
            home_team,
            yards_to_goal = yardline_100,
            down,
            yardline,
            distance = ydstogo,
            pos_team_timeouts_rem_before = posteam_timeouts_remaining,
            def_pos_team_timeouts_rem_before = defteam_timeouts_remaining,
            home_opening_kickoff,
            score_differential,
            posteam_total,
            posteam_spread,
            runoff,
            home_score,
            away_score,
            pos_score,
            def_pos_score,
            type_text,
            yr
          ) %>%
          mutate(Under_two = TimeSecsRem < 120,
                 distance = ifelse(distance == 0,1,distance),
                 period = ifelse(half == 2,3,1) + ifelse(TimeSecsRem < 900,1,0),
                 log_ydstogo = log(yards_to_goal),
                 Goal_To_Go = distance == yards_to_goal,
                 pos_score_diff_start = pos_score-def_pos_score,ep = NA) %>%
          # put in end of game conditions
          dplyr::mutate(
            # if there's a conversion with fewer than 5 minutes left and a lead, run off 40 seconds
            runoff = ifelse(between(time, 167, 300) & score_differential > 0 & qtr == 4, 40, runoff),
            # if there's a conversion right before 2 minute warning, run down to 2 minute warning
            runoff = ifelse(between(time, 127, 166) & score_differential > 0 & qtr == 4, time - 120 - 6, runoff),
            # if conversion after 2 minute warning, run down 40 seconds
            runoff = ifelse(time <= 120 & score_differential > 0 & qtr == 4, 40, runoff)
          )
      )


      if (nrow(plays) > 0) {
        plays <- plays %>%
          mutate(
            index = 1 : n()
          )
      } else {
        plays$index <- NA_real_
      }

    },
    error = function(e) {
      message("The following error has occured:")
      message(e)
    },
    warning = function(w) {
      if (warn == 1) {
        message(glue::glue("Warning: The requested GameID {espn_game_id} ({df$game_id}) is invalid!"))
      }
    },
    finally = {
    }

  )

  return(plays)
}
# prepare_df <- function(df,games) {
#  # df has specific game info
#   #games has all game info I guess?
#
#
# }
make_table <- function(df, current_situation,shiny = FALSE) {
  current_situation <- current_situation %>%
    mutate(score_differential = if_else(pos_team == home_team,home_score-away_score,away_score-home_score),#pos_score_diff_start,
           ydstogo = distance,
           yardline_100 = yards_to_goal,
           #qtr = period,
           sec = TimeSecsRem %% 60,
           min = floor(TimeSecsRem/60),
           min = ifelse(min>15,min-15,min),
           min = ifelse(min == 15 & sec>0,min-15,min),
           time = glue::glue("{min}:{str_pad(sec,2,side = 'left',pad = '0')}"))
  table <- df %>%
    arrange(-choice_prob) %>%
    # filter(!(success_prob == 0 & choice == "Field goal attempt")) %>%
    # filter(!is.na(choice_prob)) %>%
    gt() %>%
    cols_label(
      choice = "",
      choice_prob = "Win %",
      success_prob = "Success %",
      success_wp = "Succeed",
      fail_wp = "Fail"
    ) %>%
    tab_style(
      style = cell_text(color = "black", weight = "bold"),
      locations = list(
        cells_row_groups(),
        cells_column_labels(everything())
      )
    ) %>%
    tab_options(
      row_group.border.top.width = px(3),
      row_group.border.top.color = "black",
      row_group.border.bottom.color = "black",
      table_body.hlines.color = "white",
      table.border.top.color = "black",
      table.border.top.width = px(1),
      table.border.bottom.color = "white",
      table.border.bottom.width = px(1),
      column_labels.border.bottom.color = "black",
      column_labels.border.bottom.width = px(2)
    ) %>%
    fmt_number(
      columns = vars(choice_prob, success_prob, success_wp, fail_wp), decimals = 0
    ) %>%
    tab_source_note(md("Based on Ben Baldwin's NFL fourth down model using cfbfastR's WP model"
    )) %>%
    tab_style(
      style = list(
        cell_text(color = "red", weight = "bold")
      ),
      locations = cells_body(
        columns = vars(choice_prob)
      )
    )  %>%
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_body(
        columns = vars(choice)
      )
    )  %>%
    tab_spanner(label = "Win % if",
                columns = 4:5) %>%
    cols_align(
      columns = 2:5,
      align = "center"
    ) %>%
    tab_footnote(
      footnote = "Expected win % for a given decision",
      locations = cells_column_labels(2)
    ) %>%
    tab_footnote(
      footnote = "Likelihood of converting on 4th down or of making field goal",
      locations = cells_column_labels(3)
    )
  if(shiny) {
    team_info <- readRDS("data/team_info.RDS")
    # logo <- team_info %>%
    #   filter(school == current_situation$posteam) %>%
    #   pull(logo)
    wp1 <- df %>%
      arrange(-choice_prob) %>%
      dplyr::slice(1) %>%
      pull(choice_prob)
    wp2 <- df %>%
      arrange(-choice_prob) %>%
      dplyr::slice(2) %>%
      pull(choice_prob)

    diff <- wp1 - wp2
    choice <- df %>%
      arrange(-choice_prob) %>% dplyr::slice(1) %>% pull(choice)
    choice <- if_else(abs(diff) < 1, "Toss-up", choice)
    table %>%
      tab_header(
        title = md(glue::glue(#"<img src='{logo}' style='width:100px;'></img><br>
          "Correct Choice: <span style='color:#FF0102'>**{choice}**</span> (difference: <span style='color:#048004'>**{round(diff,1)}%**</span>)<br>{case_when(current_situation$score_differential < 0 ~ 'Down', current_situation$score_differential == 0 ~ 'Tied', current_situation$score_differential > 0 ~ 'Up')} {ifelse(current_situation$score_differential == 0, 'up', abs(current_situation$score_differential))}, 4th & {current_situation$ydstogo}, {current_situation$yardline_100} yards from opponent end zone")),
        subtitle = md(glue::glue("Qtr {current_situation$qtr}, {current_situation$time}"))
      )
  } else {

    table %>%
      tab_header(
        title = md(glue::glue("{case_when(current_situation$score_differential < 0 ~ 'Down', current_situation$score_differential == 0 ~ 'Tied', current_situation$score_differential > 0 ~ 'Up')} {ifelse(current_situation$score_differential == 0, 'up', abs(current_situation$score_differential))}, 4th & {current_situation$ydstogo}, {current_situation$yardline_100} yards from opponent end zone")),
        subtitle = md(glue::glue("Qtr {current_situation$qtr}, {current_situation$time}"))
      )
  }
}
make_table_data <- function(current_situation) {
  current_situation <- current_situation %>%
    mutate(spread_line = posteam_spread,
           pos_team_receives_2H_kickoff = case_when(posteam == home_team & home_opening_kickoff == 0 ~ 1,
                                                    posteam == home_team & home_opening_kickoff == 1 ~ 0,
                                                    posteam == away_team ~ home_opening_kickoff),
           pos_team = posteam)
  x <- get_punt_wp(current_situation)

  y <- get_fg_wp(current_situation)
  z <- get_go_wp(current_situation)

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

  for_return <- bind_rows(
    go, fg, punt
  ) %>%
    mutate(
      choice_prob = 100 * choice_prob,
      success_prob = 100 * success_prob,
      fail_wp = 100 * fail_wp,
      success_wp = 100 * success_wp
    )
  return(for_return)
}
make_tidy_data <- function(current_situation){
  current_situation <- current_situation %>%
    mutate(spread_line = posteam_spread,
           pos_team_receives_2H_kickoff = case_when(posteam == home_team & home_opening_kickoff == 0 ~ 1,
                                                    posteam == home_team & home_opening_kickoff == 1 ~ 0,
                                                    posteam == away_team ~ home_opening_kickoff),
           pos_team = posteam,
           play_type = type_text,
           play_text = desc,
           pos_team_score = pos_score,
           def_pos_team_score = def_pos_score)

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
    fullInput$play_type %in% c("Blocked Punt", "Punt","Safety",
                               "Blocked Punt Touchdown","Punt Return Touchdown") ~ "Punt",
    # field goal
    fullInput$play_type %in% c("Field Goal Good", "Field Goal Missed","Blocked Field Goal") ~ "Field goal attempt",
    # go for it
    fullInput$play_type %in% c("Pass Incompletion", "Pass Reception", "Passing Touchdown",
                               "Rush", "Rushing Touchdown", "Sack","Interception",
                               "Fumble Recovery (Opponent)","Pass Interception Return",
                               "Fumble Return Touchdown") ~ "Go for it",
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
tidy_to_table_data <- function(df) {
  go <- tibble::tibble(
    "choice_prob" = df$go_wp,
    "choice" = "Go for it",
    "success_prob" = df$go_success_prob,
    "fail_wp" = df$go_fail_wp,
    "success_wp" = df$go_success_wp
  ) %>%
    select(choice, choice_prob, success_prob, fail_wp, success_wp)

  punt <- tibble::tibble(
    "choice_prob" = if_else(is.na(df$punt_wp), NA_real_, df$punt_wp),
    "choice" = "Punt",
    "success_prob" = NA_real_,
    "fail_wp" = NA_real_,
    "success_wp" = NA_real_
  ) %>%
    select(choice, choice_prob, success_prob, fail_wp, success_wp)

  fg <- tibble::tibble(
    "choice_prob" = df$fg_wp,
    "choice" = "Field goal attempt",
    "success_prob" = df$fg_make_prob,
    "fail_wp" = df$fg_fail_wp,
    "success_wp" = df$fg_make_wp
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
  return(tableData)
}
# function to tweet out one play
#df is current_situation
tweet_play <- function(df,tidy = FALSE) {
  fullInput <- df %>% mutate(type_text = choice)
  if(tidy){
    tableData <- tidy_to_table_data(df)%>%
      arrange(-choice_prob)
  } else {
    tableData <- make_table_data(df) %>%
      arrange(-choice_prob)
  }

  play_desc <- df$desc %>%
    stringr::str_replace("\\([:digit:]*\\:[:digit:]+\\)\\s", "") %>%
    substr(1, 80)

  choice_emoji <- dplyr::case_when(
    fullInput$choice == "Punt" ~ "\U0001f3c8\U0001f9B5",
    fullInput$choice == "Field goal attempt" ~ "\U0001f45F\U0001f3c8",
    fullInput$choice == "Go for it" ~ "\U0001f449",
    # football to punt
    fullInput$type_text %in% c("Blocked Punt", "Punt") ~ "\U0001f3c8\U0001f9B5",
    # field goal
    fullInput$type_text %in% c("Field Goal Good", "Field Goal Missed") ~ "\U0001f45F\U0001f3c8",
    # go for it
    fullInput$type_text %in% c("Pass Incompletion", "Pass Reception", "Passing Touchdown", "Rush", "Rushing Touchdown", "Sack") ~ "\U0001f449",
    # penalty
    fullInput$type_text %in% c("Penalty") ~ "\U0001f6A8",
    TRUE ~ ""
  )


  wp1 <- tableData %>% dplyr::slice(1) %>% pull(choice_prob)
  wp2 <- tableData %>% dplyr::slice(2) %>% pull(choice_prob)

  diff <- wp1 - wp2
  choice <- tableData %>% dplyr::slice(1) %>% pull(choice)
  choice <- if_else(abs(diff) < 1, "Toss-up", choice)

  rec_emoji <- dplyr::case_when(
    choice == "Go for it" ~ "\U0001f449",
    choice == "Field goal attempt" ~ "\U0001f45F\U0001f3c8",
    choice == "Punt" ~ "\U0001f3c8\U0001f9B5",
    choice == "Toss-up" ~ "\U0001f937"
  )

  confidence <- case_when(
    abs(diff) < 1 ~ "",
    abs(diff) >= 1 & abs(diff) < 3 ~ "(MEDIUM)",
    abs(diff) >= 3 & abs(diff) <= 10 ~ "(STRONG)",
    abs(diff) > 10 ~ "(VERY STRONG)"
  )

   position <-# if_else(
  #   !is.na(df$yardline),
  #   glue::glue("at the {df$yardline}"),
    glue::glue("{df$yards_to_goal} yards from opponent end zone")
  #)##

  posteam <- df$pos_team
  defteam <- if_else(df$pos_team == df$home_team, df$away_team, df$home_team)

  table <- make_table(tableData, df,shiny = TRUE)

  table %>% gtsave("bot/post.png",zoom = 3)
  # ---> {df$away_team} ({df$away_score}) @ {df$home_team} ({df$home_score}) <---
  text <-
    glue::glue(
      "
  {df$away_team} ({df$away_score}) @ {df$home_team} ({df$home_score})
  {posteam} has 4th & {df$distance} {position}

  Recommendation {confidence}: {rec_emoji} {choice} (+{round(diff, 1)} WP)
  Actual play: {choice_emoji} {play_desc}
  ")

  # don't post if every choice is < 1 or > 99
  if (wp1 > 1 & wp2 > 1 & wp1 < 99 & wp2 < 99) {
    post_tweet(text, media = "bot/post.png")
  }
  # post_tweet(text)

}
