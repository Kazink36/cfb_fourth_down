
make_table <- function(df, current_situation) {
  current_situation <- current_situation %>%
    mutate(score_differential = pos_score_diff_start,
           ydstogo = distance,
           yardline_100 = yards_to_goal,
           qtr = period,
           sec = TimeSecsRem %% 60,
           min = round(TimeSecsRem/60),
           min = ifelse(min>15,min-15,min),
           time = glue::glue("{min}:{str_pad(sec,2,side = 'left',pad = '0')}"))
  df %>%
    arrange(-choice_prob) %>%
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
    # tab_source_note(md("**Please cite**: Ben Baldwin's fourth down model"
    # )) %>%
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
    )  %>%
    tab_header(
      title = md(glue::glue("{case_when(current_situation$score_differential < 0 ~ 'Down', current_situation$score_differential == 0 ~ 'Tied', current_situation$score_differential > 0 ~ 'Up')} {ifelse(current_situation$score_differential == 0, 'up', abs(current_situation$score_differential))}, 4th & {current_situation$ydstogo}, {current_situation$yardline_100} yards from opponent end zone")),
      subtitle = md(glue::glue("Qtr {current_situation$qtr}, {current_situation$time}"))#hms::hms(current_situation$time) %>% substr(4, 8)}"))
    )

}

current_situation <- tibble(yards_to_goal = 53, down = 4,distance = 5,pos_score = 17,def_pos_score = 22,
                            TimeSecsRem = 292,half = 2,pos_team_timeouts_rem_before = 1,
                            def_pos_team_timeouts_rem_before = 2) %>%
  mutate(Under_two = TimeSecsRem < 120,
         period = ifelse(half == 2,3,1) + ifelse(TimeSecsRem < 900,1,0),
         log_ydstogo = log(yards_to_goal),
         Goal_To_Go = distance == yards_to_goal,
         pos_score_diff_start = pos_score-def_pos_score,ep = NA)
#Utah Punt
current_situation <- tibble(yards_to_goal = 37, down = 4,distance = 11,pos_score = 7,def_pos_score = 0,
                            TimeSecsRem = 1349,half = 1,pos_team_timeouts_rem_before = 3,
                            def_pos_team_timeouts_rem_before = 3) %>%
  mutate(Under_two = TimeSecsRem < 120,
         period = ifelse(half == 2,3,1) + ifelse(TimeSecsRem < 900,1,0),
         log_ydstogo = log(yards_to_goal),
         Goal_To_Go = distance == yards_to_goal,
         pos_score_diff_start = pos_score-def_pos_score,ep = NA)
#Memphis kneel
current_situation <- tibble(yards_to_goal = 29, down = 4,distance = 10,pos_score = 27,def_pos_score = 27,
                            TimeSecsRem = 7,half = 2,pos_team_timeouts_rem_before = 1,
                            def_pos_team_timeouts_rem_before = 0) %>%
  mutate(Under_two = TimeSecsRem < 120,
         period = ifelse(half == 2,3,1) + ifelse(TimeSecsRem < 900,1,0),
         log_ydstogo = log(yards_to_goal),
         Goal_To_Go = distance == yards_to_goal,
         pos_score_diff_start = pos_score-def_pos_score,ep = NA)
#Memphis lean forward
current_situation <- tibble(yards_to_goal = 23, down = 4,distance = 10,pos_score = 27,def_pos_score = 27,
                            TimeSecsRem = 7,half = 2,pos_team_timeouts_rem_before = 1,
                            def_pos_team_timeouts_rem_before = 0) %>%
  mutate(Under_two = TimeSecsRem < 120,
         period = ifelse(half == 2,3,1) + ifelse(TimeSecsRem < 900,1,0),
         log_ydstogo = log(yards_to_goal),
         Goal_To_Go = distance == yards_to_goal,
         pos_score_diff_start = pos_score-def_pos_score,ep = NA)


x <- get_punt_wp(current_situation, punt_df)

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



table <-
table %>% gtsave("bot/table.png")
