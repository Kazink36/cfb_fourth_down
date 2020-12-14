library(tidyverse)
library(ggtext)

interp_TimeSecsRem_live <- function(pbp) {
  temp <- pbp %>%
    mutate(TimeSecsRem = ifelse(TimeSecsRem == lag(TimeSecsRem),NA,TimeSecsRem)) %>%
    select(TimeSecsRem)
  min_holder<-min(pbp$TimeSecsRem)
  #ind <- which(temp$TimeSecsRem == 1800)
  temp$TimeSecsRem[1] <- 3600
  temp$TimeSecsRem[nrow(temp)] <- min_holder
  #temp$TimeSecsRem[ind-1] <- 0
  pbp<- pbp %>%
    mutate(TimeSecsRem = round(zoo::na.approx(temp$TimeSecsRem))) %>%
    mutate(clock.minutes = floor(TimeSecsRem/60),clock.seconds = TimeSecsRem %% 60)
  return(pbp)
}

teams <- cfbscrapR::cfb_team_info()


df<-cfbscrapR::cfb_game_info(2020,week = 14,team = "Utah")

game_info <- df %>% left_join(teams,by = c("home_team" = "school")) %>%
  unnest_wider(logos,names_sep = "_") %>%
  left_join(teams %>% unnest_wider(logos,names_sep = "_"),
            by = c("away_team" = "school"),suffix = c("_home","_away"))
espn_game_id <-df$game_id
home <- game_info$abbreviation_home
away <- game_info$abbreviation_away
week <- df$week

plays <- data.frame()


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


suppressWarnings(
  plays <- drives %>%
    as_tibble() %>%
    group_by(id) %>%
    dplyr::slice(1) %>%
    ungroup() %>%
    janitor::clean_names() %>%
    dplyr::rename(
      posteam = team_abbreviation,
      qtr = period_number,
      yardline_100 = start_yards_to_endzone,
      yardline = start_possession_text,
      down = start_down,
      ydstogo = start_distance,
      desc = text,
      time = clock_display_value
    ) %>%
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
      posteam = case_when(
        posteam == "WSH" ~ "WAS",
        posteam == "LAR" ~ "LA",
        TRUE ~ posteam
      ),
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
    mutate(ep = NA,wp = NA) %>%
    mutate(TimeSecsRem = time + (4-qtr)*900) %>%
    #filter(nrow(.)!=row_number()) %>%
    interp_TimeSecsRem_live() %>%
    mutate(TimeSecsRemTrue = TimeSecsRem,
           TimeSecsRem = TimeSecsRem - ifelse(qtr < 3,1800,0)) %>%
    rename(yards_to_goal = yardline_100) %>%
    separate(start_short_down_distance_text,into = c("down","distance"),sep = " & ") %>%

    mutate(log_ydstogo = log(yards_to_goal),
           distance = ydstogo,
           Goal_To_Go = distance <= yards_to_goal,
           pos_score_diff_start = score_differential,
           Under_two = TimeSecsRem < 120) %>%
    mutate(down = str_extract(down,"^."),
           down = as.character(down)) %>%
    rename(pos_team_timeouts_rem_before = posteam_timeouts_remaining,
           def_pos_team_timeouts_rem_before = defteam_timeouts_remaining)
)

plays$ep<-predict(object = cfbscrapR:::ep_model,newdata = plays,type = "probs") %>%
  as_tibble() %>%
  mutate(ep =V1*0+V2*3+V3*-3+V4*2+V5*-7+V6*-2+V7*7) %>%
  pull(ep)#*c(0,3,-3,2,-7,-2,7),na.rm = TRUE )
plays <- plays %>%
  mutate(ExpScoreDiff = score_differential+ep,
         ExpScoreDiff_Time_Ratio = ExpScoreDiff/TimeSecsRemTrue) %>%
  filter(ExpScoreDiff_Time_Ratio != Inf)

plays$wp <- predict(object = cfbscrapR:::wp_model,newdata = plays,type = "response")
plays <- plays %>%
  mutate(wpa = wp-lag(wp),
         epa = ep - lag(ep),
         home_score = ifelse(posteam==home_team & scoring_play,case_when(type_abbreviation == "TD" ~ ifelse(str_detect(desc,"MISSED"),home_score+6,home_score+7),
                                                                         type_abbreviation == "FG" ~ home_score + 3),home_score),
         away_score = ifelse(posteam==away_team & scoring_play,case_when(type_abbreviation == "TD" ~ ifelse(str_detect(desc,"MISSED"),away_score+6,away_score+7),
                                                                         type_abbreviation == "FG" ~ away_score + 3),away_score))

images <- tibble(logo = c(game_info$logos_1_home,game_info$logos_1_away),
                 x = 52.5*60,y = c(.4,-.4))
big_plays <- plays %>%
  filter(scoring_play) %>%
  mutate(wp = ifelse(posteam == home,wp,1-wp),wp = wp-.5) %>%
  mutate(desc = str_remove(desc,",.{0,50}"),
         desc = str_remove(desc,"Timeout .{0,50}")) %>%
  select(desc,wp,TimeSecsRemTrue,posteam,wpa)
big_plays$desc <- sapply(strsplit(big_plays$desc, " "), function(x) {
  spacePosition <- cumsum(nchar(x))
  placeBreak <- spacePosition[which(diff(spacePosition %/% 20) == 1)] + 1
  result <- paste(x, collapse = " ")
  for(i in placeBreak) {
    substring(result, i, i) <- "\n"
  }
  result
})
big_plays <- big_plays %>%
  mutate(desc = paste0(desc,"\n",posteam," ",wpa*100,"%"))

plays %>%
  filter(!is.na(wp)) %>%
  mutate(wp = ifelse(posteam == home,wp,1-wp),wp = wp-.5) %>%
  #mutate(home_score = ifelse(row_number() == nrow(plays) & ))
  # mutate(wp_home = ifelse(posteam==home,wp,1-wp),
  #                               wp_away = ifelse(posteam == away,wp,1-wp)) %>%
  # select(-wp) %>% pivot_longer(cols = c("wp_home","wp_away"),names_to = "homeaway",values_to = "wp") %>% view()
  ggplot(aes(x = TimeSecsRemTrue,y = wp)) +
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed") +
  geom_hline(yintercept = c(-.5,.5),color = "black",linetype = "solid",alpha = .7) +
  geom_vline(xintercept = 60*c(0,15,30,45,60), linetype = "dashed", color= "black") +
  ggimage::geom_image(data = images,aes(x = x,y = y,image = logo),inherit.aes = FALSE,size = .15,asp = 1.2) +
  geom_area(alpha = .2) +
  geom_line(aes(color = wp),size = 2,lineend = "round") +
  # geom_point(data = big_plays) +
  # ggrepel::geom_label_repel(data = big_plays,aes(label = desc),alpha = .6) +
  scale_x_reverse(limits = c(3600,0),breaks = 60*rev(seq(0,60,5)),labels = function(x){x/60}) +
  scale_y_continuous(labels = c("100%","75%","50%","75%","100%"),limits = c(-.5,.5)) +
  scale_color_gradient2(low = game_info$color_away,mid = "gray60",high = game_info$color_home,midpoint = 0) +
  #scale_color_manual(values = c(game_info$color_home,game_info$color_away)) +
  labs(x = "Time Remaining (Minutes)",y = "Win Probability",
       title = glue::glue("<span style='color:{game_info$color_away}'>{game_info$away_team}</span> at <span style='color:{game_info$color_home}'>{game_info$home_team}</span>"),
       subtitle = glue::glue("{max(plays$away_score,na.rm = TRUE)} - {max(plays$home_score,na.rm = TRUE)}"),
       caption = "Jared Lee (@JaredDLee)\nData: ESPN | WP Model: @cfbscrapR") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = "none",
        text = element_text(size = 14),
        plot.background = element_rect(fill = "#F8F8f8"),
        plot.title = element_markdown(size = 20,hjust = .5,face = "bold"),
        plot.subtitle = element_text(size = 16,hjust = .5,face = "bold")
        )
ggsave(filename = glue::glue("{game_info$home_team}-{Sys.time()}.png"),width = 10,height = 8,units = "in")
