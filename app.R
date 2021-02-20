library(shiny)
library(shinydashboard)
library(cfbscrapR)
library(gt)
library(MASS, exclude = "select")
library(rtweet)
library(reactable)
library(htmltools)
library(tippy)
library(xgboost)
library(glue)
library(tidyverse)
source("R/bot_functions.R")
source("R/helpers.R")


#old_plays <- readRDS("data/old_plays_all.RDS")
old_plays <- map_df(2014:2020, function(x) {
    readRDS(glue::glue("data/fd_pbp_{x}.RDS"))
  }
)

team_info <- readRDS("data/team_info.RDS")
logos <- readRDS("data/logos.RDS")

header <- dashboardHeader(title = "A.I. Sports",
                          dropdownMenu(type = "notifications",badgeStatus = NULL,
                                       headerText = "",icon = icon("link"),
                                       notificationItem("4th Down Bot",
                                                        icon = icon("robot"),
                                                        status = "primary",
                                                        href = "https://twitter.com/aisports_4th"),
                                       notificationItem("A.I. Sports Website",
                                                        icon = icon("globe"),
                                                        status = "primary",
                                                        href = "http://aisportsfirm.com"),
                                       notificationItem(" A.I. Sports Twitter",
                                                        icon = icon("twitter"),
                                                        status = "primary",
                                                        href = "https://twitter.com/thefirmaisports"),
                                       notificationItem("Store",
                                                        icon = icon("tshirt"),
                                                        status = "primary",
                                                        href = "https://www.zazzle.com/store/ai_sports")
                          ))

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Model Calculator",tabName = "table"),
    #menuItem("Team Report", tabName = "summary"),
    menuItem("4th Down Plays", tabName = "plays"),
    menuItem("About", tabName = "about")

  )


)

body <- dashboardBody(
  includeCSS("style.css"),
  tags$head(tags$script('$(document).on("shiny:connected", function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            $(window).resize(function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            ')),
  fluidRow(
    tabItems(
      tabItem(tabName = "table",
              box(width = 12,
                  column( align = "center",
                          width = 6,
                          radioButtons("qtr", "Quarter", choices = 1:4, selected = 4,inline = TRUE),
                          sliderInput("minutes","Minutes left in quarter", min = 0,max = 15, value = 3),
                          sliderInput("seconds","Seconds left in quarter", min = 0,max = 60, value = 30),
                          radioButtons("pos_timeouts","Offense Timeouts", choices = 0:3, selected = 3, inline = TRUE),
                          radioButtons("def_timeouts","Defense Timeouts", choices = 0:3, selected = 3, inline = TRUE)
                  ),
                  column(
                    width = 6, align = "center",
                    sliderInput("distance","Yards to go",min = 1,max = 30,value = 5),
                    sliderInput("yards_to_goal","Yards from opponent End Zone", min = 1, max = 100, value = 35),
                    numericInput("pos_score","Offense Score",value = 0, min = 0, step = 1),
                    numericInput("def_pos_score", "Defense Score", value = 0, min = 0, step = 1),
                    numericInput("vegas_ou","Vegas Over/Under",value = 50, min = 0),
                    numericInput("posteam_spread","Offense Spread",value = 0)

                  ),
                  column(width = 12, align = "center",
                         actionButton("update", "Update", width = '50%')
                  )
              ),

              fluidRow(

                box(width = 12,
                    column(12, align="center",
                           gt_output(outputId = "suggestion") %>%
                             shinycssloaders::withSpinner(type = 6,# types see https://projects.lukehaas.me/css-loaders/
                                                          color = "#414141",
                                                          color.background = "#FFFFFF")
                    )

                )
              )
      ),
      tabItem(tabName = "summary",

              fluidRow(

                column(12, align = "center",

                       plotOutput("report_plot") %>%
                         shinycssloaders::withSpinner(type = 6,
                                                      color = "#414141",
                                                      color.background = "#FFFFFF")
                )
              )

      ),
      tabItem(tabName = "plays",
              box(width = 12,
                  column(width = 6,selectInput("team",
                                               "Team",
                                               choices = team_info %>% pull(school),
                                               selected = "Utah")),
                  column(width = 6,selectInput("season",
                                               "Season",
                                               choices = old_plays %>% distinct(season) %>% pull(season),
                                               selected = max(old_plays$season)))
                  ,
                  column(width = 12, reactableOutput("play_summary"))),
              br(),
              box(width = 12, title = "Filters",collapsible = TRUE,align = "center",
                  collapsed = FALSE,
                  column(6,
                         uiOutput("plays_def"),
                         # selectInput("team_def",
                         #             "Defense",
                         #             choices = c("ALL",team_info %>% pull(school)),
                         #             selected = "ALL"),
                         checkboxGroupInput("plays_ideal",
                                            "Ideal Decision:",
                                            inline = TRUE,
                                            selected = c("FG","Punt","Go"),
                                            choiceNames = c("FG","Punt","Go"),
                                            choiceValues = c("FG","Punt","Go"))
                         ),
                  column(6,

                         checkboxGroupInput("plays_qtr",
                                            "Quarter:",
                                            inline = TRUE,
                                            selected = c(1,2,3,4),
                                            choiceNames = c("1","2","3","4"),
                                            choiceValues = c(1,2,3,4)),
                         br(),
                         checkboxGroupInput("plays_actual",
                                            "Actual Decision:",
                                            inline = TRUE,
                                            selected = c("FG","Punt","Go"),
                                            choiceNames = c("FG","Punt","Go"),
                                            choiceValues = c("FG","Punt","Go"))),
                         ),
              column(width = 12, align = "center",
                     downloadButton("downloadData")
              )
      ),
      tabItem(tabName = "about",
              box(width = 12,
                  p("Notes: Data and WP model from @cfbscrapR. A thank you to Ben Baldwin (@benbbaldwin) for making his NFL fourth down calculator code public, which served as the basis for the majority of this model. Website by Jared Lee (@JaredDLee)."

                  ))

      )
    )
  )
)




ui <- shinydashboard::dashboardPage(header,sidebar,body,title = "AISports 4th Down",skin = "blue")

server <- function(input, output, session) {
  fullInput <- eventReactive(
    input$update,
    {

      tibble(qtr = as.numeric(input$qtr),#period = 3,
                            #half = 2,TimeSecsRem = 1638,
                            time = input$minutes*60+input$seconds,
                            pos_team = input$team,
                            #home_team = "Utah",
                            yards_to_goal = input$yards_to_goal, #yardline = "BYU 20",
                            distance = input$distance,
                            pos_team_timeouts_rem_before = as.numeric(input$pos_timeouts),
                            def_pos_team_timeouts_rem_before = as.numeric(input$def_timeouts),
                            pos_score = input$pos_score,
                            def_pos_score = input$def_pos_score,
                            vegas_total = input$vegas_ou,

                            posteam_spread = as.numeric(input$posteam_spread),
                            pos_team_receives_2H_kickoff = 1,
                            #Constants
                            down = 4,
                            home_opening_kickoff = 1,
                            runoff = 0
      ) %>%
        mutate(period = qtr,
               spread_line = posteam_spread,
               posteam_total = (-posteam_spread + vegas_total) / 2,
               home_team = pos_team,
               away_team = "Dummy",
               half = ifelse(period <= 2,1,2),
               TimeSecsRem = time + ifelse(period %in% c(1,3),900,0),
               Under_two = TimeSecsRem < 120,
               log_ydstogo = log(yards_to_goal),
               Goal_To_Go = distance == yards_to_goal,
               pos_score_diff_start = pos_score-def_pos_score,ep = NA,
               score_differential = pos_score_diff_start) %>%
        # put in end of game conditions
        dplyr::mutate(
          # if there's a conversion with fewer than 5 minutes left and a lead, run off 40 seconds
          runoff = ifelse(between(time, 167, 300) & score_differential > 0 & qtr == 4, 40, runoff),
          # if there's a conversion right before 2 minute warning, run down to 2 minute warning
          runoff = ifelse(between(time, 127, 166) & score_differential > 0 & qtr == 4, time - 120 - 6, runoff),
          # if conversion after 2 minute warning, run down 40 seconds
          runoff = ifelse(time <= 120 & score_differential > 0 & qtr == 4, 40, runoff)
        )


    } , ignoreNULL = FALSE
  )


  output$suggestion <- render_gt({
    fullInput() %>% make_table_data() %>%
      make_table(fullInput(),shiny = TRUE)
  })

  output$report_plot <- renderPlot({
    team <- input$team
    logos %>% filter(school == team) %>% pull(logos)
    image <- tibble(x = 4, y = .825,logo = logos %>% filter(school == team) %>% pull(logos))
    old_plays %>%
      filter(pos_team == team,
             choice != "Penalty",
             choice != "") %>%
      #mutate(recommendation = ifelse(strength < .001,"Toss-up",recommendation)) %>%
      select(pos_team,recommendation,choice) %>%
      count(recommendation,choice) %>%
      mutate(correct = ifelse(recommendation == choice,1,0),
             choice = fct_relevel(choice,"Field goal attempt","Punt")) %>%
      ggplot(aes(x = recommendation,y = n)) +
      geom_col(aes(fill = choice),position = "fill",alpha = .8,width = .9) +
      ggimage::geom_image(data = image,aes(x = x,y = y,image = logo),size = .2) +
      scale_y_continuous(labels = scales::percent,breaks = seq(0,1,.2),minor_breaks = seq(.1,.9,.2)) +
      scale_fill_brewer(palette = "Paired",name = "Actual Decision") +
      expand_limits(x = 4.5) +
      labs(x = "Ideal Decision",y = "",
           title = glue::glue("Decision Report - {team} 2020"),
           subtitle = "Based on recommendations from the A.I. Sports 4th Down Model",
           caption = "@JaredDLee\nData: @aisports_4th\nInspiration: @CharlieGel") +
      theme_minimal() +
      theme(panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.y = element_line(color = "gray80",size = .5),
            panel.grid.major.y = element_line(color = "gray30",size = .5),#,linetype = "dashed"),
            axis.ticks.x = element_blank(),
            axis.text.x = element_text(vjust = 5),
            axis.title.x = element_text(face = "bold",size = 16),
            legend.position = c(.87,.505),
            legend.title = element_text(face = "bold",size = 16),
            plot.background = element_rect(color = "#F8F8F8",fill = "#F8F8F8"),
            plot.title = element_text(hjust = .5,face = "bold",size = 26,vjust = -3),
            plot.subtitle = element_text(hjust = .5,face = "italic",size = 10,vjust = -8),
            title = element_text(size = 13),
            text = element_text(size = 16))
  }, height=reactive(ifelse(!is.null(input$innerWidth),input$innerWidth*3/5,0)))

  old_plays_season <- reactive({
    readRDS(glue("data/fd_pbp_{input$season}.RDS")) %>%
      filter(home_team %in% team_info$school,
             away_team %in% team_info$school)
  })

  table_data_prep <- reactive({
    old_plays_season() %>%
    filter(choice != "Penalty",
           choice != "",
           #pos_team == "Utah",
           #TimeSecsRem > 60,
           yards_to_goal > 0) %>%
      filter(pos_team == input$team) %>%
      #filter(season == input$season) %>%
    mutate(decision_value = if_else(recommendation == choice,strength,-strength)) %>%
    mutate(def_team = if_else(pos_team == home_team,away_team,home_team),
           pos_score = if_else(pos_team == home_team,home_score,away_score),
           def_pos_score = if_else(pos_team == home_team,away_score,home_score),
           score = glue::glue("{pos_score} - {def_pos_score}"),
           sec = TimeSecsRem %% 60,
           min = round(TimeSecsRem/60),
           min = ifelse(min>15,min-15,min),
           time = glue::glue("{min}:{str_pad(sec,2,side = 'left',pad = '0')}"),
           #desc = paste(time,desc),
           strength = round(100*strength,1),
           decision_value = round(100*decision_value,1),
           recommendation = case_when(recommendation == "Field goal attempt" ~"FG",
                                      recommendation == "Go for it" ~ "Go",
                                      TRUE ~ recommendation),
           choice = case_when(choice == "Field goal attempt" ~"FG",
                              choice == "Go for it" ~ "Go",
                              TRUE ~ choice),
           id = row_number()) %>%
    select(game_id,play_id,pos_team,def_team,qtr,time,distance,yards_to_goal,score,fg_wp,punt_wp,go_wp,recommendation,choice,decision_value,desc)
  })

output$plays_def <- renderUI({

  selectInput("team_def",
              "Defense",
              choices = c("ALL",table_data_prep() %>%
                            distinct(def_team) %>%
                            arrange(def_team) %>%
                            pull()),
              selected = "ALL")

})

observeEvent(input$team, {
  updateSelectInput(session,"team_def",selected = "ALL")
})
  table_data <- reactive({
    data <- table_data_prep()
    # if (input$team != "ALL") {
    #   data <- data %>%
    #     filter(pos_team == input$team)
    # }
    ##message(length(input$team_def))
    if(length(input$team_def)!= 0) {
      if (input$team_def != "ALL") {
        data <- data %>%
          filter(def_team == input$team_def)
      }
    }
    data %>%
      filter(recommendation %in% input$plays_ideal) %>%
      filter(choice %in% input$plays_actual) %>%
      filter(qtr %in% input$plays_qtr) %>%
      #filter(season == input$season) %>%
      #select(-pos_team) %>%
      mutate(id = row_number()) %>%
      return()
  })
  output$play_summary <- renderReactable({
    team <- input$team
    # table_data <- table_data_prep %>%
    #   filter(pos_team == team) %>%
    #   select(-pos_team) %>%
    #   mutate(id = row_number())

    with_tooltip <- function(value, tooltip, ...) {
      div(style = "text-decoration: underline; text-decoration-style: dotted; cursor: help",
          tippy(value, tooltip, ...))
    }
    ###message(input$team_def)
    table_data() %>%
      left_join(logos,by = c("pos_team" = "school")) %>%
      left_join(logos,by = c("def_team" = "school"),suffix = c("_pos","_def")) %>%
      select(-pos_team,-logos_pos,-def_team,-game_id,-play_id,-go_wp,-fg_wp,-punt_wp) %>%
      select(logos_def,everything()) %>%
      reactable(
        defaultColDef = colDef(
          align = "center",
          sortable = FALSE
        ),
        columns = list(
          id = colDef(
            show = FALSE,
            width = 40,
            name = ""
          ),
          # logos_pos = colDef(
          #   width = 85,
          #   #align = "left",
          #   cell = function(value) {
          #     image <- img(src = value, height = "50px",width = "50px", alt = value, align = "center")
          #   },
          #   name = "Offense"
          # ),
          logos_def = colDef(
            width = 85,
            #align = "left",
            cell = function(value) {
              image <- img(src = value, height = "50px",width = "50px", alt = value, align = "center")
            },
            name = "Opponent"
          ),
          qtr = colDef(
            sortable = TRUE,
            width = 65,
            header = with_tooltip("QTR","Quarter of the game"),
            name = "Quarter"
          ),
          time = colDef(
            width = 80,
            header = with_tooltip("Clock","Time remaining in the quarter"),
            name = "Clock"
          ),
          distance = colDef(
            sortable = TRUE,
            width = 90,
            name = "Yards to Go",
            header = with_tooltip("Distance","Yards to convert first down")
          ),
          yards_to_goal = colDef(
            sortable = TRUE,
            width = 65,
            name = "YTG",
            header = with_tooltip("YTG","Yards to opponent end zone")
          ),
          score = colDef(
            width = 75,
            name = "Score"
          ),
          recommendation = colDef(
            sortable = TRUE,
            width = 90,
            cell = function(value) {
              class <- paste0("tag status-", tolower(value))
              htmltools::div(class = class, value)
            },
            header = with_tooltip("Ideal Decision","Ideal play call according to the 4th down model"),
            name = "Ideal Decision"
          ),
          choice = colDef(
            sortable = TRUE,
            width = 90,
            cell = function(value) {
              class <- paste0("tag status-", tolower(value))
              htmltools::div(class = class, value)
            },
            header = with_tooltip("Actual Decision","Actual 4th down play call"),
            name = "Actual Decision"
          ),
          decision_value = colDef(
            sortable = TRUE,
            align = "right",
            width = 85,
            cell = function(value) {
              if (value >= 0) paste0("+", value) else value
            },
            style = function(value) {
              color <- if (value > 0) {
                "#008000"
              } else if (value < 0) {
                "#e00000"
              }
              list(fontWeight = 600, color = color)
            },
            headerStyle = list(
              textAlign = "center"
            ),
            header = with_tooltip("WP Change","Change in pre-snap win probability based on the actual decision"),
            name = "WP Change",
          ),
          desc = colDef(
            align = "left",
            minWidth = 250,
            name = "Play"
          )
        ),
        theme = reactableTheme(
          headerStyle = list(display = "flex", flexDirection = "column",justifyContent = "center"),
          # Vertically center cells
          cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center")
        )
      )
  })



  output$downloadData <- downloadHandler(
    filename = function() {
      paste('fourth_down_plays','.csv', sep='')
    },
    content = function(con) {
      write.csv(table_data(), con)
    }
  )

  observe({
    query <- parseQueryString(session$clientData$url_search)
    query1 <- paste(names(query), query, sep = "=", collapse=", ")
    # print(query1)
    # print(substr(query1, (nchar(query1) - 4), (nchar(query1))))
    # if(substr(query1, (nchar(query1) - 4), (nchar(query1))) == "2pt=1"){
    #   updateTabsetPanel(session, "inTabset", selected = "two_pt")
    # }
  })

}

shinyApp(ui, server, enableBookmarking = "url")

# input <- list(qtr = 3, minutes = 5, seconds = 30,team = "Utah",yards_to_goal = 20,distance = 5,pos_timeouts = 3,def_timeouts = 3,pos_score = 7, def_pos_score = 14,vegas_ou = 55.5, posteam_spread = -7)
#
# current_sit <- tibble(qtr = input$qtr,#period = 3,
#        #half = 2,TimeSecsRem = 1638,
#        time = input$minutes*60+input$seconds,
#        pos_team = input$team,
#        #home_team = "Utah",
#        yards_to_goal = input$yards_to_goal, #yardline = "BYU 20",
#        distance = input$distance,
#        pos_team_timeouts_rem_before = as.numeric(input$pos_timeouts),
#        def_pos_team_timeouts_rem_before = as.numeric(input$def_timeouts),
#        pos_score = input$pos_score,
#        def_pos_score = input$def_pos_score,
#        vegas_total = input$vegas_ou,
#
#        posteam_spread = input$posteam_spread,
#
#        pos_team_receives_2H_kickoff = 1,
#        #Constants
#        down = 4,
#        home_opening_kickoff = 1,
#        runoff = 0
# ) %>%
#   mutate(period = qtr,
#          spread_line = posteam_spread,
#          posteam_total = (-posteam_spread + vegas_total) / 2,
#          home_team = pos_team,
#          away_team = "Dummy",
#          half = ifelse(period <= 2,1,2),
#          TimeSecsRem = time + ifelse(period %in% c(1,3),900,0),
#          Under_two = TimeSecsRem < 120,
#          log_ydstogo = log(yards_to_goal),
#          Goal_To_Go = distance == yards_to_goal,
#          pos_score_diff_start = pos_score-def_pos_score,ep = NA,
#          score_differential = pos_score_diff_start) %>%
#   # put in end of game conditions
#   dplyr::mutate(
#     # if there's a conversion with fewer than 5 minutes left and a lead, run off 40 seconds
#     runoff = ifelse(between(time, 167, 300) & score_differential > 0 & qtr == 4, 40, runoff),
#     # if there's a conversion right before 2 minute warning, run down to 2 minute warning
#     runoff = ifelse(between(time, 127, 166) & score_differential > 0 & qtr == 4, time - 120 - 6, runoff),
#     # if conversion after 2 minute warning, run down 40 seconds
#     runoff = ifelse(time <= 120 & score_differential > 0 & qtr == 4, 40, runoff)
#   )


#
# table_data <- make_table_data(current_sit)
#
# make_table(table_data,current_sit,shiny = TRUE)
# #
# table
