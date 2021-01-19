library(shiny)
library(shinydashboard)
library(cfbscrapR)
library(gt)
library(MASS)
library(rtweet)
library(reactable)
library(htmltools)
library(tippy)
library(xgboost)
library(tidyverse)
source("R/bot_functions.R")
source("R/helpers.R")

old_plays <- readRDS("data/old_plays_all.RDS")
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
                  selectInput("team",
                              "Team",
                              choices = team_info %>% pull(school),
                              selected = "Utah"),
                  reactableOutput("play_summary")),
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

      tibble(qtr = input$qtr,#period = 3,
                            #half = 2,TimeSecsRem = 1638,
                            time = input$minutes*60+input$seconds,
                            posteam = input$team,
                            #home_team = "Utah",
                            yards_to_goal = input$yards_to_goal, #yardline = "BYU 20",
                            distance = input$distance,
                            pos_team_timeouts_rem_before = as.numeric(input$pos_timeouts),
                            def_pos_team_timeouts_rem_before = as.numeric(input$def_timeouts),
                            pos_score = input$pos_score,
                            def_pos_score = input$def_pos_score,
                            vegas_total = input$vegas_ou,

                            posteam_spread = input$posteam_spread,

                            #Constants
                            down = 4,
                            home_opening_kickoff = 1,
                            runoff = 0
      ) %>%
        mutate(period = qtr,
               posteam_total = (-posteam_spread + vegas_total) / 2,
               home_team = posteam,
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
    fullInput() %>% make_table_data(punt_df) %>%
      make_table(fullInput(),shiny = TRUE)
  })

  output$report_plot <- renderPlot({
    team <- input$team
    logos %>% filter(school == team) %>% pull(logos)
    image <- tibble(x = 4, y = .825,logo = logos %>% filter(school == team) %>% pull(logos))
    old_plays %>%
      filter(posteam == team,
             choice != "Penalty",
             choice != "") %>%
      #mutate(recommendation = ifelse(strength < .001,"Toss-up",recommendation)) %>%
      select(posteam,recommendation,choice) %>%
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

  table_data_prep <- reactive({
    old_plays %>%
    filter(choice != "Penalty",
           choice != "",
           #posteam == "Utah",
           #TimeSecsRem > 60,
           yards_to_goal > 0) %>%
      filter(posteam == input$team) %>%
    mutate(decision_value = if_else(recommendation == choice,strength,-strength)) %>%
    mutate(defteam = if_else(posteam == home_team,away_team,home_team),
           pos_score = if_else(posteam == home_team,home_score,away_score),
           def_pos_score = if_else(posteam == home_team,away_score,home_score),
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
    select(game_id,play_id,posteam,defteam,qtr,time,distance,yards_to_goal,score,fg_wp,punt_wp,go_wp,recommendation,choice,decision_value,desc)
  })

output$plays_def <- renderUI({

  selectInput("team_def",
              "Defense",
              choices = c("ALL",table_data_prep() %>%
                            distinct(defteam) %>%
                            arrange(defteam) %>%
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
    #     filter(posteam == input$team)
    # }
    ##message(length(input$team_def))
    if(length(input$team_def)!= 0) {
      if (input$team_def != "ALL") {
        data <- data %>%
          filter(defteam == input$team_def)
      }
    }
    data %>%
      filter(recommendation %in% input$plays_ideal) %>%
      filter(choice %in% input$plays_actual) %>%
      filter(qtr %in% input$plays_qtr) %>%
      #select(-posteam) %>%
      mutate(id = row_number()) %>%
      return()
  })
  output$play_summary <- renderReactable({
    team <- input$team
    # table_data <- table_data_prep %>%
    #   filter(posteam == team) %>%
    #   select(-posteam) %>%
    #   mutate(id = row_number())

    with_tooltip <- function(value, tooltip, ...) {
      div(style = "text-decoration: underline; text-decoration-style: dotted; cursor: help",
          tippy(value, tooltip, ...))
    }
    ###message(input$team_def)
    table_data() %>%
      left_join(logos,by = c("posteam" = "school")) %>%
      left_join(logos,by = c("defteam" = "school"),suffix = c("_pos","_def")) %>%
      select(-posteam,-logos_pos,-defteam,-game_id,-play_id) %>%
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

}

shinyApp(ui, server)





# current_sit <- tibble(qtr = 3,#period = 3,
#                       #half = 2,TimeSecsRem = 1638,
#                       time = 738,
#                       posteam = "Utah",
#                       #home_team = "Utah",
#                       yards_to_goal = 20, down = 4, #yardline = "BYU 20",
#                       distance = 5,
#                       pos_team_timeouts_rem_before = 3,
#                       def_pos_team_timeouts_rem_before = 3,
#                       pos_score = 22,
#                       def_pos_score = 25,
#                       #score_differential = -3,
#                       posteam_total = 50,
#                       posteam_spread = -7,
#
#                       home_opening_kickoff = 1,
#                       runoff = 0,
#                       #home_score = 25,
#                       #away_score = 22,
#                       #type_text = "Rush",
#                       #yr = 2020
#                       ) %>%
#   mutate(period = qtr,
#          home_team = posteam,
#          away_team = "Dummy",
#          half = ifelse(period <= 2,1,2),
#          TimeSecsRem = time + ifelse(period %in% c(1,3),900,0),
#          Under_two = TimeSecsRem < 120,
#          #distance = ifelse(distance == 0,1,distance),
#          #period = ifelse(half == 2,3,1) + ifelse(TimeSecsRem < 900,1,0),
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
# table_data <- make_table_data(current_sit,punt_df)
#
# make_table(table_data,current_sit,shiny = TRUE)
#
# table
