library(shiny)
library(shinyWidgets)
library(shinydashboardPlus)
library(shinythemes)
library(shinybusy)
library(reactable)
library(plotly)
library(DBI)
library(odbc)

library(modeltime)
library(tidymodels)

library(tidyverse)
library(lubridate)
library(timetk)
library(xlsx)


# Initial Inputs

sales_data_tbl <- read_rds(file = "sales_data_tbl.rds")

holidays <- read_rds(file = "holidays.rds")

project_options <- unique(sales_data_tbl$proj_id)

cat_options <- unique(sales_data_tbl$cat_id)

subdiv_options <- unique(sales_data_tbl$subdiv_id)

options(lubridate.week.start = 1)

last_week <- floor_date(today(), unit = "week") - days(1)

last_month <- floor_date(today(), unit = "month") - days(1)

ui <- navbarPage(
    title = "Прогнозирование продаж",
    collapsible = TRUE,
    inverse     = TRUE, 
    theme       = shinytheme("darkly"),
    
  #  add_busy_spinner(spin = "radar", margins = c(10, 20)),
    
    shiny::tabPanel(
        title = "Иерархическое прогнозирование",
        includeCSS("css/styles.css"),
        sidebarLayout(
            sidebarPanel(
                # h3("Что выполняется"),
                # h5("Прогнозирование продаж на любом уровне иерархии"),
                # hr(),
                h3("Как работает приложение:"),
                h5("Начальная дата прогнозов зависит от выбранной периодичности: если периодичность неделя, то прогнозы будут формироваться с текущей недели, если месяц - с текущего месяца"),
                hr(),
                # shiny::dateInput(
                #     inputId = "start_date", 
                #     label   = "Выберите начальную дату",
                #     value   = today(),
                #     weekstart = 1,
                #     language = "ru"
                # ),
                shiny::radioButtons(
                    inputId  = "period",
                    label    = "Выберите периодичность",
                    #choices = c("неделя", "месяц"),
                    choiceNames = list("неделя", "месяц"),
                    choiceValues = list("week", "month")#,
                    #selected = "неделя"
                ),
                numericInputIcon(
                    inputId = "horizon",
                    value   = 4,
                    min     = 1,
                    max     = 12,
                    label   = "Введите прогнозный период",
                    icon    = icon("chart-line")
                ),
                shiny::actionButton(inputId = "submit",
                                    label   = "Запустить прогноз",
                                    class   = "btn-primary"),
                # shiny::numericInput(
                #     inputId = "horizon",
                #     label = "Введите прогнозный период", value = 3, min = 1, max = 12),
                h5("Выберите один из уровней иерахии прогноза:"),
                shiny::selectInput(
                    inputId   = "project", 
                    label     = "Выберите проект",
                    choices   = c("Все", project_options),
                    selected = "Все"
                ),
                shiny::selectInput(
                    inputId   = "cat_napr", 
                    label     = "Выберите категор.направление",
                    choices   = c("Все", cat_options),
                    selected = "Все"
                ),
                shiny::selectInput(
                    inputId   = "subdiv", 
                    label     = "Выберите подразделение",
                    choices   = c("Все", subdiv_options),
                    selected = "Все"
                ),
                # appButton(
                #     inputId = "run_forecast",
                #     label   = "Запустить прогноз",
                #     dashboardBadge("Вперед!", color = "blue"),
                #     icon    = icon("cog"),
                #     class   = "pull-right"
                # ),
                shiny::radioButtons(
                    inputId  = "lookback",
                    label    = "Выберите временное окно",
                    choiceNames  = list("1 год", "2 года", "3 года"),
                    choiceValues = list("1 years", "2 years", "3 years"),
                    #selected = "2 years"
                ),
                hr(),
                downloadButton("download", "Выгрузить прогноз"),
                hr(),
                h5("Точность тестового прогноза"),
                div(
                    class = "row",
                    div(
                        class = "col-sm-12 panel",
                        # div(class = "panel-heading", h5("Average of Models")),
                        div(
                            class = "panel-body",
                            reactableOutput("reactable_accuracy")
                        )
                    )
                ),
            ),
            mainPanel(
                div(
                    class = "row",
                    div(
                        class = "col-sm-12 panel",
                        div(class = "panel-heading", h4("Рассчитанный прогноз")),
                        div(
                            class = "panel-body",
                            #shinycssloaders::withSpinner(plotlyOutput("plotly_forecast"))
                            plotlyOutput("plotly_forecast")
                        )
                    )
                ),
                div(
                  class = "row",
                  div(
                    class = "col-sm-12 panel",
                    div(class = "panel-heading", h4("Прогнозные значения:")),
                    div(
                      class = "panel-body",
                      tableOutput("forecast_tbl")
                    )
                  )
                ),
                div(
                    class = "row",
                    div(
                        class = "col-sm-12 panel",
                        div(class = "panel-heading", h4("Тестовый прогноз")),
                        div(class = "panel-body", 
                            # verbatimTextOutput(outputId = "print")
                            plotlyOutput("plotly_test")
                        )
                    )
                ),

                # # Used for debugging
                # verbatimTextOutput(outputId = "code"),
                # div(
                #     class = "col-sm-12",
                #     leafletOutput(outputId = "leaflet", height = "100%")
                # )
          
               
            )
        )
        
    )
    
)

# Define server logic 
server <- function(input, output, session) {

    observeEvent(input$period,{
        if(input$period == "week"){
          updateNumericInputIcon(session, "horizon", value = 4) 
        }else{ message(input$period)
          updateNumericInputIcon(session, "horizon", value = 3)
        }
    })
    
    observeEvent(input$project,{
        if(input$project != "Все"){
            updateSelectInput(session, "cat_napr", choices = "Все")
            updateSelectInput(session, "subdiv", choices = "Все")
        }else{
            updateSelectInput(session, "cat_napr", choices = c("Все", cat_options))
            updateSelectInput(session, "subdiv", choices = c("Все", subdiv_options))
        }
    })
    
    observeEvent(input$cat_napr,{
        if(input$cat_napr != "Все"){
            updateSelectInput(session, "subdiv", choices = "Все") 
        }else{
            updateSelectInput(session, "subdiv", choices = c("Все", subdiv_options))
        }
    })

    # Setup Reactive Values ----
     rv <- reactiveValues()

     observeEvent(input$submit, {
         
         show_modal_spinner(
             spin = "cube-grid",
             color = "firebrick",
             text = "Рассчет прогнозов..."
         )

           # PREPARE FULL DATA ----

         rv$full_data_tbl <- sales_data_tbl %>%

             group_by(proj_id, subdiv_id, cat_id) %>%
             summarise_by_time(date, .by = input$period, value = sum(value), lockdown = sum(lockdown)) %>%
             ungroup() %>%

             # PERFORM HIERARCHICAL AGGREGATIONS
             add_column(all_stores_id = "all_stores", .before = 1) %>%
             pivot_longer(
                 cols      = ends_with("_id"),
                 names_to  = "category",
                 values_to = "identifier"
             ) %>%

             group_by(category, identifier, date) %>%
             summarise(value = sum(value, na.rm = TRUE), lockdown = mean(lockdown)) %>%
             ungroup() %>%

             mutate(value = ifelse(value < 0, 0, value)) %>%
             
             # Remove all identifier with less than 3 sale period
             mutate(lag_ident = lag(identifier)) %>% 
             mutate(flag = case_when(lag_ident == identifier ~ 1, 
                                     TRUE ~ 0)) %>% 
             group_by(identifier) %>% 
             mutate(count_ident = cumsum(flag)) %>% 
             mutate(total_count = max(count_ident)) %>% 
             ungroup() %>%  
             filter(total_count > (input$horizon * 2 - 1)) %>% 
             select(-c(6:9)) %>%

             # APPLY TIME SERIES FEATURE ENGINEERING
             group_by(category, identifier) %>%

             # Extend
             future_frame(date, .length_out =  paste0(input$horizon, " ", input$period, "s"), .bind_data = TRUE) %>%

             # Add time series features
             tk_augment_lags(value, .lags = input$horizon, .names = "long_lag") %>%
             tk_augment_slidify(
                 long_lag,
                 .f       = ~ mean(., na.rm = TRUE),
                 .period  = c(2, 3, 4),
                 .align   = "center",
                 .partial = TRUE
             ) %>%

             ungroup() %>%

             rowid_to_column(var = "row_id") %>%

             mutate(lockdown = ifelse(is.na(lockdown), 0, lockdown)) %>%
             rowwise() %>%
             mutate(holidays = sum(seq.Date(date, date + period(1, input$period) - days(1), by = "day") %in% holidays)) %>%
             ungroup()

          rv$data_prepared_tbl <- rv$full_data_tbl %>%
             filter(!is.na(value)) %>%
             filter(!is.na(long_lag))

          rv$future_data_tbl <- rv$full_data_tbl %>%
             filter(is.na(value))

          # Models ----

         # * Train/Test ----
         rv$splits <- rv$data_prepared_tbl %>%
              time_series_split(date, assess = input$horizon, cumulative = TRUE)

          # RECIPE ----

          if(input$period == "week"){
            rv$recipe_spec <- recipe(value ~ ., data = training(rv$splits)) %>%
              update_role(row_id, date, new_role = "id") %>%
              step_timeseries_signature(date) %>%
              step_rm(matches("(.xts$)|(.iso$)|(day)|(hour)|(minute)|(second)|(am.pm)")) %>%
              step_mutate(date_week = factor(date_week, ordered = TRUE)) %>%
              step_dummy(all_nominal(), one_hot = TRUE)
          }
          if(input$period == "month"){
            rv$recipe_spec <- recipe(value ~ ., data = training(rv$splits)) %>%
              update_role(row_id, date, new_role = "id") %>%
              step_timeseries_signature(date) %>%
              step_rm(matches("(.xts$)|(.iso$)|(week)|(day)|(hour)|(minute)|(second)|(am.pm)")) %>%
              step_normalize(date_index.num, starts_with("date_year")) %>% 
              step_dummy(all_nominal(), one_hot = TRUE) %>% 
              step_zv(all_predictors())
          }
  
          rv$recipe_spec %>% prep() %>% juice()


          # ** MODEL SPECS ----

          # A. XGBOOST
          wflw_xgboost_defaults <- workflow() %>%
              add_model(
                  boost_tree(mode = "regression",
                             ) %>%
                      set_engine("xgboost")
              ) %>%
              add_recipe(rv$recipe_spec) %>%
              fit(training(rv$splits))

          wflw_xgboost_tweedie <- workflow() %>%
              add_model(
                  boost_tree(mode = "regression") %>%
                      set_engine("xgboost", objective = "reg:tweedie")
              ) %>%
              add_recipe(rv$recipe_spec) %>%
              fit(training(rv$splits))


          # B. GLMNET

          wflw_glmnet <- workflow() %>%
              add_model(
                  linear_reg(
                      penalty = 200,
                      mixture = 0.99
                  ) %>%
                      set_engine("glmnet")
              ) %>%
              add_recipe(rv$recipe_spec) %>%
              fit(training(rv$splits))
          
          # # 4. PROPHET
          # 
          # wflw_prophet <- workflow() %>%
          #   add_model(
          #     prophet_reg() %>%
          #       set_engine("prophet")
          #   ) %>%
          #   add_recipe(rv$recipe_spec) %>%
          #   fit(training(rv$splits))
          # 
          # 
          # 4. SVM ----
          wflw_fit_svm <- workflow() %>%
            add_model(
              svm_rbf() %>% set_engine("kernlab")
            ) %>%
            add_recipe(rv$recipe_spec) %>%
            fit(training(rv$splits))


        # Calibration -----
        rv$calibration_tbl <- modeltime_table(
            wflw_xgboost_defaults,
            wflw_xgboost_tweedie,
            wflw_glmnet,
            # wflw_prophet,
            wflw_fit_svm
        ) %>%
            modeltime_calibrate(testing(rv$splits)) %>%
            mutate(.model_desc = ifelse(.model_id == 2, str_c(.model_desc, " - Tweedie"), .model_desc))

          # * Forecast Test ----

          rv$test_forecast_tbl <- rv$calibration_tbl %>%
              modeltime_forecast(
                  new_data    = testing(rv$splits),
                  keep_data   = TRUE
              )

          rv$accuracy_by_identifier_tbl <- rv$test_forecast_tbl %>%
              select(identifier, .model_id, .model_desc, .index, .value,  value) %>%
              group_by(identifier, .model_id, .model_desc) %>%
              summarize_accuracy_metrics(
                  truth      = value,
                  estimate   = .value,
                  metric_set = default_forecast_accuracy_metric_set()
              ) %>%
              ungroup()

          rv$best_rmse_by_indentifier_tbl <- rv$accuracy_by_identifier_tbl %>%
              group_by(identifier) %>%
              slice_min(rmse, n = 1) %>%
              ungroup()

          # REFITTED FORECAST ----

          rv$refitted_tbl <- rv$calibration_tbl %>%
              modeltime_refit(
                  data = rv$data_prepared_tbl
              )

          rv$future_forecast_tbl <- rv$refitted_tbl %>%
              modeltime_forecast(
                  new_data    = rv$future_data_tbl,
                  actual_data = rv$data_prepared_tbl,
                  keep_data   = TRUE
              )

          # FILTER BEST ----

          rv$actual_tbl <- rv$future_forecast_tbl %>% filter(.model_desc == "ACTUAL")

          rv$future_forecast_best_tbl <- rv$future_forecast_tbl %>%
              right_join(
                  rv$best_rmse_by_indentifier_tbl %>% select(identifier, .model_id, .model_desc),
                  by = c(".model_id", ".model_desc", "identifier")
              )

          rv$test_forecast_best_tbl <- rv$test_forecast_tbl %>%
              right_join(
                  rv$best_rmse_by_indentifier_tbl %>% select(identifier, .model_id, .model_desc),
                  by = c(".model_id", ".model_desc", "identifier")
              )
          
          remove_modal_spinner()

     }) #, ignoreNULL = FALSE)
     
     ident_filter <- reactive(ifelse(input$project != "Все", input$project,
                         ifelse(input$cat_napr != "Все", input$cat_napr,
                                ifelse(input$subdiv != "Все", input$subdiv, "all_stores"))))
 
     # Plotly Forecast ----
     output$plotly_forecast <- renderPlotly({

         req(rv$future_forecast_best_tbl, rv$actual_tbl)
         
         bind_rows(
             rv$actual_tbl,
             rv$future_forecast_best_tbl
         ) %>%
             filter(identifier == ident_filter()) %>%
             filter_by_time(
                 .start_date = last(date) %-time% input$lookback, 
                 .end_date = "end"
             ) %>%
             plot_modeltime_forecast(.title = "", .legend_show = FALSE)
     })
     
     # Forecsast Values ----
     output$forecast_tbl <- renderTable({

       req(rv$future_forecast_best_tbl)

       rv$future_forecast_best_tbl %>%
         filter(identifier == ident_filter()) %>%
         select(date, .value) %>% 
         mutate(date = month(date, label = TRUE),
                .value = format(round(as.numeric(.value), 0), big.mark=","))

     })


     # Plotly Test ----
     output$plotly_test <- renderPlotly({

         req(rv$test_forecast_tbl, rv$actual_tbl)
         
         bind_rows(
             rv$actual_tbl,
             rv$test_forecast_best_tbl
         ) %>%
             filter(identifier == ident_filter()) %>%
             filter_by_time(
                 .start_date = last(date) %-time% input$lookback, 
                 .end_date = "end"
             ) %>%
             plot_modeltime_forecast(.title = "", .legend_show = FALSE)
     })


     # Test Accuracy ----
     output$reactable_accuracy <- renderReactable({
         req(rv$best_rmse_by_indentifier_tbl)
         
         # id_filter <- ifelse(input$project != "Все", input$project,
         #                     ifelse(input$cat_napr != "Все", input$cat_napr,
         #                            ifelse(input$subdiv != "Все", input$subdiv, "all_stores")))

         rv$best_rmse_by_indentifier_tbl %>%
             filter(identifier == ident_filter()) %>%
             select(.model_id, .model_desc, mape, rmse) %>%
             table_modeltime_accuracy(resizable = TRUE, bordered = TRUE)
     })
     
     # Downloadable xlsx of selected period and horizon ----
     output$download <- downloadHandler(

       filename = function() {
         paste(input$period, ".xlsx", sep = "")
       },

       content = function(file) {
         rv$future_forecast_best_tbl %>%
             filter(category %in% c("all_stores_id" ,"proj_id")) %>%
             select(identifier, date, .value, .conf_lo, .conf_hi) %>%
             mutate(date = month(date, label = TRUE)) %>% 
             write.xlsx(file, sheetName = "Проекты")
         rv$future_forecast_best_tbl %>%
             filter(category == "subdiv_id") %>%
             select(identifier, date, .value) %>%
           mutate(date = month(date, label = TRUE)) %>% 
             write.xlsx(file, sheetName = "Подразделения", append = TRUE)
         rv$future_forecast_best_tbl %>%
             filter(category == "cat_id") %>%
             select(identifier, date, .value) %>%
             mutate(date = month(date, label = TRUE)) %>% 
             write.xlsx(file, sheetName = "КН", append = TRUE)
       }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
