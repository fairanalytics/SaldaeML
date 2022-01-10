
#------------------------ multiple-select, multiple output
#' Saldae Dashboard Module UI (analytics)
#' @description Saldae Dashboard module UI : forecasting
#' @author Farid Azouaou
#' @param id  server module ID
#' @param div_width dimension information about the framework(html object)
#' @param mod_title module title (default NULL)
#' @return UI module
#' @export

SA_ML_APP_UI <- function(id,mod_title = NULL ,div_width = "col-xs-12 col-sm-6 col-md-8") {
  ns <- NS(id)
  fluidPage(
    uiOutput(ns("ml_app_board")),
    uiOutput(ns("ml_app_UI")),
    uiOutput(ns("app_ML_output"))
  )
}


#' Saldae Dashboard Module Server Analytics
#' @description Saldae Dashboard module SERVER : render and generate multiple output objects for analytics
#' @author Farid Azouaou
#' @param input  input shinydashboard elements containing information to use for output generation
#' @param output output shinydashboard element
#' @param session shiny session
#' @param tisefka reactive object containing data
#' @param div_width dimension information about the framework(html object)
#' @return output objects to be displayed in corresponding UI module
#' @export

SA_ML_APP_server <- function(input, output, session,tisefka = NULL, create_flag = TRUE) {


  target_variable <- reactive({
    req(tisefka())
    tisefka()$target_variable
  })
  explaining_variables <- reactive({
    req(tisefka())
    tisefka()$explaining_variables
  })

  observeEvent(eventExpr=explaining_variables(),handlerExpr= {
    explaining_variables()%>%purrr::imap( ~{
      output_name_app <- paste0("SA_ML_app_", .x)
      output[[output_name_app]] <- renderUI({
        if(.y == "numeric"){
          shinyWidgets::numericInputIcon(
            inputId = session$ns(output_name_app),
            label = gsub("_"," ",.x),
            icon = icon("fa-sort-numeric-up"),
            value = ""
          )
        }else if(.y =="POSIXct"){
          dateInput(
            inputId = session$ns(output_name_app),
            label = gsub("_"," ",.x),
            value = ""
          )
        }else if(.y == "hms"){
          shinyTime::timeInput(
            session$ns(output_name_app),
            label = gsub("_"," ",.x),
            value = Sys.time()
          ) 
        }else{
          ml_choices <- tisefka()$var_factors[[.x]]
          shinyWidgets::pickerInput(
            inputId = session$ns(output_name_app),
            label = gsub("_"," ",.x),
            selected = NULL,
            choices = ml_choices
          )
          
        }
      })
     #
    })
  })
output$ml_app_board <- renderUI({
  req(tisefka())
  shinydashboardPlus::box(title = "ML app board",status = "warning",width = 12,
                      ststatus = "success",closable = FALSE,collapsible = TRUE,
                      fluidRow(column(width = 3,uiOutput(session$ns("trigger_ML_app"))),
                               column(width = 3,uiOutput(session$ns("create_ML_app"))))
                      )
})
  output$trigger_ML_app <- renderUI({
    shinyWidgets::actionBttn(
      inputId = session$ns("trigger_ML_app"),
      style = "material-flat",
      color = "success",
      label = "Run App")
  })

  output$create_ML_app <- renderUI({
    if(create_flag == TRUE){
      shinyWidgets::actionBttn(
        inputId = session$ns("create_ML_app"),
        style = "material-flat",
        color = "royal",
        label = "Create ML App")
    }else{
      NULL
    }
  })


output$ml_app_UI <- renderUI({
    req(explaining_variables())
    fluidRow(
      ml_app_UI <- purrr::map(explaining_variables(), ~{
        column(width = 3, uiOutput(session$ns(paste0("SA_ML_app_",.x))))
      })
    )
    return(ml_app_UI)
})

app_ML_output <- eventReactive(input$trigger_ML_app,{
  explaining_variables()%>%purrr::map(~req(input[[paste0("SA_ML_app_",.x)]]))
      tisefka_timaynutin <-explaining_variables()%>%purrr::map(~input[[paste0("SA_ML_app_",.x)]])%>%
        stats::setNames(explaining_variables())

      tisefka_timaynutin <- do.call(data.frame,tisefka_timaynutin)
      tisefka_timaynutin <- SA_ml_feature_generation(tisefka = tisefka_timaynutin)
      tisefka_timaynutin <- do.call(dplyr::bind_cols,tisefka_timaynutin)
      app_ML_output <- predict(tisefka()$SA_lm_fit,tisefka_timaynutin)
      return(app_ML_output)
})

app_ML_output <- eventReactive(input$trigger_ML_app,{
   
  explaining_variables()%>%purrr::map(~req(input[[paste0("SA_ML_app_",.x)]]))
  tisefka_timaynutin <-explaining_variables()%>%purrr::map(~input[[paste0("SA_ML_app_",.x)]])%>%
    stats::setNames(explaining_variables())
  
  time_hms_vars <- names(explaining_variables())
  time_hms_vars <- explaining_variables()[time_hms_vars=="hms"]

  tisefka_timaynutin <- do.call(data.frame,tisefka_timaynutin)
  
  if(length(time_hms_vars)>0){
    hms_input  <- time_hms_vars%>%purrr::map(~format(tisefka_timaynutin[,.x],"%H:%M:%S"))%>%
      stats::setNames(time_hms_vars)%>%data.frame(check.names = FALSE)%>%dplyr::as.tbl()%>%
      dplyr::mutate_if(is.factor,as.character)
    tisefka_timaynutin[,time_hms_vars]<- hms_input
  }
  tisefka_timaynutin <- SA_ml_feature_generation(tisefka = tisefka_timaynutin)
  tisefka_timaynutin <- do.call(dplyr::bind_cols,tisefka_timaynutin)
  app_ML_output <- predict(tisefka()$SA_lm_fit,tisefka_timaynutin)
  return(app_ML_output)
})

output$app_ML_output <- renderUI({
  req(app_ML_output())
  shinydashboard::valueBox(value = app_ML_output(),
                           subtitle = target_variable(),
                           color = "teal",
                           icon = icon("poll"))
})
observeEvent(input$create_ML_app,{
  ml_model_file <- paste0("./ML_models/SA_ML_model_",format(Sys.time(),"%Y_%m_%d_%H%M"),".rds")
  dir.create("./ML_models")
  saveRDS(tisefka(),ml_model_file)
  rstudioapi::jobRunScript("./inst/app_starter.R")
})


######### TAGARA #########
}
