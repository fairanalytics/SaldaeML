#' Saldae Dashboard Module UI
#' @description Saldae Dashboard module UI : data upload
#' @author Farid Azouaou
#' @param id  server module ID
#' @export
ghred_tisefka_UI <- function(id){
  ns <- NS(id)
fluidPage(
  tags$head(tags$style(".progress-bar{background-color:#A6761D;}")),
  fluidRow(
    column(width = 3,
           fileInput(inputId = ns("tisefka_file"), label = "Choose CSV File",
                     multiple = FALSE,
                     accept = c("csv")
           )),
    column(width= 3,
           shinyWidgets::radioGroupButtons(
             inputId = ns("tisefka_tala"),
             label = "Data Source :",
             choices = c(
               `<i class="fas fa-table"></i>` = "table", `<i class="fas fa-database"></i>` = "database"),
             status = "success",
             justified = TRUE,
             selected = "table"
           )
    ),
    column(width = 3, uiOutput(ns("excel_tawriqt")))
  ),
  #-----------date related settings
  #---- Help text
  # uiOutput(ns("date_variable_help")),
#
#   fluidRow(column(width = 3,
#                   uiOutput(ns("date_variable"))),
#            column(width = 3,
#                   uiOutput(ns("SA_date_format")))
#            ),


  #---- Data Overview 1(key figures)
  uiOutput(ns("SA_tisefka_overview")),

  #-------- Data overview 2
  div(class = "col-xs-12 col-sm-12 col-md-12",
      shinydashboard::tabBox(width = 12, title = "Data Diagnosis",
                             tabPanel(title = "Overview",icon = icon("eye"),
                                      rhandsontable::rHandsontableOutput(ns("tisefka_view"))
                             ),
                             tabPanel(title = "Statistics",icon = icon("table"),
                                      rhandsontable::rHandsontableOutput(ns("tisefka_description"))
                             ),
                             tabPanel(title = "Outliers",icon = icon("table"),
                                      rhandsontable::rHandsontableOutput(ns("tisefka_outliers"))
                             )
      )
  )
)

}

#' Saldae dashboard module: upload data
#' @description upload rwa data and prepare it to be used for exploration and analysis
#' @author Farid Azouaou
#' @param input  input shinydashboard elements containing information to use data upload
#' @param output output shinydashboard element
#' @param session shiny session
#' @return output objects to be displayed in corresponding UI module
#' @export

ghred_tisefka_server <-function(input, output, session){

  file_tasetta <- reactive({
    req(input$tisefka_file)
    tools::file_ext(input$tisefka_file$name)
  })
  excel_tiwriqin <- reactive({
   req(file_tasetta())
   if(file_tasetta()=="csv")return(NULL)
   if(file_tasetta()=="xlsx"){
     readxl::excel_sheets(input$tisefka_file$datapath)
   }
  })
  output$excel_tawriqt <- renderUI({
    req(excel_tiwriqin())
    if(is.null(excel_tiwriqin()))return(NULL)
    shinyWidgets::pickerInput(
      inputId = session$ns("excel_tawriqt"),
      label = "Choose excel sheet:",
      choices = excel_tiwriqin(),
      options = list(
        style = "btn-primary"
      )
    )
  })

  tisefka <- reactive({
    req(file_tasetta())
   ghred_tisefka_aqerru(input_file = input$tisefka_file, tala = file_tasetta(), tawriqt = input$excel_tawriqt)
  })

tisefka_tizegzawin <- reactive({
  req(tisefka())
  tisefka()%>% dplyr::distinct(., .keep_all = TRUE)%>%
    janitor::remove_constant()%>%janitor::remove_empty()%>%
    janitor::clean_names()
})

#--------- display raw data
data_summary <- reactive({
  req(tisefka_tizegzawin())
  diag_output <- data_diagnosis_f(tisefka = tisefka_tizegzawin(), categoricals_ukud = NULL)
  return(diag_output)
})

#------- display data Overview

output$SA_tisefka_overview <- renderUI({
  req(tisefka_tizegzawin())
  fluidRow(
    # shinydashboard::valueBoxOutput(session$ns("SA_overview1")),
    shinydashboard::infoBoxOutput(session$ns("SA_overview2")),
    shinydashboard::infoBoxOutput(session$ns("SA_overview3"))

  )
})
# output$SA_overview1 <- shinydashboard::renderInfoBox({
#   req(ts_time_units())
#   shinydashboard::infoBox(title = "Time frequency",
#                           value = ts_time_units()[1],subtitle = "",color = "olive",
#                           shiny::icon("hourglass")
#   )
# })
output$SA_overview2 <- shinydashboard::renderInfoBox({
  req(tisefka_tizegzawin())
  req(data_summary())
  info_val <- paste(ncol(tisefka_tizegzawin()),"x",nrow(tisefka_tizegzawin()))
  shinydashboard::infoBox(title = "Data Info",
                          value = info_val,subtitle = "Variables(columns) x Elements(rows)",color = "green",
                          shiny::icon("bar-chart")
  )
})
output$SA_overview3 <- shinydashboard::renderInfoBox({
  req(data_summary())
  val_quality <- mean(data_summary()$diagnosis[,"missing_percent",drop=F])
  val_quality <- round((1- ifelse(is.na(val_quality),0,val_quality))*100,2)

  qual_col <- "green"
  if(val_quality < 80 )qual_col <- "navy"
  if(val_quality < 50 )qual_col <- "orange"
  if(val_quality < 30 )qual_col <- "red"

  shinydashboard::infoBox(title = "Data Quality",
                          value = paste(val_quality,"%"),subtitle = "not missing values",color = qual_col,
                          shiny::icon("tasks")
  )
})


#----- detect numerical variables
numeric_variables <- reactive({
    req(data_summary())
    dat_diag <- data_summary()$diagnosis
    numericals <- dat_diag[grepl("numeric", dat_diag$types), "variables", drop = T]
    multi_integers <- dat_diag[grepl("integer", dat_diag$types), c("unique_count", "variables"), drop = T]
    if (nrow(multi_integers)) {
      multi_integers <- multi_integers[multi_integers["unique_count"] > 10, "variables", drop = T]
    } else {
      multi_integers <- NULL
    }
    return(c(numericals, multi_integers))
})

# tisefka_overview <- reactive({
#   req(numeric_variables())
#   req(data_summary())
# })

#-------------------------

output$tisefka_description <- rhandsontable::renderRHandsontable({
  req(data_summary())
  return(rhandsontable::rhandsontable(data_summary()$beschreibung, rowHeaders = NULL, width = 1000, height = 300))
})

output$tisefka_outliers <- rhandsontable::renderRHandsontable({
  req(data_summary())
  return(rhandsontable::rhandsontable(data_summary()$outliers, rowHeaders = NULL, width = 1000, height = 300))
})

output$tisefka_view <- rhandsontable::renderRHandsontable({
  req(data_summary())
  data_exploration_view(tisefka = tisefka_tizegzawin(), tisefka_report = data_summary(),numeric_variables = numeric_variables())
})

tisefka_tizedganin <- reactive({
  req(tisefka_tizegzawin())
  return(SA_tisefka_impute(tisefka_tizegzawin()))
})


explore_output <- reactive({
  req(tisefka_tizedganin())
  output <- list()
  output$tisefka_tizegzawin <- tisefka_tizedganin()
  output$data_summary   <- data_summary()
  output$numeric_variables  <- numeric_variables()
  return(output)
})

}
