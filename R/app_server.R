#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import dplyr
#' @noRd


# Define server logic required to draw a histogram
ML_server <- function(input, output,session) {
  #initialization
  apputils::use_apputils(force_AdminLTE = TRUE)
  initialize_helper()
  #
  tisefka <- callModule(module= ghred_tisefka_server, id = "SA_ML_data_upload")
  
  tisefka_inu <- callModule(module = SA_ML_features_server, id = "SA_ML_features", tisefka = reactive({tisefka()$tisefka_tizegzawin}))
  
  output$SA_dt <- DT::renderDataTable({
    req(tisefka_inu())
    DT::datatable(tisefka_inu()$selected_data,extensions = c('Scroller','Buttons'), options = list(deferRender = TRUE, dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
  })
  
  ML_trained_results <- callModule(module = SA_ML_preprocessing_server, id = "SA_ML_engine", tisefka = reactive({tisefka_inu()}))
  
  callModule(module = SA_ML_APP_server, id = "SA_ML_App", tisefka = reactive({
    ML_trained_results()}))
  
  ###########################################
  #########                           #######
  ###             TAGARA                  ###
  ########                            #######
  ###########################################
  #--- mdel shiny
  session$onSessionEnded(function() {
    stopApp()
  })
  #----- tagara
}

