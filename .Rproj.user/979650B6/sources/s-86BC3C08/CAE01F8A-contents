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
  
  apputils::use_apputils(force_AdminLTE = TRUE)
  
  tisefka <- callModule(module= ghred_tisefka_server, id = "SA_ML_data_upload")
  
  tisefka_inu <- callModule(module = SA_ML_features_server, id = "SA_ML_features", tisefka = reactive({tisefka()$tisefka_tizegzawin}))
  
  output$SA_dt <- DT::renderDT({
    req(tisefka_inu())
    tisefka_feature_table <- tisefka_inu()$selected_data
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

