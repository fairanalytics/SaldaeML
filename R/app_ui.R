#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
library("dplyr")
ML_ui <- function(request) {
  # Define UI for application that draws a histogram
  tagList(
    shinydashboardPlus::dashboardPage(
     # md = TRUE,
      skin = "green-light",
      controlbar = shinydashboardPlus::dashboardControlbar(),
      footer = shinydashboardPlus::dashboardFooter(),
      header = shinydashboardPlus::dashboardHeader(title = "Fair Analytics Auto ML"),
      sidebar= shinydashboard::dashboardSidebar(disable = FALSE,
                                       shinydashboard::sidebarMenu(
                                         shinydashboard::menuItem("Data Preparation", tabName = "data_upload", icon = icon("table")),
                                         shinydashboard::menuItem("ML engine", tabName = "ML_model_build", icon = icon("fas fa-brain")),
                                         shinydashboard::menuItem("ML app creator", tabName = "ML_app_creator", icon = icon("fas fa-mobile"))
                                         
                                         
                                         
                                       )),
      body = shinydashboard::dashboardBody(
        shiny.info::info_panel(
          shiny.info::powered_by("Fair Analytics", link = "https://www.fairanalytics.net/"),
          position = "bottom right"
        ),
        shinybusy::add_busy_bar(color = "orange",centered = TRUE,height = "10px" ),
        shinydashboard::tabItems(
          shinydashboard::tabItem("data_upload",
                                  shinydashboard::tabBox(width = 12, title = "Data Preparation",
                                                         tabPanel(title = "Data Upload",
                                                                  ghred_tisefka_UI("SA_ML_data_upload")
                                                                  
                                                         ),tabPanel(title = "Features Engineering",
                                                                    SA_ML_features_UI("SA_ML_features")
                                                                    
                                                         ),tabPanel(title = "Cleaned Data",
                                                                    DT::DTOutput("SA_dt")
                                                         )
                                  )
                                  
          ),
          shinydashboard::tabItem("ML_model_build",
                                  shinydashboard::tabBox(width = 12, title = "Model building",
                                                         tabPanel(title = "ML training ",
                                                                  SA_ML_preprocessing_UI("SA_ML_engine")
                                                         )
                                  )
                                  
          ),
          shinydashboard::tabItem("ML_app_creator",
                                  shinydashboard::tabBox(width = 12, title = "Build ML app",
                                                         tabPanel(title = "myApplication ",
                                                                  SA_ML_APP_UI("SA_ML_App")
                                                         )
                                  )
                                  
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'SaldAutoML'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

