# source('~/Saldae_Analytics/SaldaePackages/SaldaeTidyModels/R/SA_ml_app_creation.R')
# source('~/Saldae_Analytics/SaldaePackages/SaldaeTidyModels/R/saldae_features.R')

library("dplyr")
library("ranger")
library("kernlab")
# library("glm")
library("workflows")

# Define UI for application that draws a histogram
app_ui <-  tagList(
  shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(title = span("my App ML title ",
                                                 style = "color: black; font-weight: bold ;font-size: 16px")),
    shinydashboard::dashboardSidebar(disable = FALSE,
                                     shinydashboard::sidebarMenu(
                                       shinydashboard::menuItem("my ML APP", tabName = "SA_ML_app", icon = icon("table"))
                                     )),
    shinydashboard::dashboardBody(
      shinydashboard::tabItems(

        shinydashboard::tabItem("SA_ML_app",
                                shinydashboard::tabBox(width = 12, title = "my app title",

                                                       tabPanel(title = "Features Engineering",
                                                                uiOutput("ml_trained_model_file"),
                                                                SA_ML_APP_UI("SA_ML_App")

                                                       )
                                )

        )
      )
    )
  )
)


# Define server logic required to draw a histogram
app_server <- function(input, output,session) {

  ML_available_model <- reactive({
    available_ML_models <- list.files(path = "./ML_models",pattern = ".rds")
  })

  #----------- select ML type
  output$ml_trained_model_file <- renderUI({
    ml_types <-list.files("../ML_models/",pattern = ".rds",full.names = FALSE)
    ml_types <- gsub("SA_ML_model_|.rds","",ml_types)
    shinyWidgets::pickerInput(inputId = "ml_trained_model_file",
                              label = "Select ML model:",
                              multiple = FALSE,
                              choices = ml_types
    )
  })


  ML_trained_results <- reactive({
    req(input$ml_trained_model_file)
    ml_trained_model_file <- paste0("../ML_models/SA_ML_model_",input$ml_trained_model_file,".rds")
    readRDS(ml_trained_model_file)
  })


  callModule(module = SA_ML_APP_server, id = "SA_ML_App", tisefka = reactive({ ML_trained_results()}),create_flag = FALSE)

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


shinyApp(ui = app_ui, server = app_server)
