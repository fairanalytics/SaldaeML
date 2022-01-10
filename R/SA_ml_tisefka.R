plot_test_predictions<- function(test_results = NULL , target_variable = NULL){
  test_id <- 1:nrow(test_results)
  y <- list(title = target_variable)
  test_results <- test_results%>%dplyr::mutate(test_id = test_id)
  test_pred_chart <- test_results %>% plotly::plot_ly(x = ~test_id, y = ~test_label) %>%
    plotly::add_lines(name = "Benchmark",color = I("#666666"))%>%
    plotly::add_lines(y = ~test_pred,name ="Prediction",color = I("#1B9E77"))%>%
    plotly::add_bars(y = ~delta, name = "delta",color = I("#A6761D"))%>%
    plotly::layout(legend = list(orientation = "h", x = 0.35, y = 100),yaxis = y)%>%plotly::config(displaylogo = F)
  return(test_pred_chart)
}

plot_pred_delta <- function(test_results = NULL , target_variable = NULL){
  x <- list(title = target_variable)
  pred_density <- stats::density(dplyr::pull(test_results,delta))

  plotly::plot_ly(x = ~pred_density$x, y = ~pred_density$y, type = 'scatter', mode = 'lines', fill = 'tozeroy',color = I("#A6761D"))%>%
    plotly::layout(xaxis = x,yaxis=list(title = "error distribution"))%>%plotly::config(displaylogo = F)
}

#------------------------ multiple-select, multiple output

#' Saldae Dashboard Module UI (analytics)
#' @description Saldae Dashboard module UI : forecasting
#' @author Farid Azouaou
#' @param id  server module ID
#' @param div_width dimension information about the framework(html object)
#' @param mod_title module title (default NULL)
#' @return UI module
#' @export

SA_ML_preprocessing_UI <- function(id,mod_title = NULL ,div_width = "col-xs-12 col-sm-6 col-md-8") {
  ns <- NS(id)
  fluidPage(
    uiOutput(ns("ML_preprocessing")),
    shinydashboard::tabBox(width = 12, title = "Test Prediction results",
                           tabPanel(icon("bar-chart"),
                                    SA_tisefka_overview_UI(ns("perf_view2")),
                                    fluidRow(
                                      column(width = 9,plotly::plotlyOutput(ns("test_pred_chart"))),
                                      column(width = 3, plotly::plotlyOutput(ns("test_pred_chart_delta")))
                                    )



                           ),
                           tabPanel(icon("table"),
                                      dataTableOutput(ns("test_pred_DT"))
                           )

    )
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

SA_ML_preprocessing_server <- function(input, output, session,tisefka,div_width = "col-xs-6 col-sm-12 col-md-6") {

  tisefka_final <- reactive({
    req(tisefka())
    tisefka()$selected_data
  })
  target_variable <- reactive({
    req(tisefka())
    tisefka()$target_variable
  })

  output$ML_preprocessing <- renderUI({
    shinydashboard::box(title = "Preprocessing Board",collapsible = TRUE,
                        status = "success",width = 12,
                        #-----HEADER CONTENT
                        fluidRow(

                          column(width = 2,
                                 uiOutput(session$ns("tisefka_train_split"))
                          ),
                          column(width = 1,
                                 uiOutput(session$ns("tisefka_rand_sampling"))
                          ),
                          column(width = 2,
                                 uiOutput(session$ns("ml_type"))
                          ),

                          column(width = 2,
                                 uiOutput(session$ns("ml_algorithm"))
                          ),
                          column(width = 2,
                                 uiOutput(session$ns("train_ml_model"))
                          )
                        )
    )
  })

  #----------- select algorithm
  output$ml_algorithm <- renderUI({
    req(tisefka_final())
    algorithm_choices <- c("Linear Regression","Decision Tree","SVM","Random Forest", "Gradient Boosting")
    shinyWidgets::pickerInput(inputId = session$ns("ml_algorithm"),
                              label = "Select ML Algorithm",
                              multiple = FALSE,
                              choices = algorithm_choices
    )%>%shinyhelper::helper(type = "markdown",buttonLabel="Got it",
                            icon = shiny::icon("fas fa-lightbulb"),
                            colour = "brown",
                            content = "ml_algorithms")
  })
  #----------- select ML type
  output$ml_type <- renderUI({
    req(tisefka_final())
    ml_types <-c("Regression","Classification","Clustering")
    shinyWidgets::pickerInput(inputId = session$ns("ml_type"),
                              label = "Select ML type:",
                              multiple = FALSE,
                              choices = ml_types
    )%>%shinyhelper::helper(type = "markdown",buttonLabel="Got it",
                            icon = shiny::icon("fas fa-lightbulb"),
                            colour = "brown",
                            content = "ml_types")
  })


 # Train/Test split 
 output$tisefka_train_split <- renderUI({
    req(tisefka_final())
     sliderInput(inputId = session$ns("tisefka_train_split"),label =  "Training Set split in %:",
                 min = 40, max = 100, value = 75)
  })
 
# split with or withou resampling
 
output$tisefka_rand_sampling <- renderUI({
  req(tisefka_final())
  checkboxInput(inputId = session$ns("tisefka_rand_sampling"), label = "Random Sampling")
})
# train model trigger
  output$train_ml_model <- renderUI({
    req(tisefka_final())
    shinyWidgets::actionBttn(
      inputId = session$ns("train_ml_model"),
      style = "material-flat",
      color = "success",
      label = "Train ML model")
  })


  ML_trained_model <- eventReactive(input$train_ml_model,{
    req(input$ml_type)
    req(tisefka_final())
    req(target_variable())
    print("building model")



    explaining_variable <- colnames(tisefka_final())
    explaining_variable <- explaining_variable[explaining_variable != target_variable()]

    ML_trained_model <- SA_ml_engine_lm1(tisefka = tisefka_final(),target_variable = target_variable(),train_prop= input$tisefka_train_split/100,rand_samp = input$tisefka_rand_sampling,
                                         explaining_variable = explaining_variable,ml_algo = tolower(input$ml_algorithm) ,pred_mode =tolower(input$ml_type))
    #ML_trained_model2<<- ML_trained_model
    #ML_trained_model$ml_report <- SA_ml_report_main(ML_trained_model2$SA_lm_fit$fit$fit)
    ML_trained_model$explaining_variables <- tisefka()$explaining_variables
    ML_trained_model$var_factors          <- tisefka()$var_factors
    return(ML_trained_model)
  })
  output$test_pred_DT <- renderDataTable({
    req(ML_trained_model())
    ML_trained_model()$test_predictions
  })

  output$test_pred_chart <- plotly::renderPlotly({
    req(ML_trained_model())
    ML_trained_model()$test_performances[["perf_plot"]]
  })
  
  output$test_pred_chart_delta <- plotly::renderPlotly({
    req(ML_trained_model())
    ML_trained_model()$test_performances[["delta_plot"]]
  })

  callModule(module = SA_tisefka_overview_server, id = "perf_view2" , reactive({ML_trained_model()$test_performances[["accuracy"]]}))


  ml_model_output <- reactive({
    req(ML_trained_model())
    return(ML_trained_model())
  })

}
