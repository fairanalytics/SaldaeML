
#' Saldae Dashboard Data Overview Server UI
#' @description Saldae Dashboard module UI :  value boxes  in a row containing
#' @author Farid Azouaou
#' @param id  server module ID
#' @return n  value boxes objects
#' @export

SA_tisefka_overview_UI <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("SA_tisefka_overview"))
  )

}

#' Saldae Dashboard Data Overview Server module
#' @description Saldae Dashboard module SERVER : render and generate 3 value boxes output
#' @param input  input shinydashboard elements containing information to use for output generation
#' @param output output shinydashboard element
#' @param session shiny session
#' @param tisefka reactive object containing  value data
#' @return output objects to be displayed in corresponding UI module
#' @export

SA_tisefka_overview_server <- function(input, output, session,tisefka) {

  output$SA_tisefka_overview <- renderUI({
    fluidRow(
      shinydashboard::infoBoxOutput(session$ns("SA_overview3")),
      shinydashboard::valueBoxOutput(session$ns("SA_overview1")),
      shinydashboard::infoBoxOutput(session$ns("SA_overview2"))
      

    )%>%shinyhelper::helper(type = "markdown",buttonLabel="Got it",
                            icon = shiny::icon("fas fa-lightbulb"),
                            colour = "brown",
                            content = "ml_metrics")
  })
  output$SA_overview1 <- shinydashboard::renderInfoBox({
    req(tisefka())
    library("apputils")
    val1 <- tisefka()[1,3]
    metric1 <- tisefka()[1,1]
    icon_rmse <- apputils::icon(list(src = apputils::statIcon("sd"), width = "80px"), lib = "local")
    apputils::infoBox(title = metric1,
                            value = val1,subtitle = "Model Performance",color = "olive",
                            icon = icon_rmse
    )
  })
  output$SA_overview2 <- shinydashboard::renderInfoBox({
    tisefka()
    val2 <- tisefka()[1,3]
    metric2 <- tisefka()[2,1]

    shinydashboard::infoBox(title = metric2,
                            value = val2,subtitle =  "Model Performance",color = "green",
                            shiny::icon("bar-chart")
    )
  })
  output$SA_overview3 <- shinydashboard::renderInfoBox({
    tisefka()
    val3 <- tisefka()[3,3]
    metric3 <- tisefka()[3,1]
    icon_mae <- apputils::icon(list(src = apputils::statIcon("mean"), width = "80px"), lib = "local")
    apputils::infoBox(title = metric3,
                            value = val3,subtitle = "Model Performance",color =I("#A6761D"),
                            icon = icon_mae
    )
  })
}
