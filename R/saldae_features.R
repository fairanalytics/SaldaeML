# SA_holidays_fun <- function(tisefka = NULL,date_vec = NULL){
#   tisefka <- tisefka%>%dplyr::mutate(is_end_of_month = RQuantLib::isEndOfMonth(calendar = "UnitedStates",!!as.name(date_vec)),
#                                      isHoliday = RQuantLib::isHoliday(calendar = "UnitedStates",!!as.name(date_vec)),
#                                      isBusinessDay = RQuantLib::isEndOfMonth(calendar = "UnitedStates",!!as.name(date_vec)))
#   return(tisefka)
# }


if_is_date <- function(x = NULL){
  date_formats <- c("ymd","dmy","mdy","dy","md","dm")
  valid_format <- date_formats%>%purrr::map(~ !any(is.na(lubridate::parse_date_time(na.omit(x),.x))))
  return(date_formats[unlist(valid_format)])
}

if_is_time <- function(x = NULL){
   if(any(!grepl(":",x)))return(FALSE)
   if(max(nchar(x))>9)return(FALSE)
   return(all(!is.na(lubridate::hms(x))))
}
get_HMS_features <- function(x = NULL,ekkes_constant = TRUE){
  x <- data.frame(x)%>%
    tidyr::separate(x,c("H","M","S"),sep= ":",remove  = FALSE)%>%
    dplyr::as.tbl()
  if(ekkes_constant==TRUE)x <- janitor::remove_constant(x)
  return(x)
}
SA_ml_feature_generation_base <- function(x = NULL,ekkes_constant = TRUE){

  valid_date_format <- if_is_date(head(x,30))
  valid_time_var    <- if_is_time(head(x,30))
  
  if(length(valid_date_format)==1){
    x <- lubridate::parse_date_time(x,valid_date_format)
    x <- timetk::tk_get_timeseries_signature(x)%>%
      dplyr::select(-diff,-index.num,-dplyr::ends_with(".iso"), -dplyr::ends_with(".xts"),-dplyr::ends_with(".lbl"))
      if(ekkes_constant==TRUE)x <- janitor::remove_constant(x)
  }else if(valid_time_var == TRUE){
    x <- get_HMS_features(x = x)
  }else if(is.numeric(x)){
    x <- dplyr::as.tbl(data.frame(x))
    x <- x%>%dplyr::mutate(x_abs = abs(x),x_p2 = x**2)
    if(all(x>=0))x <- x%>%dplyr::mutate(x_sqrt = sqrt(x))
    if(all(x>0))x <- x%>%dplyr::mutate(x_log = log(x))
  }else{
    x <- data.frame(x,check.names = FALSE)%>%dplyr::as.tbl()
  }
  return(x)
}

SA_ml_feature_generation <- function(tisefka= NULL ){
  #
  tisefka_diag <- dlookr::diagnose(.data = tisefka)
  # tisefka <- data.frame(tisefka%>%purrr::map_if(is.numeric,~scale(.x)),check.names = FALSE)

  #
  tisefka_taghezfant <- tisefka%>%purrr::map(~SA_ml_feature_generation_base(.x,ekkes_constant = FALSE))
  #
  tisefka_taghezfant <- names(tisefka_taghezfant)%>%purrr::map( ~stats::setNames(tisefka_taghezfant[[.x]],paste(.x,colnames(tisefka_taghezfant[[.x]]),sep="_")))%>%stats::setNames(names(tisefka_taghezfant))

  #
  tisefka_taghezfant <- names(tisefka_taghezfant)%>%purrr::map( ~stats::setNames(tisefka_taghezfant[[.x]],gsub("_x","",colnames(tisefka_taghezfant[[.x]]))))%>%stats::setNames(names(tisefka_taghezfant))
  #
  return(tisefka_taghezfant)
}

sekned_tisefka_features <- function(tisefka_features = NULL){
  my_var <- colnames(tisefka_features)[1]
  x <- list(title =  my_var)
  feat_plot <- tisefka_features%>%plotly::plot_ly( x = ~base::get(my_var),type = "histogram")%>%
    plotly::layout(xaxis = x)%>%plotly::config(displaylogo = F)
  return(feat_plot)
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

SA_ML_features_UI <- function(id,mod_title = NULL ,div_width = "col-xs-12 col-sm-6 col-md-8") {
  ns <- NS(id)
  fluidPage(
    uiOutput(ns("features_engineering_box")),
    uiOutput(ns("graphs_ui"))
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

SA_ML_features_server <- function(input, output, session,tisefka,div_width = "col-xs-6 col-sm-12 col-md-6") {

  tisefka_tizegzawin <- reactive({
    tisefka()
  })

  output$features_engineering_box <- renderUI({
    req(tisefka_tizegzawin())
    shinydashboard::box(title = "Features engineering",collapsible = TRUE,
                        status = "success",width = 12,
                        #-----HEADER CONTENT
                        fluidRow(

                          column(width = 2,
                                 uiOutput(session$ns("target_variable"))
                          ),
                          column(width = 2,
                                 uiOutput(session$ns("select_element"))
                          ),
                          column(width = 2,
                                 uiOutput(session$ns("generate_features"))
                          )
                        )
    )
  })

  #----- generate feature trigger
  output$generate_features <- renderUI({
    req(tisefka_tizegzawin())
    shinyWidgets::actionBttn(
      inputId = session$ns("generate_features"),
      style = "material-flat",
      color = "success",
      label = "Generate Features")%>%shinyhelper::helper(type = "markdown",buttonLabel="Got it",
                                                         # icon= shiny::icon("fa-lightbulb"),
                                                         colour = "green",
                                                         content = "sald_forecast")
  })

#----------- select target variable
output$target_variable <- renderUI({
    req(tisefka_tizegzawin())
    shinyWidgets::pickerInput(inputId = session$ns("target_variable"),
                              label = "Select target variable:",
                              multiple = FALSE,
                              choices = colnames(tisefka_tizegzawin())
    )
  })
  #----------- select features
  output$select_element <- renderUI({
    req(tisefka_tizegzawin())
    req(input$target_variable)
    var_choices <- colnames(tisefka_tizegzawin())
    var_choices <- var_choices[var_choices != input$target_variable]
    shinyWidgets::pickerInput(inputId = session$ns("select_element"),
                              label = "Select Explaining Variables:",
                              multiple = TRUE,
                              choices = var_choices
    )
  })
  #----------------
  tisefka_with_features <- eventReactive(input$generate_features,{
    req(tisefka_tizegzawin())
    req(input$select_element)
    tisefka_features <- SA_ml_feature_generation(tisefka = tisefka_tizegzawin()[,input$select_element])
    return(tisefka_features)
  })


  tisefka_feature_plots <- reactive({
    req(tisefka_with_features())
     names(tisefka_with_features())%>%purrr::map(~sekned_tisefka_features(tisefka_with_features()[[.x]]))%>%
      stats::setNames(names(tisefka_with_features()))
  })

  tisefka_feature_tables <- reactive({
    req(tisefka_with_features())
    return(purrr::map(.x =tisefka_with_features(),~DT::datatable(.x,extensions = 'Scroller', options = list(deferRender = TRUE, scrollY = 200, scroller = TRUE)) )%>%
             stats::setNames(names(tisefka_with_features())))
  })
  #---------------------

  observeEvent(eventExpr= tisefka_feature_tables(),handlerExpr= {
    purrr::map(names(tisefka_feature_plots()), ~{
      output_name_plot <- paste0("tisefka_plot_", .x)
      output_name_table <- paste0("tisefka_table_", .x)
      output_name_features <- paste0("tisefka_features_", .x)
      output[[output_name_table]] <- DT::renderDataTable(tisefka_feature_tables()[[.x]])
      output[[output_name_plot]] <- plotly::renderPlotly(tisefka_feature_plots()[[.x]])

      #
      my_features <- colnames(tisefka_with_features()[[.x]])
      output[[output_name_features]] <- renderUI({
        shinyWidgets::pickerInput(inputId = session$ns(output_name_features),
                                  label = "Select features:",
                                  multiple = TRUE,
                                  selected = my_features,
                                  choices  = my_features)
      })
      #
    })
  })

  output$graphs_ui <- renderUI({
    req(tisefka_feature_plots())
    plots_list <- purrr::imap(tisefka_feature_plots(), ~{
      tagList(
        div(class = div_width,
            shinydashboard::tabBox(width = 12, title = .y,
                                   tabPanel(icon("bar-chart"),
                                            fluidPage(
                                              uiOutput(session$ns(paste0("tisefka_features_",.y))),
                                              plotly::plotlyOutput(session$ns(paste0("tisefka_plot_",.y)), height = "300px")
                                            )
                                   ),tabPanel(icon("table"),
                                              DT::dataTableOutput(session$ns(paste0("tisefka_table_",.y)))
                                   )

            )
        ),
        br()
      )
    })
    tagList(plots_list)
  })



  analytics_output <- reactive({
    req(tisefka_feature_plots())
    req(tisefka_with_features())
    selected_features <- purrr::map(names(tisefka_feature_plots()),~input[[paste0("tisefka_features_", .x)]])%>%stats::setNames(names(tisefka_feature_plots()))

    selected_features <- names(selected_features)%>%purrr::map(~tisefka_with_features()[[.x]][,selected_features[[.x]]])

    selected_features[[input$target_variable]] <- tisefka_tizegzawin()[,input$target_variable]
    output <- list()
    output$tisefka_feature_tables         <-  tisefka_with_features()
    output$selected_data                  <-  do.call(cbind, selected_features)%>%dplyr::as.tbl()
    output$target_variable                <-  input$target_variable

    output$explaining_variables           <-  input$select_element
    
    

    var_class <- output$tisefka_feature_tables%>%purrr::map(~dlookr::get_class(.x)$class[1])
    var_class   <-  unlist(var_class)
    
    names(output$explaining_variables)  <- var_class
    
    
    if("factor" %in% var_class){
      ordinal_vars <- output$explaining_variables[var_class=="factor"]
      names(ordinal_vars)<- NULL
      output$var_factors  <-  tisefka_tizegzawin()%>%dplyr::select(!!ordinal_vars)%>%purrr::map(~unique(.x))
    }
    #-------------------
    return(output)
  })
}
