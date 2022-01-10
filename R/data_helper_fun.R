#' Reactive Data Exploration including statistics data quality and graphic output.
#' @param tisefka Raw data(row names as date)
#' @author Farid Azouaou
#' @param numeric_variables the numerical variables
#' @param tisefka_report Data report including outliers , statistics and quality information
#' @return  datahandontable object including start end date , statistics outliers information and charts(boxplot,distribution and raw plot)
#' @export

data_exploration_view <- function(tisefka = NULL, tisefka_report = NULL, numeric_variables = NULL) {

  tisefka <- tisefka[, numeric_variables, drop = F]

  tisefka_density <- apply(tisefka, 2, function(x) stats::density(na.omit(x))$y)
  relevant_variables <- c("variables", "outliers_cnt")
  DF <- tisefka_report$outliers[, relevant_variables]
  DF_stat <- tisefka_report$beschreibung[, c("n", "na", "mean", "sd")]
  DF <- DF%>%dplyr::bind_cols(DF_stat)

  # DF <- DF[numeric_variables, ]
  DF$Chart <- sapply(
    1:ncol(tisefka),
    function(x) {
      if(!is.numeric(tisefka_density[, x]))return(NULL)
      jsonlite::toJSON(list(
        values = tisefka%>%dplyr::pull(!!x),
        options = list(type = "line",col="green")
      ),na="null")
    }
  )

  DF$Density <- sapply(
    1:ncol(tisefka),
    function(x) {
      if(!is.numeric(tisefka_density[, x]))return(NULL)
      jsonlite::toJSON(list(
        values = tisefka_density[, x],
        options = list(type = "line")
      ))
    }
  )
  DF$Box <- sapply(
    1:ncol(tisefka),
    function(x) {
      if(!is.numeric(tisefka_density[, x]))return(NULL)
      jsonlite::toJSON(list(
        values = tisefka%>%dplyr::pull(!!x),
        options = list(type = "box")
      ),na="null")
    }
  )

  rh_plot <- rhandsontable::rhandsontable(DF, rowHeaders = NULL, width = 1000, height = 300) %>%
    rhandsontable::hot_col("Chart", renderer = htmlwidgets::JS("renderSparkline")) %>%
    rhandsontable::hot_col("Density", renderer = htmlwidgets::JS("renderSparkline")) %>%
    rhandsontable::hot_col("Box", renderer = htmlwidgets::JS("renderSparkline"))
   return(rh_plot)
}
