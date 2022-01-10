#' Read data from inputfile (CSV)
#' @author Farid Azouaou
#' @param input_file input file
#' @return tbl_df object

ghred_tisefka_csv <- function(input_file = NULL) {
  tisefka_csv <- readr::read_csv(
    file = input_file$datapath,
    # header = TRUE,
    # delim = ",",
    na = c("", "NA","ND", " ", "-", ".", "#")
  )
  tisefka_csv <- data.frame(tisefka_csv, check.names = FALSE)
  return(tisefka_csv)
}


#' Read data from inputfile (Excel)
#' @author Farid Azouaou
#' @param input_file inputfile object
#' @param tawriqt excel sheet name
#' @return tbl_df object

ghred_tisefka_excel <- function(input_file = NULL, tawriqt = NULL) {
  tisefka_excel <- readxl::read_excel(
    path = input_file$datapath,
    # header = TRUE,
    # delim = ",",
    sheet = tawriqt,
    na = c("", "NA","ND", " ", "-", ".", "#")
  )
  tisefka_excel <- data.frame(tisefka_excel, check.names = FALSE)
  return(tisefka_excel)
}

#' Read data main function
#' @author Farid Azouaou
#' @param input_file input file
#' @param tala data source (CSV/Excel/Database)
#' @param tawriqt excel sheet name
#' @return tbl_df object
#' @export

ghred_tisefka_aqerru <- function(input_file = NULL, tala = NULL, tawriqt = NULL) {
  if (tala == "csv") {
    tisefka <- ghred_tisefka_csv(input_file = input_file)
  }
  if (tala == "xlsx") {
    tisefka <- ghred_tisefka_excel(input_file = input_file, tawriqt = tawriqt)
  }
  tisefka <- tisefka%>%janitor::remove_empty(which = c("cols"))%>%
              janitor::remove_constant()
  return(tisefka)
}


#-------------------
# create html file. file name is EDA_Report.htmleda_report(carseats, "US", output_format = "html")
#' Saldae : data diagnosis
#' @description draw a diagnosis on data  from quality and statitistics perspectives.
#' @author Farid Azouaou
#' @param tisefka raw data
#' @param categoricals_ukud time_based gruping variables
#' @param ukud_unit time unit
#' @return list containing detailed diagnos (quality , comleteness outliers,...)
#' @export

data_diagnosis_f <- function(tisefka = NULL, categoricals_ukud = NULL, ukud_unit = NULL) {
  output_data_quality <- list()
  
  output_data_quality[["diagnosis"]]    <- dlookr::diagnose(.data = tisefka)
  output_data_quality[["beschreibung"]] <- dlookr::describe(.data = tisefka)
  output_data_quality[["outliers"]]     <- dlookr::diagnose_outlier(.data = tisefka)
  
  # rownames(output_data_quality[["beschreibung"]]) <- rownames(output_data_quality[["outliers"]]) <- output_data_quality[["beschreibung"]]$variable
  
  #----------------------------- afe-d ukuden yeddan akk d ukud_unit
  categoricals <- output_data_quality$diagnosis[output_data_quality$diagnosis$unique_count < 8 & output_data_quality$diagnosis$unique_count > 1, "variables"]
  output_data_quality[["categoricals"]] <- categoricals$variables
  #-----------------------------------------------------------------
  return(output_data_quality)
}

#' Saldae Missing Values Imputation:
#' @description detect and impute missing values
#' @author Farid Azouaou
#' @param tisefka original data (Dataframe or tbl object)
#' @return cleaned data (imputed missing values)
#' @export

SA_tisefka_impute <- function(tisefka = NULL){
  target_var <- dlookr::find_na(tisefka)
  var_class <- dlookr::get_class(tisefka)$class
  
  if(length(target_var)>0){
    var_class  <- var_class[target_var]
    target_var <- colnames(tisefka)[target_var]
    target_var <- target_var[var_class!= "hms" | var_class=="Date" | var_class =="POSIXct"]
    
    tisefka_amputed <- target_var%>%purrr::map(~tisefka%>%dlookr::imputate_na(!!.x ,no_attrs=TRUE ,method = "rpart") )%>%
      stats::setNames(target_var)
    tisefka_amputed<- data.frame(tisefka_amputed,check.names = TRUE)
    tisefka[,target_var] <- tisefka_amputed 
  }
  tisefka <- dplyr::tbl_df(tisefka)
  return(tisefka)
  
}
