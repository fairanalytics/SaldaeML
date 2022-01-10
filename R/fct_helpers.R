#' initilaize helper module
#' @description it activate all the necessary element to help the user to understand the app components
#' @author Farid Azouaou
#' @param target_folder the folder to use as a helpers 
#' @export

initialize_helper <- function(target_folder = "helpers"){
  helper_source  <- system.file(target_folder, package = "SaldAutoML")
  file.copy(from = helper_source,to = "./",overwrite = TRUE, recursive = TRUE)
  shinyhelper::observe_helpers(help_dir = target_folder)
}
