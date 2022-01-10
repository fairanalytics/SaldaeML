# Building a Prod-Ready, Robust Shiny Application.
# 
# README: each step of the dev files is optional, and you don't have to 
# fill every dev scripts before getting started. 
# 01_start.R should be filled at start. 
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
# 
# 
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Add one line by package you want to add as dependency
usethis::use_package( "shiny" )
usethis::use_package( "dplyr" )
usethis::use_package( "shinydashboard" )
usethis::use_package( "shinydashboardPlus" )
usethis::use_package(  "shinybusy" )
usethis::use_package( "workflows" )
usethis::use_package( "recipes" )
usethis::use_package( "rsample" )
usethis::use_package( "yardstick" )
usethis::use_package( "dlookr" )
usethis::use_package( "parsnip" )
usethis::use_package( "ranger" )
usethis::use_package( "kernlab" )
usethis::use_package( "zoo" )

usethis::use_package( "purrr" )
usethis::use_package( "readr" )


usethis::use_package( "DT" )
usethis::use_package( "htmlwidgets" )
usethis::use_package( "plotly" )
usethis::use_package( "shiny.info" )


usethis::use_package( "janitor" )
usethis::use_package( "rstudioapi" )
usethis::use_package( "shinyWidgets" )
usethis::use_package( "shinyhelper" )
usethis::use_package( "jsonlite" )
usethis::use_package( "lubridate" )
usethis::use_package( "rhandsontable" )
usethis::use_package( "readxl" )
usethis::use_package( "timetk" )

usethis::use_pipe(export = TRUE)

## Add modules ----
## Create a module infrastructure in R/
golem::add_module( name = "name_of_module1" ) # Name of the module
golem::add_module( name = "name_of_module2" ) # Name of the module

## Add helper functions ----
## Creates ftc_* and utils_*
golem::add_fct( "helpers" ) 
golem::add_utils( "helpers" )

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file( "script" )
golem::add_js_handler( "handlers" )
golem::add_css_file( "custom" )

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw( name = "my_dataset", open = FALSE ) 

## Tests ----
## Add one line by test you want to create
usethis::use_test( "app" )

# Documentation

## Vignette ----
usethis::use_vignette("SaldAutoML")
devtools::build_vignettes()

## Code coverage ----
## (You'll need GitHub there)
usethis::use_github()
usethis::use_travis()
usethis::use_appveyor()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")

