
#install.packages("glmnet")
# library(tidymodels)  # for the parsnip package, along with the rest of tidymodels
# # Helper packages
# library(readr)       # for importing data

#' Saldae Performance evaluation function
#' @description t.b.d
#' @author Farid Azouaou
#' @param  SA_test_predicted data frame containg predicted values and benchmark values
#' @param  pred_mode prediction mode (classification or regression)
#' @return  a daa frame containing performances results (AUC , RMSE , MSE , MAE , MAPE)

SA_ml_performance_f <- function(SA_test_predicted = NULL,pred_mode = "classification",target_variable = NULL){

  
  if(pred_mode == "classification"){
    SA_ml_perf <- list()
      # yardstick::roc_auc(data = SA_test_predicted,truth = test_label, test_pred,estimator ="hand_till")
      # pred <- ROCR::prediction(SA_test_predicted$test_label,SA_test_predicted$test_label)
    # perf <- performance(SA_test_predicted,"","test_pred")
    
    SA_ml_perf[["accuracy"]] <- SA_test_predicted%>% yardstick::accuracy(test_label, test_pred)
    
   
    temp_vect <-  SA_test_predicted %>% yardstick::conf_mat(truth = test_label, test_pred)
    
    temp_vect <- data.frame(temp_vect$table)%>%dplyr::mutate(perf = Prediction == Truth)
    SA_ml_perf[["confusion_matrix"]]  <- temp_vect
    SA_ml_perf[["perf_plot"]]      <- temp_vect%>%plotly::plot_ly(x = ~Truth ,y = ~Freq ,color =  ~perf, type="bar")%>%
      plotly::config(displaylogo = F)
  }
  if(pred_mode == "regression"){
    SA_ml_perf <- list()
    SA_ml_perf[["accuracy"]] <- SA_test_predicted %>%
      yardstick::metrics(truth = test_label, estimate = test_pred)%>%dplyr::mutate(.estimate = round(.estimate,4))
   
     SA_ml_perf[["perf_plot"]] <- plot_test_predictions(test_results = SA_test_predicted, target_variable = target_variable )
     SA_ml_perf[["delta_plot"]] <- plot_pred_delta(test_results = SA_test_predicted, target_variable =target_variable )  

  }
  return(SA_ml_perf)
}


#' Saldae ML model builder
#' @description t.b.d
#' @author Farid Azouaou
#' @param tisekfa prepared data for ML training
#' @param train_prop training proportion
#' @param strat_class stratification class
#' @return trained model
#' @export


SA_tisefka_bdu  <- function(tisefka = NULL , train_prop = 0.8,strat_class = NULL, rand_sample = NULL ){
  
  if(rand_sample == TRUE){
    # Fix the random numbers by setting the seed
    # This enables the analysis to be reproducible when random numbers are used
    set.seed(555)
    # Put 3/4 of the data into the training set
    tisefka_split <- rsample::initial_split(tisefka, prop = train_prop,strata = all_of(strat_class))
    
    # Create data frames for the two sets:
    train_tisefka <- rsample::training(tisefka_split)
    test_tisefka  <- rsample::testing(tisefka_split)
  }else{
    
    train_prop <- floor(train_prop * nrow(tisefka))
    train_tisefka <- tisefka%>%head(train_prop)
    test_tisefka <- tisefka%>%tail(-train_prop)
    
  }
  
  output_tisefka <- list()
  output_tisefka[["train_tisefka"]] <- train_tisefka
  output_tisefka[["test_tisefka"]] <- test_tisefka
  output_tisefka[["train_prop"]] <- train_prop
  return(output_tisefka)
}
SA_formula_generator <- function(target_variable = NULL , explaining_variable = NULL){
  # This creates the appropriate string:
  ml_formula <- as.formula(paste(target_variable, paste(explaining_variable, collapse=" + "), sep=" ~ "))
  return(ml_formula)
}


SA_ml_engine_lm1 <- function(tisefka = tisefka, target_variable = NULL, ml_algo= "linear regression",explaining_variable = NULL ,train_prop=0.75,sa_scale = FALSE,pred_mode = "regression", rand_samp = TRUE){
  tisefka <- tisefka%>%dplyr::mutate_if(is.character, as.factor)
  tune_parameters<- FALSE
  #' prepare data : split
  if(ml_algo %in% c("svm")){
    var_classes <- dlookr::get_class(tisefka)%>%mutate(is_numeric = class=="numeric")
    tisefka <- tisefka%>%dplyr::select_if(var_classes$is_numeric)
    explaining_variable <- explaining_variable[sapply(explaining_variable,function(x)x%in%colnames(tisefka))]
  }
  
  # convert target variable into factor
  if(pred_mode == "classification"){
    tisefka <- tisefka%>%dplyr::mutate_at(target_variable , as.factor)
  }
  
  
# split dataset 
  tisefka_iheggan  <- SA_tisefka_bdu(tisefka  = tisefka , train_prop = train_prop , strat_class = NULL, rand_samp = rand_samp)
  

  
  #' initializing the models
  if(ml_algo == "linear regression"){
    SA_lm_mod <-
      parsnip::linear_reg() %>%
      parsnip::set_engine("lm")
  }else if(ml_algo == "random forest"){
    SA_lm_mod <-
      parsnip::rand_forest(trees = 1000) %>%
      parsnip::set_engine("ranger") %>%
      parsnip::set_mode(pred_mode)
  }else if(ml_algo == "svm"){
    SA_lm_mod <-
      parsnip::svm_rbf(cost = 0.05) %>%
      parsnip::set_engine("kernlab") %>%
      parsnip::set_mode(pred_mode)
  }else if(ml_algo == "gradient boosting"){
    SA_lm_mod <-
      parsnip::boost_tree(trees = 2000)%>%
      parsnip::set_engine("xgboost", objective = "reg:squarederror") %>%
      parsnip::set_mode(pred_mode)
  }else if(ml_algo == "decision tree"){
    
    tune_parameters<- TRUE
    
    SA_lm_mod <-
      parsnip::decision_tree(cost_complexity = tune::tune(),
                             tree_depth = tune::tune())%>%
      parsnip::set_engine("rpart") %>%
      parsnip::set_mode(pred_mode)
    
    model_grid_search <- dials::grid_regular(dials::cost_complexity(),
                                     dials::tree_depth(),
                              levels = 5)
    SA_ML_folds <- rsample::vfold_cv(tisefka_iheggan$train_tisefka)
  }

  #' prepare formula
  #'
  ml_formula <- SA_formula_generator(target_variable = target_variable , explaining_variable = explaining_variable)
  #' prepare a recipe

  tisefka_rec <-    recipes::recipe(ml_formula, data = tisefka_iheggan$train_tisefka)

  if(sa_scale == TRUE){
    tisefka_rec <- tisefka_rec%>%
      # recipes::step_other(Neighborhood)%>%
      recipes::step_dummy(recipes::all_nominal()) %>%
      recipes::step_center(recipes::all_predictors()) %>%
      recipes::step_scale(recipes::all_predictors())
  }
  # tisefka_cls <- sapply(tisefka_iheggan$train_tisefka, class)
  # if("Date"%in%tisefka_cls){
  #   tisefka_iheggan$train_tisefka <- tisefka_iheggan$train_tisefka%>%
  #   recipes::step_holiday(date, holidays = timeDate::listHolidays("US")) %>%
  #     recipes::step_rm(date)
  # }
  # %>%
  #   update_role(flight, time_hour, new_role = "ID") %>%
  #   step_date(date, features = c("dow", "month")) %>%
  #   step_holiday(date, holidays = timeDate::listHolidays("US")) %>%
  #   step_rm(date) %>%
  #   step_dummy(all_nominal(), -all_outcomes()) %>%
  #   step_zv(all_predictors())

  #' embed the recipe to the engine
  SA_ml_wflow <-
    workflows::workflow() %>%
    workflows::add_model(SA_lm_mod) %>%
    workflows::add_recipe(tisefka_rec)

  if(tune_parameters == TRUE){
    
    #' fit the model
    SA_lm_fit <-
      SA_ml_wflow %>%
      tune::tune_grid(
        resamples = SA_ML_folds,
        grid = model_grid_search
      )
    
    CV_results <- SA_lm_fit %>% 
      tune::collect_metrics()
    
    if(pred_mode=="regression"){
      SA_lm_best <- SA_lm_fit %>%
        tune::select_best(metric = "rmse")  
    }else if(pred_mode=="classification"){
      SA_lm_best <- SA_lm_fit %>%
        tune::select_best(metric = "roc_auc")  
    }
    SA_ml_wflow <- 
      SA_ml_wflow %>% 
      tune::finalize_workflow(SA_lm_best)
  }
  
  SA_lm_fit <- 
    SA_ml_wflow %>%
    parsnip::fit(data = tisefka_iheggan$train_tisefka) 

  #' predict for test set 
  SA_test_predicted <- predict(SA_lm_fit, new_data = tisefka_iheggan$test_tisefka) %>%
    dplyr::bind_cols(tisefka_iheggan$test_tisefka %>% dplyr::select(!!target_variable))
  
  
  colnames(SA_test_predicted) <- c("test_pred","test_label")


 if(pred_mode == "regression"){
   
   prediction_digits <- SA_test_predicted%>%dplyr::pull(test_label)%>%head(300)
   
   prediction_digits <- strsplit(paste(prediction_digits),split = "\\.")
   prediction_digits <-nchar(na.omit(sapply(prediction_digits, function(x)unlist(x)[2])))
   if(length(prediction_digits)>0){
     prediction_digits <- max(prediction_digits)
     SA_test_predicted <- SA_test_predicted%>%dplyr::mutate(test_pred = round(test_pred,prediction_digits))
   }
   SA_test_predicted <- SA_test_predicted%>%
     dplyr::mutate(delta = test_label - test_pred,rel_delta = (test_label - test_pred)/test_label)%>%
     dplyr::mutate(rel_delta = round(rel_delta* 100,2)) 
 }
  SA_test_predicted2<<-SA_test_predicted
  if(pred_mode == "classification"){
    
    target_levels <- levels(tisefka_iheggan$train_tisefka%>%pull(!!target_variable))
    SA_test_predicted$test_pred <- factor(SA_test_predicted$test_pred, levels = target_levels)
  }
  
  #'
  #' model performances evaluation
  #'

  ml_output <- list()

  ml_output[["target_variable"]] <- target_variable
  ml_output[["SA_lm_fit"]] <- SA_lm_fit
  ml_output[["tisefka_iheggan"]]   <- tisefka_iheggan
  ml_output[["test_predictions"]]  <- SA_test_predicted
  ml_output[["test_performances"]] <- SA_ml_performance_f(SA_test_predicted, pred_mode = pred_mode,target_variable = target_variable)
  return(ml_output)
}

#  tisefka <- economics
# #
# #
#  explaining_variable  <- c(  "pce"   ,   "pop"    ,  "psavert" , "uempmed")
# #
#  target_variable  <- "unemploy"
#
#  aa<- SA_ml_engine_lm1(tisefka = tisefka,target_variable = target_variable,ml_algo = "random forest",
#                        explaining_variable = explaining_variable, pred_mode ="regression")
