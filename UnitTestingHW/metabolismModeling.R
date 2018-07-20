# script to run model to estimate stream metabolism based on stream discharge, dissolved oxygen, dissolved oxygen saturation, and temperature data.
#
# Arguments
# metab.data       dataframe of stream metabolism time series parameters, 
#                 assembled by dataLoad.R and reformatted by dataPrep.R
# filename
# pool_K600
# err_obs_iid
# err_proc_acor
# err_proc_iid
# ode_method
# deficit_src
#
# Outputs
#
#
# Dependencies
# library(streamMetabolizer)

metabolismModeling <- function(metab.data, filename, pool_K600 = "binned", 
                               err_obs_iid = TRUE,
                               err_proc_acor = FALSE, err_proc_iid = TRUE, 
                               ode_method = "trapezoid", deficit_src = "DO_mod"){
  #
  # check if all model variables are actually in the dataframe or not, return error if false
  #
  # check if .Rdata file of model results already exists, if it does, ask user if they want to load file from memory instead of running model
  if(file.exists(paste0("./output_model/", filename))){
    #menu(c("Yes", "No"), title = "Load model results from file? If no, function will re-run metabolism model instead.")
    output <- read.csv(paste0("./output_model/", filename), header = TRUE)
    return(output)
    break
  }
  #
  modelname <- streamMetabolizer::mm_name(type = "bayes",
                                          err_obs_iid = err_obs_iid, 
                                          err_proc_acor = err_proc_acor,
                                          err_proc_iid = err_proc_iid, 
                                          ode_method = ode_method, 
                                          deficit_src = deficit_src)
  modelspecs <- streamMetabolizer::specs(model_name = modelname, 
                                         burnin_steps = 500, saved_steps = 500)
  cat("Using a maximum likelihood estimation (MLE) method to fit your metabolism model.\n")
  # run the model
  modelfit <- streamMetabolizer::metab(modelspecs, data = metab.data)
  # extract predictions
  predictions <- streamMetabolizer::predict_metab(modelfit)
  # return model results and save results locally
  output <- list(predictions = predictions, fit = modelfit)
  write.csv(output$predictions, file = paste0("./output_model/", filename, ".csv"),
            row.names = FALSE)
  #
  return(output$predictions)
}