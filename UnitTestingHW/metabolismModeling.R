# script to run model to estimate stream metabolism based on stream discharge, dissolved oxygen, dissolved oxygen saturation, and temperature data.
#
# Arguments
# data       dataframe of stream metabolism time series parameters, 
#            assembled by dataLoad.R and reformatted by dataPrep.R
#
# Outputs
#
#
# Dependencies
# library(streamMetabolizer)

metabolismModeling <- function(data){
  # insert check if all model variables are actually in the dataframe or not, return error if false
  #
  modelname <- streamMetabolizer::mm_name(type='mle', ode_method = "trapezoid")
  modelspecs <- streamMetabolizer::specs(modelname)
  cat("Using a maximum likelihood estimation (MLE) method to fit your metabolism model.\n")
  # run the model
  modelfit <- streamMetabolizer::metab(modelspecs, data = data)
  # extract predictions
  predictions <- streamMetabolizer::predict_metab(modelfit)
  # return model results
  output <- list(predictions = predictions, fit = modelfit)
  return(output)
}