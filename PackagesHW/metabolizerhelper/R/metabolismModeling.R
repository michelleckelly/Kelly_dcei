#' \code{metabolismModeling} model metabolism with streamMetabolizer package
#'
#' estimate stream metabolism based on stream discharge, dissolved oxygen, dissolved oxygen saturation, and temperature data, using an MCMC model.
#'
#' @param metab.data dataframe of stream metabolism time series parameters, assembled by \code{\link{dataLoad}} and reformatted by \code{\link{dataPrep}}
#' @param filename character vector. input of what local .csv of model output should be named, including filepath if needed
#' @param pool_K600 character vector, how should model pool information across multiple days to estimate K600? See \code{\link[streamMetabolizer]{mm_name}} for more info. Default is "binned"
#' @param err_obs_iid logical. should IID observation error in DO data be included in the model? Defaults to TRUE, which is the streamPULSE reccommendation. See \code{\link[streamMetabolizer]{mm_name}} for more info.
#' @param err_proc_acor logical. should autocorrelated process error be included? Defaults to FALSE, which is the streamPULSE reccommendation. See \code{\link[streamMetabolizer]{mm_name}} for more info.
#' @param err_proc_iid logical. should IID process error in DO data be included in the model? Defaults to TRUE, which is the streamPULSE reccommendation. See \code{\link[streamMetabolizer]{mm_name}} for more info.
#' @param ode_method character. method for solving the differential equation for DO. See \code{\link[streamMetabolizer]{mm_name}} for full list of options, defaults to "trapezoid"
#' @param deficit_src character vector. should the DO estimate be computed from the observed or modeled DO data? defaults to "DO_mod", which is the streamPULSE reccomendation. See \code{\link[streamMetabolizer]{mm_name}} for full options.
#'
#' @return returns .csv of model predictions to local file, outputs dataframe of predictions to environment
#'
#' @examples
#' data <- dataLoad(filepath = "./input_files/streamdata.csv", lat = "40", long = "-100", tz = "America/Chicago")
#' data <- dataPrep(data, na.fill = "interpolation")
#' model_data <- metabolismModeling(data, filename = "metabolismmodel_results.csv")
#'
#' @import streamMetabolizer
#' @export
#'
metabolismModeling <- function(metab.data, filename, pool_K600 = "binned",
                               err_obs_iid = TRUE,
                               err_proc_acor = FALSE, err_proc_iid = TRUE,
                               ode_method = "trapezoid", deficit_src = "DO_mod"){
  #
  # check if all model variables are actually in the dataframe or not, return error if false
  #
  # check if metab.data contains any NA.
  if(anyNA(metab.data)){
    stop(paste("Data contains NA values, which break streamMetabolizer. Process data with",
               "\n\tdataPrep function first, then use metabolismModeling."), call. = FALSE)
  }
  # check if .csv of model results already exists
  if(file.exists(filename)){
    # if file exists, load model results from file
    output <- read.csv(filename, header = TRUE)
    message("Metabolism model results loaded from local file.")
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
  write.csv(output$predictions, file = filename,
            row.names = FALSE)
  #
  return(output$predictions)
}
