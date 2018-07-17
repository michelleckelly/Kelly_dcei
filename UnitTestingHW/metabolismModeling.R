# script to run a bayseian model to estimate stream metabolism based on stream discharge, dissolved oxygen, dissolved oxygen saturation, and temperature data.
#
# Arguments
# data       dataframe of stream metabolism time series parameters, 
#               assembled by dataLoad.R
# na.fill       character vector of the algorithm that will be used by imputeTS::na.seasplit 
#               to fill gaps in the time series data. see documentation of na.seasplit() 
#               for a list of options. Defaults to interpolation
# lat           character vector of the latitude (degrees) of the USGS gage station
# long          character vector of the longitude (degrees) of the USGS gage station
#
# Outputs
#
#
# Dependencies
#library(StreamPULSE)
#library(streamMetabolizer)

metabolismModeling <- function(data, na.fill = "interpolation"){
  # model will be broken by NA values, therefore
  # fill in NA gaps with imputeTS::na.seasplit according to algorithm of choice
  data$WaterTemp_C <- imputeTS::na.seasplit(data$WaterTemp_C, algorithm = na.fill)
  data$Discharge_m3s <- imputeTS::na.seasplit(data$Discharge_m3s, algorithm = na.fill)
  data$DO_mgL <- imputeTS::na.seasplit(data$DO_mgL, algorithm = na.fill)
  data$DOsat_pct <- imputeTS::na.seasplit(data$DOsat_pct, algorithm = na.fill)
  #
  # model will be broken by discharge values that are negative or 0, therefore
  if(any(na.omit(data$Discharge_m3s) <= 0)){
    # let user know that there are discharge values <= 0
    warning("Dataset has discharge values <= 0. Replacing these values with 0.00001.",
            call. = FALSE)
    # replace them with near-zero value
    data$Discharge_m3s[data$Discharge_m3s <= 0] <- 0.00001
  }
  #
  # convert UTC to solar time
  data$solar.time <- streamMetabolizer::convert_UTC_to_solartime(date.time = data$dateTime, 
                                                                 longitude = data$Long,
                                                                 time.type = "mean solar")
  # estimate PAR with streamMetabolizer::calc_solar_insolation
  # note: see some of the streamPULSE documentation for doing this
  apparentsolartime <- streamMetabolizer::convert_UTC_to_solartime(date.time = data$dateTime, 
                                                                   longitude = data$Long,
                                                                   time.type = "apparent solar")
  data$light <- streamMetabolizer::calc_solar_insolation(app.solar.time = apparentsolartime,
                                                         latitude = data$Lat,
                                                         format = "degrees")
  # let user know that PAR was estimated based on latitude and time
  cat("PAR estimated based on latitude and time.\n")
  #
  # estimate depth (m) from discharge data
  data$depth <- streamMetabolizer::calc_depth(data$Discharge_m3s)
  # 
  # convert DO saturation in percent to DO saturation in mg/L
  data$DO.sat <- data$DO_mgL / (data$DOsat_pct*0.01)
  #
  # rename columns so they're recognized by streamMetabolizer
  names(data)[names(data)=="DO_mgL"] <- "DO.obs"
  names(data)[names(data)=="WaterTemp_C"] <- "temp.water"
  #
  # assemble streamMetabolizer call by setting model name and parameters
  model_variables <- c("solar.time", "DO.obs", "DO.sat", "depth", "temp.water", "light")
  #
  # insert check if all model variables are actually in the dataframe or not, return error if F
  #
  # subset to just the data needed to fit the model
  fitdata <- dplyr::select_(data, .dots = model_variables)
  modelname <- streamMetabolizer::mm_name(type='mle', ode_method = "trapezoid")
  modelspecs <- streamMetabolizer::specs(modelname)
  cat("Using a maximum likelihood estimation (MLE) method to fit your metabolism model.\n")
  # run the model
  modelfit <- streamMetabolizer::metab(modelspecs, data = fitdata)
  # extract predictions
  predictions <- streamMetabolizer::predict_metab(modelfit)
  # return model results
  output <- list(predictions = predictions, fit = modelfit)
  return(output)
}