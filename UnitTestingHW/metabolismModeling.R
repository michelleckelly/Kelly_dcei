# script to run a bayseian model to estimate stream metabolism based on stream discharge, dissolved oxygen, dissolved oxygen saturation, and temperature data.
#
# Arguments
# dataset       dataframe of stream metabolism time series parameters, 
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

metabolismModeling <- function(dataset, na.fill = "interpolation", lat, long){
  # model will be broken by NA values, therefore
  # fill in NA gaps with imputeTS::na.seasplit according to algorithm of choice
  dataset <- na.seasplit(dataset, algorithm = na.fill)
  #
  # model will be broken by discharge values that are negative or 0, therefore
  if(any(na.omit(dataset$Discharge_m3s) <= 0)){
    # let user know that there are discharge values <= 0
    warning("Dataset has discharge values <= 0. Replacing these values with 0.00001.",
            call. = FALSE)
    # replace them with near-zero value
    dataset$Discharge_m3s[dataset$Discharge_m3s <= 0] <- 0.00001
  }
  #
  # convert UTC to solar time with streamMetabolizer::convert_UTC_to_solartime
  dataset$solar.time <- convert_UTC_to_solartime(date.time = dataset$dateTime, 
                                                 longitude = long,
                                                 time.type = "mean solar")
  # estimate PAR with streamMetabolizer::calc_solar_insolation
  # note: see some of the streamPULSE documentation for doing this
  apparentsolartime <- convert_UTC_to_solartime(date.time = dataset$dateTime, 
                                                longitude = long,
                                                time.type = "apparent solar")
  dataset$light <- calc_solar_insolation(app.solar.time = apparentsolartime,
                                         latitude = lat,
                                         format = "degrees")
  # let user know that PAR was estimated based on latitude and time
  cat("PAR estimated based on latitude and time data.\n")
  #
  # estimate depth from discharge data
  # 
  # convert DO saturation in percent to DO saturation in mg/L
  #
  # assemble streamMetabolizer call by setting model name and parameters
  #
  # fit the model to the data (time consuming step, perhaps use MLE model for class project)
  #
  # return dataframe of model results
}




# convert UTC to solar time
kansasR$solar.time <- convert_UTC_to_solartime(date.time = dataset$dateTime,
                                               longitude = long,
                                               time.type = "mean solar")


plot(kansasR$Discharge_m3s)
plot(kansasR1$Discharge_m3s)

kansasR <- as.list(kansasR)