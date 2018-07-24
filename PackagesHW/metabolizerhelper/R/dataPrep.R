#' \code{dataPrep} Interpolate missing data points and estimate PAR
#'
#' preps data (that has been formatted by \code{\link{dataLoad}}) for metabolism modeling. Interpolates missing data points according to method of choice, estimates PAR from longitude and time data, estimates stream depth from discharge, converts DO saturation in percent to DO saturation in mg/L
#'
#' @param data dataframe, piped from dataLoad function, contains site information and stream data
#' @param na.fill character vector, algorithm that will be used by \code{\link[imputeTS]{na.seasplit}} to fill gaps in the time series data. See documentation of \code{\link[imputeTS]{na.seasplit}} for a list of options. Defaults to interpolation
#'
#' @return dataframe, reformatted and ready to pipe into metabolismModeling.R
#'
#' @examples
#' data <- dataLoad(filepath = "./input_files/streamdata.csv", lat = "40", long = "-100", tz = "America/Chicago")
#' dataPrep(data, na.fill = "interpolation")
#'
#' @import lubridate
#' @import imputeTS
#' @import streamMetabolizer
#' @import dplyr
#' @export

dataPrep <- function(data, na.fill = "interpolation"){
  # check that dateTime is class POSIXct
  if (! lubridate::is.POSIXct(data$dateTime)){
    stop(paste("'dateTime' must be in POSIXct format."), call. = FALSE)
  }
  #
  # check that data contains correct column names
  col_names <- list("Lat", "Long", "dateTime", "Discharge_m3s",
                    "WaterTemp_C", "DO_mgL", "DOsat_pct")
  if(any(! colnames(data) %in% col_names)){
    stop(paste("Check that column names are correct. Data file must contain columns labeled:",
               "\n\t'Lat', 'Long', dateTime', 'Discharge_m3s', 'WaterTemp_C'",
               "\n\t'DO_mgL', 'DOsat_pct'"), call. = FALSE)
  }
  #
  # model will be broken by NA values, therefore
  # fill in NA gaps with imputeTS::na.seasplit according to algorithm of choice
  # note: find a way to silence these warnings after all bug fixes are done
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
                                                                 longitude = data$Long[1],
                                                                 time.type = "mean solar")
  # estimate PAR using latitude and solar time
  apparentsolartime <- streamMetabolizer::convert_UTC_to_solartime(date.time = data$dateTime,
                                                                   longitude = data$Long[1],
                                                                   time.type = "apparent solar")
  data$light <- streamMetabolizer::calc_solar_insolation(app.solar.time = apparentsolartime,
                                                         latitude = data$Lat[1],
                                                         format = "degrees")
  # let user know that PAR was estimated based on latitude and time
  message("PAR estimated based on latitude and time.\n")
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
  # subset to just the data needed to fit the model
  fitdata <- dplyr::select_(data, .dots = model_variables)
  return(fitdata)
}
