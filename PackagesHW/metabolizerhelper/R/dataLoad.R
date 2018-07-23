#' \code{dataLoad} format raw data and convert from American to SI units
#'
#' load local datafile (date-time, discharge in ft^3/s, water temperature in C, dissolved oxygen in mg/L and percent saturation), transform local time to UTC time, convert from American to SI units, attach latitude and longitude to dataframe, and then return data to user ready to pump into \code{\link{dataPrep}}
#'
#' @param filepath character vector, file path to data file, including file name and extension
#' @param lat character vector, decimal latitude of site location
#' @param long character vector, decimal longitude of site location
#' @param tz character vector of the time zone, as recognized by OlsonNames
#'
#' @return dataframe of date-time in UTC, discharge in m^3/s, water temperature in C, dissolved oxygen in mg/L and percent saturation, site latitude and longitude
#'
#' @examples
#' dataLoad(filepath = "./input_files/streamdata.csv", lat = "40", long = "-100", tz = "America/Chicago")

dataLoad <- function(filepath, lat, long, tz){
  # check if filepath is valid
  if (!file.exists(filepath)){
    stop(paste("Data file can't be found. Check that filepath is specified correctly."),
         call. = FALSE)
  }
  #
  # load data
  data <- read.csv(filepath, header = TRUE)
  col_names <- list("site_no", "dateTime", "tz_cd", "Discharge_ft3s",
                    "WaterTemp_C", "DO_mgL", "DOsat_pct")
  #
  # check if data is missing any columns
  if (any(! colnames(data) %in% col_names)){
    stop(paste("Check that column names are correct. Data file must contain columns labeled:",
               "\n\t'site_no', 'dateTime', 'tz_cd', 'Discharge_ft3s', 'WaterTemp_C'",
               "\n\t'DO_mgL', 'DOsat_pct'"), call. = FALSE)
  }
  # add columns of site latitude and longitude
  data$Lat <- as.numeric(lat)
  data$Long <- as.numeric(long)
  #
  # convert dateTime from local timezone to UTC
  data$dateTime_Local <- lubridate::mdy_hm(data$dateTime, tz = tz)
  data$dateTime_UTC <- lubridate::with_tz(data$dateTime_Local, "UTC")
  data$dateTime <- data$dateTime_UTC
  #
  # convert units from cubic foot per second to m3 per second
  ft3s_m3s <- function(ft3s){
    m3s <- ft3s*0.0283168
    return(m3s)
  }
  data$Discharge_m3s <- ft3s_m3s(data$Discharge_ft3s)
  #
  # drop unnecessary columns
  data <- data[, c("Lat", "Long", "dateTime", "WaterTemp_C", "Discharge_m3s",
                   "DO_mgL", "DOsat_pct")]
  #
  # return dataframe and exit function
  return(data)
}
