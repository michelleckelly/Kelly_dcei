# load local datafile, transform local time to UTC time, convert from American to SI units, and then return data to user
# 
# Arguments
# filepath         character vector, file path to river data
# lat              character vector, decimal latitude of site location
# long             character vector, decimal longitude of site location
# tz               character vector of the time zone, as recognized by OlsonNames()
# 
# Output
# data             dataframe of stream data (discharge, water temperature, dissolved oxygen, 
#                  photosynthetically active radiation) from USGS sensor site
#
# Dependencies
# library(lubridate)
#
# note: 
#       add check that if local file does not exist and start/end.date is not 
#         specified, return error message asking for date specification
#         can also add this check to Tests.Rmd

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
  data$dateTime_Local <- mdy_hm(data$dateTime, tz = tz)
  data$dateTime_UTC <- with_tz(data$dateTime_Local, "UTC")
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
