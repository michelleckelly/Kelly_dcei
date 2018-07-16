# Checks if datafile for Kansas River at Desoto Kansas exists locally. If datafile of stream observations not present, pulls data from USGS and merge with NEON light data file. If datafile is present, loads local datafile.
# 
# Arguments
# start.date        start date of data pull from APIs. default value is NULL
# end.date          end date of data pull from APIs. default value is NULL
# 
# Output
#                   dataframe of stream data (discharge, water temperature, 
#                   dissolved oxygen, photosynthetically active radiation) from USGS sensor
#                   site on the Kansas River at Desoto, Kansas
# Dependencies
# library(lubridate)
# library(dataRetrieval)
#
# note: 
#       add check that if local file does not exist and start/end.date is not 
#         specified, return error message asking for date specification
#         can also add this check to Tests.Rmd
#       if start date end date interval not within par data, return error
#         add this as a check to tests.Rmd

dataLoad <- function(start.date = NULL, end.date = NULL){
  #
  # add safety checks here:
  #
  # if local file does not exist, but start/end date is not specified, return error
  # if start and end date are not contained within available PAR data, return error
  #
  if(file.exists("./data_files/MetabolismData_KansasRDesotoKS.csv")){
    # if local file exists, load local file
    data <- read.csv("./data_files/MetabolismData_KansasRDesotoKS.csv", header = TRUE)
    # check that dateTime is in proper format
    data$dateTime <- ymd_hms(data$dateTime, tz = "America/Chicago")
    # return dataframe and exit function
    return(data)
  }
  if(!file.exists("./data_files/MetabolismData_KansasRDesotoKS.csv")){
    # if local file does not exist, pull and compile local file from APIs
    #
    site.code <- "06892350" # site code for Kansas River at Desoto, KS
    #
    # parameter codes: see https://nwis.waterdata.usgs.gov/usa/nwis/pmcodes
    #   00010 = water temperature, degrees C
    #   00060 = discharge, ft^3/s
    #   00300 = dissolved oxygen, mg/L
    #   00301 = dissolved oxygen, percent saturation
    parameter.codes <- c("00010", "00060", "00300", "00301")
    #
    # call to API to pull data
    data <- readNWISuv(siteNumbers = site.code, parameterCd = parameter.codes,
                       startDate = start.date, endDate = end.date)
    #
    # rename columns from codes to human-readable names
    names(data)[names(data)=="X_00010_00000"] <- "WaterTemp_C"
    names(data)[names(data)=="X_00060_00000"] <- "Discharge_m3s"
    names(data)[names(data)=="X_00300_00000"] <- "DO_mgL"
    names(data)[names(data)=="X_00301_00000"] <- "DOsat_pct"
    # drop unnecessary columns
    data <- data[, c("dateTime", "WaterTemp_C", "Discharge_m3s", "DO_mgL", "DOsat_pct")]
    # format dateTime
    data$dateTime <- ymd_hms(data$dateTime)
    #
    # convert units from cubic foot per second to m3 per second
    ft3s_m3s <- function(ft3s){
      m3s <- ft3s*0.0283168
      return(m3s)
    }
    data$Discharge_m3s <- ft3s_m3s(data$Discharge_m3s)
    #
    # dateTime time zone is UTC
    #
    # pull in light data from NEON
    PAR <- read.csv("./data_files/PAR_30min.csv", header = TRUE) # dateTime is UTC
    # format dateTime
    PAR$dateTime <- mdy_hm(PAR$dateTime)
    #
    data <- merge(data, PAR, by = "dateTime")
    #
    # save .csv file locally
    write.csv(data, row.names = FALSE, 
              file = "./data_files/MetabolismData_KansasRDesotoKS.csv")
    # return dataframe and exit function
    return(data)
  }
}