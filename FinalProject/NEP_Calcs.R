# Calculate Net Ecosystem Production (NEP) from metabolism model output
#
# Arguments
# model_output    dataframe returned by metabolismModeling
# Returns
# dataframe of date, cumulative NEP, cumulative GPP, and cumulative ER for site
NEP_calcs <- function(model_output){
  NEP_data <- data.frame(date = model_output$date,
                         GPP = model_output$GPP,
                         ER = model_output$ER)
  # to perform cumulative sum correctly, any NA values must be set to 0
  NEP_data[is.na(NEP_data)] <- 0
  # take cumulative sum
  NEP_data <- data.frame(date = NEP_data$date,
                         GPP_sum = cumsum(NEP_data$GPP),
                         ER_sum = cumsum(NEP_data$ER))
  # GPP + ER = NEP
  NEP_data$NEP <- rowSums(NEP_data[, c(2,3)])
  return(NEP_data)
}
#
# perform NEP calcs on all model data
NEP_Missip <- NEP_calcs(model_Mississippi)
NEP_Park <- NEP_calcs(model_NorthBranch)
NEP_Sacr <- NEP_calcs(model_Sacramento)
