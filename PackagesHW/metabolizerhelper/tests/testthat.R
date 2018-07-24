library(testthat)
library(metabolizerhelper)
library(lubridate)

test_check("metabolizerhelper")
#
# create dummy data
dummyData <- data.frame(site_no = c(1,1), dateTime = c("1/1/2017 0:00", "1/1/2017 0:15"),
                        tz_cd	= c("CST", "CST"), Discharge_ft3s = c(10000.1, 10000.0),
                        WaterTemp_C	= c(20, 20.5), DOsat_pct = c(90.1, 90.0),
                        DO_mgL = c(8.9, 8.9))
write.csv(dummyData, file = "./tests/dummyTestData.csv", row.names = FALSE)
#
# load in with dataLoad
lat <- "40"
long <- "-90"
tz <- "America/Chicago"
dummyData <- dataLoad("./tests/dummyTestData.csv", lat, long, tz)
#
# test dataLoad ---------
#
# time zone of dateTime column is UTC
test_that("Time zone of dataLoad(dummyData)$dateTime is UTC", {
  expect_equal(tz(dummyData$dateTime), "UTC")
})
#
# all column classes are correct
test_that("dataLoad(dummyData) has correct classes", {
  expect_is(dummyData$Lat, "numeric")
  expect_is(dummyData$Long, "numeric")
  expect_is(dummyData$dateTime, "POSIXct")
  expect_is(dummyData$WaterTemp_C, "numeric")
  expect_is(dummyData$Discharge_m3s, "numeric")
  expect_is(dummyData$DO_mgL, "numeric")
  expect_is(dummyData$DOsat_pct, "numeric")
})
#
# test dataPrep ----------
#
# use dataPrep on dummyData
dummyData_prepped <- dataPrep(dummyData)
#
# test that DO sat computed correctly
test_that("(DO saturation in mg/L) = (DO mg/L) / (DO saturation in percent)*0.01",{
  expect_equal(dummyData$DO_mgL / (dummyData$DOsat_pct*0.01), dummyData_prepped$DO.sat)
})
#
# test that NA fill worked correctly
test_that("there are no NAs in prepped data",{
  expect_equal(sum(is.na(dummyData_prepped)), 0)
})
#
# test metabolismModeling -----
#
# test that modeling aborts if NAs in dataframe
dummyData_prepped$DO.obs[1] <- NA
test_that("metabolismModeling displays error message if NAs in input dataframe",{
  expect_error(metabolismModeling(dummyData_prepped, filename = "dummyModel.csv"),
               "Data contains NA values, which break streamMetabolizer")
})
#
#
# remove dummy data file
file.remove("./tests/dummyTestData.csv")
