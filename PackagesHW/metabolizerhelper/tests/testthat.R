library(testthat)
library(metabolizerhelper)
library(lubridate)

test_check("metabolizerhelper")

# set up data, use dataLoad on data
filepath <- "./vignettes/IA_MississippiR.csv"
lat <- "41.780556"
long <- "-90.251944"
tz <- "America/Chicago"
data <- dataLoad(filepath, lat, long, tz)

# test dataLoad
test_that("Time zone of dataLoad(data)$dateTime is UTC", {
  expect_equal(tz(data$dateTime), "UTC")
})

test_that("dataLoad(data) has correct classes", {
  expect_is(data$Lat, "numeric")
  expect_is(data$Long, "numeric")
  expect_is(data$dateTime, "POSIXct")
  expect_is(data$WaterTemp_C, "numeric")
  expect_is(data$Discharge_m3s, "numeric")
  expect_is(data$DO_mgL, "numeric")
  expect_is(data$DOsat_pct, "numeric")
})

# use dataPrep on data
data <- dataPrep(data)

# test that DO sat computed correctly
# test for NAs (should be no NAs)

