% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metabolismModeling.R
\name{metabolismModeling}
\alias{metabolismModeling}
\title{\code{metabolismModeling} model metabolism with streamMetabolizer package}
\usage{
metabolismModeling(metab.data, filename, pool_K600 = "binned",
  err_obs_iid = TRUE, err_proc_acor = FALSE, err_proc_iid = TRUE,
  ode_method = "trapezoid", deficit_src = "DO_mod")
}
\arguments{
\item{metab.data}{dataframe of stream metabolism time series parameters, assembled by \code{\link{dataLoad}} and reformatted by \code{\link{dataPrep}}}

\item{filename}{character vector. input of what local .csv of model output should be named, including filepath if needed}

\item{pool_K600}{character vector, how should model pool information across multiple days to estimate K600? See \code{\link[streamMetabolizer]{mm_name}} for more info. Default is "binned"}

\item{err_obs_iid}{logical. should IID observation error in DO data be included in the model? Defaults to TRUE, which is the streamPULSE reccommendation. See \code{\link[streamMetabolizer]{mm_name}} for more info.}

\item{err_proc_acor}{logical. should autocorrelated process error be included? Defaults to FALSE, which is the streamPULSE reccommendation. See \code{\link[streamMetabolizer]{mm_name}} for more info.}

\item{err_proc_iid}{logical. should IID process error in DO data be included in the model? Defaults to TRUE, which is the streamPULSE reccommendation. See \code{\link[streamMetabolizer]{mm_name}} for more info.}

\item{ode_method}{character. method for solving the differential equation for DO. See \code{\link[streamMetabolizer]{mm_name}} for full list of options, defaults to "trapezoid"}

\item{deficit_src}{character vector. should the DO estimate be computed from the observed or modeled DO data? defaults to "DO_mod", which is the streamPULSE reccomendation. See \code{\link[streamMetabolizer]{mm_name}} for full options.}
}
\value{
returns .csv of model predictions to local file, outputs dataframe of predictions to environment
}
\description{
estimate stream metabolism based on stream discharge, dissolved oxygen, dissolved oxygen saturation, and temperature data, using an MCMC model.
}
\examples{
data <- dataLoad(filepath = "./input_files/streamdata.csv", lat = "40", long = "-100", tz = "America/Chicago")
data <- dataPrep(data, na.fill = "interpolation")
model_data <- metabolismModeling(data, filename = "metabolismmodel_results.csv")

}
