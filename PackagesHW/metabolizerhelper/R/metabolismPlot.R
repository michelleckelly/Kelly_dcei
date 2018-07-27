#' \code{metabolismPlot} generate a pretty plot of GPP and ER estimates
#'
#' returns a formatted plot of metabolism modeling results, including confidence bands on model estimates.
#'
#' @param metab_output the list object returned by \code{metabolismModeling}
#' @param filename character vector of filename for plot (including file extension)
#'
#' @return returns plot of metabolism data and saves plot to local file using ggsave()
#'
#' @examples
#' data <- dataLoad(filepath = "./input_files/streamdata.csv", lat = "40", long = "-100", tz = "America/Chicago")
#' data <- dataPrep(data, na.fill = "interpolation")
#' model_data <- metabolismModeling(data, filename = "metabolismmodel_results.csv")
#' metabolismPlot(model_data, filename = "metabolismplot.png")
#'
#' @import ggplot2
#' @import lubridate
#' @export

metabolismPlot <- function(metab_output, filename){
  metab_output$date <- lubridate::ymd(metab_output$date)
  plot <-
    ggplot2::ggplot(data = metab_output, aes(x = date)) +
      ggplot2::geom_line(aes(y = GPP, color = "GPP")) +
      ggplot2::geom_line(aes(y = ER, color = "ER")) +
      ggplot2::geom_ribbon(aes(ymin = GPP.lower, ymax = GPP.upper, fill = "GPP"), alpha = 0.3) +
      ggplot2::geom_ribbon(aes(ymin = ER.lower, ymax = ER.upper, fill = "ER"), alpha = 0.3) +
      ggplot2::geom_line(aes(y = 0), linetype = "dashed", color = "grey") +
      ggplot2::labs(x = NULL, y = bquote(O[2] ~ m^-2 ~ d^-1 ~ "(g)")) +
      ggplot2::scale_color_discrete(name = NULL) +
      ggplot2::scale_x_date(date_breaks = "1 month", date_labels = "%b") +
      ggplot2::scale_fill_discrete(name = "95% CI", labels = NULL) +
      ggplot2::theme_classic() + theme(legend.position = "top")
  ggplot2::ggsave(filename = filename, plot = plot, width = 8,
                  height = 3, units = "in")
  return(plot)
}
