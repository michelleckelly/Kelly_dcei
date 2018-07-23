# returns a formatted plot of metabolism modeling results, including confidence bands on model estimates.
#
# Arguments
# metab_output      the list object returned by metabolismModeling.R
# filename          character vector of filename for plot (including file extension)
#
# Output
#                   returns plot of metabolism data, as described in function overview
#
# Dependencies
# library(ggplot2)

metabolismPlot <- function(metab_output, filename){
  # if filename exists, load plot from file and display to user (with message that plot is loaded from file)
  metab_output$date <- ymd(metab_output$date)
  plot <-
    ggplot(data = metab_output, aes(x = date)) +
    geom_line(aes(y = GPP, color = "GPP")) +
    geom_line(aes(y = ER, color = "ER")) +
    geom_ribbon(aes(ymin = GPP.lower, ymax = GPP.upper, fill = "GPP"), alpha = 0.3) +
    geom_ribbon(aes(ymin = ER.lower, ymax = ER.upper, fill = "ER"), alpha = 0.3) +
    geom_line(aes(y = 0), linetype = "dashed", color = "grey") +
    labs(x = "Date", y = bquote(O[2] ~ m^-2 ~ d^-1 ~ "(g)")) +
    scale_color_discrete(name = NULL) +
    scale_fill_discrete(name = "95% CI", labels = NULL) +
    theme_classic() + theme(legend.position = "top")
  ggsave(filename = filename, plot = plot, width = 8,
         height = 3, units = "in")
  return(plot)
}
