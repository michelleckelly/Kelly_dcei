# plot NEP results
#
# Arguments
# NEP_dataframe   dataframe returned by NEP_calcs
# legend_pos      character string, see theme() within ggplot. "top", "bottom", "left", "right" or "none"
# ...             intended so user can specify their own y-axis breaks, if desired
#
# Returns
# time series plot of cumulative ER, GPP, and NEP
#
NEP_plotter <- function(NEP_dataframe, legend_pos, ...){
  NEP_dataframe$date <- lubridate::ymd(NEP_dataframe$date)
  ggplot(data = NEP_dataframe) +
    geom_line(aes(x = date, y = GPP_sum, color = "GPP"), size = 1.5) +
    geom_line(aes(x = date, y = ER_sum, color = "ER"), size = 1.5) +
    geom_line(aes(x = date, y = NEP, color = "NEP"), size = 1.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
    scale_x_date(date_breaks = "2 months", date_labels = "%b") +
    labs(x = NULL, y = bquote("Cumulative"~ O[2] ~ m^-2 ~ d^-1 ~ "(g)")) +
    scale_color_manual(name = NULL, 
                       values = c("NEP" = "#7CAE00", "ER" = "#F8766D", "GPP" = "#00BFC4")) +
    scale_y_continuous(...) +
    guides(color = guide_legend(override.aes = list(size = 4))) +
    theme_classic() + theme(legend.position = legend_pos)
}