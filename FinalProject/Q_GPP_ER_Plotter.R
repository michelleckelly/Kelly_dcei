# assemble plots of discharge versus GPP and ER for each site, and save a combined plot to file
# this script is not a stand alone function, and depends on variables generated within FinalProject.Rmd
#
# assemble data frame -----------------------------------------------
#
# make sure the date column of the model results is the correct class
model_Mississippi$date <- lubridate::ymd(model_Mississippi$date)
model_NorthBranch$date <- lubridate::ymd(model_NorthBranch$date)
model_Sacramento$date <- lubridate::ymd(model_Sacramento$date)
#
# find daily average discharge in the Mississippi
Q <- 
  MississippiR_raw %>%
  group_by(date = lubridate::date(dateTime)) %>%
  summarise(Q = mean(na.omit(Discharge_m3s)))
# combine discharge with metabolism model results
Q_GPP_ER <- merge(Q, model_Mississippi, by = "date")
# keep only what's needed, toss notes, GPP.upper, GPP.lower etc from model results
Q_GPP_ER <- data.frame(date = Q_GPP_ER$date, Q.Missip = Q_GPP_ER$Q, 
                       GPP.Missip = Q_GPP_ER$GPP, ER.Missip = Q_GPP_ER$ER)
#
# find daily average discharge in Park
Q <- 
  NorthBranch_raw %>%
  group_by(date = lubridate::date(dateTime)) %>%
  summarise(Q = mean(na.omit(Discharge_m3s)))
# combine discharge with metabolism model results
Q <- merge(Q, model_NorthBranch, by = "date")
# keep only what's needed, toss notes, GPP.upper, GPP.lower etc from model results
Q <- data.frame(date = Q$date, Q.Park = Q$Q, GPP.Park = Q$GPP, ER.Park = Q$ER)
# merge park results with mississippi dataframe
Q_GPP_ER <- merge(Q_GPP_ER, Q, by = "date")
#
# find daily average discharge in sacramento
Q <- 
  SacramentoR_raw %>%
  group_by(date = lubridate::date(dateTime)) %>%
  summarise(Q = mean(na.omit(Discharge_m3s)))
# combine discharge with metabolism model results
Q <- merge(Q, model_Sacramento, by = "date")
# keep only what's needed, toss notes, GPP.upper, GPP.lower etc from model results
Q <- data.frame(date = Q$date, Q.Sacr = Q$Q, GPP.Sacr = Q$GPP, ER.Sacr = Q$ER)
#
# merge all results
Q_GPP_ER <- merge(Q_GPP_ER, Q, by = "date")
#
# assemble plots ---------------------------------------------------------
#
# GPP
# Mississippi, Q GPP
a <- ggplot(data = Q_GPP_ER) +
  geom_point(aes(x = Q.Missip, y = GPP.Missip), shape = 1) +
  scale_x_continuous(breaks = c(1500, 3000, 4500)) +
  theme_classic() + labs(x = NULL, y = NULL, tag = "A")
# Sacramento, Q GPP
b <- ggplot(data = Q_GPP_ER) +
  geom_point(aes(x = Q.Sacr, y = GPP.Sacr), shape = 1) +
  scale_x_continuous(breaks = c(800, 1600, 2400)) +
  scale_y_continuous(breaks = c(-0.5, 1.5, 3.5, 5.5)) +
  labs(x = NULL, y = NULL, tag = "B") +
  theme_classic()
# Park, Q GPP
c <- ggplot(data = Q_GPP_ER) +
  geom_point(aes(x = Q.Park, y = GPP.Park), shape = 1) +
  labs(x = NULL, y = NULL, tag = "C") +
  scale_y_continuous(breaks = c(0, 3, 6, 9)) +
  theme_classic()
# ER
# Mississippi, Q ER
d <- ggplot(data = Q_GPP_ER) +
  geom_point(aes(x = Q.Missip, y = ER.Missip), shape = 1) +
  scale_x_continuous(breaks = c(1500, 3000, 4500)) +
  scale_y_continuous(breaks = c(-22, -12, -2, 8)) +
  labs(x = NULL, y = NULL, tag = "D") +
  theme_classic()
# Sacramento, Q ER
e <- ggplot(data = Q_GPP_ER) +
  geom_point(aes(x = Q.Sacr, y = ER.Sacr), shape = 1) +
  scale_x_continuous(breaks = c(800, 1600, 2400)) +
  scale_y_continuous(breaks = c(-30, -15, -0, 15)) +
  labs(x = NULL, y = NULL, tag = "E") +
  theme_classic()
# Park, Q ER
f <- ggplot(data = Q_GPP_ER) +
  geom_point(aes(x = Q.Park, y = ER.Park), shape = 1) +
  scale_y_continuous(breaks = c(-9, -6, -3, 0)) +
  labs(x = NULL, y = NULL, tag = "F") +
  theme_classic()
#
# y label for GPP plots
ylabel_GPP <- 
  textGrob(expression(paste("GPP (g ", ~O[2]~m^-2~d^-1, ")")),
           rot = 90, gp = gpar(cex = 0.9))
# y label for ER plots
ylabel_ER <- 
  textGrob(expression(paste("ER (g ", ~O[2]~m^-2~d^-1, ")")),
           rot = 90, gp = gpar(cex = 0.9))
# x label
xlabel <- 
  textGrob(expression(paste("Q (", m^3, " ", s^-1, ")")), gp = gpar(cex = 0.9))
#
# combined plot
Q_GPP_ER_plot <- grid.arrange(arrangeGrob(a,b,c, left = ylabel_GPP, ncol = 3, nrow = 1),
                              arrangeGrob(d,e,f, left = ylabel_ER, ncol = 3, nrow = 1),
                              bottom = xlabel)
Q_GPP_ER_plot
# save plot to outside file
ggsave(file = "./FinalProject/figures/Q_GPP_ERplot.png", Q_GPP_ER_plot, width = 6, height = 4)