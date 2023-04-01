

library('tidyverse')
library('scales')
library('gridExtra')
library('lubridate')
library('googlesheets4')



#bp_inX <- read.csv('/Users/willey/Google Drive/Health/OMRON.csv')
# Read google sheets data into R
# keep bp_in pristine
bp_in <- read_sheet('https://docs.google.com/spreadsheets/d/1ZdK5msA6lJPOXnp4lrSIL-EXHu5exARBoeQlcYx8jxw/edit#gid=1687413771')

# data cleanup
bp_tmp <- unique(bp_in)
after_unique <- nrow(bp_tmp)
print(paste("Dropped", nrow(bp_in) - after_unique, "duplicate rows"))

# remove observations marked to be exlcuded
bp_tmp <- bp_tmp %>% filter(is.na(Exclude))
print(paste("Dropped", after_unique - nrow(bp_tmp), "excluded rows"))

# date times
bp_tmp$dt <- as.POSIXct(as.character(paste(bp_tmp$Date, bp_tmp$Time)), format = "%d-%b-%y %H:%M")

# add cleaner col names
bp_tmp$Systolic <- bp_tmp$`Systolic (mmHg)`
bp_tmp$`Systolic (mmHg)` <- NULL
bp_tmp$Diastolic <- bp_tmp$`Diastolic (mmHg)`
bp_tmp$`Diastolic (mmHg)` <- NULL

# new df with average values per day
bp_days <- bp_tmp %>%
  group_by(Date) %>%
  summarize(sys_mean = mean(Systolic),dia_mean = mean(Diastolic))

# long instead of wide
combo_days <- gather(bp_days, measure, value, sys_mean:dia_mean)

# recent
recent_days <- bp_days %>% filter(Date >= Sys.time() - months(3))
recent_combo_days <- combo_days %>% filter(Date >= Sys.time() - months(3))

summary(bp_days$sys_mean)
sd(bp_days$sys_mean)
plot(bp_days$Date, bp_days$sys_mean)
hist(bp_days$sys_mean)

summary(recent_days$sys_mean)
sd(recent_days$sys_mean)
plot(recent_days$Date, recent_days$sys_mean)
hist(recent_days$sys_mean)


make_boxplot <- function(df, subtitle = NULL) {
  p <- ggplot(df, aes(x = measure, y = value)) +
    geom_boxplot(outlier.colour = "red") +
    ggtitle("Blood Pressure - Boxplot", subtitle = subtitle) +
    theme_bw() +
    theme(
      axis.title.y.left = element_text(color = "blue"),
      axis.text.y.left = element_text(color = "blue"),
      axis.title.y.right = element_text(color = "red"),
      axis.text.y.right = element_text(color = "red"),
      plot.title = element_text(hjust = 0.5),
      plot.caption = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    ) +
    labs(x = "")
  return(p)

}

print(make_boxplot(combo_days, "All Observations"))
print(make_boxplot(recent_combo_days, "Recent Observations"))


summary(bp_days$dia_mean)
sd(bp_days$dia_mean)
plot(bp_days$Date, bp_days$dia_mean)
hist(bp_days$dia_mean)

# for using two scales
scale_factor <- max(bp_days$sys_mean) / max(bp_days$dia_mean)

# all values
ggplot(bp_in, aes(x = Date)) +
  geom_smooth(
    aes(y = `Systolic (mmHg)`),
    method = "loess",
    col = "red",
    formula = 'y ~ x'
  ) +
  geom_smooth(aes(y =  `Diastolic (mmHg)` * scale_factor),
              method = "loess",
              formula = 'y ~ x') +
  geom_point(aes(y = `Systolic (mmHg)`), col = "blue") +
  geom_point(aes(y =  `Diastolic (mmHg)` * scale_factor), col = "red") +
  ggtitle("Blood Pressure - All Raw Values with Smoothed Line") +
  scale_y_continuous(name = "Systolic (mmHg)",
#                     sec.axis = sec_axis( ~ . / scale_factor, name = "Diastolic (mmHg)")
                    ) +
  theme_bw() +
  theme(
    axis.title.y.left = element_text(color = "blue"),
    axis.text.y.left = element_text(color = "blue"),
    axis.title.y.right = element_text(color = "red"),
    axis.text.y.right = element_text(color = "red"),
    plot.title = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0.5)
  ) +
  labs(x = "Date")

model_sys <- lm(sys_mean ~ Date, data = bp_days)
sys_r2 <- summary(model_sys)
summary(model_sys)$r.squared
model_dia <- lm(dia_mean ~ Date, data = bp_days)
summary(model_dia)
dia_r2 <- summary(model_dia)$r.squared

# day values BOTH
ggplot(bp_days, aes(x = Date)) +
  geom_hline(
    yintercept = 120,
    col = "blue",
    linewidth = .5,
    linetype = "dashed"
  ) +
  geom_hline(
    yintercept = 130,
    col = "blue",
    linewidth = .5,
    linetype = "dashed"
  ) +
  geom_hline(
    yintercept = 140,
    col = "blue",
    linewidth = .5,
    linetype = "dashed"
  ) +
  geom_hline(
    yintercept = 80 * scale_factor,
    col = "red",
    linewidth = .5,
    linetype = "dashed"
  ) +
  geom_hline(
    yintercept = 90 * scale_factor,
    col = "red",
    linewidth = .5,
    linetype = "dashed"
  ) +
  geom_smooth(aes(y = sys_mean),
              method = "lm",
              col = "blue",
              formula = 'y ~ x') +
  geom_smooth(aes(y =  dia_mean * scale_factor),
              method = "lm",
              col = "red",
              formula = 'y ~ x') +
  geom_point(aes(y = sys_mean), col = "blue") +
  geom_point(aes(y =  dia_mean * scale_factor), col = "red") +
  ggtitle("Blood Pressure - Day Average Values") +
  scale_y_continuous(name = "Systolic (mmHg)",
                     sec.axis = sec_axis(~ . / scale_factor, name = "Diastolic (mmHg)")) +
  theme_bw() +
  theme(
    axis.title.y.left = element_text(color = "blue"),
    axis.text.y.left = element_text(color = "blue"),
    axis.title.y.right = element_text(color = "red"),
    axis.text.y.right = element_text(color = "red"),
    plot.title = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0.5)
  ) +
  labs(x = "Date")

x_min = min(bp_days$Date)
dia_range_labels <-
  data.frame(
    x = c(x_min, x_min, x_min),
    y_low = c(70, 80, 90),
    y_mid = c(75, 85, 95),
    labels = c('Normal', 'Hypertension 1', 'Hypertension 2')
  )
sys_range_labels <-
  data.frame(
    x = rep(x_min, 4),
    y_low = c(120, 130, 140, 150),
    y_mid = c(115, 125, 135, 145),
    labels = c('Normal', 'Elevated', 'Hypertension 1', 'Hypertension 2')
  )



# day values SYSTOLIC
sys_plot <- ggplot(bp_days, aes(x = Date)) +
  geom_hline(
    yintercept = 120,
    col = "blue",
    linewidth = .5,
    linetype = "dashed"
  ) +
  geom_hline(
    yintercept = 130,
    col = "blue",
    linewidth = .5,
    linetype = "dashed"
  ) +
  geom_hline(
    yintercept = 140,
    col = "blue",
    linewidth = .5,
    linetype = "dashed"
  ) +
  geom_smooth(
    aes(y = sys_mean),
    method = "lm",
    col = "blue",
    formula = 'y ~ x'
  ) +
  geom_point(aes(y = sys_mean), col = "blue") +
  ggtitle("Systolic Blood Pressure - Day Average Values") +
  scale_y_continuous(name = "Systolic (mmHg)") +
  scale_x_datetime(breaks = "3 months",
                   expand = c(0, 0)) +
  theme_bw() +
  theme(
    axis.title.y.left = element_text(color = "blue"),
    axis.text.y.left = element_text(color = "blue"),
    axis.title.y.right = element_text(color = "red"),
    axis.text.y.right = element_text(color = "red"),
    plot.title = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 1,
                                margin = margin(t = -10, unit = "pt"))
  ) +
  geom_text(data = sys_range_labels,
            angle = 90,
            aes(
              x = x + days(5),
              y = y_mid,
              label = labels
            )) +
  labs(x = "Date",
       caption = paste("created",
                        format(Sys.time(),
                               "%m/%d/%Y %H:%M:%S")))

sys_plot
sys_plot_wc <- ggplot(bp_days, aes(x = Date)) +
  geom_rect(
    ymin = 0,
    ymax = 120,
    xmin = min(bp_days$Date),
    xmax = max(bp_days$Date),
    linewidth = 0,
    fill = "darkseagreen1",
    alpha = 0.025
  ) +
  geom_rect(
    ymin = 120,
    ymax = 130,
    xmin = min(bp_days$Date),
    xmax = max(bp_days$Date),
    linewidth = 0,
    fill = "yellow",
    alpha = 0.005
  ) +
  geom_rect(
    ymin = 130,
    ymax = 140,
    xmin = min(bp_days$Date),
    xmax = max(bp_days$Date),
    linewidth = 0,
    fill = "indianred1",
    alpha = 0.005
  ) +
  geom_rect(
    ymin = 140,
    ymax = 200,
    xmin = min(bp_days$Date),
    xmax = max(bp_days$Date),
    linewidth = 0,
    fill = "darkred",
    alpha = 0.01
  ) +
  geom_hline(
    yintercept = 120,
    col = "blue",
    linewidth = .5,
    linetype = "dashed"
  ) +
  geom_hline(
    yintercept = 130,
    col = "blue",
    linewidth = .5,
    linetype = "dashed"
  ) +
  geom_hline(
    yintercept = 140,
    col = "blue",
    linewidth = .5,
    linetype = "dashed"
  ) +
  geom_smooth(
    aes(y = sys_mean),
    method = "lm",
    col = "blue",
    formula = 'y ~ x'
  ) +
  geom_point(aes(y = sys_mean), col = "blue") +
  ggtitle("Systolic Blood Pressure - Day Average Values") +
  scale_y_continuous(name = "Systolic (mmHg)") +
  scale_x_datetime(breaks = "3 months",
                   expand = c(0, 0)) +
  theme_bw() +
  theme(
    axis.title.y.left = element_text(color = "blue"),
    axis.text.y.left = element_text(color = "blue"),
    axis.title.y.right = element_text(color = "red"),
    axis.text.y.right = element_text(color = "red"),
    plot.title = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 1,
                                margin = margin(t = -10, unit = "pt"))
  ) +
  geom_text(data = sys_range_labels,
            angle = 90,
            aes(
              x = x + days(5),
              y = y_mid,
              label = labels
            )) +
  labs(x = "Date",
       caption = paste("created",
                       format(Sys.time(),
                              "%m/%d/%Y %H:%M:%S")))
sys_plot_wc
# DIASTOLIC

dia_plot <- ggplot(bp_days, aes(x = Date)) +
  #  ylim
  geom_hline(
    yintercept = 80 ,
    col = "red",
    linewidth = .5,
    linetype = "dashed"
  ) +
  geom_hline(
    yintercept = 90 ,
    col = "red",
    linewidth = .5,
    linetype = "dashed"
  ) +
  geom_smooth(aes(y =  dia_mean),
              method = "lm",
              col = "red",
              formula = 'y ~ x') +
  geom_point(aes(y =  dia_mean), col = "red") +
  ggtitle("Diastolic Blood Pressure - Day Average Values") +
  scale_y_continuous(name = "Diastolic (mmHg)") +
  scale_x_datetime(breaks = "3 months",
                   expand = c(0, 0)) +
  theme_bw() +
  theme(
    axis.title.y.left = element_text(color = "red"),
    axis.text.y.left = element_text(color = "red"),
    plot.title = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 1,
                                margin = margin(t = -10, unit = "pt"))
  ) +
  geom_text(data = dia_range_labels,
            angle = 90,
            aes(x = x + days(5), y = y_mid, label = labels)) +
  labs(x = "Date",
       caption = paste("created",
                       format(Sys.time(),
                              "%m/%d/%Y %H:%M:%S")))

dia_plot

dia_plot_wc <-  ggplot(bp_days, aes(x = Date)) +
  geom_rect(
    ymin = 0,
    ymax = 80,
    xmin = min(bp_days$Date),
    xmax = max(bp_days$Date),
    linewidth = 0,
    fill = "darkseagreen1",
    alpha = 1
  ) +
  geom_rect(
    ymin = 80,
    ymax = 90,
    xmin = min(bp_days$Date),
    xmax = max(bp_days$Date),
    linewidth = 0,
    fill = "indianred1",
    alpha = 0.005
  ) +
  geom_rect(
    ymin = 90,
    ymax = 900,
    xmin = min(bp_days$Date),
    xmax = max(bp_days$Date),
    linewidth = 0,
    fill = "darkred",
    alpha = 0.01
  ) +
  #  ylim
  geom_hline(
    yintercept = 80 ,
    col = "red",
    linewidth = .5,
    linetype = "dashed"
  ) +
  geom_hline(
    yintercept = 90 ,
    col = "red",
    linewidth = .5,
    linetype = "dashed"
  ) +
  geom_smooth(aes(y =  dia_mean),
              method = "lm",
              col = "red",
              formula = 'y ~ x') +
  geom_point(aes(y =  dia_mean), col = "red") +
  ggtitle("Diastolic Blood Pressure - Day Average Values") +
  scale_y_continuous(name = "Diastolic (mmHg)") +
  scale_x_datetime(breaks = "3 months",
                   expand = c(0, 0)) +
  theme_bw() +
  theme(
    axis.title.y.left = element_text(color = "red"),
    axis.text.y.left = element_text(color = "red"),
    plot.title = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 1,
                                margin = margin(t = -10, unit = "pt"))
  ) +
  geom_text(data = dia_range_labels,
            angle = 90,
            aes(x = x + days(5), y = y_mid, label = labels)) +
  labs(x = "Date",
       caption = paste("created",
                       format(Sys.time(),
                              "%m/%d/%Y %H:%M:%S")))

dia_plot_wc

grid.arrange(sys_plot, dia_plot)
grid.arrange(sys_plot_wc, dia_plot_wc)

