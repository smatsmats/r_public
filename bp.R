

library('tidyverse')
library('scales')
library('gridExtra')
library('lubridate')


bp_in <- read.csv('/Users/willey/Google Drive/Health/OMRON.csv')
nrow(bp_in)
bp_in <- unique(bp_in)
nrow(bp_in)

bp_in$dt <- as.POSIXct(as.character(paste(bp_in$Date, bp_in$Time)), format = "%d-%b-%y %H:%M")

# new def with average values per day
bp_days <- bp_in %>%
  group_by(Date) %>%
  summarize(sys_mean = mean(Systolic..mmHg.),dia_mean = mean(Diastolic..mmHg.) )
bp_days$date_ <- as.POSIXct(as.character(bp_days$Date), format = "%d-%b-%y")

summary(bp_days$sys_mean)

sd(bp_days$sys_mean)
plot(bp_days$date_, bp_days$sys_mean)
hist(bp_days$sys_mean)

#ggplot(bp_days, aes(sys_mean)) +
#  geom_histogram(bins = 7)

summary(bp_days$dia_mean)
sd(bp_days$dia_mean)
plot(bp_days$date_, bp_days$dia_mean)
hist(bp_days$dia_mean)

# for using two scales
scale_factor <- max(bp_in$Systolic..mmHg.) / max(bp_in$Diastolic..mmHg.)

# all values
ggplot(bp_in, aes(x = dt)) +
  geom_smooth(
    aes(y = Systolic..mmHg.),
    method = "loess",
    col = "red",
    formula = 'y ~ x'
  ) +
  geom_smooth(aes(y =  Diastolic..mmHg. * scale_factor),
              method = "loess",
              formula = 'y ~ x') +
  geom_point(aes(y = Systolic..mmHg.), col = "blue") +
  geom_point(aes(y =  Diastolic..mmHg. * scale_factor), col = "red") +
  ggtitle("Blood Pressure - All Values with Smoothed Line") +
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

model_sys <- lm(sys_mean ~ date_, data = bp_days)
sys_r2 <- summary(model_sys)
summary(model_sys)$r.squared
model_dia <- lm(dia_mean ~ date_, data = bp_days)
summary(model_dia)
dia_r2 <- summary(model_dia)$r.squared

# day values BOTH
ggplot(bp_days, aes(x = date_)) +
  geom_hline(
    yintercept = 120,
    col = "blue",
    size = .5,
    linetype = "dashed"
  ) +
  geom_hline(
    yintercept = 130,
    col = "blue",
    size = .5,
    linetype = "dashed"
  ) +
  geom_hline(
    yintercept = 140,
    col = "blue",
    size = .5,
    linetype = "dashed"
  ) +
  geom_hline(
    yintercept = 80 * scale_factor,
    col = "red",
    size = .5,
    linetype = "dashed"
  ) +
  geom_hline(
    yintercept = 90 * scale_factor,
    col = "red",
    size = .5,
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

x_min = min(bp_days$date_)
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
sys_plot <- ggplot(bp_days, aes(x = date_)) +
  geom_hline(
    yintercept = 120,
    col = "blue",
    size = .5,
    linetype = "dashed"
  ) +
  geom_hline(
    yintercept = 130,
    col = "blue",
    size = .5,
    linetype = "dashed"
  ) +
  geom_hline(
    yintercept = 140,
    col = "blue",
    size = .5,
    linetype = "dashed"
  ) +
  geom_smooth(aes(y = sys_mean),
              method = "lm",
              col = "blue",
              formula = 'y ~ x') +
  geom_point(aes(y = sys_mean), col = "blue") +
  ggtitle("Systolic Blood Pressure - Day Average Values") +
  scale_y_continuous(name = "Systolic (mmHg)") +
  scale_x_datetime(breaks = "3 months", labels = date_format("%b-%d-%Y"),
                   expand = c(0, 0)) +
  theme_bw() +
  theme(
    axis.title.y.left = element_text(color = "blue"),
    axis.text.y.left = element_text(color = "blue"),
    axis.title.y.right = element_text(color = "red"),
    axis.text.y.right = element_text(color = "red"),
    plot.title = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0.5)
  ) +
  geom_text(data = sys_range_labels,
            angle = 90,
            aes(x = x + days(5), y = y_mid, label = labels)) +
  labs(x = "Date")

sys_plot
sys_plot_wc <- sys_plot +
  geom_rect(
    ymin = 0,
    ymax = 120,
    xmin = min(bp_days$date_),
    xmax = max(bp_days$date_),
    size = 0,
    fill = "darkseagreen1",
    alpha = 0.025
  ) +
  geom_rect(
    ymin = 120,
    ymax = 130,
    xmin = min(bp_days$date_),
    xmax = max(bp_days$date_),
    size = 0,
    fill = "yellow",
    alpha = 0.005
  ) +
  geom_rect(
    ymin = 130,
    ymax = 140,
    xmin = min(bp_days$date_),
    xmax = max(bp_days$date_),
    size = 0,
    fill = "indianred1",
    alpha = 0.005
  ) +
  geom_rect(
    ymin = 140,
    ymax = 200,
    xmin = min(bp_days$date_),
    xmax = max(bp_days$date_),
    size = 0,
    fill = "indianred3",
    alpha = 0.01
  )
sys_plot_wc
# DIASTOLIC

dia_plot <- ggplot(bp_days, aes(x = date_)) +
  #  ylim
  geom_hline(
    yintercept = 80 ,
    col = "red",
    size = .5,
    linetype = "dashed"
  ) +
  geom_hline(
    yintercept = 90 ,
    col = "red",
    size = .5,
    linetype = "dashed"
  ) +
  geom_smooth(aes(y =  dia_mean),
              method = "lm",
              col = "red",
              formula = 'y ~ x') +
  geom_point(aes(y =  dia_mean), col = "red") +
  ggtitle("Diastolic Blood Pressure - Day Average Values") +
  scale_y_continuous(name = "Diastolic (mmHg)") +
  scale_x_datetime(breaks = "3 months", labels = date_format("%b-%d-%Y"),
                   expand = c(0, 0)) +
  theme_bw() +
  theme(
    axis.title.y.left = element_text(color = "red"),
    axis.text.y.left = element_text(color = "red"),
    plot.title = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0.5)
  ) +
  geom_text(data = dia_range_labels,
            angle = 90,
            aes(x = x + days(5), y = y_mid, label = labels)) +
  labs(x = "Date")
dia_plot
dia_plot_wc <- dia_plot +
  geom_rect(
    ymin = 0,
    ymax = 80,
    xmin = min(bp_days$date_),
    xmax = max(bp_days$date_),
    size = 0,
    fill = "darkseagreen1",
    alpha = 0.025
  ) +
  geom_rect(
    ymin = 80,
    ymax = 90,
    xmin = min(bp_days$date_),
    xmax = max(bp_days$date_),
    size = 0,
    fill = "indianred1",
    alpha = 0.005
  ) +
  geom_rect(
    ymin = 90,
    ymax = 900,
    xmin = min(bp_days$date_),
    xmax = max(bp_days$date_),
    size = 0,
    fill = "indianred3",
    alpha = 0.01
  )

dia_plot_wc
grid.arrange(sys_plot, dia_plot)
grid.arrange(sys_plot_wc, dia_plot_wc)

