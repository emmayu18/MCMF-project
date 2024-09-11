# load packages----
library(tidyverse)
library(patchwork)
library(forecast)
library(xts)
library(tseries)
library(rugarch)
library(TTR)

# load data ----
## load data 
load("data/wrangle/time_series.rda")

## convert data to xts
data_xts <- xts(time_data[,-1], order.by=time_data$days)

# plot code ----
## not including communities
time_data_plots <- time_data %>%
  select(days, total_count, cat_academics, cat_comm, cat_leisure, cat_prof,
         face_to_face, online, pay_TRUE, price_free, price_less_50, 
         price_more_50, priority_TRUE)
var_list = names(time_data_plots)
plots <- list()
ylabs <- c("Total Count", "Academics Count", "Community Service\nCount",
           "Leisure and Arts\nCount", "Professional Skill\nBuilding Count",
           "Face to Face Count", "Online Count", "Paid Count", "Free Count",
           "Price < $50 Count", "Price > $50 Count", "Priority Region Count")
for (i in 2:13) {
  plots[[i-1]] <- ggplot(time_data_plots, aes_string(x=var_list[1], 
                                                     y=var_list[i])) +
    geom_line() +
    labs(y = ylabs[i-1], x = "Years") +
    theme_minimal()
}

## communities
time_data_com_plots <- time_data %>%
  select(-c(total_count, cat_academics, cat_comm, cat_leisure, cat_prof,
            face_to_face, online, pay_TRUE, price_free, price_less_50, 
            price_more_50, priority_TRUE)) %>%
  janitor::clean_names()
var_list = names(time_data_com_plots)
com_plots1 <- list()
for (i in 2:13) {
  com_plots1[[i-1]] <- ggplot(time_data_com_plots, aes_string(x=var_list[1], 
                                                             y=var_list[i])) +
    geom_line() +
    labs(y = names(time_data_com_plots)[i]) +
    theme_minimal()
}
com_plots2 <- list()
for (i in 14:25) {
  com_plots2[[i-13]] <- ggplot(time_data_com_plots, aes_string(x=var_list[1], 
                                                             y=var_list[i])) +
    geom_line() +
    labs(y = names(time_data_com_plots)[i]) +
    theme_minimal()
}
com_plots3 <- list()
for (i in 26:37) {
  com_plots3[[i-25]] <- ggplot(time_data_com_plots, aes_string(x=var_list[1], 
                                                             y=var_list[i])) +
    geom_line() +
    labs(y = names(time_data_com_plots)[i]) +
    theme_minimal()
}
com_plots4 <- list()
for (i in 38:49) {
  com_plots4[[i-37]] <- ggplot(time_data_com_plots, aes_string(x=var_list[1], 
                                                             y=var_list[i])) +
    geom_line() +
    labs(y = names(time_data_com_plots)[i]) +
    theme_minimal()
}
com_plots5 <- list()
for (i in 50:61) {
  com_plots5[[i-49]] <- ggplot(time_data_com_plots, aes_string(x=var_list[1], 
                                                             y=var_list[i])) +
    geom_line() +
    labs(y = names(time_data_com_plots)[i]) +
    theme_minimal()
}
com_plots6 <- list()
for (i in 62:73) {
  com_plots6[[i-61]] <- ggplot(time_data_com_plots, aes_string(x=var_list[1], 
                                                             y=var_list[i])) +
    geom_line() +
    labs(y = names(time_data_com_plots)[i]) +
    theme_minimal()
}
com_plots7 <- list()
for (i in 73:77) {
  com_plots7[[i-72]] <- ggplot(time_data_com_plots, aes_string(x=var_list[1], 
                                                             y=var_list[i])) +
    geom_line() +
    labs(y = names(time_data_com_plots)[i]) +
    theme_minimal()
}

# show plots (deciding which variables to forecast)----
Reduce(`+`, plots) + plot_annotation(
  title = "MCMF Program Count Time Series Data of Different Variables",
  theme = theme(plot.title = element_text(face = "bold", 
                                          size = 16, 
                                          hjust = 0.5)))
ggsave("model/plots/eda5.png", width = 10, height = 6, units = "in")
Reduce(`+`, com_plots1)
Reduce(`+`, com_plots2)
Reduce(`+`, com_plots3)
Reduce(`+`, com_plots4)
Reduce(`+`, com_plots5)
Reduce(`+`, com_plots6)
Reduce(`+`, com_plots7)
  # VARIABLES: total_count, cat_prof, online

## total_count
ggplot(data = time_data, mapping = aes(x = days, y = total_count)) +
  geom_line() +
  scale_x_date(date_breaks = "6 months",
               date_minor_breaks = "2 months") +
  labs(x = "Date", y = "Count")
  # there are dips during breaks (right before school starts, holiday season,
  # spring break, right when school ends)

## cat_prof
ggplot(data = time_data, mapping = aes(x = days, y = cat_prof)) +
  geom_line() +
  scale_x_date(date_breaks = "6 months",
               date_minor_breaks = "2 months") +
  labs(x = "Date", y = "Count")

## online
ggplot(data = time_data, mapping = aes(x = days, y = online)) +
  geom_line() +
  scale_x_date(date_breaks = "6 months",
               date_minor_breaks = "2 months") +
  labs(x = "Date", y = "Count")

# total_count ----
## data
### ts data
total_count_ts <- ts(data_xts$total_count, 
                     start = c(2020, as.numeric(format(as.Date("2020-01-01"), "%j"))),
                     frequency=7)

### split train and test
train_count <- head(total_count_ts, round(length(total_count_ts) * 0.6))
test_count <- tail(total_count_ts, length(total_count_ts) - length(train_count))

png(file = "model/plots/training_testing.png", width = 9, height = 6.5, 
    units = "in", res = 300)
plot(train_count, xlim = c(2020, 2180), 
     main = "MCMF Program Count Time Series Data", ylab = "Count", col = "blue",
     bty = "n", xaxt = "n", yaxt = "n", cex.main = 2)
box("plot", bty = "l", lwd = 2)
axis(side = 1, lwd = 0, lwd.ticks = 2, at = seq(2020, 2175, length.out = 7),
     labels = c("2020.0", "2020.5", "2021.0", "2021.5",
                "2022.0", "2022.5", "2023.0"))
axis(side = 2, lwd = 0, lwd.ticks = 2, las = 2)
lines(test_count, col = "red")
legend(2020.0, 2000, legend = c("Training Data", "Testing Data"),
       col=c("blue", "red"), lty=1, cex=1.5)
dev.off()

## ARFIMA modeling
### decomposition analysis
#### decomposition
decomp_stl <- stl(total_count_ts[, 1], 
                  s.window = "periodic")
plot(decomp_stl)

### ARIMA parameter selection
#### stationary testing
adf.test(train_count)
  # p-value = 0.01 < 0.05: data is stationary, no need for differencing

#### ACF plot
x = ggAcf(train_count) + 
  ggtitle("Autocorrelation Function (ACF) Plot") + 
  theme_minimal()
  # no lag cutoff, will use ARFIMA

#### PACF plot
y = ggPacf(train_count) +
  ggtitle("Partial Autocorrelation Function (PACF) Plot") +
  theme_minimal()
  # p = 2 

x + y
ggsave(filename = "model/plots/acf_pacf.png", width = 9, height = 4, units = "in")

### ARFIMA fitting 
total_count_arfima <- arfima(train_count)
fit_arfima = fitted.values(total_count_arfima)
checkresiduals(total_count_arfima)

### forecasting
forecast_arfima = forecast(total_count_arfima,level=95, h=468)
png(file = "model/plots/ARFIMA_forecast.png", width = 9, height = 6.5, 
    units = "in", res = 300)
plot(forecast_arfima, main = "MCMF Program Count Forecast", xlab = "Time",
     ylab = "Count", bty = "n", xaxt = "n", yaxt = "n", cex.main = 2)
box("plot", bty = "l", lwd = 2)
axis(side = 1, lwd = 0, lwd.ticks = 2, at = seq(2020, 2175, length.out = 7),
     labels = c("2020.0", "2020.5", "2021.0", "2021.5", "2022.0", "2022.5", "2023.0"))
axis(side = 2, lwd = 0, lwd.ticks = 2, las = 2)
dev.off()

png(file = "model/plots/ARFIMA_forecast_test.png", width = 9, height = 6.5, 
    units = "in", res = 300)
plot(forecast_arfima, main = "MCMF Program Count Forecast with Testing Data", 
     xlab = "Time", ylab = "Count", bty = "n", xaxt = "n", yaxt = "n",
     cex.main = 2)
box("plot", bty = "l", lwd = 2)
axis(side = 1, lwd = 0, lwd.ticks = 2, at = seq(2020, 2175, length.out = 7),
     labels = c("2020.0", "2020.5", "2021.0", "2021.5", "2022.0", "2022.5", "2023.0"))
axis(side = 2, lwd = 0, lwd.ticks = 2, las = 2)
lines(test_count, col = "purple")
legend(2020.0, 2300, legend = c("ARFIMA Forecast", "Testing Data"),
       col = c("#2297e6", "purple"), lty = 1, lwd = c(2.25,1), cex = 1.5)
dev.off()

### testing
forecast_values <- forecast_arfima$mean

errors <- test_count - forecast_values
mae <- mean(abs(errors))
rmse <- sqrt(mean(errors^2))
mpe <- mean(errors / test_count * 100)
mape <- mean(abs(errors / test_count)) * 100

perf_met <- tibble(Metrics = c("Mean Absolute Error (MAE)",
                               "Root Mean Squared Error (RMSE)", 
                               "Mean Percentage Error (MPE)", 
                               "Mean Average Percentage Error (MAPE)"),
                   Values = c(mae, rmse, mpe, mape)) 
save(perf_met, file = "model/plots/performance.rds")

## GARCH modeling
### fit GARCH model and find volatility
total_count_res <- total_count_arfima$residuals
garch_spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = "std" 
)
garch_fit <- ugarchfit(spec = garch_spec, data = total_count_res)
garch_vol <- as.double(sigma(garch_fit))
garch_forecast_vol <- sigma(ugarchforecast(garch_fit, n.ahead = 468))

### Calculate GARCH intervals
forecast_upper_bound <- forecast_arfima$mean + 1.96 * garch_forecast_vol
forecast_lower_bound <- forecast_arfima$mean - 1.96 * garch_forecast_vol
upper_bound <- fit_arfima + 1.96 * garch_vol
lower_bound <- fit_arfima - 1.96 * garch_vol

### plot GARCH
png(file = "model/plots/GARCH_estimate.png", width = 9, height = 6.5,
    units = "in", res = 300)
plot(train_count, main = "GARCH Estimated Volatility", ylab = "Count", lwd = 1.5,
     bty = "n", xaxt = "n", yaxt = "n", cex.main = 2)
box("plot", bty = "l", lwd = 2)
axis(side = 1, lwd = 0, lwd.ticks = 2, at = seq(2020, 2175, length.out = 7),
     labels = c("2020.0", "2020.5", "2021.0", "2021.5", "2022.0", "2022.5", "2023.0"))
axis(side = 2, lwd = 0, lwd.ticks = 2, las = 2)
lines(lower_bound,col='red')
lines(upper_bound,col='blue')
legend(2020.0, 2100, legend = c("Upper Bound", "Lower Bound", "Original Data"),
       col=c("blue", "red", "black"), lty = 1, lwd = c(1,1,1.5), cex = 1.5)
dev.off()

### plot ARFIMA-GARCH forecast
png(file = "model/plots/ARFIMA_GARCH_forecast.png", width = 9, height = 6.5,
    units = "in", res = 300)
plot(forecast_arfima, main = "ARFIMA-GARCH Forecasting", xlab = "Time", 
     ylab = "Count", bty = "n", xaxt = "n", yaxt = "n", cex.main = 2)
box("plot", bty = "l", lwd = 2)
axis(side = 1, lwd = 0, lwd.ticks = 2, at = seq(2020, 2175, length.out = 7),
     labels = c("2020.0", "2020.5", "2021.0", "2021.5", "2022.0", "2022.5", "2023.0"))
axis(side = 2, lwd = 0, lwd.ticks = 2, las = 2)
lines(forecast_lower_bound,col='red')
lines(forecast_upper_bound,col='blue')
legend(2020, 2425, legend = c("ARFIMA Forecast", "GARCH Upper Bound", 
                                "GARCH Lower Bound"), 
       col = c("#2297e6", "blue", "red"), lty = 1, lwd = c(2.25, 1, 1),
       cex = 1.5)
dev.off()

### ARFIMA-GARCH forecast with testing data
png(file = "model/plots/ARFIMA_GARCH_forecast_test.png", width = 9, height = 6.5,
    units = "in", res = 300)
plot(forecast_arfima, main = "ARFIMA-GARCH Forecasting with Testing Data", 
     xlab = "Time", ylab = "Count", bty = "n", xaxt = "n", yaxt = "n",
     cex.main = 2)
box("plot", bty = "l", lwd = 2)
axis(side = 1, lwd = 0, lwd.ticks = 2, at = seq(2020, 2175, length.out = 7),
     labels = c("2020.0", "2020.5", "2021.0", "2021.5", "2022.0", "2022.5", "2023.0"))
axis(side = 2, lwd = 0, lwd.ticks = 2, las = 2)
lines(forecast_lower_bound,col='red')
lines(forecast_upper_bound,col='blue')
lines(test_count, col = "purple")
legend(2020, 2300, legend = c("ARFIMA Forecast", "GARCH Upper Bound", 
                                "GARCH Lower Bound", "Testing Data"), 
       col = c("#2297e6", "blue", "red", "purple"), lty = 1, 
       lwd = c(2.25,1,1,1), cex = 1.5)
dev.off()

# cat_prof----
## time series data
cat_prof_ts <- ts(data_xts$cat_prof, 
                  start = c(2020, as.numeric(format(as.Date("2020-01-01"), "%j"))),
                  frequency=365)
plot(cat_prof_ts)

## split train and test
train_prof <- head(cat_prof_ts, round(length(cat_prof_ts) * 0.6))
test_prof <- tail(cat_prof_ts, length(cat_prof_ts) - length(train_prof))

## decomposition analysis
### decomposition
cat_prof_decomp <- stl(cat_prof_ts[, 1], 
                       s.window = "periodic")
plot(cat_prof_decomp)
### de-trending
cat_stl_adj <- train_prof - cat_prof_decomp$time.series[, "trend"]
plot(cat_stl_adj)

## stationary testing
adf.test(cat_stl_adj)
  # p-value = 0.01 < 0.05: data is stationary

# ## differencing
# cat_prof_diff1 <- diff(cat_stl_adj, differences = 1)
# adf.test(cat_prof_diff1)
#   # p-value = 0.01 < 0.05: now stationary
#   # d = 1

## ACF plot
# ggAcf(cat_prof_diff1)
ggAcf(cat_stl_adj, xlim = c(0,100))
  # q = 40 (max should be 5)

## PACF plot
# ggPacf(cat_prof_diff1)
ggPacf(cat_stl_adj, xlim = c(0,100))
  # p = 2 

## ARIMA fitting
cat_prof_fit <- Arima(train_prof, order = c(2,0,5))
checkresiduals(cat_prof_fit) # what does this mean?

## forecasting
cat_prof_forecast <- forecast(cat_prof_fit, h = 450)
autoplot(cat_prof_forecast) + autolayer(test_prof)

## testing
prof_test <- Arima(test_prof, model=cat_prof_fit)
accuracy(prof_test)

# online----
## time series data
online_ts <- ts(data_xts$online, 
                start = c(2020, as.numeric(format(as.Date("2020-01-01"), "%j"))),
                frequency=365)
plot(online_ts)

## split train and test
train_online <- head(online_ts, round(length(online_ts) * 0.6))
test_online <- tail(online_ts, length(online_ts) - length(train_online))

## decomposition analysis
online_SMA <- SMA(train_online, n = 20)
plot.ts(online_SMA)
### decomposition
online_decomp <- stl(online_ts[, 1], 
                     s.window = "periodic")
plot(online_decomp)
### de-trending
online_stl_adj <- online_ts - online_decomp$time.series[, "trend"]
autoplot(online_stl_adj)

## stationary testing
online_SMA <- replace(online_SMA, is.na(online_SMA), 0)
adf.test(online_SMA)
  # p-value = 0.6219 > 0.05: data is not stationary
adf.test(online_stl_adj)
  # p-value = 0.04 < 0.05: stationary

## differencing
online_diff1 <- diff(online_stl_adj, differences = 1)
adf.test(online_diff1)
  # p-value = 0.01 < 0.05: now stationary
  # d = 1
plot.ts(online_diff1)

## ACF plot
ggAcf(online_diff1, xlim = c(0,100))
  # q = 6 (max should be 5)
ggAcf(online_stl_adj, xlim = c(0,100))
  # q = 60 (max should be 5)

## PACF plot
ggPacf(online_diff1, xlim = c(0,100))
  # p = 3
ggPacf(online_stl_adj, xlim = c(0,100))
  # p = 2 

## ARIMA fitting
online_fit <- Arima(train_online, order = c(3, 1, 5))
# checkresiduals(online_fit) # what does this mean?
# online_fit <- Arima(train_online, order = c(5,0,2))
online_fit <- auto.arima(train_online)
checkresiduals(online_fit)

## forecasting
online_forecast <- forecast(online_fit, h = 450)
autoplot(online_forecast) + autolayer(test_online)

# testing
online_test <- Arima(test_online, model=online_fit)
accuracy(online_test)
