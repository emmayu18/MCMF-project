# load packages----
library(tidyverse)
library(patchwork)
library(forecast)
library(xts)
library(tseries)
library(TTR)
library(ggfortify)

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
for (i in 2:13) {
  plots[[i-1]] <- ggplot(time_data_plots, aes_string(x=var_list[1], 
                                                     y=var_list[i])) +
    geom_line() +
    labs(y = names(time_data_plots)[i]) +
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
Reduce(`+`, plots)
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
## ts data
total_count_ts <- ts(data_xts$total_count, 
                     start = c(2020, as.numeric(format(as.Date("2020-01-01"), "%j"))),
                     frequency=365)

## split train and test
train_count <- head(total_count_ts, round(length(total_count_ts) * 0.6))
test_count <- tail(total_count_ts, length(total_count_ts) - length(train_count))

## decomposition analysis
### decomposition
decomp_stl <- stl(total_count_ts[, 1], 
                  s.window = "periodic")
plot(decomp_stl)
### de-trending
stl_adj <- total_count_ts - decomp_stl$time.series[, "trend"]
autoplot(stl_adj)

## stationary testing
adf.test(stl_adj)
  # p-value < 0.05: data is stationary, no need for differencing

## ACF plot
ggAcf(stl_adj, xlim = c(0,100))
  # q = 30 (max = 5)

## PACF plot
ggPacf(stl_adj, xlim = c(0,100))
  # p = 2 

## ARIMA modeling
total_count_fit <- Arima(train_count, c(2,0,5))
checkresiduals(total_count_fit)

## forecasting
total_count_forecast <- forecast(total_count_arima, level = 95, h = 465) 
plot(total_count_forecast)
lines(test_count, col = "purple")
autoplot(total_count_forecast) + autolayer(test_count)


## testing
total_count_test <- Arima(test_count, model = total_count_fit)
accuracy(total_count_test)

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
