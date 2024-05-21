library(dplyr)
library(lubridate) 
library(forecast)
library(ggplot2)

data <- read.csv("/Users/jean-sebastiengaultier/Desktop/UChicago/Q3/Time Series/Final Project/Time-Series-Project/daily-website-visitors.csv")

data$Date <- mdy(data$Date)

columns_to_convert <- c("Page.Loads", "Unique.Visits", "First.Time.Visits", "Returning.Visits")
convert_to_integer <- function(x) {
  as.integer(gsub(",", "", x))
}
data[columns_to_convert] <- lapply(data[columns_to_convert], convert_to_integer)
str(data)

data <- data %>%
  mutate(Total.Visits = First.Time.Visits + Returning.Visits)


# Filter the training and validation datasets
train_set <- subset(data, Date >= as.Date("2014-09-14") & Date <= as.Date("2020-07-19"))
test_set <- subset(data, Date >= as.Date("2020-07-20") & Date <= as.Date("2020-08-20"))


#train_set <- subset(data, Date >= as.Date("2014-09-14") & Date <= as.Date("2019-12-31"))
#test_set <- subset(data, Date >= as.Date("2020-01-01") & Date <= as.Date("2020-12-31"))

# Confirm the first and last dates in the training and test sets
train_start_date <- min(train_set$Date)
train_end_date <- max(train_set$Date)
test_start_date <- min(test_set$Date)
test_end_date <- max(test_set$Date)

# Create a time series object for the training set
train_ts <- ts(train_set$Total.Visits, frequency = 365, start = c(year(train_start_date), yday(train_start_date)))
# Fit for YEARLY SEASONALITY
fit <- auto.arima(train_ts, stepwise = TRUE, approximation = FALSE, trace = TRUE, seasonal = TRUE) 
summary(fit)

forecast_period <- nrow(test_set)
fc <- forecast(fit, h = forecast_period)
plot(fc)




train_ts <- ts(train_set$Total.Visits, frequency = 365, start = c(year(min(train_set$Date)), as.numeric(format(min(train_set$Date), "%j"))))
test_ts <- ts(test_set$Total.Visits, frequency = 365, start = c(year(min(test_set$Date)), as.numeric(format(min(test_set$Date), "%j"))))

# Fit the ARIMA model to the training data
fit <- auto.arima(train_ts, stepwise = TRUE, approximation = FALSE, trace = TRUE, seasonal = TRUE)

forecast_values <- forecast(fit, h = 31)  # Assuming 31 days in the next month

plot(forecast_values$mean, type = "l", col = "blue", xlab = "Date", ylab = "Visits", main = "Forecast vs Actual", ylim = range(c(test_ts, forecast_values$mean)))
lines(test_ts, col = "red")
legend("topleft", legend = c("Forecast", "Actual"), col = c("blue", "red"), lty = 1)

ts_test <- ts(test_set$Total.Visits, frequency = 365, start = c(year(test_start_date), yday(test_start_date)))
# Plot forecast vs actual data
autoplot(fc) +
  autolayer(ts_test, series = "Test Data") +
  xlab("Date") + ylab("Total Visits") +
  ggtitle("Forecast vs Actual Data") +
  theme_minimal()




# Create a time series object for the training set WEEKLY
train_ts_week <- ts(train_set$Total.Visits, frequency = 7, start = c(year(train_start_date), yday(train_start_date)))
# Fit for WEEKLY SEASONALITY
fit_week <- auto.arima(train_ts_week, stepwise = TRUE, approximation = FALSE, trace = TRUE, seasonal = TRUE) 



train_ts <- ts(train_set$Total.Visits, frequency = 7, start = c(year(min(train_set$Date)), as.numeric(format(min(train_set$Date), "%j"))))
test_ts <- ts(test_set$Total.Visits, frequency = 7, start = c(year(min(test_set$Date)), as.numeric(format(min(test_set$Date), "%j"))))

# Fit the ARIMA model to the training data
fit <- auto.arima(train_ts, stepwise = TRUE, approximation = FALSE, trace = TRUE, seasonal = TRUE)

forecast_values <- forecast(fit, h = 31)  # Assuming 31 days in the next month

# Plotting the forecasted values against a subset of the actual values
plot(forecast_values$mean, type = "l", col = "blue", xlab = "Date", ylab = "Visits", main = "Forecast vs Actual", ylim = range(c(test_ts, forecast_values$mean)))
# Plot only a subset of actual values
lines(test_ts[1:31], col = "red")
legend("topleft", legend = c("Forecast", "Actual"), col = c("blue", "red"), lty = 1)

  
#auto.arima(train_ts_week, seasonal = TRUE, stepwise = FALSE, approximation = FALSE, 
#                  D = 1, max.P = 2, max.Q = 2, max.D = 1)

summary(fit_week)
forecast_period <- nrow(test_set)
fc_week <- forecast(fit_week, h = forecast_period)
plot(fc_week)

ts_test_week <- ts(test_set$Total.Visits, frequency = 7, start = c(year(test_start_date), yday(test_start_date)))
# Plot forecast vs actual data
autoplot(fc_week) +
  autolayer(ts_test_week, series = "Test Data") +
  xlab("Date") + ylab("Total Visits") +
  ggtitle("Forecast vs Actual Data") +
  theme_minimal()



train_set <- subset(data, Date >= as.Date("2014-09-14") & Date <= as.Date("2020-07-19"))
test_set <- subset(data, Date >= as.Date("2020-07-20") & Date <= as.Date("2020-08-20"))

# Create time series objects for training and testing sets
train_ts <- ts(train_set$Total.Visits, frequency = 7, start = c(year(min(train_set$Date)), as.numeric(format(min(train_set$Date), "%j"))))
test_ts <- ts(test_set$Total.Visits, frequency = 7, start = c(year(min(test_set$Date)), as.numeric(format(min(test_set$Date), "%j"))))

# Fit the ARIMA model to the training data with weekly seasonality
fit <- auto.arima(train_ts, stepwise = TRUE, approximation = FALSE, trace = TRUE, seasonal = TRUE)

forecast_values <- forecast(fit, h = 31)  # Assuming 31 days in the next month

# Indexing the predicted and actual data from 1 to 31
index <- 1:31
predicted_values <- forecast_values$mean[1:31]
actual_values <- test_ts[1:31]

# Plotting the forecasted values against the actual values with indexed x-axis
plot(index, predicted_values, type = "l", col = "blue", xlab = "Day", ylab = "Visits", main = "Weekly Seasonal Forecast vs Actual", ylim = range(c(predicted_values, actual_values)))
lines(index, actual_values, col = "red")
legend("topleft", legend = c("Forecast", "Actual"), col = c("blue", "red"), lty = 1)

# Adjusting margins to fit the plot nicely within the screen
par(mar = c(5, 4, 4, 2) + 0.1)
summary(fit)

