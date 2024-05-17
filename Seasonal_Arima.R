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

train_set <- subset(data, Date >= as.Date("2014-09-14") & Date <= as.Date("2019-12-31"))
test_set <- subset(data, Date >= as.Date("2020-01-01") & Date <= as.Date("2020-12-31"))

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


