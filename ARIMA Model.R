library(readr)
library(ggplot2)
library(forecast)
library(tidyverse)
library(lubridate)
library(tseries)
library(zoo)

data <- read_csv("daily-website-visitors.csv")

## Data Preprocessing

data$Date <- as.Date(data$Date, format="%m/%d/%Y")

head(data)

colSums(is.na(data))

data <- data %>%
  mutate(across(c(Page.Loads, Unique.Visits, First.Time.Visits, Returning.Visits), 
                ~ as.numeric(gsub(",", "", .))))

str(data)

ggplot(data, aes(x = Date, y = Unique.Visits)) +
  geom_line() +
  labs(title = "Unique Visits Over Time", x = "Date", y = "Unique Visits") +
  theme_minimal()

adf_test <- adf.test(data$Unique.Visits, alternative = "stationary")
adf_test

## The ADF test results suggest that the unique visits time series is stationary after differencing (since an ARIMA model with a differencing term of 1 was used). This stationarity is a crucial assumption for applying ARIMA modeling effectively.

# Plot ACF and PACF
par(mfrow = c(1, 2))
acf(data$Unique.Visits, main = "ACF - Unique Visits", lag.max = 40)
## The ACF plot shows the correlation between the time series and its own lagged values over different lag periods.
##### Significant autocorrelation at various lags, indicating that past values have a correlation with current values. This helps in identifying the appropriate number of AR (Auto-Regressive) terms.
##### Peaks gradually decrease, suggesting the presence of a trend or seasonal pattern.
pacf(data$Unique.Visits, main = "PACF - Unique Visits", lag.max = 40)
## The PACF plot shows the correlation between the time series and its lagged values, after removing the effects of intermediate lags.
##### Significant spikes at the initial lags, which indicates the number of AR terms required.
##### Rapid drop-off after a few lags, suggesting that additional lag terms might not significantly improve the model.
par(mfrow = c(1, 1))

### Feature Engineering

# Create lagged features
data$Lag1 <- lag(data$Unique.Visits, 1)
data$Lag7 <- lag(data$Unique.Visits, 7)

# Create rolling average features using the zoo package
data$RollingMean7 <- rollmean(data$Unique.Visits, 7, fill = NA, align = 'right')

# Remove NA values caused by lagging
data <- na.omit(data)

# Check the head of the updated dataframe
head(data)

### ARIMA Model

train_data <- subset(data, Date >= as.Date("2014-09-14") & Date <= as.Date("2020-07-17"))
valid_data <- subset(data, Date >= as.Date("2020-07-19") & Date <= as.Date("2020-08-20"))

ts_train <- ts(train_data$`Unique.Visits`, start=c(2014, 9, 14), frequency=365)
ts_valid <- ts(valid_data$`Unique.Visits`, start=c(2020, 7, 19), frequency=365)

best_arima_model <- auto.arima(ts_train)
summary(best_arima_model)

forecasted_values <- forecast(best_arima_model, h=length(ts_valid))

# Convert forecasted values to a time series object
forecasted_ts <- ts(forecasted_values$mean, start=c(2020, 7, 19), frequency=365)

autoplot(ts_valid, series="Actual") +
  autolayer(forecasted_ts, series="Forecasted") +
  labs(title="Actual vs Forecasted Unique Visitors",
       x="Date",
       y="Unique Visitors") +
  theme_minimal() +
  scale_color_manual(values=c("Actual"="blue", "Forecasted"="red"))

# Calculate accuracy metrics
accuracy_metrics <- accuracy(forecasted_ts, ts_valid)
print(accuracy_metrics)

combined_data <- data.frame(
  Date = c(train_data$Date, valid_data$Date),
  Unique_Visits = c(train_data$`Unique.Visits`, valid_data$`Unique.Visits`),
  Type = c(rep("Actual", nrow(train_data)), rep("Actual", nrow(valid_data)))
)

forecasted_data <- data.frame(
  Date = valid_data$Date,
  Unique_Visits = as.numeric(forecasted_ts),
  Type = rep("Forecasted", length(forecasted_ts))
)

combined_data <- rbind(combined_data, forecasted_data)

ggplot(combined_data, aes(x=Date, y=Unique_Visits, color=Type)) +
  geom_line() +
  labs(title="Actual vs Forecasted Unique Visitors",
       x="Date",
       y="Unique Visitors") +
  theme_minimal() +
  scale_color_manual(values=c("Actual"="black", "Forecasted"="red"))





