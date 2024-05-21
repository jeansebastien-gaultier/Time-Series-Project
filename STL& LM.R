library(ggplot2)
library(lubridate)
library(tseries)
library(forecast)
library(zoo)

df_visitors <- read.csv("C:/Users/sh558/Downloads/daily-website-visitors.csv")

head(df_visitors)

# Convert the Date column to Date type
df_visitors$Date <- as.Date(df_visitors$Date, format = "%m/%d/%Y")

# Remove commas from numeric columns and convert them to numeric type
df_visitors$Page.Loads <- as.numeric(gsub(",", "", df_visitors$Page.Loads))
df_visitors$Unique.Visits <- as.numeric(gsub(",", "", df_visitors$Unique.Visits))
df_visitors$First.Time.Visits <- as.numeric(gsub(",", "", df_visitors$First.Time.Visits))
df_visitors$Returning.Visits <- as.numeric(gsub(",", "", df_visitors$Returning.Visits))
head(df_visitors)


df_visitors$Weekend <- ifelse(df_visitors$Day.Of.Week %in% c(6, 7), 1, 0)
df_visitors$Monday <- ifelse(df_visitors$Day.Of.Week == 2, 1, 0)
df_visitors$Tuesday <- ifelse(df_visitors$Day.Of.Week == 3, 1, 0)
df_visitors$Wednesday <- ifelse(df_visitors$Day.Of.Week == 4, 1, 0)
df_visitors$Thursday <- ifelse(df_visitors$Day.Of.Week == 5, 1, 0)
df_visitors$Friday <- ifelse(df_visitors$Day.Of.Week == 6, 1, 0)
df_visitors$Saturday <- ifelse(df_visitors$Day.Of.Week == 7, 1, 0)

train_data <- df_visitors[df_visitors$Date < as.Date("2020-07-20"), ]
test_data <- df_visitors[df_visitors$Date >= as.Date("2020-07-20") & df_visitors$Date <= as.Date("2020-08-20"), ]

unique_visits_train <- ts(train_data$Unique.Visits, start = c(year(min(train_data$Date)), yday(min(train_data$Date))), frequency = 365.25)

# Seasonal Decomposition of Time Series (STL)
stl_decomp <- stl(unique_visits_train, s.window = "periodic")
plot(stl_decomp)

unique_visits_train2 <- ts(train_data$Unique.Visits, start = c(year(min(train_data$Date)), yday(min(train_data$Date))), frequency = 7)

# Seasonal Decomposition of Time Series (STL)
stl_decomp2 <- stl(unique_visits_train2, s.window = "periodic")
plot(stl_decomp2)


lm_model <- lm(Unique.Visits ~ as.numeric(Date) + Monday + Tuesday + Wednesday + Thursday + Friday + Saturday + Weekend, data = train_data)
summary(lm_model)
library(lmtest)

# Check for autocorrelation
dwtest(lm_model)



# Linear Regression predictions
lm_predictions <- predict(lm_model, newdata = test_data)

plot(lm_predictions, type = "l", main = "Linear Regression Predictions for Unique Visits", xlab = "Observation", ylab = "Unique Visits")
lines(test_data$Unique.Visits, col = "red")
legend("topright", legend = c("Predicted", "Actual"), col = c("black", "red"), lty = 1)


# Plot residuals for Linear Regression
lm_residuals <- test_data$Unique.Visits - lm_predictions
plot(lm_residuals, main = "Residuals for Linear Regression", xlab = "Observation", ylab = "Residual")





# Combine the training and test data
combined_data <- rbind(train_data, test_data)

# Fit the linear regression model
lm_model <- lm(Unique.Visits ~ ., data = train_data)
lm_predictions <- predict(lm_model, newdata = test_data)

# Plot the combined data, predictions, and actual values
plot(combined_data$Unique.Visits, type = "l", main = "Linear Regression Predictions for Unique Visits",
     xlab = "Observation", ylab = "Unique Visits")
lines(c(train_data$Unique.Visits, lm_predictions), col = "blue")
lines(test_data$Unique.Visits, col = "red")

# Add legend
legend("topright", legend = c("Combined Data", "Predicted", "Actual"),
       col = c("black", "blue", "red"), lty = 1)



# Combine the training and test data
combined_data <- rbind(train_data, test_data)

# Fit the linear regression model
lm_model <- lm(Unique.Visits ~ ., data = train_data)
lm_predictions <- predict(lm_model, newdata = test_data)

# Plot the train data
plot(train_data$Unique.Visits, type = "l", main = "Linear Regression Predictions for Unique Visits",
     xlab = "Observation", ylab = "Unique Visits", col = "black", ylim = range(combined_data$Unique.Visits))

# Plot the test data
lines(seq_along(test_data$Unique.Visits) + nrow(train_data), test_data$Unique.Visits, col = "red")

# Plot the predicted test data
lines(seq_along(lm_predictions) + nrow(train_data), lm_predictions, col = "blue")

# Add legend
legend("topright", legend = c("Train Data", "Test Data", "Predicted Test Data"),
       col = c("black", "red", "blue"), lty = 1)














lm_model1 <- lm(Unique.Visits ~ as.numeric(Date) + Monday + Tuesday + Wednesday + Thursday + Friday + Saturday + Weekend, data = train_data)
summary(lm_model1)

# Calculate evaluation metrics for the first model
lm_predictions1 <- predict(lm_model1, newdata = test_data)
mae1 <- mean(abs(test_data$Unique.Visits - lm_predictions1))
mse1 <- mean((test_data$Unique.Visits - lm_predictions1)^2)
rmse1 <- sqrt(mse1)
mape1 <- mean(abs((test_data$Unique.Visits - lm_predictions1) / test_data$Unique.Visits)) * 100

cat("Evaluation Metrics for Model 1:\n")
cat("Mean Absolute Error (MAE):", mae1, "\n")
cat("Mean Squared Error (MSE):", mse1, "\n")
cat("Root Mean Squared Error (RMSE):", rmse1, "\n")
cat("Mean Absolute Percentage Error (MAPE):", mape1, "%\n\n")
install.packages('Metrics')
library(Metrics)
lm_predictions1 <- predict(lm_model1, newdata = test_data)
mae1 <- mae(test_data$Unique.Visits, lm_predictions1)
mse1 <- mse(test_data$Unique.Visits, lm_predictions1)
rmse1 <- rmse(test_data$Unique.Visits, lm_predictions1)
mape1 <- mape(test_data$Unique.Visits, lm_predictions1)
cat("Evaluation Metrics for Model 1:\n")
cat("Mean Absolute Error (MAE):", mae1, "\n")
cat("Mean Squared Error (MSE):", mse1, "\n")
cat("Root Mean Squared Error (RMSE):", rmse1, "\n")
cat("Mean Absolute Percentage Error (MAPE):", mape1, "%\n\n")





