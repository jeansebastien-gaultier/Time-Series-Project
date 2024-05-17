df <- read.csv("/Users/jean-sebastiengaultier/Desktop/UChicago/Q3/Time Series/Final Project/daily-website-visitors.csv")
df
df$Date <- as.Date(df$Date)

# Remove commas and convert 'Unique.Visits' to integers
df$Unique.Visits <- as.integer(gsub(",", "", df$Unique.Visits))
acf(df$Unique.Visits, main = "Autocorrelation Function (ACF)", lag.max = 30)