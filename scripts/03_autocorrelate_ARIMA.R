library(tidyverse)
library(readxl)
library(zoo)
library(tseries)
library(forecast)
library(lmtest)
library(quantmod)

# tail(df_differenced)
# view(df_differenced)

## Autocorrelations: The series shows weak to moderate positive autocorrelation at lags 4 and 6, and a negative autocorrelation at lag 12.
acf(coredata(na.omit(df_differenced$infl)), lag.max = 12)


# The inflation changes (infl) today are somewhat positively related to those 4, and 6 months ago.
# But inflation 12 months ago tends to move in the opposite direction from today’s.


# Direct correlation between a time series and lag k, controlling for all shorter lags (1 to k−1).
pacf(coredata(na.omit(df_differenced$infl)), lag.max = 12)

# Rule of Thumb:
  # Use acf() to choose the q in MA(q) models.  -> q = 4 or 6
  # Use pacf() to choose the p in AR(p) models. -> p = 4 or 6

# The ACF and PACF plots suggest that the series may be modeled as an AR(2) or AR(4) process.


##  AIC: Akaike information criterion

# Identifying the orders p and q of the ARIMA(p,1,q)-model by testing different model specifications
max.order <- 6 # We only allow a maximum of two AR- and/or MA-terms 
d <- 0 # The order of integration d is set to 0 since the we have the diff of inflation rates in the data

# Defining the matrix in which the values of the AICs for different model specifications are stored
arima_aic <- matrix(NA, ncol=max.order+1, nrow=max.order+1)
row.names(arima_aic) <- c(0:max.order) # Order of AR(p) in rows
colnames(arima_aic) <- c(0:max.order) # Order of MA(q) in columns

# Calculating and storing the AICs for different model specifications
for(i in 0:max.order){
  for(j in 0:max.order){
    arima_aic[i+1,j+1]<-Arima(y=df_differenced$infl, order=c(i,d,j), include.constant =  FALSE)$aic
  }
}
arima_aic
index <- which(arima_aic == min(arima_aic), arr.ind = TRUE)
ar <- as.numeric(rownames(arima_aic)[index[1]])
ma <- as.numeric(colnames(arima_aic)[index[2]])
c(ar, ma)
arima_aic[ar+1, ma+1]
## Interpretation: The optimal ARIMA-model is ARIMA(6,0,0) with an AIC of -398.5921. (d=0 as we have diff(infl))

# Convert to ts object from zoo and estimate the optimal ARIMA-model (incl. testing for significance of the coefficients)
infl_diff_ts <- ts(coredata(df_differenced$infl), frequency = 12)  # , start = c(2004, 7)
# Estimating the optimal ARIMA-model and testing for significance of the coefficients
arima <- Arima(y=infl_diff_ts, order=c(ar,d,ma), include.constant = FALSE)
coeftest(arima)

# Interpretation: ar4 and ar6 are significant at the 95% confidence interval.
# The positive value of the ar4- and ar6-coefficient reveals that a positive change in
# the time series in the previous period leads to a positive change in the subsequent period.


# Forecast the next 12 periods (e.g., months)

forecast_arima <- forecast(arima, h = 12)
print(forecast_arima)

forecast_arima$mean        # Point forecasts
forecast_arima$lower       # Lower bounds (80% and 95%)
forecast_arima$upper       # Upper bounds (80% and 95%)

autoplot(forecast_arima) + 
  ggtitle("ARIMA Forecast for Inflation") +
  xlab("Time") + ylab("Inflation Change")


# forecast inflation rate
# Get the last known inflation level
last_infl <- tail(na.omit(df$infl), 1)
# Calculate forecasted inflation levels
forecast_changes <- forecast_arima$mean
forecast_inflation <- cumsum(forecast_changes) + last_infl
forecast_upper <- cumsum(forecast_arima$upper[,2]) + last_infl
forecast_lower <- cumsum(forecast_arima$lower[,2]) + last_infl

# Get the last date from the indexed zoo object
library(lubridate)
last_date <- tail(index(df_differenced), 1)
# Generate 12 monthly forecast dates
forecast_dates <- seq(from = as.Date(last_date) %m+% months(1), by = "month", length.out = 12)


# forecast table
forecast_table <- data.frame(
  Date = forecast_dates,
  Forecast_Inflation = round(as.numeric(forecast_inflation), 3),
  Forecast_Change = round(as.numeric(forecast_arima$mean), 3),
  Lower_95 = round(forecast_arima$lower[,2], 3),
  Upper_95 = round(forecast_arima$upper[,2], 3)
)

print(forecast_table)

# Plot
ggplot(forecast_table, aes(x = Date, y = Forecast_Inflation)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_point(color = "blue") +
  labs(
    title = "Forecasted Inflation Levels",
    x = "Date",
    y = "Inflation Rate (%)"
  ) +
  theme_minimal()
