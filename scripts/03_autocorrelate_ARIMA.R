library(tidyverse)
library(readxl)
library(zoo)
library(tseries)
library(forecast)
library(lmtest)
library(quantmod)


## Autocorrelations

acf(coredata(na.omit(df_differenced$infl)), lag.max = 12)
# The series shows weak to moderate positive autocorrelation at lags 2, 4, and 6, and a negative autocorrelation at lag 12

# Direct correlation between a time series and lag k, controlling for all shorter lags (1 to k−1).
pacf(coredata(na.omit(df_differenced$infl)), lag.max = 12)

# Rule of Thumb:
  # Use acf() to choose the q in MA(q) models.
  # Use pacf() to choose the p in AR(p) models.

# The ACF and PACF plots suggest that the series may be modeled as an AR(2) or AR(4) process.


##  AIC: Akaike information criterion

# Identifying the orders p and q of the ARIMA(p,1,q)-model by testing different model specifications
max.order <- 4 # We only allow a maximum of two AR- and/or MA-terms 
d <- 1 # The order of integration d is set to 1 since the SMI is non-sationary in levels but stationary in first differences

# Defining the matrix in which the values of the AICs for different model specifications are stored
arima_aic <- matrix(NA, ncol=max.order+1, nrow=max.order+1)
row.names(arima_aic) <- c(0:max.order) # Order of AR(p) in rows
colnames(arima_aic) <- c(0:max.order) # Order of MA(q) in columns

# Calculating and storing the AICs for different model specifications
for(i in 0:max.order){
  for(j in 0:max.order){
    arima_aic[i+1,j+1]<-Arima(y=df$infl, order=c(i,d,j), include.constant =  TRUE)$aic
  }
}
arima_aic
index <- which(arima_aic == min(arima_aic), arr.ind = TRUE)
ar <- as.numeric(rownames(arima_aic)[index[1]])
ma <- as.numeric(colnames(arima_aic)[index[2]])
c(ar, ma)
arima_aic[ar+1, ma+1]
## Interpretation: The optimal ARIMA-model is ARIMA(4,1,4) with an AIC of -356.6178.


# Estimating the optimal ARIMA-model and testing for significance of the coefficients
arima <- Arima(y=df$infl, order=c(ar,d,ma), include.constant = TRUE)
coeftest(arima)
## Interpretation: ar2, ar4, ma2, ma4 are significant at the 95% confidence interval.

## The negative value of the ar4-coefficient reveals that a positive change in the time series
## in the previous period leads to a negative change in the subsequent period.
## The negative value of the ma2-coefficient reveals that a positive residual in the previous
## period has a negative effect on the time series in the subsequent period.

