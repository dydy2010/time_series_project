library(tidyverse)
library(readxl)
library(zoo)
library(tseries)
library(forecast)
library(lmtest)
library(quantmod)


## Check stationarity

adf.test(na.omit(df_ts$ir_above))
adf.test(na.omit(df_ts$special))
adf.test(na.omit(df_ts$infl))

# All three series are not stationary.
# Take first differences of the series
df_differenced <- diff(df_ts)
# View(df_differenced)

# Check stationarity again
adf.test(na.omit(df_differenced$ir_above))
adf.test(na.omit(df_differenced$special))
adf.test(na.omit(df_differenced$infl))
# view(df_differenced)
# Two series are stationary now. but ir_above has hardly any info left (mostly zeros).
# Go on with special and infl.

# Check for correlation between the two stationary series
cor(df_differenced$special, df_differenced$infl, use = "pairwise.complete.obs")
# Very weak positive correlation.

# Run the linear regression
lin_reg <- lm(infl ~ special, data = df_differenced)
summary(lin_reg)
# No significant coefficients, R squared is very low.

# Calculate and plot the regression residuals
resid <- lin_reg$residuals
plot(y=resid, x=as.Date(time(df_differenced)), ylab="Residuals", xlab="Year", type="l", main="Regression Residuals") +
  grid()


## Alternative approaches:
## 1. Check relations only since e.g. 2020-01-01
## 2. lead-lag relation: e.g. infl(t) = a + b * special(t-1) + e(t)
## 3. Treat SNB actions as events, e.g. 2020-07-01 (reduce from 0.5 to 0.0) or 2022-10-01 (increase from 0.25 to 1.0)


# 1. Check relations only since e.g. 2020-01-01

df_differenced_2020 <- df_differenced[index(df_differenced) >= as.Date("2020-01-01")]
# view(df_differenced_2020)
# Run the linear regression
lin_reg_2020 <- lm(infl ~ special, data = df_differenced_2020)
summary(lin_reg_2020)
# Again no significant coefficients, R squared is very low.


# 2. lead-lag relation: e.g. infl(t) = a + b * special(t-1) + e(t)

# Create lagged variables
df_differenced$special_lag1 <- stats::lag(df_differenced$special, k = 1)
df_differenced$special_lag2 <- stats::lag(df_differenced$special, k = 2)
df_differenced$special_lag3 <- stats::lag(df_differenced$special, k = 3)
df_differenced$special_lag4 <- stats::lag(df_differenced$special, k = 4)
df_differenced$special_lag5 <- stats::lag(df_differenced$special, k = 5)
df_differenced$special_lag6 <- stats::lag(df_differenced$special, k = 6)
df_differenced$special_lag7 <- stats::lag(df_differenced$special, k = 7)
df_differenced$special_lag8 <- stats::lag(df_differenced$special, k = 8)
df_differenced$special_lag9 <- stats::lag(df_differenced$special, k = 9)
df_differenced$special_lag10 <- stats::lag(df_differenced$special, k = 10)
df_differenced$special_lag11 <- stats::lag(df_differenced$special, k = 11)
df_differenced$special_lag12 <- stats::lag(df_differenced$special, k = 12)
view(df_differenced)

# Fit the linear model, removing rows with NA due to lagging
lin_reg_lagged <- lm(infl ~ special_lag1 + special_lag2 + special_lag3 + special_lag4+special_lag5+special_lag6+special_lag7+special_lag8+special_lag9+special_lag10+special_lag11+special_lag12, data = na.omit(df_differenced))
summary(lin_reg_lagged)
# Again no significant coefficients, R squared is very low.
# But special_lag3 is close with p-value of 0.06. month 11 is 0.0387
# Interpretation: A change in special 11 months ago has a positive and significant effect on inflation today.
"""Lagged Linear Regression
	•	model shows that special_lag11 (11 months ago) has a statistically significant and positive effect on current inflation.
	•	This suggests a delayed association: changes in the special variable may be linked to inflation nearly a year later.
	•	However, the overall model fit is modest (Adjusted R² ~8.4%), and no other lags were significant.

Overall Interpretation: There is evidence of a specific event delayed effect, 
but it is isolated and doesn’t indicate strong overall predictive power."""



## 3. Treat SNB actions as events

df_differenced$event_2020_01 <- ifelse(index(df_differenced) >= as.Date("2020-07-01"), 1, 0)
df_differenced$event_2022_10 <- ifelse(index(df_differenced) >= as.Date("2022-10-01"), 1, 0)
# view(df_differenced)

lin_reg_events <- lm(infl ~ event_2020_01 + event_2022_10, data = na.omit(df_differenced))
summary(lin_reg_events)
# Both events are significant. But R squared is very low.
