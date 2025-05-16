library(tidyverse)
library(readxl)
library(zoo)
library(tseries)
library(forecast)
library(lmtest)
library(stats)
library(quantmod)


## Check stationarity

adf.test(na.omit(df_ts$policy_rate))
adf.test(na.omit(df_ts$infl))

# Both series are not stationary.
# Take first differences of the series
df_differenced <- diff(df_ts)

# Check stationarity again
adf.test(na.omit(df_differenced$policy_rate))
adf.test(na.omit(df_differenced$infl))
View(df_differenced)

# Both series are stationary now.

# Check for correlation between the two stationary series
cor(df_differenced$policy_rate, df_differenced$infl, use = "pairwise.complete.obs")
# Very weak positive correlation.

# Linear regression of policy_rate on inflation shows no significant coefficients and R squared is very low.
lin_reg <- lm(infl ~ policy_rate, data = df_differenced)
summary(lin_reg)

# Plot the regression residuals. The plot shows large residuals, confirming the low R squared.
resid <- lin_reg$residuals
plot(y=resid, x=as.Date(time(df_differenced)), ylab="Residuals", xlab="Year", type="l", main="Regression Residuals")
grid()

# Breusch-Pagan test for Constant variance, null hypothesis = Residuals are homoscedastic
# Perform Breusch-Pagan test
# There is no significant evidence of heteroskedasticity in the linear regression model.
bptest(lin_reg)

# Shapiro test for normality, null hypothesis = Residuals are normally distributed
# Strong rejection of the null hypothesis: The residuals of the model are NOT normally distributed.
shapiro.test(resid)

# Check for Outliers & Influential Points
plot(lin_reg, which = 1)  # Residuals vs Fitted
plot(lin_reg, which = 2)  # Q-Q plot
plot(lin_reg, which = 4)  # Cook's distance

# Durbin-Watson test for serial correlation, null hypothesis = Residuals are not autocorrelated
# The Durbin-Watson test statistic is very close to 2, which is the expected value under the
# null hypothesis of no autocorrelationhypothesis of no autocorrelationnull hypothesis of
# no autocorrelation
# p > 0.05: There is no statistically significant evidence of positive autocorrelation in the residuals.
dwtest(lin_reg) 


## Alternative approaches:

## 1. lead-lag relation: e.g. infl(t) = a + b * policy_rate(t-1) + e(t)
## 2. Treat SNB actions as events, e.g. 2020-07-01 (reduce from 0.5 to 0.0) or 2022-10-01 (increase from 0.25 to 1.0)


# 1. lead-lag relation: e.g. infl(t) = a + b * policy_rate(t-1) + e(t)

# Create lagged variables
df_differenced$policy_rate_lag1 <- stats::lag(df_differenced$policy_rate, k = 1)
df_differenced$policy_rate_lag2 <- stats::lag(df_differenced$policy_rate, k = 2)
df_differenced$policy_rate_lag3 <- stats::lag(df_differenced$policy_rate, k = 3)
df_differenced$policy_rate_lag4 <- stats::lag(df_differenced$policy_rate, k = 4)
df_differenced$policy_rate_lag5 <- stats::lag(df_differenced$policy_rate, k = 5)
df_differenced$policy_rate_lag6 <- stats::lag(df_differenced$policy_rate, k = 6)
df_differenced$policy_rate_lag7 <- stats::lag(df_differenced$policy_rate, k = 7)
df_differenced$policy_rate_lag8 <- stats::lag(df_differenced$policy_rate, k = 8)
df_differenced$policy_rate_lag9 <- stats::lag(df_differenced$policy_rate, k = 9)
df_differenced$policy_rate_lag10 <- stats::lag(df_differenced$policy_rate, k = 10)
df_differenced$policy_rate_lag11 <- stats::lag(df_differenced$policy_rate, k = 11)
df_differenced$policy_rate_lag12 <- stats::lag(df_differenced$policy_rate, k = 12)
view(df_differenced)

# Fit the linear model, removing rows with NA due to lagging
lin_reg_lagged <- lm(infl ~ policy_rate_lag1 + policy_rate_lag2 + policy_rate_lag3 +
                       policy_rate_lag4 + policy_rate_lag5 + policy_rate_lag6 +
                       policy_rate_lag7 + policy_rate_lag8 + policy_rate_lag9 +
                       policy_rate_lag10 + policy_rate_lag11 + policy_rate_lag12,
                     data = na.omit(df_differenced))
summary(lin_reg_lagged)

# Lag 1 and lag 10 are significant (p-value < 0.05), but R squared (0.06249, adjusted 002187) is very low.
# We are not sure if a lag of 10 months makes sense from a logical point of view.
# Lag 1 would make sense and the model shows that it is statistically significant.
# But the direction of lag 1 is not as expected: higher policy rate goes with higher inflation.
# Overall we are not convinced by this model.


## 2. Treat SNB actions as events

df_differenced$event_2008_10 <- ifelse(index(df_differenced) >= as.Date("2008-10-01"), 1, 0)
df_differenced$event_2014_11 <- ifelse(index(df_differenced) >= as.Date("2014-11-01"), 1, 0)
df_differenced$event_2020_01 <- ifelse(index(df_differenced) >= as.Date("2020-07-01"), 1, 0)
df_differenced$event_2022_05 <- ifelse(index(df_differenced) >= as.Date("2022-05-01"), 1, 0)
df_differenced$event_2022_10 <- ifelse(index(df_differenced) >= as.Date("2022-10-01"), 1, 0)


lin_reg_events <- lm(infl ~ event_2008_10 + event_2014_11 + event_2020_01 + event_2022_05 + event_2022_10, data = na.omit(df_differenced))
summary(lin_reg_events)
# The last event 2022_10 is significant. But R squared is very low.
