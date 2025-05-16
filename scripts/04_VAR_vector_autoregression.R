library(tidyverse)
library(readxl)
library(zoo)
library(tseries)
library(forecast)
library(lmtest)
library(stats)
library(quantmod)
library(vars)
library(car)




# Estimating vector autoregression and Granger causality: 'policy_rate' with 'infl'

VAR_model <- VAR(cbind(df_differenced$policy_rate, df_differenced$infl) , ic="AIC", lag.max = 12)
# coeftest(VAR_model)
# summary(VAR_model)
causality(VAR_model, cause="df_differenced.policy_rate")["Granger"]

# Granger Causality Test (VAR)
# The test examines whether past values of df_differenced.policy_rate help predict current values of
# inflation beyond what's already explained by past values of inflation itself.
# There is statistically significant evidence that past values of policy_rate Granger-cause infl,
# i.e., policy rates have predictive power for inflation in our model.
# Granger causality ≠ true causation (it only indicates predictive ability).

causality(VAR_model, cause="df_differenced.infl")["Granger"]
# Inflation does not Granger-cause the policy rates.


# # Estimating vector autoregression and Granger causality: "SNB event in October 2022" with inflation rates.

VAR_df <- na.omit(cbind(df_differenced$event_2022_10, df_differenced$infl))
colnames(VAR_df) <- c("event_2022_10", "infl")


VAR_model <- VAR(VAR_df , ic="AIC", lag.max = 12)
coeftest(VAR_model)
causality(VAR_model, cause="event_2022_10")["Granger"]
# Interpretation: There is strong evidence that event_2020_01 and event_2022_10 Granger-cause infl (at lag 5 and lag6).

causality(VAR_model, cause="infl")["Granger"]
# Interpretation: There is no evidence that inflation Granger-causes the policy rate.
