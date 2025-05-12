library(tidyverse)
library(readxl)
library(zoo)
library(tseries)
library(forecast)
library(lmtest)
library(quantmod)
library(vars)
library(car)



# Estimating vector autoregression and Granger causality: 'special' with 'infl'

VAR_model <- VAR(cbind(df_differenced$special, df_differenced$infl) , ic="AIC", lag.max = 12)
coeftest(VAR_model)
causality(VAR_model, cause="df_differenced.special")["Granger"]
causality(VAR_model, cause="df_differenced.infl")["Granger"]
# Interpretation: The Granger-causality test states that neither does Granger-cause the other.


# Estimating vector autoregression and Granger causality: 2 'events' with 'infl'

VAR_df <- cbind(df_differenced$event_2020_01, df_differenced$event_2022_10, df_differenced$infl)
colnames(VAR_df) <- c("event_2020_01", "event_2022_10", "infl")


VAR_model <- VAR(VAR_df , ic="AIC", lag.max = 12)
coeftest(VAR_model)
causality(VAR_model, cause="event_2020_01")["Granger"]
causality(VAR_model, cause="event_2022_10")["Granger"]
causality(VAR_model, cause="infl")["Granger"]
# Interpretation: There is Granger-causality, but we need to separate the dependencies.

# Get the number of lags used
p <- VAR_model$p

# Build hypothesis string: are all lags of event_2020_01 zero in the infl equation?
hypothesis1 <- paste0("event_2020_01.l", 1:p, " = 0", collapse = " & ")
hypothesis2 <- paste0("event_2022_10.l", 1:p, " = 0", collapse = " & ")


# Run the test on the 3rd equation (infl)
linearHypothesis(VAR_model$varresult$infl, hypothesis1)
linearHypothesis(VAR_model$varresult$infl, hypothesis2)
# Interpretation: There is strong evidence that event_2020_01 and event_2022_10 Granger-cause infl (at lag 1).
