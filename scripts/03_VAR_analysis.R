# Vector Autoregression, 
# to see each policy rate shock would have an effect on inflation and how long?

# Build the VAR data matrix
# VAR + Causality

library(vars)


# 1) Prepare the stationary data
var_data <- na.omit(
  merged_data[, c("PolicyRate","dSNB_Core")]
)

# 2) Lag-order selection
# 2) Let VARselect pick optimal p up to 12 lags
lag_sel <- VARselect(var_data,
                     lag.max = 12,
                     type    = "const")
print(lag_sel$selection)
# e.g.   AIC(n)  HQ(n)  SC(n)
#       2      2     1

# 3) Extract the Schwarz (BIC) recommendation safely
p <- lag_sel$selection[["SC(n)"]]
message("Using lag order p = ", p)
# If you prefer AIC or HQ, you could instead use:
# p <- lag_sel$selection[["AIC(n)"]]
# p <- lag_sel$selection[["HQ(n)"]]

# (Just check it’s not NA and less than  sample size)
if (is.na(p) || p < 1 || p >= nrow(var_data)) {
  stop("Invalid lag order: ", p)
}

# 4) Estimate the VAR(p) with a constant
var_mod <- VAR(var_data,
               p    = p,
               type = "const")
summary(var_mod)

# 4) Granger‐causality tests
gc_PR_to_core <- causality(var_mod, cause = "PolicyRate")$Granger
gc_core_to_PR <- causality(var_mod, cause = "dSNB_Core")$Granger
print(gc_PR_to_core)
print(gc_core_to_PR)

# 5) Plot Impulse‐Response Functions to visualize causal impact
irf_PR_core <- irf(var_mod,
                   impulse  = "PolicyRate",
                   response = "dSNB_Core",
                   n.ahead  = 12,
                   boot     = TRUE)
plot(irf_PR_core, main = "IRF: PolicyRate → ΔSNB_Core")

irf_core_PR <- irf(var_mod,
                   impulse  = "dSNB_Core",
                   response = "PolicyRate",
                   n.ahead  = 12,
                   boot     = TRUE)
plot(irf_core_PR, main = "IRF: ΔSNB_Core → PolicyRate")

# Interpretation:
# Granger causality H0: PolicyRate do not Granger-cause dSNB_Core
# Granger causality H0: dSNB_Core do not Granger-cause PolicyRate
