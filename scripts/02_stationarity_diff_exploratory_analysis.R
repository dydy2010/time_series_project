# Quick Look at Summary Statistics
# Select just the key columns
eda_summary <- merged_data %>%
  summarise(
    mean_PR    = mean(PolicyRate,    na.rm=TRUE),
    sd_PR      = sd(PolicyRate,      na.rm=TRUE),
    min_PR     = min(PolicyRate,     na.rm=TRUE),
    max_PR     = max(PolicyRate,     na.rm=TRUE),
    
    mean_core  = mean(SNB_Core,      na.rm=TRUE),
    sd_core    = sd(SNB_Core,        na.rm=TRUE),
    min_core   = min(SNB_Core,       na.rm=TRUE),
    max_core   = max(SNB_Core,       na.rm=TRUE),
  )
print(eda_summary)

library(tseries)
adf.test(merged_data$PolicyRate,    k = 12)
adf.test(merged_data$SNB_Core,      k = 12)

# interpretation:
# PolicyRate is already stationary in levels (ADF p≈0.036 < 0.05) , no need to difference it for stationarity.
# SNB_Core is non-stationary (ADF p≈0.455 > 0.05), must difference it.


# Difference SNB_Core and re-test
merged_data <- merged_data %>%
  arrange(YearMonth) %>%
  mutate(
    dSNB_Core = SNB_Core - lag(SNB_Core)
  )

# Remove the first NA
core_diff <- na.omit(merged_data$dSNB_Core)

# ADF on the differenced series
adf_dcore <- adf.test(core_diff, k = 12)
print(adf_dcore)

# plot the data to have a look, PolicyRate with ΔSNB_Core (on separate axes)

ggplot(merged_data, aes(x = YearMonth)) +
  geom_line(aes(y = PolicyRate), color = "darkgreen", linewidth = 1) +
  geom_line(aes(y = dSNB_Core * 5), color = "steelblue", linewidth = 1) +
  scale_y_continuous(
    name = "PolicyRate (%)",
    sec.axis = sec_axis(~ . / 5, name = "ΔSNB_Core (%)")
  ) +
  labs(
    title = "PolicyRate (green) vs. ΔSNB_Core (blue)",
    x     = "Month"
  ) +
  theme_minimal() +
  theme(
    axis.title.y.left  = element_text(color = "darkgreen"),
    axis.title.y.right = element_text(color = "steelblue")
  )



