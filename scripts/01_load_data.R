###install.packages("tidyverse")
library(tidyverse)
library(readxl)
library(zoo)


## Data preparation


# Load data

policy_rate_data <- read_excel("data/snb-data-snbgwdzid-en-all-20250414_1000.xlsx",
                               col_types = c("text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"),
                               skip = 21)

inflation_data <- read_excel("data/snb-data-plkoprinfla-en-all-20250422_0900.xlsx",
                             skip = 14) #skipping the first 14 rows


# Look at different available columns in the policy rate data:

pr_full <- policy_rate_data %>% 
  as_tibble() %>% 
  dplyr::select(date = "Overview",
         policy = "SNB policy rate",
         ir_above = "Interest rate on sight deposits above threshold",
         sar_fix = "SARON fixing at the close of the trading day",
         special = "Special rate  (Liquidity-shortage financing facility)") %>%
  mutate(date = ymd(date),
         (across(c(policy, ir_above, sar_fix, special),
                 ~ round(as.numeric(.), 2)))
  )

# view(pr_full)  

ggplot(pr_full, aes(x = date)) +
  geom_line(aes(y = policy, color = "Policy Rate")) +
  geom_line(aes(y = ir_above, color = "Interest rate above threshold")) +
  geom_line(aes(y = sar_fix, color = "SARON fixing")) +
  geom_line(aes(y = special, color = "Special rate")) +
  scale_color_manual(values = c("Policy Rate" = "blue",
                                "Interest rate above threshold" = "green",
                                "SARON fixing" = "brown",
                                "Special rate" = "red")) +
  labs(title = "Swiss Policy Rates: Available data",
       color = "Legend",
       y = "(Policy-Rates") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.direction = "horizontal") 


libor_data <- suppressWarnings(
  read_excel("data/snb-target rate-policy rate-2000-2025.xlsx",
                         range = cell_limits(c(18, 1), c(NA, 4)),
                         col_names = c("date", "policy_rate", "libor_3m_low", "libor_3m_high"),
                         col_types = c("text", "numeric", "numeric", "numeric")) %>%
  mutate(date = ymd(str_c(date, "-01")),
         (across(c(policy_rate, libor_3m_low, libor_3m_high),
                 ~ round(as.numeric(.), 2))),
         libor_3m_avg = (libor_3m_low + libor_3m_high) / 2) %>% 
  mutate(libor_3m_avg = round(libor_3m_avg, 2))
)

ggplot(libor_data, aes(x = date)) +
  geom_line(aes(y = policy_rate, color = "Policy Rate")) +
  geom_line(aes(y = libor_3m_low, color = "libor_3m_lower")) +
  geom_line(aes(y = libor_3m_high, color = "libor_3m_upper")) +
  geom_line(aes(y = libor_3m_avg, color = "libor_3m_avg")) +
  scale_color_manual(values = c("Policy Rate" = "blue",
                                "libor_3m_lower" = "green",
                                "libor_3m_upper" = "brown",
                                "libor_3m_avg" = "lightblue")) +
  labs(title = "Swiss Policy Rates, 3 Month Libor lower and upper limits",
       color = "Legend",
       y = "(Policy-Rates") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.direction = "horizontal") 

# The average of the 3 Month Libor lower and upper limits is the correct proxy for the
# SNB policy rate before 2020.



# Prepare data

pr <- libor_data %>%
  mutate(
  policy_rate = if_else(
    date < as.Date("2019-06-01"),
    libor_3m_avg,       
    policy_rate)) %>% 
    dplyr::select(date, policy_rate)

infl <- inflation_data %>% 
  as_tibble() %>%
  dplyr::select(date = Overview, infl = `SNB - Core inflation, trimmed mean`) %>% 
  mutate(date = ymd(str_c(date, "-01")),   # add a '-01' to the date string before making it a date
         infl = as.numeric(infl),
         infl = round(infl, 1))


# merge data

df <- inner_join(pr, infl, by = "date")  # merge the two tibbles


# convert df to a zoo time series object

df_ts <- zoo(
  df %>% dplyr::select(-date),
  order.by = df$date
)


## Visualize data

ggplot(df, aes(x = date)) +
  geom_line(aes(y = policy_rate, color = "SNB Policy Rate")) +
  geom_line(aes(y = infl, color = "SNB - Core inflation, trimmed mean")) +
  scale_color_manual(values = c("SNB Policy Rate" = "blue",
                                "SNB - Core inflation, trimmed mean" = "red")) +
  labs(title = "Swiss Policy Rates and Inflation Rates 2000-2025",
       color = "Legend",
       y = "(Inflation- / Policy-) Rates") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.direction = "horizontal")


