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
  select(date = Overview,
         policy = `SNB policy rate`,
         ir_above = `Interest rate on sight deposits above threshold`,
         sar_fix = `SARON fixing at the close of the trading day`,
         special = `Special rate  (Liquidity-shortage financing facility)`) %>%
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


# Prepare data

pr <- pr_full %>% 
  select(date, ir_above, special) %>%
  filter(!is.na(special))                 # remove rows with NA in ir_above

infl <- inflation_data %>% 
  as_tibble() %>%
  select(date = Overview, infl = `SNB - Core inflation, trimmed mean`) %>% 
  mutate(date = ymd(str_c(date, "-01")),   # add a '-01' to the date string before making it a date
         infl = as.numeric(infl),
         infl = round(infl, 1))
# view(infl)


# Group by year-month and keep the first (oldest) row in each group

pr <- pr %>%                      
  group_by(year_month = floor_date(date, "month")) %>%     # Group by month
  slice_min(date) %>%             # Keep oldest date in each month
  ungroup() %>%
  mutate(date = year_month) %>%   # Overwrite 'date' with YYYY-MM-01
  select(-year_month)             # Remove temporary column
# view(pr)


# merge data

df <- inner_join(pr, infl, by = "date")  # merge the two tibbles
view(df)


# convert df to a zoo time series object

df_ts <- zoo(
  df %>% select(-date),
  order.by = df$date
)
view(df_ts)


## Visualize data

ggplot(df, aes(x = date)) +
  geom_line(aes(y = ir_above, color = "Interest rate on sight deposits above threshold")) +
  geom_line(aes(y = special, color = "Special rate Liquidity-shortage financing facility")) +
  geom_line(aes(y = infl, color = "SNB - Core inflation, trimmed mean")) +
  scale_color_manual(values = c("Interest rate on sight deposits above threshold" = "blue",
                                "Special rate Liquidity-shortage financing facility" = "darkgreen",
                                "SNB - Core inflation, trimmed mean" = "red")) +
  labs(title = "Swiss Policy Rates and Inflation Rates 2004-2025",
       color = "Legend",
       y = "(Inflation- / Policy-) Rates") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.direction = "horizontal")
