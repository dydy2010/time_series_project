###install.packages("tidyverse")
library(tidyverse)
library(readxl)
library(zoo)


# Load data

policy_rate_data <- read_excel("data/snb-data-snbgwdzid-en-all-20250414_1000.xlsx",skip=21)
inflation_data <- read_excel("data/snb-data-plkoprinfla-en-all-20250422_0900.xlsx",skip=14) #skipping the first 14 rows

# Prepare data

pr <- policy_rate_data %>% 
  as_tibble() %>%  # like a dataframe but more 'tidy'
  select(Overview,`SNB policy rate`) %>%  # Only date and policy rates are relevant
  slice(-1) %>%                           # remove sub-header in the first line
  rename(date = Overview, pr = `SNB policy rate`) %>%
  mutate(date = ymd(date),
         pr = as.numeric(pr),
         pr = round(pr, 2)) %>%
  filter(!is.na(pr))                      # remove rows with NA in pr

pr <- pr %>%                      # Group by year-month and keep the first (oldest) row in each group
  group_by(year_month = floor_date(date, "month")) %>%     # Group by month
  slice_min(date) %>%             # Keep oldest date in each month
  ungroup() %>%
  mutate(date = year_month) %>%   # Overwrite 'date' with YYYY-MM-01
  select(-year_month)             # Remove temporary column
# view(pr)

infl <- inflation_data %>% 
  as_tibble() %>% 
  select(Overview, `SFSO - Inflation according to the national consumer price index`) %>% 
  slice(-1) %>% 
  rename(date = Overview, infl = `SFSO - Inflation according to the national consumer price index`) %>% 
  mutate(date = ymd(str_c(date, "-01")),   # add a '-01' to the date string before making it a date
         infl = as.numeric(infl),
         infl = round(infl, 1))
# view(infl)

df <- inner_join(pr, infl, by = "date")  # merge the two tibbles
view(df)

# convert df to a zoo time series object

df_ts <- zoo(
  df %>% select(-date),
  order.by = df$date
)
view(df_ts)



# Former code not taking into account missing data:

"""

# convert to data column
# str(inflation_data$Date)
# typeof(policy_rate_data$Date)
# it says chr (character) or factor, then convert it:
inflation_data$Date <- as.Date(inflation_data$Date, format ="%Y-%m-%d")
policy_rate_data$Date <- as.Date(policy_rate_data$Date)
#check again type, if double is okay
# typeof(inflation_data$Date)

#inflation_data$Date <- ym(inflation_data$Date) ### maybe Dropping this — it forces all dates to first-of-month and loses original Date
tail(inflation_data)

# keeping original date column on each table
# summarize daily inflation into monthly values or(not trying this one direction)
# fill in the policy rate for every day in each month(trying this one first)

# Monthly inflation add to YearMonth column (first of month)
inflation_data <- inflation_data %>%
  mutate(YearMonth = floor_date(Date, "month")) ### floor_date() is a convenience function from lubridate that “rounds down” a date
# Daily policy rate add to YearMonth column (first of month)
policy_rate_data <- policy_rate_data %>%
  mutate(YearMonth = floor_date(Date, "month"))

# rename the Date in inflation and date in policy rate, so that both columns are kept and doesnt cause confusion, as they are all "Date"
inflation_for_merge <- inflation_data %>%
  rename(Date_Inflation = Date) %>%
  select(YearMonth,
         Date_Inflation,
         SNB_Core,
         SFSO_Core1,
         SFSO_Core2,
         SFSO_CPI)

policy_rate_for_merge <- policy_rate_data %>%
  rename(Date_Policy = Date) %>%
  select(YearMonth,
         Date_Policy,
         SNB_Policy_Rate,
         Saron)

# forgot to deal with missing data,for the rates, date, YearMonth on both tables, maybe need to check that fist
# next step is merge(left join right join..)

"""


