###install.packages("readxl")
###install.packages("dplyr")
library(readxl)
library(lubridate)
library(dplyr)


# Load data
inflation_data <- read_excel("data/snb-data-plkoprinfla-en-all-20250422_0900.xlsx",skip=14) #skipping the first 14 rows
policy_rate_data <- read_excel("data/snb-target rate-policy rate-2000-2025.xlsx",skip=16)


# Inspect data
head(inflation_data)
tail(inflation_data)
head(policy_rate_data)
tail(policy_rate_data)

# rename the columns
policy_rate_data <- policy_rate_data %>%
  rename(
    Date = Overview
  )
inflation_data <- inflation_data %>%
  rename(
    Date = Overview
  )

# convert to data column
# str(inflation_data$Date)
# typeof(policy_rate_data$Date)
# it says chr (character) or factor, then convert it:
inflation_data  <- inflation_data  %>% mutate(Date = ym(Date))
policy_rate_data<- policy_rate_data%>% mutate(Date = ym(Date))
#check again type, if double is okay
typeof(inflation_data$Date)
typeof(policy_rate_data$Date)

# add a monthly key to both tables
# floor_date(Date, "month") takes any date (e.g. “2004-07-15” or “2000-07-01”) and “rounds it down” to the first of that month (e.g. “2004-07-01” or “2000-07-01”).
inflation_data    <- inflation_data    %>% mutate(YearMonth = floor_date(Date, "month"))
policy_rate_data  <- policy_rate_data  %>% mutate(YearMonth = floor_date(Date, "month"))

head(inflation_data)

# Prepare the 2 tables for merging
inflation_for_merge <- inflation_data %>%
  rename(Inflation_Date = Date) %>% # keep the original daily date under a new name
  select(YearMonth,
         Inflation_Date,
         `SNB - Core inflation, trimmed mean`,
         `SFSO - Core inflation 1`,
         `SFSO - Core inflation 2`,
         `SFSO - Inflation according to the national consumer price index`) %>%
  # rename long column names to short ones:
  rename(
    SNB_Core   = `SNB - Core inflation, trimmed mean`,
    Core1      = `SFSO - Core inflation 1`,
    Core2      = `SFSO - Core inflation 2`,
    CPI        = `SFSO - Inflation according to the national consumer price index`
  )%>%
  # ensure every month in range appears
  complete(YearMonth = seq(min(YearMonth), max(YearMonth), by = "month")) %>%
  # carry SNB_Core (and others) forward through any NA gaps
  fill(SNB_Core, Core1, Core2, CPI, .direction = "down")

policy_rate_for_merge <- policy_rate_data %>%
  rename(Policy_Date = Date) %>% # keep the original daily date under a new name
  select(YearMonth,
         Policy_Date,
         `Switzerland - SNB policy rate`,
         `Switzerland - SNB target range for the 3-month Libor rate in CHF - Lower limit`,
         `Switzerland - SNB target range for the 3-month Libor rate in CHF - Upper limit`) %>%
  # rename for convenience:
  rename(
    OfficialPR   = `Switzerland - SNB policy rate`,
    Range_Lower  = `Switzerland - SNB target range for the 3-month Libor rate in CHF - Lower limit`,
    Range_Upper  = `Switzerland - SNB target range for the 3-month Libor rate in CHF - Upper limit`
  )


policy_rate_for_merge <- policy_rate_for_merge %>%
  arrange(YearMonth) %>%
  mutate(
    PolicyRate = if_else(
      YearMonth < as.Date("2019-06-13"),
      (Range_Lower + Range_Upper) / 2,    # midpoint of the Libor target range
      OfficialPR                          # official policy rate from mid-2019
    )
  )%>%
  complete(YearMonth = seq(min(YearMonth), max(YearMonth), by = "month")) %>%
  fill(PolicyRate, Range_Lower, Range_Upper, OfficialPR, .direction = "down")      # carry the last known PolicyRate and others forward
#  Quick check for any remaining NAs
merged_data <- policy_rate_for_merge %>%
  left_join(inflation_for_merge, by = "YearMonth")

sum(is.na(merged_data$PolicyRate))  # should be 0
sum(is.na(merged_data$SNB_Core))    # should be 0

glimpse(merged_data)
head(merged_data, 10)



