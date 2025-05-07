###install.packages("readxl")
###install.packages("dplyr")
library(readxl)
library(lubridate)
library(dplyr)


# Load data
inflation_data <- read_excel("data/snb-data-plkoprinfla-en-all-20250422_0900.xlsx",skip=14) #skipping the first 14 rows
policy_rate_data <- read_excel("data/snb-data-snbgwdzid-en-all-20250414_1000.xlsx",skip=21)
policy_rate_data<-policy_rate_data[,c("Overview","SNB policy rate","SARON fixing at the close of the trading day")]

# Inspect data
head(inflation_data)
tail(inflation_data)
head(policy_rate_data)
tail(policy_rate_data)

# rename the columns
colnames(inflation_data)<-c("Date","SNB_Core","SFSO_Core1", "SFSO_Core2", "SFSO_CPI")
colnames(policy_rate_data)<-c("Date","SNB_Policy_Rate","Saron")

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

#Data cleaning ideas:
"""1 for policy rate data: see miro for latest logic
  1.1 Standardize policy‐rate table so it always has the same columns, 
   even when the source file only contains some of them.
	1.2.	Reconstruct a single “policy rate” series that covers the entire 2004–2025 span, by
		1.2.1 using the official SNB policy rate from 13 June 2019 onward,
	  1.2.2 computing the midpoint of the deposit‐and‐special rates between 21 Jan 2015 and 12 June 2019,
		1.2.3 and back-casting before 21 Jan 2015 by taking the special-facility rate minus 50 bp (since that facility was always policy + 50 bp)."""
""""""
# 3 after filling, deal with missing data,for the rates, date, YearMonth on both tables, maybe need to check that fist


# 2 need to deal with the inconsistent measurement
# (policy rate data only special rate before 2009-08-24, both data have missing data)

# next step is merge(left join right join..)

