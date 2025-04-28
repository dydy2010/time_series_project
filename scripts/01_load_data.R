##hihi Daniel can you see it
##hihi
###i am writing here

###install.packages("readxl")
library(readxl)
inflation_data <- read_excel(
  "~/Library/CloudStorage/OneDrive-Personal/文档/学业/硕士hslu/Lectures Lessons Subjects/Time series analysis/project/R git/time_series_project/data/snb-data-plkoprinfla-en-all-20250422_0900.xlsx",skip=14)##skipping the first 14 rows
policy_rate_data <- read_excel(
  "~/Library/CloudStorage/OneDrive-Personal/文档/学业/硕士hslu/Lectures Lessons Subjects/Time series analysis/project/R git/time_series_project/data/snb-data-snbgwdzid-en-all-20250414_1000.xlsx",skip=21)
policy_rate_data<-policy_rate_data[,c("Overview","SNB policy rate")]
`
# View the first few rows
head(inflation_data)
head(policy_rate_data)
# rename the columns
colnames(inflation_data)<-c("Date","SNB_Core","SFSO_Core1", "SFSO_Core2", "SFSO_CPI")
colnames(policy_rate_data)<-c("Date","SNB_Policy_Rate","Saron")

# convert to data column
str(inflation_data$Date)
# it says chr (character) or factor, then convert it:
inflation_data$Date <- as.Date(inflation_data$Date,format="%Y-%m")