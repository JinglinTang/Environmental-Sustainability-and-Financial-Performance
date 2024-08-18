# Load necessary libraries
library(readxl)
library(tidyverse)
library(dplyr)
library(plm)
library(lmtest)
library(summarytools)
library(lubridate)
# Load the data
setwd("/Users/jinglin/Downloads/Dissertation/")
market_data <- read.csv("FFDaily.csv")
stock_data <- read.csv("StockReturn.csv")

# Ensure date columns are in date format
market_data$date <- ymd(as.character(market_data$date))
stock_data$date <- ymd(as.character(stock_data$date))

# Merge the datasets on the date
merged_data <- merge(stock_data, market_data, by = "date")

# Add the yearly column to the merged data
merged_data$year <- year(merged_data$date)

merged_data$RET <- as.numeric(merged_data$RET)
merged_data$RET.RF <- merged_data$RET*100-merged_data$RF

merged_data <- merged_data %>%
  filter(!is.na(MKT.RF) & !is.na(RET.RF))

# Function to perform CAPM regression for a single company and year
perform_capm <- function(df) {
  if(nrow(df) > 1) { # Ensure there is more than one data point
    model <- lm(RET.RF ~ MKT.RF, data = df)
    daily_ce <- coef(model)[2] * mean(df$MKT.RF) + mean(df$RF)
    annual_ce <- (1 + daily_ce/100)^252 - 1
    return(data.frame(Intercept = coef(model)[1], Beta = coef(model)[2], R2 = summary(model)$r.squared, IdioRisk = sd(model$residuals), CE = annual_ce))
  } else {
    return(data.frame(Intercept = NA, Beta = NA, R2 = NA, IdioRisk=NA, CE = NA))
  }
}

# Apply the CAPM regression for each company and each year
capm_results <- merged_data %>%
  group_by(CUSIP, year) %>%
  do(perform_capm(.)) %>%
  ungroup()

# Set options to avoid scientific notation
options(scipen = 999)

# Save the results to a CSV file
write.csv(capm_results, "capm_results.csv")
