library(readxl)
library(tidyverse)
library(dplyr)
library(plm)
library(lmtest)
library(summarytools)
library(lubridate)
library(Hmisc)

setwd("/Users/jinglin/Downloads/Dissertation/")
data1 <- read_excel("FSBanks.xlsx", sheet="Sheet1")
capm_results <- read.csv("capm_results.csv", stringsAsFactors = FALSE)
stock_data <- read.csv("StockReturn.csv")

data1$ROA <- data1$ni/data1$at
data1$ROE <- data1$ni/data1$teq
data1$E2 <- data1$E*data1$E
data1$ESG2 <- data1$ESG*data1$ESG
data1$CostDebt <- (data1$intpn/data1$dlc)*(1-0.34)
data1$NetIncome <- (data1$ib+data1$dp)#/data1$at

#Control Variables
data1$Cap <- data1$teq/data1$at
data1$Liquidity <- data1$dptc/data1$at
data1$Efficiency <- data1$tcoe/data1$ni
#data1$AssetQuality <- data1$tcoe/data1$lcat
data1$CostDebt[is.na(data1$CostDebt) | data1$CostDebt=="Inf"] = NA

dp <- pdata.frame(data1, index = c("cusip","fyear"))
dp$lroa <- lag(dp$ROA)

colnames(capm_results)[3] <- "fyear"
colnames(capm_results)[2] <- "cusip"
capm_results$fyear <- as.numeric(capm_results$fyear)

capm_results <- pdata.frame(capm_results, index = c("cusip","fyear"))

dp <- dp %>%
  mutate(cusip = trimws(as.character(cusip)))

dp <- dp %>%
  mutate(cusip = ifelse(nchar(cusip) == 9, substr(cusip, 1, 8), cusip))

merged_data <- merge(capm_results, dp, by = c("cusip","fyear"))
merged_data <- pdata.frame(merged_data, index = c("cusip","fyear"))

stock_data$date <- as.Date(stock_data$date, format="%Y-%m-%d")
stock_data$year <- year(stock_data$date)
last_day_stock_data <- stock_data %>%
  group_by(CUSIP, year) %>%
  filter(date == max(date)) %>%
  ungroup()

last_day_stock_data <- last_day_stock_data %>%
  mutate(market_cap = SHROUT * PRC)
last_day_stock_data$year <- as.numeric(last_day_stock_data$year)
last_day_stock_data <- last_day_stock_data %>% select(all_of(c("CUSIP","year","market_cap")))

merged_data <- merge(merged_data, last_day_stock_data, by.x=c("cusip", "fyear"), by.y=c("CUSIP", "year"))

merged_data$Tobinsq <- (merged_data$at-merged_data$ceq+(merged_data$market_cap/1000))/merged_data$at

merged_data <- merged_data %>%
  group_by(fyear) %>%
  mutate(total_dptc = sum(dptc, na.rm = TRUE),
         market_share = (dptc) / total_dptc * 100000) %>%
  ungroup()

descr(merged_data,
      round.digits = 4
)

mydata <- merged_data[, c('ROA','ROE','Tobinsq','NetIncome','CE','IdioRisk','Beta','nim','CostDebt','E','E2','Cap','Liquidity','Efficiency','market_share')]
#mydata <- na.omit(mydata)
#str(mydata)
mydata.cor = rcorr(as.matrix(mydata))
mydata.cor

#symnum(mydata.cor, cutpoints = c(0.4, 0.2, 0.1, 0.05, 0.01),
#       symbols = c(" ", ".", ",", "+", "*", "B"),
#       abbr.colnames = TRUE)
#symnum(mydata.cor[["P"]], abbr.colnames = TRUE)

#Main Hypothesis
mod1 <- plm(log(NetIncome) ~ E + E2 + Cap + Liquidity + log(Efficiency),
               data = merged_data,
               na.action = na.omit,
               index = c("cusip","fyear"),
               effect = "individual",
               model = "pooling")
coeftest(mod1, vcov. = vcovHC)
summary(mod1)
nobs(mod1)

dyn_model <- pgmm(log(NetIncome) ~ lag(log(NetIncome), 1) + E + E2 + Cap + Liquidity + log(Efficiency)
                    | 
                    lag(log(NetIncome), 2:99),
                  data = merged_data, 
                  index = c("cusip"),
                  effect = "individual", 
                  model = "twosteps", 
                  transformation = "ld", 
                  collapse = TRUE
)
summary(dyn_model, robust = TRUE)
nobs(dyn_model)

dyn_model <- pgmm(ROE ~ lag(ROE, 1) + E + E2 + Cap + Liquidity + log(Efficiency)
                  | 
                    lag(ROE, 2:99) + lag(E, 1:99) + lag(E2, 1:99) + lag(Cap, 1:99)
                  + lag(Liquidity, 1:99) + lag(log(Efficiency), 1:99),
                  data = merged_data, 
                  index = c("cusip"),
                  effect = "individual", 
                  model = "twosteps", 
                  transformation = "ld", 
                  collapse = TRUE
)
summary(dyn_model, robust = TRUE)
nobs(dyn_model)

dyn_model <- pgmm(Tobinsq ~ lag(Tobinsq, 1) + E + E2 + Cap + Liquidity + log(Efficiency)
                  | 
                    lag(Tobinsq, 2:99) + lag(E, 1:99) + lag(E2, 1:99) + lag(Cap, 1:99)
                  + lag(Liquidity, 1:99) + lag(log(Efficiency), 1:99),
                  data = merged_data, 
                  index = c("cusip"),
                  effect = "individual",
                  model = "twosteps", 
                  transformation = "ld", 
                  collapse = TRUE
)
summary(dyn_model, robust = TRUE)
nobs(dyn_model)

mod2 <- plm(ROA ~ ESG + ESG2 + Cap + Liquidity + log(Efficiency),
               data = merged_data,
            index = c("cusip"),
            effect = "individual",
            model = "within")
coeftest(mod2, vcov. = vcovHC)

mod3 <- plm(ROE ~ E + E2 + Cap + Liquidity + log(Efficiency),
            data = merged_data,
            index = c("cusip","fyear"),
            effect = "twoways",
            model = "within")
coeftest(mod3, vcov. = vcovHC)

mod4 <- plm(Tobinsq ~ ESG + ESG2 + Cap + Liquidity + log(Efficiency),
            data = merged_data,
            index = c("cusip","fyear"),
            effect = "individual",
            model = "pooling")
coeftest(mod4, vcov. = vcovHC)

mod5 <- plm(Tobinsq ~ E + E2 + Cap + Liquidity + log(Efficiency),
            data = merged_data,
            index = c("cusip"),
            effect = "individual",
            model = "within")
coeftest(mod5, vcov. = vcovHC)

fe_mod <- plm(ROA ~ E + E2 + Cap + Liquidity + Efficiency, 
              data = data1,
              index = c("conm","fyear"),
              effect = "individual",
              model = "within")

coeftest(mod2, vcov. = vcovHC) #Robust Standard Errors
summary(mod2)
nobs(mod2)
coeftest(mod3, vcov. = vcovHC) #Robust Standard Errors
summary(mod3)
nobs(mod3)

coeftest(fe_mod, vcov. = vcovHC) #Robust Standard Errors
summary(fe_mod)
nobs(fe_mod)

#Valuation Channel
VM <- plm(CE ~ E + E2 + Cap + Liquidity + log(Efficiency),
           data = merged_data,
           index = c("cusip"),
           effect = "individual",
           model = "within")
coeftest(VM, vcov. = vcovHC)

dyn_model <- pgmm(CE ~ lag(CE, 1) + E + E2 + Cap + Liquidity + log(Efficiency)
                  | 
                    lag(CE, 2:99) ,
                  data = merged_data, 
                  index = c("cusip"),
                  effect = "individual", 
                  model = "twosteps", 
                  transformation = "ld", 
                  collapse = TRUE
)
summary(dyn_model, robust = TRUE)
nobs(dyn_model)

#Cash Flow Channel
CFM1 <- plm(log(ni) ~ E + E2 + Cap + Liquidity + log(Efficiency),
           data = merged_data,
           index = c("cusip"),
           effect = "individual",
           model = "within")
coeftest(CFM1, vcov. = vcovHC)
summary(CFM1)
nobs(CFM1)

dyn_model <- pgmm(NetIncome ~ lag(NetIncome, 1) + E + E2 + Cap + Liquidity + log(Efficiency)
                  | 
                    lag(NetIncome, 2:99),
                  data = merged_data, 
                  index = c("cusip"),
                  effect = "individual", 
                  model = "twosteps", 
                  transformation = "ld", 
                  collapse = TRUE
)
summary(dyn_model, robust = TRUE)
nobs(dyn_model)

CFM2 <- plm(nim ~ E + E2 + Cap + Liquidity + log(Efficiency),
            data = merged_data,
            index = c("cusip"),
            effect = "individual",
            model = "within")
coeftest(CFM2, vcov. = vcovHC)
summary(CFM2)
nobs(CFM2)

dyn_model <- pgmm(nim ~ lag(nim, 1) + E + E2 + Cap + Liquidity + log(Efficiency)
                  | 
                    lag(nim, 2:99),
                  data = merged_data, 
                  index = c("cusip"),
                  effect = "individual", 
                  model = "twosteps", 
                  transformation = "ld", 
                  collapse = TRUE
)
summary(dyn_model, robust = TRUE)
nobs(dyn_model)

#Idiosyncratic Risk Channel
IRM1 <- plm(IdioRisk ~ E + E2 + Cap + Liquidity + log(Efficiency),
            data = merged_data,
            index = c("cusip","fyear"),
            effect = "individual",
            model = "within")
coeftest(IRM1, vcov. = vcovHC)
summary(IRM1)
nobs(IRM1)

dyn_model <- pgmm(IdioRisk ~ lag(IdioRisk, 1) + E + E2 + Cap + Liquidity + log(Efficiency)
                  | lag(IdioRisk, 2:99),
                  data = merged_data, 
                  index = c("cusip"),
                  effect = "individual", 
                  model = "twosteps", 
                  transformation = "ld", 
                  collapse = TRUE
)
summary(dyn_model, robust = TRUE)
