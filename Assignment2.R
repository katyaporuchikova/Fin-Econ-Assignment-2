#Clear the environment
rm(list=ls())

#Set Working directory
setwd("C:/Users/Fredrik Hopland/Desktop/FinancialEconometrics/Assignments/Assignment 2")

#Load packages
require(tidyverse)
require(lmtest)
require(car)
require(DescTools)
require(sandwich)
require(mfx)
require(plyr)
require(stargazer)
require(ggplot2)
require(mfx)
require(lubridate)

#Load data from webpage
portfolio_BM <- read.csv("https://ba-odegaard.no/financial_data/ose_asset_pricing_data/book_market_portfolios_monthly_vw.txt")
portfolio_size <- read.csv("https://ba-odegaard.no/financial_data/ose_asset_pricing_data/equity_size_portfolios_monthly_vw.txt")
portfolio_momentum <- read.csv("https://ba-odegaard.no/financial_data/ose_asset_pricing_data/pricing_factors_monthly.txt")


#Investigate data
str(portfolio_BM)
str(portfolio_size)
str(portfolio_momentum)

summary(portfolio_BM)
summary(portfolio_size)
summary(portfolio_momentum)

#Changing Date-int to date
portfolio_BM$date <- ymd(portfolio_BM$date)
portfolio_size$date <- ymd(portfolio_size$date)
portfolio_momentum$date <- ymd(portfolio_momentum$date)

#Renaming columns before merging 
colnames(portfolio_size)[-1] <- paste("SIZE", 1:10)
colnames(portfolio_size)[1] <- paste("DATE")
colnames(portfolio_BM)[-1] <- paste0("BM", 1:10)
colnames(portfolio_BM)[1] <- paste0("DATE")
colnames (portfolio_momentum) [-1] <- paste0("MOMENTUM", 1:4)
colnames(portfolio_momentum) [1] <- paste0("DATE")

#Merging
port_size_BM_momentum <- inner_join(portfolio_momentum, portfolio_BM) %>% 
  inner_join(portfolio_size)
data <- merge(portfolio_momentum, portfolio_BM, all = FALSE)
colnames(data)[-1] <- paste0("X", 1:10) -> data

###############################################################################
    #Task 1 - Computing the exposure of each portfolio to the momentum factor
###############################################################################
coef.vec <- rep(NA, 10)
# iteration
for(i in 1:10){
  # regression
  fit1 <- lm(BM1 ~ MOMENTUM4, data = port_size_BM_momentum)
  # save coefficient
  coef.vec[i] <- fit1[...]
}


#Stargazer
stargazer(fit,
          type="text",
          keep.stat=c("n", "rsq", "adj.rsq"),
          report=('vc*t'))

#Computing regressions one by one
fit1 <- list()

fit1[[1]] <- lm(BM1 ~ MOMENTUM4, data = port_size_BM_momentum)
fit1[[2]] <- lm(BM2 ~ MOMENTUM4, data = port_size_BM_momentum)
fit1[[3]] <- lm(BM3 ~ MOMENTUM4, data = port_size_BM_momentum)
fit1[[4]] <- lm(BM4 ~ MOMENTUM4, data = port_size_BM_momentum)
fit1[[5]] <- lm(BM5 ~ MOMENTUM4, data = port_size_BM_momentum)
fit1[[6]] <- lm(BM6 ~ MOMENTUM4, data = port_size_BM_momentum)
fit1[[7]] <- lm(BM7 ~ MOMENTUM4, data = port_size_BM_momentum)
fit1[[8]] <- lm(BM8 ~ MOMENTUM4, data = port_size_BM_momentum)
fit1[[9]] <- lm(BM9 ~ MOMENTUM4, data = port_size_BM_momentum)
fit1[[10]] <- lm(BM10 ~ MOMENTUM4, data = port_size_BM_momentum)

stargazer(fit1,
          type="text",
          keep.stat=c("n", "rsq", "adj.rsq"),
          report=('vc*t'))

#Alternatively
fit2 <- list()

fit2[[1]] <- lm(MOMENTUM4 ~ BM1, data = port_size_BM_momentum)
fit2[[2]] <- lm(MOMENTUM4 ~ BM2, data = port_size_BM_momentum)
fit2[[3]] <- lm(MOMENTUM4 ~ BM3, data = port_size_BM_momentum)
fit2[[4]] <- lm(MOMENTUM4 ~ BM4, data = port_size_BM_momentum)
fit2[[5]] <- lm(MOMENTUM4 ~ BM5, data = port_size_BM_momentum)
fit2[[6]] <- lm(MOMENTUM4 ~ BM6, data = port_size_BM_momentum)
fit2[[7]] <- lm(MOMENTUM4 ~ BM7, data = port_size_BM_momentum)
fit2[[8]] <- lm(MOMENTUM4 ~ BM8, data = port_size_BM_momentum)
fit2[[9]] <- lm(MOMENTUM4 ~ BM9, data = port_size_BM_momentum)
fit2[[10]] <- lm(MOMENTUM4 ~ BM10, data = port_size_BM_momentum)

stargazer(fit2,
          type="text",
          keep.stat=c("n", "rsq", "adj.rsq"),
          report=('vc*t'),
          out = "C:/Users/Fredrik Hopland/Desktop/FinancialEconometrics/Assignments/Assignment 2//Test4.html")

################################################################################
                                #Save coefficients
################################################################################
coef.vec <- rep(NA, 5)
coef.vec[1] <- fit$coefficient

coef <- summary(lm(MOMENTUM4 ~ ., port_size_BM_momentum))$coeffisients
fit2$coefficients

