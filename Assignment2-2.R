# Assignment 2 Financial Econometrics 

# "Is momonetum priced in the norwegian stock market ?"

# cler workspace
rm(list=ls())

# libraries 
library(tidyverse)
library(DescTools) # winsorizing 
library(lubridate) # for ymd function
library(ggplot2)




# load data 
portfolio_size <- read.csv("https://ba-odegaard.no/financial_data/ose_asset_pricing_data/equity_size_portfolios_monthly_vw.txt")
portfolio_bm <- read.csv("https://ba-odegaard.no/financial_data/ose_asset_pricing_data/book_market_portfolios_monthly_vw.txt")
portfolio_momentum <- read.csv("https://ba-odegaard.no/financial_data/ose_asset_pricing_data/pricing_factors_monthly.txt")

summary(portfolio_size)
plot(portfolio_size$date,portfolio_size$X1..small.size.)

# --> maybe winsorize observation 14, seems very high 


summary(portfolio_bm)
plot(portfolio_bm$date,portfolio_bm$X2) # --> observation 221 
plot(portfolio_bm$date,portfolio_bm$X9) # --> observation 2 


summary(portfolio_momentum)
# seems fine 


# winsorize data: 

portfolio_size <- 
  portfolio_size %>%
  mutate(X1..small.size.= Winsorize(X1..small.size., probs = c(0.01, 0.99)))


portfolio_bm <- 
  portfolio_bm %>% 
  mutate(X2 = Winsorize(X2, probs = c(0.01,0.99)),
         X9 = Winsorize(X9, probs = c(0.01,0.99)))


# changing date variables 

portfolio_size$date <- ymd(portfolio_size$date)
portfolio_bm$date <- ymd(portfolio_bm$date)
portfolio_momentum$date <- ymd(portfolio_momentum$date)


# join datasets 

data <- 
  inner_join(portfolio_size,portfolio_bm,by="date") %>% 
  inner_join(portfolio_momentum,by="date")

data <- as.data.frame(data)

  

# task 1 

beta <- c()
avg_return <- c()

for(i in 1:10){

fit <-  
  lm(data[,(1+i)] ~ UMD,data = data)
  
beta[i] <- fit$coefficients[2]

avg_return[i] <- mean(data[,(1+i)])

}


# task 2 
# combine avg.return and coefficients 
plot_set <- as.data.frame(cbind(beta,avg_return))



plot(beta,avg_return)

plot_set %>% 
ggplot(aes(x = beta,y=avg_return))+
geom_point()



# task 3 
df <- as.data.frame(data[-c(12:24)])


gamma <- c()


test <- data.frame()
for(i in 1:nrow(df)){
  for(j in 1:10){
    test[j,i] = df[i,1+j]
  }
}

test <- cbind(test,beta)


for(i in 1:nrow(df)){
  fit2 = lm(test[,i]~beta,data=test)
  gamma[i] = fit2$coefficients[2]
}
 
 


# task 4 

fit3 <-
  lm(gamma ~ 1 )

summary(fit3)
summary(fit3)$coefficients[,3]

# without lm() 
se = sd(gamma)/sqrt(length(gamma))
tvalue = mean(gamma)/se




# task 5 


beta2 <- c()
avg_return2 <- c()


for(i in 1:10){
  
  fit4 <-  
    lm(data[,(11+i)] ~ UMD,data = data)
  
  beta2[i] <- fit4$coefficients[2]
  
  avg_return2[i] <- mean(data[,(11+i)])
  
}





# task 6
# combine avg.return and coefficients 
plot_set2 <- as.data.frame(cbind(beta2,avg_return2))



plot_set2 %>% 
  ggplot(aes(x = beta2,y=avg_return2))+
  geom_point()





# task 7 
df2 <- as.data.frame(data[-c(2:11,22:25)])


gamma2 <- c()


test2 <- data.frame()
for(i in 1:nrow(df)){
  for(j in 1:10){
    test2[j,i] = df[i,1+j]
  }
}

test <- cbind(test2,beta2)


for(i in 1:nrow(df2)){
  fit5 = lm(test2[,i]~beta2,data=test2)
  gamma2[i] = fit5$coefficients[2]
}



#task 8
fit6 <-
  lm(gamma2 ~ 1 )

summary(fit6)
summary(fit6)$coefficients[,3]

# without lm() 
se2 = sd(gamma2)/sqrt(length(gamma2))
tvalue2 = mean(gamma2)/se2
tvalue2














  
















