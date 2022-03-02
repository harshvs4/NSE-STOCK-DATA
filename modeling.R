#import the required packages
library(httr)
library(jsonlite)
library(dplyr)
library(rvest)
library(utils)
library(stats)
library(caTools)
library(tidyverse)
library(htm2txt)


#Reading the file
data <- read.csv("https://raw.githubusercontent.com/anthoniraj/dsr_datasets_final/main/19BDS0139.csv")


#Cleaning and Preprocessing
data<- select(data,c('timestamp','symbol','open_price','high_price','low_price','last_traded_price','prev_close_price','percent_change','traded_quantity','turnover_in_lakhs'))
data<-na.omit(data)
data$timestamp<-as.numeric(as.POSIXct(data$timestamp,format="%d-%m- %Y %H:%M"))
data$timestamp
symbols<-c(data$symbol)
symbols<-sort(symbols)
symbols<-unique(symbols)
factors<-factor(symbols)
length(symbols)


#the label part is decided after the above code is compiled
label<-c(1:46)
data$symbol<-factor(data$symbol,levels=symbols,label=label)
head(data$symbol)


#linear model
linear_model<- lm(high_price~open_price+low_price+last_traded_price+prev_close_price+percent_change+timestamp+symbol,data=data)
summary(linear_model)


#prediction values
open_price<-8035
low_price<-8015.5
ltp<-8128.75
pcp<-8050.35
pchange<-0.97
t<-291357


#stock name
stock<-"MARUTI"
sym<-toString(which(symbols==stock))


#prediction
pred<- predict(linear_model,newdata=data.frame(open_price=open_price,low_price=low_price,last_traded_price=ltp,prev_close_price=pcp,percent_change=pchange,timestamp=t,symbol=sym))


#results
print(paste0("open price of maruti stock:",open_price))
print(paste0("low_price of maruti stock:",low_price))
print(paste0("last traded price of the maruti stock",ltp))
print(paste0("previous close price of the maruti stock",pcp))
print(paste0("percent change of the maruti stock",pchange))
print(paste0("timestamp:",as.POSIXct(t,origin="1970-01-01")))
print(paste0("predicted high price:",pred))



#MODEL EVALUATION
#Encoding for dataset
symbols<- c(data$symbol)
symbols<-sort(symbols)
symbols<-unique(symbols)
factors<-factor(symbols)
label<-c(1:46)
data$symbol<-factor(data$symbol,levels=symbols,label=label)
head(data$symbol)


#Train and Test Set into a 80-20 ratio(80:20 split is the standard split size)
split<-sample.split(data,SplitRatio=0.8)
train_set<-subset(data,split==TRUE)
test_set<-subset(data,split==FALSE)
model_train<-
lm(high_price~open_price+low_price+last_traded_price+prev_close_price+percent_change+timestamp+symbol, data = train_set)
summary(model_train)
pred<-predict(model_train,newdata=test_set)
price<-test_set$high_price


#metrics
d= price-pred
mse = mean((d)^2)
mae = mean(abs(d))
rmse = sqrt(mse)
rsq = 1-(sum((d)^2)/sum((price-mean(price))^2))
print(paste0("Mean Square Error.",mse))
print(paste0("Mean Absolute Error: Rs.",mae))
print(paste0("Root Mean Square Error: Rs.", rmse))
print(paste0("R-Squared Error:",rsq))
