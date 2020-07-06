library(forecast)
library(xts)
library(dplyr)
library(data.table)

unique(data$product_content_id)

d4066298<-data[data$product_content_id=="4066298",c("sold_count","price","visit_count","favored_count","basket_count")]
d5926527$visit_sold<-d5926527$visit_count/d5926527$sold_count
d5926527$visit_sold_lag2<-shift(d5926527$visit_count,2)/d5926527$sold_count
mean(d32939029$visit_sold_lag2,na.rm = TRUE)
plot(d32939029$visit_sold_lag2)
plot(d32939029$sold_count,type="l",col="red")
par(new=TRUE)
lines(d32939029$prediction_by_visit,col="green")
##the ratio does not seem to be very stable over time. However, it is more stable than the sales
##using this value as predictor:

d5926527$visit_sold_lag2<-gsub(d5926527$visit_sold_lag2,pattern=Inf,replacement=NA)
d5926527$visit_sold_lag2<-gsub(d5926527$visit_sold_lag2,pattern=NaN,replacement=NA)
d5926527$visit_sold_lag2<-as.numeric(d5926527$visit_sold_lag2)
mean(d5926527$visit_sold_lag2,na.rm = TRUE)

d5926527$prediction_by_visit<-d5926527$visit_count/114.6968


##############################
d32939029<-data[data$product_content_id=="32939029",c("sold_count","price","visit_count","favored_count","basket_count")]
d32939029$visit_sold<-d32939029$visit_count/d32939029$sold_count
d32939029$visit_sold_lag2<-shift(d32939029$visit_count,2)/d32939029$sold_count

plot(d32939029$visit_sold_lag2)
plot(d32939029$sold_count)
acf(d32939029$sold_count)

d32939029$visit_sold_lag2<-gsub(d32939029$visit_sold_lag2,pattern=Inf,replacement=NA)
d32939029$visit_sold_lag2<-gsub(d32939029$visit_sold_lag2,pattern=NaN,replacement=NA)
d32939029$visit_sold_lag2<-as.numeric(d32939029$visit_sold_lag2)
mean(d32939029$visit_sold_lag2,na.rm = TRUE)

d32939029$prediction_by_visit<-d32939029$visit_count/83.37372


mape<-0
n<-0
for (i in rownames(d32939029)){
if(d32939029[i,"prediction_by_visit"]!=0&d32939029[i,"sold_count"]!=0){
  n<-n+1
  mape<-mape+(abs(d32939029[i,"prediction_by_visit"]-d32939029[i,"sold_count"]))
}
}
mape<-mape/n

plot(d32939029$sold_count/d32939029$favored_count)
plot(x = d32939029$price,y=d32939029$sold_count)

plot(d32939029$sold_count)
d32939029[,c("visit_count","favored_count","basket_count","price")]<-lapply(d32939029[,c("visit_count","favored_count","basket_count","price")],as.numeric)
aa_model_reg_d32939029<-auto.arima(d32939029$sold_count,xreg = cbind(d32939029[,"visit_count"],d32939029[,"price"]))
summary(aa_model_reg_d32939029)
aa_model_d32939029<-auto.arima(d32939029$sold_count,)
plot(aa_model_d32939029$fitted)


d32939029$prev_ratio1<-shift(d5926527$visit_count,3)/shift(d5926527$sold_count,1)
d32939029$prev_ratio2<-shift(d5926527$visit_count,4)/shift(d5926527$sold_count,2)
d32939029$current_ratio1<-(d32939029$prev_ratio1+d32939029$prev_ratio2)/2
