
#Importing the file
cc <- read.csv('file:///E:/ANALYTIXLABS/DATA SCIENCE USING R/BA CLASSES/CLASS 10 & 11/CREDIT CARD CS/CC GENERAL.csv')
#Descriptive statistics
summary(cc) 
#UDF to see the data distributon
mystat <- function(x){
  a <- x[!is.na(x)]
  mean <- mean(a)
  sd <- sd(a)
  min <- min(a)
  p1 <- quantile(a,.01)
  p5 <- quantile(a,.05)
  p10 <- quantile(a,.10)
  p25 <- quantile(a,.25)
  p50 <- quantile(a,.50)
  p75 <- quantile(a,.75)
  p90<- quantile(a,.90)
  p95<- quantile(a,.95)
  p99<- quantile(a,.99)
  uc <- mean+3*sd
  lc <- mean-3*sd
  max <- max(a)
  outlier <- max>uc|min<lc
  return(c(mean=mean,sd=sd,min=min,p1,p5,p10,p25,p50,p75,p90,p95,p99,max=max,uc=uc,lc=lc,outlier=outlier))
  
}

cc1 <- cc[,c(2:18)]
#Applying UDF using sapply
diag_stats <- t(data.frame(sapply(cc1,FUN = mystat)))
View(diag_stats)

#capping outliers 
M1_fun <- function(x){
  quantiles <- quantile( x, c(0.05,0.95 ),na.rm=TRUE )
  x[ x < quantiles[1] ] <- quantiles[1]
  x[ x > quantiles[2] ] <- quantiles[2]
  x
}
cc1 <- data.frame(apply(cc1[,],2,FUN=M1_fun))



#missing values imputation
#install.packages('Hmisc')
require(Hmisc)

cc1 <- data.frame(apply(cc1,2,function(x) impute(x,mean)))

#decile <- quantile(cc$MINIMUM_PAYMENTS,seq(0.1,0.9,0.1),na.rm=TRUE)
#min_pay_decile <- findInterval(cc$MINIMUM_PAYMENTS,c(-Inf,decile,Inf))
#create factor n then group by
#cc$MINIMUM_PAYMENTS <- impute(min_pay_decile,mean)
#sum(is.na(cc$MINIMUM_PAYMENTS))



#decile3 <- quantile(cc$CREDIT_LIMIT,seq(0.1,0.9,0.1),na.rm=TRUE)
#credit_limit_decile <- findInterval(cc$CREDIT_LIMIT,c(-Inf,decile,Inf))
#cc$CREDIT_LIMIT <- impute(credit_limit_decile,mean)
#sum(is.na(cc$CREDIT_LIMIT))
apply(is.na(cc1),2,sum)
cc2 <- cc1

#derived variables
cc2$MONTHLY_AVG_PURCHASE <- cc2$PURCHASES/cc2$TENURE
cc2$MONTHLY_CASH_ADVANCE <- cc2$CASH_ADVANCE/cc2$TENURE      #how do i do it with mutate fn at once?
cc2$limit_usage <- cc2$BALANCE/cc2$CREDIT_LIMIT

cc2$purchase_type <- ifelse(cc2$ONEOFF_PURCHASES==0 & cc2$INSTALLMENTS_PURCHASES==0,'NONE',
                            ifelse(cc2$ONEOFF_PURCHASES>0 & cc2$INSTALLMENTS_PURCHASES==0,'one_of',
                                   ifelse(cc2$ONEOFF_PURCHASES==0 & cc2$INSTALLMENTS_PURCHASES>0,'installment',
                                          ifelse(cc2$ONEOFF_PURCHASES>0 & cc2$INSTALLMENTS_PURCHASES>0,'both','NA'))))
cc2$purchase_type_none <- ifelse(cc2$purchase_type=='NONE',1,0)
cc2$purchase_type_one_of <- ifelse(cc2$purchase_type=='one_of',1,0)
cc2$purchase_type_installment <- ifelse(cc2$purchase_type=='installment',1,0)
cc2$purchase_type_both <- ifelse(cc2$purchase_type=='both',1,0)




cc2$pay_ratio <- cc2$PAYMENTS/cc2$MINIMUM_PAYMENTS    
cc2$TENURE <- as.numeric(cc2$TENURE)
cc2 <- cc2[,-21]
cc2$avg_amt_per_purchase <- ifelse(cc2$PURCHASES_TRX==0,0, cc2$PURCHASES/cc2$PURCHASES_TRX)
cc2$avg_amt_per_cash_advance <- ifelse(cc2$CASH_ADVANCE_TRX==0,0,cc2$CASH_ADVANCE/cc2$CASH_ADVANCE_TRX)



#correlation matrix
corrm <- cor(cc1)
#cc1 <- cc[,-c(11,17)]


require(psych)
require(GPArotation)

# deciding number of factors using eigen values
eigen(corrm)$values

require(dplyr)

eigen_values <- mutate(data.frame(eigen(corrm)$values)
                       ,cum_sum_eigen=cumsum(eigen.corrm..values)
                       , pct_var=eigen.corrm..values/sum(eigen.corrm..values)
                       , cum_pct_var=cum_sum_eigen/sum(eigen.corrm..values))
#or

eigen_values <- mutate(data.frame(eigen(corrm)$values)
                       ,cum_sum=cumsum(eigen(corrm)$values)
                       , pct_var=eigen.corrm..values/sum(eigen.corrm..values)
                       , cum_pct_var=cumsum(pct_var))

write.csv(eigen_values,'E:/R.csv')  #exporting the file
plot(eigen_values$pct_var,type = 'b') 

FA <- fa(corrm,6,rotate = 'varimax',fm='ml')  #factor analysis
print(FA)

FA_SORT <- fa.sort(FA)   #sorting the loadings

ls(FA_SORT)
FA_SORT$loadings
loadings <- data.frame(FA_SORT$loadings[1:ncol(cc1),])
write.csv(loadings,'E:/cc segmentation.csv')

#SEGMENTATION

data_final <- data.frame(scale(cc1))   #scaling the data to the same scale

cluster <- c('PURCHASES_INSTALLMENTS_FREQUENCY',
             'PURCHASES_FREQUENCY',
             'ONEOFF_PURCHASES',
             'PAYMENTS',
             'CASH_ADVANCE_TRX',
             'TENURE',
             'BALANCE',
             'PRC_FULL_PAYMENT',
             'ONEOFF_PURCHASES_FREQUENCY')
        
             
             


data_final1 <- data_final[cluster]

#building clusters using k-means

cluster3 <- kmeans(data_final1,3)
cluster4 <- kmeans(data_final1,4)
cluster5 <- kmeans(data_final1,5)
cluster6 <- kmeans(data_final1,6)
cluster7 <- kmeans(data_final1,7)
cluster8 <- kmeans(data_final1,8)

cluster3$cluster

new_final <- cbind(cc2,cluster3=cluster3$cluster,cluster4=cluster4$cluster,cluster5=cluster5$cluster,cluster6=cluster6$cluster,cluster7=cluster7$cluster,cluster8=cluster8$cluster)

##PROFILING##
#converting into factors
new_final$cluster3=factor(new_final$cluster3)
new_final$cluster4=factor(new_final$cluster4)
new_final$cluster5=factor(new_final$cluster5)
new_final$cluster6=factor(new_final$cluster6)
new_final$cluster7=factor(new_final$cluster7)
new_final$cluster8=factor(new_final$cluster8)

#install.packages('tables')
require(tables)

profile <- tabular(1+BALANCE+BALANCE_FREQUENCY+PURCHASES+
                     ONEOFF_PURCHASES+INSTALLMENTS_PURCHASES+CASH_ADVANCE+
                     PURCHASES_FREQUENCY+ONEOFF_PURCHASES_FREQUENCY+PURCHASES_INSTALLMENTS_FREQUENCY+
                     CASH_ADVANCE_FREQUENCY+CREDIT_LIMIT+PAYMENTS+
                     MINIMUM_PAYMENTS+PRC_FULL_PAYMENT+TENURE+MONTHLY_AVG_PURCHASE+
                     MONTHLY_CASH_ADVANCE+limit_usage+purchase_type_none+purchase_type_one_of+purchase_type_installment+purchase_type_both+pay_ratio ~ mean +
                     (mean*new_final$cluster3)+(mean*new_final$cluster4)+(mean*new_final$cluster5)+
                     (mean*new_final$cluster6)+(mean*new_final$cluster7)+(mean*new_final$cluster8),data=new_final)
profile1 <- as.matrix(profile)
profile1 <- data.frame(profile1)

profile2 <- tabular(1~length+(length*new_final$cluster3)+(length*new_final$cluster4)+(length*new_final$cluster5)+
                      (length*new_final$cluster6)+(length*new_final$cluster7)+(length*new_final$cluster8),data=new_final)
profile3 <- as.matrix(profile2)
profile3 <- data.frame(profile3)

#exporting the files
write.csv(profile1,"profile1.csv",row.names = FALSE)
write.csv(profile3,"profile3.csv",row.names = FALSE)














