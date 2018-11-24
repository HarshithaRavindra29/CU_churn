# -------------------
# Title : Churn Analysis
# Author: Harshitha Ravindra
# Date: Nov 2, 2018
# Analysis : Clustering - Kmeans
# -------------------
library(data.table)
library(lubridate)
library(dplyr)
Cust_data = read.csv("Member Dataset.csv")
Cust_data = as.data.table(Cust_data)

#Using only July metrics for clustering as some of them have churned in the subsequent months

norm_func = function(coln){
  val = (coln - mean(coln))/(sd(coln))
  return(val)
}
Cust_data[is.na(Cust_data)] = 0

Cust_data[,norm_jul_tr:= norm_func(July.Trans)]
Cust_data[,norm_jul_sav:= norm_func(Checking+Savings+CD+IRA+Money.Market)]
Cust_data[,norm_jul_ln:= norm_func(Visa+Mortgage+Home.Equity+Other.Loan+Vehicle)]

Cust_data[,Cust_type := ifelse(ClosedDate == "", "Active", "Churn")]


#Duration
Cust_data[,loyalty_months := ifelse(Cust_type== "Active",
                                 interval(mdy(EarliestMemAcctDate), mdy("10-01-2018")) %/% months(1),
                                 interval(mdy(EarliestMemAcctDate), mdy(ClosedDate)) %/% months(1))]

Cust_data[,norm_months:= norm_func(loyalty_months)]

#can add more variable, instead of Zip- can use number of branches in the zip or distance to the nearest zip

Cust_data[,RMT_score := norm_jul_tr+norm_jul_sav+norm_jul_ln+norm_months]
#cluster


#Clustering based on RMT score
clust = kmeans(Cust_data[,.(RMT_score)],4)
Cust_data[,Cust_clust := clust$cluster]

Cust_data[, rmt_avg := mean(RMT_score), by = (Cust_clust)]
Cust_data[,customer_segment := dense_rank(rmt_avg)]
member_clust = Cust_data[,.(Member_ID,RMT_score,customer_segment)]
write.csv(member_clust,"cust_data_cluster.csv", row.names = F)
