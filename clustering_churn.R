# -------------------
# Title : Churn Analysis
# Author: Harshitha Ravindra
# Date: Nov 2, 2018
# Analysis : Clustering - Kmeans
# -------------------
library(data.table)
library(cluster)
library(factoextra)
library(NbClust)

Cust_data = fread("CU_EDA_variables.csv")

#Using only July metrics for clustering as some of them have churned in the subsequent months

norm_func = function(coln){
  val = (coln - mean(coln))/(sd(coln))
  return(val)
}

Cust_data[,norm_jul_tr:= norm_func(July.Trans)]
Cust_data[,norm_jul_sav:= norm_func(Savings_jul)]
Cust_data[,norm_jul_ln:= norm_func(Loan_jul)]
Cust_data[,norm_months:= norm_func(loyalty_months)]

#can add more variable, instead of Zip- can use number of branches in the zip or distance to the nearest zip

Cust_data[,RMT_score := norm_jul_tr+norm_jul_sav+norm_jul_ln+norm_months]
#cluster

# Elbow method
# fviz_nbclust(Cust_data[,.(norm_months, norm_jul_tr, norm_jul_sav, norm_jul_ln)], kmeans, method = 'wss') +
#   geom_vline(xintercept = 4, linetype = 2)+
#   labs(subtitle = 'Elbow method')


#clust = kmeans(Cust_data[,.(norm_months, norm_jul_tr, norm_jul_sav, norm_jul_ln)],5)
#Cust_data[,Cust_clust := clust$cluster]
#plot(Cust_data$RMT_score , Cust_data$Cust_clust)
#clust_summary = Cust_data[,.N, by= .(Age_bin,Cust_type,Cust_clust)]


#Clustering based on RMT score
clust = kmeans(Cust_data[,.(RMT_score)],4)
Cust_data[,Cust_clust := clust$cluster]
