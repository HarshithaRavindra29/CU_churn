library(data.table)
clust_data = fread("cust_data_cluster.csv")
Pred_data = fread("member_w_pred.csv")

clustNpred = merge(clust_data,Pred_data,"Member_ID")

clustNpred[,To_target := ifelse(Cust_type=="Active" & churnProb == "Churn", ifelse(customer_segment==4,
 "Highest_Priority",ifelse(customer_segment==3,"High_Priority", ifelse(customer_segment==2,
    "Average","Target_last"))),"Safe")]

to_visualize = clustNpred[,.(Member_ID,RMT_score,Age_treated,EarliestMemAcctDate,ClosedDate,ZipCode_Validated,
                             loyalty_months,customer_segment,Cust_type,churnProb,To_target)]

write.csv(to_visualize,"Churn_to_viz.csv", row.names = F)
