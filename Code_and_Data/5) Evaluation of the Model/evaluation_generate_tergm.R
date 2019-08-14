#################################################################################
#################################################################################
# Create out-of-Sample-Forecasts                                                #
#################################################################################

# remove the old stuff
rm(list=ls())


# Set working Directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
setwd("../")
load("2) Structuring Data for the Model/Datasets/Data_ready_for_tergm_regression_0.RData")

# load packages

library(mgcv)



f_sender_id<-as.factor(f_sender_id)
f_receiver_id<-as.factor(f_receiver_id)
# Create Dataframes for easy handylign with t
f_data<-data.frame(f_response,f_time,f_sender_id,f_receiver_id,f_dist_ij,f_defagr_ij,f_indeg_i,f_indeg_j,f_lgdp_i,f_lgdp_j,f_military_ex_i,f_military_ex_j,f_outdeg_i,f_outdeg_j,f_poldiff_ij,f_polity_i,f_polity_j,f_recip_ji,f_revtrans_ij,f_share_cust_ij,f_share_sup_ij,f_trans_ij,f_lag)

tergm_raneff<-list()
tergm_no_raneff<-list()

tergm_raneff_lag<-list()
tergm_no_raneff_lag<-list()

tergm_lag<-list()

# predict t+1 based on t, t-1
for (t in 1951:2015){
  f_select<-f_data[f_data[,2]%in%c(t),]
  
  formation_model<-bam(f_response~
                         +s(f_sender_id,bs="re")
                       +s(f_receiver_id,bs="re")
                       +f_dist_ij
                       +f_defagr_ij
                       +f_recip_ji
                       +f_trans_ij
                       +f_share_sup_ij
                       +f_poldiff_ij
                       +f_outdeg_i
                       +f_outdeg_j
                       +f_lgdp_i
                       +f_lgdp_j
                       +f_military_ex_i
                       +f_military_ex_j
                       ,data=f_select,family=binomial(link = "logit"))#,discrete=TRUE,nthreads=50)
  

  
  predict_set<-f_data[f_data[,2]%in%c(t+1),]
  
  predict_set<-predict_set[which(predict_set[,4]%in%f_select[,4]),]
  predict_set<-predict_set[which(predict_set[,3]%in%f_select[,3]),]
  
  
  tergm_raneff[[t-1950]]<-cbind(predict(formation_model,newdata=predict_set,type="response"),predict_set[,c(1,3,4)])
  
  
  #########
  
  formation_model<-bam(f_response~

                       +f_dist_ij
                       +f_defagr_ij
                       +f_recip_ji
                       +f_trans_ij
                       +f_share_sup_ij
                       +f_poldiff_ij
                       +f_outdeg_i
                       +f_outdeg_j
                       +f_lgdp_i
                       +f_lgdp_j
                       +f_military_ex_i
                       +f_military_ex_j
                       ,data=f_select,family=binomial(link = "logit"))#,discrete=TRUE,nthreads=50)
  
  
  
  predict_set<-f_data[f_data[,2]%in%c(t+1),]
  
  predict_set<-predict_set[which(predict_set[,4]%in%f_select[,4]),]
  predict_set<-predict_set[which(predict_set[,3]%in%f_select[,3]),]
  
  
  tergm_no_raneff[[t-1950]]<-cbind(predict(formation_model,newdata=predict_set,type="response"),predict_set[,c(1,3,4)])
  
  ################
  
  formation_model<-bam(f_response~
                         +s(f_sender_id,bs="re")
                       +s(f_receiver_id,bs="re")
                       +f_lag
                       +f_dist_ij
                       +f_defagr_ij
                       +f_recip_ji
                       +f_trans_ij
                       +f_share_sup_ij
                       +f_poldiff_ij
                       +f_outdeg_i
                       +f_outdeg_j
                       +f_lgdp_i
                       +f_lgdp_j
                       +f_military_ex_i
                       +f_military_ex_j
                       ,data=f_select,family=binomial(link = "logit"))#,discrete=TRUE,nthreads=50)
  
  
  
  predict_set<-f_data[f_data[,2]%in%c(t+1),]
  
  predict_set<-predict_set[which(predict_set[,4]%in%f_select[,4]),]
  predict_set<-predict_set[which(predict_set[,3]%in%f_select[,3]),]
  
  
  tergm_raneff_lag[[t-1950]]<-cbind(predict(formation_model,newdata=predict_set,type="response"),predict_set[,c(1,3,4)])
  
  
  #########
  
  formation_model<-bam(f_response~
                         +f_lag
                         +f_dist_ij
                       +f_defagr_ij
                       +f_recip_ji
                       +f_trans_ij
                       +f_share_sup_ij
                       +f_poldiff_ij
                       +f_outdeg_i
                       +f_outdeg_j
                       +f_lgdp_i
                       +f_lgdp_j
                       +f_military_ex_i
                       +f_military_ex_j
                       ,data=f_select,family=binomial(link = "logit"))#,discrete=TRUE,nthreads=50)
  
  
  
  predict_set<-f_data[f_data[,2]%in%c(t+1),]
  
  predict_set<-predict_set[which(predict_set[,4]%in%f_select[,4]),]
  predict_set<-predict_set[which(predict_set[,3]%in%f_select[,3]),]
  
  
  tergm_no_raneff_lag[[t-1950]]<-cbind(predict(formation_model,newdata=predict_set,type="response"),predict_set[,c(1,3,4)])
  
  
  formation_model<-bam(f_response~
                         +f_lag
                      
                       ,data=f_select,family=binomial(link = "logit"))#,discrete=TRUE,nthreads=50)
  
  
  
  predict_set<-f_data[f_data[,2]%in%c(t+1),]
  
  predict_set<-predict_set[which(predict_set[,4]%in%f_select[,4]),]
  predict_set<-predict_set[which(predict_set[,3]%in%f_select[,3]),]
  
  
  tergm_lag[[t-1950]]<-cbind(predict(formation_model,newdata=predict_set,type="response"),predict_set[,c(1,3,4)])
  
  
  ########
  print("progress, t=")
  print(t)
}




save1<-which(ls()=="tergm_lag")
save2<-which(ls()=="tergm_raneff")
save3<-which(ls()=="tergm_no_raneff")
save4<-which(ls()=="tergm_raneff_lag")
save5<-which(ls()=="tergm_no_raneff_lag")

ls()[save5]
save1<-which(ls()=="tergm_lag")
save2<-which(ls()=="tergm_raneff")
save3<-which(ls()=="tergm_no_raneff")
save4<-which(ls()=="tergm_raneff_lag")
save5<-which(ls()=="tergm_no_raneff_lag")
ls()[save5]

rm(list=ls()[-c(save1,save2,save3,save4,save5)])

save.image("5) Evaluation of the Model/Datasets/Data_for_eval_alternative.RData")
rm(list=ls())
