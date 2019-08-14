#################################################################################
#################################################################################
# Create out-of-Sample-Forecasts                                                #
#################################################################################

# remove the old stuff
rm(list=ls())


# Set working Directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
setwd("../")
load("2) Structuring Data for the Model/Datasets/Data_ready_for_regression.RData")

# load packages

library(mgcv)


d_response_<-1-d_response

d_sender_id<-as.factor(d_sender_id)
d_receiver_id<-as.factor(d_receiver_id)

f_sender_id<-as.factor(f_sender_id)
f_receiver_id<-as.factor(f_receiver_id)
# Create Dataframes for easy handylign with t
f_data<-data.frame(f_response,f_time,f_sender_id,f_receiver_id,f_defagr_ij,f_indeg_i,f_indeg_j,f_lgdp_i,f_lgdp_j,f_military_ex_i,f_military_ex_i,f_military_ex_j,f_outdeg_i,f_outdeg_j,f_poldiff_ij,f_polity_i,f_polity_j,f_recip_ji,f_revtrans_ij,f_share_cust_ij,f_share_sup_ij,f_trans_ij,f_dist_ij)
d_data<-data.frame(d_response_,d_time,d_sender_id,d_receiver_id,d_defagr_ij,d_indeg_i,d_indeg_j,d_lgdp_i,d_lgdp_j,d_military_ex_i,d_military_ex_i,d_military_ex_j,d_outdeg_i,d_outdeg_j,d_poldiff_ij,d_polity_i,d_polity_j,d_recip_ji,d_revtrans_ij,d_share_cust_ij,d_share_sup_ij,d_trans_ij,d_dist_ij)

formation_roc<-list()
dissolution_roc<-list()

model_form<-list()
model_diss<-list()


for (t in 1951:2015){
  f_select<-f_data[f_data[,2]%in%c(t),]
  
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
  
  
  formation_roc[[t-1950]]<-cbind(predict(formation_model,newdata=predict_set,type="response"),predict_set[,c(1,3,4)])
  
  
  d_select<-d_data[d_data[,2]%in%c(t),]
  
  
  dissolution_model<-bam(d_response_~
                       +d_dist_ij
                     +d_defagr_ij
                     +d_recip_ji
                     +d_trans_ij
                     +d_share_sup_ij
                     +d_poldiff_ij
                     +d_outdeg_i
                     +d_outdeg_j
                     +d_lgdp_i
                     +d_lgdp_j
                     +d_military_ex_i
                     +d_military_ex_j
                         ,data=d_select,family=binomial(link = "logit"))#,discrete=TRUE,nthreads=40)
  
  
  predict_set<-d_data[d_data[,2]%in%c(t+1),]
  
  predict_set<-predict_set[which(predict_set[,4]%in%d_select[,4]),]
  predict_set<-predict_set[which(predict_set[,3]%in%d_select[,3]),]
  
  
  dissolution_roc[[t-1950]]<-cbind(predict(dissolution_model,newdata=predict_set,type="response"),predict_set[,c(1,3,4)])
  
  print("progress, t=")
  print(t)
}



formation_nonet<-formation_roc
dissolution_nonet<-dissolution_roc

formation_roc2<-formation_roc
dissolution_roc2<-dissolution_roc


save1<-which(ls()=="formation_roc2")
save2<-which(ls()=="dissolution_roc2")
save3<-which(ls()=="net_in_t")
save4<-which(ls()=="net_in_t_1")

ls()[save2]
rm(list=ls()[-c(save1,save2,save3,save4)])

save.image("5) Evaluation of the Model/Datasets/Data_for_eval_noraneff.RData")
rm(list=ls())
