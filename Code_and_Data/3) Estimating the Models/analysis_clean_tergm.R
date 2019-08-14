#################################################################################
#################################################################################
# Fit the models                                                                #
#################################################################################

# remove the old stuff
rm(list=ls())

# load packages
library("mgcv")


# Set working Directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
setwd("../")

# Load the Data
load("Data.RData")
load("2) Structuring Data for the Model/Datasets/Data_ready_for_tergm_regression_0.RData")

# load packages
require(mgcv)
require(parallel)



#################################################################################
#################################################################################
# Preparing the data                                                            #
#################################################################################

f_sender_id<-as.factor(f_sender_id)
f_receiver_id<-as.factor(f_receiver_id)

#################################################################################
#################################################################################
# Estimation Section                                                            #
#################################################################################


# Formation with constant effects
formation_model_c<-bam(f_response~1+
                         +s(f_time, f_sender_id,bs="fs",m=1)
                       +s(f_time, f_receiver_id,bs="fs",m=1)
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
                       ,family=binomial(link = "logit"),discrete=TRUE,nthreads=20)
save(formation_model_c,file="3) Estimating the Models/Fitted Models/tergm_model_c.RData")



# Formation with time-varying coefficients
knots<-65
spec<-c(2,1)

formation_model<-bam(f_response~
                       +s(f_time, f_sender_id,bs="fs",m=1)
                     +s(f_time, f_receiver_id,bs="fs",m=1)
                     +s(f_time,by=f_dist_ij,bs="ps",k=knots,m=spec)
                     +s(f_time,by=f_defagr_ij,bs="ps",k=knots,m=spec)
                     +s(f_time,by=f_recip_ji,bs="ps",k=knots,m=spec)
                     +s(f_time,by=f_trans_ij,bs="ps",k=knots,m=spec)
                     +s(f_time,by=f_share_sup_ij,bs="ps",k=knots,m=spec)
                     +s(f_time,by=f_poldiff_ij,bs="ps",k=knots,m=spec)
                     +s(f_time,by=f_outdeg_i,bs="ps",k=knots,m=spec)
                     +s(f_time,by=f_outdeg_j,bs="ps",k=knots,m=spec)
                     +s(f_time,by=f_lgdp_i,bs="ps",k=knots,m=spec)
                     +s(f_time,by=f_lgdp_j,bs="ps",k=knots,m=spec)
                     +s(f_time,by=f_military_ex_i,bs="ps",k=knots,m=spec)
                     +s(f_time,by=f_military_ex_j,bs="ps",k=knots,m=spec)
                     ,family=binomial(link = "logit"),discrete=TRUE,nthreads=20)


save(formation_model,file="3) Estimating the Models/Fitted Models/tergm_model.RData")


# Formation with constant effects
formation_model_c<-bam(f_response~1+
                         +s(f_time, f_sender_id,bs="fs",m=1)
                       +s(f_time, f_receiver_id,bs="fs",m=1)
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
                       ,family=binomial(link = "logit"),discrete=TRUE,nthreads=20)
save(formation_model_c,file="3) Estimating the Models/Fitted Models/tergm_model_c.RData")



# Formation with time-varying coefficients
knots<-65
spec<-c(2,1)

formation_model<-bam(f_response~
                       +s(f_time, f_sender_id,bs="fs",m=1)
                     +s(f_time, f_receiver_id,bs="fs",m=1)
                     +s(f_time,by=f_dist_ij,bs="ps",k=knots,m=spec)
                     +s(f_time,by=f_defagr_ij,bs="ps",k=knots,m=spec)
                     +s(f_time,by=f_recip_ji,bs="ps",k=knots,m=spec)
                     +s(f_time,by=f_trans_ij,bs="ps",k=knots,m=spec)
                     +s(f_time,by=f_share_sup_ij,bs="ps",k=knots,m=spec)
                     +s(f_time,by=f_poldiff_ij,bs="ps",k=knots,m=spec)
                     +s(f_time,by=f_outdeg_i,bs="ps",k=knots,m=spec)
                     +s(f_time,by=f_outdeg_j,bs="ps",k=knots,m=spec)
                     +s(f_time,by=f_lgdp_i,bs="ps",k=knots,m=spec)
                     +s(f_time,by=f_lgdp_j,bs="ps",k=knots,m=spec)
                     +s(f_time,by=f_military_ex_i,bs="ps",k=knots,m=spec)
                     +s(f_time,by=f_military_ex_j,bs="ps",k=knots,m=spec)
                     ,family=binomial(link = "logit"),discrete=TRUE,nthreads=20)


save(formation_model,file="3) Estimating the Models/Fitted Models/tergm_model.RData")



rm(list=ls())


