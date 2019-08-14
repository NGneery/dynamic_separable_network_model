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
load("2) Structuring Data for the Model/Datasets/Data_ready_for_regression_3ywindow.RData")

# load packages
require(mgcv)
require(parallel)

for (thres in seq(from=0,to=3,by=0.5)){
# Load the Data from step 3
load(paste("2) Structuring Data for the Model/Datasets/Data_ready_for_regression_",thres,".RData",sep=""))

#################################################################################
#################################################################################
# Preparing the data                                                            #
#################################################################################

# Redefine the response for the dissolution
d_response_<-1-d_response

# Define the state ids as factors
d_sender_id<-as.factor(d_sender_id)
d_receiver_id<-as.factor(d_receiver_id)
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
save(formation_model_c,file=paste("3) Estimating the Models/Fitted Models/formation_model_c_",thres,".RData",sep=""))



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


save(formation_model,file=paste("3) Estimating the Models/Fitted Models/formation_model_",thres,".RData",sep=""))


# Dissolution with constant coefficients
dissolution_model_c<-bam(d_response_~
                           +s(d_time, d_sender_id,bs="fs",m=1)
                         +s(d_time, d_receiver_id,bs="fs",m=1)
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
                         ,family=binomial(link = "logit"),discrete=TRUE,nthreads=20)

save(dissolution_model_c,file=paste("3) Estimating the Models/Fitted Models/dissolution_model_c_",thres,".RData",sep=""))


knots<-65
spec<-c(2,1)
# Dissolution with time-varying coefficients
dissolution_model<-bam(d_response_~
                         +s(d_time, d_sender_id,bs="fs",m=1)
                       +s(d_time, d_receiver_id,bs="fs",m=1)
                       +s(d_time,by=d_dist_ij,bs="ps",k=knots,m=spec)
                       +s(d_time,by=d_defagr_ij,bs="ps",k=knots,m=spec)
                       +s(d_time,by=d_recip_ji,bs="ps",k=knots,m=spec)
                       +s(d_time,by=d_trans_ij,bs="ps",k=knots,m=spec)
                       +s(d_time,by=d_share_sup_ij,bs="ps",k=knots,m=spec)
                       +s(d_time,by=d_poldiff_ij,bs="ps",k=knots,m=spec)
                       +s(d_time,by=d_outdeg_i,bs="ps",k=knots,m=spec)
                       +s(d_time,by=d_outdeg_j,bs="ps",k=knots,m=spec)
                       +s(d_time,by=d_lgdp_i,bs="ps",k=knots,m=spec)
                       +s(d_time,by=d_lgdp_j,bs="ps",k=knots,m=spec)
                       +s(d_time,by=d_military_ex_i,bs="ps",k=knots,m=spec)
                       +s(d_time,by=d_military_ex_j,bs="ps",k=knots,m=spec)
                       
                       ,family=binomial(link = "logit"),discrete=TRUE,nthreads=20)
save(dissolution_model,file=paste("3) Estimating the Models/Fitted Models/dissolution_model_",thres,".RData",sep=""))


}
rm(list=ls())


