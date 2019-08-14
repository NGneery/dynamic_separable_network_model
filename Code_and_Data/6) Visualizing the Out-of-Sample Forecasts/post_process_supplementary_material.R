#################################################################################
#################################################################################
# Visualization of the Out-of-Sample-Forecasts                                  #
#################################################################################

# Post Process the data
rm(list=ls())
# Set working Directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
setwd("../")
load("2) Structuring Data for the Model/Datasets/Data_ready_for_tergm_regression_0.RData")

# 

# Load the Data from step 1
load("5) Evaluation of the Model/Datasets/Data_for_eval.RData")
load("5) Evaluation of the Model/Datasets/Data_for_eval_noraneff.RData")
load("5) Evaluation of the Model/Datasets/Data_for_eval_alternative.RData")

# packages
library(igraph)
library(ggplot2)
library(PRROC)
library(dplyr)
#---------------------------------------------------#
# Combine formation and  dissolution into one model
#---------------------------------------------------#

stergm_with_raneff<-list()
stergm_without_raneff<-list()

for (i in 1:length(formation_roc)){
  colnames(formation_roc[[i]])<-c("predict","response","sender_id","receiver_id")
  colnames(dissolution_roc[[i]])<-c("predict","response","sender_id","receiver_id")
  stergm_with_raneff[[i]]<-rbind(formation_roc[[i]],dissolution_roc[[i]])
  
  colnames(formation_roc2[[i]])<-c("predict","response","sender_id","receiver_id")
  colnames(dissolution_roc2[[i]])<-c("predict","response","sender_id","receiver_id")
  stergm_without_raneff[[i]]<-rbind(formation_roc2[[i]],dissolution_roc2[[i]])
}


# Area under the curve for the formation

pr_auc_stergm_raneff<-c()
pr_auc_stergm_no_raneff<-c()
pr_auc_tergm_lag<-c()
pr_auc_tergm_no_raneff<-c()
pr_auc_tergm_no_raneff_lag<-c()
pr_auc_tergm_raneff_lag<-c()
pr_auc_tergm_raneff<-c()

roc_auc_stergm_raneff<-c()
roc_auc_stergm_no_raneff<-c()
roc_auc_tergm_lag<-c()
roc_auc_tergm_no_raneff<-c()
roc_auc_tergm_no_raneff_lag<-c()
roc_auc_tergm_raneff_lag<-c()
roc_auc_tergm_raneff<-c()

for (q in 1:length(stergm_with_raneff)){
  # PR
  
  r<-pr.curve(scores.class0=stergm_with_raneff[[q]][,1],weights.class0=stergm_with_raneff[[q]][,2])
  pr_auc_stergm_raneff<-c(pr_auc_stergm_raneff,r$auc.integral)
  
  r<-pr.curve(scores.class0=stergm_without_raneff[[q]][,1],weights.class0=stergm_without_raneff[[q]][,2])
  pr_auc_stergm_no_raneff<-c(pr_auc_stergm_no_raneff,r$auc.integral)
  
  r<-pr.curve(scores.class0=tergm_lag[[q]][,1],weights.class0=tergm_lag[[q]][,2])
  pr_auc_tergm_lag<-c(pr_auc_tergm_lag,r$auc.integral)
  
  r<-pr.curve(scores.class0=tergm_no_raneff[[q]][,1],weights.class0=tergm_no_raneff[[q]][,2])
  pr_auc_tergm_no_raneff<-c(pr_auc_tergm_no_raneff,r$auc.integral)
  
  r<-pr.curve(scores.class0=tergm_no_raneff_lag[[q]][,1],weights.class0=tergm_no_raneff_lag[[q]][,2])
  pr_auc_tergm_no_raneff_lag<-c(pr_auc_tergm_no_raneff_lag,r$auc.integral)
  
  r<-pr.curve(scores.class0=tergm_raneff_lag[[q]][,1],weights.class0=tergm_raneff_lag[[q]][,2])
  pr_auc_tergm_raneff_lag<-c(pr_auc_tergm_raneff_lag,r$auc.integral)
    
  r<-pr.curve(scores.class0=tergm_raneff[[q]][,1],weights.class0=tergm_raneff[[q]][,2])
  pr_auc_tergm_raneff<-c(pr_auc_tergm_raneff,r$auc.integral)

  # ROC
  
  r<-roc.curve(scores.class0=stergm_with_raneff[[q]][,1],weights.class0=stergm_with_raneff[[q]][,2])
  roc_auc_stergm_raneff<-c(roc_auc_stergm_raneff,r$auc)
  
  r<-roc.curve(scores.class0=stergm_without_raneff[[q]][,1],weights.class0=stergm_without_raneff[[q]][,2])
  roc_auc_stergm_no_raneff<-c(roc_auc_stergm_no_raneff,r$auc)
  
  r<-roc.curve(scores.class0=tergm_lag[[q]][,1],weights.class0=tergm_lag[[q]][,2])
  roc_auc_tergm_lag<-c(roc_auc_tergm_lag,r$auc)
  
  r<-roc.curve(scores.class0=tergm_no_raneff[[q]][,1],weights.class0=tergm_no_raneff[[q]][,2])
  roc_auc_tergm_no_raneff<-c(roc_auc_tergm_no_raneff,r$auc)
  
  r<-roc.curve(scores.class0=tergm_no_raneff_lag[[q]][,1],weights.class0=tergm_no_raneff_lag[[q]][,2])
  roc_auc_tergm_no_raneff_lag<-c(roc_auc_tergm_no_raneff_lag,r$auc)
  
  r<-roc.curve(scores.class0=tergm_raneff_lag[[q]][,1],weights.class0=tergm_raneff_lag[[q]][,2])
  roc_auc_tergm_raneff_lag<-c(roc_auc_tergm_raneff_lag,r$auc)
  
  r<-roc.curve(scores.class0=tergm_raneff[[q]][,1],weights.class0=tergm_raneff[[q]][,2])
  roc_auc_tergm_raneff<-c(roc_auc_tergm_raneff,r$auc)
  
  print(q)
}




PR<-cbind(pr_auc_stergm_raneff
,pr_auc_stergm_no_raneff
,pr_auc_tergm_lag
,pr_auc_tergm_no_raneff
,pr_auc_tergm_no_raneff_lag
,pr_auc_tergm_raneff_lag
,pr_auc_tergm_raneff)

ROC<-cbind(roc_auc_stergm_raneff
          ,roc_auc_stergm_no_raneff
          ,roc_auc_tergm_lag
          ,roc_auc_tergm_no_raneff
          ,roc_auc_tergm_no_raneff_lag
          ,roc_auc_tergm_raneff_lag
          ,roc_auc_tergm_raneff)

prbest_t<-apply(PR,1,max)
rocbest_t<-apply(ROC,1,max)


prbest_who<-c()
rocbest_who<-c()
for (t in 1:65){
  prbest_who<-c(prbest_who,which(PR[t,]==prbest_t[t]))
  rocbest_who<-c(rocbest_who,which(ROC[t,]==rocbest_t[t]))
  }
table(prbest_who)/sum(table(prbest_who))
table(rocbest_who)
# First the low oos predictions
pdf("6) Visualizing the Out-of-Sample Forecasts/Figures/comp1.pdf",width=14,height = 6)
par(mfrow=c(1,2),mar=c(6,6,6,6))
plot(pr_auc_stergm_raneff,xaxt="n",type="l",ylab="AUC",xlab="t",cex.main=2,cex.lab=2,cex.axis=2, main="AUC, Precision Recall",lwd=2,ylim=c(0.3,0.8),col="red",xlim=c(-5,72))
grid()
axis(1,1:length(pr_auc_stergm_raneff),1952:2016,cex.axis=2)
text(-3,pr_auc_stergm_raneff[1],"Model 7")
text(69,pr_auc_stergm_raneff[65],"Model 7")
lines(pr_auc_tergm_lag,lty=2,lwd=2)
text(-3,pr_auc_tergm_lag[1],"Model 1")
text(69,pr_auc_tergm_lag[65],"Model 1")
lines(pr_auc_tergm_no_raneff,lty=3,lwd=2)
text(-3,pr_auc_tergm_no_raneff[1],"Model 2")
text(69,pr_auc_tergm_no_raneff[65],"Model 2")
lines(pr_auc_tergm_raneff,lty=4,lwd=2)
text(-3,pr_auc_tergm_raneff[1],"Model 3")
text(69,pr_auc_tergm_raneff[65],"Model 3")

plot(roc_auc_stergm_raneff,xaxt="n",type="l",ylab="AUC",xlab="t",cex.main=2,cex.lab=2,cex.axis=2, main="AUC, Receiver Operator Characteristics",lwd=2,ylim=c(0.8,1),col="red",xlim=c(-5,72))
text(-3,roc_auc_stergm_raneff[1],"Model 7")
text(69,roc_auc_stergm_raneff[65],"Model 7")
grid()
axis(1,1:length(roc_auc_stergm_raneff),1952:2016,cex.axis=2)
lines(roc_auc_tergm_lag,lty=2,lwd=2)
text(-3,roc_auc_tergm_lag[1],"Model 1")
text(69,roc_auc_tergm_lag[65],"Model 1")
lines(roc_auc_tergm_no_raneff,lty=3,lwd=2)
text(-3,roc_auc_tergm_no_raneff[1],"Model 2")
text(69,roc_auc_tergm_no_raneff[65],"Model 2")
lines(roc_auc_tergm_raneff,lty=4,lwd=2)

text(-3,roc_auc_tergm_raneff[1],"Model 3")
text(69,roc_auc_tergm_raneff[65],"Model 3")

dev.off()
##


# First the high oos predictions
pdf("6) Visualizing the Out-of-Sample Forecasts/Figures/comp2.pdf",width=14,height = 6)
par(mfrow=c(1,2),mar=c(6,6,6,6))
plot(pr_auc_stergm_raneff,xaxt="n",type="l",ylab="AUC",xlab="t",cex.main=2,cex.lab=2,cex.axis=2, main="AUC, Precision Recall",lwd=2,ylim=c(0.5,0.8),col="red",xlim=c(-5,72))
grid()
axis(1,1:length(pr_auc_stergm_raneff),1952:2016,cex.axis=2)
text(-3,pr_auc_stergm_raneff[1],"Model 7")
text(69,pr_auc_stergm_raneff[65],"Model 7")
lines(pr_auc_tergm_no_raneff_lag,lty=2,lwd=2)

text(-3,pr_auc_tergm_no_raneff_lag[1],"Model 4")
text(69,pr_auc_tergm_no_raneff_lag[65],"Model 4")
lines(pr_auc_tergm_raneff_lag,lty=3,lwd=2)
text(-3,pr_auc_tergm_raneff_lag[1],"Model 5")
text(69,pr_auc_tergm_raneff_lag[65],"Model 5")

plot(roc_auc_stergm_raneff,xaxt="n",type="l",ylab="AUC",xlab="t",cex.main=2,cex.lab=2,cex.axis=2, main="AUC, Receiver Operator Characteristics",lwd=2,ylim=c(0.95,1),col="red",xlim=c(-5,72))
grid()
axis(1,1:length(roc_auc_stergm_raneff),1952:2016,cex.axis=2)
text(-3,roc_auc_stergm_raneff[1],"Model 7")
text(69,roc_auc_stergm_raneff[65],"Model 7")

lines(roc_auc_tergm_no_raneff_lag,lty=2,lwd=2)
text(-3,roc_auc_tergm_no_raneff_lag[1],"Model 4")
text(69,roc_auc_tergm_no_raneff_lag[65],"Model 4")
lines(roc_auc_tergm_raneff_lag,lty=3,lwd=2)
text(-3,roc_auc_tergm_raneff_lag[1],"Model 5")
text(69,roc_auc_tergm_raneff_lag[65],"Model 5")

dev.off()


pdf("6) Visualizing the Out-of-Sample Forecasts/Figures/comp3.pdf",width=14,height = 6)
par(mfrow=c(1,2),mar=c(6,6,6,6))
plot(pr_auc_stergm_raneff,xaxt="n",type="l",ylab="AUC",xlab="t",cex.main=2,cex.lab=2,cex.axis=2, main="AUC, Precision Recall",lwd=2,ylim=c(0.5,0.8),col="red",xlim=c(-5,72))
grid()
axis(1,1:length(pr_auc_stergm_raneff),1952:2016,cex.axis=2)
text(-3,pr_auc_stergm_raneff[1],"Model 7")
text(69,pr_auc_stergm_raneff[65],"Model 7")

lines(pr_auc_stergm_no_raneff,lty=2,lwd=2)
text(-3,pr_auc_stergm_no_raneff[1],"Model 6")
text(69,pr_auc_stergm_no_raneff[65],"Model 6")

plot(roc_auc_stergm_raneff,xaxt="n",type="l",ylab="AUC",xlab="t",cex.main=2,cex.lab=2,cex.axis=2, main="AUC, Receiver Operator Characteristics",lwd=2,ylim=c(0.94,1),col="red",xlim=c(-5,72))
grid()
axis(1,1:length(roc_auc_stergm_raneff),1952:2016,cex.axis=2)
text(-3,roc_auc_stergm_raneff[1],"Model 7")
text(69,roc_auc_stergm_raneff[65],"Model 7")

lines(roc_auc_stergm_no_raneff,lty=2,lwd=2)
text(-3,roc_auc_stergm_no_raneff[1],"Model 6")
text(69,roc_auc_stergm_no_raneff[65],"Model 6")


dev.off()


