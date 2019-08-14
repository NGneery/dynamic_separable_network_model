#################################################################################
#################################################################################
# Visualization of the Out-of-Sample-Forecasts                                  #
#################################################################################

# Post Process the data
rm(list=ls())
# Set working Directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
setwd("../")

load("5) Evaluation of the Model/Datasets/Data_for_eval.RData")

# packages
library(igraph)
library(ggplot2)
library(PRROC)
#---------------------------------------------#
stergm<-list()
for (i in 1:length(formation_roc)){
  colnames(formation_roc[[i]])<-c("predict","response","sender_id","receiver_id")
  colnames(dissolution_roc[[i]])<-c("predict","response","sender_id","receiver_id")
  stergm[[i]]<-rbind(formation_roc[[i]],dissolution_roc[[i]])
}
#---------------------------------------------#


## Figure ???: PR Curve and AUC Time Series for Out-of-Sample-Predictions for the Formation Model (left) and the Persistence Model (right)
pdf("6) Visualizing the Out-of-Sample Forecasts/Figures/fig_combined.pdf",height = 15,width = 20)

layout(matrix(c(1,2,3,4,5,6), nrow = 3, ncol = 2, byrow = TRUE))
par(mar=c(7,7,7,7))

# Area under the curve for the formation

f_auc<-c()
for (q in 1:length(formation_roc)){
  r<-pr.curve(scores.class0=formation_roc[[q]][,1],weights.class0=formation_roc[[q]][,2])
  f_auc<-c(f_auc,r$auc.integral)
  
  print(q)
}

plot(f_auc,xaxt="n",type="l",ylab="AUC",xlab="t",cex.main=3,cex.lab=3,cex.axis=2.5,lwd=3,ylim=c(0,1),main="Precision Recall, Formation")
grid()
axis(1,1:length(f_auc),1952:2016,cex.axis=2)

#######




# Area under the curve for the formation

f_auc<-c()
for (q in 1:length(formation_roc)){
  r<-roc.curve(scores.class0=formation_roc[[q]][,1],weights.class0=formation_roc[[q]][,2])
  f_auc<-c(f_auc,r$auc)
  
  print(q)
}

plot(f_auc,xaxt="n",type="l",ylab="AUC",xlab="t",cex.main=3,cex.lab=3,cex.axis=2.5,lwd=3,ylim=c(0,1),main="Receiver Operating Characteristic, Formation")
grid()
axis(1,1:length(f_auc),1952:2016,cex.axis=2)

#######

# Area under the curve for the formation

f_auc<-c()
for (q in 1:length(formation_roc)){
  r<-pr.curve(scores.class0=dissolution_roc[[q]][,1],weights.class0=dissolution_roc[[q]][,2])
  f_auc<-c(f_auc,r$auc.integral)
  
  print(q)
}

plot(f_auc,xaxt="n",type="l",ylab="AUC",xlab="t",cex.main=3,cex.lab=3,cex.axis=2.5,lwd=3,ylim=c(0,1),main="Precision Recall, Persistence")
grid()
axis(1,1:length(f_auc),1952:2016,cex.axis=2)

# Area under the curve for the formation

f_auc<-c()
for (q in 1:length(formation_roc)){
  r<-roc.curve(scores.class0=dissolution_roc[[q]][,1],weights.class0=dissolution_roc[[q]][,2])
  f_auc<-c(f_auc,r$auc)
  
  print(q)
}

plot(f_auc,xaxt="n",type="l",ylab="AUC",xlab="t",cex.main=3,cex.lab=3,cex.axis=2.5,lwd=3,ylim=c(0,1),main="Receiver Operating Characteristic, Persistence")
grid()
axis(1,1:length(f_auc),1952:2016,cex.axis=2)


f_auc<-c()
for (q in 1:length(formation_roc)){
  r<-pr.curve(scores.class0=stergm[[q]][,1],weights.class0=stergm[[q]][,2])
  f_auc<-c(f_auc,r$auc.integral)
  
  print(q)
}

plot(f_auc,xaxt="n",type="l",ylab="AUC",xlab="t",cex.main=3,cex.lab=3,cex.axis=2.5,lwd=3,ylim=c(0,1),main="Precision Recall, Combined")
grid()
axis(1,1:length(f_auc),1952:2016,cex.axis=2)



# Area under the curve for the formation

f_auc<-c()
for (q in 1:length(formation_roc)){
  r<-roc.curve(scores.class0=stergm[[q]][,1],weights.class0=stergm[[q]][,2])
  f_auc<-c(f_auc,r$auc)
  
  print(q)
}

plot(f_auc,xaxt="n",type="l",ylab="AUC",xlab="t",cex.main=3,cex.lab=3,cex.axis=2.5,lwd=3,ylim=c(0,1),main="Receiver Operating Characteristic, Combined")
grid()
axis(1,1:length(f_auc),1952:2016,cex.axis=2)

#######

dev.off()








###########

#---------------------------------------------#
# Now have a look at the networks, the models can generate

real_statistics<-list() 
old_statistics<-list() 
simulated_statistics<-list()
centered_statistics<-list()
for (t in 1951:2015){
  # first the Stats for the real network
  
  net_real<-net_in_t[[t+1-1950]]
  net_real[net_real>0]<-1
  # Calculate network statistics
  # order
  actors<-dim(net_real)[1]
  rS<-(rowSums(net_real)>0)
  cS<-(colSums(net_real)>0)
  ident<-(rS+cS>0)
  
  order<-sum(ident)
  
  #  size
  size<-sum(net_real)
  
  # density
  density<-size/(actors^2-actors)
  
  # median outdegree
  outdeg<-rowSums(net_real)
  median_outdeg<-median(net_real)
  
  # median indegree
  indeg<-colSums(net_real)
  median_indeg<-median(indeg)
  
  # mean indegree
  outdeg<-rowSums(net_real)
  mean_indeg<-mean(outdeg)
  
  # Reciprocity
  gr<-graph_from_adjacency_matrix(net_real)
  reciprocity<-reciprocity(gr)
  
  # Transitivity
  transitivity<-transitivity(gr)
  
  
  # shotest path
  
  path<-mean_distance(gr, directed = TRUE, unconnected = TRUE)
  
  
  stats<-c(order,size,density,mean_indeg,reciprocity,transitivity,path)
  

  
  net_real<-net_in_t_1[[t+1-1950]]
  net_real[net_real>0]<-1
  # Calculate network statistics
  # order
  actors<-dim(net_real)[1]
  rS<-(rowSums(net_real)>0)
  cS<-(colSums(net_real)>0)
  ident<-(rS+cS>0)
  
  order<-sum(ident)
  
  #  size
  size<-sum(net_real)
  
  # density
  density<-size/(actors^2-actors)
  
  # median outdegree
  outdeg<-rowSums(net_real)
  median_outdeg<-median(net_real)
  
  # median indegree
  indeg<-colSums(net_real)
  median_indeg<-median(indeg)
  
  # mean indegree
  outdeg<-rowSums(net_real)
  mean_indeg<-mean(outdeg)
  
  # Reciprocity
  gr<-graph_from_adjacency_matrix(net_real)
  reciprocity<-reciprocity(gr)
  
  # Transitivity
  transitivity<-transitivity(gr)
  
  path<-mean_distance(gr, directed = TRUE, unconnected = TRUE)
  
  
  stats_old<-c(order,size,density,mean_indeg,reciprocity,transitivity,path)
  

  
  
  
  stats_sim<-c()
for (w in 1:1000){
# formation
# extract the fit

model_form<-formation_roc[[t-1950]][,1]
# simulate new ties
new_ties<-rbinom(length(model_form),1,prob=model_form)
# select the relevant countries

adding<-formation_roc[[t-1950]][which(new_ties==1),c(3,4)]
  
start<-net_in_t_1[[t+1-1950]]
start[start>0]<-1
# add the new ties
for (i in 1:dim(adding)[1]){
  sender<-which(colnames(start)==adding[i,1])
  rec<-which(colnames(start)==adding[i,2])
  start[sender, rec]<-1
}

# dissolution
# extract the fit

model_diss<-dissolution_roc[[t-1950]][,1]

# simulate dissolution
pr_dissolution<-rbinom(length(model_diss),1,prob=model_diss)
# select the relevant countries
killing<-dissolution_roc[[t-1950]][which(pr_dissolution==0),c(3,4)]

for (i in 1:dim(killing)[1]){
  sender<-which(colnames(start)==killing[i,1])
  rec<-which(colnames(start)==killing[i,2])
  start[sender, rec]<-0
}


# Calculate network statistics
# order
actors<-dim(start)[1]
rS<-(rowSums(start)>0)
cS<-(colSums(start)>0)
ident<-(rS+cS>0)

order<-sum(ident)

#  size
size<-sum(start)

# density
density<-size/(actors^2-actors)

# median outdegree
outdeg<-rowSums(start)
median_outdeg<-median(outdeg)

# median indegree
indeg<-colSums(start)
mean_indeg<-median(indeg)

# mean indegree
outdeg<-rowSums(start)
mean_indeg<-mean(outdeg)

# Reciprocity
gr<-graph_from_adjacency_matrix(start)
reciprocity<-reciprocity(gr)

# Transitivity
transitivity<-transitivity(gr)

path<-mean_distance(gr, directed = TRUE, unconnected = TRUE)


stats_sim<-cbind(stats_sim,c(order,size,density,mean_indeg,reciprocity,transitivity,path))
#print(w)
}

  
  
real_statistics[[t-1950]]<-stats  
old_statistics[[t-1950]]<-stats_old  
simulated_statistics[[t-1950]]<-stats_sim
centered_statistics[[t-1950]]<-stats_sim-stats
print(t)
}

## Figure 11: Out-ofsample-Predictions. Boxplots show the Statistics for the simulated Networks. The solid Line gives the Staitistcs for the real Networks


# Order
order<-c()
real_order<-c()
for (t in 1:length(real_statistics)){
  order<-cbind(order,simulated_statistics[[t]][1,])
  real_order<-c(real_order,real_statistics[[t]][1])
}

pdf("6) Visualizing the Out-of-Sample Forecasts/Figures/Figure11_1.pdf")

xtr<-boxplot(order,plot=F)$stats[c(1, 5), ]
boxplot(order,main="Order",xaxt="n",ylim=c(50,150))

lines(xtr[1,],col="gray",lty=2)
lines(xtr[2,],col="gray",lty=2)
axis(1,1:65,1952:2016)
lines(real_order,col="red",lwd=4)
dev.off()


# Size
order<-c()
real_order<-c()
old_order<-c()
for (t in 1:length(real_statistics)){
  order<-cbind(order,simulated_statistics[[t]][2,])
  real_order<-c(real_order,real_statistics[[t]][2])
  old_order<-c(old_order,old_statistics[[t]][2])
}

pdf("6) Visualizing the Out-of-Sample Forecasts/Figures/Figure11_2.pdf")
xtr<-boxplot(order,plot=F)$stats[c(1, 5), ]
boxplot(order,main="Size",xaxt="n",ylim=c(80,600))

lines(xtr[1,],col="gray",lty=2)
lines(xtr[2,],col="gray",lty=2)

axis(1,1:65,1952:2016)
lines(real_order,col="red",lwd=4)

dev.off()


# Density
order<-c()
real_order<-c()
old_order<-c()
for (t in 1:length(real_statistics)){
  order<-cbind(order,simulated_statistics[[t]][3,])
  real_order<-c(real_order,real_statistics[[t]][3])
  old_order<-c(old_order,old_statistics[[t]][3])
}


pdf("6) Visualizing the Out-of-Sample Forecasts/Figures/Figure11_3.pdf")
xtr<-boxplot(order,plot=F)$stats[c(1, 5), ]
boxplot(order,main="Density",xaxt="n",ylim=c(0.005,0.03))

lines(xtr[1,],col="gray",lty=2)
lines(xtr[2,],col="gray",lty=2)

axis(1,1:65,1952:2016)
lines(real_order,col="red",lwd=4)

dev.off()


# mean_indeg
order<-c()
real_order<-c()
old_order<-c()
for (t in 1:length(real_statistics)){
  order<-cbind(order,simulated_statistics[[t]][4,])
  real_order<-c(real_order,real_statistics[[t]][4])
  old_order<-c(old_order,old_statistics[[t]][4])
}


pdf("6) Visualizing the Out-of-Sample Forecasts/Figures/Figure11_4.pdf")
xtr<-boxplot(order,plot=F)$stats[c(1, 5), ]
boxplot(order,main="Mean Indegree",xaxt="n",ylim=c(0.5,3.5))

lines(xtr[1,],col="gray",lty=2)
lines(xtr[2,],col="gray",lty=2)

axis(1,1:65,1952:2016)
lines(real_order,col="red",lwd=4)

dev.off()


# recip
order<-c()
real_order<-c()
old_order<-c()
for (t in 1:length(real_statistics)){
  order<-cbind(order,simulated_statistics[[t]][5,])
  real_order<-c(real_order,real_statistics[[t]][5])
  old_order<-c(old_order,old_statistics[[t]][5])
}


pdf("6) Visualizing the Out-of-Sample Forecasts/Figures/Figure11_5.pdf")
xtr<-boxplot(order,plot=F)$stats[c(1, 5), ]
boxplot(order,main="Reciprocity",xaxt="n",ylim=c(0,0.25))

lines(xtr[1,],col="gray",lty=2)
lines(xtr[2,],col="gray",lty=2)

axis(1,1:65,1952:2016)
lines(real_order,col="red",lwd=4)

dev.off()


# Transitivity
order<-c()
real_order<-c()
old_order<-c()
for (t in 1:length(real_statistics)){
  order<-cbind(order,simulated_statistics[[t]][6,])
  real_order<-c(real_order,real_statistics[[t]][6])
  old_order<-c(old_order,old_statistics[[t]][6])
}


pdf("6) Visualizing the Out-of-Sample Forecasts/Figures/Figure11_6.pdf")
xtr<-boxplot(order,plot=F)$stats[c(1, 5), ]
boxplot(order,main="Transitivity",xaxt="n",ylim=c(0,0.3))

lines(xtr[1,],col="gray",lty=2)
lines(xtr[2,],col="gray",lty=2)

axis(1,1:65,1952:2016)
lines(real_order,col="red",lwd=4)
dev.off()



rm(list=ls())


