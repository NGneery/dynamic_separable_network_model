#################################################################################
#################################################################################
# Visualization of the Estimation Output                                        #
#################################################################################

# remove the old stuff
rm(list=ls())


# Set working Directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
setwd("../")


# Load in the Results of the Estimation Section
load("3) Estimating the Models/Fitted Models/dissolution_model.RData")
load("3) Estimating the Models/Fitted Models//dissolution_model_c.RData")
load("3) Estimating the Models/Fitted Models//formation_model.RData")
load("3) Estimating the Models/Fitted Models//formation_model_c.RData")
# Load in the Datasets from steps 1) and 3)
load("Data.RData")
load("2) Structuring Data for the Model/Datasets/Data_ready_for_regression.RData")


# Save the plotting parameters
p_form<-plot(formation_model,rug=F,scale=F,scheme=0,select=1)
p_diss<-plot(dissolution_model,rug=F,scale=F,scheme=0,select=1)

p_form_c<-plot(formation_model_c,rug=F,scale=F,scheme=0,select=1)
p_diss_c<-plot(dissolution_model_c,rug=F,scale=F,scheme=0,select=1)

form_c_coef<-summary(formation_model_c)$p.coef
diss_c_coef<-summary(dissolution_model_c)$p.coef

##
# Time-Varying Coefficients: Network Statistics ----
pdf("4) Visualizing the Results/Figures/Figure4.pdf",width = 20,height=17.86)

# Lower Order Network stats - Basic Structure
par(mfrow=c(5,2),mai = c(0.8,0.8,0.8,0.8))

plot(formation_model,rug = F,scale=F,scheme=1,select=9,ylab =expression(paste(theta,"(t)")),xlab="t",main="Formation: Outdegree, Sender",cex.main=3,cex.lab=2.5,cex.axis=2,ylim=c(-10,2),yaxt="n")
axis(2,at=c(-40,-30,-20,-10,-5,0,10,20,30,40,50,60,70,80,90),c(-0.4,-0.3,-0.2,-0.1,-0.5,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),cex.axis=2)
abline(h=0,lty=2)
abline(h=form_c_coef[8],lty=3)
plot(dissolution_model,rug = F,scale=F,scheme=1,select=9,ylab =expression(paste(theta,"(t)")),xlab="t",main="Persistence: Outdegree, Sender",cex.main=3,cex.lab=2.5,cex.axis=2,ylim=c(-10,2),yaxt="n")
axis(2,at=c(-40,-30,-20,-10,-5,0,10,20,30,40,50,60,70,80,90),c(-0.4,-0.3,-0.2,-0.1,-0.5,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),cex.axis=2)
abline(h=0,lty=2)
abline(h=diss_c_coef[8],lty=3)

plot(formation_model,rug = F,scale=F,scheme=1,select=10,ylab =expression(paste(theta,"(t)")),xlab="t",main="Formation: Outdegree, Receiver",cex.main=3,cex.lab=2.5,cex.axis=2,ylim=c(-10,2),yaxt="n")
axis(2,at=c(-40,-30,-20,-10,-5,0,10,20,30,40,50,60,70,80,90),c(-0.4,-0.3,-0.2,-0.1,-0.05,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),cex.axis=2)
abline(h=0,lty=2)
abline(h=form_c_coef[9],lty=3)
plot(dissolution_model,rug = F,scale=F,scheme=1,select=10,ylab =expression(paste(theta,"(t)")),xlab="t",main="Persistence: Outdegree, Receiver",cex.main=3,cex.lab=2.5,cex.axis=2,ylim=c(-10,2),yaxt="n")
axis(2,at=c(-40,-30,-20,-10,-5,0,10,20,30,40,50,60,70,80,90),c(-0.4,-0.3,-0.2,-0.1,-0.05,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),cex.axis=2)
abline(h=0,lty=2)
abline(h=diss_c_coef[9],lty=3)

plot(formation_model,rug = F,scale=F,scheme=1,select=5,ylab =expression(paste(theta,"(t)")),xlab="t",main="Formation: Reciprocity, Sender-Receiver",cex.main=3,cex.lab=2.5,cex.axis=2,ylim=c(-0.4,0.9),yaxt="n")
abline(h=0,lty=2)
axis(2,at=c(-0.4,-0.3,-0.2,-0.1,0,0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90),c(-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),cex.axis=2)
abline(h=form_c_coef[4],lty=3)
plot(dissolution_model,rug = F,scale=F,scheme=1,select=5,ylab =expression(paste(theta,"(t)")),xlab="t",main="Persistence: Reciprocity, Sender-Receiver",cex.main=3,cex.lab=2.5,cex.axis=2,ylim=c(-0.4,0.9),yaxt="n")
abline(h=0,lty=2)
axis(2,at=c(-0.4,-0.3,-0.2,-0.1,0,0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90),c(-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),cex.axis=2)

abline(h=diss_c_coef[4],lty=3)

plot(formation_model,rug = F,scale=F,scheme=1,select=6,ylab =expression(paste(theta,"(t)")),xlab="t",main="Formation: Transitivity, Sender-Receiver",cex.main=3,cex.lab=2.5,cex.axis=2,ylim=c(-40,90),yaxt="n")
axis(2,at=c(-40,-30,-20,-10,0,10,20,30,40,50,60,70,80,90),c(-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),cex.axis=2)
abline(h=0,lty=2)
abline(h=form_c_coef[5],lty=3)
plot(dissolution_model,rug = F,scale=F,scheme=1,select=6,ylab =expression(paste(theta,"(t)")),xlab="t",main="Persistence: Transitivity, Sender-Receiver",cex.main=3,cex.lab=2.5,cex.axis=2,ylim=c(-40,90),yaxt="n")
axis(2,at=c(-40,-30,-20,-10,0,10,20,30,40,50,60,70,80,90),c(-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),cex.axis=2)
abline(h=0,lty=2)
abline(h=diss_c_coef[5],lty=3)

plot(formation_model,rug = F,scale=F,scheme=1,select=7,ylab =expression(paste(theta,"(t)")),xlab="t",main="Formation: Shared Suppliers, Sender-Receiver",cex.main=3,cex.lab=2.5,cex.axis=2,ylim=c(-40,90),yaxt="n")
axis(2,at=c(-40,-30,-20,-10,0,10,20,30,40,50,60,70,80,90),c(-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),cex.axis=2)
abline(h=0,lty=2)
abline(h=form_c_coef[6],lty=3)
plot(dissolution_model,rug = F,scale=F,scheme=1,select=7,ylab =expression(paste(theta,"(t)")),xlab="t",main="Persistence: Shared Suppliers, Sender-Receiver",cex.main=3,cex.lab=2.5,cex.axis=2,ylim=c(-40,90),yaxt="n")
axis(2,at=c(-40,-30,-20,-10,0,10,20,30,40,50,60,70,80,90),c(-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),cex.axis=2)
abline(h=0,lty=2)
abline(h=diss_c_coef[6],lty=3)

dev.off()

##
# Time-Varying Coefficients: Political and Economic Covariates
pdf("4) Visualizing the Results/Figures/Figure5.pdf",width = 20,height=25)


#  politics
par(mfrow=c(7,2),mai = c(0.8,0.8,0.8,0.8))

plot(formation_model,rug = F,scale=F,scheme=1,select=4,ylab =expression(paste(theta,"(t)")),xlab="t",main="Formation: Alliance, Sender-Receiver",cex.main=3,cex.lab=2.5,cex.axis=2,ylim=c(-0.1,2.8))
abline(h=0,lty=2)
abline(h=form_c_coef[3],lty=3)
plot(dissolution_model,rug = F,scale=F,scheme=1,select=4,ylab =expression(paste(theta,"(t)")),xlab="t",main="Persistence: Alliance, Sender-Receiver",cex.main=3,cex.lab=2.5,cex.axis=2,ylim=c(-0.1,2.8))
abline(h=0,lty=2)
abline(h=diss_c_coef[3],lty=3)

plot(formation_model,rug = F,scale=F,scheme=1,select=8,ylab =expression(paste(theta,"(t)")),xlab="t",main="Formation: Regime Dissimilarity, Sender-Receiver",cex.main=3,cex.lab=2.5,cex.axis=2,ylim=c(-0.08,0.03))
abline(h=0,lty=2)
abline(h=form_c_coef[7],lty=3)
plot(dissolution_model,rug = F,scale=F,scheme=1,select=8,ylab =expression(paste(theta,"(t)")),xlab="t",main="Persistence: Regime Dissimilarity, Sender-Receiver",cex.main=3,cex.lab=2.5,cex.axis=2,ylim=c(-0.08,0.03))
abline(h=0,lty=2)
abline(h=diss_c_coef[7],lty=3)

plot(formation_model,rug = F,scale=F,scheme=1,select=11,ylab =expression(paste(theta,"(t)")),xlab="t",main="Formation: log GDP, Sender",cex.main=3,cex.lab=2.5,cex.axis=2,ylim=c(-0.1,1.1))
abline(h=0,lty=2)
abline(h=form_c_coef[10],lty=3)
plot(dissolution_model,rug = F,scale=F,scheme=1,select=11,ylab =expression(paste(theta,"(t)")),xlab="t",main="Persistence: log GDP, Sender",cex.main=3,cex.lab=2.5,cex.axis=2,ylim=c(-0.1,1.1))
abline(h=0,lty=2)
abline(h=diss_c_coef[10],lty=3)

plot(formation_model,rug = F,scale=F,scheme=1,select=12,ylab =expression(paste(theta,"(t)")),xlab="t",main="Formation: log GDP, Receiver",cex.main=3,cex.lab=2,cex.axis=2,ylim=c(-0.1,0.4))
abline(h=0,lty=2)
abline(h=form_c_coef[11],lty=3)
plot(dissolution_model,rug = F,scale=F,scheme=1,select=12,ylab =expression(paste(theta,"(t)")),xlab="t",main="Persistence: log GDP, Receiver",cex.main=3,cex.lab=2.5,cex.axis=2,ylim=c(-0.1,0.4))
abline(h=0,lty=2)
abline(h=diss_c_coef[11],lty=3)

plot(formation_model,rug = F,scale=F,scheme=1,select=3,ylab =expression(paste(theta,"(t)")),xlab="t",main="Formation: log Distance, Sender-Receiver",cex.main=3,cex.lab=2.5,cex.axis=2,ylim=c(-0.2,0.2))
abline(h=0,lty=2)
abline(h=form_c_coef[2],lty=3)
plot(dissolution_model,rug = F,scale=F,scheme=1,select=3,ylab =expression(paste(theta,"(t)")),xlab="t",main="Persistence: log Distance, Sender-Receiver",cex.main=3,cex.lab=2.5,cex.axis=2,ylim=c(-0.2,0.2))
abline(h=0,lty=2)
abline(h=diss_c_coef[2],lty=3)

plot(formation_model,rug = F,scale=F,scheme=1,select=13,ylab =expression(paste(theta,"(t)")),xlab="t",main="Formation: log Military Expenditures, Sender",cex.main=3,cex.lab=2.5,cex.axis=2,ylim=c(-0.1,0.4))
abline(h=0,lty=2)
abline(h=form_c_coef[12],lty=3)
plot(dissolution_model,rug = F,scale=F,scheme=1,select=13,ylab =expression(paste(theta,"(t)")),xlab="t",main="Persistence: log Military Expenditures, Sender",cex.main=3,cex.lab=2.5,cex.axis=2,ylim=c(-0.1,0.4))
abline(h=0,lty=2)
abline(h=diss_c_coef[12],lty=3)

plot(formation_model,rug = F,scale=F,scheme=1,select=14,ylab =expression(paste(theta,"(t)")),xlab="t",main="Formation: log Military Expenditures, Receiver",cex.main=3,cex.lab=2.5,cex.axis=2,ylim=c(-0.1,0.4))
abline(h=0,lty=2)
abline(h=form_c_coef[13],lty=3)
plot(dissolution_model,rug = F,scale=F,scheme=1,select=14,ylab =expression(paste(theta,"(t)")),xlab="t",main="Persistence: log Military Expenditures, Receiver",cex.main=3,cex.lab=2.5,cex.axis=2,ylim=c(-0.1,0.4))
abline(h=0,lty=2)
abline(h=diss_c_coef[13],lty=3)
dev.off()





#######
# plotting the the random effects for the Persistence Model
######

# First extract the random effect lines for the Sending Country in the Dissolution Model
sender_raneff_container<-c()

# how many re are out there?
n_s<-length(table(d_sender_id))

# how are they called?
n_names<-names(table(d_sender_id))
# extract them
sender_raneff<-c()
for (i in 1:length(n_names)){
  ex<-nEX[which(rownames(nEX)==n_names[i]),]
  ex<-cbind(1950:2016,ex)
  sender_raneff<-cbind(p_diss[[1]]$fit[((i-1)*100+1):(i*100)],p_diss[[1]]$x,0)  
  
  for (t in 1:dim(sender_raneff)[1]){
    r<-round(sender_raneff[t,2],0)
    index<-which(ex[,1]==r)
    if (ex[index,2]==1){
      sender_raneff[t,3]<-1
    }
    
  }
  sender_raneff_container<-cbind(sender_raneff_container,sender_raneff[,1])
}



# Persistence Model: Functinal Principal Component Analsis of the smooth Random Effects for the Sender (top) and the Receiver (bottom)
pdf("4) Visualizing the Results/Figures/Figure8.pdf",width = 20,height = 20)

par(mfrow=c(1,1),mar=c(5,5,5,5))
layout(matrix(c(1,2,1,3,4,5,4,6), nrow = 4, ncol = 2, byrow = TRUE))
X<-t(sender_raneff_container)
# Correct for the discretization
w<-(2016-1950)/100
w<-w^(-0.5)
# Calculate the Covariance Matrix of X
zeta<-svd(cov(X))
# Save the scores
score_1<-X%*%zeta$u[,1]*w
score_2<-X%*%zeta$u[,2]*w
# Plot the Scores
plot(-score_1,score_2,col=0,main="Persistence: Component Score, Sender",ylab="Component 2",xlab="Component 1",cex.main=3,cex.lab=3,cex.axis=2.5)
abline(h=0,col='gray')
abline(v=0,col='gray')

# Add the country names
short<-c()
for (i in 1:length(n_names)){
  short<-c(short,laenderliste[which(laenderliste[,1]==n_names[i]),5])
}
text(-score_1,score_2,short,col=1,cex=3)

# Calculate the standard deviation among the time averages
means<-apply(sender_raneff_container,2,mean)
factor<-sd(means)

# Plot the Components against time
plot(sender_raneff[,2],1:100,ylim=c(-0.1,0.1),col=0,main=paste("PC 1 (",round(zeta$d[1]/sum(zeta$d),3)*100,"%)",sep=""),ylab="+/- sd*(Value of PC curve)",xlab="t",cex.main=3,cex.lab=3,cex.axis=2.5)
abline(h=0,col="gray")
points(sender_raneff[,2],-zeta$u[,1]*factor,pch="+",col=rep(c(0,1,0),100),cex=3)
points(sender_raneff[,2],+zeta$u[,1]*factor,pch="-",col=rep(c(0,1,0),100),cex=3)

plot(sender_raneff[,2],1:100,ylim=c(-0.1,0.1),col=0,main=paste("PC 2 (",round(zeta$d[2]/sum(zeta$d),3)*100,"%)",sep=""),ylab="+/- sd*(Value of PC curve)",xlab="t",cex.main=3,cex.lab=3,cex.axis=2.5)
abline(h=0,col="gray")
points(sender_raneff[,2],+zeta$u[,2]*factor,pch="+",col=rep(c(0,1,0),100),cex=3)
points(sender_raneff[,2],-zeta$u[,2]*factor,pch="-",col=rep(c(0,1,0),100),cex=3)
#
#
# Now extract the random effect lines for the Receiving Country in the Dissolution Model
receiver_raneff_container<-c()

# how many re are out there?
n_s<-length(table(d_receiver_id))

#how are they called?
n_names<-names(table(d_receiver_id))
# extract them
receiver_raneff<-c()
for (i in 1:length(n_names)){
  ex<-nEX[which(rownames(nEX)==n_names[i]),]
  ex<-cbind(1950:2016,ex)
  receiver_raneff<-cbind(p_diss[[2]]$fit[((i-1)*100+1):(i*100)],p_diss[[1]]$x,0)  
  
  for (t in 1:dim(receiver_raneff)[1]){
    r<-round(receiver_raneff[t,2],0)
    index<-which(ex[,1]==r)
    if (ex[index,2]==1){
      receiver_raneff[t,3]<-1
    }
    
  }
  receiver_raneff_container<-cbind(receiver_raneff_container,receiver_raneff[,1])
}

# Calculate the standard deviation of the time averages
means<-apply(receiver_raneff_container,2,mean)
factor<-sd(means)

X<-t(receiver_raneff_container)
# Correct for the discretization
w<-(2016-1950)/100
w<-w^(-0.5)
# Do principal component analysis on the covariance of X
zeta<-svd(cov(X))
score_1<-X%*%zeta$u[,1]*w
score_2<-X%*%zeta$u[,2]*w

# Plot the Scores
plot(-score_1,score_2,col=0,main="Persistence: Component Score, Receiver",ylab="Component 2",xlab="Component 1",cex.main=3,cex.lab=3,cex.axis=2.5)
abline(h=0,col='gray')
abline(v=0,col='gray')

# Add text to the scores
short<-c()
for (i in 1:length(n_names)){
  short<-c(short,laenderliste[which(laenderliste[,1]==n_names[i]),5])
}
text(-score_1,score_2,short,col=1,cex=3)


# Plot the components against time
plot(receiver_raneff[,2],1:100,ylim=c(-0.1,0.1),col=0,main=paste("PC 1 (",round(zeta$d[1]/sum(zeta$d),3)*100,"%)",sep=""),ylab="+/- sd*(Value of PC curve)",xlab="t",cex.main=3,cex.lab=3,cex.axis=2.5)
abline(h=0,col="gray")
points(receiver_raneff[,2],-zeta$u[,1]*factor,pch="+",col=rep(c(0,1,0),100),cex=3)
points(receiver_raneff[,2],+zeta$u[,1]*factor,pch="-",col=rep(c(0,1,0),100),cex=3)

plot(receiver_raneff[,2],1:100,ylim=c(-0.1,0.1),col=0,main=paste("PC 2 (",round(zeta$d[2]/sum(zeta$d),3)*100,"%)",sep=""),ylab="+/- sd*(Value of PC curve)",xlab="t",cex.main=3,cex.lab=3,cex.axis=2.5)
abline(h=0,col="gray")
points(receiver_raneff[,2],+zeta$u[,2]*factor,pch="+",col=rep(c(0,1,0),100),cex=3)
points(receiver_raneff[,2],-zeta$u[,2]*factor,pch="-",col=rep(c(0,1,0),100),cex=3)


dev.off()


#######
# plotting the the random effects for the Formation Model
######

# First extract the random effect lines for the Sending Country in the Dissolution Model
sender_raneff_container<-c()
# how many re are out there?
n_s<-length(table(f_sender_id))
#how are they called?
n_names<-names(table(f_sender_id))
# extract them
sender_raneff<-c()
for (i in 1:length(n_names)){
  ex<-nEX[which(rownames(nEX)==n_names[i]),]
  ex<-cbind(1950:2016,ex)
  sender_raneff<-cbind(p_form[[1]]$fit[((i-1)*100+1):(i*100)],p_form[[1]]$x,0)  
  
  for (t in 1:dim(sender_raneff)[1]){
    r<-round(sender_raneff[t,2],0)
    index<-which(ex[,1]==r)
    if (ex[index,2]==1){
      sender_raneff[t,3]<-1
    }
    
  }
  sender_raneff_container<-cbind(sender_raneff_container,sender_raneff[,1])
}


# Formation Model: Functional Principal Component Analsis of the smooth Random Effects for the Sender (top) and the Receiver (bottom)
pdf("4) Visualizing the Results/Figures/Figure7.pdf",width = 20,height = 20)

par(mfrow=c(1,1),mar=c(6,6,6,6))
layout(matrix(c(1,2,1,3,4,5,4,6), nrow = 4, ncol = 2, byrow = TRUE))
X<-t(sender_raneff_container)
# Correct for the discretization
w<-(2016-1950)/100
w<-w^(-0.5)
# Do Principal component analysis on the covariance of X
zeta<-svd(cov(X))
score_1<-X%*%zeta$u[,1]*w
score_2<-X%*%zeta$u[,2]*w
# Plot the Scores
plot(-score_1,score_2,col=0,main="Formation: Component Score, Sender",ylab="Component 2",xlab="Component 1",cex.main=3,cex.lab=3,cex.axis=2.5)
abline(h=0,col='gray')
abline(v=0,col='gray')

# Add the names
short<-c()
for (i in 1:length(n_names)){
  short<-c(short,laenderliste[which(laenderliste[,1]==n_names[i]),5])
}
text(-score_1,score_2,short,col=1,cex=3)

# Calculate the standard deviation of the time averages
means<-apply(sender_raneff_container,2,mean)
factor<-sd(means)

# Plot the Components against time
plot(sender_raneff[,2],1:100,ylim=c(-0.2,0.2),col=0,main=paste("PC 1 (",round(zeta$d[1]/sum(zeta$d),3)*100,"%)",sep=""),ylab="+/- sd*(Value of PC curve)",xlab="t",cex.main=3,cex.lab=3,cex.axis=2.5)
abline(h=0,col="gray")
points(sender_raneff[,2],-zeta$u[,1]*factor,pch="+",col=rep(c(0,1,0),100),cex=3)
points(sender_raneff[,2],+zeta$u[,1]*factor,pch="-",col=rep(c(0,1,0),100),cex=3)

plot(sender_raneff[,2],1:100,ylim=c(-0.2,0.2),col=0,main=paste("PC 2 (",round(zeta$d[2]/sum(zeta$d),3)*100,"%)",sep=""),ylab="+/- sd*(Value of PC curve)",xlab="t",cex.main=3,cex.lab=3,cex.axis=2.5)
abline(h=0,col="gray")
points(sender_raneff[,2],+zeta$u[,2]*factor,pch="+",col=rep(c(0,1,0),100),cex=3)
points(sender_raneff[,2],-zeta$u[,2]*factor,pch="-",col=rep(c(0,1,0),100),cex=3)

###############
# now for the Receiver
###############
receiver_raneff_container<-c()
# how many re are out there?
n_s<-length(table(d_receiver_id))
#how are they called?
n_names<-names(table(d_receiver_id))
# extract them
receiver_raneff<-c()
for (i in 1:length(n_names)){
  ex<-nEX[which(rownames(nEX)==n_names[i]),]
  ex<-cbind(1950:2016,ex)
  receiver_raneff<-cbind(p_form[[2]]$fit[((i-1)*100+1):(i*100)],p_form[[1]]$x,0)  
  
  for (t in 1:dim(receiver_raneff)[1]){
    r<-round(receiver_raneff[t,2],0)
    index<-which(ex[,1]==r)
    if (ex[index,2]==1){
      receiver_raneff[t,3]<-1
    }
    
  }
   receiver_raneff_container<-cbind(receiver_raneff_container,receiver_raneff[,1])
}


# Calculate the standard deviation of the time averages
means<-apply(receiver_raneff_container,2,mean)
factor<-sd(means)

# Do Principal Component Analysis on the Covariance of X
X<-t(receiver_raneff_container)
# Correct for Discretization
w<-(2016-1950)/100
w<-w^(-0.5)
zeta<-svd(cov(X))
score_1<-X%*%zeta$u[,1]*w
score_2<-X%*%zeta$u[,2]*w
# Plot the Scores
plot(-score_1,score_2,col=0,main="Formation: Component Score, Receiver",ylab="Component 2",xlab="Component 1",cex.main=3,cex.lab=3,cex.axis=2.5)
abline(h=0,col='gray')
abline(v=0,col='gray')
# Add names
short<-c()
for (i in 1:length(n_names)){
  short<-c(short,laenderliste[which(laenderliste[,1]==n_names[i]),5])
}
text(-score_1,score_2,short,col=1,cex=3)

# Plot the Components against time
plot(receiver_raneff[,2],1:100,ylim=c(-0.2,0.2),col=0,main=paste("PC 1 (",round(zeta$d[1]/sum(zeta$d),3)*100,"%)",sep=""),ylab="+/- sd*(Value of PC curve)",xlab="t",cex.main=3,cex.lab=3,cex.axis=2.5)
abline(h=0,col="gray")
points(receiver_raneff[,2],-zeta$u[,1]*factor,pch="+",col=rep(c(0,1,0),100),cex=3)
points(receiver_raneff[,2],+zeta$u[,1]*factor,pch="-",col=rep(c(0,1,0),100),cex=3)

plot(receiver_raneff[,2],1:100,ylim=c(-0.2,0.2),col=0,main=paste("PC 2 (",round(zeta$d[2]/sum(zeta$d),3)*100,"%)",sep=""),ylab="+/- sd*(Value of PC curve)",xlab="t",cex.main=3,cex.lab=3,cex.axis=2.5)
abline(h=0,col="gray")
points(receiver_raneff[,2],+zeta$u[,2]*factor,pch="+",col=rep(c(0,1,0),100),cex=3)
points(receiver_raneff[,2],-zeta$u[,2]*factor,pch="-",col=rep(c(0,1,0),100),cex=3)


dev.off()

##
# Predicted Time-Varying Smooth Random Effects
pdf("4) Visualizing the Results/Figures/Figure6.pdf",width = 30,height=30)
par(mfrow=c(2,2),mar= c(5,5,5,5))

# Plot the Sender Effects for the Formation
plot(sender_raneff[,2],sender_raneff[,1],type="n",main=paste("Formation, Sender"),ylim=c(-4,4),xlim=c(1946,2018),xlab="t",xaxt="n",ylab=expression(paste(phi,"(t)")),cex.main=3,cex.lab=2.5,cex.axis=2.5)
axis(1,1950:2016,1950:2016,cex.axis=2)
# how many re are out there?
n_s<-length(table(f_sender_id))
#how are they called?
n_names<-names(table(f_sender_id))
# What are the names
short<-c()
for (i in 1:length(n_names)){
  short<-c(short,laenderliste[which(laenderliste[,1]==n_names[i]),5])
}

# extract them
sender_raneff<-c()
for (i in 1:length(n_names)){
  ex<-nEX[which(rownames(nEX)==n_names[i]),]
  ex<-cbind(1950:2016,ex)
  sender_raneff<-cbind(p_form[[1]]$fit[((i-1)*100+1):(i*100)],p_form[[1]]$x,0)  
  
  for (t in 1:dim(sender_raneff)[1]){
    r<-round(sender_raneff[t,2],0)
    index<-which(ex[,1]==r)
    if (ex[index,2]==1){
      sender_raneff[t,3]<-1
    }
    
  }
  
  # Add them to the plot
  lines(sender_raneff[sender_raneff[,3]==1,2],sender_raneff[sender_raneff[,3]==1,1],lty=i)
  text(1949,sender_raneff[1,1],paste(short[i],""),cex=2,col = nEX[which(rownames(nEX)==n_names[i]),1])
  text(2018,sender_raneff[100,1],paste(short[i],""),cex=2,col = (nEX[which(rownames(nEX)==n_names[i]),67]))
}


# Now the Sender Effects for the Persistence Model
plot(sender_raneff[,2],sender_raneff[,1],xaxt="n",type="n",main=paste("Persistence, Sender"),ylim=c(-1.2,1.2),xlim=c(1946,2018),xlab="t",ylab=expression(paste(phi,"(t)")),cex.main=3,cex.lab=2.5,cex.axis=2.5)
axis(1,1950:2016,1950:2016,cex.axis=2)
# how many re are out there?
n_s<-length(table(d_sender_id))
#how are they called?
n_names<-names(table(d_sender_id))
short<-c()
for (i in 1:length(n_names)){
  short<-c(short,laenderliste[which(laenderliste[,1]==n_names[i]),5])
}
# extract them
sender_raneff<-c()
for (i in 1:length(n_names)){
  ex<-nEX[which(rownames(nEX)==n_names[i]),]
  ex<-cbind(1950:2016,ex)
  sender_raneff<-cbind(p_diss[[1]]$fit[((i-1)*100+1):(i*100)],p_diss[[1]]$x,0)  
  
  for (t in 1:dim(sender_raneff)[1]){
    r<-round(sender_raneff[t,2],0)
    index<-which(ex[,1]==r)
    if (ex[index,2]==1){
      sender_raneff[t,3]<-1
    }
    
  }
  # Add names
  lines(sender_raneff[sender_raneff[,3]==1,2],sender_raneff[sender_raneff[,3]==1,1],lty=i)
  text(1949,sender_raneff[1,1],paste(short[i],""),cex=2,col = nEX[which(rownames(nEX)==n_names[i]),1])
  text(2018,sender_raneff[100,1],paste(short[i],""),cex=2,col = (nEX[which(rownames(nEX)==n_names[i]),67]))
}
plot(receiver_raneff[,2],receiver_raneff[,1],xaxt="n",type="n",main=paste("Formation, Receiver"),ylim=c(-2.5,2),xlim=c(1946,2018),xlab="t",ylab=expression(paste(phi,"(t)")),cex.main=3,cex.lab=2.5,cex.axis=2.5)
axis(1,1950:2016,1950:2016,cex.axis=2)

# how many re are out there?
n_s<-length(table(f_receiver_id))
#how are they called?
n_names<-names(table(f_receiver_id))
short<-c()
for (i in 1:length(n_names)){
  short<-c(short,laenderliste[which(laenderliste[,1]==n_names[i]),5])
}

# extract them
receiver_raneff<-c()
for (i in 1:length(n_names)){
  ex<-nEX[which(rownames(nEX)==n_names[i]),]
  ex<-cbind(1950:2016,ex)
  receiver_raneff<-cbind(p_form[[2]]$fit[((i-1)*100+1):(i*100)],p_form[[2]]$x,0)  
  
  for (t in 1:dim(receiver_raneff)[1]){
    r<-round(receiver_raneff[t,2],0)
    index<-which(ex[,1]==r)
    if (ex[index,2]==1){
      receiver_raneff[t,3]<-1
    }
    
  }
  color<-receiver_raneff[,3]
  lines(receiver_raneff[receiver_raneff[,3]==1,2],receiver_raneff[receiver_raneff[,3]==1,1],lty=i)
  text(1949,receiver_raneff[1,1],paste(short[i],""),cex=2,col = color[1])
  text(2018,receiver_raneff[100,1],paste(short[i],""),cex=2,col =color[100])
}


plot(receiver_raneff[,2],receiver_raneff[,1],xaxt="n",type="n",main=paste("Persistence, Receiver"),ylim=c(-1,1),xlim=c(1946,2018),xlab="t",ylab=expression(paste(phi,"(t)")),cex.main=3,cex.lab=2.5,cex.axis=2.5)
axis(1,1950:2016,1950:2016,cex.axis=2)
# how many re are out there?
n_s<-length(table(d_receiver_id))
#how are they called?
n_names<-names(table(d_receiver_id))
short<-c()
for (i in 1:length(n_names)){
  short<-c(short,laenderliste[which(laenderliste[,1]==n_names[i]),5])
}
# extract them
receiver_raneff<-c()
for (i in 1:length(n_names)){
  ex<-nEX[which(rownames(nEX)==n_names[i]),]
  ex<-cbind(1950:2016,ex)
  receiver_raneff<-cbind(p_diss[[2]]$fit[((i-1)*100+1):(i*100)],p_diss[[2]]$x,0)  
  
  for (t in 1:dim(receiver_raneff)[1]){
    r<-round(receiver_raneff[t,2],0)
    index<-which(ex[,1]==r)
    if (ex[index,2]==1){
      receiver_raneff[t,3]<-1
    }
    
  }
  color<-receiver_raneff[,3]
  
  lines(receiver_raneff[receiver_raneff[,3]==1,2],receiver_raneff[receiver_raneff[,3]==1,1],lty=i)
  text(1949,receiver_raneff[1,1],paste(short[i],""),cex=2,col = color[1])
  text(2018,receiver_raneff[100,1],paste(short[i],""),cex=2,col =color[100])
}



dev.off()




############################


# Create a Table 2: Countries included in the Analysis with three-digit Country Codes and Time-Period of Inclusion in the Model.
library(xtable)
# Save as Dataframe
Table<-as.data.frame(cbind(laenderliste[1:224,c(1,5)],nEX[1:224,]))

# identify existing coutnries
rS<-rowSums(nEX[1:224,])
Table<-Table[rS!=0,]
finder<-nEX[1:224,]
finder<-finder[rS!=0,]
from<-c()
to<-c()
for (i in 1:dim(finder)[1]){
  from<-c(from,which(finder[i,]==1)[1])
  
  ind<-which(finder[i,]==1)
  len<-length(ind)
  
  to<-c(to,ind[len])
  
}

# Included dates of existence
Table<-as.data.frame(cbind(Table[,1:2],from,to))
Table[,3]<-Table[,3]+1949
Table[,4]<-Table[,4]+1949
fromto<-paste(Table[,3],"-",Table[,4])
Table<-as.data.frame(cbind(Table[,1:2],fromto))
Represent<-cbind(Table[1:57,],Table[58:114,],Table[115:171,])

# Export as latex
print(xtable(Represent,digits=c(0,0,0,0,0,0,0,0,0,0)),include.rownames=FALSE)

rm(list=ls())

