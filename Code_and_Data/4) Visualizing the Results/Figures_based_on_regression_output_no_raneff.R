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
load("3) Estimating the Models/Fitted Models/dissolution_model_no_raneff.RData")
load("3) Estimating the Models/Fitted Models//formation_model_no_raneff.RData")
# Load in the Datasets from steps 1) and 3)
load("Data.RData")
load("2) Structuring Data for the Model/Datasets/Data_ready_for_regression.RData")


# Save the plotting parameters
p_form<-plot(formation_model,rug=F,scale=F,scheme=0,select=1)
p_diss<-plot(dissolution_model,rug=F,scale=F,scheme=0,select=1)


##
#Time-Varying Coefficients: Network Statistics ----
pdf("4) Visualizing the Results/Figures/no_raneff1.pdf",width = 20,height=17.86)

# Lower Order Network stats - Basic Structure
par(mfrow=c(5,2),mai = c(0.8,0.8,0.8,0.8))

plot(formation_model,rug = F,scale=F,scheme=1,select=7,ylab =expression(paste(theta,"(t)")),xlab="t",main="Formation: Outdegree, Sender",cex.main=3,cex.lab=2.5,cex.axis=2,ylim=c(0,7),yaxt="n")
axis(2,at=c(-40,-30,-20,-10,0,5,10,20,30,40,50,60,70,80,90),c(-0.4,-0.3,-0.2,-0.1,0,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),cex.axis=2)
abline(h=0,lty=2)
plot(dissolution_model,rug = F,scale=F,scheme=1,select=7,ylab =expression(paste(theta,"(t)")),xlab="t",main="Persistence: Outdegree, Sender",cex.main=3,cex.lab=2.5,cex.axis=2,ylim=c(0,7),yaxt="n")
axis(2,at=c(-40,-30,-20,-10,0,5,10,20,30,40,50,60,70,80,90),c(-0.4,-0.3,-0.2,-0.1,0,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),cex.axis=2)
abline(h=0,lty=2)

plot(formation_model,rug = F,scale=F,scheme=1,select=8,ylab =expression(paste(theta,"(t)")),xlab="t",main="Formation: Outdegree, Receiver",cex.main=3,cex.lab=2.5,cex.axis=2,ylim=c(-10,2),yaxt="n")
axis(2,at=c(-40,-30,-20,-10,-5,0,10,20,30,40,50,60,70,80,90),c(-0.4,-0.3,-0.2,-0.1,-0.05,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),cex.axis=2)
abline(h=0,lty=2)
plot(dissolution_model,rug = F,scale=F,scheme=1,select=8,ylab =expression(paste(theta,"(t)")),xlab="t",main="Persistence: Outdegree, Receiver",cex.main=3,cex.lab=2.5,cex.axis=2,ylim=c(-10,2),yaxt="n")
axis(2,at=c(-40,-30,-20,-10,-5,0,10,20,30,40,50,60,70,80,90),c(-0.4,-0.3,-0.2,-0.1,-0.05,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),cex.axis=2)
abline(h=0,lty=2)

plot(formation_model,rug = F,scale=F,scheme=1,select=3,ylab =expression(paste(theta,"(t)")),xlab="t",main="Formation: Reciprocity, Sender-Receiver",cex.main=3,cex.lab=2.5,cex.axis=2,ylim=c(-0.4,1),yaxt="n")
abline(h=0,lty=2)
axis(2,at=c(-0.4,-0.3,-0.2,-0.1,0,0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1),c(-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),cex.axis=2)
plot(dissolution_model,rug = F,scale=F,scheme=1,select=3,ylab =expression(paste(theta,"(t)")),xlab="t",main="Persistence: Reciprocity, Sender-Receiver",cex.main=3,cex.lab=2.5,cex.axis=2,ylim=c(-0.4,1),yaxt="n")
abline(h=0,lty=2)
axis(2,at=c(-0.4,-0.3,-0.2,-0.1,0,0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1),c(-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),cex.axis=2)

plot(formation_model,rug = F,scale=F,scheme=1,select=4,ylab =expression(paste(theta,"(t)")),xlab="t",main="Formation: Transitivity, Sender-Receiver",cex.main=3,cex.lab=2.5,cex.axis=2,ylim=c(-10,110),yaxt="n")
axis(2,at=c(-40,-30,-20,-10,0,10,20,30,40,50,60,70,80,90,100,110),c(-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1),cex.axis=2)
abline(h=0,lty=2)
plot(dissolution_model,rug = F,scale=F,scheme=1,select=4,ylab =expression(paste(theta,"(t)")),xlab="t",main="Persistence: Transitivity, Sender-Receiver",cex.main=3,cex.lab=2.5,cex.axis=2,ylim=c(-10,110),yaxt="n")
axis(2,at=c(-40,-30,-20,-10,0,10,20,30,40,50,60,70,80,90,100,110),c(-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1),cex.axis=2)
abline(h=0,lty=2)

plot(formation_model,rug = F,scale=F,scheme=1,select=5,ylab =expression(paste(theta,"(t)")),xlab="t",main="Formation: Shared Suppliers, Sender-Receiver",cex.main=3,cex.lab=2.5,cex.axis=2,ylim=c(-40,70),yaxt="n")
axis(2,at=c(-40,-30,-20,-10,0,10,20,30,40,50,60,70,80,90),c(-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),cex.axis=2)
abline(h=0,lty=2)
plot(dissolution_model,rug = F,scale=F,scheme=1,select=5,ylab =expression(paste(theta,"(t)")),xlab="t",main="Persistence: Shared Suppliers, Sender-Receiver",cex.main=3,cex.lab=2.5,cex.axis=2,ylim=c(-40,70),yaxt="n")
axis(2,at=c(-40,-30,-20,-10,0,10,20,30,40,50,60,70,80,90),c(-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),cex.axis=2)
abline(h=0,lty=2)

dev.off()

##
# Time-Varying Coefficients: Political and Economic Covariates
pdf("4) Visualizing the Results/Figures/no_raneff2.pdf",width = 20,height=25)


#  politics
par(mfrow=c(7,2),mai = c(0.8,0.8,0.8,0.8))

plot(formation_model,rug = F,scale=F,scheme=1,select=2,ylab =expression(paste(theta,"(t)")),xlab="t",main="Formation: Alliance, Sender-Receiver",cex.main=3,cex.lab=2.5,cex.axis=2,ylim=c(-0.1,1.6))
abline(h=0,lty=2)
plot(dissolution_model,rug = F,scale=F,scheme=1,select=2,ylab =expression(paste(theta,"(t)")),xlab="t",main="Persistence: Alliance, Sender-Receiver",cex.main=3,cex.lab=2.5,cex.axis=2,ylim=c(-0.1,1.6))
abline(h=0,lty=2)

plot(formation_model,rug = F,scale=F,scheme=1,select=6,ylab =expression(paste(theta,"(t)")),xlab="t",main="Formation: Regime Dissimilarity, Sender-Receiver",cex.main=3,cex.lab=2.5,cex.axis=2,ylim=c(-0.07,0.03))
abline(h=0,lty=2)
plot(dissolution_model,rug = F,scale=F,scheme=1,select=6,ylab =expression(paste(theta,"(t)")),xlab="t",main="Persistence: Regime Dissimilarity, Sender-Receiver",cex.main=3,cex.lab=2.5,cex.axis=2,ylim=c(-0.07,0.03))
abline(h=0,lty=2)

plot(formation_model,rug = F,scale=F,scheme=1,select=9,ylab =expression(paste(theta,"(t)")),xlab="t",main="Formation: log GDP, Sender",cex.main=3,cex.lab=2.5,cex.axis=2,ylim=c(-0.1,0.35))
abline(h=0,lty=2)
plot(dissolution_model,rug = F,scale=F,scheme=1,select=9,ylab =expression(paste(theta,"(t)")),xlab="t",main="Persistence: log GDP, Sender",cex.main=3,cex.lab=2.5,cex.axis=2,ylim=c(-0.1,0.35))
abline(h=0,lty=2)

plot(formation_model,rug = F,scale=F,scheme=1,select=10,ylab =expression(paste(theta,"(t)")),xlab="t",main="Formation: log GDP, Receiver",cex.main=3,cex.lab=2,cex.axis=2,ylim=c(-0.05,0.25))
abline(h=0,lty=2)
plot(dissolution_model,rug = F,scale=F,scheme=1,select=10,ylab =expression(paste(theta,"(t)")),xlab="t",main="Persistence: log GDP, Receiver",cex.main=3,cex.lab=2.5,cex.axis=2,ylim=c(-0.05,0.25))
abline(h=0,lty=2)

plot(formation_model,rug = F,scale=F,scheme=1,select=1,ylab =expression(paste(theta,"(t)")),xlab="t",main="Formation: log Distance, Sender-Receiver",cex.main=3,cex.lab=2.5,cex.axis=2,ylim=c(-0.25,0.25))
abline(h=0,lty=2)
plot(dissolution_model,rug = F,scale=F,scheme=1,select=1,ylab =expression(paste(theta,"(t)")),xlab="t",main="Persistence: log Distance, Sender-Receiver",cex.main=3,cex.lab=2.5,cex.axis=2,ylim=c(-0.25,0.25))
abline(h=0,lty=2)

plot(formation_model,rug = F,scale=F,scheme=1,select=11,ylab =expression(paste(theta,"(t)")),xlab="t",main="Formation: log Military Expenditures, Sender",cex.main=3,cex.lab=2.5,cex.axis=2,ylim=c(0,0.4))
abline(h=0,lty=2)
plot(dissolution_model,rug = F,scale=F,scheme=1,select=11,ylab =expression(paste(theta,"(t)")),xlab="t",main="Persistence: log Military Expenditures, Sender",cex.main=3,cex.lab=2.5,cex.axis=2,ylim=c(-0,0.4))
abline(h=0,lty=2)

plot(formation_model,rug = F,scale=F,scheme=1,select=12,ylab =expression(paste(theta,"(t)")),xlab="t",main="Formation: log Military Expenditures, Receiver",cex.main=3,cex.lab=2.5,cex.axis=2,ylim=c(-0.1,0.5))
abline(h=0,lty=2)
plot(dissolution_model,rug = F,scale=F,scheme=1,select=12,ylab =expression(paste(theta,"(t)")),xlab="t",main="Persistence: log Military Expenditures, Receiver",cex.main=3,cex.lab=2.5,cex.axis=2,ylim=c(-0.1,0.5))
abline(h=0,lty=2)
dev.off()



