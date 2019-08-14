#################################################################################
#################################################################################
# Visualization of the Estimation Output                                        #
#################################################################################

# remove the old stuff
rm(list=ls())


# Set working Directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
setwd("../")


p_form<-list()
p_diss<-list()
p_form_c<-list()
p_diss_c<-list()
form_c_coef<-list()
diss_c_coef<-list()



# Load in the Results of the Estimation Section
load("3) Estimating the Models/Fitted Models/dissolution_model.RData")
load("3) Estimating the Models/Fitted Models/dissolution_model_c.RData")
load("3) Estimating the Models/Fitted Models/formation_model.RData")
load("3) Estimating the Models/Fitted Models/formation_model_c.RData")
# Load in the Datasets from steps 1) and 3)
load("Data.RData")
load("2) Structuring Data for the Model/Datasets/Data_ready_for_regression.RData")


# Save the plotting parameters
p_form[[1]]<-plot(formation_model,rug=F,scale=F,scheme=0,select=1)
p_diss[[1]]<-plot(dissolution_model,rug=F,scale=F,scheme=0,select=1)

p_form_c[[1]]<-plot(formation_model_c,rug=F,scale=F,scheme=0,select=1)
p_diss_c[[1]]<-plot(dissolution_model_c,rug=F,scale=F,scheme=0,select=1)

form_c_coef[[1]]<-summary(formation_model_c)$p.coef
diss_c_coef[[1]]<-summary(dissolution_model_c)$p.coef

# Load in the Results of the Estimation Section
load("3) Estimating the Models/Fitted Models/dissolution_model_2y.RData")
load("3) Estimating the Models/Fitted Models/dissolution_model_c_2y.RData")
load("3) Estimating the Models/Fitted Models/formation_model_2y.RData")
load("3) Estimating the Models/Fitted Models/formation_model_c_2y.RData")
# Load in the Datasets from steps 1) and 3)
load("Data.RData")
load("2) Structuring Data for the Model/Datasets/Data_ready_for_regression_2ywindow.RData")

# Save the plotting parameters
p_form[[2]]<-plot(formation_model,rug=F,scale=F,scheme=0,select=1)
p_diss[[2]]<-plot(dissolution_model,rug=F,scale=F,scheme=0,select=1)

p_form_c[[2]]<-plot(formation_model_c,rug=F,scale=F,scheme=0,select=1)
p_diss_c[[2]]<-plot(dissolution_model_c,rug=F,scale=F,scheme=0,select=1)

form_c_coef[[2]]<-summary(formation_model_c)$p.coef
diss_c_coef[[2]]<-summary(dissolution_model_c)$p.coef

# Load in the Results of the Estimation Section
load("3) Estimating the Models/Fitted Models/dissolution_model_3y.RData")
load("3) Estimating the Models/Fitted Models/dissolution_model_c_3y.RData")
load("3) Estimating the Models/Fitted Models/formation_model_3y.RData")
load("3) Estimating the Models/Fitted Models/formation_model_c_3y.RData")
# Load in the Datasets from steps 1) and 3)
load("Data.RData")
load("2) Structuring Data for the Model/Datasets/Data_ready_for_regression_3ywindow.RData")

# Save the plotting parameters
p_form[[3]]<-plot(formation_model,rug=F,scale=F,scheme=0,select=1)
p_diss[[3]]<-plot(dissolution_model,rug=F,scale=F,scheme=0,select=1)

p_form_c[[3]]<-plot(formation_model_c,rug=F,scale=F,scheme=0,select=1)
p_diss_c[[3]]<-plot(dissolution_model_c,rug=F,scale=F,scheme=0,select=1)

form_c_coef[[3]]<-summary(formation_model_c)$p.coef
diss_c_coef[[3]]<-summary(dissolution_model_c)$p.coef






######################################################
# Plot everything together
######################################################
pdf("4) Visualizing the Results/Figures/windows1.pdf",width = 20,height=17.86)

par(mfrow=c(5,2),mai = c(0.8,0.8,0.8,0.8))
##1
CI_up<-c()
CI_low<-c()
for (i in 2:length(p_form)){
  CI_up<-cbind(CI_up,p_form[[i]][[9]]$fit+p_form[[i]][[9]]$se)
  CI_low<-cbind(CI_low,p_form[[i]][[9]]$fit-p_form[[i]][[9]]$se)
}

plot(p_form[[1]][[9]]$fit,type="l",col="black",ylim = c(-6,2.5),yaxt="n",lwd=2,ylab =expression(paste(theta,"(t)")),xlab="t",main="Formation: Outdegree, Sender",cex.main=3,cex.lab=2.5,cex.axis=2,yaxt="n",xaxt="n")
axis(2,at=c(5,4,3,2,1,0,-1,-2,-3,-4,-5,-6,-7,-8),c(0.05,0.04,0.03,0.02,0.01,0,-0.01,-0.02,-0.03,-0.04,-0.05,-0.06,-0.07,-0.08),cex.axis=2)
axis(1,at=1:100,round(p_form[[1]][[9]]$x,0),cex.axis=2)
polygon(c(1:100, rev(1:100)), c(p_form[[which(colMeans(CI_up)==max(colMeans(CI_up)))+1]][[9]]$fit+p_form[[which(colMeans(CI_up)==max(colMeans(CI_up)))+1]][[9]]$se, rev(p_form[[which(colMeans(CI_low)==min(colMeans(CI_low)))+1]][[9]]$fit-p_form[[which(colMeans(CI_low)==min(colMeans(CI_low)))+1]][[9]]$se)),
        col = "lightgray", border = NA)


polygon(c(1:100, rev(1:100)), c(p_form[[1]][[9]]$fit+p_form[[1]][[9]]$se, rev(p_form[[1]][[9]]$fit-p_form[[1]][[9]]$se)),
        col = "darkgray", border = NA)
lines(p_form[[1]][[9]]$fit,type="l",col="black",ylim = c(-8,5),yaxt="n",lwd=3)
abline(h=0,lty=2)

for (i in 2:length(p_form)){
  lines(p_form[[i]][[9]]$fit,col="black",lty=2)
}

##
CI_up<-c()
CI_low<-c()
for (i in 2:length(p_diss)){
  CI_up<-cbind(CI_up,p_diss[[i]][[9]]$fit+p_diss[[i]][[9]]$se)
  CI_low<-cbind(CI_low,p_diss[[i]][[9]]$fit-p_diss[[i]][[9]]$se)
}

plot(p_diss[[1]][[9]]$fit,type="l",col="black",ylim = c(-6,2.5),yaxt="n",lwd=2,ylab =expression(paste(theta,"(t)")),xlab="t",main="Persistence: Outdegree, Sender",cex.main=3,cex.lab=2.5,cex.axis=2,yaxt="n",xaxt="n")
axis(2,at=c(5,4,3,2,1,0,-1,-2,-3,-4,-5,-6,-7,-8),c(0.05,0.04,0.03,0.02,0.01,0,-0.01,-0.02,-0.03,-0.04,-0.05,-0.06,-0.07,-0.08),cex.axis=2)
axis(1,at=1:100,round(p_diss[[1]][[9]]$x,0),cex.axis=2)
polygon(c(1:100, rev(1:100)), c(p_diss[[which(colMeans(CI_up)==max(colMeans(CI_up)))+1]][[9]]$fit+p_diss[[which(colMeans(CI_up)==max(colMeans(CI_up)))+1]][[9]]$se, rev(p_diss[[which(colMeans(CI_low)==min(colMeans(CI_low)))+1]][[9]]$fit-p_diss[[which(colMeans(CI_low)==min(colMeans(CI_low)))+1]][[9]]$se)),
        col = "lightgray", border = NA)


polygon(c(1:100, rev(1:100)), c(p_diss[[1]][[9]]$fit+p_diss[[1]][[9]]$se, rev(p_diss[[1]][[9]]$fit-p_diss[[1]][[9]]$se)),
        col = "darkgray", border = NA)
lines(p_diss[[1]][[9]]$fit,type="l",col="black",ylim = c(-8,5),yaxt="n",lwd=3)
abline(h=0,lty=2)

for (i in 2:length(p_diss)){
  lines(p_diss[[i]][[9]]$fit,col="black",lty=2)
}


##
CI_up<-c()
CI_low<-c()
for (i in 2:length(p_form)){
  CI_up<-cbind(CI_up,p_form[[i]][[10]]$fit+p_form[[i]][[10]]$se)
  CI_low<-cbind(CI_low,p_form[[i]][[10]]$fit-p_form[[i]][[10]]$se)
}

plot(p_form[[1]][[10]]$fit,type="l",col="black",ylim = c(-13,1),yaxt="n",lwd=2,ylab =expression(paste(theta,"(t)")),xlab="t",main="Formation: Outdegree, Receiver",cex.main=3,cex.lab=2.5,cex.axis=2,yaxt="n",xaxt="n")
axis(2,at=c(5:-13),c(seq(from=0.05,to=-0.13,by=-0.01)),cex.axis=2)
axis(1,at=1:100,round(p_form[[1]][[10]]$x,0),cex.axis=2)
polygon(c(1:100, rev(1:100)), c(p_form[[which(colMeans(CI_up)==max(colMeans(CI_up)))+1]][[10]]$fit+p_form[[which(colMeans(CI_up)==max(colMeans(CI_up)))+1]][[10]]$se, rev(p_form[[which(colMeans(CI_low)==min(colMeans(CI_low)))+1]][[10]]$fit-p_form[[which(colMeans(CI_low)==min(colMeans(CI_low)))+1]][[10]]$se)),
        col = "lightgray", border = NA)


polygon(c(1:100, rev(1:100)), c(p_form[[1]][[10]]$fit+p_form[[1]][[10]]$se, rev(p_form[[1]][[10]]$fit-p_form[[1]][[10]]$se)),
        col = "darkgray", border = NA)
lines(p_form[[1]][[10]]$fit,type="l",col="black",ylim = c(-8,5),yaxt="n",lwd=3)
abline(h=0,lty=2)

for (i in 2:length(p_form)){
  lines(p_form[[i]][[10]]$fit,col="black",lty=2)
  
}

##
CI_up<-c()
CI_low<-c()
for (i in 2:length(p_diss)){
  CI_up<-cbind(CI_up,p_diss[[i]][[10]]$fit+p_diss[[i]][[10]]$se)
  CI_low<-cbind(CI_low,p_diss[[i]][[10]]$fit-p_diss[[i]][[10]]$se)
}

plot(p_diss[[1]][[10]]$fit,type="l",col="black",ylim = c(-13,1),yaxt="n",lwd=2,ylab =expression(paste(theta,"(t)")),xlab="t",main="Persistence: Outdegree, Receiver",cex.main=3,cex.lab=2.5,cex.axis=2,yaxt="n",xaxt="n")
axis(2,at=c(5:-13),c(seq(from=0.05,to=-0.13,by=-0.01)),cex.axis=2)
axis(1,at=1:100,round(p_diss[[1]][[10]]$x,0),cex.axis=2)
polygon(c(1:100, rev(1:100)), c(p_diss[[which(colMeans(CI_up)==max(colMeans(CI_up)))+1]][[10]]$fit+p_diss[[which(colMeans(CI_up)==max(colMeans(CI_up)))+1]][[10]]$se, rev(p_diss[[which(colMeans(CI_low)==min(colMeans(CI_low)))+1]][[10]]$fit-p_diss[[which(colMeans(CI_low)==min(colMeans(CI_low)))+1]][[10]]$se)),
        col = "lightgray", border = NA)


polygon(c(1:100, rev(1:100)), c(p_diss[[1]][[10]]$fit+p_diss[[1]][[10]]$se, rev(p_diss[[1]][[10]]$fit-p_diss[[1]][[10]]$se)),
        col = "darkgray", border = NA)
lines(p_diss[[1]][[10]]$fit,type="l",col="black",ylim = c(-8,5),yaxt="n",lwd=3)
abline(h=0,lty=2)

for (i in 2:length(p_diss)){
  lines(p_diss[[i]][[10]]$fit,col="black",lty=2)
  
}

##
CI_up<-c()
CI_low<-c()
for (i in 2:length(p_form)){
  CI_up<-cbind(CI_up,p_form[[i]][[5]]$fit+p_form[[i]][[5]]$se)
  CI_low<-cbind(CI_low,p_form[[i]][[5]]$fit-p_form[[i]][[5]]$se)
}

plot(p_form[[1]][[5]]$fit,type="l",col="black",ylim = c(-1,2),yaxt="n",lwd=2,ylab =expression(paste(theta,"(t)")),xlab="t",main="Formation: Reciprocity, Sender-Receiver",cex.main=3,cex.lab=2.5,cex.axis=2,yaxt="n",xaxt="n")
axis(2,at=c(5:-13),c(seq(from=0.05,to=-0.13,by=-0.01)),cex.axis=2)
axis(1,at=1:100,round(p_form[[1]][[5]]$x,0),cex.axis=2)
polygon(c(1:100, rev(1:100)), c(p_form[[which(colMeans(CI_up)==max(colMeans(CI_up)))+1]][[5]]$fit+p_form[[which(colMeans(CI_up)==max(colMeans(CI_up)))+1]][[5]]$se, rev(p_form[[which(colMeans(CI_low)==min(colMeans(CI_low)))+1]][[5]]$fit-p_form[[which(colMeans(CI_low)==min(colMeans(CI_low)))+1]][[5]]$se)),
        col = "lightgray", border = NA)


polygon(c(1:100, rev(1:100)), c(p_form[[1]][[5]]$fit+p_form[[1]][[5]]$se, rev(p_form[[1]][[5]]$fit-p_form[[1]][[5]]$se)),
        col = "darkgray", border = NA)
lines(p_form[[1]][[5]]$fit,type="l",col="black",ylim = c(-8,5),yaxt="n",lwd=3)
abline(h=0,lty=2)

for (i in 2:length(p_form)){
  lines(p_form[[i]][[5]]$fit,col="black",lty=2)
  
}

##
CI_up<-c()
CI_low<-c()
for (i in 2:length(p_diss)){
  CI_up<-cbind(CI_up,p_diss[[i]][[5]]$fit+p_diss[[i]][[5]]$se)
  CI_low<-cbind(CI_low,p_diss[[i]][[5]]$fit-p_diss[[i]][[5]]$se)
}

plot(p_diss[[1]][[5]]$fit,type="l",col="black",ylim = c(-1,2),yaxt="n",lwd=2,ylab =expression(paste(theta,"(t)")),xlab="t",main="Persistence: Reciprocity, Sender-Receiver",cex.main=3,cex.lab=2.5,cex.axis=2,yaxt="n",xaxt="n")
axis(2,at=c(5:-13),c(seq(from=0.05,to=-0.13,by=-0.01)),cex.axis=2)
axis(1,at=1:100,round(p_diss[[1]][[5]]$x,0),cex.axis=2)
polygon(c(1:100, rev(1:100)), c(p_diss[[which(colMeans(CI_up)==max(colMeans(CI_up)))+1]][[5]]$fit+p_diss[[which(colMeans(CI_up)==max(colMeans(CI_up)))+1]][[5]]$se, rev(p_diss[[which(colMeans(CI_low)==min(colMeans(CI_low)))+1]][[5]]$fit-p_diss[[which(colMeans(CI_low)==min(colMeans(CI_low)))+1]][[5]]$se)),
        col = "lightgray", border = NA)


polygon(c(1:100, rev(1:100)), c(p_diss[[1]][[5]]$fit+p_diss[[1]][[5]]$se, rev(p_diss[[1]][[5]]$fit-p_diss[[1]][[5]]$se)),
        col = "darkgray", border = NA)
lines(p_diss[[1]][[5]]$fit,type="l",col="black",ylim = c(-8,5),yaxt="n",lwd=3)
abline(h=0,lty=2)

for (i in 2:length(p_diss)){
  lines(p_diss[[i]][[5]]$fit,col="black",lty=2)
  
}

##
CI_up<-c()
CI_low<-c()
for (i in 2:length(p_form)){
  CI_up<-cbind(CI_up,p_form[[i]][[6]]$fit+p_form[[i]][[6]]$se)
  CI_low<-cbind(CI_low,p_form[[i]][[6]]$fit-p_form[[i]][[6]]$se)
}

plot(p_form[[1]][[6]]$fit,type="l",col="black",ylim = c(-20,60),yaxt="n",lwd=2,ylab =expression(paste(theta,"(t)")),xlab="t",main="Formation: Transitivity, Sender-Receiver",cex.main=3,cex.lab=2.5,cex.axis=2,yaxt="n",xaxt="n")
axis(2,at=c(60:-20),c(seq(from=0.60,to=-0.20,by=-0.01)),cex.axis=2)
axis(1,at=1:100,round(p_form[[1]][[6]]$x,0),cex.axis=2)
polygon(c(1:100, rev(1:100)), c(p_form[[which(colMeans(CI_up)==max(colMeans(CI_up)))+1]][[6]]$fit+p_form[[which(colMeans(CI_up)==max(colMeans(CI_up)))+1]][[6]]$se, rev(p_form[[which(colMeans(CI_low)==min(colMeans(CI_low)))+1]][[6]]$fit-p_form[[which(colMeans(CI_low)==min(colMeans(CI_low)))+1]][[6]]$se)),
        col = "lightgray", border = NA)


polygon(c(1:100, rev(1:100)), c(p_form[[1]][[6]]$fit+p_form[[1]][[6]]$se, rev(p_form[[1]][[6]]$fit-p_form[[1]][[6]]$se)),
        col = "darkgray", border = NA)
lines(p_form[[1]][[6]]$fit,type="l",col="black",ylim = c(-8,5),yaxt="n",lwd=3)
abline(h=0,lty=2)

for (i in 2:length(p_form)){
  lines(p_form[[i]][[6]]$fit,col="black",lty=2)
  
}

##
CI_up<-c()
CI_low<-c()
for (i in 2:length(p_diss)){
  CI_up<-cbind(CI_up,p_diss[[i]][[6]]$fit+p_diss[[i]][[6]]$se)
  CI_low<-cbind(CI_low,p_diss[[i]][[6]]$fit-p_diss[[i]][[6]]$se)
}

plot(p_diss[[1]][[6]]$fit,type="l",col="black",ylim = c(-20,60),yaxt="n",lwd=2,ylab =expression(paste(theta,"(t)")),xlab="t",main="Persistence: Transitivity, Sender-Receiver",cex.main=3,cex.lab=2.5,cex.axis=2,yaxt="n",xaxt="n")
axis(2,at=c(60:-20),c(seq(from=0.60,to=-0.20,by=-0.01)),cex.axis=2)
axis(1,at=1:100,round(p_diss[[1]][[6]]$x,0),cex.axis=2)
polygon(c(1:100, rev(1:100)), c(p_diss[[which(colMeans(CI_up)==max(colMeans(CI_up)))+1]][[6]]$fit+p_diss[[which(colMeans(CI_up)==max(colMeans(CI_up)))+1]][[6]]$se, rev(p_diss[[which(colMeans(CI_low)==min(colMeans(CI_low)))+1]][[6]]$fit-p_diss[[which(colMeans(CI_low)==min(colMeans(CI_low)))+1]][[6]]$se)),
        col = "lightgray", border = NA)


polygon(c(1:100, rev(1:100)), c(p_diss[[1]][[6]]$fit+p_diss[[1]][[6]]$se, rev(p_diss[[1]][[6]]$fit-p_diss[[1]][[6]]$se)),
        col = "darkgray", border = NA)
lines(p_diss[[1]][[6]]$fit,type="l",col="black",ylim = c(-8,5),yaxt="n",lwd=3)
abline(h=0,lty=2)

for (i in 2:length(p_diss)){
  lines(p_diss[[i]][[6]]$fit,col="black",lty=2)
  
}

##
CI_up<-c()
CI_low<-c()
for (i in 2:length(p_form)){
  CI_up<-cbind(CI_up,p_form[[i]][[7]]$fit+p_form[[i]][[7]]$se)
  CI_low<-cbind(CI_low,p_form[[i]][[7]]$fit-p_form[[i]][[7]]$se)
}

plot(p_form[[1]][[7]]$fit,type="l",col="black",ylim = c(-55,45),yaxt="n",lwd=2,ylab =expression(paste(theta,"(t)")),xlab="t",main="Formation: Shared Suppliers, Sender-Receiver",cex.main=3,cex.lab=2.5,cex.axis=2,yaxt="n",xaxt="n")
axis(2,at=c(50:-40),c(seq(from=0.50,to=-0.40,by=-0.01)),cex.axis=2)
axis(1,at=1:100,round(p_form[[1]][[7]]$x,0),cex.axis=2)
polygon(c(1:100, rev(1:100)), c(p_form[[which(colMeans(CI_up)==max(colMeans(CI_up)))+1]][[7]]$fit+p_form[[which(colMeans(CI_up)==max(colMeans(CI_up)))+1]][[7]]$se, rev(p_form[[which(colMeans(CI_low)==min(colMeans(CI_low)))+1]][[7]]$fit-p_form[[which(colMeans(CI_low)==min(colMeans(CI_low)))+1]][[7]]$se)),
        col = "lightgray", border = NA)


polygon(c(1:100, rev(1:100)), c(p_form[[1]][[7]]$fit+p_form[[1]][[7]]$se, rev(p_form[[1]][[7]]$fit-p_form[[1]][[7]]$se)),
        col = "darkgray", border = NA)
lines(p_form[[1]][[7]]$fit,type="l",col="black",ylim = c(-8,5),yaxt="n",lwd=3)
abline(h=0,lty=2)

for (i in 2:length(p_form)){
  lines(p_form[[i]][[7]]$fit,col="black",lty=2)
  
}

##
CI_up<-c()
CI_low<-c()
for (i in 2:length(p_diss)){
  CI_up<-cbind(CI_up,p_diss[[i]][[7]]$fit+p_diss[[i]][[7]]$se)
  CI_low<-cbind(CI_low,p_diss[[i]][[7]]$fit-p_diss[[i]][[7]]$se)
}

plot(p_diss[[1]][[7]]$fit,type="l",col="black",ylim = c(-55,45),yaxt="n",lwd=2,ylab =expression(paste(theta,"(t)")),xlab="t",main="Persistence: Shared Suppliers, Sender-Receiver",cex.main=3,cex.lab=2.5,cex.axis=2,yaxt="n",xaxt="n")
axis(2,at=c(50:-40),c(seq(from=0.50,to=-0.40,by=-0.01)),cex.axis=2)
axis(1,at=1:100,round(p_diss[[1]][[7]]$x,0),cex.axis=2)
polygon(c(1:100, rev(1:100)), c(p_diss[[which(colMeans(CI_up)==max(colMeans(CI_up)))+1]][[7]]$fit+p_diss[[which(colMeans(CI_up)==max(colMeans(CI_up)))+1]][[7]]$se, rev(p_diss[[which(colMeans(CI_low)==min(colMeans(CI_low)))+1]][[7]]$fit-p_diss[[which(colMeans(CI_low)==min(colMeans(CI_low)))+1]][[7]]$se)),
        col = "lightgray", border = NA)


polygon(c(1:100, rev(1:100)), c(p_diss[[1]][[7]]$fit+p_diss[[1]][[7]]$se, rev(p_diss[[1]][[7]]$fit-p_diss[[1]][[7]]$se)),
        col = "darkgray", border = NA)
lines(p_diss[[1]][[7]]$fit,type="l",col="black",ylim = c(-8,5),yaxt="n",lwd=3)
abline(h=0,lty=2)

for (i in 2:length(p_diss)){
  lines(p_diss[[i]][[7]]$fit,col="black",lty=2)
  
}


dev.off()




# Figure 5: Time-Varying Coefficients: Political and Economic Covariates
pdf("4) Visualizing the Results/Figures/windows2.pdf",width = 20,height=25)



#  politics
par(mfrow=c(7,2),mai = c(0.8,0.8,0.8,0.8))

##
CI_up<-c()
CI_low<-c()
for (i in 2:length(p_form)){
  CI_up<-cbind(CI_up,p_form[[i]][[4]]$fit+p_form[[i]][[4]]$se)
  CI_low<-cbind(CI_low,p_form[[i]][[4]]$fit-p_form[[i]][[4]]$se)
}

plot(p_form[[1]][[4]]$fit,type="l",col="black",ylim = c(-1,3),lwd=2,ylab =expression(paste(theta,"(t)")),xlab="t",main="Formation: Alliance, Sender-Receiver",cex.main=3,cex.lab=2.5,cex.axis=2,xaxt="n")
axis(1,at=1:100,round(p_form[[1]][[4]]$x,0),cex.axis=2)
polygon(c(1:100, rev(1:100)), c(p_form[[which(colMeans(CI_up)==max(colMeans(CI_up)))+1]][[4]]$fit+p_form[[which(colMeans(CI_up)==max(colMeans(CI_up)))+1]][[4]]$se, rev(p_form[[which(colMeans(CI_low)==min(colMeans(CI_low)))+1]][[4]]$fit-p_form[[which(colMeans(CI_low)==min(colMeans(CI_low)))+1]][[4]]$se)),
        col = "lightgray", border = NA)


polygon(c(1:100, rev(1:100)), c(p_form[[1]][[4]]$fit+p_form[[1]][[4]]$se, rev(p_form[[1]][[4]]$fit-p_form[[1]][[4]]$se)),
        col = "darkgray", border = NA)
lines(p_form[[1]][[4]]$fit,type="l",col="black",ylim = c(-8,5),yaxt="n",lwd=3)
abline(h=0,lty=2)

for (i in 2:length(p_form)){
  lines(p_form[[i]][[4]]$fit,col="black",lty=2)
  
}

##
CI_up<-c()
CI_low<-c()
for (i in 2:length(p_diss)){
  CI_up<-cbind(CI_up,p_diss[[i]][[4]]$fit+p_diss[[i]][[4]]$se)
  CI_low<-cbind(CI_low,p_diss[[i]][[4]]$fit-p_diss[[i]][[4]]$se)
}

plot(p_diss[[1]][[4]]$fit,type="l",col="black",ylim = c(-1,3),lwd=2,ylab =expression(paste(theta,"(t)")),xlab="t",main="Persistence: Alliance, Sender-Receiver",cex.main=3,cex.lab=2.5,cex.axis=2,xaxt="n")
axis(1,at=1:100,round(p_diss[[1]][[4]]$x,0),cex.axis=2)
polygon(c(1:100, rev(1:100)), c(p_diss[[which(colMeans(CI_up)==max(colMeans(CI_up)))+1]][[4]]$fit+p_diss[[which(colMeans(CI_up)==max(colMeans(CI_up)))+1]][[4]]$se, rev(p_diss[[which(colMeans(CI_low)==min(colMeans(CI_low)))+1]][[4]]$fit-p_diss[[which(colMeans(CI_low)==min(colMeans(CI_low)))+1]][[4]]$se)),
        col = "lightgray", border = NA)


polygon(c(1:100, rev(1:100)), c(p_diss[[1]][[4]]$fit+p_diss[[1]][[4]]$se, rev(p_diss[[1]][[4]]$fit-p_diss[[1]][[4]]$se)),
        col = "darkgray", border = NA)
lines(p_diss[[1]][[4]]$fit,type="l",col="black",ylim = c(-8,5),yaxt="n",lwd=3)
abline(h=0,lty=2)

for (i in 2:length(p_diss)){
  lines(p_diss[[i]][[4]]$fit,col="black",lty=2)
  
}

##
CI_up<-c()
CI_low<-c()
for (i in 2:length(p_form)){
  CI_up<-cbind(CI_up,p_form[[i]][[8]]$fit+p_form[[i]][[8]]$se)
  CI_low<-cbind(CI_low,p_form[[i]][[8]]$fit-p_form[[i]][[8]]$se)
}

plot(p_form[[1]][[8]]$fit,type="l",col="black",ylim = c(-0.1,0.1),lwd=2,ylab =expression(paste(theta,"(t)")),xlab="t",main="Formation: Regime Dissimilarity, Sender-Receiver",cex.main=3,cex.lab=2.5,cex.axis=2,xaxt="n")
axis(1,at=1:100,round(p_form[[1]][[8]]$x,0),cex.axis=2)
polygon(c(1:100, rev(1:100)), c(p_form[[which(colMeans(CI_up)==max(colMeans(CI_up)))+1]][[8]]$fit+p_form[[which(colMeans(CI_up)==max(colMeans(CI_up)))+1]][[8]]$se, rev(p_form[[which(colMeans(CI_low)==min(colMeans(CI_low)))+1]][[8]]$fit-p_form[[which(colMeans(CI_low)==min(colMeans(CI_low)))+1]][[8]]$se)),
        col = "lightgray", border = NA)


polygon(c(1:100, rev(1:100)), c(p_form[[1]][[8]]$fit+p_form[[1]][[8]]$se, rev(p_form[[1]][[8]]$fit-p_form[[1]][[8]]$se)),
        col = "darkgray", border = NA)
lines(p_form[[1]][[8]]$fit,type="l",col="black",ylim = c(-8,5),yaxt="n",lwd=3)
abline(h=0,lty=2)

for (i in 2:length(p_form)){
  lines(p_form[[i]][[8]]$fit,col="black",lty=2)
  
}

##
CI_up<-c()
CI_low<-c()
for (i in 2:length(p_diss)){
  CI_up<-cbind(CI_up,p_diss[[i]][[8]]$fit+p_diss[[i]][[8]]$se)
  CI_low<-cbind(CI_low,p_diss[[i]][[8]]$fit-p_diss[[i]][[8]]$se)
}

plot(p_diss[[1]][[8]]$fit,type="l",col="black",ylim = c(-0.1,0.1),lwd=2,ylab =expression(paste(theta,"(t)")),xlab="t",main="Persistence: Regime Dissimilarity, Sender-Receiver",cex.main=3,cex.lab=2.5,cex.axis=2,xaxt="n")
axis(1,at=1:100,round(p_diss[[1]][[8]]$x,0),cex.axis=2)
polygon(c(1:100, rev(1:100)), c(p_diss[[which(colMeans(CI_up)==max(colMeans(CI_up)))+1]][[8]]$fit+p_diss[[which(colMeans(CI_up)==max(colMeans(CI_up)))+1]][[8]]$se, rev(p_diss[[which(colMeans(CI_low)==min(colMeans(CI_low)))+1]][[8]]$fit-p_diss[[which(colMeans(CI_low)==min(colMeans(CI_low)))+1]][[8]]$se)),
        col = "lightgray", border = NA)


polygon(c(1:100, rev(1:100)), c(p_diss[[1]][[8]]$fit+p_diss[[1]][[8]]$se, rev(p_diss[[1]][[8]]$fit-p_diss[[1]][[8]]$se)),
        col = "darkgray", border = NA)
lines(p_diss[[1]][[8]]$fit,type="l",col="black",ylim = c(-8,5),yaxt="n",lwd=3)
abline(h=0,lty=2)

for (i in 2:length(p_diss)){
  lines(p_diss[[i]][[8]]$fit,col="black",lty=2)
  
}

##
CI_up<-c()
CI_low<-c()
for (i in 2:length(p_form)){
  CI_up<-cbind(CI_up,p_form[[i]][[11]]$fit+p_form[[i]][[11]]$se)
  CI_low<-cbind(CI_low,p_form[[i]][[11]]$fit-p_form[[i]][[11]]$se)
}

plot(p_form[[1]][[11]]$fit,type="l",col="black",ylim = c(0,1.5),lwd=2,ylab =expression(paste(theta,"(t)")),xlab="t",main="Formation: log GDP, Sender",cex.main=3,cex.lab=2.5,cex.axis=2,xaxt="n")
axis(1,at=1:100,round(p_form[[1]][[11]]$x,0),cex.axis=2)
polygon(c(1:100, rev(1:100)), c(p_form[[which(colMeans(CI_up)==max(colMeans(CI_up)))+1]][[11]]$fit+p_form[[which(colMeans(CI_up)==max(colMeans(CI_up)))+1]][[11]]$se, rev(p_form[[which(colMeans(CI_low)==min(colMeans(CI_low)))+1]][[11]]$fit-p_form[[which(colMeans(CI_low)==min(colMeans(CI_low)))+1]][[11]]$se)),
        col = "lightgray", border = NA)


polygon(c(1:100, rev(1:100)), c(p_form[[1]][[11]]$fit+p_form[[1]][[11]]$se, rev(p_form[[1]][[11]]$fit-p_form[[1]][[11]]$se)),
        col = "darkgray", border = NA)
lines(p_form[[1]][[11]]$fit,type="l",col="black",ylim = c(-8,5),yaxt="n",lwd=3)
abline(h=0,lty=2)

for (i in 2:length(p_form)){
  lines(p_form[[i]][[11]]$fit,col="black",lty=2)
  
}

##
CI_up<-c()
CI_low<-c()
for (i in 2:length(p_diss)){
  CI_up<-cbind(CI_up,p_diss[[i]][[11]]$fit+p_diss[[i]][[11]]$se)
  CI_low<-cbind(CI_low,p_diss[[i]][[11]]$fit-p_diss[[i]][[11]]$se)
}

plot(p_diss[[1]][[11]]$fit,type="l",col="black",ylim = c(0,1.5),lwd=2,ylab =expression(paste(theta,"(t)")),xlab="t",main="Persistence: log GDP, Sender",cex.main=3,cex.lab=2.5,cex.axis=2,xaxt="n")
axis(1,at=1:100,round(p_diss[[1]][[11]]$x,0),cex.axis=2)
polygon(c(1:100, rev(1:100)), c(p_diss[[which(colMeans(CI_up)==max(colMeans(CI_up)))+1]][[11]]$fit+p_diss[[which(colMeans(CI_up)==max(colMeans(CI_up)))+1]][[11]]$se, rev(p_diss[[which(colMeans(CI_low)==min(colMeans(CI_low)))+1]][[11]]$fit-p_diss[[which(colMeans(CI_low)==min(colMeans(CI_low)))+1]][[11]]$se)),
        col = "lightgray", border = NA)


polygon(c(1:100, rev(1:100)), c(p_diss[[1]][[11]]$fit+p_diss[[1]][[11]]$se, rev(p_diss[[1]][[11]]$fit-p_diss[[1]][[11]]$se)),
        col = "darkgray", border = NA)
lines(p_diss[[1]][[11]]$fit,type="l",col="black",ylim = c(-8,5),yaxt="n",lwd=3)
abline(h=0,lty=2)

for (i in 2:length(p_diss)){
  lines(p_diss[[i]][[11]]$fit,col="black",lty=2)
  
}

##
CI_up<-c()
CI_low<-c()
for (i in 2:length(p_form)){
  CI_up<-cbind(CI_up,p_form[[i]][[12]]$fit+p_form[[i]][[12]]$se)
  CI_low<-cbind(CI_low,p_form[[i]][[12]]$fit-p_form[[i]][[12]]$se)
}

plot(p_form[[1]][[12]]$fit,type="l",col="black",ylim = c(0,0.5),lwd=2,ylab =expression(paste(theta,"(t)")),xlab="t",main="Formation: log GDP, Receiver",cex.main=3,cex.lab=2.5,cex.axis=2,xaxt="n")
axis(1,at=1:100,round(p_form[[1]][[12]]$x,0),cex.axis=2)
polygon(c(1:100, rev(1:100)), c(p_form[[which(colMeans(CI_up)==max(colMeans(CI_up)))+1]][[12]]$fit+p_form[[which(colMeans(CI_up)==max(colMeans(CI_up)))+1]][[12]]$se, rev(p_form[[which(colMeans(CI_low)==min(colMeans(CI_low)))+1]][[12]]$fit-p_form[[which(colMeans(CI_low)==min(colMeans(CI_low)))+1]][[12]]$se)),
        col = "lightgray", border = NA)


polygon(c(1:100, rev(1:100)), c(p_form[[1]][[12]]$fit+p_form[[1]][[12]]$se, rev(p_form[[1]][[12]]$fit-p_form[[1]][[12]]$se)),
        col = "darkgray", border = NA)
lines(p_form[[1]][[12]]$fit,type="l",col="black",ylim = c(-8,5),yaxt="n",lwd=3)
abline(h=0,lty=2)

for (i in 2:length(p_form)){
  lines(p_form[[i]][[12]]$fit,col="black",lty=2)
  
}

##
CI_up<-c()
CI_low<-c()
for (i in 2:length(p_diss)){
  CI_up<-cbind(CI_up,p_diss[[i]][[12]]$fit+p_diss[[i]][[12]]$se)
  CI_low<-cbind(CI_low,p_diss[[i]][[12]]$fit-p_diss[[i]][[12]]$se)
}

plot(p_diss[[1]][[12]]$fit,type="l",col="black",ylim = c(0,0.5),lwd=2,ylab =expression(paste(theta,"(t)")),xlab="t",main="Persistence: log GDP, Receiver",cex.main=3,cex.lab=2.5,cex.axis=2,xaxt="n")
axis(1,at=1:100,round(p_diss[[1]][[12]]$x,0),cex.axis=2)
polygon(c(1:100, rev(1:100)), c(p_diss[[which(colMeans(CI_up)==max(colMeans(CI_up)))+1]][[12]]$fit+p_diss[[which(colMeans(CI_up)==max(colMeans(CI_up)))+1]][[12]]$se, rev(p_diss[[which(colMeans(CI_low)==min(colMeans(CI_low)))+1]][[12]]$fit-p_diss[[which(colMeans(CI_low)==min(colMeans(CI_low)))+1]][[12]]$se)),
        col = "lightgray", border = NA)


polygon(c(1:100, rev(1:100)), c(p_diss[[1]][[12]]$fit+p_diss[[1]][[12]]$se, rev(p_diss[[1]][[12]]$fit-p_diss[[1]][[12]]$se)),
        col = "darkgray", border = NA)
lines(p_diss[[1]][[12]]$fit,type="l",col="black",ylim = c(-8,5),yaxt="n",lwd=3)
abline(h=0,lty=2)

for (i in 2:length(p_diss)){
  lines(p_diss[[i]][[12]]$fit,col="black",lty=2)
  
}

##
CI_up<-c()
CI_low<-c()
for (i in 2:length(p_form)){
  CI_up<-cbind(CI_up,p_form[[i]][[3]]$fit+p_form[[i]][[3]]$se)
  CI_low<-cbind(CI_low,p_form[[i]][[3]]$fit-p_form[[i]][[3]]$se)
}

plot(p_form[[1]][[3]]$fit,type="l",col="black",ylim = c(-0.2,0.2),lwd=2,ylab =expression(paste(theta,"(t)")),xlab="t",main="Formation: log Distance, Sender-Receiver",cex.main=3,cex.lab=2.5,cex.axis=2,xaxt="n")
axis(1,at=1:100,round(p_form[[1]][[3]]$x,0),cex.axis=2)
polygon(c(1:100, rev(1:100)), c(p_form[[which(colMeans(CI_up)==max(colMeans(CI_up)))+1]][[3]]$fit+p_form[[which(colMeans(CI_up)==max(colMeans(CI_up)))+1]][[3]]$se, rev(p_form[[which(colMeans(CI_low)==min(colMeans(CI_low)))+1]][[3]]$fit-p_form[[which(colMeans(CI_low)==min(colMeans(CI_low)))+1]][[3]]$se)),
        col = "lightgray", border = NA)


polygon(c(1:100, rev(1:100)), c(p_form[[1]][[3]]$fit+p_form[[1]][[3]]$se, rev(p_form[[1]][[3]]$fit-p_form[[1]][[3]]$se)),
        col = "darkgray", border = NA)
lines(p_form[[1]][[3]]$fit,type="l",col="black",ylim = c(-8,5),yaxt="n",lwd=3)
abline(h=0,lty=2)

for (i in 2:length(p_form)){
  lines(p_form[[i]][[3]]$fit,col="black",lty=2)
  
}

##
CI_up<-c()
CI_low<-c()
for (i in 2:length(p_diss)){
  CI_up<-cbind(CI_up,p_diss[[i]][[3]]$fit+p_diss[[i]][[3]]$se)
  CI_low<-cbind(CI_low,p_diss[[i]][[3]]$fit-p_diss[[i]][[3]]$se)
}

plot(p_diss[[1]][[3]]$fit,type="l",col="black",ylim = c(-0.2,0.2),lwd=2,ylab =expression(paste(theta,"(t)")),xlab="t",main="Persistence: log Distance, Sender-Receiver",cex.main=3,cex.lab=2.5,cex.axis=2,xaxt="n")
axis(1,at=1:100,round(p_diss[[1]][[3]]$x,0),cex.axis=2)
polygon(c(1:100, rev(1:100)), c(p_diss[[which(colMeans(CI_up)==max(colMeans(CI_up)))+1]][[3]]$fit+p_diss[[which(colMeans(CI_up)==max(colMeans(CI_up)))+1]][[3]]$se, rev(p_diss[[which(colMeans(CI_low)==min(colMeans(CI_low)))+1]][[3]]$fit-p_diss[[which(colMeans(CI_low)==min(colMeans(CI_low)))+1]][[3]]$se)),
        col = "lightgray", border = NA)


polygon(c(1:100, rev(1:100)), c(p_diss[[1]][[3]]$fit+p_diss[[1]][[3]]$se, rev(p_diss[[1]][[3]]$fit-p_diss[[1]][[3]]$se)),
        col = "darkgray", border = NA)
lines(p_diss[[1]][[3]]$fit,type="l",col="black",ylim = c(-8,5),yaxt="n",lwd=3)
abline(h=0,lty=2)

for (i in 2:length(p_diss)){
  lines(p_diss[[i]][[3]]$fit,col="black",lty=2)
  
}

##
CI_up<-c()
CI_low<-c()
for (i in 2:length(p_form)){
  CI_up<-cbind(CI_up,p_form[[i]][[13]]$fit+p_form[[i]][[13]]$se)
  CI_low<-cbind(CI_low,p_form[[i]][[13]]$fit-p_form[[i]][[13]]$se)
}

plot(p_form[[1]][[13]]$fit,type="l",col="black",ylim = c(-0.1,0.45),lwd=2,ylab =expression(paste(theta,"(t)")),xlab="t",main="Formation: log Military Expenditures, Sender",cex.main=3,cex.lab=2.5,cex.axis=2,xaxt="n")
axis(1,at=1:100,round(p_form[[1]][[13]]$x,0),cex.axis=2)
polygon(c(1:100, rev(1:100)), c(p_form[[which(colMeans(CI_up)==max(colMeans(CI_up)))+1]][[13]]$fit+p_form[[which(colMeans(CI_up)==max(colMeans(CI_up)))+1]][[13]]$se, rev(p_form[[which(colMeans(CI_low)==min(colMeans(CI_low)))+1]][[13]]$fit-p_form[[which(colMeans(CI_low)==min(colMeans(CI_low)))+1]][[13]]$se)),
        col = "lightgray", border = NA)


polygon(c(1:100, rev(1:100)), c(p_form[[1]][[13]]$fit+p_form[[1]][[13]]$se, rev(p_form[[1]][[13]]$fit-p_form[[1]][[13]]$se)),
        col = "darkgray", border = NA)
lines(p_form[[1]][[13]]$fit,type="l",col="black",ylim = c(-8,5),yaxt="n",lwd=3)
abline(h=0,lty=2)

for (i in 2:length(p_form)){
  lines(p_form[[i]][[13]]$fit,col="black",lty=2)
  
}

##
CI_up<-c()
CI_low<-c()
for (i in 2:length(p_diss)){
  CI_up<-cbind(CI_up,p_diss[[i]][[13]]$fit+p_diss[[i]][[13]]$se)
  CI_low<-cbind(CI_low,p_diss[[i]][[13]]$fit-p_diss[[i]][[13]]$se)
}

plot(p_diss[[1]][[13]]$fit,type="l",col="black",ylim = c(-0.1,0.45),lwd=2,ylab =expression(paste(theta,"(t)")),xlab="t",main="Persistence: log Military Expenditures, Sender",cex.main=3,cex.lab=2.5,cex.axis=2,xaxt="n")
axis(1,at=1:100,round(p_diss[[1]][[13]]$x,0),cex.axis=2)
polygon(c(1:100, rev(1:100)), c(p_diss[[which(colMeans(CI_up)==max(colMeans(CI_up)))+1]][[13]]$fit+p_diss[[which(colMeans(CI_up)==max(colMeans(CI_up)))+1]][[13]]$se, rev(p_diss[[which(colMeans(CI_low)==min(colMeans(CI_low)))+1]][[13]]$fit-p_diss[[which(colMeans(CI_low)==min(colMeans(CI_low)))+1]][[13]]$se)),
        col = "lightgray", border = NA)


polygon(c(1:100, rev(1:100)), c(p_diss[[1]][[13]]$fit+p_diss[[1]][[13]]$se, rev(p_diss[[1]][[13]]$fit-p_diss[[1]][[13]]$se)),
        col = "darkgray", border = NA)
lines(p_diss[[1]][[13]]$fit,type="l",col="black",ylim = c(-8,5),yaxt="n",lwd=3)
abline(h=0,lty=2)

for (i in 2:length(p_diss)){
  lines(p_diss[[i]][[13]]$fit,col="black",lty=2)
  
}

##
CI_up<-c()
CI_low<-c()
for (i in 2:length(p_form)){
  CI_up<-cbind(CI_up,p_form[[i]][[14]]$fit+p_form[[i]][[14]]$se)
  CI_low<-cbind(CI_low,p_form[[i]][[14]]$fit-p_form[[i]][[14]]$se)
}

plot(p_form[[1]][[14]]$fit,type="l",col="black",ylim = c(-0.2,0.5),lwd=2,ylab =expression(paste(theta,"(t)")),xlab="t",main="Formation: log Military Expenditures, Receiver",cex.main=3,cex.lab=2.5,cex.axis=2,xaxt="n")
axis(1,at=1:100,round(p_form[[1]][[14]]$x,0),cex.axis=2)
polygon(c(1:100, rev(1:100)), c(p_form[[which(colMeans(CI_up)==max(colMeans(CI_up)))+1]][[14]]$fit+p_form[[which(colMeans(CI_up)==max(colMeans(CI_up)))+1]][[14]]$se, rev(p_form[[which(colMeans(CI_low)==min(colMeans(CI_low)))+1]][[14]]$fit-p_form[[which(colMeans(CI_low)==min(colMeans(CI_low)))+1]][[14]]$se)),
        col = "lightgray", border = NA)


polygon(c(1:100, rev(1:100)), c(p_form[[1]][[14]]$fit+p_form[[1]][[14]]$se, rev(p_form[[1]][[14]]$fit-p_form[[1]][[14]]$se)),
        col = "darkgray", border = NA)
lines(p_form[[1]][[14]]$fit,type="l",col="black",ylim = c(-8,5),yaxt="n",lwd=3)
abline(h=0,lty=2)

for (i in 2:length(p_form)){
  lines(p_form[[i]][[14]]$fit,col="black",lty=2)
  
}

##
CI_up<-c()
CI_low<-c()
for (i in 2:length(p_diss)){
  CI_up<-cbind(CI_up,p_diss[[i]][[14]]$fit+p_diss[[i]][[14]]$se)
  CI_low<-cbind(CI_low,p_diss[[i]][[14]]$fit-p_diss[[i]][[14]]$se)
}

plot(p_diss[[1]][[14]]$fit,type="l",col="black",ylim = c(-0.2,0.5),lwd=2,ylab =expression(paste(theta,"(t)")),xlab="t",main="Persistence: log Military Expenditures, Receiver",cex.main=3,cex.lab=2.5,cex.axis=2,xaxt="n")
axis(1,at=1:100,round(p_diss[[1]][[14]]$x,0),cex.axis=2)
polygon(c(1:100, rev(1:100)), c(p_diss[[which(colMeans(CI_up)==max(colMeans(CI_up)))+1]][[14]]$fit+p_diss[[which(colMeans(CI_up)==max(colMeans(CI_up)))+1]][[14]]$se, rev(p_diss[[which(colMeans(CI_low)==min(colMeans(CI_low)))+1]][[14]]$fit-p_diss[[which(colMeans(CI_low)==min(colMeans(CI_low)))+1]][[14]]$se)),
        col = "lightgray", border = NA)


polygon(c(1:100, rev(1:100)), c(p_diss[[1]][[14]]$fit+p_diss[[1]][[14]]$se, rev(p_diss[[1]][[14]]$fit-p_diss[[1]][[14]]$se)),
        col = "darkgray", border = NA)
lines(p_diss[[1]][[14]]$fit,type="l",col="black",ylim = c(-8,5),yaxt="n",lwd=3)
abline(h=0,lty=2)

for (i in 2:length(p_diss)){
  lines(p_diss[[i]][[14]]$fit,col="black",lty=2)
  
}
dev.off()
