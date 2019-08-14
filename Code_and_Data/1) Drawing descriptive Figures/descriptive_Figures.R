#################################################################################
#################################################################################
# Produce descriptive Stats                                                     #
#################################################################################


# remove the old stuff 
rm(list=ls())

##
# Set working Directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# Load the Data
load("../Data.RData")

##
# load some libraries
library("statnet")
library("stargazer")


pdf("Figures/Figure1_2.pdf",height = 10,width=20)
t=2016

# extract the network data 
Y<-amk[[t-1949]][1:224,1:224]

# give names to the nodes
colnames(Y)<-laenderliste[1:224,5]
rownames(Y)<-laenderliste[1:224,5]

# Discretize the network
Y[Y>0]<-1

# Define the Adjacency Matrix as Network
net<-network(Y)

# Scale the Nodes and the labels
cex_nodes<-1.5+rowSums(Y)*0.04
cex_label<-cex_nodes

# Plot the Network
plot(net,displayisolates=F,label=network.vertex.names(net),label.pos=5,label.cex=0.6,edge.lwd=1,vertex.col="gray",edge.col="lightgray",vertex.cex=cex_nodes)

dev.off()

##
# The International Arms Trade Network in 2015 (top) and 2016 (bottom),
# Vertex Size proportional to the Outdegree.

pdf("Figures/Figure1_1.pdf",height = 10,width=20)
t=2015

# extract the network data 
Y<-amk[[t-1949]][1:224,1:224]

# give names to the nodes
colnames(Y)<-laenderliste[1:224,5]
rownames(Y)<-laenderliste[1:224,5]

# Discretize the network
Y[Y>0]<-1

# Define the Adjacency Matrix as Network
net<-network(Y)

# Scale the Nodes and the labels
cex_nodes<-1.5+rowSums(Y)*0.04
cex_label<-cex_nodes

# Plot the Network
plot(net,displayisolates=F,label=network.vertex.names(net),label.pos=5,label.cex=0.6,edge.lwd=1,vertex.col="gray",edge.col="lightgray",vertex.cex=cex_nodes)

dev.off()


##
# load the igraph library and remove the statnet library as the are conflicting
detach("package:statnet", unload=TRUE)
library(igraph)

##
# Define some containers for variables needed
size<-c()
density<-c()
recip<-c()
transi<-c()
outdeg_dist<-list()
indeg_dist<-list()

for (t in 1950:2016){

# extract the network data 
Y<-amk[[t-1949]][1:224,1:224]
  
# Include only countries that are coded as existent
  for (i in 224:1){
    if (nEX[,t-1949][1:224][i]==0){
      Y<-Y[-i,]
      Y<-Y[,-i]
      
    }
  }

# Discretize the Network  
Y[Y>0]<-1
 
# Define the order as the sum of edges in the network  
order<-c(order,sum(Y))

# Define the size as the number of actors in the given period  
size<-c(size, dim(Y)[1])

# Define the density as the number of edges divided by the number of potential edges  
density<-c(density,sum(Y)/(dim(Y)[1]^2-dim(Y)[1]))

# Safe the Number of outdegrees and indegrees  
outdeg_dist[[t-1949]]<-rowSums(Y)
indeg_dist[[t-1949]]<-colSums(Y)
  
# Define an igraph object based on the adjacency matrix Y
gr<-graph_from_adjacency_matrix(Y)

# Let igraph calculate the degree of reciprovity and transitivity of the graph  
recip<-c(recip,reciprocity(gr))
transi<-c(transi,transitivity(gr))
  
}

# Define the relevant time-span
t<-1950:2016

##
# Time Series of Global Network Statistics for the
# International Arms Trade Network for the included Countries
pdf("Figures/Figure10.pdf",height = 10,width=10)
par(mfrow=c(2,2),mar=c(4.5,4.5,4.5,4.5))
plot(t,size,type="l",ylab = "Number of Countries",xlab="t",cex.main=3,cex.lab=1.5,cex.axis=1.5)
grid()
plot(t,density,type="l",ylab = "Density",xlab="t",cex.main=3,cex.lab=1.5,cex.axis=1.5)
grid()
plot(t,recip,type="l",ylab = "Reciprocity",xlab="t",cex.main=3,cex.lab=1.5,cex.axis=1.5)
grid()
plot(t,transi,type="l",ylab = "Transitivity",xlab="t",cex.main=3,cex.lab=1.5,cex.axis=1.5)
grid()

##
# Degree Distributions of the  Outdegree (left) and Indegree (right),
# Averages over all Years with the Whiskers showing the Minimum and Maximum Values.
# In each Plot the Axes are in logarithmic Scale.
dev.off()

pdf("Figures/Figure2.pdf",height = 10,width=20)

par(mfrow=c(1,2),mar=c(5,5,5,5))

# Safe the Outdegree Distribution in a list
tables<-list()
for (t in 1:67){
tables[[t]]<-table(outdeg_dist[[t]])
}

# Safe the number of degrees for each year
all_deg<-c()
for (t in 1:67){
degree_t<-tables[[t]]   
all_deg<-c(all_deg,sum(degree_t))
}

# Safe the outdegrees with the respective occurence
collect<-list()
for (i in 0:66){
row<-c()
for (t in 1:67){
   sel<- which(rownames(tables[[t]])==paste(i))
   if (length(sel)==0){
     row<-c(row,0) 
   }
   if (length(sel)>0){
   row<-c(row,tables[[t]][sel]) 
   }
}
collect[[i+1]]<-row
}


mean<-c()
lower<-c()
higher<-c()
for (i in 1:67){
  mean<-c(mean,mean(collect[[i]]/all_deg))
  lower<-c(lower,min(c(collect[[i]])/all_deg))
  higher<-c(higher,max(c(collect[[i]])/all_deg))
}

lower[lower==0]<-0.00001
mean[mean==0]<-0.00001
options(scipen=999)
plot(1:67,mean,type="b",log = "xy",ylim=c(0.0001,1),xaxt="n",xlab="log(Outdegree)",ylab="log(Frequency)",cex.main=3,cex.lab=2,cex.axis=2)
lines(lower,col="gray")
lines(higher,col="gray")
axis(1,1:67,0:66,cex.lab=1.5,cex.axis=1.5)
arrows(1:67, lower, 1:67, higher, length=0.05, angle=90, code=3,col="gray")



####
tables<-list()
for (t in 1:67){
  tables[[t]]<-table(indeg_dist[[t]])
}



all_deg<-c()

for (t in 1:67){
  degree_t<-tables[[t]]   
  all_deg<-c(all_deg,sum(degree_t))
}


collect<-list()
for (i in 0:66){
  row<-c()
  for (t in 1:67){
    sel<- which(rownames(tables[[t]])==paste(i))
    if (length(sel)==0){
      row<-c(row,0) 
    }
    if (length(sel)>0){
      row<-c(row,tables[[t]][sel]) 
    }
  }
  collect[[i+1]]<-row
}

mean<-c()
lower<-c()
higher<-c()
for (i in 1:17){
  mean<-c(mean,mean(collect[[i]]/all_deg))
  
  lower<-c(lower,min(c(collect[[i]]/all_deg)))
  higher<-c(higher,max(c(collect[[i]]/all_deg)))
}
lower[lower==0]<-0.00001
plot(1:17,mean,log = "xy",xaxt="n",type="b",ylim = c(0.0001,1),xlab="log(Indegree)",ylab="log(Frequency)",cex.main=3,cex.lab=2,cex.axis=2)
lines(lower,col="gray")
lines(higher,col="gray")
arrows(1:17, lower, 1:17, higher, length=0.05, angle=90, code=3,col="gray")
axis(1,1:17,0:16,cex.lab=1.5,cex.axis=1.5)

dev.off()


load("Data.RData")

# Binarize the network

amk_bin<-list()

for (t in 1:length(amk)){
  amk_t<-amk[[t]][1:224,1:224]
  amk_t[amk_t>0]<-1
  
  amk_bin[[t]]<-amk_t
}


C<-c()

for (tau in 1:66){
  print(tau)
  
  mat2<-c()
  for (t in (tau+1):length(amk)){
    
    mat<-amk_bin[[t]]
    for (q in 1:tau){
      
      mat<-mat*amk_bin[[t-q]]
      
    }
    mat2<-c(mat2,sum(mat))
  }
  C<-c(C,sum(mat2))
}

all<-c()
for (i in 1:length(amk)){
  all<-c(all,sum(amk_bin[[i]]))
}

C<-c(sum(all),C)

Cd<-c()
for (i in (length(C)-1):1){
  Cd[i]<-C[i]-C[i+1]
}



Cd_per<-C/sum(all)
C_per<-Cd/sum(all)

pdf("Figures/Figure_share_distribution.pdf",height = 8,width=16)
par(mfrow=c(1,2),mar=c(4.5, 4.5, 4.5, 4.5))
plot(C_per,type = "S",xlab="Number of subsequent transfers",ylab="Share",cex.main=3,cex.lab=2,cex.axis=2,xaxt="n",ylim=c(0,0.35),lwd=3)
axis(1,0:length(C),cex.axis=2)
plot(Cd_per,type="S",xlab="Number of subsequent transfers",ylab="Cumulatiave Share",cex.main=3,cex.lab=2,cex.axis=2,xaxt="n",ylim=c(0,1),lwd=3)
axis(1,0:length(C),cex.axis=2)
dev.off()



#################################################################################
# Plot the distribution of the log-TIV                                          #
#################################################################################

all_data<-c()
for (t in 1:length(amk)){
  for (i in 1:dim(amk[[1]])[1]){
    for (j in 1:dim(amk[[1]])[1]){
      if (amk[[t]][i,j]!=0){
        all_data<-c(all_data,amk[[t]][i,j])
      }
    }    
  }
  print(t)
}

pdf("Figures/density.pdf",width = 8,height = 4)
plot(density(log(all_data)),xaxt="n",main="Kernel Density Estimator, TIV values of Arms Transfers")
axis(1,-4:8,round(exp(-4:8),2))
dev.off()

#################################################################################
# Show the quantiles of the TIV distribution                                    #
#################################################################################

stargazer(quantile(all_data,seq(0,0.25,0.025)),type="text")

rm(list=ls())