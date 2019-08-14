#################################################################################
#################################################################################
# Bring the data in the needed format                                           #
#################################################################################

rm(list=ls())

# Load packages
library("igraph")


for (thres in seq(from=0,to=3,by=0.5)){

  # Set working Directory
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
  
  
  # Load the Data
  load("../Data.RData")
  # Load packages
  
  # Formation
  f_response<-c()
  f_dist_ij<-c()
  f_polity_i<-c()
  f_polity_j<-c()
  f_time<-c()
  f_recip_ji<-c()
  f_outdeg_i<-c()
  f_indeg_j<-c()
  f_outdeg_j<-c()
  f_indeg_i<-c()
  f_trans_ij<-c()
  f_revtrans_ij<-c()
  f_share_sup_ij<-c()
  f_share_cust_ij<-c()
  f_military_ex_i<-c()
  f_military_ex_j<-c()
  f_lgdp_i<-c()
  f_lgdp_j<-c()
  f_poldiff_ij<-c()
  f_sender_id<-c()
  f_defagr_ij<-c()
  f_receiver_id<-c()

f_lag<-c()


# Real outcome
net_in_t<-list()
net_in_t_1<-list()

for (t in 1951:2016){

# Define the identifier for A_t intersect A_t_1

A_t<-nEX[,t-1949][1:224]
A_t_1<-nEX[,t-1-1949][1:224]

####### Special Treatment for the Transition from the Soviet Union to Russia
####### and Czechoslovakia to Czech Republic and Slovakia
# Shift them on additional year into existence in order to have clear defined predecessor states

if (t==1992){
  # Set the SU into existence in 1992
  A_t[176]<-1
}

if (t==1993){
  # Set the Czechoslovakia into existence in 1992 and 1993
  A_t[52]<-1
  A_t_1[52]<-1
}

# Select those countries existing in both years
B_t_t_1<-A_t*A_t_1

# extract the data for t and t-1

Y<-amk[[t-1949]][1:224,1:224]
rS_t<-as.numeric(rowSums(Y)>thres)
cS_t<-as.numeric(colSums(Y)>thres)

if (t==1993){
  # The response of the Soviet Union will be counted for Russia in 1991/1992
  Y[176,]<-Y[154,]
}

if (t==1992){
  # The response of Czechoslovakia will be counted for Czech Republic and Slovakia in 1992/1993
  Y[52,]<-Y[51,]+Y[168,]
}

Y_t_1<-amk[[t-1950]][1:224,1:224]
rS_t_1<-as.numeric(rowSums(Y_t_1)>thres)
cS_t_1<-as.numeric(colSums(Y_t_1)>thres)

gdp<-GDPimp[,t-1949]
gdp_t_1<-GDPimp[,t-1950]

milex<-military_ex[,t-1949]
milex_t_1<-military_ex[,t-1950]

pol<-autopolity[t-1950]

dist<-cdist

pol<-poldiff(t)
pol_t_1<-poldiff(t-1)

def_agr_t_1<-daml[[t-1950]]

# Create the networks Y_t_t_1, Y_t_1_t
for (i in 224:1){
  if (B_t_t_1[i]==0){
    Y<-Y[-i,]
    Y<-Y[,-i]
    
    dist<-dist[-i,]
    dist<-dist[,-i]
    
    Y_t_1<-Y_t_1[-i,]
    Y_t_1<-Y_t_1[,-i]
    
    pol<-pol[-i]
    
    milex_t_1<-milex_t_1[-i]
    
    pol_t_1<-pol_t_1[,-i]
    pol_t_1<-pol_t_1[-i,]
    
    def_agr_t_1<-def_agr_t_1[-i,]
    def_agr_t_1<-def_agr_t_1[,-i]
    
    gdp_t_1<-gdp_t_1[-i]
    
  }
}


net_in_t[[t-1950]]<-Y
net_in_t_1[[t-1950]]<-Y_t_1

# now the covariates must be structured as matrices

# Military Expenditures for the sender
milex<-log(1+milex_t_1)
milex_i<-matrix(rep(milex,dim(Y)[1]),ncol=dim(Y)[1],nrow=dim(Y)[1])

# Military Expenditures for the receiver
milex_j<-t(matrix(rep(milex,dim(Y)[1]),ncol=dim(Y)[1],nrow=dim(Y)[1]))

#polity for the sender
mpolity_i<-matrix(rep(pol,dim(Y)[1]),ncol=dim(Y)[1],nrow=dim(Y)[1])

# polity for the receiver
mpolity_j<-t(matrix(rep(pol,dim(Y)[1]),ncol=dim(Y)[1],nrow=dim(Y)[1]))

# GDP for the sender
gdp<-log(gdp_t_1)
gdp_i<-matrix(rep(gdp,dim(Y)[1]),ncol=dim(Y)[1],nrow=dim(Y)[1])

# GDP for the receiver
gdp_j<-t(matrix(rep(gdp,dim(Y)[1]),ncol=dim(Y)[1],nrow=dim(Y)[1]))




# Define the response variable as a binary variable
Y[Y<=thres]<-0
Y[Y>thres]<-1

Y_t_1[Y_t_1<=thres]<-0
Y_t_1[Y_t_1>thres]<-1


# Now the interesting stuff with the network statistics

# Formation network
Y_new_formed<-Y



# Which could have formed
Y_potential<-matrix(1,ncol=dim(Y_new_formed)[1],nrow=dim(Y_new_formed)[1])

diag(Y_potential)<-0

# RECIPROCITY
# given that an edge (i,j) could have formed from t-1 to t
# was there an edge (i,j) in t-1
Y_rec_t_1<-matrix(0,ncol=dim(Y_new_formed)[1],nrow=dim(Y_new_formed)[1])
for (i in 1:dim(Y_new_formed)[1]){
  for (j in 1:dim(Y_new_formed)[1]){
    if (Y_potential[i,j]==1){
      if (Y_t_1[j,i]==1){
        Y_rec_t_1[i,j]<-1
      }
    }
  }
}

# The normed outdegree is the same as the unnormed one

Y_rec_t_1_n<-Y_rec_t_1

# OUTDEGREE
# given that an edge (i,j) could have formed from t-1 to t
# how many outdegrees were present by i
Y_outdeg_t_1<-rowSums(Y_t_1)
# Q how many outdegrees were possible in theory
# A N-1
# Norm the outdegrees
Y_outdeg_t_1_n<-Y_outdeg_t_1/(dim(Y)[1]-1)

# Formulate the outdegree as a matrix

Y_outdeg<-matrix(rep(Y_outdeg_t_1_n,dim(Y)[1]),ncol=dim(Y)[1],nrow=dim(Y)[1])

# INDEGREE
# given that an edge (i,j) could have formed from t-1 to t
# how many idegrees were present by j
Y_indeg_t_1<-colSums(Y_t_1)

# Q how many indegrees were possible in theory
# A N-1
# Norm the outdegrees
Y_indeg_t_1_n<-Y_indeg_t_1/(dim(Y)[1]-1)

# Formulate the indegree as a matrix

Y_indeg<-t(matrix(rep(Y_indeg_t_1_n,dim(Y)[1]),ncol=dim(Y)[1],nrow=dim(Y)[1]))

# Transitivity
# given that an edge (i,j) could have formed from t-1 to t
# how many transitiv relationship from i to k and from k to j were present in t-1
Y_trans_t_1<-matrix(0,ncol=dim(Y_new_formed)[1],nrow=dim(Y_new_formed)[1])
for (i in 1:dim(Y_new_formed)[1]){
  for (j in 1:dim(Y_new_formed)[1]){
    if (Y_potential[i,j]==1){
      
      sel_i<-which(Y_t_1[i,]==1)
      
      for(p in 1:length(sel_i)){
        
        sel_k<-which(Y_t_1[sel_i[p],]==1)
        
        if (j %in% sel_k){
          Y_trans_t_1[i,j]<-Y_trans_t_1[i,j]+1
        }
        
      }
    }
  }
}

# Q how many transitive pattern would be possible?
# A N-2

Y_trans_t_1_n<-Y_trans_t_1/(dim(Y)[1]-2)

# reverse Transitivity
# given that an edge (i,j) could have formed from t-1 to t
# how many transitive relationship from j to k and from k to i were present in t-1
Y_revtrans_t_1<-matrix(0,ncol=dim(Y_new_formed)[1],nrow=dim(Y_new_formed)[1])
for (i in 1:dim(Y_new_formed)[1]){
  for (j in 1:dim(Y_new_formed)[1]){
    if (Y_potential[i,j]==1){
      
      sel_j<-which(Y_t_1[j,]==1)
      
      for(p in 1:length(sel_j)){
        
        sel_k<-which(Y_t_1[sel_j[p],]==1)
        
        if (i %in% sel_k){
          Y_revtrans_t_1[i,j]<-Y_revtrans_t_1[i,j]+1
        }
        
      }
    }
  }
}

# Q how many transitive pattern would be possible?
# A N-2

Y_revtrans_t_1_n<-Y_revtrans_t_1/(dim(Y)[1]-2)

# SHARED SUPPLIER
# given that an edge (i,j) could have formed from t-1 to t
# how many commen supplier k with (k,i), (k,j) were present in t-1
Y_sharesup_t_1<-matrix(0,ncol=dim(Y_new_formed)[1],nrow=dim(Y_new_formed)[1])
for (i in 1:dim(Y_new_formed)[1]){
  for (j in 1:dim(Y_new_formed)[1]){
    if (Y_potential[i,j]==1){
      
      buy_i<-which(Y_t_1[,i]==1)
      buy_j<-which(Y_t_1[,j]==1)
      
      Y_sharesup_t_1[i,j]<-length(intersect(buy_i,buy_j))
    }
  }
}
# Q how many shared supplier would be possible?
# A N-2

Y_sharesup_t_1_n<-Y_sharesup_t_1/(dim(Y)[1]-2)

# SHARED CUSTOMER
# given that an edge (i,j) could have formed from t-1 to t
# how many common customer k with (i,k), (j,k) were present in t-1
Y_sharecus_t_1<-matrix(0,ncol=dim(Y_new_formed)[1],nrow=dim(Y_new_formed)[1])
for (i in 1:dim(Y_new_formed)[1]){
  for (j in 1:dim(Y_new_formed)[1]){
    if (Y_potential[i,j]==1){
      
      sel_i<-which(Y_t_1[i,]==1)
      sel_j<-which(Y_t_1[j,]==1)
      
      Y_sharecus_t_1[i,j]<-length(intersect(sel_i,sel_j))
    }
  }
}
# Q how many shared supplier would be possible?
# A N-2

Y_sharecus_t_1_n<-Y_sharecus_t_1/(dim(Y)[1]-2)



# now we have to select all observations and covariates with potential ties in t


response<-c()
dist_ij<-c()
polity_i<-c()
polity_j<-c()
time<-c()
recip_ji<-c()
outdeg_i<-c()
indeg_j<-c()
outdeg_j<-c()
indeg_i<-c()
trans_ij<-c()
revtrans_ij<-c()
share_sup_ij<-c()
share_cust_ij<-c()
military_ex_i<-c()
military_ex_j<-c()
lgdp_i<-c()
lgdp_j<-c()
poldiff_ij<-c()
sender_id<-c()
receiver_id<-c()
defagr_ij<-c()

lag_ij<-c()

for (i in 1:dim(Y)[1]){
  for (j in 1:dim(Y)[1]){
    if (Y_potential[i,j]==1){
      
      response<-c(response,Y_new_formed[i,j])
      lag_ij<-c(lag_ij,Y_t_1[i,j])
      time<-c(time,t)
      
      dist_ij<-c(dist_ij,log(1+cdist[i,j]))
      
      polity_i<-c(polity_i,mpolity_i[i,j])
      polity_j<-c(polity_j,mpolity_j[i,j])
      
      recip_ji<-c(recip_ji,Y_rec_t_1_n[i,j])
      
      outdeg_i<-c(outdeg_i,Y_outdeg[i,j])
      outdeg_j<-c(outdeg_j,Y_outdeg[j,i])
      
      indeg_j<-c(indeg_j,Y_indeg[i,j])
      indeg_i<-c(indeg_i,Y_indeg[j,i])
      
      trans_ij<-c(trans_ij,Y_trans_t_1_n[i,j])
      
      revtrans_ij<-c(revtrans_ij,Y_revtrans_t_1_n[i,j])
      
      share_sup_ij<-c(share_sup_ij,Y_sharesup_t_1_n[i,j])
      
      share_cust_ij<-c(share_cust_ij,Y_sharecus_t_1_n[i,j])
      
      military_ex_i<-c(military_ex_i,milex_i[i,j])
      
      military_ex_j<-c(military_ex_j,milex_j[i,j])
      
      lgdp_i<-c(lgdp_i,gdp_i[i,j])
      
      lgdp_j<-c(lgdp_j,gdp_j[i,j])
    
      defagr_ij<-c( defagr_ij, def_agr_t_1[i,j])
      
      poldiff_ij<-c(poldiff_ij,pol_t_1[i,j])
      
      sender_id<-c(sender_id,colnames(Y)[i])
      receiver_id<-c(receiver_id,colnames(Y)[j])
    }
  }
}

# Now we save the results for the formation

# Formation
f_response<-c(f_response,response)
f_polity_i<-c(f_polity_i,polity_i)
f_dist_ij<-c(f_dist_ij,dist_ij)
f_polity_j<-c(f_polity_j,polity_j)
f_time<-c(f_time,time)
f_recip_ji<-c(f_recip_ji,recip_ji)
f_outdeg_i<-c(f_outdeg_i,outdeg_i)
f_indeg_j<-c(f_indeg_j,indeg_j)
f_outdeg_j<-c(f_outdeg_j,outdeg_j)
f_indeg_i<-c(f_indeg_i,indeg_i)
f_trans_ij<-c(f_trans_ij,trans_ij)
f_revtrans_ij<-c(f_revtrans_ij,revtrans_ij)
f_share_sup_ij<-c(f_share_sup_ij,share_sup_ij)
f_share_cust_ij<-c(f_share_cust_ij,share_cust_ij)
f_military_ex_i<-c(f_military_ex_i,military_ex_i)
f_military_ex_j<-c(f_military_ex_j,military_ex_j)
f_lgdp_i<-c(f_lgdp_i,lgdp_i)
f_lgdp_j<-c(f_lgdp_j,lgdp_j)
f_poldiff_ij<-c(f_poldiff_ij,poldiff_ij)
f_sender_id<-c(f_sender_id,sender_id)
f_receiver_id<-c(f_receiver_id,receiver_id)
f_defagr_ij<-c(f_defagr_ij,defagr_ij)
f_lag<-c(f_lag,lag_ij)

print("Progress t=")
print(t)


}



save<-which(ls()%in%list(
                       "f_response","f_dist_ij","f_polity_i","f_polity_j","f_defagr_ij","f_time","f_recip_ji","f_outdeg_i","f_outdeg_j","f_indeg_j","f_indeg_i","f_trans_ij","f_revtrans_ij","f_share_sup_ij",
                       "f_share_cust_ij","f_military_ex_i","f_military_ex_j",
                       "f_lgdp_i","f_lgdp_j",
                       "f_poldiff_ij","f_sender_id","f_receiver_id","f_lag","net_in_t","net_in_t_1","thres"))

ls()[save]
save<-which(ls()%in%list(
  "f_response","f_dist_ij","f_polity_i","f_polity_j","f_defagr_ij","f_time","f_recip_ji","f_outdeg_i","f_outdeg_j","f_indeg_j","f_indeg_i","f_trans_ij","f_revtrans_ij","f_share_sup_ij",
  "f_share_cust_ij","f_military_ex_i","f_military_ex_j",
  "f_lgdp_i","f_lgdp_j",
  "f_poldiff_ij","f_sender_id","f_receiver_id","f_lag","net_in_t","net_in_t_1","thres"))
rm(list=ls()[-save])

save.image(paste("Datasets/Data_ready_for_tergm_regression_",thres,".RData",sep=""))


}
