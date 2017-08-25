# Authors:Fangyu Qu, Jiawei Wei, Jenny Xu, Jing Zhang, Yunyan Zhang
# Columbia University
# Carry Trade on Currencies

# Set up 
library(chron)
library(lubridate)
library(ggplot2)
library(zoo)
library(reshape2)
setwd("/Users/jennyxu/Downloads/CarryTrade")
data<-read.csv("carry_to_risk.csv") #carry-to-risk signal: interest rate No.1 - interest ratw No.2)/annulized volatility between two currencies
forward<-read.csv("3mo_fwd.csv")
spot<-read.csv("spot.csv")
i_differential<-read.csv("Numerator.csv")

# Calculate positions according to the carry-to-risk signal
coln=length(data[1,])
row=length(data[,1])
position=matrix(0,ncol=coln-1,nrow=row)
for (i in 1:row)
{
  X=data[i,2:coln]
  buy=which.max(data[i,2:coln])
  buy2=which(X[1,] == (sort(unique(X),partial=coln-2)[coln-2])[1,1])
  position[i,buy+1]=1 
  position[i,buy2+1]=1
}

i_differential=i_differential[,-1]
i_matrix=matrix(0,nrow=row,ncol=2)
# Calculate return
for (i in 1:57){
i_matrix[i,1]=i_differential[i,which(position[i,]==1)[1]]
i_matrix[i,2]=i_differential[i,which(position[i,]==1)[2]]
}
## Calculate covariance of each 2 pair
spot[,1]<-as.Date(spot[,1],format='%Y/%m/%d')
exracov<-matrix(0,nrow = 57,ncol = 3)
for(i in 1:56){
  exracov[i,1]<-cov(spot[1+(21*(i-1)):(21*i),c(which(position[i,]==1)[1],which(position[i,]==1)[2])])[1,1]
  exracov[i,2]<-cov(spot[1+(21*(i-1)):(21*i),c(which(position[i,]==1)[1],which(position[i,]==1)[2])])[2,2]
  exracov[i,3]<-cov(spot[1+(21*(i-1)):(21*i),c(which(position[i,]==1)[1],which(position[i,]==1)[2])])[1,2]
}
  cova<-matrix(0,ncol = 2,nrow = 2)

# Mean-variance optimization function
optSharpe <- function(returns, cov){
  try(if(length(returns) != ncol(cov) | length(returns) != nrow(cov))
    stop("cov must be a square matrix with its # of rows of columns equal to length(returns)"))
  ones <-	rep(1, length(returns))
  A <- returns %*% solve(cov) %*% returns
  B <- returns %*% solve(cov) %*% ones
  C <- ones %*% solve(cov) %*% ones
  D <- A * C - B * B
  optSha <- A / B
  optPort <- ((optSha * C - B ) / D * returns) %*% solve(cov) + 
    ((A - optSha * B ) / D * ones) %*% solve(cov)
	if(optPort[1] > 2){
	optPort = c(2,-1)
	}
	if(optPort[2] > 2){
	optPort = c(-1,2)
	}
  return(optPort)
}

# Assume the initial portfolio balance to be 100,000,000
Initial=100000000
forward[,1]<-as.Date(forward[,1],format='%Y/%m/%d')
# Intitalize daily pnl
PNL=array(dim = length(forward[,1]))
PNL[1]=0
# Assume that we enter the position at the beginning of each month
m=0 #month
y=2012 #year
k=0 #carry-to-risk signal row
N=90
TC=0.0005 #Transition cost
for (n in 2:length(forward[,1]))
{
l=((month(forward[n,1])-1)%%3)*30+day(forward[n,1])
  if (month(forward[n-1,1])==12){ #if the date goes to next year, then the month should be 1
    if (year(forward[n,1])==(y+1)){
      m=1
      y=y+1
      k=k+1
      if (k==1)
      {opt_position=c(0.5,0.5)}
      else
      {cova[1,1]<-exracov[k-1,1]
      cova[1,2]<-exracov[k-1,3]
      cova[2,1]<-exracov[k-1,3]
      cova[2,2]<-exracov[k-1,2]
      opt_position=optSharpe(i_matrix[k,],cova)}
	    print(opt_position)
      f=forward[n,which(position[k,]!=0)+1]
      s0=spot[n,which(position[k,]!=0)+1]
      print(f)
      print(s0)
      PNL[n]=PNL[n-1]-0.0005
	    lastPNL = PNL[n]
    }
    else
      {
      s=spot[n,which(position[k,]==1)+1]
      PNL[n]=sum(as.matrix((f-s0)*(l/(s0*N))+(s-s0)/s0)*opt_position)+lastPNL
    }
  }else{
    if (month(forward[n,1])==(m+1)&&(m+1)<12){ #if the date goes to next month
      m=m+1
      k=k+1
      if (k==1)
      {opt_position=c(0.5,0.5)}
      else
      {cova[1,1]<-exracov[k-1,1]
      cova[1,2]<-exracov[k-1,3]
      cova[2,1]<-exracov[k-1,3]
      cova[2,2]<-exracov[k-1,2]
      opt_position=optSharpe(i_matrix[k,],cova)}
      f=forward[n,which(position[k,]!=0)+1]
      s0=spot[n,which(position[k,]!=0)+1]
	print(opt_position)
      print(f)
      print(s0)
      PNL[n]=PNL[n-1]-TC
	lastPNL = PNL[n]
    }else{
      s=spot[n,which(position[k,]==1)+1]
      PNL[n]=sum(as.matrix((f-s0)*(l/(s0*N))+(s-s0)/s0)*opt_position) + lastPNL
    }
  }
}
PNL = PNL + 1
plot(forward[,1],PNL,type="l",col="blue",xlab = "Date",ylab=" ")

