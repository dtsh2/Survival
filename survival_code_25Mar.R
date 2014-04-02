###############################################
rm(list=ls())
setwd("~/ali_peel_survival")
## set working directory
## if working from a folder, used the next line and paste in

setwd("~/Dropbox/ali_peel_survival") # set working directory

dat <- read.csv("eidolon_teeth_data.csv")
dat<-dat[1:14,1:8]
attach(dat)

#Control parameters for model maximization

control1<-nls.control(maxiter = 100000,
                      tol = 1e-05,
                      minFactor = 1/1024,
                      printEval = F,
                      warnOnly = T)

#
##

max(Ghana)

startingvalues<-list(a=max(Ghana),a2=0.2)
# constant
G1<-nls(Ghana~a*exp(-a2*Age),
        start=startingvalues,
        algorithm="port",
        trace=T,
        control=control1)
summary(G1)

#####################

# openbugs

library(R2OpenBUGS)
setwd("~/ali_peel_survival")
sink("model.txt")
cat("
    model{
    a~dunif(0,1000)
    a2~dnorm(0,10)
    
    precision<-1/ghana.var
    ghana.var<-ghana.sd*ghana.sd
    ghana.sd~dunif(0,100)
    
    for (i in 1:nobs){
    Ghana[i]~dnorm(mu[i],precision)
    mu[i]<-a*exp(-a2*Age[i])
    }
    }
    
    ",fill=T)
sink()
## 


dat <- read.csv("eidolon_teeth_data.csv")

win.data<-list(Ghana=dat$Ghana,Age=dat$Age,nobs=length(dat$Ghana))
inits<-function()
  list(a=rnorm(1,300),a2=rnorm(1,0),ghana.sd=runif(1,1,30))
params<-c("a","a2","ghana.var")
nc=3
ni=10000
nb=1000
nt=1
outGhanaE<-bugs(data=win.data,inits=inits,parameters.to.save=params,
          model.file="model.txt",n.thin=nt,n.chains=nc,n.burnin=nb,
          n.iter=ni,debug=T,DIC=T,working.directory=getwd())
plot(outGhanaE)
hist(outGhanaE$sims.list$a2)

######################################################
# mat
##
## nls2
st1 <- expand.grid(a = seq(100, 400, len = 10),
                   a1 = seq(0.1, 10, len = 10),
                   b1 = seq(0.1, 10, len = 10),
                   a2 = seq(0.1, 10, len = 10))

#startingvalues<-list(a=300,a1=2,b1=2,a2=0.2)
library(nls2)

M2<-nls2(Ghana~a*exp(-a2*Age)*exp((-a1/b1)*(1-exp(-b1*Age))),
         start=st1,
         algorithm="grid-search")#,
         #trace=T,
         #control=control1)
summary(M2)
##

####################
sink("modelMature.txt")
cat("
    model{
    a~dunif(0,1000)
    a1~dnorm(0,100)
    a2~dnorm(0,100)
    b1~dnorm(0,100)
       
    precision<-1/ghana.var
    ghana.var<-ghana.sd*ghana.sd
    ghana.sd~dunif(0,100)
    
    for (i in 1:nobs){
    Ghana[i]~dnorm(mu[i],precision)
    mu[i]<-a*exp(-a2*Age[i])*exp((-a1/b1)*(1-exp(-b1*Age[i])))
    }
    }
    
    ",fill=T)
sink()
## 


dat <- read.csv("eidolon_teeth_data.csv")

win.data<-list(Ghana=dat$Ghana,Age=dat$Age,nobs=length(dat$Ghana))
inits<-function()
  list(a=rnorm(1,300),a1=rnorm(1,5),a2=rnorm(1,0),b1=rnorm(1,7),ghana.sd=runif(1,0,30))
params<-c("a","a1","a2","b1","ghana.var")

nc=3
ni=10000
nb=1000
nt=1
outGhanaMat<-bugs(data=win.data,inits=inits,parameters.to.save=params,
          model.file="modelMature.txt",n.thin=nt,n.chains=nc,n.burnin=nb,
          n.iter=ni,debug=T,DIC=T,working.directory=getwd())

plot(outGhanaMat)
hist(outGhanaMat$sims.list$a2)
outGhanaMat

################################################################
## nls2
st1 <- expand.grid(a = seq(100, 400, len = 10),
                   a2 = seq(0.1, 10, len = 10),
                   a3 = seq(-0.01,- 10, len = 10),
                   b3 = seq(-0.01,- 10, len = 10))

#startingvalues<-list(a=300,a1=2,b1=2,a2=0.2)

M3<-nls2(Ghana~a*exp(-a2*Age)*exp((-a3/b3)*(1-exp(b3*Age))),
         start=st1,
         algorithm="grid-search")#,
#trace=T,
#control=control1)
summary(M3)
# sen
sink("modelSen.txt")
cat("
    model{
    a~dunif(0,1000)
    a3~dnorm(0,100)
    a2~dnorm(0,100)
    b3~dnorm(0,100)
    
    precision<-1/ghana.var
    ghana.var<-ghana.sd*ghana.sd
    ghana.sd~dunif(0,100)
    
    for (i in 1:nobs){
    Ghana[i]~dnorm(mu[i],precision)
    mu[i]<-a*exp(-a2*Age[i])*exp((-a3/b3)*(1-exp(b3*Age[i])))
    }
    }
    
    ",fill=T)
sink()
## 


dat <- read.csv("eidolon_teeth_data.csv")

win.data<-list(Ghana=dat$Ghana,Age=dat$Age,nobs=length(dat$Ghana))
inits<-function()
  list(a=rnorm(1,300),a3=rnorm(1,-5.5),a2=rnorm(1,0),b3=rnorm(1,-7.7),ghana.sd=runif(1,0,30))
params<-c("a","a3","a2","b3","ghana.var")

nc=3
ni=10000
nb=1000
nt=10
outGhanaSen<-bugs(data=win.data,inits=inits,parameters.to.save=params,
          model.file="modelSen.txt",n.thin=nt,n.chains=nc,n.burnin=nb,
          n.iter=ni,debug=T,DIC=T,working.directory=getwd())

plot(outGhanaSen)
outGhanaSen
par(mfrow=c(2,2))

###############################################################
## both
## nls2
st1 <- expand.grid(a = seq(100, 400, len = 5),
                   a1 = seq(0.1, 10, len = 5),
                   a2 = seq(0.1, 10, len = 5),
                   a3 = seq(-0.01,- 10, len = 5),
                   b1 = seq(-0.01,- 10, len = 5),
                   b3 = seq(-0.01,- 10, len = 5))

#startingvalues<-list(a=300,a1=2,b1=2,a2=0.2)

M4<-nls2(Ghana~a*exp(-a2*Age)*exp((-a3/b3)*(1-exp(b3*Age)))*exp((-a1/b1)*(1-exp(-b1*Age))),
         start=st1,
         algorithm="grid-search")#,
#trace=T,
#control=control1)
summary(M4)

sink("modelSiler.txt")
cat("
    model{
    a~dunif(0,1000)
    a1~dnorm(0,100)
    a2~dnorm(0,100)
    a3~dnorm(0,100)
    b1~dnorm(0,100)
    b3~dnorm(0,100)
    
    precision<-1/ghana.var
    ghana.var<-ghana.sd*ghana.sd
    ghana.sd~dunif(0,400)
    
    for (i in 1:nobs){
    Ghana[i]~dnorm(mu[i],precision)
    mu[i]<-a*exp(-a2*Age[i])*exp((-a3/b3)*(1-exp(b3*Age[i])))*exp((-a1/b1)*(1-exp(-b1*Age[i])))
    }
    }
    
    ",fill=T)
sink()
## 


dat <- read.csv("eidolon_teeth_data.csv")

win.data<-list(Ghana=dat$Ghana,Age=dat$Age,nobs=length(dat$Ghana))
inits<-function()
  list(a=rnorm(1,250),a1=rnorm(1,0.1),a2=rnorm(1,0.05),a3=rnorm(1,-6),b1=rnorm(1,-0.07),b3=rnorm(1,-8),ghana.sd=runif(1,1,30))
params<-c("a","a1","a2","a3","b1","b3","ghana.var")
nc=3
ni=10000
nb=1000
nt=10
outGhanaSiler<-bugs(data=win.data,inits=inits,parameters.to.save=params,
          model.file="modelSiler.txt",n.thin=nt,n.chains=nc,n.burnin=nb,
          n.iter=ni,debug=T,DIC=T,working.directory=getwd())

#############
## ploting
######################################
outGhanaE$DIC
a=outGhanaE$mean$a
a2=outGhanaE$mean$a2

plot(dat$Age,a*exp(-a2*dat$Age),ylab="",xlab="Age",type="l",ylim=c(0,300))
## plot CI
predictions<-array(dim=c(length(dat$Age),length(outGhanaE$sims.list$a)))
for (i in 1:length(dat$Age)){
  predictions[i,]<-outGhanaE$sims.list$a*exp(-outGhanaE$sims.list$a2*dat$Age[i])
}
LPB<-apply(predictions,1,quantile,probs=0.025)
UPB<-apply(predictions,1,quantile,probs=0.975)
points(0:13,LPB,type="l",col="grey")
points(0:13,UPB,type="l",col="grey") # check 1 or 0 start
polygon(c(rev(dat$Age), dat$Age), c(rev(LPB),UPB), col =  "#00000010", border = NA)

outGhanaSen$DIC
a=outGhanaSen$mean$a
a2=outGhanaSen$mean$a2
b1=outGhanaSen$mean$b1
b3=outGhanaSen$mean$b3

lines(dat$Age,a*exp(-a2*dat$Age)*exp((-a3/b3)*(1-exp(b3*dat$Age))),lty=2)
## plot CI
predictions<-array(dim=c(length(dat$Age),length(outGhanaSen$sims.list$a)))
for (i in 1:length(dat$Age)){
  predictions[i,]<-outGhanaSen$sims.list$a*exp(-outGhanaSen$sims.list$a2*dat$Age[i])*exp((-outGhanaSen$sims.list$a3/outGhanaSen$sims.list$b3)*(1-exp(outGhanaSen$sims.list$b3*dat$Age[i])))
}
LPB<-apply(predictions,1,quantile,probs=0.025)
UPB<-apply(predictions,1,quantile,probs=0.975)
points(0:13,LPB,type="l",col="grey",lty=2)
points(0:13,UPB,type="l",col="grey",lty=2) # check 1 or 0 start
polygon(c(rev(dat$Age), dat$Age), c(rev(LPB),UPB), col =  "#00000010", border = NA)

outGhanaMat$DIC
a=outGhanaMat$mean$a
a1=outGhanaMat$mean$a1
a2=outGhanaMat$mean$a2
b1=outGhanaMat$mean$b1

lines(dat$Age,a*exp(-a2*dat$Age)*exp((-a1/b1)*(1-exp(-b1*dat$Age))),lty=3)
predictions<-array(dim=c(length(dat$Age),length(outGhanaMat$sims.list$a)))
for (i in 1:length(dat$Age)){
  predictions[i,]<-outGhanaMat$sims.list$a*exp(-outGhanaMat$sims.list$a2*dat$Age[i])*exp((-outGhanaMat$sims.list$a1/outGhanaMat$sims.list$b1)*(1-exp(-outGhanaMat$sims.list$b1*dat$Age[i])))
}
LPB<-apply(predictions,1,quantile,probs=0.025)
UPB<-apply(predictions,1,quantile,probs=0.975)
points(0:13,LPB,type="l",col="grey",lty=3)
points(0:13,UPB,type="l",col="grey",lty=3) # check 1 or 0 start
polygon(c(rev(dat$Age), dat$Age), c(rev(LPB),UPB), col =  "#00000010", border = NA)

outGhanaSiler$DIC

a=outGhanaSiler$mean$a
a1=outGhanaSiler$mean$a1
a2=outGhanaSiler$mean$a2
a3=outGhanaSiler$mean$a3
b1=outGhanaSiler$mean$b1
b3=outGhanaSiler$mean$b3

lines(dat$Age,a*exp(-a2*dat$Age)*exp((-a3/b3)*(1-exp(b3*dat$Age)))*exp((-a1/b1)*(1-exp(-b1*dat$Age))),lty=4)
predictions<-array(dim=c(length(dat$Age),length(outGhanaSiler$sims.list$a)))
for (i in 1:length(dat$Age)){
  predictions[i,]<-outGhanaSiler$sims.list$a*exp(-outGhanaSiler$sims.list$a2*dat$Age[i])*exp((-outGhanaSiler$sims.list$a3/outGhanaSiler$sims.list$b3)*(1-exp(outGhanaSiler$sims.list$b3*dat$Age[i])))*exp((-outGhanaSiler$sims.list$a1/outGhanaSiler$sims.list$b1)*(1-exp(-outGhanaSiler$sims.list$b1*dat$Age[i])))
}
LPB<-apply(predictions,1,quantile,probs=0.025)
UPB<-apply(predictions,1,quantile,probs=0.975)
points(0:13,LPB,type="l",col="grey",lty=4)
points(0:13,UPB,type="l",col="grey",lty=4) # check 1 or 0 start
polygon(c(rev(dat$Age), dat$Age), c(rev(LPB),UPB), col =  "#00000010", border = NA)

legend("topright",legend=c("Constant","Maturation","Senescence","Both"),
       lty=1:4,bty="n")

points(dat$Age,Ghana,bg="black",pch=21)

min=min(outGhanaE$DIC,outGhanaMat$DIC,outGhanaSen$DIC,outGhanaSiler$DIC)
barplot(c(outGhanaE$DIC-min,outGhanaMat$DIC-min,outGhanaSen$DIC-min,outGhanaSiler$DIC-min),
        names.arg=c("Constant","Maturation","Senescence","Both"))

par(mfrow=c(3,2))
hist(outGhanaSiler$sims.list$a, main="a",xlab="",col="grey")
abline(v=outGhanaSiler$mean$a,col="red")
hist(outGhanaSiler$sims.list$a1, main="a1",xlab="",col="grey")
abline(v=outGhanaSiler$mean$a1,col="red")
hist(outGhanaSiler$sims.list$a2, main="a2",xlab="",col="grey")
abline(v=outGhanaSiler$mean$a2,col="red")
hist(outGhanaSiler$sims.list$a3, main="a3",xlab="",col="grey")
abline(v=outGhanaSiler$mean$a3,col="red")
hist(outGhanaSiler$sims.list$b1, main="b1",xlab="",col="grey")
abline(v=outGhanaSiler$mean$b1,col="red")
hist(outGhanaSiler$sims.list$b3, main="b3",xlab="",col="grey")
abline(v=outGhanaSiler$mean$b3,col="red")

###########################################
# plotting posterior vs priors
plot(density(outGhanaSiler$sims.list$a), main="a",xlab="",xlim=c(0,1000))
polygon(density(outGhanaSiler$sims.list$a), col =  "#00000010", border = NA)
lines(density(runif(1:1000,min=0,max=1000)))
polygon(density(runif(1:1000,min=0,max=1000)), col =  "#00000010", border = NA)

plot(density(outGhanaSiler$sims.list$a1), main="a1",xlab="")
polygon(density(outGhanaSiler$sims.list$a1), col =  "#00000010", border = NA)
lines(density((rnorm(1:1000,mean=0,sd=10))))
polygon(density((rnorm(1:1000,mean=0,sd=10))), col =  "#00000010", border = NA)

plot(density(outGhanaSiler$sims.list$a2), main="a2",xlab="")
polygon(density(outGhanaSiler$sims.list$a2), col =  "#00000010", border = NA)
lines(density((rnorm(1:1000,mean=0,sd=10))))
polygon(density((rnorm(1:1000,mean=0,sd=10))), col =  "#00000010", border = NA)

plot(density(outGhanaSiler$sims.list$a3), main="a3",xlab="")
polygon(density(outGhanaSiler$sims.list$a3), col =  "#00000010", border = NA)
lines(density((rnorm(1:1000,mean=0,sd=10))))
polygon(density((rnorm(1:1000,mean=0,sd=10))), col =  "#00000010", border = NA)

plot(density(outGhanaSiler$sims.list$b1), main="b1",xlab="")
polygon(density(outGhanaSiler$sims.list$b1), col =  "#00000010", border = NA)
lines(density((rnorm(1:1000,mean=0,sd=10))))
polygon(density((rnorm(1:1000,mean=0,sd=10))), col =  "#00000010", border = NA)

plot(density(outGhanaSiler$sims.list$b3), main="b3",xlab="")
polygon(density(outGhanaSiler$sims.list$b3), col =  "#00000010", border = NA)
lines(density((rnorm(1:1000,mean=0,sd=10))))
polygon(density((rnorm(1:1000,mean=0,sd=10))), col =  "#00000010", border = NA)

########################

## calculate survival lx-1/lx
## ploting
######################################

a=outGhanaE$mean$a
a2=outGhanaE$mean$a2
SConst<-a*exp(-a2*dat$Age)

outGhanaSen$DIC
a=outGhanaSen$mean$a
a2=outGhanaSen$mean$a2
b1=outGhanaSen$mean$b1
b3=outGhanaSen$mean$b3
SSen<-a*exp(-a2*dat$Age)*exp((-a3/b3)*(1-exp(b3*dat$Age)))

outGhanaMat$DIC
a=outGhanaMat$mean$a
a1=outGhanaMat$mean$a1
a2=outGhanaMat$mean$a2
b1=outGhanaMat$mean$b1
SMat<-a*exp(-a2*dat$Age)*exp((-a1/b1)*(1-exp(-b1*dat$Age)))

a=outGhanaSiler$mean$a
a1=outGhanaSiler$mean$a1
a2=outGhanaSiler$mean$a2
a3=outGhanaSiler$mean$a3
b1=outGhanaSiler$mean$b1
b3=outGhanaSiler$mean$b3
SSiler<-a*exp(-a2*dat$Age)*exp((-a3/b3)*(1-exp(b3*dat$Age)))*exp((-a1/b1)*(1-exp(-b1*dat$Age)))
############################
pConst=(SConst[1:13+1])/(SConst[1:13])
pSen=(SSen[1:13+1])/(SSen[1:13])
pMat=(SMat[1:13+1])/(SMat[1:13])
pSiler=(SSiler[1:13+1])/(SSiler[1:13])
par(mfrow=c(1,1))
plot(0:12,pConst,ylim=c(0,1),type="l",xlab="Age",ylab="Survival")
lines(0:12,pSen,lty=2)
lines(0:12,pMat,lty=3)
lines(0:12,pSiler,lty=4)
legend("bottomright",legend=c("Constant","Maturation","Senescence","Both"),
       lty=1:4,bty="n")

##################
## try SaoTome

startingvalues<-list(a=max(SaoTome),a2=0.2)
# constant
G1<-nls(SaoTome~a*exp(-a2*Age),
        start=startingvalues,
        algorithm="port",
        trace=T,
        control=control1)
summary(G1)

#####################

# openbugs

library(R2OpenBUGS)
setwd("~/ali_peel_survival")
sink("allmodel.txt")
cat("
    model{
    a~dunif(0,1000)
    a2~dnorm(0,3)
    
    precision<-1/data.var
    data.var<-data.sd*data.sd
    data.sd~dunif(0,100)
    
    for (i in 1:nobs){
    data[i]~dnorm(mu[i],precision)
    mu[i]<-a*exp(-a2*Age[i])
    }
    }
    
    ",fill=T)
sink()
## 


dat <- read.csv("eidolon_teeth_data.csv")

win.data<-list(data=dat$SaoTome,Age=dat$Age,nobs=length(dat$SaoTome))
inits<-function()
  list(a=rnorm(1,30),a2=rnorm(1,0),data.sd=runif(1,1,30))
params<-c("a","a2","data.var")
nc=3
ni=10000
nb=1000
nt=1
outSaoTomeE<-bugs(data=win.data,inits=inits,parameters.to.save=params,
                model.file="allmodel.txt",n.thin=nt,n.chains=nc,n.burnin=nb,
                n.iter=ni,debug=T,DIC=T,working.directory=getwd())
plot(outSaoTomeE)
hist(outSaoTomeE$sims.list$a2)

a=24.6
a2=0.3
Age=dat$Age
plot(dat$Age,a*exp(-a2*Age),type="l")
points(dat$Age,dat$SaoTome)

######################################################
# mat
##
## nls2
st1 <- expand.grid(a = seq(1, 40, len = 10),
                   a1 = seq(0.1, 10, len = 10),
                   b1 = seq(0.1, 10, len = 10),
                   a2 = seq(0.1, 10, len = 10))

#startingvalues<-list(a=300,a1=2,b1=2,a2=0.2)
library(nls2)

M2<-nls2(SaoTome~a*exp(-a2*Age)*exp((-a1/b1)*(1-exp(-b1*Age))),
         start=st1,
         algorithm="grid-search")#,
#trace=T,
#control=control1)
summary(M2)
##
startingvalues<-list(a=max(SaoTome),a1=1.2,b1=1.2,a2=0.1)
G2<-nls(SaoTome~a*exp(-a2*Age)*exp((-a1/b1)*(1-exp(-b1*Age))),
        start=startingvalues,
        algorithm="port",
        trace=T,
        control=control1)
summary(G2)
####################
sink("allmodelMature.txt")
cat("
    model{
    a~dunif(0,1000)
    a1~dnorm(0,100)
    a2~dnorm(0,100)
    b1~dnorm(0,100)
    
    precision<-1/data.var
    data.var<-data.sd*data.sd
    data.sd~dunif(0,100)
    
    for (i in 1:nobs){
    data[i]~dnorm(mu[i],precision)
    mu[i]<-a*exp(-a2*Age[i])*exp((-a1/b1)*(1-exp(-b1*Age[i])))
    }
    }
    
    ",fill=T)
sink()
## 


dat <- read.csv("eidolon_teeth_data.csv")

win.data<-list(data=dat$SaoTome,Age=dat$Age,nobs=length(dat$SaoTome))
inits<-function()
  list(a=rnorm(1,30),a1=rnorm(1,5),a2=rnorm(1,0),b1=rnorm(1,7),data.sd=runif(1,0,30))
params<-c("a","a1","a2","b1","data.var")

nc=3
ni=10000
nb=1000
nt=1
outSaoTomeMat<-bugs(data=win.data,inits=inits,parameters.to.save=params,
                  model.file="allmodelMature.txt",n.thin=nt,n.chains=nc,n.burnin=nb,
                  n.iter=ni,debug=T,DIC=T,working.directory=getwd())

plot(outSaoTomeMat)
hist(outSaoTomeMat$sims.list$a2)
outSaoTomeMat
a=24.2
a1=0.1
b1=0.000001
a2=0.1
Age=dat$Age
plot(dat$Age,a*exp(-a2*Age)*exp((-a1/b1)*(1-exp(-b1*Age))),type="l")
points(dat$Age,dat$SaoTome)

################################################################
## nls2
st1 <- expand.grid(a = seq(1, 40, len = 10),
                   a2 = seq(0.1, 10, len = 10),
                   a3 = seq(-0.01,- 10, len = 10),
                   b3 = seq(-0.01,- 10, len = 10))

#startingvalues<-list(a=300,a1=2,b1=2,a2=0.2)

M3<-nls2(SaoTome~a*exp(-a2*Age)*exp((-a3/b3)*(1-exp(b3*Age))),
         start=st1,
         algorithm="grid-search")#,
#trace=T,
#control=control1)
summary(M3)
# sen
startingvalues<-list(a=max(SaoTome),a2=0.1,a3=-1.12,b3=-1.12)
G3<-nls(SaoTome~a*exp(-a2*Age)*exp((-a3/b3)*(1-exp(b3*Age))),
        start=startingvalues,
        algorithm="port",
        trace=T,
        control=control1)
summary(G3)

# sen

sink("allmodelSen.txt")
cat("
    model{
    a~dunif(0,1000)
    a3~dnorm(0,100)
    a2~dnorm(0,100)
    b3~dnorm(0,100)
    
    precision<-1/data.var
    data.var<-data.sd*data.sd
    data.sd~dunif(0,100)
    
    for (i in 1:nobs){
    data[i]~dnorm(mu[i],precision)
    mu[i]<-a*exp(-a2*Age[i])*exp((-a3/b3)*(1-exp(b3*Age[i])))
    }
    }
    
    ",fill=T)
sink()
## 


dat <- read.csv("eidolon_teeth_data.csv")

win.data<-list(data=dat$SaoTome,Age=dat$Age,nobs=length(dat$SaoTome))
inits<-function()
  list(a=rnorm(1,30),a3=rnorm(1,-6),a2=rnorm(1,0),b3=rnorm(1,-2),data.sd=runif(1,0,30))
params<-c("a","a3","a2","b3","data.var")

nc=3
ni=10000
nb=1000
nt=10
outSaoTomeSen<-bugs(data=win.data,inits=inits,parameters.to.save=params,
                  model.file="allmodelSen.txt",n.thin=nt,n.chains=nc,n.burnin=nb,
                  n.iter=ni,debug=T,DIC=T,working.directory=getwd())

plot(outSaoTomeSen)
outSaoTomeSen

###############################################################
## both
## nls2
st1 <- expand.grid(a = seq(1, 40, len = 5),
                   a1 = seq(0.1, 10, len = 5),
                   a2 = seq(0.1, 10, len = 5),
                   a3 = seq(-0.01,- 10, len = 5),
                   b1 = seq(-0.01,- 10, len = 5),
                   b3 = seq(-0.01,- 10, len = 5))

#startingvalues<-list(a=300,a1=2,b1=2,a2=0.2)

M4<-nls2(SaoTome~a*exp(-a2*Age)*exp((-a3/b3)*(1-exp(b3*Age)))*exp((-a1/b1)*(1-exp(-b1*Age))),
         start=st1,
         algorithm="grid-search")#,
#trace=T,
#control=control1)
summary(M4)

# both
startingvalues<-list(a=max(SaoTome),a1=0.1,b1=-0.01,a2=0.1,a3=-5,b3=-10)
G4<-nls(SaoTome~a*exp(-a2*Age)*exp((-a3/b3)*(1-exp(b3*Age)))*exp((-a1/b1)*(1-exp(-b1*Age))),
        start=startingvalues,
        algorithm="port",
        trace=T,
        control=control1)
summary(G4)


sink("allmodelSiler.txt")
cat("
    model{
    a~dunif(0,1000)
    a1~dnorm(0,100)
    a2~dnorm(0,100)
    a3~dnorm(0,100)
    b1~dnorm(0,100)
    b3~dnorm(0,100)
    
    precision<-1/data.var
    data.var<-data.sd*data.sd
    data.sd~dunif(0,400)
    
    for (i in 1:nobs){
    data[i]~dnorm(mu[i],precision)
    mu[i]<-a*exp(-a2*Age[i])*exp((-a3/b3)*(1-exp(b3*Age[i])))*exp((-a1/b1)*(1-exp(-b1*Age[i])))
    }
    }
    
    ",fill=T)
sink()
## 


dat <- read.csv("eidolon_teeth_data.csv")

win.data<-list(data=dat$SaoTome,Age=dat$Age,nobs=length(dat$SaoTome))
inits<-function()
  list(a=rnorm(1,30),a1=rnorm(1,0.1),a2=rnorm(1,0.1),a3=rnorm(1,-5),b1=rnorm(1,-0.01),b3=rnorm(1,-10),data.sd=runif(1,1,30))
params<-c("a","a1","a2","a3","b1","b3","data.var")
nc=3
ni=10000
nb=1000
nt=10
outSaoTomeSiler<-bugs(data=win.data,inits=inits,parameters.to.save=params,
                    model.file="allmodelSiler.txt",n.thin=nt,n.chains=nc,n.burnin=nb,
                    n.iter=ni,debug=T,DIC=T,working.directory=getwd())

#############

plot(Age,SaoTome,main="SaoTome",ylab="data")
lines(fitted(G1),lty=1)
lines(fitted(G2),lty=2)
lines(fitted(G3),lty=3)
lines(fitted(G4),lty=4)

##

legend("topright",legend=c("Constant","Maturation","Senescence","Both"),
       lty=1:4)

## AIC all
AIC(G1,G2,G3,G4)
res.aic<-AIC(G1,G2,G3,G4)

## model weights
aics<-res.aic[order(-res.aic$AIC),]
for(i in 1:dim(aics)[1]){
  aics$diff[i]<-aics$AIC[1]-aics$AIC[i]}
aics$wi<-2.71828182845904523536^(-0.5*aics$diff)
aics$aic.weights<-aics$wi/sum(aics$wi)
## 
aics
plot(aics$aic.weights,xaxt="n",xlab="model",ylab="AIC weights",pch=16,main="SaoTome")
mtext(c("Both","Constant","Maturation","Senescence"),side=1,at=1:4)

## NB can use barplot
barplot(aics$aic.weights,xaxt="n",xlab="model",ylab="AIC weights",width=0.8,main="SaoTome")
mtext(c("Both","Constant","Maturation","Senescence"),side=1,at=0.5:3.5)

######################################
outSaoTomeE$DIC
a=outSaoTomeE$mean$a
a2=outSaoTomeE$mean$a2

plot(0:13,a*exp(-a2*dat$Age),ylab="",xlab="Age",type="l",ylim=c(0,40))
## plot CI
predictions<-array(dim=c(length(dat$Age),length(outSaoTomeE$sims.list$a)))
for (i in 1:length(dat$Age)){
  predictions[i,]<-outSaoTomeE$sims.list$a*exp(-outSaoTomeE$sims.list$a2*dat$Age[i])
}
LPB<-apply(predictions,1,quantile,probs=0.025)
UPB<-apply(predictions,1,quantile,probs=0.975)
points(0:13,LPB,type="l",col="grey")
points(0:13,UPB,type="l",col="grey") # check 1 or 0 start
polygon(c(rev(dat$Age), dat$Age), c(rev(LPB),UPB), col =  "#00000010", border = NA)

outSaoTomeSen$DIC
a=outSaoTomeSen$mean$a
a2=outSaoTomeSen$mean$a2
b1=outSaoTomeSen$mean$b1
b3=outSaoTomeSen$mean$b3

lines(0:13,a*exp(-a2*dat$Age)*exp((-a3/b3)*(1-exp(b3*dat$Age))),lty=2)
## plot CI
predictions<-array(dim=c(length(dat$Age),length(outSaoTomeSen$sims.list$a)))
for (i in 1:length(dat$Age)){
  predictions[i,]<-outSaoTomeSen$sims.list$a*exp(-outSaoTomeSen$sims.list$a2*dat$Age[i])*exp((-outSaoTomeSen$sims.list$a3/outSaoTomeSen$sims.list$b3)*(1-exp(outSaoTomeSen$sims.list$b3*dat$Age[i])))
}
LPB<-apply(predictions,1,quantile,probs=0.025)
UPB<-apply(predictions,1,quantile,probs=0.975)
points(0:13,LPB,type="l",col="grey",lty=2)
points(0:13,UPB,type="l",col="grey",lty=2) # check 1 or 0 start
polygon(c(rev(dat$Age), dat$Age), c(rev(LPB),UPB), col =  "#00000010", border = NA)

outSaoTomeMat$DIC
a=outSaoTomeMat$mean$a
a1=outSaoTomeMat$mean$a1
a2=outSaoTomeMat$mean$a2
b1=outSaoTomeMat$mean$b1

lines(0:13,a*exp(-a2*dat$Age)*exp((-a1/b1)*(1-exp(-b1*dat$Age))),lty=3)
predictions<-array(dim=c(length(dat$Age),length(outSaoTomeMat$sims.list$a)))
for (i in 1:length(dat$Age)){
  predictions[i,]<-outSaoTomeMat$sims.list$a*exp(-outSaoTomeMat$sims.list$a2*dat$Age[i])*exp((-outSaoTomeMat$sims.list$a1/outSaoTomeMat$sims.list$b1)*(1-exp(-outSaoTomeMat$sims.list$b1*dat$Age[i])))
}
LPB<-apply(predictions,1,quantile,probs=0.025)
UPB<-apply(predictions,1,quantile,probs=0.975)
points(0:13,LPB,type="l",col="grey",lty=3)
points(0:13,UPB,type="l",col="grey",lty=3) # check 1 or 0 start
polygon(c(rev(dat$Age), dat$Age), c(rev(LPB),UPB), col =  "#00000010", border = NA)

outSaoTomeSiler$DIC

a=outSaoTomeSiler$mean$a
a1=outSaoTomeSiler$mean$a1
a2=outSaoTomeSiler$mean$a2
a3=outSaoTomeSiler$mean$a3
b1=outSaoTomeSiler$mean$b1
b3=outSaoTomeSiler$mean$b3

lines(0:13,a*exp(-a2*dat$Age)*exp((-a3/b3)*(1-exp(b3*dat$Age)))*exp((-a1/b1)*(1-exp(-b1*dat$Age))),lty=4)
predictions<-array(dim=c(length(dat$Age),length(outSaoTomeSiler$sims.list$a)))
for (i in 1:length(dat$Age)){
  predictions[i,]<-outSaoTomeSiler$sims.list$a*exp(-outSaoTomeSiler$sims.list$a2*dat$Age[i])*exp((-outSaoTomeSiler$sims.list$a3/outSaoTomeSiler$sims.list$b3)*(1-exp(outSaoTomeSiler$sims.list$b3*dat$Age[i])))*exp((-outSaoTomeSiler$sims.list$a1/outSaoTomeSiler$sims.list$b1)*(1-exp(-outSaoTomeSiler$sims.list$b1*dat$Age[i])))
}
LPB<-apply(predictions,1,quantile,probs=0.025)
UPB<-apply(predictions,1,quantile,probs=0.975)
points(0:13,LPB,type="l",col="grey",lty=4)
points(0:13,UPB,type="l",col="grey",lty=4) # check 1 or 0 start
polygon(c(rev(dat$Age), dat$Age), c(rev(LPB),UPB), col =  "#00000010", border = NA)

legend("topright",legend=c("Constant","Maturation","Senescence","Both"),
       lty=1:4,bty="n")

points(dat$Age,SaoTome,bg="black",pch=21)

min=min(outSaoTomeE$DIC,outSaoTomeMat$DIC,outSaoTomeSen$DIC,outSaoTomeSiler$DIC)
barplot(c(outSaoTomeE$DIC-min,outSaoTomeMat$DIC-min,outSaoTomeSen$DIC-min,outSaoTomeSiler$DIC-min),
        names.arg=c("Constant","Maturation","Senescence","Both"))

par(mfrow=c(3,2))
hist(outSaoTomeSiler$sims.list$a, main="a",xlab="",col="grey")
abline(v=outSaoTomeSiler$mean$a,col="red")
hist(outSaoTomeSiler$sims.list$a1, main="a1",xlab="",col="grey")
abline(v=outSaoTomeSiler$mean$a1,col="red")
hist(outSaoTomeSiler$sims.list$a2, main="a2",xlab="",col="grey")
abline(v=outSaoTomeSiler$mean$a2,col="red")
hist(outSaoTomeSiler$sims.list$a3, main="a3",xlab="",col="grey")
abline(v=outSaoTomeSiler$mean$a3,col="red")
hist(outSaoTomeSiler$sims.list$b1, main="b1",xlab="",col="grey")
abline(v=outSaoTomeSiler$mean$b1,col="red")
hist(outSaoTomeSiler$sims.list$b3, main="b3",xlab="",col="grey")
abline(v=outSaoTomeSiler$mean$b3,col="red")
###############################################################################
## calculate survival lx-1/lx
## ploting
######################################

a=outSaoTomeE$mean$a
a2=outSaoTomeE$mean$a2
SConst<-a*exp(-a2*dat$Age)

outSaoTomeSen$DIC
a=outSaoTomeSen$mean$a
a2=outSaoTomeSen$mean$a2
b1=outSaoTomeSen$mean$b1
b3=outSaoTomeSen$mean$b3
SSen<-a*exp(-a2*dat$Age)*exp((-a3/b3)*(1-exp(b3*dat$Age)))

outSaoTomeMat$DIC
a=outSaoTomeMat$mean$a
a1=outSaoTomeMat$mean$a1
a2=outSaoTomeMat$mean$a2
b1=outSaoTomeMat$mean$b1
SMat<-a*exp(-a2*dat$Age)*exp((-a1/b1)*(1-exp(-b1*dat$Age)))

a=outSaoTomeSiler$mean$a
a1=outSaoTomeSiler$mean$a1
a2=outSaoTomeSiler$mean$a2
a3=outSaoTomeSiler$mean$a3
b1=outSaoTomeSiler$mean$b1
b3=outSaoTomeSiler$mean$b3
SSiler<-a*exp(-a2*dat$Age)*exp((-a3/b3)*(1-exp(b3*dat$Age)))*exp((-a1/b1)*(1-exp(-b1*dat$Age)))
############################
pConst=(SConst[1:13+1])/(SConst[1:13])
pSen=(SSen[1:13+1])/(SSen[1:13])
pMat=(SMat[1:13+1])/(SMat[1:13])
pSiler=(SSiler[1:13+1])/(SSiler[1:13])
plot(0:12,pConst,ylim=c(0,1),type="l",xlab="Age",ylab="Survival")
lines(0:12,pSen,lty=2)
lines(0:12,pMat,lty=3)
lines(0:12,pSiler,lty=4)
######################################
## Principe

startingvalues<-list(a=max(Principe),a2=0.2)
# constant
G1<-nls(Principe~a*exp(-a2*Age),
        start=startingvalues,
        algorithm="port",
        trace=T,
        control=control1)
summary(G1)

#####################

# openbugs

library(R2OpenBUGS)
setwd("~/ali_peel_survival")
sink("allmodel.txt")
cat("
    model{
    a~dunif(0,1000)
    a2~dnorm(0,3)
    
    precision<-1/data.var
    data.var<-data.sd*data.sd
    data.sd~dunif(0,100)
    
    for (i in 1:nobs){
    data[i]~dnorm(mu[i],precision)
    mu[i]<-a*exp(-a2*Age[i])
    }
    }
    
    ",fill=T)
sink()
## 


dat <- read.csv("eidolon_teeth_data.csv")

win.data<-list(data=dat$Principe,Age=dat$Age,nobs=length(dat$Principe))
inits<-function()
  list(a=rnorm(1,30),a2=rnorm(1,0),data.sd=runif(1,1,30))
params<-c("a","a2","data.var")
nc=3
ni=10000
nb=1000
nt=1
outPrincipeE<-bugs(data=win.data,inits=inits,parameters.to.save=params,
                  model.file="allmodel.txt",n.thin=nt,n.chains=nc,n.burnin=nb,
                  n.iter=ni,debug=T,DIC=T,working.directory=getwd())
plot(outPrincipeE)
hist(outPrincipeE$sims.list$a2)

######################################################
# mat
##
## nls2
st1 <- expand.grid(a = seq(1, 20, len = 10),
                   a1 = seq(0.1, 10, len = 10),
                   b1 = seq(0.1, 10, len = 10),
                   a2 = seq(0.1, 10, len = 10))

#startingvalues<-list(a=300,a1=2,b1=2,a2=0.2)
library(nls2)

M2<-nls2(Principe~a*exp(-a2*Age)*exp((-a1/b1)*(1-exp(-b1*Age))),
         start=st1,
         algorithm="grid-search")#,
#trace=T,
#control=control1)
summary(M2)
##
startingvalues<-list(a=max(Principe),a1=0.1,b1=0.1,a2=0.1)
G2<-nls(Principe~a*exp(-a2*Age)*exp((-a1/b1)*(1-exp(-b1*Age))),
        start=startingvalues,
        algorithm="port",
        trace=T,
        control=control1)
summary(G2)
####################
sink("allmodelMature.txt")
cat("
    model{
    a~dunif(0,1000)
    a1~dnorm(0,100)
    a2~dnorm(0,100)
    b1~dnorm(0,100)
    
    precision<-1/data.var
    data.var<-data.sd*data.sd
    data.sd~dunif(0,100)
    
    for (i in 1:nobs){
    data[i]~dnorm(mu[i],precision)
    mu[i]<-a*exp(-a2*Age[i])*exp((-a1/b1)*(1-exp(-b1*Age[i])))
    }
    }
    
    ",fill=T)
sink()
## 


dat <- read.csv("eidolon_teeth_data.csv")

win.data<-list(data=dat$Principe,Age=dat$Age,nobs=length(dat$Principe))
inits<-function()
  list(a=rnorm(1,20),a1=rnorm(1,0.1),a2=rnorm(1,0.1),b1=rnorm(1,0.1),data.sd=runif(1,0,30))
params<-c("a","a1","a2","b1","data.var")

nc=3
ni=10000
nb=1000
nt=1
outPrincipeMat<-bugs(data=win.data,inits=inits,parameters.to.save=params,
                    model.file="allmodelMature.txt",n.thin=nt,n.chains=nc,n.burnin=nb,
                    n.iter=ni,debug=T,DIC=T,working.directory=getwd())

plot(outPrincipeMat)
hist(outPrincipeMat$sims.list$a2)
outPrincipeMat

################################################################
## nls2
st1 <- expand.grid(a = seq(1, 30, len = 10),
                   a2 = seq(0.1, 10, len = 10),
                   a3 = seq(-0.01,- 10, len = 10),
                   b3 = seq(-0.01,- 10, len = 10))

#startingvalues<-list(a=300,a1=2,b1=2,a2=0.2)

M3<-nls2(Principe~a*exp(-a2*Age)*exp((-a3/b3)*(1-exp(b3*Age))),
         start=st1,
         algorithm="grid-search")#,
#trace=T,
#control=control1)
summary(M3)
# sen
startingvalues<-list(a=max(Principe),a2=0.1,a3=-0.1,b3=-0.1)
G3<-nls(Principe~a*exp(-a2*Age)*exp((-a3/b3)*(1-exp(b3*Age))),
        start=startingvalues,
        algorithm="port",
        trace=T,
        control=control1)
summary(G3)

# sen

sink("allmodelSen.txt")
cat("
    model{
    a~dunif(0,1000)
    a3~dnorm(0,100)
    a2~dnorm(0,100)
    b3~dnorm(0,100)
    
    precision<-1/data.var
    data.var<-data.sd*data.sd
    data.sd~dunif(0,100)
    
    for (i in 1:nobs){
    data[i]~dnorm(mu[i],precision)
    mu[i]<-a*exp(-a2*Age[i])*exp((-a3/b3)*(1-exp(b3*Age[i])))
    }
    }
    
    ",fill=T)
sink()
## 


dat <- read.csv("eidolon_teeth_data.csv")

win.data<-list(data=dat$Principe,Age=dat$Age,nobs=length(dat$Principe))
inits<-function()
  list(a=rnorm(1,11),a3=rnorm(1,-0.01),a2=rnorm(1,0.1),b3=rnorm(1,-0.01),data.sd=runif(1,0,30))
params<-c("a","a3","a2","b3","data.var")

nc=3
ni=10000
nb=1000
nt=10
outPrincipeSen<-bugs(data=win.data,inits=inits,parameters.to.save=params,
                    model.file="allmodelSen.txt",n.thin=nt,n.chains=nc,n.burnin=nb,
                    n.iter=ni,debug=T,DIC=T,working.directory=getwd())

plot(outPrincipeSen)
outPrincipeSen

###############################################################
## both
## nls2
st1 <- expand.grid(a = seq(1, 40, len = 5),
                   a1 = seq(0.1, 10, len = 5),
                   a2 = seq(0.1, 10, len = 5),
                   a3 = seq(-0.01,- 10, len = 5),
                   b1 = seq(-0.01,- 10, len = 5),
                   b3 = seq(-0.01,- 10, len = 5))


#startingvalues<-list(a=300,a1=2,b1=2,a2=0.2)

M4<-nls2(Principe~a*exp(-a2*Age)*exp((-a3/b3)*(1-exp(b3*Age)))*exp((-a1/b1)*(1-exp(-b1*Age))),
         start=st1,
         algorithm="grid-search")#,
#trace=T,
#control=control1)
summary(M4)

# both
startingvalues<-list(a=max(Principe),a1=0.1,b1=-0.001,a2=0.1,a3=-0.001,b3=-10)
G4<-nls(Principe~a*exp(-a2*Age)*exp((-a3/b3)*(1-exp(b3*Age)))*exp((-a1/b1)*(1-exp(-b1*Age))),
        start=startingvalues,
        algorithm="port",
        trace=T,
        control=control1)
summary(G4)


sink("allmodelSiler.txt")
cat("
    model{
    a~dunif(0,1000)
    a1~dnorm(0,100)
    a2~dnorm(0,100)
    a3~dnorm(0,100)
    b1~dnorm(0,100)
    b3~dnorm(0,100)
    
    precision<-1/data.var
    data.var<-data.sd*data.sd
    data.sd~dunif(0,400)
    
    for (i in 1:nobs){
    data[i]~dnorm(mu[i],precision)
    mu[i]<-a*exp(-a2*Age[i])*exp((-a3/b3)*(1-exp(b3*Age[i])))*exp((-a1/b1)*(1-exp(-b1*Age[i])))
    }
    }
    
    ",fill=T)
sink()
## 


dat <- read.csv("eidolon_teeth_data.csv")

win.data<-list(data=dat$Principe,Age=dat$Age,nobs=length(dat$Principe))
inits<-function()
  list(a=rnorm(1,15),a1=rnorm(1,0.1),a2=rnorm(1,0.1),a3=rnorm(1,-0.01),b1=rnorm(1,-0.01),b3=rnorm(1,-10),data.sd=runif(1,1,30))
params<-c("a","a1","a2","a3","b1","b3","data.var")
nc=3
ni=10000
nb=1000
nt=10
outPrincipeSiler<-bugs(data=win.data,inits=inits,parameters.to.save=params,
                      model.file="allmodelSiler.txt",n.thin=nt,n.chains=nc,n.burnin=nb,
                      n.iter=ni,debug=T,DIC=T,working.directory=getwd())

#############

plot(Age,Principe,main="Principe",ylab="data")
lines(fitted(G1),lty=1)
lines(fitted(G2),lty=2)
lines(fitted(G3),lty=3)
lines(fitted(G4),lty=4)

##

legend("topright",legend=c("Constant","Maturation","Senescence","Both"),
       lty=1:4)

## AIC all
AIC(G1,G2,G3,G4)
res.aic<-AIC(G1,G2,G3,G4)

## model weights
aics<-res.aic[order(-res.aic$AIC),]
for(i in 1:dim(aics)[1]){
  aics$diff[i]<-aics$AIC[1]-aics$AIC[i]}
aics$wi<-2.71828182845904523536^(-0.5*aics$diff)
aics$aic.weights<-aics$wi/sum(aics$wi)
## 
aics
plot(aics$aic.weights,xaxt="n",xlab="model",ylab="AIC weights",pch=16,main="Principe")
mtext(c("Both","Constant","Maturation","Senescence"),side=1,at=1:4)

## NB can use barplot
barplot(aics$aic.weights,xaxt="n",xlab="model",ylab="AIC weights",width=0.8,main="Principe")
mtext(c("Both","Constant","Maturation","Senescence"),side=1,at=0.5:3.5)

######################################
outPrincipeE$DIC
a=outPrincipeE$mean$a
a2=outPrincipeE$mean$a2

plot(0:13,a*exp(-a2*dat$Age),ylab="",xlab="Age",type="l",ylim=c(0,20))
## plot CI
predictions<-array(dim=c(length(dat$Age),length(outPrincipeE$sims.list$a)))
for (i in 1:length(dat$Age)){
  predictions[i,]<-outPrincipeE$sims.list$a*exp(-outPrincipeE$sims.list$a2*dat$Age[i])
}
LPB<-apply(predictions,1,quantile,probs=0.025)
UPB<-apply(predictions,1,quantile,probs=0.975)
points(0:13,LPB,type="l",col="grey")
points(0:13,UPB,type="l",col="grey") # check 1 or 0 start
polygon(c(rev(dat$Age), dat$Age), c(rev(LPB),UPB), col =  "#00000010", border = NA)

outPrincipeSen$DIC
a=outPrincipeSen$mean$a
a2=outPrincipeSen$mean$a2
b1=outPrincipeSen$mean$b1
b3=outPrincipeSen$mean$b3

lines(0:13,a*exp(-a2*dat$Age)*exp((-a3/b3)*(1-exp(b3*dat$Age))),lty=2)
## plot CI
predictions<-array(dim=c(length(dat$Age),length(outPrincipeSen$sims.list$a)))
for (i in 1:length(dat$Age)){
  predictions[i,]<-outPrincipeSen$sims.list$a*exp(-outPrincipeSen$sims.list$a2*dat$Age[i])*exp((-outPrincipeSen$sims.list$a3/outPrincipeSen$sims.list$b3)*(1-exp(outPrincipeSen$sims.list$b3*dat$Age[i])))
}
LPB<-apply(predictions,1,quantile,probs=0.025)
UPB<-apply(predictions,1,quantile,probs=0.975)
points(0:13,LPB,type="l",col="grey",lty=2)
points(0:13,UPB,type="l",col="grey",lty=2) # check 1 or 0 start
polygon(c(rev(dat$Age), dat$Age), c(rev(LPB),UPB), col =  "#00000010", border = NA)

outPrincipeMat$DIC
a=outPrincipeMat$mean$a
a1=outPrincipeMat$mean$a1
a2=outPrincipeMat$mean$a2
b1=outPrincipeMat$mean$b1

lines(0:13,a*exp(-a2*dat$Age)*exp((-a1/b1)*(1-exp(-b1*dat$Age))),lty=3)
predictions<-array(dim=c(length(dat$Age),length(outPrincipeMat$sims.list$a)))
for (i in 1:length(dat$Age)){
  predictions[i,]<-outPrincipeMat$sims.list$a*exp(-outPrincipeMat$sims.list$a2*dat$Age[i])*exp((-outPrincipeMat$sims.list$a1/outPrincipeMat$sims.list$b1)*(1-exp(-outPrincipeMat$sims.list$b1*dat$Age[i])))
}
LPB<-apply(predictions,1,quantile,probs=0.025)
UPB<-apply(predictions,1,quantile,probs=0.975)
points(0:13,LPB,type="l",col="grey",lty=3)
points(0:13,UPB,type="l",col="grey",lty=3) # check 1 or 0 start
polygon(c(rev(dat$Age), dat$Age), c(rev(LPB),UPB), col =  "#00000010", border = NA)

outPrincipeSiler$DIC

a=outPrincipeSiler$mean$a
a1=outPrincipeSiler$mean$a1
a2=outPrincipeSiler$mean$a2
a3=outPrincipeSiler$mean$a3
b1=outPrincipeSiler$mean$b1
b3=outPrincipeSiler$mean$b3

lines(0:13,a*exp(-a2*dat$Age)*exp((-a3/b3)*(1-exp(b3*dat$Age)))*exp((-a1/b1)*(1-exp(-b1*dat$Age))),lty=4)
predictions<-array(dim=c(length(dat$Age),length(outPrincipeSiler$sims.list$a)))
for (i in 1:length(dat$Age)){
  predictions[i,]<-outPrincipeSiler$sims.list$a*exp(-outPrincipeSiler$sims.list$a2*dat$Age[i])*exp((-outPrincipeSiler$sims.list$a3/outPrincipeSiler$sims.list$b3)*(1-exp(outPrincipeSiler$sims.list$b3*dat$Age[i])))*exp((-outPrincipeSiler$sims.list$a1/outPrincipeSiler$sims.list$b1)*(1-exp(-outPrincipeSiler$sims.list$b1*dat$Age[i])))
}
LPB<-apply(predictions,1,quantile,probs=0.025)
UPB<-apply(predictions,1,quantile,probs=0.975)
points(0:13,LPB,type="l",col="grey",lty=4)
points(0:13,UPB,type="l",col="grey",lty=4) # check 1 or 0 start
polygon(c(rev(dat$Age), dat$Age), c(rev(LPB),UPB), col =  "#00000010", border = NA)

legend("topright",legend=c("Constant","Maturation","Senescence","Both"),
       lty=1:4,bty="n")

points(dat$Age,Principe,bg="black",pch=21)

min=min(outPrincipeE$DIC,outPrincipeMat$DIC,outPrincipeSen$DIC,outPrincipeSiler$DIC)
barplot(c(outPrincipeE$DIC-min,outPrincipeMat$DIC-min,outPrincipeSen$DIC-min,outPrincipeSiler$DIC-min),
        names.arg=c("Constant","Maturation","Senescence","Both"))

par(mfrow=c(3,2))
hist(outPrincipeSiler$sims.list$a, main="a",xlab="",col="grey")
abline(v=outPrincipeSiler$mean$a,col="red")
hist(outPrincipeSiler$sims.list$a1, main="a1",xlab="",col="grey")
abline(v=outPrincipeSiler$mean$a1,col="red")
hist(outPrincipeSiler$sims.list$a2, main="a2",xlab="",col="grey")
abline(v=outPrincipeSiler$mean$a2,col="red")
hist(outPrincipeSiler$sims.list$a3, main="a3",xlab="",col="grey")
abline(v=outPrincipeSiler$mean$a3,col="red")
hist(outPrincipeSiler$sims.list$b1, main="b1",xlab="",col="grey")
abline(v=outPrincipeSiler$mean$b1,col="red")
hist(outPrincipeSiler$sims.list$b3, main="b3",xlab="",col="grey")
abline(v=outPrincipeSiler$mean$b3,col="red")
###############################################################################
## calculate survival lx-1/lx
## ploting
######################################

a=outPrincipeE$mean$a
a2=outPrincipeE$mean$a2
SConst<-a*exp(-a2*dat$Age)

outPrincipeSen$DIC
a=outPrincipeSen$mean$a
a2=outPrincipeSen$mean$a2
b1=outPrincipeSen$mean$b1
b3=outPrincipeSen$mean$b3
SSen<-a*exp(-a2*dat$Age)*exp((-a3/b3)*(1-exp(b3*dat$Age)))

outPrincipeMat$DIC
a=outPrincipeMat$mean$a
a1=outPrincipeMat$mean$a1
a2=outPrincipeMat$mean$a2
b1=outPrincipeMat$mean$b1
SMat<-a*exp(-a2*dat$Age)*exp((-a1/b1)*(1-exp(-b1*dat$Age)))

a=outPrincipeSiler$mean$a
a1=outPrincipeSiler$mean$a1
a2=outPrincipeSiler$mean$a2
a3=outPrincipeSiler$mean$a3
b1=outPrincipeSiler$mean$b1
b3=outPrincipeSiler$mean$b3
SSiler<-a*exp(-a2*dat$Age)*exp((-a3/b3)*(1-exp(b3*dat$Age)))*exp((-a1/b1)*(1-exp(-b1*dat$Age)))
############################
pConst=(SConst[1:13+1])/(SConst[1:13])
pSen=(SSen[1:13+1])/(SSen[1:13])
pMat=(SMat[1:13+1])/(SMat[1:13])
pSiler=(SSiler[1:13+1])/(SSiler[1:13])
plot(0:12,pConst,ylim=c(0,1),type="l",xlab="Age",ylab="Survival")
lines(0:12,pSen,lty=2)
lines(0:12,pMat,lty=3)
lines(0:12,pSiler,lty=4)
######################################
#########################################################################################################################################

## Morogoro

startingvalues<-list(a=max(Morogoro),a2=0.2)
# constant
G1<-nls(Morogoro~a*exp(-a2*Age),
        start=startingvalues,
        algorithm="port",
        trace=T,
        control=control1)
summary(G1)

#####################

# openbugs

library(R2OpenBUGS)
setwd("~/ali_peel_survival")
sink("allmodel.txt")
cat("
    model{
    a~dunif(0,1000)
    a2~dnorm(0,3)
    
    precision<-1/data.var
    data.var<-data.sd*data.sd
    data.sd~dunif(0,100)
    
    for (i in 1:nobs){
    data[i]~dnorm(mu[i],precision)
    mu[i]<-a*exp(-a2*Age[i])
    }
    }
    
    ",fill=T)
sink()
## 


dat <- read.csv("eidolon_teeth_data.csv")

win.data<-list(data=dat$Morogoro,Age=dat$Age,nobs=length(dat$Morogoro))
inits<-function()
  list(a=rnorm(1,30),a2=rnorm(1,0),data.sd=runif(1,1,30))
params<-c("a","a2","data.var")
nc=3
ni=10000
nb=1000
nt=1
outMorogoroE<-bugs(data=win.data,inits=inits,parameters.to.save=params,
                   model.file="allmodel.txt",n.thin=nt,n.chains=nc,n.burnin=nb,
                   n.iter=ni,debug=T,DIC=T,working.directory=getwd())
plot(outMorogoroE)
hist(outMorogoroE$sims.list$a2)

######################################################
# mat
##
## nls2
st1 <- expand.grid(a = seq(1, 40, len = 10),
                   a1 = seq(0.1, 10, len = 10),
                   b1 = seq(0.1, 10, len = 10),
                   a2 = seq(0.1, 10, len = 10))

#startingvalues<-list(a=300,a1=2,b1=2,a2=0.2)
library(nls2)

M2<-nls2(Morogoro~a*exp(-a2*Age)*exp((-a1/b1)*(1-exp(-b1*Age))),
         start=st1,
         algorithm="grid-search")#,
#trace=T,
#control=control1)
summary(M2)
##
startingvalues<-list(a=max(Morogoro),a1=0.1,b1=0.1,a2=0.1)
G2<-nls(Morogoro~a*exp(-a2*Age)*exp((-a1/b1)*(1-exp(-b1*Age))),
        start=startingvalues,
        algorithm="port",
        trace=T,
        control=control1)
summary(G2)
####################
sink("allmodelMature.txt")
cat("
    model{
    a~dunif(0,1000)
    a1~dnorm(0,100)
    a2~dnorm(0,100)
    b1~dnorm(0,100)
    
    precision<-1/data.var
    data.var<-data.sd*data.sd
    data.sd~dunif(0,100)
    
    for (i in 1:nobs){
    data[i]~dnorm(mu[i],precision)
    mu[i]<-a*exp(-a2*Age[i])*exp((-a1/b1)*(1-exp(-b1*Age[i])))
    }
    }
    
    ",fill=T)
sink()
## 


dat <- read.csv("eidolon_teeth_data.csv")

win.data<-list(data=dat$Morogoro,Age=dat$Age,nobs=length(dat$Morogoro))
inits<-function()
  list(a=rnorm(1,14),a1=rnorm(1,0.1),a2=rnorm(1,0.1),b1=rnorm(1,0.1),data.sd=runif(1,0,30))
params<-c("a","a1","a2","b1","data.var")

nc=3
ni=10000
nb=1000
nt=1
outMorogoroMat<-bugs(data=win.data,inits=inits,parameters.to.save=params,
                     model.file="allmodelMature.txt",n.thin=nt,n.chains=nc,n.burnin=nb,
                     n.iter=ni,debug=T,DIC=T,working.directory=getwd())

plot(outMorogoroMat)
hist(outMorogoroMat$sims.list$a2)
outMorogoroMat

################################################################
## nls2
st1 <- expand.grid(a = seq(1, 30, len = 10),
                   a2 = seq(0.1, 10, len = 10),
                   a3 = seq(-0.01,- 10, len = 10),
                   b3 = seq(-0.01,- 10, len = 10))

#startingvalues<-list(a=300,a1=2,b1=2,a2=0.2)

M3<-nls2(Morogoro~a*exp(-a2*Age)*exp((-a3/b3)*(1-exp(b3*Age))),
         start=st1,
         algorithm="grid-search")#,
#trace=T,
#control=control1)
summary(M3)
# sen
startingvalues<-list(a=max(Morogoro),a2=0.1,a3=-0.1,b3=-0.1)
G3<-nls(Morogoro~a*exp(-a2*Age)*exp((-a3/b3)*(1-exp(b3*Age))),
        start=startingvalues,
        algorithm="port",
        trace=T,
        control=control1)
summary(G3)

# sen

sink("allmodelSen.txt")
cat("
    model{
    a~dunif(0,1000)
    a3~dnorm(0,100)
    a2~dnorm(0,100)
    b3~dnorm(0,100)
    
    precision<-1/data.var
    data.var<-data.sd*data.sd
    data.sd~dunif(0,100)
    
    for (i in 1:nobs){
    data[i]~dnorm(mu[i],precision)
    mu[i]<-a*exp(-a2*Age[i])*exp((-a3/b3)*(1-exp(b3*Age[i])))
    }
    }
    
    ",fill=T)
sink()
## 


dat <- read.csv("eidolon_teeth_data.csv")

win.data<-list(data=dat$Morogoro,Age=dat$Age,nobs=length(dat$Morogoro))
inits<-function()
  list(a=rnorm(1,11),a3=rnorm(1,-0.01),a2=rnorm(1,0.1),b3=rnorm(1,-0.01),data.sd=runif(1,0,30))
params<-c("a","a3","a2","b3","data.var")

nc=3
ni=10000
nb=1000
nt=10
outMorogoroSen<-bugs(data=win.data,inits=inits,parameters.to.save=params,
                     model.file="allmodelSen.txt",n.thin=nt,n.chains=nc,n.burnin=nb,
                     n.iter=ni,debug=T,DIC=T,working.directory=getwd())

plot(outMorogoroSen)
outMorogoroSen

###############################################################
## both
## nls2
st1 <- expand.grid(a = seq(1, 40, len = 5),
                   a1 = seq(0.1, 10, len = 5),
                   a2 = seq(0.1, 10, len = 5),
                   a3 = seq(-0.01,- 10, len = 5),
                   b1 = seq(-0.01,- 10, len = 5),
                   b3 = seq(-0.01,- 10, len = 5))


#startingvalues<-list(a=300,a1=2,b1=2,a2=0.2)

M4<-nls2(Morogoro~a*exp(-a2*Age)*exp((-a3/b3)*(1-exp(b3*Age)))*exp((-a1/b1)*(1-exp(-b1*Age))),
         start=st1,
         algorithm="grid-search")#,
#trace=T,
#control=control1)
summary(M4)

# both
startingvalues<-list(a=max(Morogoro),a1=0.1,b1=-0.001,a2=0.1,a3=-0.001,b3=-10)
G4<-nls(Morogoro~a*exp(-a2*Age)*exp((-a3/b3)*(1-exp(b3*Age)))*exp((-a1/b1)*(1-exp(-b1*Age))),
        start=startingvalues,
        algorithm="port",
        trace=T,
        control=control1)
summary(G4)


sink("allmodelSiler.txt")
cat("
    model{
    a~dunif(0,1000)
    a1~dnorm(0,100)
    a2~dnorm(0,100)
    a3~dnorm(0,100)
    b1~dnorm(0,100)
    b3~dnorm(0,100)
    
    precision<-1/data.var
    data.var<-data.sd*data.sd
    data.sd~dunif(0,400)
    
    for (i in 1:nobs){
    data[i]~dnorm(mu[i],precision)
    mu[i]<-a*exp(-a2*Age[i])*exp((-a3/b3)*(1-exp(b3*Age[i])))*exp((-a1/b1)*(1-exp(-b1*Age[i])))
    }
    }
    
    ",fill=T)
sink()
## 


dat <- read.csv("eidolon_teeth_data.csv")

win.data<-list(data=dat$Morogoro,Age=dat$Age,nobs=length(dat$Morogoro))
inits<-function()
  list(a=rnorm(1,10),a1=rnorm(1,0.1),a2=rnorm(1,0.1),a3=rnorm(1,-0.01),b1=rnorm(1,-0.01),b3=rnorm(1,-10),data.sd=runif(1,1,30))
params<-c("a","a1","a2","a3","b1","b3","data.var")
nc=3
ni=10000
nb=1000
nt=10
outMorogoroSiler<-bugs(data=win.data,inits=inits,parameters.to.save=params,
                       model.file="allmodelSiler.txt",n.thin=nt,n.chains=nc,n.burnin=nb,
                       n.iter=ni,debug=T,DIC=T,working.directory=getwd())

#############

plot(Age,Morogoro,main="Morogoro",ylab="data")
lines(fitted(G1),lty=1)
lines(fitted(G2),lty=2)
lines(fitted(G3),lty=3)
lines(fitted(G4),lty=4)

##

legend("topright",legend=c("Constant","Maturation","Senescence","Both"),
       lty=1:4)

## AIC all
AIC(G1,G2,G3,G4)
res.aic<-AIC(G1,G2,G3,G4)

## model weights
aics<-res.aic[order(-res.aic$AIC),]
for(i in 1:dim(aics)[1]){
  aics$diff[i]<-aics$AIC[1]-aics$AIC[i]}
aics$wi<-2.71828182845904523536^(-0.5*aics$diff)
aics$aic.weights<-aics$wi/sum(aics$wi)
## 
aics
plot(aics$aic.weights,xaxt="n",xlab="model",ylab="AIC weights",pch=16,main="Morogoro")
mtext(c("Both","Constant","Maturation","Senescence"),side=1,at=1:4)

## NB can use barplot
barplot(aics$aic.weights,xaxt="n",xlab="model",ylab="AIC weights",width=0.8,main="Morogoro")
mtext(c("Both","Constant","Maturation","Senescence"),side=1,at=0.5:3.5)

######################################
outMorogoroE$DIC
a=outMorogoroE$mean$a
a2=outMorogoroE$mean$a2

plot(0:13,a*exp(-a2*dat$Age),ylab="",xlab="Age",type="l",ylim=c(0,30))
## plot CI
predictions<-array(dim=c(length(dat$Age),length(outMorogoroE$sims.list$a)))
for (i in 1:length(dat$Age)){
  predictions[i,]<-outMorogoroE$sims.list$a*exp(-outMorogoroE$sims.list$a2*dat$Age[i])
}
LPB<-apply(predictions,1,quantile,probs=0.025)
UPB<-apply(predictions,1,quantile,probs=0.975)
points(0:13,LPB,type="l",col="grey")
points(0:13,UPB,type="l",col="grey") # check 1 or 0 start
polygon(c(rev(dat$Age), dat$Age), c(rev(LPB),UPB), col =  "#00000010", border = NA)

outMorogoroSen$DIC
a=outMorogoroSen$mean$a
a2=outMorogoroSen$mean$a2
b1=outMorogoroSen$mean$b1
b3=outMorogoroSen$mean$b3

lines(0:13,a*exp(-a2*dat$Age)*exp((-a3/b3)*(1-exp(b3*dat$Age))),lty=2)
## plot CI
predictions<-array(dim=c(length(dat$Age),length(outMorogoroSen$sims.list$a)))
for (i in 1:length(dat$Age)){
  predictions[i,]<-outMorogoroSen$sims.list$a*exp(-outMorogoroSen$sims.list$a2*dat$Age[i])*exp((-outMorogoroSen$sims.list$a3/outMorogoroSen$sims.list$b3)*(1-exp(outMorogoroSen$sims.list$b3*dat$Age[i])))
}
LPB<-apply(predictions,1,quantile,probs=0.025)
UPB<-apply(predictions,1,quantile,probs=0.975)
points(0:13,LPB,type="l",col="grey",lty=2)
points(0:13,UPB,type="l",col="grey",lty=2) # check 1 or 0 start
polygon(c(rev(dat$Age), dat$Age), c(rev(LPB),UPB), col =  "#00000010", border = NA)

outMorogoroMat$DIC
a=outMorogoroMat$mean$a
a1=outMorogoroMat$mean$a1
a2=outMorogoroMat$mean$a2
b1=outMorogoroMat$mean$b1

lines(0:13,a*exp(-a2*dat$Age)*exp((-a1/b1)*(1-exp(-b1*dat$Age))),lty=3)
predictions<-array(dim=c(length(dat$Age),length(outMorogoroMat$sims.list$a)))
for (i in 1:length(dat$Age)){
  predictions[i,]<-outMorogoroMat$sims.list$a*exp(-outMorogoroMat$sims.list$a2*dat$Age[i])*exp((-outMorogoroMat$sims.list$a1/outMorogoroMat$sims.list$b1)*(1-exp(-outMorogoroMat$sims.list$b1*dat$Age[i])))
}
LPB<-apply(predictions,1,quantile,probs=0.025)
UPB<-apply(predictions,1,quantile,probs=0.975)
points(0:13,LPB,type="l",col="grey",lty=3)
points(0:13,UPB,type="l",col="grey",lty=3) # check 1 or 0 start
polygon(c(rev(dat$Age), dat$Age), c(rev(LPB),UPB), col =  "#00000010", border = NA)

outMorogoroSiler$DIC

a=outMorogoroSiler$mean$a
a1=outMorogoroSiler$mean$a1
a2=outMorogoroSiler$mean$a2
a3=outMorogoroSiler$mean$a3
b1=outMorogoroSiler$mean$b1
b3=outMorogoroSiler$mean$b3

lines(0:13,a*exp(-a2*dat$Age)*exp((-a3/b3)*(1-exp(b3*dat$Age)))*exp((-a1/b1)*(1-exp(-b1*dat$Age))),lty=4)
predictions<-array(dim=c(length(dat$Age),length(outMorogoroSiler$sims.list$a)))
for (i in 1:length(dat$Age)){
  predictions[i,]<-outMorogoroSiler$sims.list$a*exp(-outMorogoroSiler$sims.list$a2*dat$Age[i])*exp((-outMorogoroSiler$sims.list$a3/outMorogoroSiler$sims.list$b3)*(1-exp(outMorogoroSiler$sims.list$b3*dat$Age[i])))*exp((-outMorogoroSiler$sims.list$a1/outMorogoroSiler$sims.list$b1)*(1-exp(-outMorogoroSiler$sims.list$b1*dat$Age[i])))
}
LPB<-apply(predictions,1,quantile,probs=0.025)
UPB<-apply(predictions,1,quantile,probs=0.975)
points(0:13,LPB,type="l",col="grey",lty=4)
points(0:13,UPB,type="l",col="grey",lty=4) # check 1 or 0 start
polygon(c(rev(dat$Age), dat$Age), c(rev(LPB),UPB), col =  "#00000010", border = NA)
        
legend("topright",legend=c("Constant","Maturation","Senescence","Both"),
       lty=1:4,bty="n")

points(dat$Age,Morogoro,bg="black",pch=21)

min=min(outMorogoroE$DIC,outMorogoroMat$DIC,outMorogoroSen$DIC,outMorogoroSiler$DIC)
barplot(c(outMorogoroE$DIC-min,outMorogoroMat$DIC-min,outMorogoroSen$DIC-min,outMorogoroSiler$DIC-min),
        names.arg=c("Constant","Maturation","Senescence","Both"))

par(mfrow=c(3,2))
hist(outMorogoroSiler$sims.list$a, main="a",xlab="",col="grey")
abline(v=outMorogoroSiler$mean$a,col="red")
hist(outMorogoroSiler$sims.list$a1, main="a1",xlab="",col="grey")
abline(v=outMorogoroSiler$mean$a1,col="red")
hist(outMorogoroSiler$sims.list$a2, main="a2",xlab="",col="grey")
abline(v=outMorogoroSiler$mean$a2,col="red")
hist(outMorogoroSiler$sims.list$a3, main="a3",xlab="",col="grey")
abline(v=outMorogoroSiler$mean$a3,col="red")
hist(outMorogoroSiler$sims.list$b1, main="b1",xlab="",col="grey")
abline(v=outMorogoroSiler$mean$b1,col="red")
hist(outMorogoroSiler$sims.list$b3, main="b3",xlab="",col="grey")
abline(v=outMorogoroSiler$mean$b3,col="red")

###############################################################################
## calculate survival lx-1/lx
## ploting
######################################

a=outMorogoroE$mean$a
a2=outMorogoroE$mean$a2
SConst<-a*exp(-a2*dat$Age)

outMorogoroSen$DIC
a=outMorogoroSen$mean$a
a2=outMorogoroSen$mean$a2
b1=outMorogoroSen$mean$b1
b3=outMorogoroSen$mean$b3
SSen<-a*exp(-a2*dat$Age)*exp((-a3/b3)*(1-exp(b3*dat$Age)))

outMorogoroMat$DIC
a=outMorogoroMat$mean$a
a1=outMorogoroMat$mean$a1
a2=outMorogoroMat$mean$a2
b1=outMorogoroMat$mean$b1
SMat<-a*exp(-a2*dat$Age)*exp((-a1/b1)*(1-exp(-b1*dat$Age)))

a=outMorogoroSiler$mean$a
a1=outMorogoroSiler$mean$a1
a2=outMorogoroSiler$mean$a2
a3=outMorogoroSiler$mean$a3
b1=outMorogoroSiler$mean$b1
b3=outMorogoroSiler$mean$b3
SSiler<-a*exp(-a2*dat$Age)*exp((-a3/b3)*(1-exp(b3*dat$Age)))*exp((-a1/b1)*(1-exp(-b1*dat$Age)))
############################
pConst=(SConst[1:13+1])/(SConst[1:13])
pSen=(SSen[1:13+1])/(SSen[1:13])
pMat=(SMat[1:13+1])/(SMat[1:13])
pSiler=(SSiler[1:13+1])/(SSiler[1:13])
plot(0:12,pConst,ylim=c(0,1),type="l",xlab="Age",ylab="Survival")
lines(0:12,pSen,lty=2)
lines(0:12,pMat,lty=3)
lines(0:12,pSiler,lty=4)
######################################

###############################################################################


## DarEsSalaam

startingvalues<-list(a=max(DarEsSalaam),a2=0.2)
# constant
G1<-nls(DarEsSalaam~a*exp(-a2*Age),
        start=startingvalues,
        algorithm="port",
        trace=T,
        control=control1)
summary(G1)

#####################

# openbugs

library(R2OpenBUGS)
setwd("~/ali_peel_survival")
sink("allmodel.txt")
cat("
    model{
    a~dunif(0,1000)
    a2~dnorm(0,3)
    
    precision<-1/data.var
    data.var<-data.sd*data.sd
    data.sd~dunif(0,100)
    
    for (i in 1:nobs){
    data[i]~dnorm(mu[i],precision)
    mu[i]<-a*exp(-a2*Age[i])
    }
    }
    
    ",fill=T)
sink()
## 


dat <- read.csv("eidolon_teeth_data.csv")

win.data<-list(data=dat$DarEsSalaam,Age=dat$Age,nobs=length(dat$DarEsSalaam))
inits<-function()
  list(a=rnorm(1,30),a2=rnorm(1,0),data.sd=runif(1,1,30))
params<-c("a","a2","data.var")
nc=3
ni=10000
nb=1000
nt=1
outDarEsSalaamE<-bugs(data=win.data,inits=inits,parameters.to.save=params,
                   model.file="allmodel.txt",n.thin=nt,n.chains=nc,n.burnin=nb,
                   n.iter=ni,debug=T,DIC=T,working.directory=getwd())
plot(outDarEsSalaamE)
hist(outDarEsSalaamE$sims.list$a2)

######################################################
# mat
##
## nls2
st1 <- expand.grid(a = seq(1, 40, len = 10),
                   a1 = seq(0.1, 10, len = 10),
                   b1 = seq(0.1, 10, len = 10),
                   a2 = seq(0.1, 10, len = 10))

#startingvalues<-list(a=300,a1=2,b1=2,a2=0.2)
library(nls2)

M2<-nls2(DarEsSalaam~a*exp(-a2*Age)*exp((-a1/b1)*(1-exp(-b1*Age))),
         start=st1,
         algorithm="grid-search")#,
#trace=T,
#control=control1)
summary(M2)
##
###############
sink("allmodelMature.txt")
cat("
    model{
    a~dunif(0,1000)
    a1~dnorm(0,100)
    a2~dnorm(0,100)
    b1~dnorm(0,100)
    
    precision<-1/data.var
    data.var<-data.sd*data.sd
    data.sd~dunif(0,100)
    
    for (i in 1:nobs){
    data[i]~dnorm(mu[i],precision)
    mu[i]<-a*exp(-a2*Age[i])*exp((-a1/b1)*(1-exp(-b1*Age[i])))
    }
    }
    
    ",fill=T)
sink()
## 


dat <- read.csv("eidolon_teeth_data.csv")

win.data<-list(data=dat$DarEsSalaam,Age=dat$Age,nobs=length(dat$DarEsSalaam))
inits<-function()
  list(a=rnorm(1,9),a1=rnorm(1,1),a2=rnorm(1,0.1),b1=rnorm(1,1),data.sd=runif(1,0,30))
params<-c("a","a1","a2","b1","data.var")

nc=3
ni=10000
nb=1000
nt=1
outDarEsSalaamMat<-bugs(data=win.data,inits=inits,parameters.to.save=params,
                     model.file="allmodelMature.txt",n.thin=nt,n.chains=nc,n.burnin=nb,
                     n.iter=ni,debug=T,DIC=T,working.directory=getwd())

plot(outDarEsSalaamMat)
hist(outDarEsSalaamMat$sims.list$a2)
outDarEsSalaamMat

################################################################
## nls2
st1 <- expand.grid(a = seq(1, 30, len = 10),
                   a2 = seq(0.1, 10, len = 10),
                   a3 = seq(-0.01,- 10, len = 10),
                   b3 = seq(-0.01,- 10, len = 10))

#startingvalues<-list(a=300,a1=2,b1=2,a2=0.2)

M3<-nls2(DarEsSalaam~a*exp(-a2*Age)*exp((-a3/b3)*(1-exp(b3*Age))),
         start=st1,
         algorithm="grid-search")#,
#trace=T,
#control=control1)
summary(M3)
# sen
startingvalues<-list(a=max(DarEsSalaam),a2=0.1,a3=-0.1,b3=-0.1)
G3<-nls(DarEsSalaam~a*exp(-a2*Age)*exp((-a3/b3)*(1-exp(b3*Age))),
        start=startingvalues,
        algorithm="port",
        trace=T,
        control=control1)
summary(G3)

# sen

sink("allmodelSen.txt")
cat("
    model{
    a~dunif(0,1000)
    a3~dnorm(0,100)
    a2~dnorm(0,100)
    b3~dnorm(0,100)
    
    precision<-1/data.var
    data.var<-data.sd*data.sd
    data.sd~dunif(0,100)
    
    for (i in 1:nobs){
    data[i]~dnorm(mu[i],precision)
    mu[i]<-a*exp(-a2*Age[i])*exp((-a3/b3)*(1-exp(b3*Age[i])))
    }
    }
    
    ",fill=T)
sink()
## 


dat <- read.csv("eidolon_teeth_data.csv")

win.data<-list(data=dat$DarEsSalaam,Age=dat$Age,nobs=length(dat$DarEsSalaam))
inits<-function()
  list(a=rnorm(1,7.4),a3=rnorm(1,-0.01),a2=rnorm(1,0.1),b3=rnorm(1,-0.1),data.sd=runif(1,0,30))
params<-c("a","a3","a2","b3","data.var")

nc=3
ni=10000
nb=1000
nt=10
outDarEsSalaamSen<-bugs(data=win.data,inits=inits,parameters.to.save=params,
                     model.file="allmodelSen.txt",n.thin=nt,n.chains=nc,n.burnin=nb,
                     n.iter=ni,debug=T,DIC=T,working.directory=getwd())

plot(outDarEsSalaamSen)
outDarEsSalaamSen

###############################################################
## both
## nls2
st1 <- expand.grid(a = seq(1, 40, len = 5),
                   a1 = seq(0.1, 10, len = 5),
                   a2 = seq(0.1, 10, len = 5),
                   a3 = seq(-0.01,- 10, len = 5),
                   b1 = seq(-0.01,- 10, len = 5),
                   b3 = seq(-0.01,- 10, len = 5))


#startingvalues<-list(a=300,a1=2,b1=2,a2=0.2)

M4<-nls2(DarEsSalaam~a*exp(-a2*Age)*exp((-a3/b3)*(1-exp(b3*Age)))*exp((-a1/b1)*(1-exp(-b1*Age))),
         start=st1,
         algorithm="grid-search")#,
#trace=T,
#control=control1)
summary(M4)

# both
startingvalues<-list(a=max(DarEsSalaam),a1=0.1,b1=-0.001,a2=0.1,a3=-0.001,b3=-10)
G4<-nls(DarEsSalaam~a*exp(-a2*Age)*exp((-a3/b3)*(1-exp(b3*Age)))*exp((-a1/b1)*(1-exp(-b1*Age))),
        start=startingvalues,
        algorithm="port",
        trace=T,
        control=control1)
summary(G4)


sink("allmodelSiler.txt")
cat("
    model{
    a~dunif(0,1000)
    a1~dnorm(0,100)
    a2~dnorm(0,100)
    a3~dnorm(0,100)
    b1~dnorm(0,100)
    b3~dnorm(0,100)
    
    precision<-1/data.var
    data.var<-data.sd*data.sd
    data.sd~dunif(0,400)
    
    for (i in 1:nobs){
    data[i]~dnorm(mu[i],precision)
    mu[i]<-a*exp(-a2*Age[i])*exp((-a3/b3)*(1-exp(b3*Age[i])))*exp((-a1/b1)*(1-exp(-b1*Age[i])))
    }
    }
    
    ",fill=T)
sink()
## 


dat <- read.csv("eidolon_teeth_data.csv")

win.data<-list(data=dat$DarEsSalaam,Age=dat$Age,nobs=length(dat$DarEsSalaam))
inits<-function()
  list(a=rnorm(1,10),a1=rnorm(1,0.1),a2=rnorm(1,0.1),a3=rnorm(1,-0.01),b1=rnorm(1,-0.01),b3=rnorm(1,-10),data.sd=runif(1,1,30))
params<-c("a","a1","a2","a3","b1","b3","data.var")
nc=3
ni=10000
nb=1000
nt=10
outDarEsSalaamSiler<-bugs(data=win.data,inits=inits,parameters.to.save=params,
                       model.file="allmodelSiler.txt",n.thin=nt,n.chains=nc,n.burnin=nb,
                       n.iter=ni,debug=T,DIC=T,working.directory=getwd())

#############

plot(Age,DarEsSalaam,main="DarEsSalaam",ylab="data")
lines(fitted(G1),lty=1)
lines(fitted(G2),lty=2)
lines(fitted(G3),lty=3)
lines(fitted(G4),lty=4)

##

legend("topright",legend=c("Constant","Maturation","Senescence","Both"),
       lty=1:4)

## AIC all
AIC(G1,G2,G3,G4)
res.aic<-AIC(G1,G2,G3,G4)

## model weights
aics<-res.aic[order(-res.aic$AIC),]
for(i in 1:dim(aics)[1]){
  aics$diff[i]<-aics$AIC[1]-aics$AIC[i]}
aics$wi<-2.71828182845904523536^(-0.5*aics$diff)
aics$aic.weights<-aics$wi/sum(aics$wi)
## 
aics
plot(aics$aic.weights,xaxt="n",xlab="model",ylab="AIC weights",pch=16,main="DarEsSalaam")
mtext(c("Both","Constant","Maturation","Senescence"),side=1,at=1:4)

## NB can use barplot
barplot(aics$aic.weights,xaxt="n",xlab="model",ylab="AIC weights",width=0.8,main="DarEsSalaam")
mtext(c("Both","Constant","Maturation","Senescence"),side=1,at=0.5:3.5)

######################################
outDarEsSalaamE$DIC
a=outDarEsSalaamE$mean$a
a2=outDarEsSalaamE$mean$a2

plot(0:13,a*exp(-a2*dat$Age),ylab="",xlab="Age",type="l",ylim=c(0,15))
## plot CI
predictions<-array(dim=c(length(dat$Age),length(outDarEsSalaamE$sims.list$a)))
for (i in 1:length(dat$Age)){
  predictions[i,]<-outDarEsSalaamE$sims.list$a*exp(-outDarEsSalaamE$sims.list$a2*dat$Age[i])
}
LPB<-apply(predictions,1,quantile,probs=0.025)
UPB<-apply(predictions,1,quantile,probs=0.975)
points(0:13,LPB,type="l",col="grey")
points(0:13,UPB,type="l",col="grey") # check 1 or 0 start
polygon(c(rev(dat$Age), dat$Age), c(rev(LPB),UPB), col =  "#00000010", border = NA)

outDarEsSalaamSen$DIC
a=outDarEsSalaamSen$mean$a
a2=outDarEsSalaamSen$mean$a2
b1=outDarEsSalaamSen$mean$b1
b3=outDarEsSalaamSen$mean$b3

lines(0:13,a*exp(-a2*dat$Age)*exp((-a3/b3)*(1-exp(b3*dat$Age))),lty=2)
## plot CI
predictions<-array(dim=c(length(dat$Age),length(outDarEsSalaamSen$sims.list$a)))
for (i in 1:length(dat$Age)){
  predictions[i,]<-outDarEsSalaamSen$sims.list$a*exp(-outDarEsSalaamSen$sims.list$a2*dat$Age[i])*exp((-outDarEsSalaamSen$sims.list$a3/outDarEsSalaamSen$sims.list$b3)*(1-exp(outDarEsSalaamSen$sims.list$b3*dat$Age[i])))
}
LPB<-apply(predictions,1,quantile,probs=0.025)
UPB<-apply(predictions,1,quantile,probs=0.975)
points(0:13,LPB,type="l",col="grey",lty=2)
points(0:13,UPB,type="l",col="grey",lty=2) # check 1 or 0 start
polygon(c(rev(dat$Age), dat$Age), c(rev(LPB),UPB), col =  "#00000010", border = NA)

outDarEsSalaamMat$DIC
a=outDarEsSalaamMat$mean$a
a1=outDarEsSalaamMat$mean$a1
a2=outDarEsSalaamMat$mean$a2
b1=outDarEsSalaamMat$mean$b1

lines(0:13,a*exp(-a2*dat$Age)*exp((-a1/b1)*(1-exp(-b1*dat$Age))),lty=3)
predictions<-array(dim=c(length(dat$Age),length(outDarEsSalaamMat$sims.list$a)))
for (i in 1:length(dat$Age)){
  predictions[i,]<-outDarEsSalaamMat$sims.list$a*exp(-outDarEsSalaamMat$sims.list$a2*dat$Age[i])*exp((-outDarEsSalaamMat$sims.list$a1/outDarEsSalaamMat$sims.list$b1)*(1-exp(-outDarEsSalaamMat$sims.list$b1*dat$Age[i])))
}
LPB<-apply(predictions,1,quantile,probs=0.025)
UPB<-apply(predictions,1,quantile,probs=0.975)
points(0:13,LPB,type="l",col="grey",lty=3)
points(0:13,UPB,type="l",col="grey",lty=3) # check 1 or 0 start
polygon(c(rev(dat$Age), dat$Age), c(rev(LPB),UPB), col =  "#00000010", border = NA)

outDarEsSalaamSiler$DIC

a=outDarEsSalaamSiler$mean$a
a1=outDarEsSalaamSiler$mean$a1
a2=outDarEsSalaamSiler$mean$a2
a3=outDarEsSalaamSiler$mean$a3
b1=outDarEsSalaamSiler$mean$b1
b3=outDarEsSalaamSiler$mean$b3

lines(0:13,a*exp(-a2*dat$Age)*exp((-a3/b3)*(1-exp(b3*dat$Age)))*exp((-a1/b1)*(1-exp(-b1*dat$Age))),lty=4)
predictions<-array(dim=c(length(dat$Age),length(outDarEsSalaamSiler$sims.list$a)))
for (i in 1:length(dat$Age)){
  predictions[i,]<-outDarEsSalaamSiler$sims.list$a*exp(-outDarEsSalaamSiler$sims.list$a2*dat$Age[i])*exp((-outDarEsSalaamSiler$sims.list$a3/outDarEsSalaamSiler$sims.list$b3)*(1-exp(outDarEsSalaamSiler$sims.list$b3*dat$Age[i])))*exp((-outDarEsSalaamSiler$sims.list$a1/outDarEsSalaamSiler$sims.list$b1)*(1-exp(-outDarEsSalaamSiler$sims.list$b1*dat$Age[i])))
}
LPB<-apply(predictions,1,quantile,probs=0.025)
UPB<-apply(predictions,1,quantile,probs=0.975)
points(0:13,LPB,type="l",col="grey",lty=4)
points(0:13,UPB,type="l",col="grey",lty=4) # check 1 or 0 start
polygon(c(rev(dat$Age), dat$Age), c(rev(LPB),UPB), col =  "#00000010", border = NA)

legend("topright",legend=c("Constant","Maturation","Senescence","Both"),
       lty=1:4,bty="n")

points(dat$Age,DarEsSalaam,bg="black",pch=21)

min=min(outDarEsSalaamE$DIC,outDarEsSalaamMat$DIC,outDarEsSalaamSen$DIC,outDarEsSalaamSiler$DIC)
barplot(c(outDarEsSalaamE$DIC-min,outDarEsSalaamMat$DIC-min,outDarEsSalaamSen$DIC-min,outDarEsSalaamSiler$DIC-min),
        names.arg=c("Constant","Maturation","Senescence","Both"))

par(mfrow=c(3,2))
hist(outDarEsSalaamSiler$sims.list$a, main="a",xlab="",col="grey")
abline(v=outDarEsSalaamSiler$mean$a,col="red")
hist(outDarEsSalaamSiler$sims.list$a1, main="a1",xlab="",col="grey")
abline(v=outDarEsSalaamSiler$mean$a1,col="red")
hist(outDarEsSalaamSiler$sims.list$a2, main="a2",xlab="",col="grey")
abline(v=outDarEsSalaamSiler$mean$a2,col="red")
hist(outDarEsSalaamSiler$sims.list$a3, main="a3",xlab="",col="grey")
abline(v=outDarEsSalaamSiler$mean$a3,col="red")
hist(outDarEsSalaamSiler$sims.list$b1, main="b1",xlab="",col="grey")
abline(v=outDarEsSalaamSiler$mean$b1,col="red")
hist(outDarEsSalaamSiler$sims.list$b3, main="b3",xlab="",col="grey")
abline(v=outDarEsSalaamSiler$mean$b3,col="red")

###############################################################################
## calculate survival lx-1/lx
## ploting
######################################

a=outDarEsSalaamE$mean$a
a2=outDarEsSalaamE$mean$a2
SConst<-a*exp(-a2*dat$Age)

outDarEsSalaamSen$DIC
a=outDarEsSalaamSen$mean$a
a2=outDarEsSalaamSen$mean$a2
b1=outDarEsSalaamSen$mean$b1
b3=outDarEsSalaamSen$mean$b3
SSen<-a*exp(-a2*dat$Age)*exp((-a3/b3)*(1-exp(b3*dat$Age)))

outDarEsSalaamMat$DIC
a=outDarEsSalaamMat$mean$a
a1=outDarEsSalaamMat$mean$a1
a2=outDarEsSalaamMat$mean$a2
b1=outDarEsSalaamMat$mean$b1
SMat<-a*exp(-a2*dat$Age)*exp((-a1/b1)*(1-exp(-b1*dat$Age)))

a=outDarEsSalaamSiler$mean$a
a1=outDarEsSalaamSiler$mean$a1
a2=outDarEsSalaamSiler$mean$a2
a3=outDarEsSalaamSiler$mean$a3
b1=outDarEsSalaamSiler$mean$b1
b3=outDarEsSalaamSiler$mean$b3
SSiler<-a*exp(-a2*dat$Age)*exp((-a3/b3)*(1-exp(b3*dat$Age)))*exp((-a1/b1)*(1-exp(-b1*dat$Age)))
############################
pConst=(SConst[1:13+1])/(SConst[1:13])
pSen=(SSen[1:13+1])/(SSen[1:13])
pMat=(SMat[1:13+1])/(SMat[1:13])
pSiler=(SSiler[1:13+1])/(SSiler[1:13])
plot(0:12,pConst,ylim=c(0,1),type="l",xlab="Age",ylab="Survival")
lines(0:12,pSen,lty=2)
lines(0:12,pMat,lty=3)
lines(0:12,pSiler,lty=4)
######################################

##########################################################

## Bioko

startingvalues<-list(a=max(Bioko),a2=0.2)
# constant
G1<-nls(Bioko~a*exp(-a2*Age),
        start=startingvalues,
        algorithm="port",
        trace=T,
        control=control1)
summary(G1)

#####################

# openbugs

library(R2OpenBUGS)
setwd("~/ali_peel_survival")
sink("allmodel.txt")
cat("
    model{
    a~dunif(0,1000)
    a2~dnorm(0,3)
    
    precision<-1/data.var
    data.var<-data.sd*data.sd
    data.sd~dunif(0,100)
    
    for (i in 1:nobs){
    data[i]~dnorm(mu[i],precision)
    mu[i]<-a*exp(-a2*Age[i])
    }
    }
    
    ",fill=T)
sink()
## 


dat <- read.csv("eidolon_teeth_data.csv")

win.data<-list(data=dat$Bioko,Age=dat$Age,nobs=length(dat$Bioko))
inits<-function()
  list(a=rnorm(1,84),a2=rnorm(3,0),data.sd=runif(1,1,30))
params<-c("a","a2","data.var")
nc=3
ni=10000
nb=1000
nt=1
outBiokoE<-bugs(data=win.data,inits=inits,parameters.to.save=params,
                      model.file="allmodel.txt",n.thin=nt,n.chains=nc,n.burnin=nb,
                      n.iter=ni,debug=T,DIC=T,working.directory=getwd())
plot(outBiokoE)
hist(outBiokoE$sims.list$a2)

######################################################
# mat
##
## nls2
st1 <- expand.grid(a = seq(1, 100, len = 10),
                   a1 = seq(0.1, 10, len = 10),
                   b1 = seq(0.1, 10, len = 10),
                   a2 = seq(0.1, 10, len = 10))

#startingvalues<-list(a=300,a1=2,b1=2,a2=0.2)
library(nls2)

M2<-nls2(Bioko~a*exp(-a2*Age)*exp((-a1/b1)*(1-exp(-b1*Age))),
         start=st1,
         algorithm="grid-search")#,
#trace=T,
#control=control1)
summary(M2)
##
startingvalues<-list(a=max(Bioko),a1=0.1,b1=0.1,a2=0.1)
G2<-nls(Bioko~a*exp(-a2*Age)*exp((-a1/b1)*(1-exp(-b1*Age))),
        start=startingvalues,
        algorithm="port",
        trace=T,
        control=control1)
summary(G2)
####################
sink("allmodelMature.txt")
cat("
    model{
    a~dunif(0,1000)
    a1~dnorm(0,100)
    a2~dnorm(0,100)
    b1~dnorm(0,100)
    
    precision<-1/data.var
    data.var<-data.sd*data.sd
    data.sd~dunif(0,100)
    
    for (i in 1:nobs){
    data[i]~dnorm(mu[i],precision)
    mu[i]<-a*exp(-a2*Age[i])*exp((-a1/b1)*(1-exp(-b1*Age[i])))
    }
    }
    
    ",fill=T)
sink()
## 


dat <- read.csv("eidolon_teeth_data.csv")

win.data<-list(data=dat$Bioko,Age=dat$Age,nobs=length(dat$Bioko))
inits<-function()
  list(a=rnorm(1,89),a1=rnorm(1,8),a2=rnorm(1,0.1),b1=rnorm(1,2.3),data.sd=runif(1,0,30))
params<-c("a","a1","a2","b1","data.var")

nc=3
ni=10000
nb=1000
nt=1
outBiokoMat<-bugs(data=win.data,inits=inits,parameters.to.save=params,
                        model.file="allmodelMature.txt",n.thin=nt,n.chains=nc,n.burnin=nb,
                        n.iter=ni,debug=T,DIC=T,working.directory=getwd())

plot(outBiokoMat)
hist(outBiokoMat$sims.list$a2)
outBiokoMat

################################################################
## nls2
st1 <- expand.grid(a = seq(1, 30, len = 10),
                   a2 = seq(0.1, 10, len = 10),
                   a3 = seq(-0.01,- 10, len = 10),
                   b3 = seq(-0.01,- 10, len = 10))

#startingvalues<-list(a=300,a1=2,b1=2,a2=0.2)

M3<-nls2(Bioko~a*exp(-a2*Age)*exp((-a3/b3)*(1-exp(b3*Age))),
         start=st1,
         algorithm="grid-search")#,
#trace=T,
#control=control1)
summary(M3)
# sen
startingvalues<-list(a=max(Bioko),a2=0.1,a3=-0.1,b3=-0.1)
G3<-nls(Bioko~a*exp(-a2*Age)*exp((-a3/b3)*(1-exp(b3*Age))),
        start=startingvalues,
        algorithm="port",
        trace=T,
        control=control1)
summary(G3)

# sen

sink("allmodelSen.txt")
cat("
    model{
    a~dunif(0,1000)
    a3~dnorm(0,100)
    a2~dnorm(0,100)
    b3~dnorm(0,100)
    
    precision<-1/data.var
    data.var<-data.sd*data.sd
    data.sd~dunif(0,100)
    
    for (i in 1:nobs){
    data[i]~dnorm(mu[i],precision)
    mu[i]<-a*exp(-a2*Age[i])*exp((-a3/b3)*(1-exp(b3*Age[i])))
    }
    }
    
    ",fill=T)
sink()
## 


dat <- read.csv("eidolon_teeth_data.csv")

win.data<-list(data=dat$Bioko,Age=dat$Age,nobs=length(dat$Bioko))
inits<-function()
  list(a=rnorm(1,30),a3=rnorm(1,-9),a2=rnorm(1,0.1),b3=rnorm(1,-5),data.sd=runif(1,0,30))
params<-c("a","a3","a2","b3","data.var")

nc=3
ni=10000
nb=1000
nt=10
outBiokoSen<-bugs(data=win.data,inits=inits,parameters.to.save=params,
                        model.file="allmodelSen.txt",n.thin=nt,n.chains=nc,n.burnin=nb,
                        n.iter=ni,debug=T,DIC=T,working.directory=getwd())

plot(outBiokoSen)
outBiokoSen

###############################################################
## both
## nls2
st1 <- expand.grid(a = seq(1, 40, len = 5),
                   a1 = seq(0.1, 10, len = 5),
                   a2 = seq(0.1, 10, len = 5),
                   a3 = seq(-0.01,- 10, len = 5),
                   b1 = seq(-0.01,- 10, len = 5),
                   b3 = seq(-0.01,- 10, len = 5))


#startingvalues<-list(a=300,a1=2,b1=2,a2=0.2)

M4<-nls2(Bioko~a*exp(-a2*Age)*exp((-a3/b3)*(1-exp(b3*Age)))*exp((-a1/b1)*(1-exp(-b1*Age))),
         start=st1,
         algorithm="grid-search")#,
#trace=T,
#control=control1)
summary(M4)

# both
startingvalues<-list(a=max(Bioko),a1=0.1,b1=-0.001,a2=0.1,a3=-0.001,b3=-10)
G4<-nls(Bioko~a*exp(-a2*Age)*exp((-a3/b3)*(1-exp(b3*Age)))*exp((-a1/b1)*(1-exp(-b1*Age))),
        start=startingvalues,
        algorithm="port",
        trace=T,
        control=control1)
summary(G4)


sink("allmodelSiler.txt")
cat("
    model{
    a~dunif(0,1000)
    a1~dnorm(0,100)
    a2~dnorm(0,100)
    a3~dnorm(0,100)
    b1~dnorm(0,100)
    b3~dnorm(0,100)
    
    precision<-1/data.var
    data.var<-data.sd*data.sd
    data.sd~dunif(0,400)
    
    for (i in 1:nobs){
    data[i]~dnorm(mu[i],precision)
    mu[i]<-a*exp(-a2*Age[i])*exp((-a3/b3)*(1-exp(b3*Age[i])))*exp((-a1/b1)*(1-exp(-b1*Age[i])))
    }
    }
    
    ",fill=T)
sink()
## 


dat <- read.csv("eidolon_teeth_data.csv")

win.data<-list(data=dat$Bioko,Age=dat$Age,nobs=length(dat$Bioko))
inits<-function()
  list(a=rnorm(1,10),a1=rnorm(1,0.1),a2=rnorm(1,0.1),a3=rnorm(1,-0.01),b1=rnorm(1,-0.01),b3=rnorm(1,-10),data.sd=runif(1,1,30))
params<-c("a","a1","a2","a3","b1","b3","data.var")
nc=3
ni=10000
nb=1000
nt=10
outBiokoSiler<-bugs(data=win.data,inits=inits,parameters.to.save=params,
                          model.file="allmodelSiler.txt",n.thin=nt,n.chains=nc,n.burnin=nb,
                          n.iter=ni,debug=T,DIC=T,working.directory=getwd())

#############

plot(Age,Bioko,main="Bioko",ylab="data")
lines(fitted(G1),lty=1)
lines(fitted(G2),lty=2)
lines(fitted(G3),lty=3)
lines(fitted(G4),lty=4)

##

legend("topright",legend=c("Constant","Maturation","Senescence","Both"),
       lty=1:4)

## AIC all
AIC(G1,G2,G3,G4)
res.aic<-AIC(G1,G2,G3,G4)

## model weights
aics<-res.aic[order(-res.aic$AIC),]
for(i in 1:dim(aics)[1]){
  aics$diff[i]<-aics$AIC[1]-aics$AIC[i]}
aics$wi<-2.71828182845904523536^(-0.5*aics$diff)
aics$aic.weights<-aics$wi/sum(aics$wi)
## 
aics
plot(aics$aic.weights,xaxt="n",xlab="model",ylab="AIC weights",pch=16,main="Bioko")
mtext(c("Both","Constant","Maturation","Senescence"),side=1,at=1:4)

## NB can use barplot
barplot(aics$aic.weights,xaxt="n",xlab="model",ylab="AIC weights",width=0.8,main="Bioko")
mtext(c("Both","Constant","Maturation","Senescence"),side=1,at=0.5:3.5)

######################################
outBiokoE$DIC
a=outBiokoE$mean$a
a2=outBiokoE$mean$a2

plot(0:13,a*exp(-a2*dat$Age),ylab="",xlab="Age",type="l",ylim=c(0,30))
## plot CI
predictions<-array(dim=c(length(dat$Age),length(outBiokoE$sims.list$a)))
for (i in 1:length(dat$Age)){
  predictions[i,]<-outBiokoE$sims.list$a*exp(-outBiokoE$sims.list$a2*dat$Age[i])
}
LPB<-apply(predictions,1,quantile,probs=0.025)
UPB<-apply(predictions,1,quantile,probs=0.975)
points(0:13,LPB,type="l",col="grey")
points(0:13,UPB,type="l",col="grey") # check 1 or 0 start
polygon(c(rev(dat$Age), dat$Age), c(rev(LPB),UPB), col =  "#00000010", border = NA)

outBiokoSen$DIC
a=outBiokoSen$mean$a
a2=outBiokoSen$mean$a2
b1=outBiokoSen$mean$b1
b3=outBiokoSen$mean$b3

lines(0:13,a*exp(-a2*dat$Age)*exp((-a3/b3)*(1-exp(b3*dat$Age))),lty=2)
## plot CI
predictions<-array(dim=c(length(dat$Age),length(outBiokoSen$sims.list$a)))
for (i in 1:length(dat$Age)){
  predictions[i,]<-outBiokoSen$sims.list$a*exp(-outBiokoSen$sims.list$a2*dat$Age[i])*exp((-outBiokoSen$sims.list$a3/outBiokoSen$sims.list$b3)*(1-exp(outBiokoSen$sims.list$b3*dat$Age[i])))
}
LPB<-apply(predictions,1,quantile,probs=0.025)
UPB<-apply(predictions,1,quantile,probs=0.975)
points(0:13,LPB,type="l",col="grey",lty=2)
points(0:13,UPB,type="l",col="grey",lty=2) # check 1 or 0 start
polygon(c(rev(dat$Age), dat$Age), c(rev(LPB),UPB), col =  "#00000010", border = NA)

outBiokoMat$DIC
a=outBiokoMat$mean$a
a1=outBiokoMat$mean$a1
a2=outBiokoMat$mean$a2
b1=outBiokoMat$mean$b1

lines(0:13,a*exp(-a2*dat$Age)*exp((-a1/b1)*(1-exp(-b1*dat$Age))),lty=3)
predictions<-array(dim=c(length(dat$Age),length(outBiokoMat$sims.list$a)))
for (i in 1:length(dat$Age)){
  predictions[i,]<-outBiokoMat$sims.list$a*exp(-outBiokoMat$sims.list$a2*dat$Age[i])*exp((-outBiokoMat$sims.list$a1/outBiokoMat$sims.list$b1)*(1-exp(-outBiokoMat$sims.list$b1*dat$Age[i])))
}
LPB<-apply(predictions,1,quantile,probs=0.025)
UPB<-apply(predictions,1,quantile,probs=0.975)
points(0:13,LPB,type="l",col="grey",lty=3)
points(0:13,UPB,type="l",col="grey",lty=3) # check 1 or 0 start
polygon(c(rev(dat$Age), dat$Age), c(rev(LPB),UPB), col =  "#00000010", border = NA)

outBiokoSiler$DIC

a=outBiokoSiler$mean$a
a1=outBiokoSiler$mean$a1
a2=outBiokoSiler$mean$a2
a3=outBiokoSiler$mean$a3
b1=outBiokoSiler$mean$b1
b3=outBiokoSiler$mean$b3

lines(0:13,a*exp(-a2*dat$Age)*exp((-a3/b3)*(1-exp(b3*dat$Age)))*exp((-a1/b1)*(1-exp(-b1*dat$Age))),lty=4)
predictions<-array(dim=c(length(dat$Age),length(outBiokoSiler$sims.list$a)))
for (i in 1:length(dat$Age)){
  predictions[i,]<-outBiokoSiler$sims.list$a*exp(-outBiokoSiler$sims.list$a2*dat$Age[i])*exp((-outBiokoSiler$sims.list$a3/outBiokoSiler$sims.list$b3)*(1-exp(outBiokoSiler$sims.list$b3*dat$Age[i])))*exp((-outBiokoSiler$sims.list$a1/outBiokoSiler$sims.list$b1)*(1-exp(-outBiokoSiler$sims.list$b1*dat$Age[i])))
}
LPB<-apply(predictions,1,quantile,probs=0.025)
UPB<-apply(predictions,1,quantile,probs=0.975)
points(0:13,LPB,type="l",col="grey",lty=4)
points(0:13,UPB,type="l",col="grey",lty=4) # check 1 or 0 start
polygon(c(rev(dat$Age), dat$Age), c(rev(LPB),UPB), col =  "#00000010", border = NA)

legend("topright",legend=c("Constant","Maturation","Senescence","Both"),
       lty=1:4,bty="n")

points(dat$Age,Bioko,bg="black",pch=21)

min=min(outBiokoE$DIC,outBiokoMat$DIC,outBiokoSen$DIC,outBiokoSiler$DIC)
barplot(c(outBiokoE$DIC-min,outBiokoMat$DIC-min,outBiokoSen$DIC-min,outBiokoSiler$DIC-min),
        names.arg=c("Constant","Maturation","Senescence","Both"))

par(mfrow=c(3,2))
hist(outBiokoSiler$sims.list$a, main="a",xlab="",col="grey")
abline(v=outBiokoSiler$mean$a,col="red")
hist(outBiokoSiler$sims.list$a1, main="a1",xlab="",col="grey")
abline(v=outBiokoSiler$mean$a1,col="red")
hist(outBiokoSiler$sims.list$a2, main="a2",xlab="",col="grey")
abline(v=outBiokoSiler$mean$a2,col="red")
hist(outBiokoSiler$sims.list$a3, main="a3",xlab="",col="grey")
abline(v=outBiokoSiler$mean$a3,col="red")
hist(outBiokoSiler$sims.list$b1, main="b1",xlab="",col="grey")
abline(v=outBiokoSiler$mean$b1,col="red")
hist(outBiokoSiler$sims.list$b3, main="b3",xlab="",col="grey")
abline(v=outBiokoSiler$mean$b3,col="red")

###############################################################################
## calculate survival lx-1/lx
## ploting
######################################

a=outBiokoE$mean$a
a2=outBiokoE$mean$a2
SConst<-a*exp(-a2*dat$Age)

outBiokoSen$DIC
a=outBiokoSen$mean$a
a2=outBiokoSen$mean$a2
b1=outBiokoSen$mean$b1
b3=outBiokoSen$mean$b3
SSen<-a*exp(-a2*dat$Age)*exp((-a3/b3)*(1-exp(b3*dat$Age)))

outBiokoMat$DIC
a=outBiokoMat$mean$a
a1=outBiokoMat$mean$a1
a2=outBiokoMat$mean$a2
b1=outBiokoMat$mean$b1
SMat<-a*exp(-a2*dat$Age)*exp((-a1/b1)*(1-exp(-b1*dat$Age)))

a=outBiokoSiler$mean$a
a1=outBiokoSiler$mean$a1
a2=outBiokoSiler$mean$a2
a3=outBiokoSiler$mean$a3
b1=outBiokoSiler$mean$b1
b3=outBiokoSiler$mean$b3
SSiler<-a*exp(-a2*dat$Age)*exp((-a3/b3)*(1-exp(b3*dat$Age)))*exp((-a1/b1)*(1-exp(-b1*dat$Age)))
############################
pConst=(SConst[1:13+1])/(SConst[1:13])
pSen=(SSen[1:13+1])/(SSen[1:13])
pMat=(SMat[1:13+1])/(SMat[1:13])
pSiler=(SSiler[1:13+1])/(SSiler[1:13])
plot(0:12,pConst,ylim=c(0,1),type="l",xlab="Age",ylab="Survival")
lines(0:12,pSen,lty=2)
lines(0:12,pMat,lty=3)
lines(0:12,pSiler,lty=4)
######################################
