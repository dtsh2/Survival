# outDarEsSalaamMat$DIC
###############################################
rm(list=ls())
getwd()
#setwd("~/Dropbox/ali_peel_survival") # set working directory

dat <- read.csv("weightedAge.csv")
dat<-dat[1:14,1:8]
attach(dat)

#####################

# openbugs

library(R2OpenBUGS)

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
                n.iter=ni,debug=F,DIC=T,working.directory=getwd())
plot(outGhanaE)
hist(outGhanaE$sims.list$a2)

######################################################
# mat

####################
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
# sen

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

###############################################################
## both

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

plot(dat$Age,a*exp(-a2*dat$Age),ylab="Count",xlab="Age",type="l",ylim=c(0,300),main="Ghana")
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

points(dat$Age,dat$Ghana,bg="black",pch=21)

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
lines(density(runif(1:1000,min=0,max=1000)),col="red")
polygon(density(runif(1:1000,min=0,max=1000)), col =  "#FF000010", border = NA)

plot(density(outGhanaSiler$sims.list$a1), main="a1",xlab="")
polygon(density(outGhanaSiler$sims.list$a1), col =  "#00000010", border = NA)
lines(density((rnorm(1:1000,mean=0,sd=10))),col="red")
polygon(density((rnorm(1:1000,mean=0,sd=10))), col =  "#FF000010", border = NA)

plot(density(outGhanaSiler$sims.list$a2), main="a2",xlab="")
polygon(density(outGhanaSiler$sims.list$a2), col =  "#00000010", border = NA)
lines(density((rnorm(1:1000,mean=0,sd=10))),col="red")
polygon(density((rnorm(1:1000,mean=0,sd=10))), col =  "#FF000010", border = NA)

plot(density(outGhanaSiler$sims.list$a3), main="a3",xlab="")
polygon(density(outGhanaSiler$sims.list$a3), col =  "#00000010", border = NA)
lines(density((rnorm(1:1000,mean=0,sd=10))),,col="red")
polygon(density((rnorm(1:1000,mean=0,sd=10))), col =  "#FF000010", border = NA)

plot(density(outGhanaSiler$sims.list$b1), main="b1",xlab="")
polygon(density(outGhanaSiler$sims.list$b1), col =  "#00000010", border = NA)
lines(density((rnorm(1:1000,mean=0,sd=10))),col="red")
polygon(density((rnorm(1:1000,mean=0,sd=10))), col =  "#FF000010", border = NA)

plot(density(outGhanaSiler$sims.list$b3), main="b3",xlab="")
polygon(density(outGhanaSiler$sims.list$b3), col =  "#00000010", border = NA)
lines(density((rnorm(1:1000,mean=0,sd=10))),col="red")
polygon(density((rnorm(1:1000,mean=0,sd=10))), col =  "#FF000010", border = NA)

########################

## calculate survival lx-1/lx
## ploting
######################################

a=outGhanaE$mean$a
a2=outGhanaE$mean$a2
GhanaSConst<-a*exp(-a2*dat$Age)

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
GhanapConst=(GhanaSConst[1:13+1])/(GhanaSConst[1:13])
pSen=(SSen[1:13+1])/(SSen[1:13])
pMat=(SMat[1:13+1])/(SMat[1:13])
pSiler=(SSiler[1:13+1])/(SSiler[1:13])
par(mfrow=c(1,1))
plot(0:12,GhanapConst,ylim=c(0,1),type="l",xlab="Age",ylab="Survival",main="Ghana")
lines(0:12,pSen,lty=2)
lines(0:12,pMat,lty=3)
lines(0:12,pSiler,lty=4)
legend("bottomright",legend=c("Constant","Maturation","Senescence","Both"),
       lty=1:4,bty="n")

##################
## try SaoTome

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

# sen

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
######################################
outSaoTomeE$DIC
a=outSaoTomeE$mean$a
a2=outSaoTomeE$mean$a2

plot(0:13,a*exp(-a2*dat$Age),ylab="Count",xlab="Age",type="l",ylim=c(0,40),main= "Sao Tome")
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

points(dat$Age,dat$SaoTome,bg="black",pch=21)

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
# plotting posterior vs priors
plot(density(outSaoTomeSiler$sims.list$a), main="a",xlab="",xlim=c(0,200))
polygon(density(outSaoTomeSiler$sims.list$a), col =  "#00000010", border = NA)
lines(density(runif(1:1000,min=0,max=1000)),col="red")
polygon(density(runif(1:1000,min=0,max=1000)), col =  "#FF000010", border = NA)

plot(density(outSaoTomeSiler$sims.list$a1), main="a1",xlab="")
polygon(density(outSaoTomeSiler$sims.list$a1), col =  "#00000010", border = NA)
lines(density((rnorm(1:1000,mean=0,sd=10))),col="red")
polygon(density((rnorm(1:1000,mean=0,sd=10))), col =  "#FF000010", border = NA)

plot(density(outSaoTomeSiler$sims.list$a2), main="a2",xlab="")
polygon(density(outSaoTomeSiler$sims.list$a2), col =  "#00000010", border = NA)
lines(density((rnorm(1:1000,mean=0,sd=10))),col="red")
polygon(density((rnorm(1:1000,mean=0,sd=10))), col =  "#FF000010", border = NA)

plot(density(outSaoTomeSiler$sims.list$a3), main="a3",xlab="")
polygon(density(outSaoTomeSiler$sims.list$a3), col =  "#00000010", border = NA)
lines(density((rnorm(1:1000,mean=0,sd=10))),,col="red")
polygon(density((rnorm(1:1000,mean=0,sd=10))), col =  "#FF000010", border = NA)

plot(density(outSaoTomeSiler$sims.list$b1), main="b1",xlab="")
polygon(density(outSaoTomeSiler$sims.list$b1), col =  "#00000010", border = NA)
lines(density((rnorm(1:1000,mean=0,sd=10))),col="red")
polygon(density((rnorm(1:1000,mean=0,sd=10))), col =  "#FF000010", border = NA)

plot(density(outSaoTomeSiler$sims.list$b3), main="b3",xlab="")
polygon(density(outSaoTomeSiler$sims.list$b3), col =  "#00000010", border = NA)
lines(density((rnorm(1:1000,mean=0,sd=10))),col="red")
polygon(density((rnorm(1:1000,mean=0,sd=10))), col =  "#FF000010", border = NA)

###############################

## calculate survival lx-1/lx
## ploting
######################################

a=outSaoTomeE$mean$a
a2=outSaoTomeE$mean$a2
SaoTomeSConst<-a*exp(-a2*dat$Age)

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
SaoTomepConst=(SaoTomeSConst[1:13+1])/(SaoTomeSConst[1:13])
pSen=(SSen[1:13+1])/(SSen[1:13])
pMat=(SMat[1:13+1])/(SMat[1:13])
pSiler=(SSiler[1:13+1])/(SSiler[1:13])
plot(0:12,SaoTomepConst,ylim=c(0,1),type="l",xlab="Age",ylab="Survival",main="Sao Tome")
lines(0:12,pSen,lty=2)
lines(0:12,pMat,lty=3)
lines(0:12,pSiler,lty=4)
######################################
## Principe

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

######################################
outPrincipeE$DIC
a=outPrincipeE$mean$a
a2=outPrincipeE$mean$a2

plot(0:13,a*exp(-a2*dat$Age),ylab="Count",xlab="Age",type="l",ylim=c(0,20),main="Principe")
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

points(dat$Age,dat$Principe,bg="black",pch=21)

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
PrincipeSConst<-a*exp(-a2*dat$Age)

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
PrincipepConst=(PrincipeSConst[1:13+1])/(PrincipeSConst[1:13])
pSen=(SSen[1:13+1])/(SSen[1:13])
pMat=(SMat[1:13+1])/(SMat[1:13])
pSiler=(SSiler[1:13+1])/(SSiler[1:13])
plot(0:12,PrincipepConst,ylim=c(0,1),type="l",xlab="Age",ylab="Survival",main="Principe")
lines(0:12,pSen,lty=2)
lines(0:12,pMat,lty=3)
lines(0:12,pSiler,lty=4)
######################################
#########################################################################################################################################

## Morogoro

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


######################################
outMorogoroE$DIC
a=outMorogoroE$mean$a
a2=outMorogoroE$mean$a2

plot(0:13,a*exp(-a2*dat$Age),ylab="Count",xlab="Age",type="l",ylim=c(0,50),main="Morogoro")
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

lines(0:13,mean(outMorogoroSen$sims.list$a)*exp(-mean(outMorogoroSen$sims.list$a2)*dat$Age)*exp((-mean(outMorogoroSen$sims.list$a3)/mean(outMorogoroSen$sims.list$b3))*(1-exp(mean(outMorogoroSen$sims.list$b3)*dat$Age))),lty=2)
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
MorogoroSConst<-a*exp(-a2*dat$Age)

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
MorogoropConst=(MorogoroSConst[1:13+1])/(MorogoroSConst[1:13])
pSen=(SSen[1:13+1])/(SSen[1:13])
pMat=(SMat[1:13+1])/(SMat[1:13])
pSiler=(SSiler[1:13+1])/(SSiler[1:13])
plot(0:12,MorogoropConst,ylim=c(0,1),type="l",xlab="Age",ylab="Survival",main="Morogoro")
lines(0:12,pSen,lty=2)
lines(0:12,pMat,lty=3)
lines(0:12,pSiler,lty=4)
######################################

## DarEsSalaam

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

######################################
outDarEsSalaamE$DIC
a=outDarEsSalaamE$mean$a
a2=outDarEsSalaamE$mean$a2

plot(0:13,a*exp(-a2*dat$Age),ylab="Count",xlab="Age",type="l",ylim=c(0,30),main="Dar Es Salaam")
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
DarEsSalaamSConst<-a*exp(-a2*dat$Age)

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
DarEsSalaampConst=(DarEsSalaamSConst[1:13+1])/(DarEsSalaamSConst[1:13])
pSen=(SSen[1:13+1])/(SSen[1:13])
pMat=(SMat[1:13+1])/(SMat[1:13])
pSiler=(SSiler[1:13+1])/(SSiler[1:13])
plot(0:12,DarEsSalaampConst,ylim=c(0,1),type="l",xlab="Age",ylab="Survival",main="Dar Es Salaam")
lines(0:12,pSen,lty=2)
lines(0:12,pMat,lty=3)
lines(0:12,pSiler,lty=4)
######################################

##########################################################
#

## plot expectation
## survival vs population size
## additive 0.9 constant across N
## compensatory declines across N

plot(c(0,100000), c(0,1), ylab = "Survival", xlab = "Population size",type="n",xaxt="n")
## the x- and y-axis, and an integer grid
lines(x=c(0,100000),y=c(0.8,0.8),lty=2,col=1)
lines(x=c(0,100000),y=c(0.8,0),lty=2,col=2)
mtext("High",side=1,at=100000)
mtext("Low",side=1,at=0)
legend("left",legend=c("Additive","Compensatory"),
       lty=c(2,2),bty="n",col=1:2)
## add data
points(x=c(100000),y=c(GhanapConst[1]),pch=1)
points(x=c(10000),y=c(MorogoropConst[1]),pch=2)
points(x=c(5000),y=c(SaoTomepConst[1]),pch=3)
points(x=c(20000),y=c(PrincipepConst[1]),pch=4)
points(x=c(6000),y=c(DarEsSalaampConst[1]),pch=5)

legend("bottomleft",legend=c("Ghana","Morogoro","Sao Tome","Principe","Dar Es Salaam"),
       pch=1:5,bty="n")

## survival vs harvest
## additive decline
## conpensatory - const till xs

plot(c(0,1), c(0,1), ylab = "Survival", xlab = "Harvest",type="n",xaxt="n")
## the x- and y-axis, and an integer grid
lines(x=c(0,1),y=c(0.8,0),lty=2,col=1)
lines(x=c(0,0.5),y=c(0.8,0.8),lty=2,col=2)
lines(x=c(0.5,1),y=c(0.8,0.5),lty=2,col=2)
mtext("High",side=1,at=1)
mtext("Low",side=1,at=0)
legend("left",legend=c("Additive","Compensatory"),
       lty=c(2,2),bty="n",col=1:2)

## add data
points(x=c(0.5),y=c(GhanapConst[1]),pch=1)
points(x=c(0),y=c(MorogoropConst[1]),pch=2)
points(x=c(1),y=c(SaoTomepConst[1]),pch=3)
points(x=c(0),y=c(PrincipepConst[1]),pch=4)
points(x=c(0),y=c(DarEsSalaampConst[1]),pch=5)

legend("bottomleft",legend=c("Ghana","Morogoro","Sao Tome","Principe","Dar Es Salaam"),
       pch=1:5,bty="n")
