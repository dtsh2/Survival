PrincipepSiler=(PrincipeSSiler[1:15+1])/(PrincipeSSiler[1:15])
tiff("PrincipeS.tiff",width=8,height=8,units='in',res=300, compression = "lzw")
plot(0:14,PrincipepConst,ylim=c(0,1),type="l",xlab="Age",ylab="Survival",main="Principe")
lines(0:14,PrincipepSen,lty=2)
lines(0:14,PrincipepMat,lty=3)
lines(0:14,PrincipepSiler,lty=4)
dev.off()
##########################################
## run code for Morogoro
########################################
# const
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
# sen
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
######################################################################
# plo
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
######################################################################
# plotting
#######################################################################
tiff("Morogoro.tiff",width=8,height=8,units='in',res=300, compression = "lzw")
plot(0:15,outMorogoroE$mean$a*exp(-outMorogoroE$mean$a2*dat$Age),ylab="Count",xlab="Age",type="l",ylim=c(0,50),main="Morogoro")
## plot CI
predictions<-array(dim=c(length(dat$Age),length(outMorogoroE$sims.list$a)))
for (i in 1:length(dat$Age)){
predictions[i,]<-outMorogoroE$sims.list$a*exp(-outMorogoroE$sims.list$a2*dat$Age[i])
}
LPB<-apply(predictions,1,quantile,probs=0.025)
UPB<-apply(predictions,1,quantile,probs=0.975)
points(0:15,LPB,type="l",col="grey")
points(0:15,UPB,type="l",col="grey") # check 1 or 0 start
polygon(c(rev(dat$Age), dat$Age), c(rev(LPB),UPB), col =  "#00000010", border = NA)
lines(0:15,mean(outMorogoroSen$sims.list$a)*exp(-mean(outMorogoroSen$sims.list$a2)*dat$Age)*exp((-mean(outMorogoroSen$sims.list$a3)/mean(outMorogoroSen$sims.list$b3))*(1-exp(mean(outMorogoroSen$sims.list$b3)*dat$Age))),lty=2)
## plot CI
predictions<-array(dim=c(length(dat$Age),length(outMorogoroSen$sims.list$a)))
for (i in 1:length(dat$Age)){
predictions[i,]<-outMorogoroSen$sims.list$a*exp(-outMorogoroSen$sims.list$a2*dat$Age[i])*exp((-outMorogoroSen$sims.list$a3/outMorogoroSen$sims.list$b3)*(1-exp(outMorogoroSen$sims.list$b3*dat$Age[i])))
}
LPB<-apply(predictions,1,quantile,probs=0.025)
UPB<-apply(predictions,1,quantile,probs=0.975)
points(0:15,LPB,type="l",col="grey",lty=2)
points(0:15,UPB,type="l",col="grey",lty=2) # check 1 or 0 start
polygon(c(rev(dat$Age), dat$Age), c(rev(LPB),UPB), col =  "#00000010", border = NA)
lines(0:15,outMorogoroMat$mean$a*exp(-outMorogoroMat$mean$a2*dat$Age)*exp((-outMorogoroMat$mean$a1/outMorogoroMat$mean$b1)*(1-exp(-outMorogoroMat$mean$b1*dat$Age))),lty=3)
predictions<-array(dim=c(length(dat$Age),length(outMorogoroMat$sims.list$a)))
for (i in 1:length(dat$Age)){
predictions[i,]<-outMorogoroMat$sims.list$a*exp(-outMorogoroMat$sims.list$a2*dat$Age[i])*exp((-outMorogoroMat$sims.list$a1/outMorogoroMat$sims.list$b1)*(1-exp(-outMorogoroMat$sims.list$b1*dat$Age[i])))
}
LPB<-apply(predictions,1,quantile,probs=0.025)
UPB<-apply(predictions,1,quantile,probs=0.975)
points(0:15,LPB,type="l",col="grey",lty=3)
points(0:15,UPB,type="l",col="grey",lty=3) # check 1 or 0 start
polygon(c(rev(dat$Age), dat$Age), c(rev(LPB),UPB), col =  "#00000010", border = NA)
lines(0:15,outMorogoroSiler$mean$a*exp(-outMorogoroSiler$mean$a2*dat$Age)*exp((-outMorogoroSiler$mean$a3/outMorogoroSiler$mean$b3)*(1-exp(outMorogoroSiler$mean$b3*dat$Age)))*exp((-outMorogoroSiler$mean$a1/outMorogoroSiler$mean$b1)*(1-exp(-outMorogoroSiler$mean$b1*dat$Age))),lty=4)
predictions<-array(dim=c(length(dat$Age),length(outMorogoroSiler$sims.list$a)))
for (i in 1:length(dat$Age)){
predictions[i,]<-outMorogoroSiler$sims.list$a*exp(-outMorogoroSiler$sims.list$a2*dat$Age[i])*exp((-outMorogoroSiler$sims.list$a3/outMorogoroSiler$sims.list$b3)*(1-exp(outMorogoroSiler$sims.list$b3*dat$Age[i])))*exp((-outMorogoroSiler$sims.list$a1/outMorogoroSiler$sims.list$b1)*(1-exp(-outMorogoroSiler$sims.list$b1*dat$Age[i])))
}
LPB<-apply(predictions,1,quantile,probs=0.025)
UPB<-apply(predictions,1,quantile,probs=0.975)
points(0:15,LPB,type="l",col="grey",lty=4)
points(0:15,UPB,type="l",col="grey",lty=4) # check 1 or 0 start
polygon(c(rev(dat$Age), dat$Age), c(rev(LPB),UPB), col =  "#00000010", border = NA)
legend("topright",legend=c("Constant","Maturation","Senescence","Both"),
lty=1:4,bty="n")
points(dat$Age,Morogoro,bg="black",pch=21)
dev.off()
min=min(outMorogoroE$DIC,outMorogoroMat$DIC,outMorogoroSen$DIC,outMorogoroSiler$DIC)
tiff("MorogoroDIC.tiff",width=8,height=8,units='in',res=300, compression = "lzw")
barplot(c(outMorogoroE$DIC-min,outMorogoroMat$DIC-min,outMorogoroSen$DIC-min,outMorogoroSiler$DIC-min),
names.arg=c("Constant","Maturation","Senescence","Both"))
dev.off()
tiff("MorogoroSilerParDist.tiff",width=8,height=8,units='in',res=300, compression = "lzw")
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
dev.off()
###############################################################################
## calculate survival lx-1/lx
par(mfrow=c(1,1))
######################################
MorogoroSConst<-outMorogoroE$mean$a*exp(-outMorogoroE$mean$a2*dat$Age)
MorogoroSSen<-outMorogoroSen$mean$a*exp(-outMorogoroSen$mean$a2*dat$Age)*exp((-outMorogoroSen$mean$a3/outMorogoroSen$mean$b3)*(1-exp(outMorogoroSen$mean$b3*dat$Age)))
MorogoroSMat<-outMorogoroMat$mean$a*exp(-outMorogoroMat$mean$a2*dat$Age)*exp((-outMorogoroMat$mean$a1/outMorogoroMat$mean$b1)*(1-exp(-outMorogoroMat$mean$b1*dat$Age)))
MorogoroSSiler<-outMorogoroSiler$mean$a*exp(-outMorogoroSiler$mean$a2*dat$Age)*exp((-outMorogoroSiler$mean$a3/outMorogoroSiler$mean$b3)*(1-exp(outMorogoroSiler$mean$b3*dat$Age)))*exp((-outMorogoroSiler$mean$a1/outMorogoroSiler$mean$b1)*(1-exp(-outMorogoroSiler$mean$b1*dat$Age)))
############################
MorogoropConst=(MorogoroSConst[1:15+1])/(MorogoroSConst[1:15])
MorogoropSen=(MorogoroSSen[1:15+1])/(MorogoroSSen[1:15])
MorogoropMat=(MorogoroSMat[1:15+1])/(MorogoroSMat[1:15])
MorogoropSiler=(MorogoroSSiler[1:15+1])/(MorogoroSSiler[1:15])
tiff("MorogoroS.tiff",width=8,height=8,units='in',res=300, compression = "lzw")
plot(0:14,MorogoropConst,ylim=c(0,1),type="l",xlab="Age",ylab="Survival",main="Morogoro")
lines(0:14,MorogoropSen,lty=2)
lines(0:14,MorogoropMat,lty=3)
lines(0:14,MorogoropSiler,lty=4)
dev.off()
##########################################
##
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
# sen
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
######
# both
#####################################################################################
tiff("Dar.tiff",width=8,height=8,units='in',res=300, compression = "lzw")
plot(0:15,outDarEsSalaamE$mean$a*exp(-outDarEsSalaamE$mean$a2*dat$Age),ylab="Count",xlab="Age",type="l",ylim=c(0,30),main="Dar Es Salaam")
## plot CI
predictions<-array(dim=c(length(dat$Age),length(outDarEsSalaamE$sims.list$a)))
for (i in 1:length(dat$Age)){
predictions[i,]<-outDarEsSalaamE$sims.list$a*exp(-outDarEsSalaamE$sims.list$a2*dat$Age[i])
}
LPB<-apply(predictions,1,quantile,probs=0.025)
UPB<-apply(predictions,1,quantile,probs=0.975)
points(0:15,LPB,type="l",col="grey")
points(0:15,UPB,type="l",col="grey") # check 1 or 0 start
polygon(c(rev(dat$Age), dat$Age), c(rev(LPB),UPB), col =  "#00000010", border = NA)
lines(0:15,outDarEsSalaamSen$mean$a*exp(-outDarEsSalaamSen$mean$a2*dat$Age)*exp((-outDarEsSalaamSen$mean$a3/outDarEsSalaamSen$mean$b3)*(1-exp(outDarEsSalaamSen$mean$b3*dat$Age))),lty=2)
## plot CI
predictions<-array(dim=c(length(dat$Age),length(outDarEsSalaamSen$sims.list$a)))
for (i in 1:length(dat$Age)){
predictions[i,]<-outDarEsSalaamSen$sims.list$a*exp(-outDarEsSalaamSen$sims.list$a2*dat$Age[i])*exp((-outDarEsSalaamSen$sims.list$a3/outDarEsSalaamSen$sims.list$b3)*(1-exp(outDarEsSalaamSen$sims.list$b3*dat$Age[i])))
}
LPB<-apply(predictions,1,quantile,probs=0.025)
UPB<-apply(predictions,1,quantile,probs=0.975)
points(0:15,LPB,type="l",col="grey",lty=2)
points(0:15,UPB,type="l",col="grey",lty=2) # check 1 or 0 start
polygon(c(rev(dat$Age), dat$Age), c(rev(LPB),UPB), col =  "#00000010", border = NA)
lines(0:15,outDarEsSalaamMat$mean$a*exp(-outDarEsSalaamMat$mean$a2*dat$Age)*exp((-outDarEsSalaamMat$mean$a1/outDarEsSalaamMat$mean$b1)*(1-exp(-outDarEsSalaamMat$mean$b1*dat$Age))),lty=3)
predictions<-array(dim=c(length(dat$Age),length(outDarEsSalaamMat$sims.list$a)))
for (i in 1:length(dat$Age)){
predictions[i,]<-outDarEsSalaamMat$sims.list$a*exp(-outDarEsSalaamMat$sims.list$a2*dat$Age[i])*exp((-outDarEsSalaamMat$sims.list$a1/outDarEsSalaamMat$sims.list$b1)*(1-exp(-outDarEsSalaamMat$sims.list$b1*dat$Age[i])))
}
LPB<-apply(predictions,1,quantile,probs=0.025)
UPB<-apply(predictions,1,quantile,probs=0.975)
points(0:15,LPB,type="l",col="grey",lty=3)
points(0:15,UPB,type="l",col="grey",lty=3) # check 1 or 0 start
polygon(c(rev(dat$Age), dat$Age), c(rev(LPB),UPB), col =  "#00000010", border = NA)
lines(0:15,outDarEsSalaamSiler$mean$a*exp(-outDarEsSalaamSiler$mean$a2*dat$Age)*exp((-outDarEsSalaamSiler$mean$a3/outDarEsSalaamSiler$mean$b3)*(1-exp(outDarEsSalaamSiler$mean$b3*dat$Age)))*exp((-outDarEsSalaamSiler$mean$a1/outDarEsSalaamSiler$mean$b1)*(1-exp(-outDarEsSalaamSiler$mean$b1*dat$Age))),lty=4)
predictions<-array(dim=c(length(dat$Age),length(outDarEsSalaamSiler$sims.list$a)))
for (i in 1:length(dat$Age)){
predictions[i,]<-outDarEsSalaamSiler$sims.list$a*exp(-outDarEsSalaamSiler$sims.list$a2*dat$Age[i])*exp((-outDarEsSalaamSiler$sims.list$a3/outDarEsSalaamSiler$sims.list$b3)*(1-exp(outDarEsSalaamSiler$sims.list$b3*dat$Age[i])))*exp((-outDarEsSalaamSiler$sims.list$a1/outDarEsSalaamSiler$sims.list$b1)*(1-exp(-outDarEsSalaamSiler$sims.list$b1*dat$Age[i])))
}
LPB<-apply(predictions,1,quantile,probs=0.025)
UPB<-apply(predictions,1,quantile,probs=0.975)
points(0:15,LPB,type="l",col="grey",lty=4)
points(0:15,UPB,type="l",col="grey",lty=4) # check 1 or 0 start
polygon(c(rev(dat$Age), dat$Age), c(rev(LPB),UPB), col =  "#00000010", border = NA)
legend("topright",legend=c("Constant","Maturation","Senescence","Both"),
lty=1:4,bty="n")
points(dat$Age,DarEsSalaam,bg="black",pch=21)
dev.off()
tiff("DarDIC.tiff",width=8,height=8,units='in',res=300, compression = "lzw")
min=min(outDarEsSalaamE$DIC,outDarEsSalaamMat$DIC,outDarEsSalaamSen$DIC,outDarEsSalaamSiler$DIC)
barplot(c(outDarEsSalaamE$DIC-min,outDarEsSalaamMat$DIC-min,outDarEsSalaamSen$DIC-min,outDarEsSalaamSiler$DIC-min),
names.arg=c("Constant","Maturation","Senescence","Both"))
dev.off()
tiff("DarSilerParDist.tiff",width=8,height=8,units='in',res=300, compression = "lzw")
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
dev.off()
###############################################################################
## calculate survival lx-1/lx
par(mfrow=c(1,1))
######################################
DarEsSalaamSConst<-outDarEsSalaamE$mean$a*exp(-outDarEsSalaamE$mean$a2*dat$Age)
DarEsSalaamSSen<-outDarEsSalaamSen$mean$a*exp(-outDarEsSalaamSen$mean$a2*dat$Age)*exp((-outDarEsSalaamSen$mean$a3/outDarEsSalaamSen$mean$b3)*(1-exp(outDarEsSalaamSen$mean$b3*dat$Age)))
DarEsSalaamSMat<-outDarEsSalaamMat$mean$a*exp(-outDarEsSalaamMat$mean$a2*dat$Age)*exp((-outDarEsSalaamMat$mean$a1/outDarEsSalaamMat$mean$b1)*(1-exp(-outDarEsSalaamMat$mean$b1*dat$Age)))
DarEsSalaamSSiler<-outDarEsSalaamSiler$mean$a*exp(-outDarEsSalaamSiler$mean$a2*dat$Age)*exp((-outDarEsSalaamSiler$mean$a3/outDarEsSalaamSiler$mean$b3)*(1-exp(outDarEsSalaamSiler$mean$b3*dat$Age)))*exp((-outDarEsSalaamSiler$mean$a1/outDarEsSalaamSiler$mean$b1)*(1-exp(-outDarEsSalaamSiler$mean$b1*dat$Age)))
############################
DarEsSalaampConst=(DarEsSalaamSConst[1:15+1])/(DarEsSalaamSConst[1:15])
DarEsSalaampSen=(DarEsSalaamSSen[1:15+1])/(DarEsSalaamSSen[1:15])
DarEsSalaampMat=(DarEsSalaamSMat[1:15+1])/(DarEsSalaamSMat[1:15])
DarEsSalaampSiler=(DarEsSalaamSSiler[1:15+1])/(DarEsSalaamSSiler[1:15])
tiff("DarS.tiff",width=8,height=8,units='in',res=300, compression = "lzw")
plot(0:14,DarEsSalaampConst,ylim=c(0,1),type="l",xlab="Age",ylab="Survival",main="Dar Es Salaam")
lines(0:14,DarEsSalaampSen,lty=2)
lines(0:14,DarEsSalaampMat,lty=3)
lines(0:14,DarEsSalaampSiler,lty=4)
dev.off()
######################################
## plot expectation
## survival vs population size
## additive 0.9 constant across N
## compensatory declines across N
tiff("compensationVsN.tiff",width=8,height=8,units='in',res=300, compression = "lzw")
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
dev.off()
## survival vs harvest
## additive decline
## conpensatory - const till xs
tiff("compensationVsHarvest.tiff",width=8,height=8,units='in',res=300, compression = "lzw")
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
dev.off()
#### save results for tables
## const
write.table(x=outGhanaE$summary,file="GhanaConst.csv",sep=",")
write.table(x=outGhanaE$DIC,file="GhanaConstDIC.csv",sep=",")
write.table(x=outPrincipeE$summary,file="PrincipeConst.csv",sep=",")
write.table(x=outPrincipeE$DIC,file="PrincipeConstDIC.csv",sep=",")
write.table(x=outSaoTomeE$summary,file="SaoTomeConst.csv",sep=",")
write.table(x=outSaoTomeE$DIC,file="SaoTomeConstDIC.csv",sep=",")
write.table(x=outMorogoroE$summary,file="MorogoroConst.csv",sep=",")
write.table(x=outMorogoroE$DIC,file="MorogoroConstDIC.csv",sep=",")
write.table(x=outDarEsSalaamE$summary,file="DarEsSalaamConst.csv",sep=",")
write.table(x=outDarEsSalaamE$DIC,file="DarEsSalaamConstDIC.csv",sep=",")
## sen
write.table(x=outGhanaSen$summary,file="GhanaSen.csv",sep=",")
write.table(x=outGhanaSen$DIC,file="GhanaSenDIC.csv",sep=",")
write.table(x=outPrincipeSen$summary,file="PrincipeSen.csv",sep=",")
write.table(x=outPrincipeSen$DIC,file="PrincipeSenDIC.csv",sep=",")
write.table(x=outSaoTomeSen$summary,file="SaoTomeSen.csv",sep=",")
write.table(x=outSaoTomeSen$DIC,file="SaoTomeSenDIC.csv",sep=",")
write.table(x=outMorogoroSen$summary,file="MorogoroSen.csv",sep=",")
write.table(x=outMorogoroSen$DIC,file="MorogoroSenDIC.csv",sep=",")
write.table(x=outDarEsSalaamSen$summary,file="DarEsSalaamSen.csv",sep=",")
write.table(x=outDarEsSalaamSen$DIC,file="DarEsSalaamSenDIC.csv",sep=",")
write.table(x=outGhanaSen$summary,file="GhanaSen.csv",sep=",")
write.table(x=outGhanaSen$DIC,file="GhanaSenDIC.csv",sep=",")
## mat
write.table(x=outPrincipeMat$summary,file="PrincipeMat.csv",sep=",")
write.table(x=outPrincipeMat$DIC,file="PrincipeMatDIC.csv",sep=",")
write.table(x=outSaoTomeMat$summary,file="SaoTomeMat.csv",sep=",")
write.table(x=outSaoTomeMat$DIC,file="SaoTomeMatDIC.csv",sep=",")
write.table(x=outMorogoroMat$summary,file="MorogoroMat.csv",sep=",")
write.table(x=outMorogoroMat$DIC,file="MorogoroMatDIC.csv",sep=",")
write.table(x=outDarEsSalaamMat$summary,file="DarEsSalaamMat.csv",sep=",")
write.table(x=outDarEsSalaamMat$DIC,file="DarEsSalaamMatDIC.csv",sep=",")
## both
write.table(x=outGhanaSiler$summary,file="GhanaSiler.csv",sep=",")
write.table(x=outGhanaSiler$DIC,file="GhanaSilerDIC.csv",sep=",")
write.table(x=outPrincipeSiler$summary,file="PrincipeSiler.csv",sep=",")
write.table(x=outPrincipeSiler$DIC,file="PrincipeSilerDIC.csv",sep=",")
write.table(x=outSaoTomeSiler$summary,file="SaoTomeSiler.csv",sep=",")
write.table(x=outSaoTomeSiler$DIC,file="SaoTomeSilerDIC.csv",sep=",")
write.table(x=outMorogoroSiler$summary,file="MorogoroSiler.csv",sep=",")
write.table(x=outMorogoroSiler$DIC,file="MorogoroSilerDIC.csv",sep=",")
write.table(x=outDarEsSalaamSiler$summary,file="DarEsSalaamSiler.csv",sep=",")
write.table(x=outDarEsSalaamSiler$DIC,file="DarEsSalaamSilerDIC.csv",sep=",")
## constant survival rates for regression
constantS<-round(c(GhanapConst[1],PrincipepConst[1],SaoTomepConst[1],MorogoropConst[1],DarEsSalaampConst[1]),3)
constantS<-as.data.frame(constantS)
row.names(constantS)<-c("Ghana","Principe","SaoTome","Morogoro","DarEsSalaam")
write.table(x=constantS,file="constantSregression.csv",sep=",")
savehistory("~/GitHub/Survival/survival_code_Hist.R")
load("~/GitHub/Survival/results.env.RData")
hist(outSaoTomeE$sims.list$a2)
points(dat$Age,dat$SaoTome)
tiff("SaoTome.tiff",width=8,height=8,units='in',res=300, compression = "lzw")
plot(0:15,outSaoTomeE$mean$a*exp(-outSaoTomeE$mean$a2*dat$Age),ylab="Count",xlab="Age",type="l",ylim=c(0,40),main= "Sao Tome")
## plot CI
predictions<-array(dim=c(length(dat$Age),length(outSaoTomeE$sims.list$a)))
for (i in 1:length(dat$Age)){
predictions[i,]<-outSaoTomeE$sims.list$a*exp(-outSaoTomeE$sims.list$a2*dat$Age[i])
}
LPB<-apply(predictions,1,quantile,probs=0.025)
UPB<-apply(predictions,1,quantile,probs=0.975)
points(0:15,LPB,type="l",col="grey")
points(0:15,UPB,type="l",col="grey") # check 1 or 0 start
polygon(c(rev(dat$Age), dat$Age), c(rev(LPB),UPB), col =  "#00000010", border = NA)
lines(0:15,outSaoTomeSen$mean$a*exp(-outSaoTomeSen$mean$a2*dat$Age)*exp((-outSaoTomeSen$mean$a3/outSaoTomeSen$mean$b3)*(1-exp(outSaoTomeSen$mean$b3*dat$Age))),lty=2)
## plot CI
predictions<-array(dim=c(length(dat$Age),length(outSaoTomeSen$sims.list$a)))
for (i in 1:length(dat$Age)){
predictions[i,]<-outSaoTomeSen$sims.list$a*exp(-outSaoTomeSen$sims.list$a2*dat$Age[i])*exp((-outSaoTomeSen$sims.list$a3/outSaoTomeSen$sims.list$b3)*(1-exp(outSaoTomeSen$sims.list$b3*dat$Age[i])))
}
LPB<-apply(predictions,1,quantile,probs=0.025)
UPB<-apply(predictions,1,quantile,probs=0.975)
points(0:15,LPB,type="l",col="grey",lty=2)
points(0:15,UPB,type="l",col="grey",lty=2) # check 1 or 0 start
polygon(c(rev(dat$Age), dat$Age), c(rev(LPB),UPB), col =  "#00000010", border = NA)
lines(0:15,outSaoTomeMat$mean$a*exp(-outSaoTomeMat$mean$a2*dat$Age)*exp((-outSaoTomeMat$mean$a1/outSaoTomeMat$mean$b1)*(1-exp(-outSaoTomeMat$mean$b1*dat$Age))),lty=3)
predictions<-array(dim=c(length(dat$Age),length(outSaoTomeMat$sims.list$a)))
for (i in 1:length(dat$Age)){
predictions[i,]<-outSaoTomeMat$sims.list$a*exp(-outSaoTomeMat$sims.list$a2*dat$Age[i])*exp((-outSaoTomeMat$sims.list$a1/outSaoTomeMat$sims.list$b1)*(1-exp(-outSaoTomeMat$sims.list$b1*dat$Age[i])))
}
LPB<-apply(predictions,1,quantile,probs=0.025)
UPB<-apply(predictions,1,quantile,probs=0.975)
points(0:15,LPB,type="l",col="grey",lty=3)
points(0:15,UPB,type="l",col="grey",lty=3) # check 1 or 0 start
polygon(c(rev(dat$Age), dat$Age), c(rev(LPB),UPB), col =  "#00000010", border = NA)
lines(0:15,outSaoTomeSiler$mean$a*exp(-outSaoTomeSiler$mean$a2*dat$Age)*exp((-outSaoTomeSiler$mean$a3/outSaoTomeSiler$mean$b3)*(1-exp(outSaoTomeSiler$mean$b3*dat$Age)))*exp((-outSaoTomeSiler$mean$a1/outSaoTomeSiler$mean$b1)*(1-exp(-outSaoTomeSiler$mean$b1*dat$Age))),lty=4)
predictions<-array(dim=c(length(dat$Age),length(outSaoTomeSiler$sims.list$a)))
for (i in 1:length(dat$Age)){
predictions[i,]<-outSaoTomeSiler$sims.list$a*exp(-outSaoTomeSiler$sims.list$a2*dat$Age[i])*exp((-outSaoTomeSiler$sims.list$a3/outSaoTomeSiler$sims.list$b3)*(1-exp(outSaoTomeSiler$sims.list$b3*dat$Age[i])))*exp((-outSaoTomeSiler$sims.list$a1/outSaoTomeSiler$sims.list$b1)*(1-exp(-outSaoTomeSiler$sims.list$b1*dat$Age[i])))
}
LPB<-apply(predictions,1,quantile,probs=0.025)
UPB<-apply(predictions,1,quantile,probs=0.975)
points(0:15,LPB,type="l",col="grey",lty=4)
points(0:15,UPB,type="l",col="grey",lty=4) # check 1 or 0 start
polygon(c(rev(dat$Age), dat$Age), c(rev(LPB),UPB), col =  "#00000010", border = NA)
legend("topright",legend=c("Constant","Maturation","Senescence","Both"),
lty=1:4,bty="n")
points(dat$Age,dat$SaoTome,bg="black",pch=21)
dev.off()
save.image("~/GitHub/Survival/results.env.RData")
savehistory("~/GitHub/Survival/survival_code_Hist.R")
