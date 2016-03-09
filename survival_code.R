###############################################
rm(list=ls())
## read data
## MUST RUN 
## run 'initial_cond.R' r code first to generate values
## May need to re-run if chooses wrong init value
dat <- read.csv("weightedAge.csv")
#dat<-dat[1:14,1:8]
attach(dat)
## must detach at end of run - see end code

pdf("age_data.pdf",height=6,width=8)
barplot(as.matrix(dat[3:8]), main="", ylab="Frequency",xlab='Age', beside=TRUE, 
        col='grey')
dev.off()
#####################

# openbugs

library(R2OpenBUGS)

nc=3 # # chains
ni=10000 # # its
nb=1000 # # burnin
nt=1 # thinning

# paramets

paramsC<-c("a","a2","data.var")
paramsM<-c("a","a1","a2","b1","data.var")
paramsS<-c("a","a3","a2","b3","data.var")
paramsB<-c("a","a1","a2","a3","b1","b3","data.var")
paramsMS<-c("a","a1","a3","b1","b3","data.var")

# choose country
country = Accra # Allpops/Bioko/Accra/Principe/SaoTome/DarEsSalaam/Morogoro
file_name = "Accra"

# read initial conditions

initC<-constant_init$Accra
initS<-sen_init$Accra
initM<-mat_init$Accra
initB<-both_init$Accra

# data
win.data<-list(data=country,Age=Age,nobs=length(country))

##########################################
## run code for the country
########################################
# constant survival

inits<-function()
  initC
outC<-bugs(data=win.data,inits=inits,parameters.to.save=paramsC,
                  model.file="allmodel.txt",n.thin=nt,n.chains=nc,n.burnin=nb,
                  n.iter=ni,debug=T,DIC=T,working.directory=getwd())
# plot(out)
# hist(out$sims.list$a2)
write.table(x=outC$summary,file=paste(file_name,'Const.csv',sep=""))

######################################################
# maturation

inits<-function()
  initM

outM<-bugs(data=win.data,inits=inits,parameters.to.save=paramsM,
                    model.file="allmodelMature.txt",n.thin=nt,n.chains=nc,n.burnin=nb,
                    n.iter=ni,debug=T,DIC=T,working.directory=getwd())
write.table(x=outM$summary,file=paste(file_name,'Mat.csv',sep=""))

################################################################
# senescence

inits<-function()
  initS

outS<-bugs(data=win.data,inits=inits,parameters.to.save=paramsS,
                    model.file="allmodelSen.txt",n.thin=nt,n.chains=nc,n.burnin=nb,
                    n.iter=ni,debug=T,DIC=T,working.directory=getwd())
write.table(x=outS$summary,file=paste(file_name,'Sen.csv',sep=""))

###############################################################
## both

inits<-function()
  initB

outB<-bugs(data=win.data,inits=inits,parameters.to.save=paramsB,
                      model.file="allmodelSiler.txt",n.thin=nt,n.chains=nc,n.burnin=nb,
                      n.iter=ni,debug=T,DIC=T,working.directory=getwd())
write.table(x=outB$summary,file=paste(file_name,'Both.csv',sep=""))

###############################################################
## both

inits<-function()
  initMS

outMS<-bugs(data=win.data,inits=inits,parameters.to.save=paramsMS,
           model.file="allmodelMatSen.txt",n.thin=nt,n.chains=nc,n.burnin=nb,
           n.iter=ni,debug=T,DIC=T,working.directory=getwd())
write.table(x=outMS$summary,file=paste(file_name,'MatSen.csv',sep=""))

## const with declining population for sens analysis

inits<-function()
  initC
outCr<-bugs(data=win.data,inits=inits,parameters.to.save=paramsC,
           model.file="allmodelr.txt",n.thin=nt,n.chains=nc,n.burnin=nb,
           n.iter=ni,debug=T,DIC=T,working.directory=getwd())

write.table(x=outCr$summary,file=paste(file_name,'Constr.csv',sep=""))

##############################################################
## models and predictions for plotting
##############################################################
## const
const<-outC$mean$a*exp(-outC$mean$a2*Age)

predictionsC<-array(dim=c(length(Age),length(outC$sims.list$a)))
for (i in 1:length(Age)){
  predictionsC[i,]<-outC$sims.list$a*exp(-outC$sims.list$a2*Age[i])
}
LPBc<-apply(predictionsC,1,quantile,probs=0.025)
UPBc<-apply(predictionsC,1,quantile,probs=0.975)

## const r

constr<-outCr$mean$a*exp(-outCr$mean$a2*Age)

predictionsCr<-array(dim=c(length(Age),length(outCr$sims.list$a)))
for (i in 1:length(Age)){
  predictionsCr[i,]<-outCr$sims.list$a*exp(-outCr$sims.list$a2*Age[i])
}
LPBcr<-apply(predictionsCr,1,quantile,probs=0.025)
UPBcr<-apply(predictionsCr,1,quantile,probs=0.975)


## sen
sen<-outS$mean$a*exp(-outS$mean$a2*Age)*exp((-outS$mean$a3/outS$mean$b3)*(1-exp(outS$mean$b3*Age)))

predictionsS<-array(dim=c(length(Age),length(outS$sims.list$a)))
for (i in 1:length(Age)){
  predictionsS[i,]<-outS$sims.list$a*exp(-outS$sims.list$a2*Age[i])*
    exp((-outS$sims.list$a3/outS$sims.list$b3)*
         (1-exp(outS$sims.list$b3*Age[i])))
}
LPBs<-apply(predictionsS,1,quantile,probs=0.025)
UPBs<-apply(predictionsS,1,quantile,probs=0.975)

## mat
mat<-outM$mean$a*exp(-outM$mean$a2*Age)*exp((-outM$mean$a1/outM$mean$b1)*(1-exp(-outM$mean$b1*Age)))

predictionsM<-array(dim=c(length(Age),length(outM$sims.list$a)))
for (i in 1:length(Age)){
  predictionsM[i,]<-outM$sims.list$a*exp(-outM$sims.list$a2*Age[i])*exp((-outM$sims.list$a1/outM$sims.list$b1)*(1-exp(-outM$sims.list$b1*dat$Age[i])))
}
LPBm<-apply(predictionsM,1,quantile,probs=0.025)
UPBm<-apply(predictionsM,1,quantile,probs=0.975)

## both/Siler
both<-outB$mean$a*exp(-outB$mean$a2*dat$Age)*exp((-outB$mean$a3/outB$mean$b3)*(1-exp(outB$mean$b3*dat$Age)))*exp((-outB$mean$a1/outB$mean$b1)*(1-exp(-outB$mean$b1*dat$Age)))
predictionsB<-array(dim=c(length(Age),length(outB$sims.list$a)))
for (i in 1:length(Age)){
  predictionsB[i,]<-outB$sims.list$a*exp(-outB$sims.list$a2*Age[i])*exp((-outB$sims.list$a3/outB$sims.list$b3)*(1-exp(outB$sims.list$b3*dat$Age[i])))*exp((-outB$sims.list$a1/outB$sims.list$b1)*(1-exp(-outB$sims.list$b1*Age[i])))
}
LPBb<-apply(predictionsB,1,quantile,probs=0.025)
UPBb<-apply(predictionsB,1,quantile,probs=0.975)

## Mat _ Sen
mat_sen<-outMS$mean$a*exp((-outMS$mean$a3/outMS$mean$b3)*(1-exp(outMS$mean$b3*dat$Age)))*exp((-outMS$mean$a1/outMS$mean$b1)*(1-exp(-outMS$mean$b1*dat$Age)))
predictionsMS<-array(dim=c(length(Age),length(outMS$sims.list$a)))
for (i in 1:length(Age)){
  predictionsMS[i,]<-outMS$sims.list$a*exp((-outMS$sims.list$a3/outMS$sims.list$b3)*(1-exp(outMS$sims.list$b3*dat$Age[i])))*exp((-outMS$sims.list$a1/outMS$sims.list$b1)*(1-exp(-outMS$sims.list$b1*Age[i])))
}
LPBms<-apply(predictionsMS,1,quantile,probs=0.025)
UPBms<-apply(predictionsMS,1,quantile,probs=0.975)

## plotting

pdf(paste(file_name,'.pdf', sep=''),width=12, height=6)
#tiff(paste(file_name,'.tiff', sep=''),width=8,height=8,units='in',res=300, compression = "lzw")

plot(0:15,const,ylab="Frequency",xlab="Age",type="l",ylim=c(0,max(country,na.rm=T)+20),main=file_name)
## plot CI
points(0:15,LPBc,type="l",col="grey")
points(0:15,UPBc,type="l",col="grey") # check 1 or 0 start
polygon(c(rev(Age), Age), c(rev(LPBc),UPBc), col =  "#00000010", border = NA)

#####

lines(0:15,sen,lty=2)
points(0:15,LPBs,type="l",col="grey",lty=2)
points(0:15,UPBs,type="l",col="grey",lty=2) # check 1 or 0 start
polygon(c(rev(Age), Age), c(rev(LPBs),UPBs), col =  "#00000010", border = NA)

lines(0:15,mat,lty=3)
points(0:15,LPBm,type="l",col="grey",lty=3)
points(0:15,UPBm,type="l",col="grey",lty=3) # check 1 or 0 start
polygon(c(rev(Age), Age), c(rev(LPBm),UPBm), col =  "#00000010", border = NA)

lines(0:15,both,lty=4)
points(0:15,LPBb,type="l",col="grey",lty=4)
points(0:15,UPBb,type="l",col="grey",lty=4) # check 1 or 0 start
polygon(c(rev(Age), Age), c(rev(LPBb),UPBb), col =  "#00000010", border = NA)

lines(0:15,mat_sen,lty=5)
points(0:15,LPBms,type="l",col="grey",lty=5)
points(0:15,UPBms,type="l",col="grey",lty=5) # check 1 or 0 start
polygon(c(rev(Age), Age), c(rev(LPBms),UPBms), col =  "#00000010", border = NA)

legend("topright",legend=c("Constant","Maturation","Senescence","Siler",'Maturation-Senescence'),
       lty=1:5,bty="n")
points(Age,country,bg="black",pch=21)

dev.off()

## r vs constant pop size
pdf(paste(file_name,'dec_stable.pdf', sep=''),width=12, height=6)

#tiff(paste(file_name,'dec_stable.tiff', sep=''),width=8,height=8,units='in',res=300, compression = "lzw")

plot(0:15,const,ylab="Frequency",xlab="Age",type="l",ylim=c(0,max(country,na.rm=T)+20),main=file_name)
## plot CI
points(0:15,LPBc,type="l",col="grey")
points(0:15,UPBc,type="l",col="grey") # check 1 or 0 start
polygon(c(rev(Age), Age), c(rev(LPBc),UPBc), col =  "#00000010", border = NA)

lines(0:15,constr,col='red')
## plot CI
points(0:15,LPBcr,type="l",col="red")
points(0:15,UPBcr,type="l",col="red") # check 1 or 0 start
polygon(c(rev(Age), Age), c(rev(LPBcr),UPBcr), col =  rgb(1,0,0,alpha=0.1) , border = NA)

legend("topright",legend=c("Constant, r = 0","Constant, r = -0.05"),
       lty=1,bty="n",col=c("black",rgb(1,0,0,alpha=0.5)))
points(Age,country,bg="black",pch=21)

dev.off()

#####

## DIC model selection

x=cbind(outC$DIC,outM$DIC,outS$DIC,outB$DIC,outMS$DIC)
colnames(x)<-c("C",'M',"S","B","MS");rownames(x)<-"DIC"
write.table(x,file=paste(file_name,'DIC.csv',sep=""))

min=min(outC$DIC,outM$DIC,outS$DIC,outB$DIC,outMS$DIC)

pdf(paste(file_name,'dic.pdf', sep=''),width=6,height=8)
#tiff(paste(file_name,'dic.tiff', sep=''),width=8,height=8,units='in',res=300, compression = "lzw")
barplot(c(outC$DIC-min,outM$DIC-min,outS$DIC-min,outB$DIC-min),
        names.arg=c("Constant","Maturation","Senescence","Both","Mat-Sen"))
dev.off()

# pars distributions

# const
pdf(paste(file_name,'pars_const.pdf', sep=''),width=12,height=4)
#tiff(paste(file_name,'pars_const.tiff', sep=''),width=8,height=3,units='in',res=300, compression = "lzw")
par(mfrow=c(1,2))
hist(outC$sims.list$a, main="a",xlab="",col="grey")
abline(v=outC$mean$a,col="red")
hist(outC$sims.list$a2, main="a2",xlab="",col="grey")
abline(v=outC$mean$a2,col="red")
dev.off()

# mat
pdf(paste(file_name,'pars_mat.pdf', sep=''),width=12,height=8)
#tiff(paste(file_name,'pars_mat.tiff', sep=''),width=8,height=6,units='in',res=300, compression = "lzw")
par(mfrow=c(2,2))
hist(outM$sims.list$a, main="a",xlab="",col="grey")
abline(v=outM$mean$a,col="red")
hist(outM$sims.list$a1, main="a1",xlab="",col="grey")
abline(v=outM$mean$a1,col="red")
hist(outM$sims.list$a2, main="a2",xlab="",col="grey")
abline(v=outM$mean$a2,col="red")
hist(outM$sims.list$b1, main="b1",xlab="",col="grey")
abline(v=outM$mean$b1,col="red")
dev.off()

# sen
pdf(paste(file_name,'pars_sen.pdf', sep=''),width=12,height=8)
#tiff(paste(file_name,'pars_sen.tiff', sep=''),width=8,height=6,units='in',res=300, compression = "lzw")
par(mfrow=c(2,2))
hist(outS$sims.list$a, main="a",xlab="",col="grey")
abline(v=outS$mean$a,col="red")
hist(outS$sims.list$a2, main="a2",xlab="",col="grey")
abline(v=outS$mean$a2,col="red")
hist(outS$sims.list$a3, main="a3",xlab="",col="grey")
abline(v=outS$mean$a3,col="red")
hist(outS$sims.list$b3, main="b3",xlab="",col="grey")
abline(v=outS$mean$b3,col="red")
dev.off()

# both
pdf(paste(file_name,'pars_siler.pdf', sep=''),width=12,height=8)
#tiff(paste(file_name,'pars_siler.tiff', sep=''),width=8,height=9,units='in',res=300, compression = "lzw")
par(mfrow=c(3,2))
hist(outB$sims.list$a, main="a",xlab="",col="grey")
abline(v=outB$mean$a,col="red")
hist(outB$sims.list$a1, main="a1",xlab="",col="grey")
abline(v=outB$mean$a1,col="red")
hist(outB$sims.list$a2, main="a2",xlab="",col="grey")
abline(v=outB$mean$a2,col="red")
hist(outB$sims.list$a3, main="a3",xlab="",col="grey")
abline(v=outB$mean$a3,col="red")
hist(outB$sims.list$b1, main="b1",xlab="",col="grey")
abline(v=outB$mean$b1,col="red")
hist(outB$sims.list$b3, main="b3",xlab="",col="grey")
abline(v=outB$mean$b3,col="red")
dev.off()

# mat - sen
tiff(paste(file_name,'pars_mat_sen.tiff', sep=''),width=8,height=9,units='in',res=300, compression = "lzw")
par(mfrow=c(3,2))
hist(outMS$sims.list$a, main="a",xlab="",col="grey")
abline(v=outMS$mean$a,col="red")
hist(outMS$sims.list$a1, main="a1",xlab="",col="grey")
abline(v=outMS$mean$a1,col="red")
hist(outMS$sims.list$a3, main="a3",xlab="",col="grey")
abline(v=outMS$mean$a3,col="red")
hist(outMS$sims.list$b1, main="b1",xlab="",col="grey")
abline(v=outMS$mean$b1,col="red")
hist(outMS$sims.list$b3, main="b3",xlab="",col="grey")
abline(v=outMS$mean$b3,col="red")
dev.off()

# plotting posterior vs priors

pdf(paste(file_name,'siler_post_pri.pdf', sep=''),width=12,height=8)
#tiff(paste(file_name,'siler_post_pri.tiff', sep=''),width=8,height=9,units='in',res=300, compression = "lzw")
par(mfrow=c(3,2))
plot(density(outB$sims.list$a), main="a",xlab="")
polygon(density(outB$sims.list$a), col =  "#00000010", border = NA)
lines(density(runif(1:1000,min=0,max=1000)),col="red")
polygon(density(runif(1:1000,min=0,max=1000)), col =  "#FF000010", border = NA)

plot(density(outB$sims.list$a1), main="a1",xlab="")
polygon(density(outB$sims.list$a1), col =  "#00000010", border = NA)
lines(density((rnorm(1:1000,mean=0,sd=10))),col="red")
polygon(density((rnorm(1:1000,mean=0,sd=10))), col =  "#FF000010", border = NA)

plot(density(outB$sims.list$a2), main="a2",xlab="")
polygon(density(outB$sims.list$a2), col =  "#00000010", border = NA)
lines(density((rnorm(1:1000,mean=0,sd=10))),col="red")
polygon(density((rnorm(1:1000,mean=0,sd=10))), col =  "#FF000010", border = NA)

plot(density(outB$sims.list$a3), main="a3",xlab="")
polygon(density(outB$sims.list$a3), col =  "#00000010", border = NA)
lines(density((rnorm(1:1000,mean=0,sd=10))),col="red")
polygon(density((rnorm(1:1000,mean=0,sd=10))), col =  "#FF000010", border = NA)

plot(density(outB$sims.list$b1), main="b1",xlab="")
polygon(density(outB$sims.list$b1), col =  "#00000010", border = NA)
lines(density((rnorm(1:1000,mean=0,sd=10))),col="red")
polygon(density((rnorm(1:1000,mean=0,sd=10))), col =  "#FF000010", border = NA)

plot(density(outB$sims.list$b3), main="b3",xlab="")
polygon(density(outB$sims.list$b3), col =  "#00000010", border = NA)
lines(density((rnorm(1:1000,mean=0,sd=10))),col="red")
polygon(density((rnorm(1:1000,mean=0,sd=10))), col =  "#FF000010", border = NA)
dev.off()

## calculate survival lx-1/lx
######################################
# const
constd=const[1:15]-const[1:15+1]
constq<-constd/const[1:15]
constp<-1-constq
constP<-numeric(length(constp))
constP[1]<-1
for (i in 2:length(constP)){
constP[i]<-constp[i-1]*constP[i-1]
}

n <- length(const)
length(constd)<-n
length(constq)<-n
length(constp)<-n
length(constP)<-n

write.table(x=cbind(const,constd,constq,constp,constP),file=paste(file_name,'const_life_table.csv',sep=""))

## mat
matd=mat[1:15]-mat[1:15+1]
matq<-matd/mat[1:15]
matp<-1-matq
matP<-numeric(length(matp))
matP[1]<-1
for (i in 2:length(matP)){
  matP[i]<-matp[i-1]*matP[i-1]
}

n <- length(mat)
length(matd)<-n
length(matq)<-n
length(matp)<-n
length(matP)<-n

write.table(x=cbind(mat,matd,matq,matp,matP),file=paste(file_name,'mat_life_table.csv',sep=""))

## sen
send=sen[1:15]-sen[1:15+1]
senq<-send/sen[1:15]
senp<-1-senq
senP<-numeric(length(senp))
senP[1]<-1
for (i in 2:length(senP)){
  senP[i]<-senp[i-1]*senP[i-1]
}

n <- length(sen)
length(send)<-n
length(senq)<-n
length(senp)<-n
length(senP)<-n

write.table(x=cbind(sen,send,senq,senp,senP),file=paste(file_name,'sen_life_table.csv',sep=""))

## both
bothd=both[1:15]-both[1:15+1]
bothq<-bothd/both[1:15]
bothp<-1-bothq
bothP<-numeric(length(bothp))
bothP[1]<-1
for (i in 2:length(bothP)){
  bothP[i]<-bothp[i-1]*bothP[i-1]
}

n <- length(both)
length(bothd)<-n
length(bothq)<-n
length(bothp)<-n
length(bothP)<-n

write.table(x=cbind(both,bothd,bothq,bothp,bothP),file=paste(file_name,'both_life_table.csv',sep=""))
#####
## mat_sen
msd=mat_sen[1:15]-mat_sen[1:15+1]
msq<-msd/mat_sen[1:15]
msp<-1-msq
msP<-numeric(length(msp))
msP[1]<-1
for (i in 2:length(msP)){
  msP[i]<-msp[i-1]*msP[i-1]
}

n <- length(mat_sen)
length(msd)<-n
length(msq)<-n
length(msp)<-n
length(msP)<-n

write.table(x=cbind(mat_sen,msd,msq,msp,msP),file=paste(file_name,'mat_sen_life_table.csv',sep=""))

#####

pdf(paste(file_name,'_ST.pdf', sep=''),width=6,height=6)
#tiff(paste(file_name,'_ST.tiff', sep=''),width=8,height=8,units='in',res=300, compression = "lzw")
plot(0:15,constp,ylim=c(0,1),type="l",xlab="Age",ylab="Annual survival",main=file_name)
lines(0:15,senp,lty=2)
lines(0:15,matp,lty=3)
lines(0:15,bothp,lty=4)
lines(0:15,msp,lty=5)
dev.off()
detach(dat)

########################################
### only run when all the country's data are available
## plot expectation
## survival vs population size
## additive 0.9 constant across N
## compensatory declines across N

pdf("compensationVsN.pdf",width=6,height=6)
#tiff("compensationVsN.tiff",width=8,height=8,units='in',res=300, compression = "lzw")
plot(c(0,100000), c(0,1), ylab = "Annual survival", xlab = "Population size",type="n",xaxt="n")
## the x- and y-axis, and an integer grid
lines(x=c(0,100000),y=c(0.8,0.8),lty=2,col=1)
lines(x=c(0,100000),y=c(0.8,0),lty=2,col=2)
mtext("High",side=1,at=100000)
mtext("Low",side=1,at=0)
legend("left",legend=c("Additive","Compensatory"),
       lty=c(2,2),bty="n",col=1:2)
## add data
# y<-read.table(paste(file_name,"const_life_table.csv",sep=""))
# OR
yG <-read.table("Accraconst_life_table.csv")
yM <-read.table("Morogoroconst_life_table.csv")
yS <-read.table("SaoTomeconst_life_table.csv")
yP <-read.table("Principeconst_life_table.csv")
yD <-read.table("DarEsSalaamconst_life_table.csv")

### list countries
points(x=c(100000),y=yG$constp[1],pch=1)
points(x=c(10000),y=yM$constp[1],pch=2)
points(x=c(5000),y=yS$constp[1],pch=3)
points(x=c(20000),y=yP$constp[1],pch=4)
points(x=c(6000),y=yD$constp[1],pch=5)

legend("bottomleft",legend=c("Accra","Morogoro","Sao Tome","Principe","Dar Es Salaam"),
       pch=1:5,bty="n")
dev.off()

## survival vs harvest
## additive decline
## conpensatory - const till xs
pdf("compensationVsHarvest.pdf",width=6,height=6)
#tiff("compensationVsHarvest.tiff",width=8,height=8,units='in',res=300, compression = "lzw")
plot(c(0,1), c(0,1), ylab = "Annual survival", xlab = "Harvest",type="n",xaxt="n")
## the x- and y-axis, and an integer grid
lines(x=c(0,1),y=c(0.8,0),lty=2,col=1)
lines(x=c(0,0.5),y=c(0.8,0.8),lty=2,col=2)
lines(x=c(0.5,1),y=c(0.8,0.5),lty=2,col=2)
mtext("High",side=1,at=1)
mtext("Low",side=1,at=0)
legend("left",legend=c("Additive","Compensatory"),
       lty=c(2,2),bty="n",col=1:2)

## add data
points(x=c(0.5),y=yG$constp[1],pch=1)
points(x=c(0),y=yM$constp[1],pch=2)
points(x=c(1),y=yS$constp[1],pch=3)
points(x=c(0),y=yP$constp[1],pch=4)
points(x=c(0),y=yD$constp[1],pch=5)

legend("bottomleft",legend=c("Accra","Morogoro","Sao Tome","Principe","Dar Es Salaam"),
       pch=1:5,bty="n")
dev.off()

const_s_regression<-c(
yG$constp[1],
yM$constp[1],
yS$constp[1],
yP$constp[1],
yD$constp[1])
cres<-as.data.frame(const_s_regression)
rownames(cres)<-c("Accra","Morogoro","SaoTome","Principe","DarEsSalaam")
write.csv(cres,"constSreg_data.csv")
