###############################################
rm(list=ls())
## read data
dat <- read.csv("weightedAge.csv")
#dat<-dat[1:14,1:8]
attach(dat)
## must detach at end of run - see end code
# openbugs

library(R2OpenBUGS)

nc=3 # # chains
ni=10000 # # its
nb=1000 # # burnin
nt=1 # thinning

# paramets

paramsW<-c("alpha_1","alpha_2","alpha_3",
           "data.var")

# choose country
country = Accra # Allpops/Bioko/Accra/Principe/SaoTome/DarEsSalaam/Morogoro
file_name = "Accra"

# read initial conditions

names_w <-c('Accra')

w_init<-as.data.frame(cbind(
  # Accra 
list(alpha_1=rnorm(1,200),alpha_2=rnorm(1,0),alpha_3=0,
     data.sd=runif(1,0,100))))

colnames(w_init)<-names_w

initW<-w_init$Accra

# data
win.data<-list(data=country,Age=Age,nobs=length(country))

##########################################
## run code for the country
########################################
# Weibull

inits<-function()
  initW
outW<-bugs(data=win.data,inits=initW,parameters.to.save=paramsW,
           model.file="w_model.txt",n.thin=nt,n.chains=nc,n.burnin=nb,
           n.iter=ni,debug=T,DIC=T,working.directory=getwd())
 plot(outW)
 hist(outW$sims.list$beta_d)
  hist(outW$sims.list$lambda_d)

#wei<-outW$mean$beta_d*outW$mean$lambda_d*(outW$mean$lambda_d*Age)*exp(outW$mean$beta_d-1)
wei<-outW$mean$lambda_d*Age*exp(outW$mean$beta_d-1)

predictionsW<-array(dim=c(length(Age),length(outW$sims.list$beta_d)))
for (i in 1:length(Age)){
  #predictionsW[i,]<-outW$mean$beta_d*outW$mean$lambda_d*Age[i]*exp(outW$mean$beta_d-1)
  predictionsW[i,]<-outW$mean$lambda_d*Age[i]*exp(outW$mean$beta_d-1)
}
LPBw<-apply(predictionsW,1,quantile,probs=0.025)
UPBw<-apply(predictionsW,1,quantile,probs=0.975)

plot(0:15,wei,ylab="Count",xlab="Age",type="l",ylim=c(0,max(country,na.rm=T)+20),main=file_name)
## plot CI
points(0:15,LPBw,type="l",col="grey")
points(0:15,UPBw,type="l",col="grey") # check 1 or 0 start
polygon(c(rev(Age), Age), c(rev(LPBw),UPBw), col =  "#00000010", border = NA)

detach(dat)
