data_dar<-read.csv("DarEsSalaamboth.csv",sep='')
data_mor<-read.csv("Morogoroboth.csv",sep='')
data_gha<-read.csv("Accraboth.csv",sep='')
data_sao<-read.csv("SaoTomeboth.csv",sep='')
data_pri<-read.csv("Principeboth.csv",sep='')

library(psych)
my.stats <- data.frame(mean=c(data_dar[2:6,1],
                              data_mor[2:6,1],
                              data_gha[2:6,1],
                              data_sao[2:6,1],
                              data_pri[2:6,1]),
                       se=c(data_dar[2:6,2],
                            data_mor[2:6,2],
                            data_gha[2:6,2],
                            data_sao[2:6,2],
                            data_pri[2:6,2]))


ind_a1 <- seq(1, nrow(my.stats), by=5)
my.stats_a1<-my.stats[ind_a1, ]
ind_a2 <- seq(2, nrow(my.stats), by=5)
my.stats_a2<-my.stats[ind_a2, ]
ind_a3 <- seq(3, nrow(my.stats), by=5)
my.stats_a3<-my.stats[ind_a3, ]
ind_b1 <- seq(4, nrow(my.stats), by=5)
my.stats_b1<-my.stats[ind_b1, ]
ind_b3 <- seq(5, nrow(my.stats), by=5)
my.stats_b3<-my.stats[ind_b3, ]

lablist<-c("Dar Es Salaam","Morogoro","Accra","Sao Tome","Principe")
error.bars(stats=my.stats_a1,main="Siler model, a1",xaxt="n",ylab="Parameter",xlab="")
text(seq(1, 5, by=1), par("usr")[3] - 0.06, labels = lablist, srt = 45, pos = 1, xpd = TRUE)
error.bars(stats=my.stats_a2,main="Siler model, a2",xaxt="n",ylab="Parameter",xlab="")
text(seq(1, 5, by=1), par("usr")[3] - 0.06, labels = lablist, srt = 45, pos = 1, xpd = TRUE)
error.bars(stats=my.stats_a3,main="Siler model, a3",xaxt="n",ylab="Parameter",xlab="")
text(seq(1, 5, by=1), par("usr")[3] - 0.06, labels = lablist, srt = 45, pos = 1, xpd = TRUE)
error.bars(stats=my.stats_b1,main="Siler model, b1",xaxt="n",ylab="Parameter",xlab="")
text(seq(1, 5, by=1), par("usr")[3] - 0.06, labels = lablist, srt = 45, pos = 1, xpd = TRUE)
error.bars(stats=my.stats_b3,main="Siler model, b3",xaxt="n",ylab="Parameter",xlab="")
text(seq(1, 5, by=1), par("usr")[3] - 0.06, labels = lablist, srt = 45, pos = 1, xpd = TRUE)

error.bars(stats=my.stats,main="Siler model parameters",xaxt="n",ylab="Parameter estimate",xlab="",
           col=rep(1:5,5),arrow.col=rep(1:5,5))
text(seq(2.5, 25, by=5), par("usr")[3] - 0.1, labels = lablist, srt = 45, pos = 1, xpd = TRUE)
abline(v=seq(5.5, 25, by=5),lty=2,col='grey')

my.dat <- data.frame(mean=c(data_dar[2:6,1],
                              data_mor[2:6,1],
                              data_gha[2:6,1],
                              data_sao[2:6,1],
                              data_pri[2:6,1]),
                       up=c(data_dar[2:6,7],
                            data_mor[2:6,7],
                            data_gha[2:6,7],
                            data_sao[2:6,7],
                            data_pri[2:6,7]),
                       low=c(data_dar[2:6,3],
                            data_mor[2:6,3],
                            data_gha[2:6,3],
                            data_sao[2:6,3],
                            data_pri[2:6,3]))
par(mar=c(7.1, 4.1, 4.1, 4.1))
plot(my.dat$mean,ylim=c(min(my.dat),max(my.dat)+0.1),ylab='estimate',xaxt='n',xlab='',pch=rep(1:5,5))
segments(x0=1:25,x1=1:25,y0=my.dat$up,y1=my.dat$low,lty=1:5)
abline(v=seq(5.5, 25, by=5),lty=2,col='grey')
text(seq(2.5, 25, by=5), par("usr")[3] - 0.2, labels = lablist, srt = 45, pos = 1, xpd = TRUE)
legend("top", inset=c(0,-0.05), horiz=T,legend=c("a1","a2","a3","b1","b3"), pch=1:5,lty=1:5, title="",bty='y',)
box()
#dev.off()

# points by hunting pressure
plot(my.dat$mean[seq(1,25,5)],pch=c(3,3,2,1,3),main="a1")
plot(my.dat$mean[seq(2,25,5)],pch=c(3,3,2,1,3),main="a2")
plot(my.dat$mean[seq(3,25,5)],pch=c(3,3,2,1,3),main="a3")
plot(my.dat$mean[seq(4,25,5)],pch=c(3,3,2,1,3),main="b1")
plot(my.dat$mean[seq(5,25,5)],pch=c(3,3,2,1,3),main="b3")

##
data_dar<-read.csv("DarEsSalaamConst.csv",sep='',header=T)
data_mor<-read.csv("MorogoroConst.csv",sep='',header=T)
data_gha<-read.csv("AccraConst.csv",sep='',header=T)
data_sao<-read.csv("SaoTomeConst.csv",sep='',header=T)
data_pri<-read.csv("PrincipeConst.csv",sep='',header=T)

library(psych)

lablist<-c("Dar Es Salaam","Morogoro","Accra","Sao Tome","Principe")

my.dat <- data.frame(mean=c(data_dar[2,1],
                            data_mor[2,1],
                            data_gha[2,1],
                            data_sao[2,1],
                            data_pri[2,1]),
                     up=c(data_dar[2,7],
                          data_mor[2,7],
                          data_gha[2,7],
                          data_sao[2,7],
                          data_pri[2,7]),
                     low=c(data_dar[2,3],
                           data_mor[2,3],
                           data_gha[2,3],
                           data_sao[2,3],
                           data_pri[2,3]),
                     prop.M=c(0.9076923,
                              0.3928571,
                              0.7356322,
                              0.4605263,
                              0.5294118),
                     pop.N=c(6000,
                             10000,
                             100000,
                             5000,
                             20000),
                     hunt=c(0.1,
                            0.095,
                            0.5,
                            1,
                            0.09))

par(mar=c(7.1, 4.1, 4.1, 4.1))
plot(y=1-my.dat$mean,x=my.dat$prop.M,ylim=c(0,1),ylab='Estimated annual survival',xlab='Proportion male',
     pch=16,xlim=c(0,1))
segments(x0=my.dat$prop.M,x1=my.dat$prop.M,y0=1-my.dat$up,y1=1-my.dat$low,lty=1)

#load data 
constantS=read.csv('constSreg_data.csv',header = T)
survivalvgender=read.csv('survivalvgender.csv',header = T)
rownames(survivalvgender)=survivalvgender$Location
# Add survival results
survivalvgender[,5]=constantS[,2]

modelM=lm(survival.est ~ prop.M ,data = survivalvgender)

# Plot survival versus gender
# par(pty="s")
# plot(survivalvgender$prop.M,survivalvgender$survival.est,xlab="Proportion male",ylab="Survival rate",xlim=c(0,1),ylim=c(0,1),pch=20)
#abline(modelM,lty=2,col='grey')
text(0.2,0.67,"Morogoro",cex=0.8,pos=4)
text(0.475,0.80,"Sao Tome",cex=0.8,pos=2)
text(0.55,0.85,"Principe",cex=0.8,pos=3)
text(0.74,0.92,"Accra",cex=0.8,pos=3)
text(0.91,0.78,"Dar es",cex=0.8,pos=1)
text(0.91,0.73,"Salaam",cex=0.8,pos=1)
dev.copy2pdf(file="Effect of sex on survival_dtsh.pdf", width = 7, height = 7)

###
par(mar=c(7.1, 4.1, 4.1, 4.1))
plot(y=1-my.dat$mean,x=my.dat$pop.N,ylim=c(0,1),ylab='Estimated annual survival',xlab='Colony size',
     pch=16,xlim=c(0,max(my.dat$pop.N)))
segments(x0=my.dat$pop.N,x1=my.dat$pop.N,y0=1-my.dat$up,y1=1-my.dat$low,lty=1)

text(my.dat$pop.N[2],1-my.dat$mean[2]-0.1,"Morogoro",cex=0.8,pos=4)
text(my.dat$pop.N[4]+1000,1-my.dat$mean[4]-0.1,"Sao",cex=0.8,pos=2)
text(my.dat$pop.N[4]+1300,1-my.dat$mean[4]-0.14,"Tome",cex=0.8,pos=2)
text(my.dat$pop.N[5]+5000,1-my.dat$mean[5]-0.1,"Principe",cex=0.8,pos=3)
text(my.dat$pop.N[3]-1000,1-my.dat$mean[3]-0.15,"Accra",cex=0.8,pos=3)
text(my.dat$pop.N[1],1-my.dat$mean[1]+0.18,"Dar es",cex=0.8,pos=1)
text(my.dat$pop.N[1],1-my.dat$mean[1]+0.14,"Salaam",cex=0.8,pos=1)
dev.copy2pdf(file="Effect of colony size on survival_dtsh.pdf", width = 7, height = 7)

##
###
par(mar=c(7.1, 4.1, 4.1, 4.1))
plot(y=1-my.dat$mean,x=my.dat$hunt,ylim=c(0,1),ylab='Estimated annual survival',xlab='Harvest',
     pch=16,xlim=c(0,1))
segments(x0=my.dat$hunt,x1=my.dat$hunt,y0=1-my.dat$up,y1=1-my.dat$low,lty=1)

text(my.dat$hunt[2]+0.02,1-my.dat$mean[2]-0.05,"Morogoro",cex=0.8,pos=4)
text(my.dat$hunt[4],1-my.dat$mean[4]-0.05,"Sao Tome",cex=0.8,pos=2)
text(my.dat$hunt[5]-0.06,1-my.dat$mean[5]-0,"Principe",cex=0.8,pos=3)
text(my.dat$hunt[3],1-my.dat$mean[3]-0.15,"Accra",cex=0.8,pos=3)
text(my.dat$hunt[1]+0.08,1-my.dat$mean[1]+0.1,"Dar es",cex=0.8,pos=1)
text(my.dat$hunt[1]+0.08,1-my.dat$mean[1]+0.06,"Salaam",cex=0.8,pos=1)
dev.copy2pdf(file="Effect of hunting on survival_dtsh.pdf", width = 7, height = 7)

##
life_ex<--1/log(1-my.dat$mean)
mean(life_ex)
up95<-mean(life_ex)+1.96*sd(life_ex)
lo95<-mean(life_ex)-1.96*sd(life_ex)
