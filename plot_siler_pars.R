data_dar<-read.csv("DarEsSalaamboth.csv",sep='')
data_mor<-read.csv("Morogoroboth.csv",sep='')
data_gha<-read.csv("Ghanaboth.csv",sep='')
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

