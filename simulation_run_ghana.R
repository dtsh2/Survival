## simulations
# run the main file first
# import model parameters
outC<-read.csv('AccraConst.csv',sep='')
dat <- read.csv("weightedAge.csv")
# run simulations
Age<-0:45
const<-outC$mean[1]*exp(-outC$mean[2]*Age)
round(const)

pdf("simulation_age_Accra.pdf",width=6,height=6)
plot(const,ylab="Frequency",xlab='Age',pch=21,bg="grey",ylim=c(0,max(dat$Accra)),main="Accra")
points(dat$Accra,pch=19)
legend('topright',c("Predicted","Data"),pch=c(21,19),bty='n',pt.bg=c('grey','black'))
abline(h=0,lty=2)
dev.off()
