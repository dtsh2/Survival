#load data 
constantS=read.csv('constSreg_data.csv',header = T)
survivalvgender=read.csv('survivalvgender.csv',header = T)
rownames(survivalvgender)=survivalvgender$Location
# Add survival results
survivalvgender[,5]=constantS[,2]

modelM=lm(survival.est ~ prop.M ,data = survivalvgender)

# Plot survival versus gender
par(pty="s")
plot(survivalvgender$prop.M,survivalvgender$survival.est,xlab="Proportion male",ylab="Annual survival rate",xlim=c(0,1),ylim=c(0,1),pch=20)
abline(modelM)
text(0.25,0.67,"Morogoro",cex=0.8,pos=4)
text(0.475,0.80,"Sao Tome",cex=0.8,pos=2)
text(0.55,0.81,"Principe",cex=0.8,pos=3)
text(0.74,0.88,"Accra",cex=0.8,pos=3)
text(0.91,0.86,"Dar es",cex=0.8,pos=1)
text(0.91,0.82,"Salaam",cex=0.8,pos=1)
dev.copy2pdf(file="Effect of sex on survival.pdf", width = 7, height = 7)
