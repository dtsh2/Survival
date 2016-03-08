names_c <-c('Ghana','SaoTome','Principe','Morogoro',"DarEsSalaam")

constant_init<-as.data.frame(cbind(
# Ghana 
list(a=rnorm(1,300),a2=rnorm(1,0),data.sd=runif(1,1,30)),
# Sao Tome
list(a=rnorm(1,30),a2=rnorm(1,0),data.sd=runif(1,1,30)),
# Principe
list(a=rnorm(1,30),a2=rnorm(1,0),data.sd=runif(1,1,30)),
# Morogoro 
list(a=rnorm(1,30),a2=rnorm(1,0),data.sd=runif(1,1,30)),
# DarEsSalaam
list(a=rnorm(1,30),a2=rnorm(1,0),data.sd=runif(1,1,30))))
colnames(constant_init)<-names_c

mat_init<-as.data.frame(cbind(
# Ghana 
list(a=rnorm(1,300),a1=rnorm(1,5),a2=rnorm(1,0),b1=rnorm(1,7),data.sd=runif(1,0,30)),
# Sao Tome
list(a=rnorm(1,30),a1=rnorm(1,5),a2=rnorm(1,0),b1=rnorm(1,7),data.sd=runif(1,0,30)),
# Principe
list(a=rnorm(1,20),a1=rnorm(1,0.1),a2=rnorm(1,0.1),b1=rnorm(1,0.1),data.sd=runif(1,0,30)),
# Morogoro 
list(a=rnorm(1,14),a1=rnorm(1,0.1),a2=rnorm(1,0.1),b1=rnorm(1,0.1),data.sd=runif(1,0,30)),
# DarEsSalaam
list(a=rnorm(1,18),a1=rnorm(1,0.5),a2=rnorm(1,0.5),b1=rnorm(1,0.5),data.sd=runif(1,0,30))))
colnames(mat_init)<-names_c

sen_init<-as.data.frame(cbind(
# Ghana 
list(a=rnorm(1,300),a3=rnorm(1,-5.5),a2=rnorm(1,0),b3=rnorm(1,-7.7),data.sd=runif(1,0,30)),
# Sao Tome
list(a=rnorm(1,30),a3=rnorm(1,-6),a2=rnorm(1,0),b3=rnorm(1,-2),data.sd=runif(1,0,30)),
# Principe
list(a=rnorm(1,11),a3=rnorm(1,-0.01),a2=rnorm(1,0.1),b3=rnorm(1,-0.01),data.sd=runif(1,0,30)),
# Morogoro 
list(a=rnorm(1,30),a3=rnorm(1,-0.01),a2=rnorm(1,0.1),b3=rnorm(1,-0.01),data.sd=runif(1,0,30)),
# DarEsSalaam
list(a=rnorm(1,18),a3=rnorm(1,0.05),a2=rnorm(1,-0.05),b3=rnorm(1,0.05),data.sd=runif(1,0,30))))
colnames(sen_init)<-names_c

both_init<-as.data.frame(cbind(
# Ghana 
list(a=rnorm(1,250),a1=rnorm(1,0.1),a2=rnorm(1,0.05),a3=rnorm(1,-6),b1=rnorm(1,-0.07),b3=rnorm(1,-8),data.sd=runif(1,1,30)),
# Sao Tome
list(a=rnorm(1,30),a1=rnorm(1,0.1),a2=rnorm(1,0.1),a3=rnorm(1,-5),b1=rnorm(1,-0.01),b3=rnorm(1,-10),data.sd=runif(1,1,30)),
# Principe
list(a=rnorm(1,12),a1=rnorm(1,0.05),a2=rnorm(1,0.01),a3=rnorm(1,-0.05),b1=rnorm(1,-0.01),b3=rnorm(1,0),data.sd=runif(1,1,30)),
# Morogoro 
list(a=rnorm(1,30),a1=rnorm(1,0.1),a2=rnorm(1,0.1),a3=rnorm(1,-0.01),b1=rnorm(1,-0.01),b3=rnorm(1,-10),data.sd=runif(1,1,30)),
# DarEsSalaam
list(a=rnorm(1,10),a1=rnorm(1,0.1),a2=rnorm(1,0.1),a3=rnorm(1,-0.01),b1=rnorm(1,-0.01),b3=rnorm(1,-10),data.sd=runif(1,1,30))))
colnames(both_init)<-names_c

sen_mat_init<-as.data.frame(cbind(
  # Ghana 
  list(a=rnorm(1,300),a1=rnorm(1,5),b1=rnorm(1,7),data.sd=runif(1,0,30)),
  # Sao Tome
  list(a=rnorm(1,30),a1=rnorm(1,5),b1=rnorm(1,7),data.sd=runif(1,0,30)),
  # Principe
  list(a=rnorm(1,20),a1=rnorm(1,0.1),b1=rnorm(1,0.1),data.sd=runif(1,0,30)),
  # Morogoro 
  list(a=rnorm(1,14),a1=rnorm(1,0.1),b1=rnorm(1,0.1),data.sd=runif(1,0,30)),
  # DarEsSalaam
  list(a=rnorm(1,18),a1=rnorm(1,0.5),b1=rnorm(1,0.5),data.sd=runif(1,0,30))))
colnames(sen_mat_init)<-names_c
