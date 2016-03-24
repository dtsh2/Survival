lablist<-c("Dar Es Salaam","Morogoro","Accra","Sao Tome","Principe")
modlist<-c("Constant","Maturation","Senescence","Mat_Sen","Siler")

const_data_dar<-read.csv("DarEsSalaamConst.csv",sep='')
const_data_mor<-read.csv("MorogoroConst.csv",sep='')
const_data_gha<-read.csv("AccraConst.csv",sep='')
const_data_sao<-read.csv("SaoTomeConst.csv",sep='')
const_data_pri<-read.csv("PrincipeConst.csv",sep='')

mat_data_dar<-read.csv("DarEsSalaamMat.csv",sep='')
mat_data_mor<-read.csv("MorogoroMat.csv",sep='')
mat_data_gha<-read.csv("AccraMat.csv",sep='')
mat_data_sao<-read.csv("SaoTomeMat.csv",sep='')
mat_data_pri<-read.csv("PrincipeMat.csv",sep='')

sen_data_dar<-read.csv("DarEsSalaamSen.csv",sep='')
sen_data_mor<-read.csv("MorogoroSen.csv",sep='')
sen_data_gha<-read.csv("AccraSen.csv",sep='')
sen_data_sao<-read.csv("SaoTomeSen.csv",sep='')
sen_data_pri<-read.csv("PrincipeSen.csv",sep='')

m_s_data_dar<-read.csv("DarEsSalaamMatSen.csv",sep='')
m_s_data_mor<-read.csv("MorogoroMatSen.csv",sep='')
m_s_data_gha<-read.csv("AccraMatSen.csv",sep='')
m_s_data_sao<-read.csv("SaoTomeMatSen.csv",sep='')
m_s_data_pri<-read.csv("PrincipeMatSen.csv",sep='')

siler_data_dar<-read.csv("DarEsSalaamboth.csv",sep='')
siler_data_mor<-read.csv("Morogoroboth.csv",sep='')
siler_data_gha<-read.csv("Accraboth.csv",sep='')
siler_data_sao<-read.csv("SaoTomeboth.csv",sep='')
siler_data_pri<-read.csv("Principeboth.csv",sep='')

newDF<-data.frame(matrix(NA,nrow=25,ncol=8))
colnames(newDF)<-c("Model",'Location',"a",'a1','a2','a3','b1','b3')
newDF[1:5,1]<-modlist[1]
newDF[6:10,1]<-modlist[2]
newDF[11:15,1]<-modlist[3]
newDF[16:20,1]<-modlist[4]
newDF[21:25,1]<-modlist[5]
newDF[,2]<-lablist
newDF[1:5,3]<-c(const_data_dar$mean[1],
                const_data_mor$mean[1],
                const_data_gha$mean[1],
                const_data_sao$mean[1],
                const_data_pri$mean[1])
newDF[6:10,3]<-c(mat_data_dar$mean[1],
                mat_data_mor$mean[1],
                mat_data_gha$mean[1],
                mat_data_sao$mean[1],
                mat_data_pri$mean[1])
newDF[11:15,3]<-c(sen_data_dar$mean[1],
                sen_data_mor$mean[1],
                sen_data_gha$mean[1],
                sen_data_sao$mean[1],
                sen_data_pri$mean[1])
newDF[16:20,3]<-c(m_s_data_dar$mean[1],
                  m_s_data_mor$mean[1],
                m_s_data_gha$mean[1],
                m_s_data_sao$mean[1],
                m_s_data_pri$mean[1])
newDF[21:25,3]<-c(siler_data_dar$mean[1],
                  siler_data_mor$mean[1],
                siler_data_gha$mean[1],
                siler_data_sao$mean[1],
                siler_data_pri$mean[1])
newDF[,3]<-round(newDF[,3],0)

newDF[6:10,4]<-c(mat_data_dar$mean[2],
                 mat_data_mor$mean[2],
                 mat_data_gha$mean[2],
                 mat_data_sao$mean[2],
                 mat_data_pri$mean[2])
newDF[16:20,4]<-c(m_s_data_dar$mean[2],
                  m_s_data_mor$mean[2],
                  m_s_data_gha$mean[2],
                  m_s_data_sao$mean[2],
                  m_s_data_pri$mean[2])
newDF[21:25,4]<-c(siler_data_dar$mean[2],
                  siler_data_mor$mean[2],
                  siler_data_gha$mean[2],
                  siler_data_sao$mean[2],
                  siler_data_pri$mean[2])
newDF[,4]<-round(newDF[,4],3)

newDF[1:5,5]<-c(const_data_dar$mean[2],
                const_data_mor$mean[2],
                const_data_gha$mean[2],
                const_data_sao$mean[2],
                const_data_pri$mean[2])
newDF[6:10,5]<-c(mat_data_dar$mean[3],
                 mat_data_mor$mean[3],
                 mat_data_gha$mean[3],
                 mat_data_sao$mean[3],
                 mat_data_pri$mean[3])
newDF[11:15,5]<-c(sen_data_dar$mean[3],
                  sen_data_mor$mean[3],
                  sen_data_gha$mean[3],
                  sen_data_sao$mean[3],
                  sen_data_pri$mean[3])
newDF[21:25,5]<-c(siler_data_dar$mean[3],
                  siler_data_mor$mean[3],
                  siler_data_gha$mean[3],
                  siler_data_sao$mean[3],
                  siler_data_pri$mean[3])
newDF[,5]<-round(newDF[,5],3)

newDF[11:15,6]<-c(sen_data_dar$mean[2],
                  sen_data_mor$mean[2],
                  sen_data_gha$mean[2],
                  sen_data_sao$mean[2],
                  sen_data_pri$mean[2])
newDF[16:20,6]<-c(m_s_data_dar$mean[3],
                  m_s_data_mor$mean[3],
                  m_s_data_gha$mean[3],
                  m_s_data_sao$mean[3],
                  m_s_data_pri$mean[3])
newDF[21:25,6]<-c(siler_data_dar$mean[4],
                  siler_data_mor$mean[4],
                  siler_data_gha$mean[4],
                  siler_data_sao$mean[4],
                  siler_data_pri$mean[4])
newDF[,6]<-round(newDF[,6],3)

newDF[6:10,7]<-c(mat_data_dar$mean[4],
                 mat_data_mor$mean[4],
                 mat_data_gha$mean[4],
                 mat_data_sao$mean[4],
                 mat_data_pri$mean[4])
newDF[16:20,7]<-c(m_s_data_dar$mean[4],
                  m_s_data_mor$mean[4],
                  m_s_data_gha$mean[4],
                  m_s_data_sao$mean[4],
                  m_s_data_pri$mean[4])
newDF[21:25,7]<-c(siler_data_dar$mean[5],
                  siler_data_mor$mean[5],
                  siler_data_gha$mean[5],
                  siler_data_sao$mean[5],
                  siler_data_pri$mean[5])
newDF[,7]<-round(newDF[,7],3)


newDF[11:15,8]<-c(sen_data_dar$mean[4],
                     sen_data_mor$mean[4],
                     sen_data_gha$mean[4],
                     sen_data_sao$mean[4],
                     sen_data_pri$mean[4])
newDF[16:20,8]<-c(m_s_data_dar$mean[5],
                     m_s_data_mor$mean[5],
                     m_s_data_gha$mean[5],
                     m_s_data_sao$mean[5],
                     m_s_data_pri$mean[5])
newDF[21:25,8]<-c(siler_data_dar$mean[6],
                     siler_data_mor$mean[6],
                     siler_data_gha$mean[6],
                     siler_data_sao$mean[6],
                     siler_data_pri$mean[6])
newDF[,8]<-round(newDF[,8],3)
##
newDF_lo<-data.frame(matrix(NA,nrow=25,ncol=8))
colnames(newDF_lo)<-c("Model",'Location',"a",'a1','a2','a3','b1','b3')
newDF_lo[1:5,1]<-modlist[1]
newDF_lo[6:10,1]<-modlist[2]
newDF_lo[11:15,1]<-modlist[3]
newDF_lo[16:20,1]<-modlist[4]
newDF_lo[21:25,1]<-modlist[5]
newDF_lo[,2]<-lablist

newDF_lo[1:5,3]<-c(const_data_dar$X2.5[1],
                const_data_mor$X2.5[1],
                const_data_gha$X2.5[1],
                const_data_sao$X2.5[1],
                const_data_pri$X2.5[1])
newDF_lo[6:10,3]<-c(mat_data_dar$X2.5[1],
                 mat_data_mor$X2.5[1],
                 mat_data_gha$X2.5[1],
                 mat_data_sao$X2.5[1],
                 mat_data_pri$X2.5[1])
newDF_lo[11:15,3]<-c(sen_data_dar$X2.5[1],
                  sen_data_mor$X2.5[1],
                  sen_data_gha$X2.5[1],
                  sen_data_sao$X2.5[1],
                  sen_data_pri$X2.5[1])
newDF_lo[16:20,3]<-c(m_s_data_dar$X2.5[1],
                  m_s_data_mor$X2.5[1],
                  m_s_data_gha$X2.5[1],
                  m_s_data_sao$X2.5[1],
                  m_s_data_pri$X2.5[1])
newDF_lo[21:25,3]<-c(siler_data_dar$X2.5[1],
                  siler_data_mor$X2.5[1],
                  siler_data_gha$X2.5[1],
                  siler_data_sao$X2.5[1],
                  siler_data_pri$X2.5[1])
newDF_lo[,3]<-round(newDF_lo[,3],0)

newDF_lo[6:10,4]<-c(mat_data_dar$X2.5[2],
                 mat_data_mor$X2.5[2],
                 mat_data_gha$X2.5[2],
                 mat_data_sao$X2.5[2],
                 mat_data_pri$X2.5[2])
newDF_lo[16:20,4]<-c(m_s_data_dar$X2.5[2],
                  m_s_data_mor$X2.5[2],
                  m_s_data_gha$X2.5[2],
                  m_s_data_sao$X2.5[2],
                  m_s_data_pri$X2.5[2])
newDF_lo[21:25,4]<-c(siler_data_dar$X2.5[2],
                  siler_data_mor$X2.5[2],
                  siler_data_gha$X2.5[2],
                  siler_data_sao$X2.5[2],
                  siler_data_pri$X2.5[2])
newDF_lo[,4]<-round(newDF_lo[,4],3)

newDF_lo[1:5,5]<-c(const_data_dar$X2.5[2],
                const_data_mor$X2.5[2],
                const_data_gha$X2.5[2],
                const_data_sao$X2.5[2],
                const_data_pri$X2.5[2])
newDF_lo[6:10,5]<-c(mat_data_dar$X2.5[3],
                 mat_data_mor$X2.5[3],
                 mat_data_gha$X2.5[3],
                 mat_data_sao$X2.5[3],
                 mat_data_pri$X2.5[3])
newDF_lo[11:15,5]<-c(sen_data_dar$X2.5[3],
                  sen_data_mor$X2.5[3],
                  sen_data_gha$X2.5[3],
                  sen_data_sao$X2.5[3],
                  sen_data_pri$X2.5[3])
newDF_lo[21:25,5]<-c(siler_data_dar$X2.5[3],
                  siler_data_mor$X2.5[3],
                  siler_data_gha$X2.5[3],
                  siler_data_sao$X2.5[3],
                  siler_data_pri$X2.5[3])
newDF_lo[,5]<-round(newDF_lo[,5],3)

newDF_lo[11:15,6]<-c(sen_data_dar$X2.5[2],
                  sen_data_mor$X2.5[2],
                  sen_data_gha$X2.5[2],
                  sen_data_sao$X2.5[2],
                  sen_data_pri$X2.5[2])
newDF_lo[16:20,6]<-c(m_s_data_dar$X2.5[3],
                  m_s_data_mor$X2.5[3],
                  m_s_data_gha$X2.5[3],
                  m_s_data_sao$X2.5[3],
                  m_s_data_pri$X2.5[3])
newDF_lo[21:25,6]<-c(siler_data_dar$X2.5[4],
                  siler_data_mor$X2.5[4],
                  siler_data_gha$X2.5[4],
                  siler_data_sao$X2.5[4],
                  siler_data_pri$X2.5[4])
newDF_lo[,6]<-round(newDF_lo[,6],3)

newDF_lo[6:10,7]<-c(mat_data_dar$X2.5[4],
                 mat_data_mor$X2.5[4],
                 mat_data_gha$X2.5[4],
                 mat_data_sao$X2.5[4],
                 mat_data_pri$X2.5[4])
newDF_lo[16:20,7]<-c(m_s_data_dar$X2.5[4],
                  m_s_data_mor$X2.5[4],
                  m_s_data_gha$X2.5[4],
                  m_s_data_sao$X2.5[4],
                  m_s_data_pri$X2.5[4])
newDF_lo[21:25,7]<-c(siler_data_dar$X2.5[5],
                  siler_data_mor$X2.5[5],
                  siler_data_gha$X2.5[5],
                  siler_data_sao$X2.5[5],
                  siler_data_pri$X2.5[5])
newDF_lo[,7]<-round(newDF_lo[,7],3)

newDF_lo[11:15,8]<-c(sen_data_dar$X2.5[4],
                     sen_data_mor$X2.5[4],
                     sen_data_gha$X2.5[4],
                     sen_data_sao$X2.5[4],
                     sen_data_pri$X2.5[4])
newDF_lo[16:20,8]<-c(m_s_data_dar$X2.5[5],
                     m_s_data_mor$X2.5[5],
                     m_s_data_gha$X2.5[5],
                     m_s_data_sao$X2.5[5],
                     m_s_data_pri$X2.5[5])
newDF_lo[21:25,8]<-c(siler_data_dar$X2.5[6],
                     siler_data_mor$X2.5[6],
                     siler_data_gha$X2.5[6],
                     siler_data_sao$X2.5[6],
                     siler_data_pri$X2.5[6])
newDF_lo[,8]<-round(newDF_lo[,8],3)

##
newDF_hi<-data.frame(matrix(NA,nrow=25,ncol=8))
colnames(newDF_hi)<-c("Model",'Location',"a",'a1','a2','a3','b1','b3')
newDF_hi[1:5,1]<-modlist[1]
newDF_hi[6:10,1]<-modlist[2]
newDF_hi[11:15,1]<-modlist[3]
newDF_hi[16:20,1]<-modlist[4]
newDF_hi[21:25,1]<-modlist[5]
newDF_hi[,2]<-lablist

newDF_hi[1:5,3]<-c(const_data_dar$X97.5[1],
                   const_data_mor$X97.5[1],
                   const_data_gha$X97.5[1],
                   const_data_sao$X97.5[1],
                   const_data_pri$X97.5[1])
newDF_hi[6:10,3]<-c(mat_data_dar$X97.5[1],
                    mat_data_mor$X97.5[1],
                    mat_data_gha$X97.5[1],
                    mat_data_sao$X97.5[1],
                    mat_data_pri$X97.5[1])
newDF_hi[11:15,3]<-c(sen_data_dar$X97.5[1],
                     sen_data_mor$X97.5[1],
                     sen_data_gha$X97.5[1],
                     sen_data_sao$X97.5[1],
                     sen_data_pri$X97.5[1])
newDF_hi[16:20,3]<-c(m_s_data_dar$X97.5[1],
                     m_s_data_mor$X97.5[1],
                     m_s_data_gha$X97.5[1],
                     m_s_data_sao$X97.5[1],
                     m_s_data_pri$X97.5[1])
newDF_hi[21:25,3]<-c(siler_data_dar$X97.5[1],
                     siler_data_mor$X97.5[1],
                     siler_data_gha$X97.5[1],
                     siler_data_sao$X97.5[1],
                     siler_data_pri$X97.5[1])
newDF_hi[,3]<-round(newDF_hi[,3],0)

newDF_hi[6:10,4]<-c(mat_data_dar$X97.5[2],
                    mat_data_mor$X97.5[2],
                    mat_data_gha$X97.5[2],
                    mat_data_sao$X97.5[2],
                    mat_data_pri$X97.5[2])
newDF_hi[16:20,4]<-c(m_s_data_dar$X97.5[2],
                     m_s_data_mor$X97.5[2],
                     m_s_data_gha$X97.5[2],
                     m_s_data_sao$X97.5[2],
                     m_s_data_pri$X97.5[2])
newDF_hi[21:25,4]<-c(siler_data_dar$X97.5[2],
                     siler_data_mor$X97.5[2],
                     siler_data_gha$X97.5[2],
                     siler_data_sao$X97.5[2],
                     siler_data_pri$X97.5[2])
newDF_hi[,4]<-round(newDF_hi[,4],3)

newDF_hi[1:5,5]<-c(const_data_dar$X97.5[2],
                   const_data_mor$X97.5[2],
                   const_data_gha$X97.5[2],
                   const_data_sao$X97.5[2],
                   const_data_pri$X97.5[2])
newDF_hi[6:10,5]<-c(mat_data_dar$X97.5[3],
                    mat_data_mor$X97.5[3],
                    mat_data_gha$X97.5[3],
                    mat_data_sao$X97.5[3],
                    mat_data_pri$X97.5[3])
newDF_hi[11:15,5]<-c(sen_data_dar$X97.5[3],
                     sen_data_mor$X97.5[3],
                     sen_data_gha$X97.5[3],
                     sen_data_sao$X97.5[3],
                     sen_data_pri$X97.5[3])
newDF_hi[21:25,5]<-c(siler_data_dar$X97.5[3],
                     siler_data_mor$X97.5[3],
                     siler_data_gha$X97.5[3],
                     siler_data_sao$X97.5[3],
                     siler_data_pri$X97.5[3])
newDF_hi[,5]<-round(newDF_hi[,5],3)

newDF_hi[11:15,6]<-c(sen_data_dar$X97.5[2],
                     sen_data_mor$X97.5[2],
                     sen_data_gha$X97.5[2],
                     sen_data_sao$X97.5[2],
                     sen_data_pri$X97.5[2])
newDF_hi[16:20,6]<-c(m_s_data_dar$X97.5[3],
                     m_s_data_mor$X97.5[3],
                     m_s_data_gha$X97.5[3],
                     m_s_data_sao$X97.5[3],
                     m_s_data_pri$X97.5[3])
newDF_hi[21:25,6]<-c(siler_data_dar$X97.5[4],
                     siler_data_mor$X97.5[4],
                     siler_data_gha$X97.5[4],
                     siler_data_sao$X97.5[4],
                     siler_data_pri$X97.5[4])
newDF_hi[,6]<-round(newDF_hi[,6],3)

newDF_hi[6:10,7]<-c(mat_data_dar$X97.5[4],
                    mat_data_mor$X97.5[4],
                    mat_data_gha$X97.5[4],
                    mat_data_sao$X97.5[4],
                    mat_data_pri$X97.5[4])
newDF_hi[16:20,7]<-c(m_s_data_dar$X97.5[4],
                     m_s_data_mor$X97.5[4],
                     m_s_data_gha$X97.5[4],
                     m_s_data_sao$X97.5[4],
                     m_s_data_pri$X97.5[4])
newDF_hi[21:25,7]<-c(siler_data_dar$X97.5[5],
                     siler_data_mor$X97.5[5],
                     siler_data_gha$X97.5[5],
                     siler_data_sao$X97.5[5],
                     siler_data_pri$X97.5[5])
newDF_hi[,7]<-round(newDF_hi[,7],3)

newDF_hi[11:15,8]<-c(sen_data_dar$X97.5[4],
                     sen_data_mor$X97.5[4],
                     sen_data_gha$X97.5[4],
                     sen_data_sao$X97.5[4],
                     sen_data_pri$X97.5[4])
newDF_hi[16:20,8]<-c(m_s_data_dar$X97.5[5],
                     m_s_data_mor$X97.5[5],
                     m_s_data_gha$X97.5[5],
                     m_s_data_sao$X97.5[5],
                     m_s_data_pri$X97.5[5])
newDF_hi[21:25,8]<-c(siler_data_dar$X97.5[6],
                     siler_data_mor$X97.5[6],
                     siler_data_gha$X97.5[6],
                     siler_data_sao$X97.5[6],
                     siler_data_pri$X97.5[6])
newDF_hi[,8]<-round(newDF_hi[,8],3)

newDF$testal<- paste(newDF$a,newDF_lo$a, sep = ' (') 
newDF$testa1l<- paste(newDF$a1,newDF_lo$a1, sep = ' (')
newDF$testa2l<- paste(newDF$a2,newDF_lo$a2, sep = ' (')
newDF$testa3l<- paste(newDF$a3,newDF_lo$a3, sep = ' (')
newDF$testb1l<- paste(newDF$b1,newDF_lo$b1, sep = ' (')
newDF$testb3l<- paste(newDF$b3,newDF_lo$b3, sep = ' (')

newDF$testau<- paste(newDF$testal,newDF_hi$a, sep = '-') 
newDF$testa1u<- paste(newDF$testa1l,newDF_hi$a1, sep = '-')
newDF$testa2u<- paste(newDF$testa2l,newDF_hi$a2, sep = '-')
newDF$testa3u<- paste(newDF$testa3l,newDF_hi$a3, sep = '-')
newDF$testb1u<- paste(newDF$testb1l,newDF_hi$b1, sep = '-')
newDF$testb3u<- paste(newDF$testb3l,newDF_hi$b3, sep = '-')

newDF$testaci<- paste(newDF$testau,")", sep = '') 
newDF$testa1ci<- paste(newDF$testa1u,")", sep = '')
newDF$testa2ci<- paste(newDF$testa2u,")", sep = '')
newDF$testa3ci<- paste(newDF$testa3u,")", sep = '')
newDF$testb1ci<- paste(newDF$testb1u,")", sep = '')
newDF$testb3ci<- paste(newDF$testb3u,")", sep = '')

write.csv(newDF,file="test_supp_info.csv")
