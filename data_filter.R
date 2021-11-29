#Missing persons
#Install
install.packages("chron")

# Original data
dir<-file.choose()
data<-read.csv(dir)
head(data)
#Variables 
data1<-data[,c("X02","X03","X04","X05","X06","X12","X14","X24","X25","X26","X36","X37","X38","X39","X41","X46","X47","X48")]
head(data1)
#dataset size
dim(data1)

#Dates transformation
library(chron)
#Time 1 - Report
data1$X02<- strptime(as.character(data1[,"X02"]), "%d/%m/%y")
data1$time_reporte<-paste(data1[,"X02"],data1[,"X03"])
dtparts = t(as.data.frame(strsplit(data1$time_reporte,' ')))
thetimes1 = chron(dates=dtparts[,1],times=dtparts[,2],format=c('y-m-d','h:m:s'))
data1$reporte_year<-as.numeric(format(thetimes1, "%Y"))
data1$reporte_mes<-as.numeric(format(thetimes1, "%m"))

#Time 2 - Event
data1$X04<- strptime(as.character(data1[,"X04"]), "%d/%m/%y")
data1$time_evento<-paste(data1[,"X04"],data1[,"X05"])
dtparts2 = t(as.data.frame(strsplit(data1$time_evento,' ')))
thetimes2 = chron(dates=dtparts2[,1],times=dtparts2[,2],format=c('y-m-d','h:m:s'))

#Time 3 - View
data1$X25<- strptime(as.character(data1[,"X25"]), "%d/%m/%y")
data1$time_avistamiento<-paste(data1[,"X25"],data1[,"X26"])
dtparts3 = t(as.data.frame(strsplit(data1$time_avistamiento,' ')))
thetimes3= chron(dates=dtparts3[,1],times=dtparts3[,2],format=c('y-m-d','h:m:s'))

#Time 4 - Located
data1$X36<- strptime(as.character(data1[,"X36"]), "%d/%m/%y")
data1$time_localizados<-paste(data1[,"X36"],data1[,"X37"])
dtparts4 = t(as.data.frame(strsplit(data1$time_localizados,' ')))
thetimes4= chron(dates=dtparts4[,1],times=dtparts4[,2],format=c('y-m-d','h:m:s'))

#Time 5 - Admission
data1$X46<- strptime(as.character(data1[,"X46"]), "%d/%m/%y")
data1$time_ingreso<-paste(data1[,"X46"],data1[,"X47"])
dtparts5 = t(as.data.frame(strsplit(data1$time_ingreso,' ')))
library(chron)
thetimes5= chron(dates=dtparts5[,1],times=dtparts5[,2],format=c('y-m-d','h:m:s'))

#Time 6 - Date of death
data1$X48<- strptime(as.character(data1[,"X48"]), "%d/%m/%y")
data1$time_fallecimiento<-data1[,"X48"]
dtparts6 = t(as.data.frame(strsplit(data1$time_fallecimiento,' ')))
thetimes6= chron(dates=dtparts5[,1],format=c('y-m-d'))

# Times
tm1 <- as.POSIXct(thetimes1)
tm2 <- as.POSIXct(thetimes2)
tm3 <- as.POSIXct(thetimes3)
tm4 <- as.POSIXct(thetimes4)
tm5 <- as.POSIXct(thetimes5)
tm6 <- as.POSIXct(thetimes6)

#Time difference
data1$avistamiento_evento<-difftime(tm2, tm3, units = "hours")
data1$evento_reporte<-difftime(tm1, tm2, units = "hours")
data1$reporte_localizacion<-difftime(tm4, tm1, units = "hours")
data1$localizacion_ingreso<-difftime(tm5, tm4, units = "hours")
data1$reporte_fallecimiento<-difftime(tm6, tm1, units = "hours")

#Remove cases
rep1<-which(data1$X06=="CALIFORNIA",arr.ind = FALSE)
rep2<-which(data1$X06=="GEORGIA",arr.ind = FALSE)
rep3<-which(data1$X06=="NEW YORK",arr.ind = FALSE)
rep4<-which(data1$X06=="NO ESPECIFICADO",arr.ind = FALSE)
rep5<-which(data1$X06=="TEXAS",arr.ind = FALSE)
no_val<-c(rep1,rep2,rep3,rep4,rep5)
no_mexicanos<-length(no_val) #Ojo con este dato equivale a las personas que no estan ubicadas en México
data2<-data1[-no_val,]
dim(data2)

#Non-located
data_nolocalizados<-subset(data2,X38=="AUN SIN LOCALIZAR")
dim(data_nolocalizados)

#Located
data_localizados<-subset(data2,X38!="AUN SIN LOCALIZAR")
dim(data_localizados) # Recuerda que en este número ya se omitieron aquellos que no fueran de México

#Remove NA 
data_localizados.na.rl<- data_localizados[!is.na(data_localizados$reporte_localizacion), ]
dim(data_localizados.na.rl)

#Partition located alive
data_localizados_vivo<-subset(data_localizados.na.rl,X38=="VIVO")
dim(data_localizados_vivo)

#Dead located partitions
data_localizados_muerto<-subset(data_localizados.na.rl,X38=="MUERTO")
dim(data_localizados_muerto)

#live inconsistency
data_localizados_vivo_SIN<-subset(data_localizados_vivo,reporte_localizacion>=0)
dim(data_localizados_vivo_SIN)

#inconsistency dead
data_localizados_muerto_SIN<-subset(data_localizados_muerto,reporte_localizacion>=0)
dim(data_localizados_muerto_SIN)

#Filtered base
total <- rbind(data_nolocalizados, data_localizados_vivo_SIN, data_localizados_muerto_SIN)
dim(total)

data_general<-total[,c("X06","X12","X14","X38","time_reporte","reporte_year","reporte_mes","time_evento","time_avistamiento","time_localizados","time_ingreso","time_fallecimiento","avistamiento_evento","evento_reporte","reporte_localizacion","localizacion_ingreso","reporte_fallecimiento")]

names(data_general)<-c("Estado","Sexo","Edad","Status","Time_reporte","Year_reporte","Mes_reporte","Time_evento","Time_avistamiento","Time_localizados","Time_ingreso","Time_fallecimiento","Avistamiento_evento","Evento_reporte","Reporte_localizacion","Localizacion_ingreso","Reporte_fallecimiento")

#Save data
write.csv(data_general,'/Users/sofia.huerta.p/Desktop/Base_General.csv')

#Save data
write.csv(total,'/Users/sofia.huerta.p/Desktop/Base_General.csv')
