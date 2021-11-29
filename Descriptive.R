#Descriptive analysis
#General data loading

dir<-file.choose()
data_general<-read.csv(dir)
head(data_general)

NO_LOCAL<-subset(data_general,Status=="AUN SIN LOCALIZAR")
dim(NO_LOCAL)
VIVO<-subset(data_general,Status=="VIVO")
dim(VIVO)
MUERTO<-subset(data_general,Status=="MUERTO")
dim(MUERTO)

#Frequency Table - Counts by Status
tabla1<-table(NO_LOCAL$Estado,NO_LOCAL$Year_reporte)
tab1<-as.matrix(tabla1)
write.csv(tab1,'No_Local.csv')

tabla2<-table(VIVO$Estado,VIVO$Year_reporte)
tab2<-as.matrix(tabla2)
write.csv(tab2,'VIVO.csv')

tabla3<-table(MUERTO$Estado,MUERTO$Year_reporte)
tab3<-as.matrix(tabla3)
write.csv(tab3,'MUERTO.csv')

#Validation of general counts
sum(tabla1)
sum(tabla2)
sum(tabla3)

#Table of frequencies - Medians by status

resume1<-aggregate(Reporte_localizacion ~  Estado + Year_reporte, data = VIVO, FUN= "median" )

resume2<-aggregate(Reporte_localizacion ~  Estado + Year_reporte, data = MUERTO, FUN= "median" )

nombres<-rownames(tabla1)
var<-colnames(tabla1)
mat1<-matrix(0,length(nombres),length(var))
mat2<-matrix(0,length(nombres),length(var))
rownames(mat1)<-nombres
colnames(mat1)<-var
rownames(mat2)<-nombres
colnames(mat2)<-var

for (i in 1:nrow(resume1)){
	for(j in 1:nrow(mat1)){
		for(k in 1:ncol(mat1)){
			if(resume1[i,1]==rownames(mat1)[j]&resume1[i,2]==colnames(mat1)[k]){
			values<-resume1[i,3]
			mat1[j,k]<-values
			}
		}
	}
}

for (i in 1:nrow(resume2)){
	for(j in 1:nrow(mat2)){
		for(k in 1:ncol(mat2)){
			if(resume2[i,1]==rownames(mat2)[j]&resume2[i,2]==colnames(mat2)[k]){
			values<-resume2[i,3]
			mat2[j,k]<-values
			}
		}
	}
}

#Medium hours of live
mat1
write.csv(mat1,'VIVO_HORAS.csv')

#Medium hours of death
mat2
write.csv(mat2,'MUERTO_HORAS.csv')

#Counting of living/dead hours
#Time intervals of
#24 hrs (1 día) 
#48 hrs (2 días) 
#72 hrs (3 días) 
#96 hrs (4 días)
#120 hrs (5 días) 
#1 semana (7 días) 
#1 mes (30 días) 
#6 meses (180 días) 
#1 año (365 días) 
#3 años (1095 días) 
#5 años (1825 días)
#10 años (3650 días)

Tabla_horas<-function(data){
data<-data
# 24 Horas (1 día)
data01<- subset(data,Reporte_localizacion>=0 & data$Reporte_localizacion<=24)
if(length(data01!=NULL)){
	data01$Horario<-1
}else{
	NULL
}
# 48 Horas (2 días)
data02<- subset(data,(Reporte_localizacion>24 & Reporte_localizacion<=48))
if(length(data02!=NULL)){
	data02$Horario<-2
}else{
	NULL
}
# 72 Horas (3 días)
data03<- subset(data,(Reporte_localizacion>48 & Reporte_localizacion<=72))
if(length(data03!=NULL)){
	data03$Horario<-3
}else{
	NULL
}

# 96 Horas (4 días)
data04<- subset(data,(Reporte_localizacion>72 & Reporte_localizacion<=96))
if(length(data04!=NULL)){
	data04$Horario<-4
}else{
	NULL
}
# 120 Horas (5 días)
data05<- subset(data,(Reporte_localizacion>96 & Reporte_localizacion<=120))
if(length(data05!=NULL)){
	data05$Horario<-5
}else{
	NULL
}
# Una semana (7 días) 168 horas 5 a los 7 días
data06<- subset(data,(Reporte_localizacion>120 & Reporte_localizacion<=168))
if(length(data06!=NULL)){
	data06$Horario<-6
}else{
	NULL
}
# Un mes (30 días) 720 horas 7 días al año
data07<- subset(data,(Reporte_localizacion>168 & Reporte_localizacion<=720))
if(length(data07!=NULL)){
	data07$Horario<-7
}else{
	NULL
}
# tres meses (90 días) 2160 horas mes al año
data08<- subset(data,(Reporte_localizacion>720 & Reporte_localizacion<=8760))
if(length(data08!=NULL)){
	data08$Horario<-8
}else{
	NULL
}
# > al año (365 días)  a los 3 años
data09<- subset(data,Reporte_localizacion>8760 & Reporte_localizacion<=26280)
if(length(data09!=NULL)){
	data09$Horario<-9
}else{
	NULL
}
# > de 3 años (365 días) a los 5 años
data10<- subset(data,Reporte_localizacion>26280 & Reporte_localizacion<=43800)
if(length(data10!=NULL)){
	data10$Horario<-10
}else{
	NULL
}
# > de 5 años (365 días) a los 10
data11<- subset(data,Reporte_localizacion>43800 & Reporte_localizacion<=87600)
if(length(data11!=NULL)){
	data11$Horario<-11
}else{
	NULL
}
# > de 10 años (365 días) 
data12<- subset(data,Reporte_localizacion>87600)
if(length(data12!=NULL)){
	data12$Horario<-12
}else{
	NULL
}

clase<-rbind(data01,data02,data03,data04,data05,data06,data07,data08,data09,data10,data11,data12)

return(clase)
}

vivos<-Tabla_horas(VIVO)
tabla_vivos<-table(vivos$Horario,vivos$Sexo)
rownames(tabla_vivos)<-c("0 a 24 hrs (1 día)", 
">24 a 48 hrs (2 día)",
">48 a 72 hrs (3 días)", 
">72 a 96 hrs(4 días)", 
">96 a 120 hrs (5 días)",
">120 hrs a la semana (7 días)", 
">1 semana al mes (30 días)", 
">1 mes al año (365 días)", 
">1 a 3 año (1095 días)", 
">3 a 5 años (1825 días)", 
">5 a 10 años (3650 días)",
">10 años (>3650 días)")

muertos<-Tabla_horas(MUERTO)
tabla_muertos<-table(muertos$Horario,muertos$Sexo)
rownames(tabla_muertos)<-c("0 a 24 hrs (1 día)", 
">24 a 48 hrs (2 día)",
">48 a 72 hrs (3 días)", 
">72 a 96 hrs(4 días)", 
">96 a 120 hrs (5 días)",
">120 hrs a la semana (7 días)", 
">1 semana al mes (30 días)", 
">1 mes al año (365 días)", 
">1 a 3 año (1095 días)", 
">3 a 5 años (1825 días)", 
">5 a 10 años (3650 días)")

write.csv(tabla_vivos,'tabla_vivos.csv')
tabla_vivos
sum(tabla_vivos)
write.csv(tabla_muertos,'tabla_muertos.csv')
tabla_muertos
sum(tabla_muertos)
