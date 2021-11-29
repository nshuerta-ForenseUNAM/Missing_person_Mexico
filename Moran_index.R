library(sf)
library(ggplot2)
library(rgeos)
library(RODBC)
library(dplyr)
require(maps)
library(ape)

data_points<- st_read("~/Desktop/Trazabilidad/Desaparecidos/Mxico_Estados/Mxico_Estados.shp")
# Revome accents
state_name<-data_points$ESTADO
state_name[11]<-"Ciudad de Mexico"
state_name[7]<-"Queretaro"
state_name[9]<-"Michoacan"
state_name[10]<-"Mexico"
state_name[14]<-"Yucatan"
state_name[29]<-"San Luis Potosi"
state_name[30]<-"Nuevo Leon"

sp_cent <- gCentroid(as(data_points, "Spatial"), byid = TRUE)
sf_cent <- st_centroid(data_points)

ggplot() + 
  geom_sf(data = data_points, fill = 'white') +
  geom_sf(data = sp_cent %>% st_as_sf, color = 'blue') + 
  geom_sf(data = sf_cent, color = 'red') 

centro<-data.frame(sp_cent)
Datas_Centro<-data.frame(centro,state_name)

#Non-located
dir<-file.choose()
tabla_nolocalizados<-read.csv(dir)
head(tabla_nolocalizados)

#Joint 
ds_map<- left_join(Datas_Centro, tabla_nolocalizados, by = "state_name")
ds_map_NA<-ds_map[complete.cases(ds_map), ]
inf.dists <- as.matrix(dist(cbind(lon=ds_map_NA$x, lat=ds_map_NA$y)))
inf.dists.inv <- 1/inf.dists
diag(inf.dists.inv) <- 0

#Year
#Variables
#son los años (X2006, X2007, X2008, X2009, X2010, X2011, X2012, X2013, X2014, X2015, X2016, X2017, X2018)
moran<-function(variable){
	var<-variable
	values<-as.numeric(ds_map_NA[,var])
	m<-Moran.I(values, inf.dists.inv)
	print(paste0("Year:",var," Moran Index ","(Observed: ",round(m$observed,digits=4),", Expected: ",round(m$expected,digits=4),", P-value: ",round(m$p.value,digits=4),")"))
}

vari<-c("X2006", "X2007", "X2008", "X2009", "X2010", "X2011", "X2012", "X2013", "X2014", "X2015", "X2016", "X2017", "X2018")

print("NO LOCALIZADOS - Moran Index", quote=FALSE)
for(i in 1:length(vari)){
	moran(vari[i])
}


#Living 
dir<-file.choose()
tabla_vivos<-read.csv(dir)
head(tabla_vivos)

#Joint 
ds_map<- left_join(Datas_Centro, tabla_vivos, by = "state_name")
ds_map_NA<-ds_map[complete.cases(ds_map), ]
inf.dists <- as.matrix(dist(cbind(lon=ds_map_NA$x, lat=ds_map_NA$y)))
inf.dists.inv <- 1/inf.dists
diag(inf.dists.inv) <- 0

#Year
moran<-function(variable){
	var<-variable
	values<-as.numeric(ds_map_NA[,var])
	m<-Moran.I(values, inf.dists.inv)
	print(paste0("Year:",var," Moran Index ","(Observed: ",round(m$observed,digits=4),", Expected: ",round(m$expected,digits=4),", P-value: ",round(m$p.value,digits=4),")"))
}

vari<-c("X2006", "X2007", "X2008", "X2009", "X2010", "X2011", "X2012", "X2013", "X2014", "X2015", "X2016", "X2017", "X2018")

print("LOCALIZADOS VIVOS - Moran Index", quote=FALSE)
for(i in 1:length(vari)){
	moran(vari[i])
}

#Lifeless
dir<-file.choose()
tabla_muertos<-read.csv(dir)
head(tabla_muertos)

#Joint 
ds_map<- left_join(Datas_Centro, tabla_vivos, by = "state_name")
ds_map_NA<-ds_map[complete.cases(ds_map), ]
inf.dists <- as.matrix(dist(cbind(lon=ds_map_NA$x, lat=ds_map_NA$y)))
inf.dists.inv <- 1/inf.dists
diag(inf.dists.inv) <- 0

#Year
moran<-function(variable){
	var<-variable
	values<-as.numeric(ds_map_NA[,var])
	m<-Moran.I(values, inf.dists.inv)
	print(paste0("Year:",var," Moran Index ","(Observed: ",round(m$observed,digits=4),", Expected: ",round(m$expected,digits=4),", P-value: ",round(m$p.value,digits=4),")"))
}


vari<-c("X2006", "X2007", "X2008", "X2009", "X2010", "X2011", "X2012", "X2013", "X2014", "X2015", "X2016", "X2017", "X2018")

print("LOCALIZADOS MUERTOS - Moran Index", quote=FALSE)
for(i in 1:length(vari)){
	moran(vari[i])
}
