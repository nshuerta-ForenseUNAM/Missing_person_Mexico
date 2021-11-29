#Maps

library(ggplot2)
library(dplyr)
require(maps)

#Use the generated bases 
#non-located_counts
dir<-file.choose()
no_local<-read.csv(dir)
head(no_local)
region<-c("01","02","03","04","07","08","09","05","06","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32")
no_local$region<-region

#living_counts
dir<-file.choose()
vivo<-read.csv(dir)
head(vivo)
vivo$region<-region

#lifeless_counts
dir<-file.choose()
muerto<-read.csv(dir)
head(muerto)
muerto$region<-region

#living_hours
dir<-file.choose()
hora_vivo<-read.csv(dir)
head(hora_vivo)
hora_vivo$region<-region

#lifeless_counts
dir<-file.choose()
hora_muerto<-read.csv(dir)
head(hora_muerto)
hora_muerto$region<-region

load("~/Desktop/mxstate.map.RData")
datamap<-mxstate.map
map1<- left_join(datamap, no_local, by = "region")
map2<- left_join(datamap, vivo, by = "region")
map3<- left_join(datamap, muerto, by = "region")
map4<- left_join(datamap, hora_vivo, by = "region")
map5<- left_join(datamap, hora_muerto, by = "region")

#non-located_counts
P1<-ggplot(map1, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = X2006),colour = "white")+
  theme_bw()+
  ggtitle("2006") + 
  theme(plot.title = element_text(hjust = 0.5,color="black", size=25, face="bold"),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  scale_fill_gradient(low = "gray", high = "steelblue4") + 
  labs(fill = "Counts",x = NULL,y = NULL) #+

P2<-ggplot(map1, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = X2007),colour = "white")+
  theme_bw()+
  ggtitle("2007") + 
  theme(plot.title = element_text(hjust = 0.5,color="black", size=25, face="bold"),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  scale_fill_gradient(low = "gray", high = "steelblue4") + 
  labs(fill = "Counts",x = NULL,y = NULL) #+
 
P3<-ggplot(map1, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = X2008),colour = "white")+
  theme_bw()+
  ggtitle("2008") + 
  theme(plot.title = element_text(hjust = 0.5,color="black", size=25, face="bold"),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  scale_fill_gradient(low = "gray", high = "steelblue4") + 
  labs(fill = "Counts",x = NULL,y = NULL) #+
 
P4<-ggplot(map1, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = X2009),colour = "white")+
  theme_bw()+
  ggtitle("2009") + 
  theme(plot.title = element_text(hjust = 0.5,color="black", size=25, face="bold"),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  scale_fill_gradient(low = "gray", high = "steelblue4") + 
  labs(fill = "Counts",x = NULL,y = NULL) #+
 
P5<-ggplot(map1, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = X2010),colour = "white")+
  theme_bw()+
  ggtitle("2010") + 
  theme(plot.title = element_text(hjust = 0.5,color="black", size=25, face="bold"),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  scale_fill_gradient(low = "gray", high = "steelblue4") + 
  labs(fill = "Counts",x = NULL,y = NULL) #+
 
P6<-ggplot(map1, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = X2011),colour = "white")+
  theme_bw()+
  ggtitle("2011") + 
  theme(plot.title = element_text(hjust = 0.5,color="black", size=25, face="bold"),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  scale_fill_gradient(low = "gray", high = "steelblue4") + 
  labs(fill = "Counts",x = NULL,y = NULL) #+
 
P7<-ggplot(map1, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = X2012),colour = "white")+
  theme_bw()+
  ggtitle("2012") + 
  theme(plot.title = element_text(hjust = 0.5,color="black", size=25, face="bold"),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  scale_fill_gradient(low = "gray", high = "steelblue4") + 
  labs(fill = "Counts",x = NULL,y = NULL) #+
 
P8<-ggplot(map1, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = X2013),colour = "white")+
  theme_bw()+
  ggtitle("2013") + 
  theme(plot.title = element_text(hjust = 0.5,color="black", size=25, face="bold"),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  scale_fill_gradient(low = "gray", high = "steelblue4") + 
  labs(fill = "Counts",x = NULL,y = NULL) #+
 
P9<-ggplot(map1, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = X2014),colour = "white")+
  theme_bw()+
  ggtitle("2014") + 
  theme(plot.title = element_text(hjust = 0.5,color="black", size=25, face="bold"),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  scale_fill_gradient(low = "gray", high = "steelblue4") + 
  labs(fill = "Counts",x = NULL,y = NULL) #+
 
P10<-ggplot(map1, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = X2015),colour = "white")+
  theme_bw()+
  ggtitle("2015") + 
  theme(plot.title = element_text(hjust = 0.5,color="black", size=25, face="bold"),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  scale_fill_gradient(low = "gray", high = "steelblue4") + 
  labs(fill = "Counts",x = NULL,y = NULL) #+
 
P11<-ggplot(map1, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = X2016),colour = "white")+
  theme_bw()+
  ggtitle("2016") + 
  theme(plot.title = element_text(hjust = 0.5,color="black", size=25, face="bold"),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  scale_fill_gradient(low = "gray", high = "steelblue4") + 
  labs(fill = "Counts",x = NULL,y = NULL) #+
 
P12<-ggplot(map1, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = X2017),colour = "white")+
  theme_bw()+
  ggtitle("2017") + 
  theme(plot.title = element_text(hjust = 0.5,color="black", size=25, face="bold"),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  scale_fill_gradient(low = "gray", high = "steelblue4") + 
  labs(fill = "Counts",x = NULL,y = NULL) #+
 
P13_1<-ggplot(map1, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = X2018),colour = "white")+
  theme_bw()+
  ggtitle("Missing Persons") + 
  theme(plot.title = element_text(hjust = 0.5,color="black", size=25, face="bold"),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  scale_fill_gradient(low = "gray", high = "steelblue4") + 
labs(fill = "Counts",x = NULL,y = NULL)

require(gridExtra)
ga<-grid.arrange(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, ncol=3,nrow=4)
g <- arrangeGrob(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, ncol=3,nrow=4) #generates g

ggsave(file="Figure1.tiff", g,dpi = 320,width = 60, height = 60, units = "cm")


#living_counts

P1<-ggplot(map2, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = X2006),colour = "white")+
  theme_bw()+
  ggtitle("2006") + 
  theme(plot.title = element_text(hjust = 0.5,color="black", size=25, face="bold"),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  scale_fill_gradient(low = "gray", high = "aquamarine4") + 
labs(fill = "Counts",x = NULL,y = NULL)

P2<-ggplot(map2, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = X2007),colour = "white")+
  theme_bw()+
  ggtitle("2007") + 
  theme(plot.title = element_text(hjust = 0.5,color="black", size=25, face="bold"),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  scale_fill_gradient(low = "gray", high = "aquamarine4") + 
labs(fill = "Counts",x = NULL,y = NULL)

P3<-ggplot(map2, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = X2008),colour = "white")+
  theme_bw()+
  ggtitle("2008") + 
  theme(plot.title = element_text(hjust = 0.5,color="black", size=25, face="bold"),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  scale_fill_gradient(low = "gray", high = "aquamarine4") + 
labs(fill = "Counts",x = NULL,y = NULL)

P4<-ggplot(map2, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = X2009),colour = "white")+
  theme_bw()+
  ggtitle("2009") + 
  theme(plot.title = element_text(hjust = 0.5,color="black", size=25, face="bold"),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  scale_fill_gradient(low = "gray", high = "aquamarine4") + 
labs(fill = "Counts",x = NULL,y = NULL)

P5<-ggplot(map2, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = X2010),colour = "white")+
  theme_bw()+
  ggtitle("2010") + 
  theme(plot.title = element_text(hjust = 0.5,color="black", size=25, face="bold"),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  scale_fill_gradient(low = "gray", high = "aquamarine4") + 
labs(fill = "Counts",x = NULL,y = NULL)

P6<-ggplot(map2, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = X2011),colour = "white")+
  theme_bw()+
  ggtitle("2011") + 
  theme(plot.title = element_text(hjust = 0.5,color="black", size=25, face="bold"),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  scale_fill_gradient(low = "gray", high = "aquamarine4") + 
labs(fill = "Counts",x = NULL,y = NULL)

P7<-ggplot(map2, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = X2012),colour = "white")+
  theme_bw()+
  ggtitle("2012") + 
  theme(plot.title = element_text(hjust = 0.5,color="black", size=25, face="bold"),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  scale_fill_gradient(low = "gray", high = "aquamarine4") + 
labs(fill = "Counts",x = NULL,y = NULL)

P8<-ggplot(map2, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = X2013),colour = "white")+
  theme_bw()+
  ggtitle("2013") + 
  theme(plot.title = element_text(hjust = 0.5,color="black", size=25, face="bold"),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  scale_fill_gradient(low = "gray", high = "aquamarine4") + 
labs(fill = "Counts",x = NULL,y = NULL)

P9<-ggplot(map2, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = X2014),colour = "white")+
  theme_bw()+
  ggtitle("2014") + 
  theme(plot.title = element_text(hjust = 0.5,color="black", size=25, face="bold"),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  scale_fill_gradient(low = "gray", high = "aquamarine4") + 
labs(fill = "Counts",x = NULL,y = NULL)

P10<-ggplot(map2, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = X2015),colour = "white")+
  theme_bw()+
  ggtitle("2015") + 
  theme(plot.title = element_text(hjust = 0.5,color="black", size=25, face="bold"),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  scale_fill_gradient(low = "gray", high = "aquamarine4") + 
labs(fill = "Counts",x = NULL,y = NULL)

P11<-ggplot(map2, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = X2016),colour = "white")+
  theme_bw()+
  ggtitle("2016") + 
  theme(plot.title = element_text(hjust = 0.5,color="black", size=25, face="bold"),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  scale_fill_gradient(low = "gray", high = "aquamarine4") + 
labs(fill = "Counts",x = NULL,y = NULL)

P12<-ggplot(map2, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = X2017),colour = "white")+
  theme_bw()+
  ggtitle("2017") + 
  theme(plot.title = element_text(hjust = 0.5,color="black", size=25, face="bold"),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  scale_fill_gradient(low = "gray", high = "aquamarine4") + 
labs(fill = "Counts",x = NULL,y = NULL)

P13_2<-ggplot(map2, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = X2018),colour = "white")+
  theme_bw()+
  ggtitle("Found alive") + 
  theme(plot.title = element_text(hjust = 0.5,color="black", size=25, face="bold"),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  scale_fill_gradient(low = "gray", high = "aquamarine4") + 
  labs(fill = "Counts",x = NULL,y = NULL) #+
 
require(gridExtra)
ga<-grid.arrange(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, ncol=3,nrow=4)
g <- arrangeGrob(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, ncol=3,nrow=4) #generates g

ggsave(file="figure2.tiff", g,dpi = 320,width = 60, height = 60, units = "cm")

#lifeless_counts

P1<-ggplot(map3, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = X2006),colour = "white")+
  theme_bw()+
  ggtitle("2006") + 
  theme(plot.title = element_text(hjust = 0.5,color="black", size=25, face="bold"),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  scale_fill_gradient(low = "gray", high = "darkorchid4") + 
  labs(fill = "Counts",x = NULL,y = NULL) #+

P2<-ggplot(map3, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = X2007),colour = "white")+
  theme_bw()+
  ggtitle("2007") + 
  theme(plot.title = element_text(hjust = 0.5,color="black", size=25, face="bold"),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  scale_fill_gradient(low = "gray", high = "darkorchid4") + 
  labs(fill = "Counts",x = NULL,y = NULL) #+
 
P3<-ggplot(map3, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = X2008),colour = "white")+
  theme_bw()+
  ggtitle("2008") + 
  theme(plot.title = element_text(hjust = 0.5,color="black", size=25, face="bold"),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  scale_fill_gradient(low = "gray", high = "darkorchid4") + 
  labs(fill = "Counts",x = NULL,y = NULL) #+
 
P4<-ggplot(map3, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = X2009),colour = "white")+
  theme_bw()+
  ggtitle("2009") + 
  theme(plot.title = element_text(hjust = 0.5,color="black", size=25, face="bold"),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  scale_fill_gradient(low = "gray", high = "darkorchid4") + 
  labs(fill = "Counts",x = NULL,y = NULL) #+
 
P5<-ggplot(map3, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = X2010),colour = "white")+
  theme_bw()+
  ggtitle("2010") + 
  theme(plot.title = element_text(hjust = 0.5,color="black", size=25, face="bold"),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  scale_fill_gradient(low = "gray", high = "darkorchid4") + 
  labs(fill = "Counts",x = NULL,y = NULL) #+
 
P6<-ggplot(map3, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = X2011),colour = "white")+
  theme_bw()+
  ggtitle("2011") + 
  theme(plot.title = element_text(hjust = 0.5,color="black", size=25, face="bold"),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  scale_fill_gradient(low = "gray", high = "darkorchid4") + 
  labs(fill = "Counts",x = NULL,y = NULL) #+
 
P7<-ggplot(map3, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = X2012),colour = "white")+
  theme_bw()+
  ggtitle("2012") + 
  theme(plot.title = element_text(hjust = 0.5,color="black", size=25, face="bold"),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  scale_fill_gradient(low = "gray", high = "darkorchid4") + 
  labs(fill = "Counts",x = NULL,y = NULL) #+
 
P8<-ggplot(map3, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = X2013),colour = "white")+
  theme_bw()+
  ggtitle("2013") + 
  theme(plot.title = element_text(hjust = 0.5,color="black", size=25, face="bold"),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  scale_fill_gradient(low = "gray", high = "darkorchid4") + 
  labs(fill = "Counts",x = NULL,y = NULL) #+
 
P9<-ggplot(map3, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = X2014),colour = "white")+
  theme_bw()+
  ggtitle("2014") + 
  theme(plot.title = element_text(hjust = 0.5,color="black", size=25, face="bold"),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  scale_fill_gradient(low = "gray", high = "darkorchid4") + 
  labs(fill = "Counts",x = NULL,y = NULL) #+
 
P10<-ggplot(map3, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = X2015),colour = "white")+
  theme_bw()+
  ggtitle("2015") + 
  theme(plot.title = element_text(hjust = 0.5,color="black", size=25, face="bold"),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  scale_fill_gradient(low = "gray", high = "darkorchid4") + 
  labs(fill = "Counts",x = NULL,y = NULL) #+
 
P11<-ggplot(map3, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = X2016),colour = "white")+
  theme_bw()+
  ggtitle("2016") + 
  theme(plot.title = element_text(hjust = 0.5,color="black", size=25, face="bold"),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  scale_fill_gradient(low = "gray", high = "darkorchid4") + 
  labs(fill = "Counts",x = NULL,y = NULL) #+
 
P12<-ggplot(map3, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = X2017),colour = "white")+
  theme_bw()+
  ggtitle("2017") + 
  theme(plot.title = element_text(hjust = 0.5,color="black", size=25, face="bold"),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  scale_fill_gradient(low = "gray", high = "darkorchid4") + 
  labs(fill = "Counts",x = NULL,y = NULL) #+
 
P13_3<-ggplot(map3, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = X2018),colour = "white")+
  theme_bw()+
  ggtitle("Found dead") + 
  theme(plot.title = element_text(hjust = 0.5,color="black", size=25, face="bold"),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  scale_fill_gradient(low = "gray", high = "darkorchid4") + 
  labs(fill = "Counts",x = NULL,y = NULL) #+
 
require(gridExtra)
ga<-grid.arrange(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, ncol=3,nrow=4)
g <- arrangeGrob(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, ncol=3,nrow=4) #generates g

ggsave(file="figure3.tiff", g,dpi = 320,width = 60, height = 60, units = "cm")

#living_hours

P1<-ggplot(map4, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = X2006),colour = "white")+
  theme_bw()+
  ggtitle("2006") + 
  theme(plot.title = element_text(hjust = 0.5,color="black", size=25, face="bold"),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  scale_fill_gradient(low = "gray", high = "goldenrod1") + 
  labs(fill = "Median hours",x = NULL,y = NULL) #+
 
P2<-ggplot(map4, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = X2007),colour = "white")+
  theme_bw()+
  ggtitle("2007") + 
  theme(plot.title = element_text(hjust = 0.5,color="black", size=25, face="bold"),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  scale_fill_gradient(low = "gray", high = "goldenrod1") + 
  labs(fill = "Median hours",x = NULL,y = NULL) #+
  
P3<-ggplot(map4, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = X2008),colour = "white")+
  theme_bw()+
  ggtitle("2008") + 
  theme(plot.title = element_text(hjust = 0.5,color="black", size=25, face="bold"),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  scale_fill_gradient(low = "gray", high = "goldenrod1") + 
  labs(fill = "Median hours",x = NULL,y = NULL) #+
  
P4<-ggplot(map4, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = X2009),colour = "white")+
  theme_bw()+
  ggtitle("2009") + 
  theme(plot.title = element_text(hjust = 0.5,color="black", size=25, face="bold"),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  scale_fill_gradient(low = "gray", high = "goldenrod1") + 
   labs(fill = "Median hours",x = NULL,y = NULL) #+
 
P5<-ggplot(map4, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = X2010),colour = "white")+
  theme_bw()+
  ggtitle("2010") + 
  theme(plot.title = element_text(hjust = 0.5,color="black", size=25, face="bold"),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  scale_fill_gradient(low = "gray", high = "goldenrod1") + 
  labs(fill = "Median hours",x = NULL,y = NULL) #+
  
P6<-ggplot(map4, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = X2011),colour = "white")+
  theme_bw()+
  ggtitle("2011") + 
  theme(plot.title = element_text(hjust = 0.5,color="black", size=25, face="bold"),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  scale_fill_gradient(low = "gray", high = "goldenrod1") + 
  labs(fill = "Median hours",x = NULL,y = NULL) #+
  
P7<-ggplot(map4, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = X2012),colour = "white")+
  theme_bw()+
  ggtitle("2012") + 
  theme(plot.title = element_text(hjust = 0.5,color="black", size=25, face="bold"),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  scale_fill_gradient(low = "gray", high = "goldenrod1") + 
   labs(fill = "Median hours",x = NULL,y = NULL) #+
  
P8<-ggplot(map4, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = X2013),colour = "white")+
  theme_bw()+
  ggtitle("2013") + 
  theme(plot.title = element_text(hjust = 0.5,color="black", size=25, face="bold"),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  scale_fill_gradient(low = "gray", high = "goldenrod1") + 
  labs(fill = "Median hours",x = NULL,y = NULL) #+
  
P9<-ggplot(map4, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = X2014),colour = "white")+
  theme_bw()+
  ggtitle("2014") + 
  theme(plot.title = element_text(hjust = 0.5,color="black", size=25, face="bold"),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  scale_fill_gradient(low = "gray", high = "goldenrod1") + 
  labs(fill = "Median hours",x = NULL,y = NULL) #+
  
P10<-ggplot(map4, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = X2015),colour = "white")+
  theme_bw()+
  ggtitle("2015") + 
  theme(plot.title = element_text(hjust = 0.5,color="black", size=25, face="bold"),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  scale_fill_gradient(low = "gray", high = "goldenrod1") + 
  labs(fill = "Median hours",x = NULL,y = NULL) #+
  
P11<-ggplot(map4, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = X2016),colour = "white")+
  theme_bw()+
  ggtitle("2016") + 
  theme(plot.title = element_text(hjust = 0.5,color="black", size=25, face="bold"),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  scale_fill_gradient(low = "gray", high = "goldenrod1") + 
  labs(fill = "Median hours",x = NULL,y = NULL) #+
  
P12<-ggplot(map4, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = X2017),colour = "white")+
  theme_bw()+
  ggtitle("2017") + 
  theme(plot.title = element_text(hjust = 0.5,color="black", size=25, face="bold"),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  scale_fill_gradient(low = "gray", high = "goldenrod1") + 
  labs(fill = "Median hours",x = NULL,y = NULL) #+
  
P13_4<-ggplot(map4, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = X2018),colour = "white")+
  theme_bw()+
  ggtitle("Found alive (Hours)") + 
  theme(plot.title = element_text(hjust = 0.5,color="black", size=25, face="bold"),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  scale_fill_gradient(low = "gray", high = "goldenrod1") + 
  labs(fill = "Median hours",x = NULL,y = NULL) #+
  
require(gridExtra)
ga<-grid.arrange(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, ncol=3,nrow=4)
g <- arrangeGrob(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, ncol=3,nrow=4) #generates g
ggsave(file="figure4.tiff", g,dpi = 320,width = 60, height = 60, units = "cm")

#lifeless_hours

P1<-ggplot(map5, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = X2006),colour = "white")+
  theme_bw()+
  ggtitle("2006") + 
  theme(plot.title = element_text(hjust = 0.5,color="black", size=25, face="bold"),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  scale_fill_gradient(low = "gray", high = "firebrick3") + 
  labs(fill = "Median hours",x = NULL,y = NULL) #+
  
P2<-ggplot(map5, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = X2007),colour = "white")+
  theme_bw()+
  ggtitle("2007") + 
  theme(plot.title = element_text(hjust = 0.5,color="black", size=25, face="bold"),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  scale_fill_gradient(low = "gray", high = "firebrick3") + 
  labs(fill = "Median hours",x = NULL,y = NULL) #+
   
P3<-ggplot(map5, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = X2008),colour = "white")+
  theme_bw()+
  ggtitle("2008") + 
  theme(plot.title = element_text(hjust = 0.5,color="black", size=25, face="bold"),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  scale_fill_gradient(low = "gray", high = "firebrick3") + 
   labs(fill = "Median hours",x = NULL,y = NULL) #+
   
P4<-ggplot(map5, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = X2009),colour = "white")+
  theme_bw()+
  ggtitle("2009") + 
  theme(plot.title = element_text(hjust = 0.5,color="black", size=25, face="bold"),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  scale_fill_gradient(low = "gray", high = "firebrick3") + 
   labs(fill = "Median hours",x = NULL,y = NULL) #+
  
P5<-ggplot(map5, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = X2010),colour = "white")+
  theme_bw()+
  ggtitle("2010") + 
  theme(plot.title = element_text(hjust = 0.5,color="black", size=25, face="bold"),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  scale_fill_gradient(low = "gray", high = "firebrick3") + 
  labs(fill = "Median hours",x = NULL,y = NULL) #+
  
P6<-ggplot(map5, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = X2011),colour = "white")+
  theme_bw()+
  ggtitle("2011") + 
  theme(plot.title = element_text(hjust = 0.5,color="black", size=25, face="bold"),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  scale_fill_gradient(low = "gray", high = "firebrick3") + 
   labs(fill = "Median hours",x = NULL,y = NULL) #+
  
P7<-ggplot(map5, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = X2012),colour = "white")+
  theme_bw()+
  ggtitle("2012") + 
  theme(plot.title = element_text(hjust = 0.5,color="black", size=25, face="bold"),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  scale_fill_gradient(low = "gray", high = "firebrick3") + 
  labs(fill = "Median hours",x = NULL,y = NULL) #+
 
P8<-ggplot(map5, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = X2013),colour = "white")+
  theme_bw()+
  ggtitle("2013") + 
  theme(plot.title = element_text(hjust = 0.5,color="black", size=25, face="bold"),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  scale_fill_gradient(low = "gray", high = "firebrick3") + 
  labs(fill = "Median hours",x = NULL,y = NULL) #+
 
P9<-ggplot(map5, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = X2014),colour = "white")+
  theme_bw()+
  ggtitle("2014") + 
  theme(plot.title = element_text(hjust = 0.5,color="black", size=25, face="bold"),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  scale_fill_gradient(low = "gray", high = "firebrick3") + 
  labs(fill = "Median hours",x = NULL,y = NULL) #+
 
P10<-ggplot(map5, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = X2015),colour = "white")+
  theme_bw()+
  ggtitle("2015") + 
  theme(plot.title = element_text(hjust = 0.5,color="black", size=25, face="bold"),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  scale_fill_gradient(low = "gray", high = "firebrick3") + 
  labs(fill = "Median hours",x = NULL,y = NULL) #+
 
P11<-ggplot(map5, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = X2016),colour = "white")+
  theme_bw()+
  ggtitle("2016") + 
  theme(plot.title = element_text(hjust = 0.5,color="black", size=25, face="bold"),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  scale_fill_gradient(low = "gray", high = "firebrick3") + 
  labs(fill = "Median hours",x = NULL,y = NULL) #+
  
P12<-ggplot(map5, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = X2017),colour = "white")+
  theme_bw()+
  ggtitle("2017") + 
  theme(plot.title = element_text(hjust = 0.5,color="black", size=25, face="bold"),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  scale_fill_gradient(low = "gray", high = "firebrick3") + 
  labs(fill = "Median hours",x = NULL,y = NULL) #+
 
P13_5<-ggplot(map5, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = X2018),colour = "white")+
  theme_bw()+
  ggtitle("Found dead (Hours)") + 
  theme(plot.title = element_text(hjust = 0.5,color="black", size=25, face="bold"),panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  scale_fill_gradient(low = "gray", high = "firebrick3") + 
  labs(fill = "Median hours",x = NULL,y = NULL) #+
  
require(gridExtra)
ga<-grid.arrange(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, ncol=3,nrow=4)
g <- arrangeGrob(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, ncol=3,nrow=4) #generates g

ggsave(file="figure5.tiff", g,dpi = 320,width = 60, height = 60, units = "cm")

require(gridExtra)
za<-grid.arrange(P13_1, P13_2,P13_3,P13_4,P13_5, ncol=3,nrow=2)
z <- arrangeGrob(P13_1, P13_2,P13_3,P13_4,P13_5, ncol=3,nrow=2) #generates g

ggsave(file="comparison2018.tiff", z,dpi = 320,width = 120, height = 60, units = "cm")
