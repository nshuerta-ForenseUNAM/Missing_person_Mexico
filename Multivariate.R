#Multivariant
#Cluster 
#Living_counts
#The data comes from the code generated in descriptive
#Required data: tabla2, tabla3, mat1 y mat2

library("NbClust")

r1 <- NbClust(tabla2, distance = "euclidean", min.nc = 2,max.nc = 10, method = "complete", index ="all")
tabla_cluster<-table(r1$Best.partition)
cluster<-as.numeric(max(names(tabla_cluster)))
d1 <- dist(tabla2, method = "euclidean") # 
fit1 <- hclust(d1, method="ward.D2")
plot(fit1,main=NULL,hang = -1, cex = 0.6) #
hcd <- as.dendrogram(fit1)
nodePar <- list(lab.cex = 0.8, pch = c(NA, 19), 
                cex = 0.8, col = "aquamarine4")
plot(hcd, ylab = "Height", nodePar = nodePar,horiz = FALSE,main="Found alive (Counts)")
groups1 <- cutree(fit1, k=cluster) 
rect.hclust(fit1, k=cluster, border="aquamarine4")


#Living_hours
r2 <- NbClust(mat1, distance = "euclidean", min.nc = 2,max.nc = 10, method = "complete", index ="all")
tabla_cluster<-table(r2$Best.partition)
cluster<-as.numeric(max(names(tabla_cluster)))
d2 <- dist(mat1, method = "euclidean") # 
fit2 <- hclust(d2, method="ward.D2")
plot(fit2,main=NULL,hang = -1, cex = 0.6) #
hcd <- as.dendrogram(fit2)
nodePar <- list(lab.cex = 0.8, pch = c(NA, 19), 
                cex = 0.8, col = "aquamarine4")
plot(hcd, ylab = "Height", nodePar = nodePar,horiz = FALSE,main="Found alive (Median Hours)")
groups1 <- cutree(fit2, k=cluster) 
rect.hclust(fit2, k=cluster, border="aquamarine4")

#Lifeless_counts
# Remove 0s
#https://www.rdocumentation.org/packages/NbClust/versions/3.0/topics/NbClust
#r3 <- NbClust(tabla3, distance = "euclidean", min.nc = 2,max.nc = 10, method = "complete", index ="all")
#Ward2
r3 <- NbClust(tabla3, distance = "euclidean", min.nc = 2,max.nc = 10, method = "ward.D2", index ="all")
tabla_cluster<-table(r3$Best.partition)
cluster<-as.numeric(max(names(tabla_cluster)))

dev.new()
d1 <- dist(tabla3, method = "euclidean") # 
fit1 <- hclust(d1, method="ward.D2")
plot(fit1,main=NULL,hang = -1, cex = 0.6) #
hcd <- as.dendrogram(fit1)
nodePar <- list(lab.cex = 0.8, pch = c(NA, 19),cex = 0.8, col = "darkorchid4")
plot(hcd, ylab = "Height", nodePar = nodePar,horiz = FALSE,main="Found dead (Counts)")
groups1 <- cutree(fit1, k=cluster) 
rect.hclust(fit1, k=cluster, border="darkorchid4")

dev.new()
#Lifeless_hours
mat21<-mat2[-18,]
r4 <- NbClust(mat21, distance = "euclidean", min.nc = 2,max.nc = 10, method = "complete", index ="all")
tabla_cluster<-table(r4$Best.partition)
cluster<-as.numeric(max(names(tabla_cluster)))

d2 <- dist(mat21, method = "euclidean") # 
fit2 <- hclust(d2, method="ward.D2")
plot(fit2,main=NULL,hang = -1, cex = 0.6) #
hcd <- as.dendrogram(fit2)
nodePar <- list(lab.cex = 0.8, pch = c(NA, 19), 
                cex = 0.8, col = "darkorchid4")
plot(hcd, ylab = "Height", nodePar = nodePar,horiz = FALSE,main="Found dead (Median Hours)")
groups1 <- cutree(fit2, k=cluster) 
rect.hclust(fit2, k=cluster, border="darkorchid4")

#-------------------------------------------------------------------------------------
#Canonical correlation analysis
#Living_counts_hours (state/years)
require(CCA)
library(corrplot)
library(RColorBrewer)

#Clusters 
dev.new()
correl1 <- matcor(t(tabla2), t(mat1))
img.matcor(correl1, type = 2)
M11<-correl1$Xcor
dev.new()
corrplot(M11, type="upper", tl.col="black",tl.cex=0.4,col=brewer.pal(n=8, name="RdYlBu"))

M12<-correl1$Ycor
dev.new()
corrplot(M12, type="upper", tl.col="black",tl.cex=0.4,col=brewer.pal(n=8, name="RdYlBu"))

M13<-correl1$XYcor
dev.new()
corrplot(M13, type="upper", tl.col="black",tl.cex=0.4,col=brewer.pal(n=8, name="RdYlBu"))

dev.new()
M131<-M13[1:32,33:64]
corrplot(M131, type="full", tl.col="black",tl.cex=0.6,method="circle",
         col= colorRampPalette(c("gray","white", "aquamarine4"))(10),title="Found alive (Counts & Median Hours)",mar=c(0,0,1,0))

#Lifeless_counts_hours (state/years)
mat21<-mat2[-18,]
dev.new()
correl2 <- matcor(t(tabla3), t(mat21))
img.matcor(correl2, type = 2)

M21<-correl2$Xcor
dev.new()
corrplot(M21, type="upper", tl.col="black",tl.cex=0.4,col=brewer.pal(n=8, name="RdYlBu"))

M22<-correl2$Ycor
dev.new()
corrplot(M22, type="upper", tl.col="black",tl.cex=0.4,col=brewer.pal(n=8, name="RdYlBu"))

M23<-correl2$XYcor
dev.new()
corrplot(M23, type="upper", tl.col="black",tl.cex=0.4,col=brewer.pal(n=8, name="RdYlBu"))
dev.new()
M231<-M23[1:31,32:62]
corrplot(M231, type="full", tl.col="black",tl.cex=0.6,method="circle",
         col= colorRampPalette(c("gray","white", "darkorchid4"))(10),title="Found dead (Counts & Median Hours)",mar=c(0,0,1,0))
