bank0<-read.table("D:/R-3.3.3/JohnsonWichern Data sets/T11-9.DAT")
x<-as.matrix(bank0)[,c(-1,-2,-11)]
y<-apply(x,2,as.numeric)
d<-dist(y,method="euclidean",diag=TRUE)
plot(hclust(d,"single"))
plot(hclust(d,"complete"))
