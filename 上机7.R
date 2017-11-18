#10.10
library(pracma)
R11<-matrix(c(1.0,0.615,0.615,1.0),2,2)
R12<-matrix(c(-0.111,-0.195,-0.266,-0.085),2,2)
R21<-t(R12)
R22<-matrix(c(1.0,-0.269,-0.269,1.0),2,2)
R<-sqrtm(R11)$Binv%*%R12%*%solve(R22)%*%R21%*%sqrtm(R11)$Binv
r1<-sqrt(eigen(R)$values[1])
r2<-sqrt(eigen(R)$values[2])
u1<-t(eigen(R)$vectors[,1])%*%sqrtm(R11)$Binv
v1<-t(1/r1*sqrtm(R22)$Binv%*%R21%*%sqrtm(R11)$Binv%*%eigen(R)$vectors[,1])%*%sqrtm(R22)$Binv

#10.12
library(pracma)
R11<-matrix(c(1.0,0.80,0.80,1.0),2,2)
R21<-matrix(c(0.26,0.67,0.34,0.33,0.59,0.34),3,2)
R12<-t(R21)
R22<-matrix(c(1.0,0.37,0.21,0.37,1.0,0.35,0.21,0.35,1.0),3,3)
R<-sqrtm(R11)$Binv%*%R12%*%solve(R22)%*%R21%*%sqrtm(R11)$Binv
r1<-sqrt(eigen(R)$values[1])
r2<-sqrt(eigen(R)$values[2])
n<-70
p<-2
q<-3
test1<--(n-1-(p+q+1)/2)*log((1-r1*r1)*(1-r2*r2))
x1<-qchisq(1-0.05,p*q)
test2<--(n-1-(p+q+1)/2)*log(1-r2*r2)
x2<-qchisq(1-0.05,(p-1)*(q-1))
u1<-t(eigen(R)$vectors[,1])%*%sqrtm(R11)$Binv
v1<-t(1/r1*sqrtm(R22)$Binv%*%R21%*%sqrtm(R11)$Binv%*%eigen(R)$vectors[,1])%*%sqrtm(R22)$Binv
r11<-u1%*%R11
r12<-u1%*%R12
r21<-v1%*%R21
r22<-v1%*%R22
p1<-r11%*%t(r11)/p
p2<-r22%*%t(r22)/q


