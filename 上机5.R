#8.10
bank0<-read.table("D:/R-3.3.3/JohnsonWichern Data sets/T8-4.DAT")
m<-as.matrix(bank0)
S<-cov(m)
eigen(S)
lambda1<-eigen(S)$values[1]
lambda2<-eigen(S)$values[2]
lambda3<-eigen(S)$values[3]
lambda4<-eigen(S)$values[4]
lambda5<-eigen(S)$values[5]
p1<-lambda1/(lambda1+lambda2+lambda3+lambda4+lambda5)
p2<-lambda2/(lambda1+lambda2+lambda3+lambda4+lambda5)
p3<-lambda3/(lambda1+lambda2+lambda3+lambda4+lambda5)
n<-103
lambda1min<-lambda1/(1+qnorm(1-0.1/(2*3))*sqrt(2/n))
lambda1max<-lambda1/(1-qnorm(1-0.1/(2*3))*sqrt(2/n))
lambda2min<-lambda2/(1+qnorm(1-0.1/(2*3))*sqrt(2/n))
lambda2max<-lambda2/(1-qnorm(1-0.1/(2*3))*sqrt(2/n))
lambda3min<-lambda3/(1+qnorm(1-0.1/(2*3))*sqrt(2/n))
lambda3max<-lambda3/(1-qnorm(1-0.1/(2*3))*sqrt(2/n))

#8.12
bank0<-read.table("D:/R-3.3.3/JohnsonWichern Data sets/T1-5.DAT")
m<-as.matrix(bank0)
S<-cov(m)
R<-cor(m)
eigen(S)
eigen(R)
p1<-eigen(S)$values[1]/sum(eigen(S)$values)
p2<-(eigen(R)$values[1]+eigen(R)$values[2]+eigen(R)$values[3])/sum(eigen(R)$values)

#8.14
bank0<-read.table("D:/R-3.3.3/JohnsonWichern Data sets/T5-1.DAT")
m<-as.matrix(bank0)
S<-cov(m)
p1<-eigen(S)$values[1]/sum(eigen(S)$values)
e1<-eigen(S)$vectors[,1]
Y1<-m%*%e1
qqnorm(Y1)

#8.17
bank0<-read.table("D:/R-3.3.3/JohnsonWichern Data sets/T1-8.DAT")
m<-as.matrix(bank0)
S<-cov(m)
eigen(S)
p<-(eigen(S)$values[1]+eigen(S)$values[2])/sum(eigen(S)$values)

#9.18
R<-matrix(c(1,0.4919,0.2636,0.4653,-0.2277,0.0652,0.4919,1,0.3127,0.3506,-0.1917,0.2045,0.2636,0.3127,1,0.4108,0.0647,0.2493,0.4653,0.3506,0.4108,1,-0.2249,0.2293,-0.2277,-0.1917,0.0647,-0.2249,1,-0.2144,0.0652,0.2045,0.2493,0.2293,-0.2144,1),6,6)
R1<-R[1:4,1:4]
lambda1<-eigen(R1)$values[1]
lambda2<-eigen(R1)$values[2]
e1<-eigen(R1)$vectors[,1]
e2<-eigen(R1)$vectors[,2]
LP1<-sqrt(lambda1)*e1
LP2<-sqrt(lambda2)*e2
LM1<-fa(R1,nfactors=1,n.obs=120,rotate="none",SMC=FALSE,fm="ml")
LM2<-fa(R1,nfactors=2,n.obs=120,rotate="none",SMC=FALSE,fm="ml")
LRa<-fa(R1,nfactors=2,n.obs=120,rotate="varimax",SMC=FALSE,fm="pa")
LRb<-fa(R1,nfactors=2,n.obs=120,rotate="varimax",SMC=FALSE,fm="ml")
eigen(R)
L1<-fa(R,nfactors=3,n.obs=120,rotate="none",SMC=FALSE,fm="pa")
L2<-fa(R,nfactors=3,n.obs=120,rotate="none",SMC=FALSE,fm="ml")
LR1<-fa(R,nfactors=3,n.obs=120,rotate="varimax",SMC=FALSE,fm="pa")
LR2<-fa(R,nfactors=3,n.obs=120,rotate="varimax",SMC=FALSE,fm="ml")


#9.19
bank0<-read.table("D:/R-3.3.3/JohnsonWichern Data sets/T9-12.DAT")
m<-as.matrix(bank0)
z<-scale(m,center=T,scale=T)
L2<-factanal(x=z,factors=2,scores="regression",rotation="none",method="mle")
L3<-factanal(x=z,factors=3,scores="regression",rotation="none",method="mle")
LR2<-factanal(x=z,factors=2,scores="regression",rotation="varimax",method="mle")
LR3<-factanal(x=z,factors=3,scores="regression",rotation="varimax",method="mle")
h2<-diag(as.matrix(L2$loadings)%*%t(as.matrix(L2$loadings)))
sigma2<-as.matrix(L2$loadings)%*%t(as.matrix(L2$loadings))+diag(L2$uniquenesses)
h3<-diag(as.matrix(L3$loadings)%*%t(as.matrix(L3$loadings)))
sigma3<-as.matrix(L3$loadings)%*%t(as.matrix(L3$loadings))+diag(L3$uniquenesses)
re2<-cor(z)-sigma2
re3<-cor(z)-sigma3
n<-50
p<-7
test2<-(n-1-(2*p+4*2+5)/6)*log(det(sigma2)/det(cov(z)))
test3<-(n-1-(2*p+4*3+5)/6)*log(det(sigma3)/det(cov(z)))
k2<-qchisq(0.99,((p-2)^2-p-2)/2)
k3<-qchisq(0.99,((p-3)^2-p-3)/2)
X<-c(110,98,105,15,18,20,35)
x<-(X-apply(m,2,mean))/sqrt(diag(cov(m)))
f1<-solve(t(L3$loadings)%*%solve(diag(h3))%*%L3$loadings)%*%t(L3$loadings)%*%solve(diag(h3))%*%x
f2<-solve(sqrt(3)+solve(t(L3$loadings)%*%solve(diag(h3))%*%L3$loadings))%*%f1


