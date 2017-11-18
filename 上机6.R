#9.20
bank0<-read.table("D:/R-3.3.3/JohnsonWichern Data sets/T1-5.DAT")
m<-as.matrix(bank0[,c(1,2,5,6)])
S<-cov(m)
lambda1<-eigen(S)$values[1]
lambda2<-eigen(S)$values[2]
e1<-eigen(S)$vectors[,1]
e2<-eigen(S)$vectors[,2]
LP1<-sqrt(lambda1)*e1
LP2<-sqrt(lambda2)*e2
V<-sqrt(diag(diag(S)))
LM1<-V%*%factanal(m,factors=1,rotation="none",method="mle")$loadings
LM2<-fa(m,nfactors=2,n.obs=42,rotate="none",SMC=FALSE,covar=TRUE,fm="ml")
fi1<-diag(diag(S))-diag(diag(LM1%*%t(LM1)))

#9.21
bank0<-read.table("D:/R-3.3.3/JohnsonWichern Data sets/T1-5.DAT")
m<-as.matrix(bank0)[,c(1,2,5,6)]
LPR<-principal(m,nfactors=2,rotate="varimax",covar=TRUE)
LMR<-fa(m,nfactors=2,n.obs=42,rotate="varimax",SMC=FALSE,covar=TRUE,fm="ml")

#9.22
bank0<-read.table("D:/R-3.3.3/JohnsonWichern Data sets/T1-5.DAT")
m<-as.matrix(bank0)[,c(1,2,5,6)]
score1<-fa(m,nfactors=2,n.obs=42,rotate="none",SMC=FALSE,scores="Bartlett",covar=TRUE,fm="ml")$scores
score2<-fa(m,nfactors=2,n.obs=42,rotate="none",SMC=FALSE,scores="regression",covar=TRUE,fm="ml")$scores
score3<-principal(m,nfactors=2,rotate="none",scores=TRUE,covar=TRUE)$scores

#9.23
bank0<-read.table("D:/R-3.3.3/JohnsonWichern Data sets/T1-5.DAT")
m<-as.matrix(bank0[,c(1,2,5,6)])
R<-cor(m)
lambda1<-eigen(R)$values[1]
lambda2<-eigen(R)$values[2]
e1<-eigen(R)$vectors[,1]
e2<-eigen(R)$vectors[,2]
LP1<-sqrt(lambda1)*e1
LP2<-sqrt(lambda2)*e2
LM1<-factanal(m,factors=1,rotation="none",method="mle")$loadings
LM2<-fa(m,nfactors=2,n.obs=42,rotate="none",SMC=FALSE,fm="ml")
fi1<-diag(diag(R))-diag(diag(LM1%*%t(LM1)))






