library(ellipse)
library(MASS)
library(mvtnorm)
library(lattice)
library(SciViews)


mu<- c(1, -1, 2)
Sigma<-matrix(c(1, -1/2, -1/4, -1/2, 1, -1/4, -1/4, -1/4, 1), 3, 3)
Sigma; mu
X<- mvrnorm(n = 1000, mu, Sigma)
eigen_X<- eigen(Sigma)
round(eigen_X$values, 3)
round(eigen_X$vectors, 3)
C<-eigen_X$vectors
a1<- C[,1]
a2<- C[,2]
a3<- C[,3]
X_kde <- kde2d(X[,1], X[,2], n = 50) 
image(X_kde)
contour(X_kde, add = T) 

# Lo shape dei dati ? pi? o meno elittico, come c'era da aspettarsi da una normale multivariata. 
# Non conosco un altro modo per visualizzare densit? trivariate.

# Sappiamo gi? che la distribuzione di X3|(X1,X2)=(x1,x2) ? normale (probabilit?), resta da calcolarne media e varianza.
# X3|X1,X2 ha media 2-(1/2)x1-(1/2)x2 e varianza 3/4.
# X1,X2|X3=0 ha media t(3/2,-1/2) e varianza c(15/16,-9/16,-9/16,15/16) (si legge per colonne)

Sigma_1<- matrix(c(15/16,-9/16,-9/16,15/16),2,2)
mu_1<-c(3/2,-1/2)
alpha<-0.1
qchisq(alpha,df=2)
X_cond<-mvrnorm(n=1000,mu_1,Sigma_1)
one<- rep(1,1000)
D<- as.matrix(X_cond-one%*%t(mu_1))
d<- rep(0,1000)
for(i in 1:1000) d[i]<- c(t(D[i,])%*%solve(Sigma_1)%*%as.matrix(D[i,])) 
round(d,3)
norm_check<- sum(d<=qchisq(alpha,df=2))/1000
round(norm_check,3)
pairs(scale(X_cond), lower.panel = panel.cor, upper.panel = panel.ellipse1, el.level = 1-alpha, diag.panel=panel.hist, pch = 16)
lines(ellipse(Sigma_1,mu_1,0.9), col ="red", lwd=1.5)

