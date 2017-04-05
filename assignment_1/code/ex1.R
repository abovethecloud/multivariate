library(ellipse)
library(MASS)
library(mvtnorm)
library(lattice)
library(SciViews)
library(rgl)

# Reading the dataset
socio <- read.table("data/socio.txt")
socio <- socio[,c(2,3,4)]
socio$V2
names(socio)

# Taking the log of the variables
socio$V2 <- log(socio$V2) 
socio$V3 <- log(socio$V3)
socio$V4 <- log(socio$V4)

# Computing sample mean vector, covariance and correlation matrices
mean<- round(colMeans(socio), 3)
mean
S <- round(cov(socio), 3)
S
R <- round(cor(socio), 3)
R

# Interpretazione della matrice R: lungo la diagonale trovo la correlazione delle variabili con s? stesse, quindi ? giusto che torni 1.
# La matrice ? giustamente diagonale. Si evidenzia una correlazione di 0.39 (in valore assoluto) tra V2vsV4 e V3vsV4,
# mentre V3 pare essere debolmente correlata a V2.

# Plotting the boxplots on the same scale
boxplot(socio$V2, socio$V3, socio$V4, names = c("x2", "x3", "x4"), main = "Boxplots", col = "red", border = 1, outcol="blue", outpch = 16)
# Plotting the boxplots with stretched axes
par(mfrow = c(1, 3))
boxplot(socio$V2, names = c("V2"), main = "Boxplot x2", col = "red", border = 1, outcol="blue", outpch = 16)
boxplot(socio$V3, names = c("V3"), main = "Boxplot x3", col = "red", border = 1, outcol="blue", outpch = 16)
boxplot(socio$V4, names = c("V4"), main = "Boxplot x4", col = "red", border = 1, outcol="blue", outpch = 16)
# Getting the values of outliers
outV2 = boxplot.stats(socio$V2)$out
outV3 = boxplot.stats(socio$V3)$out
index_outV3 = which(socio$V3 == outV3, arr.ind = T)
outV4 = boxplot.stats(socio$V4)$out
index_outV4 = which(socio$V4 == outV4, arr.ind = T)

# Non sembrano esserci outliers univarati per quanto riguarda V2, mentre ne vengono rilevati uno a testa per V3 e V4
# Corrispondono alla 34esima osservazione per quanto riguarda V3 e alla 47esima per quanto riguarda V4.

par(mfrow= c(3,1))
qqnorm(socio$V2, main = "Q-Q plot for x2", pch = 16)
qqline(socio$V2, col = 2)
qqnorm(socio$V3, main = "Q-Q plot for x3", pch = 16)
qqline(socio$V3, col = 2)
qqnorm(socio$V4, main = "Q-Q plot for x4", pch = 16)
qqline(socio$V4, col = 2)

# Interpretazione dei normal plots: tutte e 3 le variabili paiono avere un andamento simil gaussiano, anche se vi sono problemi agli estremi.


col.index<-rep(1,61)
pch.index <- rep(16, 61)
cex.index <- rep(1, 61)
col.index[index_outV3]<-2; col.index[index_outV4]<-4
pch.index[index_outV3]<-15; pch.index[index_outV4]<-15
cex.index[index_outV3]<-1.2; cex.index[index_outV4]<-1.2


pairs(scale(socio), labels = c("x2", "x3", "x4"), lower.panel = NULL, col = col.index, pch=pch.index, cex = cex.index)


# Interpretazione degli scatterplots bivariati: la 34esima osservazione viene rilevata come outlier
# in V2vsV3 e in V3vsV4. La 47esima osservazione viene rilevata come outlier in V2vsV4 e in V3vsV4.
# Non sembrano esserci altri outliers (quanto meno non palesi).


n<- dim(socio)[1]
n
one<- rep(1,n)
one
D<- as.matrix(socio) - one%*%t(mean)
d<- rep(0,n) # Mahalanobis distance
for(i in 1:n) d[i]<- c(t(D[i,])%*%solve(S)%*%as.matrix(D[i,])) 
round(d,3)

panel.ellipse1_alpha <- function(x, y, ...){
  panel.ellipse(x, y, el.level = 1-alpha, ...)
}

alpha<-0.1
qchisq(alpha,df=3)
norm_check<- sum(d<=qchisq(alpha,df=3))/n
pairs(scale(socio), lower.panel = panel.cor, upper.panel = panel.ellipse1_alpha, diag.panel=panel.hist)  
round(norm_check,3)

# norm_check si discosta poco dal 50%, il che tende a far accettare la normalit? multivariata.

par(mfrow= c(1,1))
plot(qchisq(ppoints(d), df=3), sort(d), pch=16, main = "Chi-square Q-Q plot of Mahalanobis distance", ylab = "d")
abline(0, 1, col = 2)

# Il pattern chi quadro sembra essere soddisfatto da tutte le osservazioni tranne 2, che si discostano sensibilmente.

round(d,3)

# Come previsto le osservazioni 34 e 47 hanno una distanza molto maggiore rispetto alla media, il che conferma
# il fatto che siano outliers. Notare che anche l'osservazione 6 ha una distanza insolitamente alta, sebbene non 
# quanto le altre 2.
# Inoltre si discostano dalle altre anche la 48esima e 49esima osservazione, con distanza di Malhanobis intorno all'8
round(sort(d), 3)
col.index[6]<- 5; col.index[48]<-3; col.index[49]<-6
pairs(scale(socio), lower.panel = panel.cor, col = col.index, pch = 16)

plot3d(socio$V2, socio$V3, socio$V4, xlab = "x2", ylab = "x3", zlab = "x4", col = col.index, pch = pch.index, cex = cex.index)
# snapshot3d(nomefile.png)

bmatrix = function(x, digits=3, ...) {
  library(xtable)
  default_args = list(include.colnames=FALSE, only.contents=TRUE,
                      include.rownames=FALSE, hline.after=NULL, comment=FALSE,
                      print.results=FALSE)
  passed_args = list(...)
  calling_args = c(list(x=xtable(x, digits=digits)),
                   c(passed_args,
                     default_args[setdiff(names(default_args), names(passed_args))]))
  return(cat("\\begin{bmatrix}\n",
             do.call(print.xtable, calling_args),
             "\\end{bmatrix}"))
}