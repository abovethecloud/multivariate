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
S<- round(cov(socio), 3)
S
R<- round(cor(socio), 3)
R

# Interpretazione della matrice R: lungo la diagonale trovo la correlazione delle variabili con s? stesse, quindi ? giusto che torni 1.
# La matrice ? giustamente diagonale. Si evidenzia una correlazione di 0.39 (in valore assoluto) tra V2vsV4 e V3vsV4,
# mentre V3 pare essere debolmente correlata a V2.

# Plotting the boxplots on the same scale
boxplot(socio$V2, socio$V3, socio$V4, names = c("V2", "V3", "V4"), main = "Boxplots")

# Plotting the boxplots with stretched axes
par(mfrow = c(1, 3))
boxplot(socio$V2, names = c("V2"), main = "Boxplot V2")
boxplot(socio$V3, names = c("V3"), main = "Boxplot V3")
boxplot(socio$V4, names = c("V4"), main = "Boxplot V4")

# Getting the values of outliers
boxplot.stats(socio$V2)$out
boxplot.stats(socio$V3)$out
boxplot.stats(socio$V4)$out

# Non sembrano esserci outliers univarati per quanto riguarda V2, mentre ne vengono rilevati uno a testa per V3 e V4
# Corrispondono alla 34esima osservazione per quanto riguarda V3 e alla 47esima per quanto riguarda V4.
# TODO: Automatically find the number of the observations (outliers)

par(mfrow= c(3,1))
qqnorm(socio$V2, main = "Q-Q plot for V2")
qqline(socio$V2, col = 2)
qqnorm(socio$V3, main = "Q-Q plot for V3")
qqline(socio$V3, col = 2)
qqnorm(socio$V4, main = "Q-Q plot for V4")
qqline(socio$V4, col = 2)

# Interpretazione dei normal plots: tutte e 3 le variabili paiono avere un andamento simil gaussiano, anche se vi sono problemi agli estremi.


col.index<-rep(1,61)
col.index[34]<-2; col.index[47]<-4
pairs(scale(socio), lower.panel = panel.cor, col = col.index, pch=16)


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

alpha<-0.05
qchisq(alpha,df=3)
norm_check<- sum(d<=qchisq(alpha,df=3))/n
pairs(scale(socio), lower.panel = panel.cor, upper.panel = panel.ellipse, el.level = 1-alpha, diag.panel=panel.hist)  
round(norm_check,3)

# norm_check si discosta poco dal 50%, il che tende a far accettare la normalit? multivariata.

par(mfrow= c(1,1))
plot(qchisq(ppoints(d), df=3), sort(d), pch=16, main = "Socio data", ylab = "d")
abline(0, 1, col = 2)

# Il pattern chi quadro sembra essere soddisfatto da tutte le osservazioni tranne 2, che si discostano sensibilmente.

round(d,3)

# Come previsto le osservazioni 34 e 47 hanno una distanza molto maggiore rispetto alla media, il che conferma
# il fatto che siano outliers. Notare che anche l'osservazione 6 ha una distanza insolitamente alta, sebbene non 
# quanto le altre 2.
# Inoltre si discostano dalle altre anche la 48esima e 49esima osservazione, con distanza di Malhanobis intorno all'8

col.index[6]<- 5; col.index[48]<-3; col.index[49]<-6
pairs(scale(socio), lower.panel = panel.cor, col = col.index, pch = 16)
round(sort(d), 3)
