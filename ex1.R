library(ellipse)
library(MASS)
library(mvtnorm)
library(lattice)
library(SciViews)


socio<- read.table("socio.txt")
socio<- socio[,c(2,3,4)]
socio$V2
names(socio)
socio$V2<- log(socio$V2) 
socio$V3<- log(socio$V3)
socio$V4<- log(socio$V4) 
mean<- round(colMeans(socio), 3)
mean
S<- round(cov(socio), 3)
S
R<- round(cor(socio), 3)
R

# Interpretazione della matrice R: lungo la diagonale trovo la correlazione delle variabili con sè stesse, quindi è giusto che torni 1.
# La matrice è giustamente diagonale. Si evidenzia una correlazione di 0.39 (in valore assoluto) tra V2vsV4 e V3vsV4,
# mentre V3 pare essere debolmente correlata a V2.

boxplot(socio$V2, socio$V3, socio$V4, names = c("V2", "V3", "V4"), main = "Boxplots")
boxplot.stats(socio$V2)$out
boxplot.stats(socio$V3)$out
boxplot.stats(socio$V4)$out

# Non sembrano esserci outliers univarati per quanto riguarda V2, mentre ne vengono rilevati uno a testa per V3 e V4
# Corrispondono alla 34esima osservazione per quanto riguarda V3 e alla 47esima per quanto riguarda V4.

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
pairs(scale(socio), lower.panel = panel.cor, col = col.index)


# Interpretazione degli scatterplots bivariati: la 34esima osservazione viene rilevata come outlier
# in V2vsV3 e in V3vsV4. La 47esima osservazione viene rilevata come outlier in V2vsV4 e in V3vsV4.
# Non sembrano esserci altri outliers (quanto meno non palesi).


n<- dim(socio)[1]
n
one<- rep(1,n)
one
D<- as.matrix(socio-one%*%t(mean))
d<- rep(0,n)
for(i in 1:n) d[i]<- c(t(D[i,])%*%solve(S)%*%as.matrix(D[i,])) 
round(d,3)

alpha<-0.5
qchisq(alpha,df=3)
norm_check<- sum(d<=qchisq(alpha,df=3))/n
pairs(scale(socio), lower.panel = panel.cor, upper.panel = panel.ellipse, diag.panel=panel.hist)  
round(norm_check,3)

# norm_check si discosta poco dal 50%, il che tende a far accettare la normalità multivariata.

plot(qchisq(ppoints(d), df=3), sort(d), pch=16, main = "Socio data", ylab = "d")
abline(0, 1, col = 2)

# Il pattern chi quadro sembra essere soddisfatto da tutte le osservazioni tranne 2, che si discostano sensibilmente.

round(d,3)

# Come previsto le osservazione 34 3 47 hanno una distanza molto maggiore rispetto alla media, il che conferma
# il fatto che siano outliers. Notare che anche l'osservazione 6 ha una distanza insolitamente alta, sebbene non 
# quanto le altre 2.

col.index[6]<- 5
pairs(scale(socio), lower.panel = panel.cor, col = col.index)
