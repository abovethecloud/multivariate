library(ellipse)
library(MASS)
library(mvtnorm)
library(lattice)
library(SciViews)

socio<- read.table("data/socio.txt")
socio_pca<- prcomp(socio, scale = T)
round(socio_pca$rotation, 3)
pc12<-summary(socio_pca)$importance[3, c(1,2)]
pc12

# Le prime 2 componenti principali riescono a spiegare circa il 67% della varianza totale.

screeplot(socio_pca, type="lines", main="Screeplot")
abline(v = 3, col = 4, lty = 2, lwd = 2)
abline(v = 4, col = 2, lty = 2, lwd = 2)

# Lo screeplot conferma semplicemente quanto visto nel summary della PCA: le prime 3 componenti riescono a spiegare
# un abbondante 80% della varianza totale. Una riduzione a dimensione 3 ? quindi plausibile, ma si potrebbe anche
# considerarne una a 4, che garantirebbe un 95% di spiegazione della varianza totale (sconsigliabile).

