######################################################################################
#### Coin Test
#### 1. Legt eure Gruppen fest und rechnet den jeweiligen Part. Macht euch 3min
####    Gedanken, was genau passiert ist nach der Anpassung der Funktion.
#### 2. Tauscht die Gruppen und ändert die Standardwerte so ab, 
####    dass wir eine H1 rechnen.
#### 3. Ändert die "Sigma"-Werte und beschreibt, was passiert.

set.seed(99999)
### Setze Standardwerte (bekannt vom Termin am 15.05)
mu1 <- 1          # Gruppenwert 1
mu2 <- 10         # Gruppenwert 2
sigma1 <- 2        # Varianzenhomogenität für beide Gruppen
sigma2 <- 2
Nj    <- c(5, 5)  # Stichprobengrößen festlegen
DVa   <- round(rnorm(Nj[1], mu1, sigma1))
DVb   <- round(rnorm(Nj[2], mu2, sigma2))
DVab  <- c(DVa, DVb)                  # AV-Vektor (Verbindung von a und b)
IVbtw <- factor(rep(c("A", "B"), Nj)) # UV-Zuordnung zu den Gruppen (A,B)
# Boxplot der Basisgruppen zum Vergleich
boxplot(DVab~IVbtw,xlab="Gruppe",ylab="AV - Werte",ylim=c(0,mu2*2))

### Gruppe 1: Verwende verschiedene "distributions"
## Informiert euch über:
## ?oneway_test
## ?independence_test.formula
(ot <- oneway_test(DVab ~ IVbtw, alternative="less", distribution="approximate"))
# für Varianzeninhomogenität:
x11()
op <- par(mfrow=c(1,2), cex.lab=.8, cex.main=.6)
supp <- support(ot)
dens <- sapply(supp, dperm, object=ot)
plot(supp, dens, xlab="Support", ylab=NA, pch=20, main="Dichte der Permutationsverteilung")

qEmp <- sapply(ppoints(supp), qperm, object=ot)
plot(qEmp, ecdf(qEmp)(qEmp), col="gray60", pch=16,
     xlab="Mittelwertsdifferenzen", ylab="kumulierte relative Häufigkeit",
     main="Kumulierte relative Häufigkeiten und Verteilungsfunktion")

### FRAGE AN STUDENTEN:
## Was fällt euch auf beim Plot?


### Gruppe 2: Verwende verschiedene "alternative=.."
## Informiert euch über:
## ?independence_test.formula
(ot <- oneway_test(DVab ~ IVbtw, alternative="two.sided", distribution="approximate"))
# für Varianzeninhomogenität:
x11()
op <- par(mfrow=c(1,2), cex.lab=.8, cex.main=.6)
supp <- support(ot)
dens <- sapply(supp, dperm, object=ot)
plot(supp, dens, xlab="Support", ylab=NA, pch=20, main="Dichte der Permutationsverteilung")
qEmp <- sapply(ppoints(supp), qperm, object=ot)
plot(qEmp, ecdf(qEmp)(qEmp), col="gray60", pch=16,
     xlab="Mittelwertsdifferenzen", ylab="kumulierte relative Häufigkeit",
     main="Kumulierte relative Häufigkeiten und Verteilungsfunktion")
### FRAGE AN STUDENTEN:
## Ändert sich der p-Wert?

