###### Vorbereitung #########################################################################
## Installiere und Lade Packages
# install.packages("rpanel")
# library(rpanel)
# install.packages("coin")
library(coin)
# leere liste
# rm(list=ls())      # Setzt ideales Modell (Y zu X-Werten s.o.) -> tatsächliche Parameter
set.seed(99999)
#########################################################################################

########################################################################################
####  Übung1: Simulation eines t-Tests unter H0 auf klassischem Weg
# Insgesamt soll 1000 mal simuliert werden, d.h. wir führen 1000 t-test durch. Wobei
# wir immer vom mu der Gruppe ausgehen und einen Fehler hinzufügen.
# Im Endeffekt bilden wir 1000 Stichproben, die sich im Rahmen von mu und Sigma
# in einer Normalverteilung befinden.
m <- 1000       # Anzahl der Simulationswiederholungen
# Gruppenwerte
mu1 <- 10         # Gruppenwert 1
mu2 <- 10         # Gruppenwert 2
# Stichprobengrößen festlegen
Nj    <- c(5, 5)  
# DVab  entspricht x
sigma1 <- 3
sigma2 <- 3
# Sigma wird auf 0 gesetzt, da sonst immer derselbe p-Wert errechnet wird
DVa   <- round(rnorm(Nj[1], mu1, 0))
DVb   <- round(rnorm(Nj[2], mu2, 0))
DVab  <- c(DVa, DVb)  
# Simulation: experimentelle Zuordnung zu der UV -> Gruppenzuordnung
# UV-Zuordnung zu den Gruppen (A,B)
IVbtw <- factor(rep(c("A", "B"), Nj)) 
# Berechnung für einen Durchlauf: 
# Was würde so passieren: t.test(DVab~IVbtw,alternative="less",var.equal=TRUE)$p.value
t.test(DVab+c(rnorm(5, 0, sigma1),rnorm(5, 0, sigma2)) ~ IVbtw, alternative="less", var.equal=TRUE)$p.value
# less -> daher sollte A < B sein, damit H1 angenommen wird aufgrund des p-Wertes
# Alternativ: Repliziere dies mit dem Fehlerterm
p <- replicate(m, t.test(DVab+c(rnorm(5, 0, sigma1),rnorm(5, 0, sigma2)) ~ IVbtw, alternative="less", var.equal=TRUE)$p.value)
# rnorm(1,0,sigma)
# x11()
hist(p)
# Welches Ergebnis erwartet ihr?
mean(p<0.05) 
# Prozentsatz der nicht so extremen Werte:
100-mean(p<0.05)*100

### FRAGE AN STUDENTEN:
# Welches Ergebnis erwarten wir bei H1?
# Was passiert wenn ich sigma1/2 unter H1 verändere?
# Was unterscheidet dieses Vorgehen vom Permutationstest?

######################################################################################
#### Übung 2: 2-SP t-Test über Permutation ###########################################
mu1 <- 10         # Gruppenwert 1
mu2 <- 10         # Gruppenwert 2
sigma1 <- 3        # Varianzenhomogenität für beide Gruppen
sigma2 <- 3
Nj    <- c(5, 5)  # Stichprobengrößen festlegen
### Simulation: Stichprobenwerte ziehen aus einer Normalverteilung
DVa   <- round(rnorm(Nj[1], mu1, sigma1))
DVb   <- round(rnorm(Nj[2], mu2, sigma2))
### Simulation: experimentelle Zuordnung zu der UV -> Gruppenzuordnung
DVab  <- c(DVa, DVb)                  # AV-Vektor (Verbindung von a und b)
IVbtw <- factor(rep(c("A", "B"), Nj)) # UV-Zuordnung zu den Gruppen (A,B)

### Bildliche Darstellung der Varianzen
#x11()
boxplot(DVab~IVbtw,xlab="Gruppe",ylab="AV - Werte",ylim=c(0,20))

### Berechne den normalen ttest:
t.test(DVab ~ IVbtw, alternative="less", var.equal=TRUE)$p.value
t.test(DVab ~ IVbtw, alternative="less", var.equal=TRUE)$statistic

# Setze Indizes für oben festgelegte Daten (1-10)
idx      <- seq(along=DVab)   #Indizes Gesamtdaten
idxA     <- combn(idx, Nj[1]) #alle n1-Kombinationen für Gruppe A
# Mittelwertsdifferenz für gegebene Indizes x der Gruppe A*
# Mittelwertsdifferenz für alle unterschiedlichen Zusammensetzungen
getDM    <- function(x) {mean(DVab[x]) - mean(DVab[!(idx %in% x)])}
# 1. Teil: DVab[idxA[,1]] sind die Werte aus der idxA Spalte x.
#          Mittwelwert Gruppe A (rausgepickt aus den 
#          1-10 Indizes, hauptsache nur 5)
# 2. Teil: minus den Mittelwert des Restes, der nicht ausgewählt wurde 
#         (Komplementärmenge).
# DMstar wendet diese Funktion an und setzt die jeweiligen idxAs ein.
DMstar   <- apply(idxA, 2, getDM)   # Anzahl Berechnungen: length(idxA[1,])
# Mittelwertsdifferenz der Basisstichprobe:
DMbase   <- mean(DVa) - mean(DVb)
###p-Wert: Anteil der mind. so extremen Mittelwertsdifferenzen
# (wichtig: Sollte in Beträgen gerechnet werden)
(pVal    <- sum(DMstar <= DMbase) / length(DMstar))
# Summe liegt zwischen 0 und length(DMstar)
# Wenn alle kleiner sind, dann = 1; ansonsten zu 0

x11()
op <- par(mfrow=c(1,2), cex.lab=.8, cex.main=.6)
hist(DMstar, freq=FALSE, breaks="FD", xlab="Mittelwertsdifferenzen",
     ylab = "Dichte",
     main="Permutationstest: Histogramm Mittelwertsdifferenzen")
curve(dnorm(x, mean(DMstar), sd(DMstar)), lwd=2, col="blue", add=TRUE)
plot(DMstar, ecdf(DMstar)(DMstar), col="gray60", pch=16,
     xlab="Mittelwertsdifferenzen", ylab="kumulierte relative Häufigkeit",
     main="Kumulierte relative Häufigkeiten und Verteilungsfunktion")
curve(pnorm(x, mean(DMstar), sd(DMstar)),col="blue", lwd=2, add=TRUE)

######################################################################################
#Verkürzte Variante:
mu1 <- 10         # Gruppenwert 1
mu2 <- 10         # Gruppenwert 2
sigma1 <- 3       # Varianzenhomogenität für beide Gruppen
sigma2 <- 3
Nj    <- c(5, 5)  # Stichprobengrößen festlegen
DVa   <- round(rnorm(Nj[1], mu1, sigma1))
DVb   <- round(rnorm(Nj[2], mu2, sigma2))
DVab  <- c(DVa, DVb)                  # AV-Vektor (Verbindung von a und b)
IVbtw <- factor(rep(c("A", "B"), Nj)) # UV-Zuordnung zu den Gruppen (A,B)

(ot <- oneway_test(DVab ~ IVbtw, alternative="less", distribution="exact"))
# für Varianzeninhomogenität:
#alt <- c(rep(mu1,5),rep(mu2,5))+c(rnorm(5, 0, sigma1),rnorm(5, 0, sigma2))
#(ot <- oneway_test(alt ~ IVbtw, alternative="less", distribution="exact"))
x11()
op <- par(mfrow=c(1,2), cex.lab=.8, cex.main=.6)
supp <- support(ot)
dens <- sapply(supp, dperm, object=ot)
plot(supp, dens, xlab="Support", ylab=NA, pch=20, main="Dichte der Permutationsverteilung")
qEmp <- sapply(ppoints(supp), qperm, object=ot)
plot(qEmp, ecdf(qEmp)(qEmp), col="gray60", pch=16,
     xlab="Mittelwertsdifferenzen", ylab="kumulierte relative Häufigkeit",
     main="Kumulierte relative Häufigkeiten und Verteilungsfunktion")

### Berechne den normalen ttest:
t.test(DVab ~ IVbtw, alternative="less", var.equal=TRUE)$p.value
t.test(DVab ~ IVbtw, alternative="less", var.equal=TRUE)$statistic

### FRAGE AN STUDENTEN
# Wie kann ich im Plot Kumulierte relative Häufigkeiten und Verteilungsfunktion 
# den p-Wert ablesen?


# EXKURS: t-Werte dieser Permutationen errechnen 
#         Wie viele t-Wert sind extremer als ursprünglicher? -> p.Wert
#         Literatur: Yu (2003)
# Base t-wert:
t_base <- t.test(DVab ~ IVbtw, alternative="less", var.equal=TRUE)$statistic
# t-Werte der Permutationen
get_tval    <- function(x) {t.test(c(DVab[x],DVab[!(idx %in% x)]) ~ IVbtw, alternative="less", var.equal=TRUE)$statistic}
# t-test mit Vektorbeispiel: c(DVab[idxA[,6]],DVab[!(idx %in% idxA[,6])])
t_perm <- apply(idxA, 2, get_tval)
(p_perm    <- sum(t_perm <= t_base) / length(t_perm)) 
# Obacht: gilt bei Hypothese "less" (t.emp>t.krit) 
# ---> H0: wenn viele t_perm kleiner sind als t_base -> pVal nähert sich 1 an

# Plotten zum Ablesen:
x11()
plot(t_perm, ecdf(t_perm)(t_perm), col="gray60", pch=16,
     xlab="t_perm", ylab="kumulierte ",
     main="Kumulierte relative Häufigkeiten und Verteilungsfunktion")
abline(v=t_base,col='blue',lwd=2)
abline(h=pVal,col='blue',lwd=2)