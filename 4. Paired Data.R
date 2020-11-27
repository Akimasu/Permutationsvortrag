####### Test auf gleiche Lageparameter in ABHÄNGIGEN Stichproben #######
library(coin) # ?oneway_test()

##### Festlegung der Grundwert #########################################
n <- 10                            # Anzahl Personen
id <- factor(rep(1:n, times=2))    # Faktor Personen-ID

## Testdaten
pre <- round(rnorm(n, 1, 2))      # Daten Zeitpunkt prä
post <- round(rnorm(n, 10, 2))     # Daten Zeitpunkt post
ges <- c(pre, post)                # Gesamt-Daten

## Festlegen des Faktors des Messzeitpunkts
# Die ersten 12 Werte werden als "pre", die letzten als "post" gelabelt.
group <- factor(rep(0:1, each=n), labels=c("pre", "post"))

## Durchführung des Testes
(ot <- oneway_test(ges ~ group | id, alternative="less",distribution="exact"))

### AUFGABE AN STUDENTEN
## Berechne den t-Wert des Testes und vergleiche diesen:
#t.test(____ ~ _____, alternative="______", paired=TRUE)$p.value
## Manipuliere die Daten,sodass p signifikant werden sollte.

## Plotten des Outputs

# Dichteverteilung der Teststatistik von oneway_test()
x11()
par(mfrow=c(1,2))
supp <- support(ot)
dens <- sapply(supp, dperm, object=ot) # Dichte
plot(supp, dens, xlab="Support", ylab=NA, pch=20, main="Dichte Permutationsverteilung")
# Q-Q-Plot der Teststatistik von oneway_test() gegen Standard-NV
qEmp <- sapply(ppoints(supp), qperm, object=ot) # Quantile Teststat.
qqnorm(qEmp, xlab="Quantile Normalverteilung",ylab="Permutations-Quantile",
        main="Permutations- vs. theoretische NV-Quantile")
abline(a=0, b=1, lwd=2, col="blue")


################################################################################################
### EXKURS: Manuell berechnen (Für Interessierte)

# alle 2^N Möglichk., pro Person Vorzeichen der Differenz zu permutieren
mw_diff <- pre - post                     # personenweise Messwertdifferenzen
ord_list <- lapply(numeric(n), function(x) { c(-1, 1) } )
ord_matrix <- data.matrix(expand.grid(ord_list)) # alle 2^N Permutationen

# für Gesamt-Permutation x der Vorzeichen: mittlere personenweise Diff.
get_mwD <- function(x) { mean(abs(mw_diff) * x) }
m_diff_star <- apply(ord_matrix, 1, get_mwD) # mittlere Differenzen alle Permut.
m_diff_base <- mean(mw_diff) # mittl. Differenz Basis-Stichprobe

tol <- .Machine$double.eps^0.5 # numerische Toleranz

# MDstar kleiner Basis-MD ODER MDstar ungefähr gleich Basis-MD?
MDsIsLEQ <- (m_diff_star < m_diff_base) | (abs(m_diff_star - m_diff_base) < tol)
# p-Wert: Anteil der mind. so extremen personenweisen Differenzen
(pVal <- sum(MDsIsLEQ) / length(m_diff_star))

