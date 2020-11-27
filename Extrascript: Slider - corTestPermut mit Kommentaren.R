# Beispiel: Permutationstests im Vergleich mit Pearson Korrelationstests

rm(list=ls())
require(mvtnorm)
set.seed(99999)

c <- 0.3
if (interactive()) {
  draw <- function(panel) {
    n <- 30                 #Groesse der einzelnen Stichprobe
    m <- 1000               #Anzahl der Permutationen in einer Stichprobe
    k <- 10                 #Anzahl von Einzelstichproben für den Vergleich
    
    corP <- rep(0,k)        #hier werden die Ergebnisse der k Korrelationstests gespeichert
    corPPermut <- rep(0,k)  #hier werden die Ergebnisse der k Permutationstests gespeichert
    
    for (i in 1:k){         #Schleife ueber die Einzelstichproben
      # Multivariate Normalverteilung -> generiert Daten ähnlich wie rnorm
      # Je Durchgang (1-k) ergeben sich neue Werte innerhalb der Bedingungen.
      xy <- rmvnorm(n, mean = c(0,0), sigma = cbind(c(1,panel$c),c(panel$c,1))) #Daten
      
      # Berechnet Korrelationstest der eben gezogenen Daten (einmalig)
      # xy[,1] ist die erste Spalte in xy und stellt quasi den x-Wert dar.
      # xy[,2]) ist der dazugehörige y-Wert. 
      # Durch die rmvnorm Bedingung sollten all diese zu ~0.3 korrelieren.
      ct <- cor.test(xy[,1],xy[,2])   #der Pearson Korrelationstest
      
      # Je Durchgang (1-k) ergibt sich genau ein p-Wert der Korrelation der diesmalig
      # gezogenen Daten.
      corP[i] <- ct$p.value #speichere den p value
      
      # Permutation:
      # Hier werden Korrelationen für m-Permutationen der Kriteriumswerte berechnet.
      # n steht hier für 30 Personen/Objekte.
      permutCors <- sapply(1:m,function(i) cor(xy[,1],sample(xy[,2],n)))
      # Der Anteil von Permut-Korrelationen, die dem Betrage nach groesser sind
      # als die Korrelation der Stichprobe, dient als p-Wert.
      corPPermut[i] <- sum(abs(permutCors)>abs(ct$estimate))/m
      
      #Ausgabe
      cat(paste("p-Wert des Korrelationstest: ",round(corP[i],4),"\np-Wert des Permutationstest: ",corPPermut[i],"\n"))
    }
    
    #der Vergleich von PearsonTest und Permutationstest als Plot
    par(mfrow=c(1,2))
    plot(corP,corPPermut,xlab="p value of cor.test",ylab = "p value of permutation test",main = "Comparison of PearsonTest and PermutationTest")
    abline(0,1,lty="dotted")                    #die Linie perfekter Uebereinstimmung
    text(0.6,0.2,paste("correlation of p values: ",round(cor(corP,corPPermut),4)))
    abline(lm(corPPermut~corP),col=2)           #die Regressionsgerade des pWert Vergleichs
    
    #die Prozentualen Abweichungen der p values der Permutationstests von den p values des PearsonTests
    plot(corP,100*(corPPermut/corP-1),xlab="p value of cor.test",ylab = "deviation of pPermut in %",main = "Percent Error of PermutationTest")
    abline(0,0,lty="dotted") #die Linie perfekter Uebereinstimmung
    dev.copy(pdf,"PearsonTestPermutTestComparison.pdf",width=12,height=7)
    dev.off()
    
    panel
  }
  panel <- rp.control(title='Regler') 
  rp.slider(panel,  c, 0.2, 0.5, showvalue = TRUE, resolution=0.1 , action = draw)
}

