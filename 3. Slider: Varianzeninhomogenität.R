#### Slider: Ändere aktive einen Wert, der Sigma 1 und 2 variiert 

### FRAGE AN STUDENTEN:
# Was lässt sich beobachten?

if (interactive()) {
  draw <- function(panel) {
    mu1 <- 100  
    mu2 <- 100
    Nj    <- c(5, 5) # Gruppengrößen
    ### simuliert Stichprobenziehen aus einer Normalverteilung
    #sigma1 <- 2
    DVa   <- round(rnorm(Nj[1], mu1, panel$sigma))
    DVb   <- round(rnorm(Nj[2], mu2, panel$sigma2))
    ### simuliert experimentelle Zuordnung zu der UV
    DVab  <- c(DVa, DVb) #AV
    IVbtw <- factor(rep(c("A", "B"), Nj)) ## UV Zuordnung zu den Gruppen
    ##### -> jetzt permutationstest
    (ot <- oneway_test(DVab ~ IVbtw, alternative="less", distribution="approximate"))
    # für Varianzeninhomogenität:
    op <- par(mfrow=c(1,2), cex.lab=.8, cex.main=.6)
    supp <- support(ot)
    dens <- sapply(supp, dperm, object=ot)
    plot(supp, dens, xlab="Support", ylab=NA, pch=20, main="Dichte der Permutationsverteilung")
    qEmp <- sapply(ppoints(supp), qperm, object=ot)
    plot(qEmp, ecdf(qEmp)(qEmp), col="gray60", pch=16,
         xlab=paste("Mittelwertsdifferenzen; sigma2 = ",panel$sigma), ylab="kumulierte relative Häufigkeit",
         main="Kumulierte relative Häufigkeiten und Verteilungsfunktion")
    
    panel
  }
  panel <- rp.control(title='Regler') 
  rp.slider(panel,  sigma, 2,10, showvalue = TRUE, resolution=1 , action = draw)
  rp.slider(panel,  sigma2, 2,10, showvalue = TRUE, resolution=1 , action = draw)
  
}
