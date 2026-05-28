graph_taux_perte_tag <- function(donnee, objReport = NULL, langue = 'fr') {
  switch(
    langue,
    'fr' = {
      labAn <- 'Nombre années'
      labTxPerte <- 'Taux cumulatif de perte'
    },
    'en' = {
      labAn <- 'Number of year'
      labTxPerte <- 'Cumulative tag loss'
    },
    'bil' = {
      labAn <- 'Nb années/year'
      labTxPerte <- 'Tx cumul. perte/Cumul. tag loss'
    }
  )
  # tauxCummulPerte <- donnee$tauxPerte$simpleTag /
  #   (donnee$tauxPerte$simpleTag + 2 * donnee$tauxPerte$doubleTag)
  tEnMer <- sort(unique(donnee$tauxPerte$nbAnEnMer))
  prop.tEnMer <- rep(NA, length(tEnMer))
  nAuMoinsUnTag <- rep(NA, length(tEnMer))
  for (i.an in tEnMer) {
    temp <- subset(donnee$tauxPerte, nbAnEnMer == i.an)
    temp.table <- table(temp$nbTagRecap)
    prop.tEnMer[i.an] <- temp.table['1'] /
      (temp.table['1'] + 2 * temp.table['2'])
    nAuMoinsUnTag[i.an] <- sum(temp.table)
  }
  plot(
    tEnMer,
    prop.tEnMer,
    cex = 3 * sqrt(nAuMoinsUnTag / max(nAuMoinsUnTag)),
    xlim = c(0, max(tEnMer)),
    ylim = c(0, max(prop.tEnMer)),
    xlab = labAn,
    ylab = labTxPerte
  )
  curve(
    objReport$assymptoteTauxPerte *
      (1 - exp(-objReport$accroissementTauxPerte * x)),
    add = TRUE
  )
  text(
    x = 4,
    y = 0.3,
    labels = bquote(
      y ==
        .(round(objReport$assymptoteTauxPerte, 3)) *
          (1 - exp(.(round(objReport$accroissementTauxPerte, 3)) * x))
    ),
    cex = 1,
    col = "blue"
  )
  ## legend('topleft', inset=0.03, legend=paste(c('assymptote','accroissement'), round(c(obj$report()$assymptoteTauxPerte,obj$report()$accroissementTauxPerte),3), sep=': '))
}
