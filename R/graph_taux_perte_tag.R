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
  tauxCummulPerte <- donnee$tauxPerte$simpleTag /
    (donnee$tauxPerte$simpleTag + 2 * donnee$tauxPerte$doubleTag)
  nbTot <- apply(
    donnee$tauxPerte[, c('doubleTag', 'simpleTag')],
    1,
    function(x) {
      x['simpleTag'] + 2 * x['doubleTag']
    }
  )
  plot(
    donnee$tauxPerte$tEnMer,
    tauxCummulPerte,
    cex = 3 * sqrt(nbTot / max(nbTot)),
    xlim = c(0, nrow(donnee$tauxPerte)),
    ylim = c(0, max(tauxCummulPerte)),
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
