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
  plot(
    names(donnee$tauxPerte),
    donnee$tauxPerte,
    xlim = c(0, length(donnee$tauxPerte)),
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
