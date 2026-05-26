graph_C <- function(
  donnee,
  param,
  objReport = NULL,
  tacProj = NA,
  fProj = NA,
  valProj = NULL,
  Cmax = NA,
  langue = 'fr'
) {
  switch(
    langue,
    'fr' = {
      labAn <- 'Année'
      labDeb <- "Débarquements ('000t)"
      labF <- 'Taux instantané'
      lab.msy = 'RMS'
    },
    'en' = {
      labAn <- 'Year'
      labDeb <- "Landings ('000t)"
      labF <- 'Instantaneous rate'
      lab.msy = 'MSY'
    },
    'bil' = {
      labAn <- 'Année/Year'
      labDeb <- "Débarquements/Landings ('000t)"
      labF <- 'Taux instantané/Instantaneous rate'
      lab.msy = 'RMS/MSY'
    }
  )
  if (!any(is.na(tacProj)) | !any(is.na(fProj))) {
    nbAnProj <- max(length(tacProj[[1]]), length(fProj), na.rm = TRUE)
  } else {
    nbAnProj <- 0
  }
  ##
  ## if(nbAnProj > 0){
  ##     val <- projeter(objReport, tacProj, fProj)
  ## }else{
  ##     val <- list()
  ##     val$Cproj <- NA
  ## }
  plot(
    donnee$anneesFittees,
    objReport$Cpred / 1000,
    type = 'l',
    xlim = range(donnee$anneesFittees) + c(0, nbAnProj),
    ylim = c(0, max(2500, donnee$Cobs$valeur / 1000, Cmax, na.rm = TRUE)),
    xlab = labAn,
    ylab = labDeb
  )
  abline(h = 0, col = 'grey70')
  lines(donnee$Cobs[, 'annee'], donnee$Cobs[, 'valeur'] / 1000, col = 'pink')
  for (i in seq_along(donnee$Cobs[, 'valeur'])) {
    if (donnee$Cobs[i, 'annee'] %in% donnee$anneesFittees) {
      lines(
        rep(donnee$Cobs[i, 'annee'], 2),
        c(
          donnee$Cobs[i, 'valeur'] / 1000,
          objReport$Cpred[donnee$anneesFitteesID[
            donnee$anneesFittees == donnee$Cobs[i, 'annee']
          ]] /
            1000
        ),
        lty = 1,
        col = 2,
        lwd = 2
      )
    }
  }
  lines(donnee$anneesFittees, objReport$Cpred / 1000)
  if (!is.null(objReport$Cequilibre)) {
    #des points de référence peuvent être calculés
    abline(
      h = objReport$Cequilibre[which.max(objReport$Cequilibre)] / 1000,
      col = 2
    )
    axis(
      4,
      at = objReport$Cequilibre[which.max(objReport$Cequilibre)] / 1000,
      labels = 'msy',
      col.axis = 2,
      las = 1
    )
  }
  if (nbAnProj > 0) {
    for (i in seq_along(tacProj)) {
      points(
        max(donnee$anneesFittees) +
          seq(1, by = 1, length.out = length(valProj[[i]]$Bproj)),
        tacProj[[i]],
        pch = i,
        col = 1
      )
      lines(
        max(donnee$anneesFittees) +
          seq(0, by = 1, length.out = length(valProj[[i]]$Bproj) + 1),
        c(tail(objReport$Cpred, 1) / 1000, tacProj[[i]]),
        lty = 2
      )
    }
  }
  if (!is.null(objReport$Cequilibre)) {
    #des points de référence peuvent être calculés
    legend(
      'topleft',
      inset = 0.03,
      legend = paste(
        lab.msy,
        '=',
        c(round(objReport$Cequilibre[which.max(objReport$Cequilibre)] / 1000))
      )
    )
  }
  ## text(x=mean(par('usr')[c(1,2)]), y=diff(par('usr')[c(3,4)])*0.9 + par('usr')[3], labels='A', cex=1.5)
  ##
}
