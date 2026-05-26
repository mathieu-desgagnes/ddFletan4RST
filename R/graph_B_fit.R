#' Produit un graphique de l'ajustement de la biomasse aux indicateurs
#'
#' @param donnee
#' @param param
#' @param objReport
#' @param ylimLog
#' @param pl
#' @param plsd
#' @param residusStd
#' @param langue
#' @param labels
#' @param qLegende
#'
#' @returns
#' @export
#'
#' @examples
graph_B_fit <- function(
  donnee,
  param,
  objReport = NULL,
  ylimLog = c(-3.1, 3.1),
  pl = list(),
  plsd = list(),
  residusStd = TRUE,
  langue = 'fr',
  labels = '',
  qLegende = TRUE
) {
  switch(
    langue,
    'fr' = {
      labAn <- 'Année'
      labBiom <- "Biomasse ('000t)"
      labAP <- c('Brms', 'PRL', 'PRS')
      legendRelNom <- c(
        'Relevé chalut MPO',
        'Relevé palangre',
        'Relevé chalut GA',
        'Relevé chalut LH',
        'Relevé sentinelle NGSL',
        'PUE commericale'
      )
    },
    'en' = {
      labAn <- 'Year'
      labBiom <- "Biomass ('000t)"
      labAP <- c('Bmsy', 'LRP', 'USR')
      legendRelNom <- c(
        'DFO trawl survey',
        'Longline survey',
        'GA trawl survey',
        'LH trawl survey',
        'Sentinelle survey NGSL',
        'Commerical CPUE'
      )
    },
    'bil' = {
      labAn <- 'Année/Year'
      labBiom <- "Biomasse/Biomass ('000t)"
      labAP <- c('Brms/Bmsy', 'PRL/LRP', 'PRS/USR')
      legendRelNom <- c(
        'MPO/DFO',
        'Palangre/Longline',
        'GA',
        'LH',
        'Sen NGSL',
        'PUE/CPUE'
      )
    }
  )
  plot(
    donnee$anneesFittees,
    objReport$Bpred / 1000 / 1000,
    type = 'l',
    xlim = range(donnee$anneesFittees),
    ylim = c(
      0,
      max(
        objReport$Bpred,
        donnee$Bobs_abs$valeur / objReport$qRel_abs[donnee$Bobs_abs$source],
        donnee$Bobs$valeur / objReport$qRel[donnee$Bobs$source]
      )
    ) /
      1000 /
      1000,
    xlab = labAn,
    ylab = labBiom
  )
  if (length(plr) > 0) {
    polygon(
      c(donnee$anneesFittees, rev(donnee$anneesFittees)),
      c(plr$Bpred + 1.96 * plrsd$Bpred, rev(plr$Bpred - 1.96 * plrsd$Bpred)) /
        1000 /
        1000,
      border = NA,
      col = 'grey70'
    )
    ## lines(c(donnee$anneesFittees[1]-1,donnee$anneesFittees), plr$Bpred+1.96*plrsd$Bpred/1000/1000, lty=2, col=2)
    ## lines(c(donnee$anneesFittees[1]-1,donnee$anneesFittees), plr$Bpred-1.96*plrsd$Bpred/1000/1000, lty=2, col=2)
  }
  lines(c(donnee$anneesFittees), objReport$Bpred / 1000 / 1000, col = 'grey50')
  abline(h = 0, col = 'grey')
  if (!is.null(objReport$Cequilibre)) {
    #des points de référence peuvent être calculés
    abline(
      h = objReport$Bequilibre[which.max(objReport$Cequilibre)] / 1000 / 1000,
      lty = 2
    ) #Bmsy
    axis(
      4,
      at = objReport$Bequilibre[which.max(objReport$Cequilibre)] / 1000 / 1000,
      labels = labAP[1],
      las = 1
    )
    axis(
      4,
      at = objReport$Bequilibre[which.max(objReport$Cequilibre)] /
        1000 /
        1000 *
        c(0.4, 0.8),
      labels = labAP[2:3],
      las = 1
    )
    abline(
      h = objReport$Bequilibre[which.max(objReport$Cequilibre)] /
        1000 /
        1000 *
        c(0.4, 0.8),
      lty = 2,
      col = 2:3
    ) #Blim et Bsup
    ## axis(4, at=tail(objReport$Bpred/1000/1000,1), labels=round(tail(objReport$Bpred,1)/objReport$Bequilibre[which.max(objReport$Cequilibre)],2),
    ##      las=1)
    ## axis(4, at=c(0.5,1.5,2,2.5,3,3.5)*objReport$Bequilibre[which.max(objReport$Cequilibre)]/1000/1000, labels=c(0.5,1.5,2,2.5,3,3.5),
    ##      las=1)
  }
  for (i in unique(donnee$Bobs_abs$source)) {
    temp <- subset(donnee$Bobs_abs, source == i)
    temp <- temp[order(temp$annee), ]
    lines(
      temp$annee,
      temp$valeur / objReport$qRel_abs[i] / 1000 / 1000,
      col = i + 1,
      lwd = 0.5
    )
    points(
      temp$annee,
      temp$valeur / objReport$qRel_abs[i] / 1000 / 1000,
      pch = 21,
      bg = i + 1
    )
  }
  for (i in unique(donnee$Bobs$source)) {
    temp <- subset(donnee$Bobs, source == i)
    temp <- temp[order(temp$annee), ]
    lines(
      temp$annee,
      temp$valeur / objReport$qRel[i] / 1000 / 1000,
      col = i + 1 + unique(donnee$Bobs_abs$source),
      lwd = 0.5
    )
    points(
      temp$annee,
      temp$valeur / objReport$qRel[i] / 1000 / 1000,
      pch = 21,
      bg = i + 1 + unique(donnee$Bobs_abs$source)
    )
  }
  if (isTRUE(qLegende)) {
    legend(
      'topleft',
      inset = 0.03,
      legend = paste0(
        legendRelNom,
        ', q=',
        c(signif(c(objReport$qRel_abs, objReport$qRel), 3))
      ),
      lty = rep(1, length(c(objReport$qRel_abs, objReport$qRel))),
      pch = rep(21, length(c(objReport$qRel_abs, objReport$qRel))),
      pt.bg = 1:length(c(objReport$qRel_abs, objReport$qRel)) + 1
    )
  } else {
    legend(
      'topleft',
      inset = 0.03,
      legend = legendRelNom,
      lty = rep(1, length(c(objReport$qRel_abs, objReport$qRel))),
      pch = rep(21, length(c(objReport$qRel_abs, objReport$qRel))),
      pt.bg = 1:length(c(objReport$qRel_abs, objReport$qRel)) + 1
    )
  }
  text(
    x = mean(par('usr')[c(1, 2)]),
    y = diff(par('usr')[c(3, 4)]) * 0.7 + par('usr')[3],
    labels = labels,
    cex = 1.5
  )
}
