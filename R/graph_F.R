graph_F <- function(
  donnee,
  param,
  objReport = NULL,
  tacProj = NA,
  fProj = NA,
  valProj = NULL,
  Fmax = NA,
  pl = list(),
  plsd = list(),
  langue = 'fr'
) {
  switch(
    langue,
    'fr' = {
      labAn <- 'Année'
      labDeb <- "Débarquements ('000t)"
      labF <- 'Taux instantané'
      lab.fmsy = 'Frms'
    },
    'en' = {
      labAn <- 'Year'
      labDeb <- "Landings ('000t)"
      labF <- 'Instantaneous rate'
      lab.fmsy = 'Fmsy'
    },
    'bil' = {
      labAn <- 'Année/Year'
      labDeb <- "Débarquements/Landings ('000t)"
      labF <- 'Taux instantané/Instantaneous rate'
      lab.fmsy = 'Frms/Fmsy'
    }
  )
  if (!any(is.na(tacProj)) | !any(is.na(fProj))) {
    nbAnProj <- max(length(tacProj[[1]]), length(fProj), na.rm = TRUE)
  } else {
    nbAnProj <- 0
  }
  ##
  plot(
    donnee$anneesFittees,
    objReport$F,
    type = 'l',
    xlim = range(donnee$anneesFittees) + c(0, nbAnProj),
    ylim = c(0, max(0.3, Fmax, na.rm = TRUE)),
    xlab = labAn,
    ylab = labF
  )
  if (length(pl) > 0) {
    polygon(
      c(donnee$anneesFittees, rev(donnee$anneesFittees)),
      -log(
        1 -
          c(
            0.001 + 0.9 * plogis(pl$trans_TauxExp + 1.96 * plsd$trans_TauxExp),
            rev(
              0.001 + 0.9 * plogis(pl$trans_TauxExp - 1.96 * plsd$trans_TauxExp)
            )
          )
      ),
      border = NA,
      col = 'grey70'
    )
    ## lines(donnee$anneesFittees, 0.001 + 0.9*plogis(pl$trans_TauxExp+1.96*plsd$trans_TauxExp), lty=2, col=2)
    ## lines(donnee$anneesFittees, 0.001 + 0.9*plogis(pl$trans_TauxExp-1.96*plsd$trans_TauxExp), lty=2, col=2)
  }
  lines(donnee$anneesFittees, objReport$F, col = 'grey40')
  abline(h = 0, col = 'grey70')
  abline(h = objReport$M, col = 4, lwd = 2)
  axis(4, at = objReport$M, labels = 'M', col.axis = 4, las = 1)
  ## lines(donnee$anneesFittees[length(objReport$F)+c(-9,0)], rep(mean(tail(objReport$F,10)),2), col=3, lwd=3)
  ## for(i in 1){
  ##     temp <- subset(donnee$Bobs, source==i)
  ##     points(temp$annee,
  ##            1-exp(-donnee$Cobs[donnee$Cobs$annee%in%temp$annee,'valeur']/(temp$valeur)), pch=21, bg=i+1)
  ##            ## 1-exp(-donnee$Cobs[donnee$Cobs$annee%in%temp$annee,'valeur']/(temp$valeur/objReport$qRel[i])), pch=21, bg=i+1)
  ##            ## 1-exp(-donnee$Cobs[donnee$Cobs$annee%in%temp$annee,'valeur']/(temp$valeur/objReport$qRel[i]*exp(-objReport$M))), pch=21, bg=i+1)
  ## }
  ## if(!is.null(objReport$nTagCapt)) points(donnee$anneesFittees, 1-exp(-(objReport$NtagCapt/objReport$tauxRetour)/c(0,tail(objReport$Ntag,-1))),
  ##                                         pch=21, bg=5)
  ## nbAn <- 10
  anneesPosees <- sort(unique(donnee$nTagsRetourObs$anneePose))
  for (i.tag in seq_along(anneesPosees)) {
    ## for(i.tag in 1:nrow(donnee$nTagsRetourObs)){
    nTag <- objReport$nTag1[donnee$anneesFittees == anneesPosees[i.tag], ] +
      objReport$nTag2[donnee$anneesFittees == anneesPosees[i.tag], ]
    lesquels <- which(donnee$nTagsRetourObs$anneePose == anneesPosees[i.tag])
    U.temp <- donnee$nTagsRetourObs[lesquels, 'valeur'] /
      (nTag[
        donnee$anneesFittees %in%
          (donnee$nTagsRetourObs[lesquels, 'anneeRecap'] - 1)
      ] *
        exp(-objReport$M) *
        donnee$tauxRetour)
    F.temp <- 1 - exp(-U.temp)
    lines(
      donnee$nTagsRetourObs[lesquels, 'anneeRecap'],
      F.temp,
      type = 'o',
      col = anneesPosees[i.tag] + 1
    )
  }
  if (!is.null(objReport$Cequilibre)) {
    #des points de référence peuvent être calculés
    legend(
      'topright',
      inset = 0.03,
      legend = paste(
        c('M=', paste0(lab.fmsy, '=')),
        c(
          round(objReport$M, 3),
          round(
            -log(1 - objReport$Uequilibre[which.max(objReport$Cequilibre)]),
            3
          )
        )
      ),
      lty = c(1, 1, 1, NA, NA),
      col = c(4, 3, 2, NA, NA),
      lwd = c(2, 2, 3, NA, NA)
    )
    abline(
      h = -log(1 - objReport$Uequilibre[which.max(objReport$Cequilibre)]),
      col = 3,
      lwd = 2
    )
    axis(
      4,
      at = -log(1 - objReport$Uequilibre[which.max(objReport$Cequilibre)]),
      labels = lab.fmsy,
      col.axis = 3,
      las = 1
    )
  } else {
    legend(
      'topright',
      inset = 0.03,
      legend = paste(c('M=', 'F 10ans'), c(round(objReport$M, 3), ' ')),
      lty = c(1, 1, NA, NA),
      col = c(4, 3, NA, NA),
      lwd = c(2, 3, NA, NA)
    )
  }
  ## legend('topleft', inset=0.03, legend=c('Deb/Releve','tags'), pch=rep(16,2), col=c(2,5))
  ## axis(4, at=mean(tail(objReport$F,10)), labels=round(mean(tail(objReport$F,10)),4), col.lab=3)
  if (nbAnProj > 0) {
    for (i in seq_along(tacProj)) {
      points(
        max(donnee$anneesFittees) +
          seq(1, by = 1, length.out = length(valProj[[i]]$fProj)),
        valProj[[i]]$fProj,
        pch = i,
        col = 1
      )
      lines(
        max(donnee$anneesFittees) +
          seq(0, by = 1, length.out = length(valProj[[i]]$fProj) + 1),
        c(tail(objReport$F, 1), valProj[[i]]$fProj),
        lty = 2
      )
    }
  }
  lines(donnee$anneesFittees, objReport$F)
  text(
    x = mean(par('usr')[c(1, 2)]),
    y = diff(par('usr')[c(3, 4)]) * 0.7 + par('usr')[3],
    labels = 'D',
    cex = 1.5
  )
}
