graph_B_residus <- function(
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
      labRes <- 'Résidus standardisés'
      labResNstd <- 'Résidus'
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
      labRes <- 'Standardized residuals'
      labResNstd <- 'Residuals'
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
      labRes <- 'Résidus/Residuals'
      labResNstd <- 'Résidus/Residuals'
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
  ##
  ## erreur d'observation
  if (is.null(ylimLog)) {
    ## à valider... et optimiser pour tenir compte de tous les indices de biomasse possibles
    ylimLog <- range(c(
      log(subset(donnee$Bobs, source == 1)$valeur / objReport$qRel[1]) -
        log(objReport$Bpred[
          c(donnee$anneesFittees[1] - 1, donnee$anneesFittees) %in%
            subset(donnee$Bobs, source == 1)$annee
        ]),
      log(subset(donnee$Bobs, source == 2)$valeur / objReport$qRel[2]) -
        log(objReport$Bpred[
          c(donnee$anneesFittees[1] - 1, donnee$anneesFittees) %in%
            subset(donnee$Bobs, source == 2)$annee
        ]),
      log(subset(donnee$Bobs, source == 3)$valeur / objReport$qRel[3]) -
        log(objReport$Bpred[
          c(donnee$anneesFittees[1] - 1, donnee$anneesFittees) %in%
            subset(donnee$Bobs, source == 3)$annee
        ])
    ))
  }
  valUnique <- unique(donnee$Bobs_abs$source)
  for (i in valUnique) {
    Bobs.temp <- subset(donnee$Bobs_abs, source == i)
    plot(
      Bobs.temp$annee,
      log(Bobs.temp$valeur / objReport$qRel_abs[i]) -
        (log(objReport$Bpred)[match(
          Bobs.temp$annee + 1,
          donnee$anneesFittees
        )]),
      type = 'n',
      xlim = range(donnee$anneesFittees),
      ylim = ylimLog,
      xlab = labAn,
      ylab = labRes
    )
    Bres.ylim <- par('usr')[3:4]
    abline(h = 0, col = 'grey')
    for (j in 1:nrow(Bobs.temp)) {
      annee <- Bobs.temp[j, 'annee']
      if (annee %in% donnee$anneesFittees) {
        if (isTRUE(residusStd)) {
          lines(
            rep(annee, 2),
            c(
              0,
              log(Bobs.temp[j, 'valeur'] / objReport$qRel_abs[i]) -
                log(objReport$Bpred[donnee$anneesFitteesID[
                  donnee$anneesFittees == annee
                ]])
            ) /
              objReport$sigma_Bobs_abs[Bobs.temp[j, 'sigma']],
            lwd = 2,
            col = i + 1
          )
          points(
            annee,
            (log(Bobs.temp[j, 'valeur'] / objReport$qRel_abs[i]) -
              log(objReport$Bpred[donnee$anneesFitteesID[
                donnee$anneesFittees == annee
              ]])) /
              objReport$sigma_Bobs_abs[Bobs.temp[j, 'sigma']],
            pch = 21,
            bg = i + 1
          )
        } else {
          ##
          ## à valider
          ##
          ## lines(rep(annee+seq(-0.4,0.4,length.out=length(unique(donnee$Bobs$source)))[i], 2),
          ##       c(0,log(Bobs.temp[j,'valeur']/objReport$qRel[i])-log(objReport$Bpred[donnee$anneesFitteesID[donnee$anneesFittees==annee]])),
          ##       lwd=2, col=i+1)
          ## points(annee+seq(-0.4,0.4,length.out=length(unique(donnee$Bobs$source)))[i],
          ## (log(Bobs.temp[j,'valeur']/objReport$qRel[i])-log(objReport$Bpred[donnee$anneesFitteesID[donnee$anneesFittees==annee]])),
          ## pch=21, bg=i+1)
        }
      }
    }
    for (i.sig in unique(Bobs.temp$sigma)) {
      temp2 <- subset(Bobs.temp, sigma == i.sig)
      if (all(temp2[, 'valeur'] > 0)) {
        if (isTRUE(residusStd)) {
          ## fit.temp <- lm(val~an, data=list(an=temp2[,'annee']+seq(-0.4,0.4,length.out=length(unique(donnee$Bobs$source)))[i],
          ##                                  val=(log(temp2[,'valeur']/objReport$qRel[i])-
          ##                                       log(objReport$Bpred[donnee$anneesFitteesID[donnee$anneesFittees%in%temp2$annee]+1]))/objReport$sigma_Bobs[i.sig]))
          ## lines(range(temp2[,'annee'])+seq(-0.4,0.4,length.out=length(unique(donnee$Bobs$source)))[i], c(predict(fit.temp)[1], tail(predict(fit.temp),1)), col=i+1, lwd=3)
          lines(
            smooth.spline(
              list(
                x = temp2[, 'annee'],
                y = (log(temp2[, 'valeur'] / objReport$qRel_abs[i]) -
                  log(objReport$Bpred[donnee$anneesFitteesID[
                    donnee$anneesFittees %in% temp2$annee
                  ]])) /
                  objReport$sigma_Bobs_abs[i.sig]
              ),
              df = 2.4
            ),
            col = i + 1,
            lwd = 3
          )
        } else {
          ##
          ## à valider
          ##
          ## fit.temp <- lm(val~an, data=list(an=temp2[,'annee']+seq(-0.4,0.4,length.out=length(unique(donnee$Bobs$source)))[i],
          ##                                  val=(log(temp2[,'valeur']/objReport$qRel[i])-
          ##                                       log(objReport$Bpred[donnee$anneesFitteesID[donnee$anneesFittees%in%temp2$annee]+1]))))
          ## lines(range(temp2[,'annee'])+seq(-0.4,0.4,length.out=length(unique(donnee$Bobs$source)))[i], c(predict(fit.temp)[1], tail(predict(fit.temp),1)), col=i+1, lwd=3)
        }
      }
    }
  }
  ##
  ## autre relevés
  valUnique <- unique(donnee$Bobs$source)
  for (i in valUnique) {
    Bobs.temp <- subset(donnee$Bobs, source == i)
    plot(
      Bobs.temp$annee,
      log(Bobs.temp$valeur / objReport$qRel[i]) -
        (log(objReport$Bpred)[match(
          Bobs.temp$annee + 1,
          donnee$anneesFittees
        )]),
      type = 'n',
      xlim = range(donnee$anneesFittees),
      ylim = ylimLog,
      xlab = labAn,
      ylab = labRes
    )
    Bres.ylim <- par('usr')[3:4]
    abline(h = 0, col = 'grey')
    for (j in 1:nrow(Bobs.temp)) {
      annee <- Bobs.temp[j, 'annee']
      if (annee %in% donnee$anneesFittees) {
        if (isTRUE(residusStd)) {
          lines(
            rep(annee, 2),
            c(
              0,
              log(Bobs.temp[j, 'valeur'] / objReport$qRel[i]) -
                log(objReport$Bpred[donnee$anneesFitteesID[
                  donnee$anneesFittees == annee
                ]])
            ) /
              objReport$sigma_Bobs[Bobs.temp[j, 'sigma']],
            lwd = 2,
            col = i + 1 + unique(donnee$Bobs_abs$source)
          )
          points(
            annee,
            (log(Bobs.temp[j, 'valeur'] / objReport$qRel[i]) -
              log(objReport$Bpred[donnee$anneesFitteesID[
                donnee$anneesFittees == annee
              ]])) /
              objReport$sigma_Bobs[Bobs.temp[j, 'sigma']],
            pch = 21,
            bg = i + 1 + unique(donnee$Bobs_abs$source)
          )
        } else {
          ##
          ## à valider
          ##
          ## lines(rep(annee+seq(-0.4,0.4,length.out=length(unique(donnee$Bobs$source)))[i], 2),
          ##       c(0,log(Bobs.temp[j,'valeur']/objReport$qRel[i])-log(objReport$Bpred[donnee$anneesFitteesID[donnee$anneesFittees==annee]])),
          ##       lwd=2, col=i+1)
          ## points(annee+seq(-0.4,0.4,length.out=length(unique(donnee$Bobs$source)))[i],
          ## (log(Bobs.temp[j,'valeur']/objReport$qRel[i])-log(objReport$Bpred[donnee$anneesFitteesID[donnee$anneesFittees==annee]])),
          ## pch=21, bg=i+1)
        }
      }
    }
    for (i.sig in unique(Bobs.temp$sigma)) {
      temp2 <- subset(Bobs.temp, sigma == i.sig)
      if (all(temp2[, 'valeur'] > 0)) {
        if (isTRUE(residusStd)) {
          ## fit.temp <- lm(val~an, data=list(an=temp2[,'annee']+seq(-0.4,0.4,length.out=length(unique(donnee$Bobs$source)))[i],
          ##                                  val=(log(temp2[,'valeur']/objReport$qRel[i])-
          ##                                       log(objReport$Bpred[donnee$anneesFitteesID[donnee$anneesFittees%in%temp2$annee]+1]))/objReport$sigma_Bobs[i.sig]))
          ## lines(range(temp2[,'annee'])+seq(-0.4,0.4,length.out=length(unique(donnee$Bobs$source)))[i], c(predict(fit.temp)[1], tail(predict(fit.temp),1)), col=i+1, lwd=3)
          lines(
            smooth.spline(
              list(
                x = temp2[, 'annee'],
                y = (log(temp2[, 'valeur'] / objReport$qRel[i]) -
                  log(objReport$Bpred[donnee$anneesFitteesID[
                    donnee$anneesFittees %in% temp2$annee
                  ]])) /
                  objReport$sigma_Bobs[i.sig]
              ),
              df = 2.4
            ),
            col = i + 1 + unique(donnee$Bobs_abs$source),
            lwd = 3
          )
        } else {
          ##
          ## à valider
          ##
          ## fit.temp <- lm(val~an, data=list(an=temp2[,'annee']+seq(-0.4,0.4,length.out=length(unique(donnee$Bobs$source)))[i],
          ##                                  val=(log(temp2[,'valeur']/objReport$qRel[i])-
          ##                                       log(objReport$Bpred[donnee$anneesFitteesID[donnee$anneesFittees%in%temp2$annee]+1]))))
          ## lines(range(temp2[,'annee'])+seq(-0.4,0.4,length.out=length(unique(donnee$Bobs$source)))[i], c(predict(fit.temp)[1], tail(predict(fit.temp),1)), col=i+1, lwd=3)
        }
      }
    }
  }
  ##
  text(
    x = mean(par('usr')[c(1, 2)]),
    y = diff(par('usr')[c(3, 4)]) * 0.9 + par('usr')[3],
    labels = labels,
    cex = 1.5
  )
}
