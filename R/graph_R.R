graph_R <- function(
  donnee,
  param,
  objReport = NULL,
  tacProj = NA,
  fProj = NA,
  valProj = NULL,
  ylimLog = c(-3.1, 3.1),
  pl = list(),
  plsd = list(),
  langue = 'fr',
  residus = TRUE,
  q = TRUE,
  lesquels = c(1, 2, 3)
) {
  switch(
    langue,
    'fr' = {
      labAn <- 'Année'
      labN <- "Nombre ('000)"
      labBiom <- "Biomasse ('000t)"
      labRes <- 'Résidus standardisés'
      lab.a <- 'à'
      lab.impute = 'Valeur attribuée'
      labNombre <- 'En nombre'
      labPoids <- 'En poids'
    },
    'en' = {
      labAn <- 'Year'
      labN <- "Number ('000)"
      labBiom <- "Biomass ('000t)"
      labRes <- 'Standardized residuals'
      lab.a <- 'to'
      lab.impute = 'Assigned value'
      labNombre <- 'In number'
      labPoids <- 'In weight'
    },
    'bil' = {
      labAn <- 'Année/Year'
      labN <- "Nombre/Number ('000)"
      labBiom <- "Biomasse/Biomass ('000t)"
      labRes <- 'Résidus/Residuals'
      lab.a <- 'à/to'
      lab.impute = 'Valeur attribuée/Assigned value'
      labNombre <- "Nombre/Number"
      labPoids <- 'Poids/Weight'
    }
  )
  if (!any(is.na(tacProj)) | !any(is.na(fProj))) {
    nbAnProj <- max(length(tacProj), length(fProj), na.rm = TRUE)
  } else {
    nbAnProj <- 0
  }
  if (1 %in% lesquels) {
    plot(
      donnee$anneesFittees,
      objReport$Rpred / 1000,
      type = 'l',
      xlim = range(c(donnee$anneesFittees, donnee$Robs$annee)) + c(0, nbAnProj),
      ylim = c(
        0,
        max(
          objReport$Rpred,
          donnee$Robs[, 'valeur'] / objReport$qRecru[donnee$Robs[, 'source']],
          na.rm = TRUE
        )
      ) /
        1000,
      ylab = labN,
      xlab = labAn
    )
    ## abline(v=1992.5, col='grey50')
    ## abline(v=1992.5+9, col='grey50', lty=3)
    ##
    if (length(pl) > 0) {
      polygon(
        c(donnee$anneesFittees, rev(donnee$anneesFittees)),
        c(
          exp(pl$log_Rpred + 1.96 * plsd$log_Rpred),
          rev(exp(pl$log_Rpred - 1.96 * plsd$log_Rpred))
        ) /
          1000,
        border = NA,
        col = 'grey70'
      )
      ## lines(donnee$anneesFittees, exp(pl$log_Rpred+1.96*plsd$log_Rpred)/1000, lty=2, col=2)
      ## lines(donnee$anneesFittees, exp(pl$log_Rpred-1.96*plsd$log_Rpred)/1000, lty=2, col=2)
    }
    lines(donnee$anneesFittees, objReport$Rpred / 1000, col = 'grey40')
    abline(h = 0, col = 'grey')
    for (i in unique(donnee$Robs[, 'source'])) {
      temp <- subset(donnee$Robs, source == i)
      lines(
        temp[, 'annee'],
        temp[, 'valeur'] / objReport$qRecru[i] / 1000,
        type = 'o',
        col = i + 1,
        lty = 3
      )
      points(
        temp[, 'annee'],
        temp[, 'valeur'] / objReport$qRecru[i] / 1000,
        col = temp$sigma + 1
      )
      temp2 <- subset(
        donnee$Robs,
        source == i & annee >= donnee$anneesFittees[max(temp$annee)]
      )
      if (nrow(temp2) > 0) {
        lines(
          temp2[, 'annee'],
          temp2[, 'valeur'] / objReport$qRecru[i] / 1000,
          type = 'o',
          col = i + 1,
          lty = 3
        )
        points(
          temp2[, 'annee'],
          temp2[, 'valeur'] / objReport$qRecru[i] / 1000,
          col = temp$sigma + 1
        )
      }
    }
    if (isTRUE(q)) {
      legend(
        'topleft',
        inset = 0.03,
        legend = paste(
          'Rel',
          1:length(objReport$qRecru),
          'q=',
          round(objReport$qRecru, 3)
        ),
        pch = 1,
        lty = 3,
        col = unique(donnee$Robs[, 'source']) + 1
      )
    } else {
      legend(
        'topleft',
        inset = 0.03,
        legend = c(
          paste(c(69, 77, 84), lab.a, c(77, 84, 91), 'cm'),
          lab.impute
        ),
        pch = 1,
        lty = 3,
        col = c(0, unique(donnee$Robs[, 'source'])) + 2
      )
    }
    lines(
      range(donnee$anneesRfixe),
      rep(
        mean((objReport$Rpred / 1000)[which(
          donnee$anneesFittees %in% donnee$anneesRfixe
        )]),
        2
      ),
      col = 2,
      lwd = 2
    )
    axis(
      4,
      at = mean((objReport$Rpred / 1000)[which(
        donnee$anneesFittees %in% donnee$anneesRfixe
      )]),
      labels = round(mean((objReport$Rpred / 1000)[which(
        donnee$anneesFittees %in% donnee$anneesRfixe
      )]))
    )
    if (nbAnProj > 0) {
      for (i in seq_along(tacProj)) {
        points(
          max(donnee$anneesFittees) +
            seq(1, by = 1, length.out = length(valProj[[i]]$Rproj)),
          valProj[[i]]$Rproj / 1000,
          pch = i,
          col = 1
        )
        lines(
          max(donnee$anneesFittees) +
            seq(0, by = 1, length.out = length(valProj[[i]]$Rproj) + 1),
          c(tail(objReport$Rpred, 1), valProj[[i]]$Rproj) / 1000,
          lty = 2
        )
      }
    }
    text(
      x = mean(par('usr')[c(1, 2)]),
      y = diff(par('usr')[c(3, 4)]) * 0.7 + par('usr')[3],
      labels = 'B',
      cex = 1.5
    )
  }
  ##
  ## residus
  if (2 %in% lesquels) {
    if (is.null(ylimLog)) {
      ylimLog <- range(c(
        log(subset(donnee$Robs, source == 1)$valeur / objReport$qRecru[1]) -
          log(objReport$Rpred[
            donnee$anneesFittees %in% subset(donnee$Robs, source == 1)$annee
          ]),
        log(subset(donnee$Robs, source == 2)$valeur / objReport$qRecru[2]) -
          log(objReport$Rpred[
            donnee$anneesFittees %in% subset(donnee$Robs, source == 2)$annee
          ]),
        log(subset(donnee$Robs, source == 3)$valeur / objReport$qRecru[3]) -
          log(objReport$Rpred[
            donnee$anneesFittees %in% subset(donnee$Robs, source == 3)$annee
          ])
      ))
    }
    plot(
      subset(donnee$Robs, source == 1)$annee - 0.4,
      (log(subset(donnee$Robs, source == 1)$valeur / objReport$qRecru[1]) -
        (log(objReport$Rpred)[subset(donnee$Robs, source == 1)$annee])) /
        objReport$sigma_Robs[1],
      xlim = range(donnee$anneesFittees) + c(0, nbAnProj),
      ylim = ylimLog,
      type = 'n',
      xlab = labAn,
      ylab = labRes
    )
    abline(h = 0, col = 'grey')
    for (i in unique(donnee$Robs$source)) {
      temp <- subset(donnee$Robs, source == i)
      for (j in 1:nrow(temp)) {
        annee <- temp[j, 'annee']
        if (annee %in% donnee$anneesFittees) {
          sigma <- temp[j, 'sigma']
          lines(
            rep(annee + c(-0.4, 0, 0.4)[i], 2),
            c(
              0,
              (log(temp[j, 'valeur'] / objReport$qRecru[i]) -
                log(objReport$Rpred[donnee$anneesFittees == annee])) /
                objReport$sigma_Robs[sigma]
            ),
            lwd = 2,
            col = sigma + 1
          )
          points(
            annee + c(-0.4, 0, 0.4)[i],
            (log(temp[j, 'valeur'] / objReport$qRecru[i]) -
              log(objReport$Rpred[donnee$anneesFittees == annee])) /
              objReport$sigma_Robs[sigma],
            pch = 21,
            bg = sigma + 1
          )
        }
      }
    }
    ## text(x=mean(par('usr')[c(1,2)]), y=diff(par('usr')[c(3,4)])*0.9 + par('usr')[3], labels='B', cex=1.5)
  }
  ##
  ## proportion de recures R/N
  if (3 %in% lesquels) {
    plot(
      donnee$anneesFittees,
      objReport$Rpred / objReport$Npred,
      type = 'l',
      xlim = range(donnee$anneesFittees) + c(0, nbAnProj),
      ylim = c(0, 0.5),
      xlab = labAn,
      ylab = 'Proportion'
    )
    lines(
      donnee$anneesFittees,
      objReport$Rpred *
        subset(donnee$omegaK, annee %in% donnee$anneesFittees)$valeur /
        objReport$Bpred,
      col = 2
    )
    abline(h = 0, col = 'grey')
    legend(
      'topright',
      inset = 0.03,
      legend = c(labNombre, labPoids),
      col = c(1, 2),
      lty = 1
    )
    text(
      x = mean(par('usr')[c(1, 2)]),
      y = diff(par('usr')[c(3, 4)]) * 0.9 + par('usr')[3],
      labels = 'C',
      cex = 1.5
    )
    if (nbAnProj > 0) {
      for (i in seq_along(tacProj)) {
        points(
          max(donnee$anneesFittees) +
            seq(1, by = 1, length.out = length(valProj[[i]]$Rproj)),
          valProj[[i]]$Rproj / valProj[[i]]$Nproj,
          pch = i,
          col = 1
        )
        lines(
          max(donnee$anneesFittees) +
            seq(0, by = 1, length.out = length(valProj[[i]]$Rproj) + 1),
          c(tail(objReport$Rpred, 1), valProj[[i]]$Rproj) /
            c(tail(objReport$Npred, 1), valProj[[i]]$Nproj),
          lty = 2
        )
        points(
          max(donnee$anneesFittees) +
            seq(1, by = 1, length.out = length(valProj[[i]]$Rproj)),
          valProj[[i]]$Rproj *
            tail(donnee$omegaK$valeur, 1) /
            valProj[[i]]$Bproj,
          pch = i,
          col = 2
        )
        lines(
          max(donnee$anneesFittees) +
            seq(0, by = 1, length.out = length(valProj[[i]]$Rproj) + 1),
          c(tail(objReport$Rpred, 1), valProj[[i]]$Rproj) *
            tail(donnee$omegaK$valeur, 1) /
            c(objReport$Bpred, valProj[[i]]$Bproj),
          lty = 2,
          col = 2
        )
      }
    }
  }
}
