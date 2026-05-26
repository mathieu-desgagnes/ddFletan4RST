graph_kobe <- function(
  donnee,
  param,
  objReport = NULL,
  tacProj = NA,
  fProj = NA,
  Bmax = NA,
  langue = 'fr',
  pl = NULL,
  plsd = NULL
) {
  switch(
    langue,
    'fr' = {
      labAn <- 'Année'
      labBiom <- "Biomasse ('000t)"
      labRes <- 'Résidus standardisés'
    },
    'en' = {
      labAn <- 'Year'
      labBiom <- "Biomass ('000t)"
      labRes <- 'Standardized residuals'
    },
    'bil' = {
      labAn <- 'Année/Year'
      labBiom <- "Biomasse/Biomass ('000t)"
      labRes <- 'Résidus/Residuals'
    }
  )
  if (!any(is.na(tacProj)) | !any(is.na(fProj))) {
    nbAnProj <- max(length(tacProj[[1]]), length(fProj), na.rm = TRUE)
  } else {
    nbAnProj <- 0
  }
  if (is.null(objReport$Cequilibre)) {
    x.max <- max(objReport$Bpred, Bmax, na.rm = TRUE) / 1000 / 1000
  } else {
    x.max <- max(
      objReport$Bpred,
      objReport$Bequilibre[which.max(objReport$Cequilibre)],
      Bmax,
      plr$Bpred + 1.96 * plrsd$Bpred,
      na.rm = TRUE
    ) /
      1000 /
      1000
  }
  if (is.null(objReport$Cequilibre)) {
    y.max <- max(objReport$F)
  } else {
    y.max <- min(
      max(
        c(
          objReport$F,
          -log(1 - objReport$Uequilibre[which.max(objReport$Cequilibre)])
        ),
        na.rm = TRUE
      ),
      2 * -log(1 - objReport$Uequilibre[which.max(objReport$Cequilibre)])
    )
  }
  plot(
    objReport$Bpred / 1000 / 1000,
    objReport$F,
    type = 'o',
    xlim = c(0, x.max * 1.1),
    ylim = c(0, y.max * 1.1),
    xaxs = 'i',
    yaxs = 'i',
    xlab = "B85+ ('000 t)",
    ylab = "F"
  )
  abline(h = 0, col = 'grey70')
  abline(v = 0, col = 'grey70')
  abline(h = objReport$M, col = 4, lty = 4)
  if (!is.null(pl)) {
    lines(
      rep(tail(objReport$Bpred, 1) / 1000 / 1000, 2),
      -log(
        1 -
          c(
            tail(
              0.001 +
                0.9 * plogis(pl$trans_TauxExp - 1.96 * plsd$trans_TauxExp),
              1
            ),
            tail(
              0.001 +
                0.9 * plogis(pl$trans_TauxExp + 1.96 * plsd$trans_TauxExp),
              1
            )
          )
      ),
      lwd = 3,
      col = 4
    )
    lines(
      c(
        tail(plr$Bpred - 1.96 * plrsd$Bpred, 1),
        tail(plr$Bpred + 1.96 * plrsd$Bpred, 1)
      ) /
        1000 /
        1000,
      rep(-log(1 - c(tail(0.001 + 0.9 * plogis(pl$trans_TauxExp), 1))), 2),
      lwd = 3,
      col = 4
    )
  }
  lines(objReport$Bpred / 1000 / 1000, objReport$F, type = 'o')
  if (nbAnProj > 0) {
    for (i in seq_along(tacProj)) {
      points(
        valProj[[i]]$Bproj / 1000 / 1000,
        valProj[[i]]$fProj,
        pch = i,
        col = 1
      )
      lines(
        c(tail(objReport$Bpred, 1), valProj[[i]]$Bproj) / 1000 / 1000,
        c(-log(1 - tail(objReport$tauxExp, 1)), valProj[[i]]$fProj),
        lty = 2
      )
    }
  }
  lesquels <- which(donnee$anneesFittees %in% pretty(donnee$anneesFittees))
  text(
    (objReport$Bpred / 1000 / 1000)[lesquels],
    (objReport$F)[lesquels],
    labels = donnee$anneesFittees[lesquels],
    pos = 3
  )
  ##
  if (!is.null(objReport$Cequilibre)) {
    #des points de référence peuvent être calculés
    abline(
      v = objReport$Bequilibre[which.max(objReport$Cequilibre)] / 1000 / 1000,
      lty = 2
    )
    abline(
      v = objReport$Bequilibre[which.max(objReport$Cequilibre)] /
        1000 /
        1000 *
        c(0.4, 0.8),
      lty = 3,
      col = 'grey70'
    )
    abline(
      h = -log(1 - objReport$Uequilibre[which.max(objReport$Cequilibre)]),
      col = 2
    )
    abline(h = objReport$M, col = 4)
    axis(
      4,
      at = -log(1 - objReport$Uequilibre[which.max(objReport$Cequilibre)]),
      labels = 'Fmsy',
      col.axis = 2,
      las = 1
    )
    axis(4, at = objReport$M, labels = 'M', col.axis = 4, las = 1)
    axis(
      3,
      at = objReport$Bequilibre[which.max(objReport$Cequilibre)] /
        1000 /
        1000 *
        c(0.4, 0.8, 1),
      labels = c('0.4', '0.8', 'Bmsy'),
      las = 1
    )
  }
  legend(
    'topright',
    inset = 0.03,
    legend = paste0(
      c('B', 'F'),
      tail(donnee$anneesFittees, 1),
      c('/Bmsy = ', '/Fmsy = '),
      c(
        round(
          tail(objReport$Bpred, 1) /
            objReport$Bequilibre[which.max(objReport$Cequilibre)],
          2
        ),
        round(
          tail(objReport$F, 1) /
            -log(1 - objReport$Uequilibre[which.max(objReport$Cequilibre)]),
          2
        )
      )
    )
  )
}
