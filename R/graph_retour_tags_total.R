graph_retour_tag_total <- function(
  donnee,
  param,
  objReport = NULL,
  tacProj = NA,
  fProj = NA,
  valProj = NULL,
  langue = 'fr',
  residus = TRUE
) {
  switch(
    langue,
    'fr' = {
      labAn <- 'Année'
      labNbRet <- 'Nombre de retours'
      labTxRet <- 'Taux de retour'
      labSurvie <- 'Survie post-marquage'
      labAnCapt = 'Année de capture'
      labAnMarq = 'Années de marquage'
    },
    'en' = {
      labAn <- 'Year'
      labNbRet <- 'Number of returns'
      labTxRet <- 'Return rate'
      labSurvie <- 'Post-tagging survival'
      labAnCapt = 'Year of capture'
      labAnMarq = 'Year of tagging'
    },
    'bil' = {
      labAn <- 'Année/Year'
      labNbRet <- 'Nombre de retours/Number of returns'
      labTxRet <- 'Taux de retour/Return rate'
      labSurvie <- 'Survie post-marquage/Post-tagging survival'
      labAnCapt = 'Capture'
      labAnMarq = 'Marquage/Tagging'
    }
  )
  ##
  nObs <- aggregate(
    donnee$nTagsRetourObs$valeur,
    by = donnee$nTagsRetourObs['anneeRecap'],
    FUN = sum
  )
  nPred <- cbind(
    anneeRecap = donnee$anneesFittees,
    y = round(apply(objReport$nTagRetourPred, 2, sum, na.rm = TRUE))
  )
  nTag <- merge(nObs, nPred, all = TRUE)
  nTag <- merge(
    nTag,
    cbind(
      anneeRecap = donnee$anneesFittees,
      n1 = round(apply(objReport$nTag1, 2, sum, na.rm = TRUE))
    )
  )
  nTag <- merge(
    nTag,
    cbind(
      anneeRecap = donnee$anneesFittees,
      n2 = round(apply(objReport$nTag2, 2, sum, na.rm = TRUE))
    )
  )
  nTag$ntot <- nTag$n1 + nTag$n2
  plot(
    0,
    0,
    type = 'n',
    xlim = range(nTag$annee),
    ylim = c(0, max(nTag$x, nTag$y, na.rm = TRUE)),
    xlab = labAn,
    ylab = labNbRet,
    axes = FALSE
  )
  box()
  axis(1)
  axis(2)
  abline(h = 0, col = 'grey70')
  for (i in seq_len(nrow(nTag))) {
    lines(
      rep(nTag[i, 'anneeRecap'], 2),
      nTag[i, c('x', 'y')],
      type = 'o',
      col = 1,
      pch = 1
    )
    points(nTag[i, c('anneeRecap', 'x')], pch = 21, bg = 2)
  }
  ratio <- max(nTag$y, na.rm = TRUE) / max(nTag$ntot, na.rm = TRUE)
  lines(nTag$anneeRecap + 1, nTag$ntot * ratio, type = 'o', col = 3)
  axis(
    4,
    at = pretty(par('usr')[3:4] / ratio) * ratio,
    labels = pretty(par('usr')[3:4] / ratio)
  )
  legend(
    'topleft',
    inset = 0.03,
    legend = c('Observé', 'Prédit', 'n total'),
    pch = c(21, 1, 1),
    col = c(1, 1, 3),
    pt.bg = c(2, NA, NA),
    lty = c(NA, NA, 1)
  )
}
