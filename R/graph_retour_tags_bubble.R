graph_retour_tag_bubble <- function(
  donnee,
  param,
  objReport = NULL,
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
      labAtt = 'Nombre attendu'
      labObs = 'Nombre observé'
    },
    'en' = {
      labAn <- 'Year'
      labNbRet <- 'Number of returns'
      labTxRet <- 'Return rate'
      labSurvie <- 'Post-tagging survival'
      labAnCapt = 'Year of capture'
      labAnMarq = 'Year of tagging'
      labAtt = 'Expected number'
      labObs = 'Observed number'
    },
    'bil' = {
      labAn <- 'Année/Year'
      labNbRet <- 'Nombre de retours/Number of returns'
      labTxRet <- 'Taux de retour/Return rate'
      labSurvie <- 'Survie post-marquage/Post-tagging survival'
      labAnCapt = 'Capture'
      labAnMarq = 'Marquage/Tagging'
      labAtt = 'Attendu/Expected'
      labObs = 'Observé/Observed'
    }
  )
  ##
  ## temp <- donnee$nTagsRetour; temp[which(temp==0)] <- NA
  ## plot(donnee$anneesFittees, objReport$nTagRetourPred,
  ##      xlim=range(donnee$anneesFittees)+c(0,nbAnProj), ylim=c(0,max(c(objReport$nTagRetour,temp),na.rm=TRUE)),type='l',
  ##      xlab=labAn, ylab=labNbRet)#; abline(h=0, col='grey70')
  ## points(donnee$anneesFittees, temp, pch=21, bg=3)
  ## for(i in seq_along(temp)){
  ##     if(!is.na(temp[i])){
  ##         lines(rep(donnee$anneesFittees[i],2), c(temp[i],objReport$NtagCapt[i]), lty=1, col=2)
  ##     }
  ## }
  ## legend('topleft', inset=0.03,
  ##        legend=paste(c('tx ret.=','s. p-m='),
  ##                     c(round(objReport$tauxRetour,2), round(donnee$sPostMarquage,2))))
  ##
  nTagPose.part <- subset(donnee$nTagsPoses, unTagPose > 0 | deuxTagPose > 0)
  plot(
    0,
    0,
    type = 'n',
    xlim = c(
      head(nTagPose.part$annee, 1) + 1,
      tail(donnee$anneesFittees, 1)
    ) +
      c(-0.5, 0.5),
    ylim = c(
      0,
      max(donnee$nTagsRetourObs$valeur, objReport$nTagRetourPred, na.rm = TRUE)
    ),
    xlab = labAn,
    ylab = labNbRet,
  )
  abline(h = 0, col = 'grey70')
  for (i in seq_along(nTagPose.part$annee)) {
    lines(
      donnee$anneesFittees[1:ncol(objReport$nTagRetourPred)] +
        rep(
          seq(
            -0.1,
            0.1,
            length.out = length(unique(donnee$nTagsRetourObs$anneeRecap))
          ),
          100
        )[i],
      objReport$nTagRetourPred[
        donnee$anneesFitteesID[donnee$anneesFittees == nTagPose.part$annee[i]],
      ],
      type = 'o',
      col = i + 1,
      lwd = 2,
      pch = 16
    )
    obs.temp <- subset(
      donnee$nTagsRetourObs,
      anneePose == nTagPose.part$annee[i]
    )
    points(
      obs.temp$anneeRecap +
        rep(
          seq(
            -0.1,
            0.1,
            length.out = length(unique(donnee$nTagsRetourObs$anneeRecap))
          ),
          100
        )[i],
      obs.temp[, 'valeur'],
      pch = 1,
      col = i + 1
    ) #pch=22, bg=i+1)
    if (nrow(obs.temp) >= 1) {
      for (j in 1:nrow(obs.temp)) {
        lines(
          rep(
            obs.temp$anneeRecap[j] +
              rep(
                seq(
                  -0.1,
                  0.1,
                  length.out = length(unique(donnee$nTagsRetourObs$anneeRecap))
                ),
                100
              )[i],
            2
          ),
          c(
            objReport$nTagRetourPred[
              donnee$anneesFittees == nTagPose.part$annee[i],
              donnee$anneesFittees == obs.temp[j, 'anneeRecap']
            ],
            obs.temp[j, 'valeur']
          ),
          col = i + 1
        )
      }
    }
  }
  ## axis(3, at=1:length(objReport$F), labels=round(objReport$F,3), tick=FALSE, line=-1, cex.axis=0.7)
  ## axis(1, at=donnee$anneesFittees[1:ncol(objReport$nTagRetourPred)], labels=round(apply(objReport$nTagRetourPred,2,sum,na.rm=TRUE),1), tick=FALSE,
  ##      line=-2, cex.axis=0.7)
  ## temp <- aggregate(donnee$nTagsRetourObs$valeur, by=donnee$nTagsRetourObs['anneeRecap'], FUN=sum)
  ## axis(3, at=temp$anneeRecap, labels=temp$x, tick=FALSE, line=-2, cex.axis=0.7)
  temp <- round(apply(objReport$nTagRetourPred, 2, sum, na.rm = TRUE))
  axis(
    3,
    at = donnee$anneesFittees,
    labels = round(temp),
    tick = FALSE,
    line = -2,
    cex.axis = 0.7
  )
  legend(
    'topleft',
    inset = 0.03,
    legend = paste(
      c(labTxRet, labSurvie),
      c(round(objReport$tauxRetour, 2), round(donnee$sPostMarquage, 2))
    )
  )
  legend(
    'bottomright',
    inset = 0.03,
    legend = c(labAtt, labObs),
    pch = c(1, 16)
  )

  ## text(x=mean(par('usr')[c(1,2)]), y=diff(par('usr')[c(3,4)])*0.9 + par('usr')[3], labels='A', cex=1.5)
  ##
  ## résiduels
  if (residus) {
    plot(
      0,
      0,
      type = 'n',
      xlim = range(donnee$nTagsRetourObs$anneeRecap),
      ylim = range(donnee$nTagsRetourObs$anneePose) + c(-0.5, 0.5),
      xlab = labAnCapt,
      ylab = labAnMarq
    )
    for (i in 1:nrow(donnee$nTagsRetourObs)) {
      temp <- donnee$nTagsRetourObs[i, ]
      res <- temp$valeur -
        objReport$nTagRetourPred[
          donnee$anneesFittees == temp$anneePose,
          donnee$anneesFittees == temp$anneeRecap
        ]
      resStd <- res /
        sqrt(objReport$nTagRetourPred[
          donnee$anneesFittees == temp$anneePose,
          donnee$anneesFittees == temp$anneeRecap
        ])
      points(
        temp$anneeRecap,
        temp$anneePose,
        cex = 2 * sqrt(abs(resStd)),
        bg = ifelse(res > 0, 2, 4),
        pch = 21
      )
    }
    ## text(x=mean(par('usr')[c(1,2)]), y=diff(par('usr')[c(3,4)])*0.9 + par('usr')[3], labels='B', cex=1.5)
  }
}
