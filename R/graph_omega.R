graph_omega <- function(
  donnee,
  param,
  objReport = NULL,
  ylimLog = c(-3.1, 3.1),
  langue = 'fr',
  residus = TRUE,
  plr = NULL,
  plrsd = NULL,
  lesquels = c(1, 2, 3)
) {
  switch(
    langue,
    'fr' = {
      labAn <- 'Année'
      labPoids <- "Poids individuel moyen (kg)"
      labRes <- 'Résidus standardisé'
      labLong = 'Taille individuelle moyenne (cm)'
    },
    'en' = {
      labAn <- 'Year'
      labPoids <- "Mean individual weight (kg)"
      labRes <- 'Strandardized residuals'
      labLong = 'Mean indivudual length (cm)'
    },
    'bil' = {
      labAn <- 'Année/Year'
      labPoids <- "Poids individuel moyen/Mean indivudual weight (kg)"
      labRes <- 'Résidus/Residuals'
      labLong = 'Taille individuelle moyenne/Mean individual length'
    }
  )
  ##
  if (1 %in% lesquels) {
    plot(
      donnee$anneesFittees,
      objReport$omegaPred,
      type = 'l',
      xlim = range(donnee$anneesFittees),
      ylim = c(
        7,
        min(max(donnee$Oobs$valeur, tail(objReport$omegaPred, 20)), 35)
      ),
      xlab = labAn,
      ylab = labPoids
    )
    if (length(plr) > 0) {
      polygon(
        c(donnee$anneesFittees, rev(donnee$anneesFittees)),
        c(
          plr$omegaPred + 1.96 * plrsd$omegaPred,
          rev(plr$omegaPred - 1.96 * plrsd$omegaPred)
        ),
        border = NA,
        col = 'grey70'
      )
    }
    lines(donnee$anneesFittees, objReport$omegaPred, col = 'grey40')
    abline(h = 0, col = 'grey70')
    box()
    for (i in sort(unique(donnee$Oobs$source))) {
      temp <- subset(donnee$Oobs, source == i)
      points(temp$annee, temp$valeur, pch = 16, col = i + 1)
    }
    axis(
      4,
      at = as.numeric(donnee$lpAlpha) *
        pretty(
          (par('usr')[3:4] / as.numeric(donnee$lpAlpha))^(1 /
            as.numeric(donnee$lpBeta))
        )^as.numeric(donnee$lpBeta),
      labels = pretty(
        (par('usr')[3:4] / as.numeric(donnee$lpAlpha))^(1 /
          as.numeric(donnee$lpBeta))
      )
    )
    mtext(text = labLong, side = 4, line = 3)
    legend(
      'topleft',
      inset = 0.03,
      legend = switch(
        langue,
        'fr' = c(
          'Commercial, à quai',
          'Commercial, en mer',
          'Relevé palangre',
          'Relevé chalut'
        ),
        'en' = c(
          'Commercial, at port',
          'Commercial, at sea',
          'Longline survey',
          'Trawl survey'
        ),
        'bil' = c(
          'Commercial, à quai/at port',
          'Commercial, en mer/at sea',
          'Relevé/Survey',
          'Relevé/Survey'
        )
      ),
      pch = 16,
      col = c(2, 3, 4, 5),
      bg = 'white'
    )
    text(
      x = mean(par('usr')[c(1, 2)]),
      y = diff(par('usr')[c(3, 4)]) * 0.7 + par('usr')[3],
      labels = 'C',
      cex = 1.5
    )
  }
  ##
  if (2 %in% lesquels) {
    ## residuels
    if (residus) {
      if (is.null(ylimLog)) {
        ylimLog <- range(c(
          log(subset(donnee$Oobs, source == 1)$valeur) -
            log(objReport$omegaPred[
              donnee$anneesFittes %in% subset(donnee$Oobs, source == 1)$annee
            ]),
          log(subset(donnee$Oobs, source == 2)$valeur) -
            log(objReport$omegaPred[
              donnee$anneesFittes %in% subset(donnee$Oobs, source == 2)$annee
            ])
        ))
      }
      plot(
        subset(donnee$Oobs, source == 1)$annee - 0.2,
        (subset(donnee$Oobs, source == 1)$valeur -
          objReport$omegaPred[subset(donnee$Oobs, source == 1)$annee]) /
          objReport$sigma_oBar[1],
        ## (log(subset(donnee$Oobs, source==1)$valeur)-log(objReport$omegaPred[subset(donnee$Oobs, source==1)$annee])),
        xlim = range(donnee$anneesFittees),
        ylim = ylimLog,
        type = 'n',
        xlab = labAn,
        ylab = labRes
      )
      abline(h = 0, col = 'grey')
      for (i in unique(donnee$Oobs$source)) {
        temp <- subset(donnee$Oobs, source == i)
        for (j in 1:nrow(temp)) {
          annee <- temp[j, 'annee']
          if (annee %in% donnee$anneesFittees) {
            sigma <- temp[j, 'sigma']
            lines(
              rep(annee + c(-0.2, 0, 0.2)[i], 2),
              (c(
                0,
                temp[j, 'valeur'] -
                  objReport$omegaPred[donnee$anneesFitteesID[
                    donnee$anneesFittees == annee
                  ]]
              )) /
                objReport$sigma_oBar[sigma],
              lwd = 2,
              col = i + 1
            )
            ## lines(rep(donnee$anneesFittees[annee]+c(-0.2,0,0.2)[i], 2), (c(0,log(temp[j,'valeur'])-log(objReport$omegaPred[annee]))),
            ##       lwd=2, col=i+1)
            points(
              annee + c(-0.2, 0, 0.2)[i],
              (temp[j, 'valeur'] -
                objReport$omegaPred[donnee$anneesFitteesID[
                  donnee$anneesFittees == annee
                ]]) /
                objReport$sigma_oBar[sigma],
              pch = 16,
              col = i + 1
            )
            ## points(donnee$anneesFittees[annee]+c(-0.2,0,0.2)[i], (log(temp[j,'valeur'])-log(objReport$omegaPred[annee])), pch=16, col=i+1)
          }
        }
      }
      ## text(x=mean(par('usr')[c(1,2)]), y=diff(par('usr')[c(3,4)])*0.9 + par('usr')[3], labels='B', cex=1.5)
    }
  }
}
