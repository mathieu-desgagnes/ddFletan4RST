graph_age_longueur <- function(donnee, param, objReport = NULL, langue = 'fr') {
  switch(
    langue,
    'fr' = {
      labAge <- 'Age'
      labTaille <- 'Taille (cm)'
      labPoids <- 'Poids (kg)'
      labPoidsA <- 'Poids (kg), âge a'
      labPoidsB <- 'Poids (kg), âge a+1'
    },
    'en' = {
      labAge <- 'Age'
      labTaille <- 'Size (cm)'
      labPoids <- 'Weight (kg)'
      labPoidsA <- 'Weight (kg), age a'
      labPoidsB <- 'Weight (kg), âge a+1'
    },
    'bil' = {
      labAge <- 'Age'
      labTaille <- 'Taille/Size (cm)'
      labPoids <- 'Poids/Weight (kg)'
      labPoidsA <- 'Poids/Weight, age a'
      labPoidsB <- 'Poids/Weight, age a+1'
    }
  )
  ##
  ## age-longueur
  plot(
    donnee$croiss$age,
    donnee$croiss$longueur,
    xlim = c(0, max(donnee$croiss$age)),
    ylim = c(0, max(donnee$croiss$longueur)),
    xlab = labAge,
    ylab = labTaille
  )
  abline(v = 0, col = 'grey70')
  abline(h = 0, col = 'grey70')
  abline(h = c(81, 85), col = 3)
  abline(v = donnee$lagSSR, col = 3)
  axis(
    4,
    at = c(81, 85),
    labels = c(81, 85),
    col.ticks = 3,
    col.axis = 3,
    las = 1,
    cex.axis = 0.6
  )
  curve(
    objReport$linf * (1 - exp(-objReport$K * (x - (objReport$t0)))),
    add = TRUE,
    col = 2
  )
  legend(
    'bottomright',
    inset = 0.03,
    legend = paste(
      c('Linf=', 'K=', 't0='),
      c(round(objReport$linf), round(objReport$K, 3), round(objReport$t0, 2))
    )
  )
  if (FALSE) {
    #graph des résidus
    plot(
      donnee$croiss$age,
      (donnee$croiss$longueur) -
        (objReport$linf *
          (1 - exp(-objReport$K * (donnee$croiss$age - (objReport$t0))))),
      main = 'Residus croissance',
      xlab = 'Age',
      ylab = 'Résidus'
    )
    abline(h = 0)
    ## plot(donnee$croiss$age, log(donnee$croiss$longueur)-log(objReport$linf * (1-exp(-objReport$K * (donnee$croiss$age-(objReport$t0))))),
    ##      main='Residus croissance', xlab='Age', ylab='Résidus'); abline(h=0)
  }
  ## text(x=mean(par('usr')[c(1,2)]), y=diff(par('usr')[c(3,4)])*0.9 + par('usr')[3], labels='A', cex=1.5)
  ##
  ## ## age-poids
  ## plot(donnee$croiss$age, donnee$croiss$poids, xlim=c(0,max(donnee$croiss$age)), xlab=labAge, ylab=labPoids)
  ## abline(h=0, col='grey70'); abline(h=donnee$lpAlpha*c(81,85)^donnee$lpBeta, col=3); abline(v=donnee$lagSSR, col=3)
  ## axis(4, at=donnee$lpAlpha*c(81,85)^donnee$lpBeta, labels=c(81,85), col.ticks=3, col.axis=3, las=1, cex.axis=0.6)
  ## curve(donnee$lpAlpha*(objReport$linf * (1-exp(-objReport$K * (x-(objReport$t0)))))^donnee$lpBeta, add=TRUE, col=2)
  ## ## legend('topleft', inset=0.03, legend=paste(c('Winf=','WK=','t0='),
  ## ##                                               c(round(donnee$lpAlpha*objReport$linf^donnee$lpBeta), round(objReport$K,3), round(objReport$t0,2))))
  ## text(x=mean(par('usr')[c(1,2)]), y=diff(par('usr')[c(3,4)])*0.9 + par('usr')[3], labels='B', cex=1.5)
  ##
  ## poids+t vs poids_t+1
  taille.age <- objReport$linf *
    (1 - exp(-objReport$K * (1:100 - (objReport$t0))))
  poids.age <- donnee$lpAlpha * taille.age^donnee$lpBeta
  poids.min <- min(donnee$Oobs$valeur)
  poids.max <- max(donnee$Oobs$valeur)
  plot(
    head(poids.age, -1),
    tail(poids.age, -1),
    xlim = c(0, poids.age[head(which(poids.age > poids.max), 1) + 2]),
    ylim = c(0, poids.age[head(which(poids.age > poids.max), 1) + 3]),
    xlab = labPoidsA,
    ylab = labPoidsB
  )
  ## plot(head(poids.age,-1), tail(poids.age,-1), xlim=c(0,200), ylim=c(0,200),
  ##      xlab='Poids moyen, âge a', ylab='Poids moyen, âge a+1')
  abline(v = donnee$lpAlpha * 85^donnee$lpBeta)
  axis(3, at = donnee$lpAlpha * 85^donnee$lpBeta, labels = '85 cm')
  curve(
    objReport$alpha + x * objReport$rho,
    from = poids.min,
    to = poids.max,
    add = TRUE,
    col = 4,
    lwd = 3
  )
  abline(a = objReport$alpha, b = objReport$rho)
  points(head(poids.age, -1), tail(poids.age, -1))
  ## abline(a=0,b=1, col='grey70', lty=2)
  legend(
    'bottomright',
    inset = 0.03,
    legend = paste(
      c('alpha=', 'rho='),
      c(round(objReport$alpha, 2), round(objReport$rho, 3))
    )
  )
  noms <- 1:20
  noms[which(!noms %in% pretty(noms))] <- NA
  text(
    head(poids.age, -1)[noms],
    tail(poids.age, -1)[noms],
    labels = noms,
    pos = 1
  )
  ## text(x=mean(par('usr')[c(1,2)]), y=diff(par('usr')[c(3,4)])*0.9 + par('usr')[3], labels='B', cex=1.5)
}
