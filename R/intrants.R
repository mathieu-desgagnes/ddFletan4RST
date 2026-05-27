#' Construction de l'objet des données d'intrant
#'
#' À explorer:
#'   - inclure un paramètre indiquand le nom du dossier de sauvegarde des données du modèle (et vérifier si existe)
#'   - vérifier s'il y a une manière de les avoir directement acutaliés
#'   - (exclure les comparatifs et séparer en différentes séries-navires) dans les relevé MPO
#'
#'
#' @param annee dernière année à considérer dans les données
#'
#' @returns
#' @export
#'
#' @examples
calculer_intrants <- function(annee) {
  dd_data <- list()

  dd_data$anneesFittees <- 1983:2025
  ##
  dd_data$anneesFitteesID <- seq_along(dd_data$anneesFittees)
  names(dd_data$anneesFitteesID) <- dd_data$anneesFittees
  ##

  ## Débaquements observés, kilogramme, année civile
  load(file.path(
    'S:',
    'Flétan',
    'evaluation stock',
    'input',
    annee,
    'data.RData'
  ))
  temp <- na.omit(data$tac[, c('annee', 'consolideCivil')])
  dimnames(temp)[[2]] <- c('annee', 'debarquement')
  Cobs <- na.omit(as.data.frame(list(
    annee = temp$annee,
    valeur = temp$debarquement * 1000
  )))
  dd_data$Cobs <- Cobs
  ##
  write.csv2(
    Cobs,
    file = file.path(
      'S:',
      'Flétan',
      'Delay-diff flétan',
      'input',
      annee,
      'debarquements.csv'
    )
  )

  ## indices d'abondance absolu: mpo au chalut, combinée nord-sud
  ## load(file.path('S:','Flétan','evaluation stock', 'input', annee, 'data.RData'))
  temp.85plus <- data$mpo$indiceCombine[['classe85plus']]$pue[, c(
    'annee',
    'bmcGsl'
  )]
  temp.81plus <- data$mpo$indiceCombine[['classe81plus']]$pue[, c(
    'annee',
    'bmcGsl'
  )]
  Bobs <- temp.81plus
  Bobs[
    match(2010:max(temp.85plus$annee), temp.85plus$annee),
    'bmcGsl'
  ] <- temp.85plus[
    match(2010:max(temp.85plus$annee), temp.85plus$annee),
    'bmcGsl'
  ]
  dimnames(Bobs)[[2]] <- c('annee', 'valeur')
  write.csv2(
    Bobs,
    file = file.path(
      'S:',
      'Flétan',
      'Delay-diff flétan',
      'input',
      annee,
      'indiceCombine.csv'
    )
  )
  ##
  Bobs$source <- 1
  Bobs$sigma <- 1
  Bobs$nom <- 'indiceCombine'
  dd_data$Bobs_abs <- Bobs
  ##

  ## indices d'abondance relatif
  ## - (sentinelle sGSL) * Les fichiers sont disponible dans S:/Flétan/evaluation stock/input/2022/Moncton, mais le fomat diffère des autes et ne peut pas être traité de la même manière
  load(
    file.path(
      'S:',
      'Flétan',
      'evaluation stock',
      'input',
      annee,
      'releve',
      'indiceAbondanceMpoNgsl.RData'
    ),
    verbose = 1
  )
  dd_data$Bobs <- data.frame(
    annee = numeric(),
    valeur = numeric(),
    source = numeric(),
    sigma = numeric(),
    nom = character()
  )
  ## Gadus
  temp <- as.data.frame(ybarTot$GA[['classe81plus']]$pue[, c('annee', 'moy')])
  dimnames(temp)[[2]] <- c('annee', 'valeur')
  write.csv2(
    temp,
    file = file.path(
      'S:',
      'Flétan',
      'Delay-diff flétan',
      'input',
      annee,
      'ia_gadus.csv'
    )
  )
  temp$source <- max(dd_data$Bobs$source, 0) + 1
  temp$sigma <- max(dd_data$Bobs$sigma, 0) + 1
  temp$nom <- 'gadus'
  dd_data$Bobs <- rbind(dd_data$Bobs, temp)
  ## Lady Hammond
  temp <- as.data.frame(ybarTot$LH[['classe81plus']]$pue[, c('annee', 'moy')])
  dimnames(temp)[[2]] <- c('annee', 'valeur')
  write.csv2(
    temp,
    file = file.path(
      'S:',
      'Flétan',
      'Delay-diff flétan',
      'input',
      annee,
      'ia_ladyhammond.csv'
    )
  )
  temp$source <- max(dd_data$Bobs$source, 0) + 1
  temp$sigma <- max(dd_data$Bobs$sigma, 0) + 1
  temp$nom <- 'ladyhammond'
  dd_data$Bobs <- rbind(dd_data$Bobs, temp)
  ## sentinelle nGSL
  nom_fichier <- file.path(
    'S:',
    'Flétan',
    'evaluation stock',
    'input',
    annee,
    'releve',
    'indiceAbondanceSenNgsl.RData'
  )
  file.info(nom_fichier)$mtime
  load(nom_fichier, verbose = 1)
  temp.81plus <- as.data.frame(ybarTot$sen[['classe81plus']]$pue[, c(
    'annee',
    'moy'
  )])
  dimnames(temp.81plus)[[2]] <- c('annee', 'valeur')
  temp.85plus <- as.data.frame(ybarTot$sen[['classe85plus']]$pue[, c(
    'annee',
    'moy'
  )])
  dimnames(temp.85plus)[[2]] <- c('annee', 'valeur')
  temp <- temp.81plus
  temp[match(2010:max(temp.85plus$annee), temp$annee), 'valeur'] <- temp.85plus[
    match(2010:max(temp.85plus$annee), temp.85plus$annee),
    'valeur'
  ]
  write.csv2(
    temp,
    file = file.path(
      'S:',
      'Flétan',
      'Delay-diff flétan',
      'input',
      annee,
      'ia_sentinelleNGSL.csv'
    )
  )
  temp$source <- max(dd_data$Bobs$source, 0) + 1
  temp$sigma <- max(dd_data$Bobs$sigma, 0) + 1
  temp$nom <- 'senNGSL'
  dd_data$Bobs <- rbind(dd_data$Bobs, temp)
  ## relevé à la palangre
  nom_fichier <- file.path(
    'S:',
    'Flétan',
    'Relevé 4RST',
    'Analyses',
    'output',
    paste0(annee, 'pue.csv')
  )
  file.info(nom_fichier)$mtime
  temp <- read.csv2(nom_fichier, stringsAsFactors = FALSE)[, c('X', 'moy')]
  dimnames(temp)[[2]] <- c('annee', 'valeur')
  write.csv2(
    temp,
    file = file.path(
      'S:',
      'Flétan',
      'Delay-diff flétan',
      'input',
      annee,
      'relPalangre.csv'
    )
  )
  temp$source <- max(dd_data$Bobs$source) + 1
  temp$sigma <- max(dd_data$Bobs$sigma) + 1
  temp$nom <- 'relPalangre'
  dd_data$Bobs <- rbind(dd_data$Bobs, temp)
  ## pue commerciale
  nom_fichier <- file.path(
    'S:',
    'Flétan',
    'evaluation stock',
    'output',
    annee,
    'csv',
    'pueCommCivile.csv'
  )
  file.info(nom_fichier)$mtime
  temp <- read.csv2(nom_fichier, stringsAsFactors = FALSE)[, c('annee', 'moy')]
  dimnames(temp)[[2]] <- c('annee', 'valeur')
  write.csv2(
    temp,
    file = file.path(
      'S:',
      'Flétan',
      'Delay-diff flétan',
      'input',
      annee,
      'pueCommCivile.csv'
    )
  )
  temp$source <- max(dd_data$Bobs$source) + 1
  temp$sigma <- max(dd_data$Bobs$sigma) + 1
  temp$nom <- 'pueCommCivil'
  dd_data$Bobs <- rbind(dd_data$Bobs, temp)
  ##
  ## (sentinelle sGSL)
  ##

  ## indice de recrutement, tranche de taille de l'indice combiné, MPO sGSL+nGSL (69-77cm, 77-84cm, 84-91cm), décallé selon nombre d'années avant recrutement
  ## par défaut, comme les données proviennent de la même source, le même sigma est utilisé
  dd_data$Robs <- data.frame(
    annee = numeric(),
    valeur = numeric(),
    source = numeric(),
    sigma = numeric(),
    nom = character()
  )
  ##
  ## load(file.path('S:','Flétan','evaluation stock', 'input', annee, 'data.RData'))
  temp <- data$mpo$indiceCombine[['classe69a77']]$nue[, c('annee', 'bmcGsl')]
  dimnames(temp)[[2]] <- c('annee', 'valeur')
  temp$source <- 1
  temp$sigma <- 1
  temp$annee <- temp$annee + 2
  temp$nom <- 'recru69a77'
  dd_data$Robs <- rbind(dd_data$Robs, temp)
  ##
  temp <- data$mpo$indiceCombine[['classe77a84']]$nue[, c('annee', 'bmcGsl')]
  dimnames(temp)[[2]] <- c('annee', 'valeur')
  temp$source <- 2
  temp$sigma <- 2
  temp$annee <- temp$annee + 1
  temp$nom <- "recru77a84"
  dd_data$Robs <- rbind(dd_data$Robs, temp)
  ##
  temp <- data$mpo$indiceCombine[['classe84a91']]$nue[, c('annee', 'bmcGsl')]
  dimnames(temp)[[2]] <- c('annee', 'valeur')
  temp$source <- 3
  temp$sigma <- 3
  temp$annee <- temp$annee
  temp$nom <- "recru84a91"
  dd_data$Robs <- rbind(dd_data$Robs, temp)
  ##
  recru <- dd_data$Robs[, c('annee', 'valeur', 'nom')]
  write.csv2(
    recru,
    file = file.path(
      'S:',
      'Flétan',
      'Delay-diff flétan',
      'input',
      annee,
      'recrutement.csv'
    )
  )

  ## taille moyenne observée
  ## ATTENTION: vérifier que les données de palangre sont 85cm et plus!!!!!
  ##
  ## observateurs, douteux: omega avant 1998
  nom_fichier <- file.path(
    'S:',
    'Flétan',
    'evaluation stock',
    'input',
    annee,
    'observateurs',
    'lectureObservateur.RData'
  )
  file.info(nom_fichier)$mtime
  load(file = nom_fichier, verbose = TRUE)
  obs <- obj$poidsMoy
  ##
  ## echantillonneurs
  nom_fichier <- file.path(
    'S:',
    'Flétan',
    'evaluation stock',
    'input',
    annee,
    'echantillonneurs',
    'lectureEchantillonneur.RData'
  )
  file.info(nom_fichier)$mtime
  load(file = nom_fichier, verbose = TRUE)
  ech <- ech$poidsMoy
  ##
  ## relevé palangre, valider l'approche et voir si différent en utilisant la moyenne des poissons individuels
  relPal.pue <- read.csv2(
    file.path(
      'S:',
      'Flétan',
      'Relevé 4RST',
      'Analyses',
      'output',
      annee,
      'csv',
      'pue.csv'
    ),
    stringsAsFactors = FALSE
  )
  relPal.nue <- read.csv2(
    file.path(
      'S:',
      'Flétan',
      'Relevé 4RST',
      'Analyses',
      'output',
      annee,
      'csv',
      'nue.csv'
    ),
    stringsAsFactors = FALSE
  )
  relPal.pue$poidsMoy <- relPal.pue$moy / relPal.nue$moy
  relPal.poids <- relPal.pue[, c('X', 'poidsMoy')]
  ##
  ## relevé MPO
  ## load(file.path('S:','Flétan','evaluation stock', 'input', annee, 'data.RData'))

  pue.85plus <- data$mpo$indiceCombine[['classe85plus']]$pue[, c(
    'annee',
    'bmcGsl'
  )]
  nue.85plus <- data$mpo$indiceCombine[['classe85plus']]$nue[, c(
    'annee',
    'bmcGsl'
  )]
  pue.81plus <- data$mpo$indiceCombine[['classe81plus']]$pue[, c(
    'annee',
    'bmcGsl'
  )]
  nue.81plus <- data$mpo$indiceCombine[['classe81plus']]$nue[, c(
    'annee',
    'bmcGsl'
  )]
  pue.85plus$poidsMoy <- pue.85plus$bmcGsl / nue.85plus$bmcGsl
  pue.81plus$poidsMoy <- pue.81plus$bmcGsl / nue.81plus$bmcGsl
  relMpo <- pue.81plus[, c('annee', 'poidsMoy')]
  relMpo[
    match(2010:max(pue.85plus$annee), relMpo$annee),
    'poidsMoy'
  ] <- pue.85plus[match(2010:max(pue.85plus$annee), relMpo$annee), 'poidsMoy']
  ##
  ## joindre les différentes sources
  ## echantillonneur
  omega <- as.data.frame(list(
    annee = as.numeric(dimnames(ech)[[1]]),
    valeur = ech[, c('tout')]
  ))
  omega$source <- 1
  omega$sigma <- 1
  dimnames(omega)[[2]] <- c('annee', 'valeur', 'source', 'sigma')
  ## obs
  temp <- as.data.frame(list(
    annee = as.numeric(dimnames(obs)[[1]]),
    valeur = obs[, c('tout')]
  ))
  temp[as.character(1998:2009), 'valeur'] <- obs[
    as.character(1998:2009),
    c('81plus')
  ]
  temp[as.character(2010:max(temp[, 'annee'])), 'valeur'] <- obs[
    as.character(2010:max(temp[, 'annee'])),
    c('85plus')
  ]
  temp$source <- max(omega[, 'source']) + 1
  temp$sigma <- max(omega[, 'sigma']) + 1
  dimnames(temp)[[2]] <- c('annee', 'valeur', 'source', 'sigma')
  omega <- rbind(omega, temp)
  ## relevé palangre
  temp <- relPal.poids
  temp$source <- max(omega[, 'source']) + 1
  temp$sigma <- max(omega[, 'sigma']) + 1
  dimnames(temp)[[2]] <- c('annee', 'valeur', 'source', 'sigma')
  omega <- rbind(omega, temp)
  ## relevé chalut MPO
  temp <- relMpo
  temp$source <- max(omega[, 'source']) + 1
  temp$sigma <- max(omega[, 'sigma']) + 1
  dimnames(temp)[[2]] <- c('annee', 'valeur', 'source', 'sigma')
  omega <- rbind(omega, temp)
  ##
  write.csv2(
    omega,
    file = file.path(
      'S:',
      'Flétan',
      'Delay-diff flétan',
      'input',
      annee,
      'tailleMoyObservee.csv'
    )
  )
  ##
  dd_data$Oobs <- omega
  if (FALSE) {
    plot(
      omega[omega$source == 1, c('annee', 'valeur')],
      type = 'o',
      col = 2,
      ylim = c(10, 28)
    )
    for (i in 2:4) {
      lines(
        omega[omega$source == i, c('annee', 'valeur')],
        type = 'o',
        col = i + 1
      )
    }
  }

  ## perte de biomasse associé au changement de taille en 2010
  ## load(file.path('S:','Flétan','evaluation stock', 'input', annee, 'data.RData'))
  pue.85plus <- data$mpo$indiceCombine[['classe85plus']]$pue[, c(
    'annee',
    'bmcGsl'
  )]
  pue.81plus <- data$mpo$indiceCombine[['classe81plus']]$pue[, c(
    'annee',
    'bmcGsl'
  )]
  ## voir le graphique dans fonction graph.intrants() du fichier graph.r
  nbAnDansMoyenne <- 8 # des valeurs de 6, 10 et 12 peuvent aussi être envisagées
  dd_data$drop2010 <- mean((pue.85plus$bmcGsl / pue.81plus$bmcGsl)[
    pue.85plus$annee %in%
      (2010 + c(-(nbAnDansMoyenne / 2):(nbAnDansMoyenne / 2 - 1)))
  ])

  ## paramètre de la relation longueur-poids
  nom_fichier <- file.path(
    'S:',
    'Flétan',
    'evaluation stock',
    'input',
    annee,
    'releve',
    'masseLongueur.RData'
  )
  load(file = nom_fichier, verbose = 1)
  file.info(nom_fichier)$mtime
  dd_data$lpAlpha <- as.numeric(relationML$fit$plr$A / 1000)
  dd_data$lpBeta <- as.numeric(relationML$fit$plr$B)
  ## poids moyen au recrutement(age-1): utilisé pour la relation ssr, donc utiliser 85cm comme définition de stock
  dd_data$omegaKmoins1 <- mean(relationML$l2m(77:84)) / 1000 #poids moyen estimé des taille 85a95
  ## poids moyen des poissons "perdus" lors du changement de taille
  dd_data$poidsMoy81a85 <- mean(relationML$l2m(81:85)) / 1000

  ## données de longueur à l'age
  nom_fichier <- file.path(
    'S:',
    'Flétan',
    'Otolithes',
    'BD',
    'BDtemporaire.RData'
  )
  load(file = nom_fichier, verbose = 1)
  file.info(nom_fichier)$mtime
  dd_data$croiss <- res$longAge[, c('age', 'longueur', 'sexe')] #dans longAge, seuls les valeurs utilisables sont conservées (enlevé observateur en bas d'un certain age, extrêmes)

  ## nombre de tags posés, sur individus de plus de 77cm (capturables l'année suivante)
  nom_fichier <- file.path(
    'S:',
    'Flétan',
    'Relevé 4RST',
    'Analyses',
    'output',
    annee,
    'csv',
    'nbTagPose_pourF.csv'
  )
  tagPose <- read.csv2(file = nom_fichier, stringsAsFactors = FALSE)
  print(file.info(nom_fichier)$mtime)
  ##
  unTagPose <- cbind(tagPose[, c('X', 'unTagPose')], 1)
  dimnames(unTagPose)[[2]] <- c('annee', 'valeur', 'nbTag')
  deuxTagPose <- cbind(tagPose[, c('X', 'deuxTagPose')], 2)
  dimnames(deuxTagPose)[[2]] <- c('annee', 'valeur', 'nbTag')
  dd_data$nTagsPoses <- as.data.frame(list(
    annee = dd_data$anneesFittees,
    'unTagPose' = rep(0, length(dd_data$anneesFittees)),
    'deuxTagPose' = rep(0, length(dd_data$anneesFittees))
  ))
  dd_data$nTagsPoses[
    match(unTagPose$annee, dd_data$nTagsPoses$annee),
    'unTagPose'
  ] <- unTagPose$valeur
  dd_data$nTagsPoses[
    match(deuxTagPose$annee, dd_data$nTagsPoses$annee),
    'deuxTagPose'
  ] <- deuxTagPose$valeur

  ## nombre de tag retournés au MPO, par année de pose et année de recaptrue
  nom_fichier <- file.path(
    'S:',
    'Flétan',
    'Relevé 4RST',
    'Analyses',
    'output',
    annee,
    'csv',
    'nbRetourTag_pourF.csv'
  )
  recap <- read.csv2(file = nom_fichier, stringsAsFactors = FALSE)
  print(file.info(nom_fichier)$mtime)
  ##
  dd_data$nTagsRetourObs <- data.frame(
    anneePose = numeric(),
    anneeRecap = numeric(),
    valeur = numeric(),
    source = numeric()
  )
  anneePose <- recap$X
  anneeRetour <- as.numeric(gsub("[^0-9]", "", dimnames(recap)[[2]][-1]))
  for (i in anneePose) {
    for (j in anneeRetour) {
      if (i < j) {
        #ne considère pas les retours réalisés la même année que la pose
        temp <- list(
          anneePose = i,
          anneeRecap = j,
          valeur = recap[recap$X == i, paste0('retour_', j)],
          source = 1
        )
        dd_data$nTagsRetourObs <- rbind(dd_data$nTagsRetourObs, temp)
      }
    }
  }

  ## nombre de tags perdus selon le temps passé en mer
  fichierPerteTag <- file.path(
    'S:',
    'Flétan',
    'Relevé 4RST',
    'Analyses',
    'output',
    annee,
    'csv',
    'perteTag.csv'
  )
  perteTag <- read.csv2(file = fichierPerteTag, stringsAsFactors = FALSE)
  print(file.info(fichierPerteTag)$mtime)
  ## calcul du taux de perte (Lebris etal 2009)
  perteTag <- subset(perteTag, nbAnEnMer > 0)
  temp <- table(perteTag$nbAnEnMer, perteTag$nbTagPerdu)
  dd_data$tauxPerte <- as.data.frame(list(
    tEnMer = as.numeric(dimnames(temp)[[1]]),
    doubleTag = temp[, 1],
    simpleTag = temp[, 2]
  ))
  ##
  ## (donnees d'analyse de force de cohorte pour déterminer mortalité naturelle)
  ##
  dd_data$a2010 <- 2010
  ##
  dd_data$anneesRfixe <- 2009:2023
  ##
  dd_data$RvalMin <- 10000
  ##
  ## poids moyen au recrutement
  omegaK_apres2010 <- mean(relationML$l2m(85:92)) / 1000 #poids moyen estimé des taille 85a92
  omegaK_avant2010 <- mean(relationML$l2m(81:88)) / 1000 #poids moyen estimé des taille 81a88
  dd_data$omegaK <- as.data.frame(list(
    annee = dd_data$anneesFittees,
    valeur = rep(omegaK_avant2010, length(dd_data$anneesFittees))
  ))
  dd_data$omegaK[
    dd_data$omegaK$annee >= dd_data$a2010,
    'valeur'
  ] <- omegaK_apres2010
  ##
  dd_data$sPostMarquage <- 0.98
  ##
  dd_data$tauxRetour <- 0.6

  save(
    dd_data,
    file = file.path(
      'data',
      'dd_data.RData'
    )
  )
}
