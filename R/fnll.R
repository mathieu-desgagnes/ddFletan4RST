#' Fonction RTMB à optimiser.
#'
#' Les objets dd_data et dd_param doivent exister au moment où ce modèle est optimisé.
#'
#' @returns La valeur de vraissemblance calculée
#' @export
#'
#' @examples #À venir
fnll <- function(dd_param, fit = TRUE) {
  RTMB::getAll(dd_param, dd_data)
  ##
  ## Écart-type des fonctions vraissemblances
  sigma_Bobs_abs <- exp(log_sigma_Bobs_abs)
  sigma_Bobs <- exp(log_sigma_Bobs)
  ## sigma_Bproc <- exp(log_sigma_Bproc)
  sigma_oBar <- exp(log_sigma_oBar)
  sigma_C <- exp(log_sigma_C)
  sigma_Robs <- exp(log_sigma_Robs)
  sigma_Robs <- c(sigma_Robs, log_propSigma_Robs * mean(sigma_Robs))
  sigma_longAge <- exp(log_sigma_longAge)
  ## un random walk sur le recrutement
  sigma_Rrw <- exp(log_sigma_Rrw)
  sigma_tauxPerte <- exp(log_sigma_tauxPerte) #taux de perte d'étiquette
  ##
  ## Bobs <- OBS(Bobs)
  ##
  ## Paramètres variables dans le temps
  ## Bpred <- exp(log_Bpred) #Bpred inclus B0, donc donc commence à l'an 0
  Rpred <- exp(log_Rpred) #Vecteur des recrutements prédits
  tauxExp <- 0.001 + 0.9 * plogis(trans_TauxExp) #Vecteurs des taux d'exploitation annuels prédits, entre 0.001 et 0.9
  M <- plogis(trans_M) #Mortalité naturelle instantanée
  F <- -log(1 - tauxExp) #Vecteur de mortalité par la pêche instantanée
  Z <- F + M #Vecteur de mortalité totale instantanée
  s <- exp(-M) * (1 - tauxExp) #Vecteur des taux de survie annuels, équivalent à exp(-Z)
  ##
  ## Paramètres invariables dans le temps
  B0 <- exp(log_B0) #Biomasse à année 0
  N0 <- exp(log_N0) #Nombre d'individus à année 0
  ## N0 <- B0/quantile(Oobs$valeur, 0.25)
  qRel_abs <- 2 * plogis(trans_Qrel_abs) #Capturabilité du l'indicateur pricipal (lui-même exprimé en nombre absolu), entre 0 et 2
  qRel <- 2 * plogis(trans_Qrel) #vecteur des capturabilités des autres indicateurs, entre 0 et 2
  qRecru <- 2 * plogis(trans_RapportQrecru) * qRel_abs[1] #Vecteur des capturabilité des recrues (nombre absolu) relatif à la capturabilité des adultes
  ##
  linf <- exp(log_Linf) #von B
  K <- exp(log_K) #von B
  t0 <- -5 + 10 * plogis(trans_T0) #von B, entre -5 et 5
  assymptoteTauxPerte <- plogis(trans_assymptoteTauxPerte) #Assymptote du maximum de la courbe du taux de perte d'étiquette
  accroissementTauxPerte <- exp(log_accroissementTauxPerte) #Pente initiale de la courbe du taux de perte d'étiquette
  ## Fmsy <- exp(log_Fmsy)
  ## Umsy <- 1-exp(-Fmsy)
  ##
  ## calcul des paramètre du graphique Ford-Walford (rho et alpha), selon l'étendue des longueurs moyennes observés
  poidsMoyMin <- min(Oobs$valeur) #Valeur miminale de poids pour l'ajustement de la droite
  ## ageMin <- log(1-((poidsMoyMin/lpAlpha)^(1/lpBeta))/linf) / -K + t0
  ## poidsMoyMin.plus1an <- lpAlpha * (linf * (1-exp(-K * ((ageMin+1)-t0))))^lpBeta
  poidsMoyMin.plus1an <- lpAlpha *
    (linf *
      (1 - exp(-K)) +
      exp(-K) * (poidsMoyMin / lpAlpha)^(1 / lpBeta))^lpBeta #Valeur à age+1 de la valeur minimale poids pour l'ajustement de la droite
  ##
  poidsMoyMax <- max(Oobs$valeur) #Valeur maximale de poids pour l'ajustement de la droite
  ## ageMax <- log(1-((poidsMoyMax/lpAlpha)^(1/lpBeta))/linf) / -K + t0
  ## poidsMoyMax.plus1an <- lpAlpha * (linf * (1-exp(-K * ((ageMax+1)-t0))))^lpBeta
  poidsMoyMax.plus1an <- lpAlpha *
    (linf *
      (1 - exp(-K)) +
      exp(-K) * (poidsMoyMax / lpAlpha)^(1 / lpBeta))^lpBeta #Valeur à age+1 de la valeur maximale poids pour l'ajustement de la droite
  ##
  rho <- (poidsMoyMax.plus1an - poidsMoyMin.plus1an) /
    (poidsMoyMax - poidsMoyMin) #Valeur du paramètre rho de Ford-Walford
  alpha <- poidsMoyMin.plus1an - poidsMoyMin * rho #Valeur du paramètre alpha de Ford-Walford
  ##
  ## Initialisation à l'an 1
  Bpred <- B0
  Npred <- N0
  Cpred <- tauxExp[1] * Bpred[1] * exp(-M) #pêche à lieu après mortalité naturelle
  ##
  ## Progression annuelle
  for (i in anneesFitteesID[-1]) {
    Bpred[i] <- s[i] *
      alpha *
      Npred[i - 1] +
      s[i] * rho * Bpred[i - 1] +
      omegaK[omegaK[, 'annee'] == anneesFittees[i], 'valeur'] * Rpred[i - 1]
    Npred[i] <- s[i] * Npred[i - 1] + Rpred[i - 1]
    Cpred[i] <- tauxExp[i] * Bpred[i] * exp(-M) #pêche après mortalité naturelle
    if (i == anneesFitteesID[anneesFittees == 2010]) {
      # ajuster la biomasse pour le changement de taille légale en 2010
      Bpred[i] <- Bpred[i] * drop2010
      ## soustraction du nombre de poisson en moins, selon la biomasse soustraite et le poids moyen à cette taille
      Npred[i] <- Npred[i] - Bpred[i] * (1 - drop2010) / poidsMoy81a85
    }
  }
  omegaPred <- Bpred / Npred #Poids moyen des individus de la population
  ##
  ## calcul du taux de perte d'étiquettes, et probabilité annuelle de perdre ses 2 étiquettes
  tauxPerte.age <- 0:100 #Vecteur du nombre d'années en mer
  tauxPerte.cummul <- assymptoteTauxPerte *
    (1 - exp(-accroissementTauxPerte * tauxPerte.age)) #Vecteur du taux de perte cummulatif selon le nombre d'années en mer
  tauxPerte.annuel <- c(0, diff(tauxPerte.cummul)) #Vecteur du taux de perte annuel, selon le nombre d'années en mer
  ## ##
  ## nbEtiqPerdu <- matrix(nrow=length(tauxPerte.age), ncol=5)
  ## nbEtiqPerdu[1,] <- c(1,0,0,0,0)
  ## for(i.an in 2:nrow(nbEtiqPerdu)){
  ##     ## 2étiquette; premierÉtiq; deuxièmeÉtiq; aucunÉtiq; taux1etiqSur2etiq
  ##     nbEtiqPerdu[i.an,1] <- nbEtiqPerdu[i.an-1,1]*(1-tauxPerte.annuel[i.an])^2
  ##     nbEtiqPerdu[i.an,2] <- nbEtiqPerdu[i.an-1,1]*(1-tauxPerte.annuel[i.an])*tauxPerte.annuel[i.an] +
  ##         nbEtiqPerdu[i.an-1,2]*(1-tauxPerte.annuel[i.an])
  ##     nbEtiqPerdu[i.an,3] <- nbEtiqPerdu[i.an-1,1]*(1-tauxPerte.annuel[i.an])*tauxPerte.annuel[i.an] +
  ##         nbEtiqPerdu[i.an-1,3]*(1-tauxPerte.annuel[i.an])
  ##     nbEtiqPerdu[i.an,4] <- nbEtiqPerdu[i.an-1,1]*tauxPerte.annuel[i.an]^2 +
  ##         nbEtiqPerdu[i.an-1,2]*tauxPerte.annuel[i.an] +
  ##         nbEtiqPerdu[i.an-1,3]*tauxPerte.annuel[i.an] +
  ##         nbEtiqPerdu[i.an-1,4]
  ##     nbEtiqPerdu[i.an,5] <- (nbEtiqPerdu[i.an,2]+nbEtiqPerdu[i.an,3])/nbEtiqPerdu[i.an,1]
  ## }
  ## nbEtiqPerdu <- cbind(nbEtiqPerdu, c(0,diff(nbEtiqPerdu[,4]))) #nouveau individus sortis de la pop à chaque année
  ##
  ## suivi des tags présents dans l'eau et recapturés
  nTag1 <- matrix(0, nrow = length(anneesFittees), ncol = length(anneesFittees)) #Matrice du nombre de poissons possédants 1 seul tag
  nTag2 <- matrix(0, nrow = length(anneesFittees), ncol = length(anneesFittees)) #Matrice du nombre de poissons possédants 2 tags
  nTagRetourPred <- matrix(
    NA,
    nrow = length(anneesFittees),
    ncol = length(anneesFittees)
  ) #Matrice du nombre de tags recapturés, il n'y a pas de captures l'année de marquage
  ##
  for (i in 1:(nrow(nTag1) - 1)) {
    nTag1[i, i] <- nTagsPoses[
      nTagsPoses$annee == anneesFittees[i] & nTagsPoses$nbTag == 1,
      'valeur'
    ] *
      sPostMarquage
    nTag2[i, i] <- nTagsPoses[
      nTagsPoses$annee == anneesFittees[i] & nTagsPoses$nbTag == 2,
      'valeur'
    ] *
      sPostMarquage
    for (j in (i + 1):ncol(nTag1)) {
      #remplir le triangle supérieur
      ## nTag.temp <- nTag[i,j-1] * (1-nbEtiqPerdu[j-i+1,5])
      ## nTag[i,j] <- nTag.temp * s[j]
      nTag1[i, j] <- nTag1[i, j - 1] *
        s[j] *
        (1 - tauxPerte.annuel[j - i + 1]) +
        nTag2[i, j - 1] *
          s[j] *
          2 *
          (1 - tauxPerte.annuel[j - i + 1]) *
          tauxPerte.annuel[j - i + 1]
      nTag2[i, j] <- nTag2[i, j - 1] *
        s[j] *
        (1 - tauxPerte.annuel[j - i + 1]) *
        (1 - tauxPerte.annuel[j - i + 1])
      nTagRetourPred[i, j] <- (nTag1[i, j - 1] + nTag2[i, j - 1]) *
        exp(-M) *
        tauxExp[j] *
        tauxRetour
    }
  }
  nTag1[nrow(nTag1), ncol(nTag1)] <- nTagsPoses[
    nTagsPoses$annee == anneesFittees[nrow(nTag1)] & nTagsPoses$nbTag == 1,
    'valeur'
  ] *
    sPostMarquage
  nTag2[nrow(nTag2), ncol(nTag2)] <- nTagsPoses[
    nTagsPoses$annee == anneesFittees[nrow(nTag2)] & nTagsPoses$nbTag == 2,
    'valeur'
  ] *
    sPostMarquage
  ##
  ## Recrutement théorique pour la partie horizontale de la relation SSR de type Hockey-Stick
  Rfixe <- mean(Rpred[which(anneesFittees %in% c(anneesRfixe))])
  ##
  ## Calcul des valeurs à l'équilibre pour déterminer les points de référence
  Uequilibre <- seq(0, 0.5, by = 0.001)
  kappa <- (1 -
    (1 + rho) * exp(-M) * (1 - Uequilibre) +
    rho * exp(-M)^2 * (1 - Uequilibre)^2) /
    (tail(omegaK$valeur, 1) - rho * omegaKmoins1 * exp(-M) * (1 - Uequilibre))
  Bequilibre <- Rfixe / kappa
  Cequilibre <- Bequilibre * Uequilibre
  ## lequel.msy <- which.max(Cequilibre)
  ## Umsy <- Uequilibre[lequel.msy]
  ## msy <- Cequilibre[lequel.msy]
  ## Bmsy <- Bequilibre[lequel.msy]
  ##

  ######
  ##
  ## Vraissemblances
  ##
  ######
  ##
  ## erreur de processus sur la biomasse, retirer le premier Bpred
  ## nll.Bproc <- -sum(dnorm(tail(log_Bpred,-1), log(Bpred.proc), sigma_Bproc, log=TRUE), na.rm=TRUE)
  ##
  ## erreur d'ajustement du taux cummulatif de perte d'étiquette, indépendant
  nll.tauxPerte <- -sum(dnorm(
    tauxPerte,
    tauxPerte.cummul[1:length(tauxPerte)],
    sigma_tauxPerte,
    log = TRUE
  ))
  # nll.tauxPerte <- -sum(dnorm(
  #   log(tauxPerteTag[-1, 'tauxPerte']),
  #   log(nbEtiqPerdu[2:nrow(tauxPerteTag), 5]),
  #   sigma_tauxPerte,
  #   log = TRUE
  # ))
  ##
  ## marche aléatoire du recrutement
  nll.recru <- -sum(
    dnorm(log(Rpred[-length(Rpred)]), log(Rpred[-1]), sigma_Rrw, log = TRUE),
    na.rm = TRUE
  )
  ##
  ## erreur d'ajustement de la longueur à l'age
  nll.longAge <- -sum(
    dnorm(
      croiss$longueur,
      linf * (1 - exp(-K * (croiss$age - t0))),
      sigma_longAge,
      log = TRUE
    ),
    na.rm = TRUE
  )
  ##
  ## erreur d'observation sur les captures
  nll.Cobs <- -sum(
    dnorm(
      log(Cobs[Cobs$annee %in% anneesFittees, 'valeur']),
      log(Cpred),
      sigma_C,
      log = TRUE
    ),
    na.rm = TRUE
  )
  ##
  ## erreur d'observation sur les indices d'abondance
  nll.Bobs <- 0
  ## indice d'abondance principal en absolu
  for (i in 1:nrow(Bobs_abs)) {
    if (
      is.finite(log(Bobs_abs[i, 2])) &
        Bobs_abs[i, 1] %in% anneesFittees
    ) {
      annee <- anneesFitteesID[anneesFittees == as.character(Bobs_abs[i, 1])] #aller chercher l'indice correspondant à l'année
      source <- Bobs_abs[i, 3]
      sigma <- Bobs_abs[i, 4]
      nll.Bobs <- nll.Bobs -
        dnorm(
          log(Bobs_abs[i, 2]),
          log(Bpred[annee] * qRel_abs[source]),
          sigma_Bobs_abs[sigma],
          log = TRUE
        )
    }
  }
  ## indice d'abondance secondaire, possiblement en relatif
  for (i in 1:nrow(Bobs)) {
    if (
      is.finite(log(Bobs[i, 2])) &
        Bobs[i, 1] %in% anneesFittees
    ) {
      annee <- anneesFitteesID[anneesFittees == as.character(Bobs[i, 1])] #aller chercher l'indice correspondant à l'année
      source <- Bobs[i, 3]
      sigma <- Bobs[i, 4]
      nll.Bobs <- nll.Bobs -
        dnorm(
          log(Bobs[i, 2]),
          log(Bpred[annee] * qRel[source]),
          sigma_Bobs[sigma],
          log = TRUE
        )
    }
  }
  ##
  ## erreur d'observation sur les indices de recrutement
  nll.Robs <- 0
  for (i in 1:nrow(Robs)) {
    if (
      is.finite(log(Robs[i, 2])) &
        Robs[i, 1] %in% anneesFittees
    ) {
      annee <- anneesFitteesID[anneesFittees == as.character(Robs[i, 1])] #aller chercher l'indice correspondant à l'année
      source <- Robs[i, 3]
      sigma <- Robs[i, 4]
      if (annee <= max(anneesFittees - 1)) {
        nll.Robs <- nll.Robs -
          dnorm(
            log(Robs[i, 2]),
            log(Rpred[annee] * qRecru[source]),
            sigma_Robs[sigma],
            log = TRUE
          )
      }
    }
  }
  ##
  ## erreur d'observation sur les poids moyens
  nll.oBar <- 0
  for (i in 1:nrow(Oobs)) {
    if (
      is.finite(log(Oobs[i, 2])) &
        Oobs[i, 1] %in% anneesFittees
    ) {
      annee <- anneesFitteesID[anneesFittees == as.character(Oobs[i, 1])] #aller chercher l'indice correspondant à l'année
      source <- Oobs[i, 3]
      sigma <- Oobs[i, 4]
      nll.oBar <- nll.oBar -
        dnorm(Oobs[i, 2], omegaPred[annee], sigma_oBar[sigma], log = TRUE)
    }
  }
  ##
  ## erreur d'observation sur les retours d'étiquettes
  nll.tag <- 0
  for (i in 1:nrow(nTagsRetourObs)) {
    if (nTagsRetourObs[i, 'anneeRecap'] %in% anneesFittees) {
      anPose <- anneesFitteesID[
        anneesFittees == as.character(nTagsRetourObs[i, 'anneePose'])
      ]
      anRecap <- anneesFitteesID[
        anneesFittees == as.character(nTagsRetourObs[i, 'anneeRecap'])
      ]
      ## nll.tag <- nll.tag - dnorm(nTagsRetourObs[i,'valeur'], nTagRetourPred[anPose,anRecap], sigma_retourTag, log=TRUE)
      ## nll.tag <- nll.tag - dnorm(log(nTagsRetourObs[i,'valeur']), log(nTagRetourPred[anPose,anRecap]), sigma_retourTag, log=TRUE)
      nll.tag <- nll.tag -
        dpois(
          nTagsRetourObs[i, 'valeur'],
          nTagRetourPred[anPose, anRecap],
          log = TRUE
        )
    }
  }
  ##
  ## prior sur N0
  ## nll.N0 <- -dnorm(log(N0), log(Bpred[1]/quantile(Oobs$valeur, 0.25)), sd=5, log=TRUE)
  ##
  ## prior sur omega0
  nll.b0n0 <- -dnorm(log(B0 / N0), log(15), sd = 1, log = TRUE)
  ##
  ## prior sur M
  ## nll.M <- -dnorm(M, 0.12516, sd=0.05597339, log=TRUE)
  ## nll.M <- -dnorm(M, 0.1698, sd=0.0537, log=TRUE)
  ##
  ## ## calcul de M selon mortalité des tags satellites
  ## rcensor= rcensor.DR2 #rcensor.DR #
  ## ## Censored survival:
  ## D = exp(-M*time.DR/365)
  ## ## Density for each uncensored observations:
  ## d <- (M/365) * D
  ## ## Calculate log-likelihood values: [set to zero components not to include in estimates]
  ## nll.DR <- sum(-(1-rcensor)*log(d) - rcensor*log(D))
  ##
  ##
  ##
  ## vraissemblance totale
  nll <- #nll.Bproc +
    nll.recru +
    nll.longAge +
    nll.Cobs +
    nll.Bobs +
    nll.Robs +
    nll.oBar +
    nll.tag +
    nll.b0n0 +
    nll.tauxPerte # +
  ## nll.DR +
  ## nll.M# +
  ## nll.N0
  ## nll.ssr
  ##
  ##
  ##
  ##
  ## Sorties
  ## REPORT(nll.Bproc)
  REPORT(nll.recru)
  REPORT(nll.Robs)
  REPORT(nll.Bobs)
  REPORT(nll.oBar)
  REPORT(nll.Cobs)
  REPORT(nll.longAge)
  REPORT(nll.tag)
  REPORT(nll.tauxPerte)
  ## REPORT(nll.ssr)
  ## REPORT(nll.M)
  ## REPORT(nll.DR)
  REPORT(sigma_Bobs_abs)
  REPORT(sigma_Bobs)
  ## REPORT(sigma_Bproc)
  REPORT(sigma_oBar)
  REPORT(sigma_C)
  REPORT(sigma_Rrw)
  REPORT(sigma_Robs)
  REPORT(sigma_longAge)
  ## REPORT(sigma_retourTag)
  REPORT(sigma_tauxPerte)
  ##
  REPORT(N0)
  REPORT(Bpred)
  ## REPORT(Bpred.proc)
  REPORT(Npred)
  REPORT(Rpred)
  REPORT(s)
  REPORT(tauxExp)
  REPORT(qRel_abs)
  REPORT(qRel)
  REPORT(qRecru)
  REPORT(Cpred)
  REPORT(omegaPred)
  REPORT(rho)
  REPORT(alpha)
  REPORT(M)
  REPORT(F)
  REPORT(Z)
  REPORT(linf)
  REPORT(K)
  REPORT(t0)
  REPORT(nTag1)
  REPORT(nTag2)
  REPORT(nTagRetourPred)
  REPORT(tauxRetour)
  REPORT(sPostMarquage)
  REPORT(accroissementTauxPerte)
  REPORT(assymptoteTauxPerte)
  ## REPORT(nbEtiqPerdu)
  REPORT(Rfixe)
  REPORT(Uequilibre)
  REPORT(Bequilibre)
  REPORT(Cequilibre)
  REPORT(nll)
  ADREPORT(omegaPred)
  ADREPORT(Bpred)
  ##
  nll
}
