#' Exécute l'ajustement du modèle Delay-Difference en RTMB
#'
#' @returns Null
#' @export
#'
#' @import RTMB
#'
#' @examples ##DO NOT RUN
run_dd <- function() {
  ##
  ## load_all()
  ##
  load(file = file.path('data', 'dd_data.RData'), verbose = 1)
  load(file = file.path('data', 'dd_param.RData'), verbose = 1)
  ##
  randomVal <- c('log_Rpred')
  ##
  mapVal <- list(log_sigma_C = factor(NA), trans_M = factor(NA))
  ##
  source(file.path('R', 'fnll.R'))
  obj <- RTMB::MakeADFun(fnll, dd_param, random = randomVal, map = mapVal)
  ##
  fit <- nlminb(
    obj$par,
    obj$fn,
    obj$gr,
    control = list(eval.max = 100000000, iter.max = 100000000)
  )
  ##
  sdr <- sdreport(obj)
  pl <- as.list(sdr, "Est")
  plsd <- as.list(sdr, "Std")
  plr <- as.list(sdr, "Est", report = TRUE)
  plrsd <- as.list(sdr, "Std", report = TRUE)
  ##
  par(mfcol = c(3, 7), mar = c(4, 4, 0, 1) + 0.1)
  ylimLog <- c(-3.1, 3.1)
  graph_B_fit(
    donnee = dd_data,
    param = dd_param,
    objReport = obj$report(),
    langue = 'fr',
    ylimLog = ylimLog,
    pl = pl,
    plsd = plsd
  )
  graph_B_residus(
    donnee = dd_data,
    param = dd_param,
    objReport = obj$report(),
    langue = 'fr',
    ylimLog = ylimLog,
    pl = pl,
    plsd = plsd
  )
  graph_R(
    donnee = dd_data,
    param = dd_param,
    objReport = obj$report(),
    langue = 'fr',
    ylimLog = ylimLog,
    pl = pl,
    plsd = plsd
  )
  graph_kobe(
    donnee = dd_data,
    param = dd_param,
    objReport = obj$report(),
    langue = 'fr',
    pl = pl,
    plsd = plsd
  )
  graph_omega(
    donnee = dd_data,
    param = dd_param,
    objReport = obj$report(),
    langue = 'fr',
    ylimLog = ylimLog,
    plr = plr,
    plrsd = plrsd
  )
  ## graph.N(donnee=dd_data, param=dd_param, objReport=obj$report(), langue='fr', tacProj=tacProj)
  graph_C(
    donnee = dd_data,
    param = dd_param,
    objReport = obj$report(),
    langue = 'fr'
  )
  graph_F(
    donnee = dd_data,
    param = dd_param,
    objReport = obj$report(),
    langue = 'fr',
    pl = pl,
    plsd = plsd
  )
  graph_retour_tag_bubble(
    donnee = dd_data,
    param = dd_param,
    objReport = obj$report(),
    langue = 'fr'
  )
  graph_retour_tag_total(
    donnee = dd_data,
    param = dd_param,
    objReport = obj$report(),
    langue = 'fr'
  )
  graph_taux_perte_tag(
    donnee = dd_data,
    objReport = obj$report(),
    langue = 'fr'
  )
  ##
  ## graph.SSR(donnee=dd_data, param=dd_param, objReport=obj$report(), langue='fr', lesquels=1)
  graph_age_longueur(
    donnee = dd_data,
    param = dd_param,
    objReport = obj$report(),
    langue = 'fr'
  )
  ##
  sdr
}
