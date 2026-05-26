#' Exécute l'ajustement du modèle Delay-Difference en RTMB
#'
#' @returns Null
#' @export
#'
#' @import RTMB
#'
#' @examples run_dd()
run_dd <- function() {
  ##
  load(file = file.path('data', 'dd_data.RData'), verbose = 1)
  load(file = file.path('data', 'dd_param.RData'), verbose = 1)
  ##
  randomVal <- c('logRpred')
  ##
  mapVal <- list(logSigma_C = factor(NA), trans_M = factor(NA))
  ##
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
}
