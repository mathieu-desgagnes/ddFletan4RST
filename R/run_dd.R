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
  randomVal <- c('logRpred')
  ##
  mapVal <- list(logSigma_C = factor(NA))
  ##
  obj <- RTMB::MakeADFun(fnll, dd_param, random = randomVal, map = mapVal)
  ##
}
