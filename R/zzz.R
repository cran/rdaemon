#' @useDynLib rdaemon, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @import futile.logger
#' @import base64enc
#' @import utils
NULL

.onLoad <- function(libname, pkgname){
    clientData$lastRegisteredDaemon <- paste0("daemon_", Sys.getpid())
    clientData$lastRegisteredTaskId <- paste0("task_", Sys.getpid())
}
