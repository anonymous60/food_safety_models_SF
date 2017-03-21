
#'
#'
#' sffoodinspectr package
#'
#' @docType package
#' @name sffoodinspectr
#' @author
#
NULL

.onLoad <- function(libname, pkgname) {
  utils::data("Model_ABT", "Model_randomForest_mTry",
              package=pkgname, envir=parent.env(environment()))
}
