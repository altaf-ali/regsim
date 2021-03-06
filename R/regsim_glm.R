#' Regression Simulation for Generalized Linear Models
#'
#' \code{regsim.glm} simulates quantities of interest from a generalized linear model
#'
#' @param object a regression model, usually, a result of a call to \link[stats]{glm}
#' @param x a list of explanatory variables
#' @param num number of iterations to run
#' @param link model link function
#' @param ... additional arguments passed to class-specific functions
#' @examples
#' library(regsim)
#'
#' y <- swiss$Fertility > median(swiss$Fertility)
#' model <- glm(y ~ Education + Agriculture, family = binomial, data = swiss)
#' x <- list(Agriculture = seq(1, 100, 5))
#' sim <- regsim(model, x)
#' summary(sim)
#' @export
regsim.glm <- function(object, x, num = 1000, link = NULL, ...) {
  if (is.null(link))
    link <- stats::family(object)$linkinv

  regsim_common(object, x, num, link)
}
