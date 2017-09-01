#' Regression Simulation for Linear Models
#'
#' \code{regsim.lm} simulate quantities of interest from a linear regression model
#'
#' @param object a regression model, usually, a result of a call to \link[stats]{lm}
#' @param x a list of explanatory variables
#' @param num number of iterations to run
#' @param ... additional arguments passed to class-specific functions
#' @examples
#' library(regsim)
#'
#' model <- lm(mpg ~ wt + cyl, data = mtcars)
#' x <- list(
#'   wt = seq(1, 5, 0.1),
#'   cyl = mean(mtcars$cyl)
#' )
#' sim <- regsim(model, x)
#' summary(sim)
#' @export
regsim.lm <- function(object, x, num = 1000, ...) {
  return(regsim_common(object, x, num, ...))
}
