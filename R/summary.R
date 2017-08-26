#' Summarizing Regression Simulations
#'
#' summary method for class "regsim"
#'
#' @param object an object of class "regsim", usually obtained by calling the
#' \code{regsim} function.
#' @param ... additional arguments passed to class-specific functions
#' @examples
#' library(regsim)
#'
#' model <- lm(mpg ~ wt + cyl, data = mtcars)
#' sim <- regsim(model, list(wt = seq(1, 5, 0.1), cyl = mean(mtcars$cyl)))
#' summary(sim)
#' @export
summary.regsim <- function(object, ...) {
  x <- as.data.frame(t(object$x))
  colnames(x) <- names(stats::coefficients(object$model))

  ev_summary <- cbind(
    x,
    t(apply(object$ev, 2, function(x) {
      c(mean = mean(x),
        sd = stats::sd(x),
        stats::quantile(x, probs = c(.025, .5, .975))
      )
    })))

  return(ev_summary)
}
