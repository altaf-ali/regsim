#' Plot Regression Simulations
#'
#' plots regression simulation results
#'
#' @param object an object of class "regsim", usually obtained by calling the
#' \code{regsim} function.
#' @param xvar variable to plot on the x-axis
#' @param ... additional arguments passed to class-specific functions
#' @examples
#' library(regsim)
#'
#' model <- lm(mpg ~ wt + cyl, data = mtcars)
#' sim <- regsim(model, list(wt = seq(1, 5, 0.1), cyl = mean(mtcars$cyl)))
#' plot(sim, ~wt)
#' @export
plot.regsim <- function(object, xvar, ...) {
  regsim_summary <- summary(object)

  x <- labels(stats::terms(xvar))[1]
  if (!x %in% names(regsim_summary)) {
    stop(paste(x, "not in the model"))
  }

  ci <- data.frame(
    x = regsim_summary[, x],
    y = regsim_summary[, "50%"],
    y_min = regsim_summary[, "2.5%"],
    y_max = regsim_summary[, "97.5%"]
  )

  graphics::plot(y ~ x, data = ci, type = "l", ...)
  graphics::lines(ci$x, ci$y_min, lty = 2)
  graphics::lines(ci$x, ci$y_max, lty = 2)
}