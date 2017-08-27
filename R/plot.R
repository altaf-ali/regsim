#' Plot Regression Simulations
#'
#' plots regression simulation results
#'
#' @param x an object of class "regsim", usually obtained by calling the
#' \code{regsim} function.
#' @param var variable to plot on the x-axis
#' @param ... additional arguments passed to class-specific functions
#' @examples
#' library(regsim)
#'
#' model <- lm(mpg ~ wt + cyl, data = mtcars)
#' sim <- regsim(model, list(wt = seq(1, 5, 0.1), cyl = mean(mtcars$cyl)))
#' plot(sim, ~wt)
#' @export
plot.regsim <- function(x, var, ...) {
  # defaults
  lty <- 1
  lty.ci <- 2
  regsim_summary <- summary(x)

  xvar <- labels(stats::terms(var))[1]
  if (!xvar %in% names(regsim_summary)) {
    stop(paste(xvar, "not in the model"))
  }

  ci <- data.frame(
    x = regsim_summary[, xvar],
    y = regsim_summary[, "50%"],
    y_min = regsim_summary[, "2.5%"],
    y_max = regsim_summary[, "97.5%"]
  )
  colnames(ci)[1] <- xvar
  colnames(ci)[2] <- "Expected.Value"

  do_plot <- function(f, data, lty, lty.ci, ...) {
    graphics::plot(f,
                   data,
                   type = "l",
                   lty = lty,
                   ...)

    graphics::lines(data[,1], data$y_min, lty = lty.ci)
    graphics::lines(data[,1], data$y_max, lty = lty.ci)
  }

  do.call(
    do_plot,
    list(
      stats::as.formula(paste(rev(colnames(ci)[1:2]), collapse = " ~ ")),
      data = ci,
      lty = lty,
      lty.ci = lty.ci
    )
  )
}
