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
  regsim_summary <- summary(x)

  xvar <- labels(stats::terms(var))[1]
  if (!xvar %in% names(regsim_summary)) {
    stop(paste(xvar, "not in the model"))
  }

  plot_data <- data.frame(
    x = regsim_summary[, xvar],
    y = regsim_summary[, "50%"],
    y_min = regsim_summary[, "2.5%"],
    y_max = regsim_summary[, "97.5%"]
  )

  plot_mean <- function(x, y, ...) {
    graphics::plot(x, y, ...)
  }

  default_args <- list(
    type = "l",
    xlab = xvar,
    ylab = "Expected Value"
  )

  # capture ... args and split between plot and ci args
  args <- list(...)
  ci_suffix <- ".ci$"
  ci_arg_indices <- grepl(ci_suffix, names(args))
  plot_args <- args[!ci_arg_indices]
  ci_args <- args[ci_arg_indices]
  names(ci_args) <- gsub(ci_suffix, "", names(ci_args))

  default_args <- default_args[setdiff(names(default_args), names(plot_args))]

  do.call(plot_mean, c(list(x = plot_data$x,
                            y = plot_data$y),
                       default_args,
                       plot_args))

  # plot confidence interval
  plot_ci <- function(x, y_min, y_max, ...) {
    graphics::lines(x, y_min, ...)
    graphics::lines(x, y_max, ...)
  }

  default_args <- list(
    lty = 2
  )

  default_args <- default_args[setdiff(names(default_args), names(ci_args))]

  do.call(plot_ci, c(list(x = plot_data$x,
                          y_min = plot_data$y_min,
                          y_max = plot_data$y_max),
                     default_args,
                     ci_args))
}
