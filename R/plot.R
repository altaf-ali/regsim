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

  var_labels <- labels(stats::terms(var))

  # if (!all(var_labels %in% names(regsim_summary))) {
  #   stop(paste(xvar, "not in the model"))
  # }

  xvar <- var_labels[1]

  if (length(var_labels) > 1) {
    zvar <- var_labels[2]
  } else {
    zvar <- "z"
    regsim_summary[, zvar] <- 0
  }

  plot_data <- data.frame(
    x = regsim_summary[, xvar],
    y = regsim_summary[, "50%"],
    y_min = regsim_summary[, "2.5%"],
    y_max = regsim_summary[, "97.5%"],
    z = regsim_summary[, zvar]
  )

  default_args <- list(
    xlim = range(plot_data$x),
    ylim = c(max(0, min(plot_data$y)-sd(plot_data$y)), c(min(1, max(plot_data$y)+sd(plot_data$y)))),
    xlab = xvar,
    ylab = "Expected Value"
  )

  # capture ... args
  args <- list(...)
  default_args <- default_args[setdiff(names(default_args), names(args))]

  plot_create <- function(...) {
    graphics::plot(0,pch = "",...)
  }

  do.call(plot_create, c(default_args, args))

  groups <- sort(unique(regsim_summary[, zvar]))

  # not a good idea to do this, but ok for now
  color_map <- color_add_alpha(
    c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF"),
    alpha = 0.4
  )

  for (i in 1:length(groups)) {
    group_data <- plot_data[plot_data$z == groups[i], ]

    graphics::polygon(x = c(group_data$x, rev(group_data$x)),
                      y = c(group_data$y_min, rev(group_data$y_max)),
                      border = NA,
                      col = color_map[i])

    graphics::lines(group_data$x, group_data$y)
  }

  if (length(groups) > 1) {
    graphics::legend("bottomright",
                     y = NULL,
                     groups,
                     inset = .02,
                     title = zvar,
                     fill = color_map)
  }
}

color_add_alpha <- function(col, alpha = 1) {
  apply(sapply(col, grDevices::col2rgb) / 255, 2, function(x) {
    grDevices::rgb(x[1], x[2], x[3], alpha = alpha)
  })
}
