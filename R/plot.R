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
plot.regsim <- function(x, var, emphedges = FALSE, ...) {
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

  # demphazie edges
  if (emphedges){

     # sample 200 ev's
    ifelse(nrow(x$ev)>201, p.ev <- apply(x$ev, 2, function(x) sample(x, 201)), p.ev <- x$ev)

    # oder each column
    p.ev <- apply(p.ev, 2, sort)

    default_args <- list(
      xlim = range(regsim_summary[, xvar]),
      ylim = c(max(0, min(p.ev)-sd(p.ev)), c(min(1, max(p.ev)+sd(p.ev)))),
      xlab = xvar,
      ylab = "Expected Value"
    )

    plot_data <- data.frame(
        z = regsim_summary[, zvar]
    )

    # capture ... args
    args <- list(...)
    default_args <- default_args[setdiff(names(default_args), names(args))]

    plot_create <- function(...) {
      graphics::plot(0,pch = "",...)
    }

    do.call(plot_create, c(default_args, args))

    groups <- sort(unique(regsim_summary[, zvar]))

    for (i in 1:length(groups)) {
      group_data <- plot_data[plot_data$z == groups[i], ]

      # uncertainty
      for (a in 1:100){

        # not a good idea to do this, but ok for now
        color_map <- color_add_alpha(
          c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF"),
          alpha = max(30,60-a*.4) / 100
        )

        graphics::lines(regsim_summary[, xvar], p.ev[101-a,], col=color_map[i])
        graphics::lines(regsim_summary[, xvar], p.ev[101+a,], col=color_map[i])
      }

      # expected value
      graphics::lines(regsim_summary[, xvar], p.ev[101,], col=color_map[i], lwd = 2)

      if (length(groups) > 1) {
        graphics::legend("bottomright",
                         y = NULL,
                         groups,
                         inset = .02,
                         title = zvar,
                         fill = color_map)
        }
    }
  }

  # default plot
  if (!emphedges) {
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


}

color_add_alpha <- function(col, alpha = 1) {
  apply(sapply(col, grDevices::col2rgb) / 255, 2, function(x) {
    grDevices::rgb(x[1], x[2], x[3], alpha = alpha)
  })
}
