#' Plot Regression Simulations
#'
#' plots regression simulation results
#'
#' @param x an object of class \code{regsim}, usually obtained by calling the
#' \link{regsim} function.
#' @param formula a \link[stats]{formula} such as ~x or ~x1 + x2
#' @param ... additional arguments passed to class-specific functions
#' @examples
#' library(regsim)
#'
#' model <- lm(mpg ~ wt + cyl, data = mtcars)
#' sim <- regsim(model, list(wt = seq(1, 5, 0.1), cyl = mean(mtcars$cyl)))
#' plot(sim, ~wt)
#' @export
plot.regsim <- function(x, formula, ...) {
  regsim_summary <- summary(x)

  vars <- labels(stats::terms(formula))

  # if (!all(vars %in% names(regsim_summary))) {
  #   stop(paste(xvar, "not in the model"))
  # }

  xvar <- vars[1]

  if (length(vars) > 1) {
    zvar <- tryCatch(colnames(stats::model.matrix(formula, x$model$model))[3], error = vars[2])
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
  plot_data <- plot_data[order(plot_data$z, plot_data$x), ]

  plot_args <- list(
    x = NULL,
    xlim = range(plot_data$x),
    ylim = c(min(plot_data$y_min), max(plot_data$y_max)),
    xlab = xvar,
    ylab = "Expected Value"
  )

  # capture all ... args
  args <- list(...)

  # split args by each layer
  layers <- c("lines", "legend", "polygon")

  arg_matrix <- as.data.frame(sapply(layers, function(arg_type, args) {
    suffix <- sprintf("^%s.", arg_type)
    grepl(suffix, names(args))
  }, args, simplify = FALSE))

  layer_args <- sapply(arg_matrix, function(arg_type, args) {
    args <- args[arg_type]
    names(args) <- gsub("^.*?\\.", "", names(args))
    return(args)
  }, args, simplify = FALSE)

  # merge default and user-specified args
  merge_args <- function(default_args, args) {
    c(default_args[setdiff(names(default_args), names(args))], args)
  }

  if (nrow(arg_matrix))
    plot_args <- merge_args(plot_args, args[apply(!arg_matrix, 1, all)])

  do.call(graphics::plot, plot_args)

  groups <- sort(unique(regsim_summary[, zvar]))

  polygon_args <- list(
    border = NA,
    col = "Set1"
  )

  polygon_args <- merge_args(polygon_args, layer_args$polygon)

  # see if it's a brewer palette
  if (length(polygon_args$col) == 1) {
    if (polygon_args$col %in% rownames(RColorBrewer::brewer.pal.info)) {
      polygon_args$col <- as.vector(grDevices::adjustcolor(
        RColorBrewer::brewer.pal(max(3, length(groups)), polygon_args$col),
        alpha.f = 0.4
      ))
    }
  }

  # if not enough colors, then just duplicate them
  if (length(polygon_args$col) < length(groups))
    polygon_args$col <- rep(polygon_args$col, length(groups))

  # run through all the groups
  for (i in 1:length(groups)) {
    group_data <- plot_data[plot_data$z == groups[i], ]

    group_args <- list(
      x = c(group_data$x, rev(group_data$x)),
      y = c(group_data$y_min, rev(group_data$y_max)),
      col = polygon_args$col[i]
    )

    do.call(graphics::polygon, merge_args(polygon_args, group_args))

    lines_args <- list(
      x = group_data$x,
      y = group_data$y
    )

    do.call(graphics::lines, merge_args(lines_args, layer_args$lines))
  }

  if (length(groups) > 1) {

    # create legend if there are more groups
    legend_args <- list(
      x = "bottomright",
      y = NULL,
      legend = groups,
      title = zvar,
      fill = polygon_args$col,
      y.intersp = 0.8
    )

    if (plot_data$y[nrow(plot_data)/length(groups)] < mean(graphics::par("usr")[3:4]))
      legend_args$x <- "topright"

    do.call(graphics::legend, merge_args(legend_args, layer_args$legend))
  }
}


