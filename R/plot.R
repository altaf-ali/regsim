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
#' x <- list(
#'   wt = seq(1, 5, 0.1),
#'   cyl = mean(mtcars$cyl)
#' )
#' sim <- regsim(model, x)
#' plot(sim, ~wt)
#' @export
plot.regsim <- function(x, formula, ...) {
  regsim_summary <- summary(x)

  vars <- labels(stats::terms(formula))
  unknown_vars <- setdiff(vars, names(regsim_summary))

  if (length(unknown_vars))
    stop(paste(paste(unknown_vars, collapse = ", "), "not in the model"))

  xvar <- vars[1]

  if (length(vars) > 1) {
    zvar <- vars[2]
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
    grepl(sprintf("^%s.", arg_type), names(args))
  }, args, simplify = FALSE))

  layer_args <- sapply(arg_matrix, function(arg_type, args) {
    args <- args[arg_type]
    names(args) <- gsub("^.*?\\.", "", names(args))
    return(args)
  }, args, simplify = FALSE)

  if (nrow(arg_matrix))
    plot_args <- merge_args(plot_args, args[apply(!arg_matrix, 1, all)])

  do.call(graphics::plot, plot_args)

  groups <- sort(unique(regsim_summary[, zvar]))

  polygon_args <- list(
    border = NA,
    col = "Set1"
  )

  layer_args$polygon <- merge_args(polygon_args, layer_args$polygon)

  # special handling of color specific args
  alpha <- 0.4
  layer_args$lines <- set_color_args(layer_args$lines, "col", length(groups))
  layer_args$polygon <- set_color_args(layer_args$polygon, "col", length(groups), alpha = alpha)
  layer_args$legend <- set_color_args(layer_args$legend, "fill", length(groups), alpha = alpha)

  # run through all the groups
  for (i in 1:length(groups)) {
    group_data <- plot_data[plot_data$z == groups[i], ]

    polygon_args <- list(
      x = c(group_data$x, rev(group_data$x)),
      y = c(group_data$y_min, rev(group_data$y_max))
    )

    do.call(graphics::polygon, merge_args(polygon_args, layer_args$polygon, length(groups), i))

    lines_args <- list(
      x = group_data$x,
      y = group_data$y
    )

    do.call(graphics::lines, merge_args(lines_args, layer_args$lines, length(groups), i))
  }

  if (length(groups) > 1) {

    # create legend if there are more groups
    legend_args <- list(
      x = "bottomright",
      y = NULL,
      legend = groups,
      title = zvar,
      fill = layer_args$polygon$col,
      y.intersp = 0.8
    )

    if (plot_data$y[nrow(plot_data)/length(groups)] < mean(graphics::par("usr")[3:4]))
      legend_args$x <- "topright"

    do.call(graphics::legend, merge_args(legend_args, layer_args$legend))
  }
}

# merge default and user-specified args
merge_args <- function(default_args, args, n = 0, i = 0) {
  args <- c(default_args[setdiff(names(default_args), names(args))], args)
  sapply(args, function(arg) {
    if ((length(arg) == n) && i)
      arg[[i]]
    else
      arg
  }, simplify = FALSE)
}

# set color arg from brewer palette if one is provided
set_color_args <- function(arg_list, color_key, n, alpha = 1) {

  is.brewer.pal <- function(arg) {
    arg %in% rownames(RColorBrewer::brewer.pal.info)
  }

  sapply(names(arg_list), function(key) {
    val <- arg_list[[key]]

    if (key == color_key && length(val) == 1 && is.brewer.pal(val)) {
      color_pal <- as.vector(grDevices::adjustcolor(
        RColorBrewer::brewer.pal(max(3, n), val),
        alpha.f = alpha))

      color_pal[1:n]
    }
    else {
      val
    }
  }, simplify = FALSE)
}
