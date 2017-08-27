#' Regression Simulation
#'
#' \code{regsim} simulates quantities of interest from regression models
#'
#' @param object a regression model, usually, a result of a call to \code{lm} or
#' \code{glm}
#' @param x a list of explanatory variables
#' @param num number of iterations to run
#' @param ... additional arguments passed to class-specific functions
#' @examples
#' library(regsim)
#'
#' model <- lm(mpg ~ wt + cyl, data = mtcars)
#' sim <- regsim(model, list(wt = seq(1, 5, 0.1), cyl = mean(mtcars$cyl)))
#' summary(sim)
#' @export
regsim <- function (object, x, num = 1000, ...) {
  UseMethod("regsim", object)
}

formula_rhs <- function(object) {
  f <- stats::formula(object)[[3]]
  eval(substitute(~ f))
}

central_tendency <- function(x) {
  if (is.character(x))
    x <- as.factor(x)

  if (is.factor(x))
    return(factor(names(which.max(table(x))), levels = levels(x)))

  if (is.integer(x) && unique(x) == 2)
    return(stats::median(x))

  return(mean(x))
}

regsim_common <- function(object, x, num = 1000, link = NULL) {
  coefs <- stats::coefficients(object)

  # check explanatory variables given to us
  unknown_vars <- setdiff(names(x), names(coefs))

  if (length(unknown_vars)) {
    stop(paste(paste(unknown_vars, collapse = ", "), "not in the model"))
  }

  # convert list of explanatory variables to a data.frame
  xprofiles <- expand.grid(x)

  formula_rhs_terms <- attr(stats::terms(stats::formula(object)), "term.labels")
  missing_vars <- setdiff(formula_rhs_terms, names(x))
  available_vars <- intersect(missing_vars, colnames(object$model))

  xprofiles[,available_vars] <- lapply(object$model[,available_vars, drop = FALSE], central_tendency)

  # construct model matrix from model formula and data provided
  design_matrix <- stats::model.matrix(formula_rhs(object), xprofiles)

  # abort if anything is still NA
  if (any(is.na(xprofiles))) {
    stop("can't continue with NAs")
  }

  ev <- MASS::mvrnorm(num, coefs, stats::vcov(object)) %*% t(design_matrix)

  if (!is.null(link))
    ev <- link(ev)

  return_value <- list(
    model = object,
    x = design_matrix,
    ev = ev
  )

  class(return_value) <- "regsim"

  return(return_value)
}

