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

regsim_common <- function(object, x, num, link = NULL) {
  coefs <- stats::coefficients(object)

  # check explanatory variables given to us
  unknown_vars <- setdiff(names(x), names(coefs))

  if (length(unknown_vars)) {
    stop(paste(paste(unknown_vars, collapse = ", "), "not in the model"))
  }

  # convert list of explanatory variables to a data.frame
  xdf <- as.data.frame(x)

  # setup profile matrix
  xprofiles <- matrix(nrow = length(coefs), ncol = nrow(xdf))

  # fill in what's given and set other variables to mean
  for (i in seq_along(coefs)) {
    xval <- NA # hopefully this will catch bugs or user errors
    term <- names(coefs)[i]

    if (term %in% names(xdf)) {
      xval <- xdf[, term]
    } else {
      if (term == "(Intercept)") {
        xval <- 1
      } else {
        xval <- mean(object$model[, term])
      }
    }
    xprofiles[i,] <- xval
  }

  # abort if anything is still NA
  if (any(is.na(xprofiles))) {
    stop("can't continue with NAs")
  }

  ev <- MASS::mvrnorm(num, coefs, stats::vcov(object)) %*% xprofiles

  if (!is.null(link))
    ev <- link(ev)

  return_value <- list(
    model = object,
    x = xprofiles,
    ev = ev
  )

  class(return_value) <- "regsim"

  return(return_value)
}
