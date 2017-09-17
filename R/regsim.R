#' Regression Simulation
#'
#' \code{regsim} simulates quantities of interest from regression models
#'
#' @param object a regression model, usually, a result of a call to
#' \link[stats]{lm} or \link[stats]{glm}
#' @param x a list of explanatory variables
#' @param num number of iterations to run
#' @param ... additional arguments passed to class-specific functions
#' @examples
#' library(regsim)
#'
#' model <- lm(mpg ~ wt + cyl, data = mtcars)
#' x <- list(wt = seq(1, 5, 0.1))
#' sim <- regsim(model, x)
#' summary(sim)
#' @export
regsim <- function (object, x, num = 1000, ...) {
  UseMethod("regsim", object)
}

#----------------------------------------------------------------------
# extract right hand side of a formula
formula_rhs <- function(object) {
  f <- stats::formula(object)[[3]]
  eval(substitute(~ f))
}

#----------------------------------------------------------------------
# return the central tendency of a vector
central_tendency <- function(x) {
  if (is.character(x))
    x <- as.factor(x)

  if (is.factor(x))
    return(factor(names(which.max(table(x))), levels = levels(x)))

  if (any(is.integer(x), is.numeric(x)) && length(unique(x)) == 2)
    return(stats::median(x))

  return(mean(x))
}

#----------------------------------------------------------------------
# return formula terms from a model object, stripping any functions
formula_terms <- function(object) {
  term_labels <- attr(stats::terms(stats::formula(object)), "term.labels")

  # this captures any transformation functions (log, lag, as.factor, poly, etc)
  regex_expr <- "^([a-zA-Z][a-zA-Z0-9_\\.]*)(?:\\s*\\(\\s*([a-zA-Z0-9_\\.]*\\s*)(?:,\\s*[a-zA-Z0-9_\\.]*\\s*)*\\)){0,1}\\s*$"
  regex_matches <- regmatches(term_labels, regexec(regex_expr, term_labels))

  regex_matches <- lapply(regex_matches, trimws)                    # trim whitespace
  regex_matches <- lapply(regex_matches, setdiff, '')               # remove empty strings
  regex_matches <- regex_matches[lapply(regex_matches, length) > 0] # remove 0-length vectors
  return(sapply(regex_matches, tail, n=1))
}

#----------------------------------------------------------------------
# common regsim function called by regsim.lm and regsim.glm
regsim_common <- function(object, x, num = 1000, link = NULL) {
  formula_rhs_terms <- formula_terms(object)

  # check explanatory variables given to us
  unknown_vars <- setdiff(names(x), formula_rhs_terms)

  # poly option
  a.poly <- grepl("poly", formula_rhs_terms)
  if (length(a.poly)) a.poly <- which(a.poly)
  if(length(unknown_vars)){
    if(grepl(unknown_vars[a.poly], formula_rhs_terms[a.poly]) & grepl("poly", formula_rhs_terms[a.poly])){
      if (length(unknown_vars)==1) unknown_vars <- NULL
      if (length(unknown_vars)>1) unknown_vars <- unknown_vars[-a.poly]
    }
  }

  if (length(unknown_vars))
    stop(paste(paste(unknown_vars, collapse = ", "), "not in the model"))

  # convert any character variables to factor - just hope they are already
  # factors in the original data
  char_vars <- names(x[sapply(x, is.character)])
  x[char_vars] = lapply(char_vars, function(i) {
    factor(x[[i]], levels = levels(object$model[,i]))
  })

  # convert list of explanatory variables to a data.frame
  xprofiles <- expand.grid(x)

  missing_vars <- setdiff(formula_rhs_terms, names(x))
  available_vars <- intersect(missing_vars, colnames(object$model))

  xprofiles[,available_vars] <- lapply(object$model[,available_vars, drop = FALSE], central_tendency)

  # construct model matrix from model formula and data provided
  design_matrix <- stats::model.matrix(formula_rhs(object), xprofiles)

  # abort if anything is still NA
  if (any(is.na(xprofiles))) {
    stop("can't continue with NAs")
  }

  sims <- MASS::mvrnorm(num, stats::coefficients(object), stats::vcov(object))

  ev <- sims %*% t(design_matrix)

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

