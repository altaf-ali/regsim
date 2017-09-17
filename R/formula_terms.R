#----------------------------------------------------------------------
formula.regsim <- function(object) {
  formula(object$model)
}

#----------------------------------------------------------------------
formula_terms <- function (object) {
  UseMethod("formula_terms", object)
}

#----------------------------------------------------------------------
# return formula terms from a model object, stripping any functions
formula_terms_common <- function(formula) {
  term_labels <- labels(stats::terms(formula))

  # this captures any transformation functions (log, lag, as.factor, poly, etc)
  regex_expr <- "^([a-zA-Z][a-zA-Z0-9_\\.]*)(?:\\s*\\(\\s*([a-zA-Z0-9_\\.]*\\s*)(?:,\\s*[a-zA-Z0-9_\\.]*\\s*)*\\)){0,1}\\s*$"
  regex_matches <- regmatches(term_labels, regexec(regex_expr, term_labels))

  regex_matches <- lapply(regex_matches, trimws)                    # trim whitespace
  regex_matches <- lapply(regex_matches, setdiff, '')               # remove empty strings
  regex_matches <- regex_matches[lapply(regex_matches, length) > 0] # remove 0-length vectors
  return(sapply(regex_matches, tail, n=1))
}

#----------------------------------------------------------------------
formula_terms.formula <- function(formula) {
  formula_terms_common(formula)
}

#----------------------------------------------------------------------
formula_terms.lm <- function(object) {
  formula_terms_common(formula(object))
}

#----------------------------------------------------------------------
formula_terms.glm <- function(object) {
  formula_terms_common(formula(object))
}
