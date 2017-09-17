#----------------------------------------------------------------------
formula_terms <- function (object) {
  UseMethod("formula_terms", object)
}

#----------------------------------------------------------------------
formula_terms.formula <- function(formula) {
  return(formula_terms_common(formula))
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
  return(sapply(regex_matches, utils::tail, n=1))
}

