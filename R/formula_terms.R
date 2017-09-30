#----------------------------------------------------------------------
formula_terms <- function (object) {
  UseMethod("formula_terms", object)
}

#----------------------------------------------------------------------
# return formula terms from a model object, stripping any functions
formula_terms.formula <- function(formula) {
  term_labels <- labels(stats::terms(formula))

  # this captures any transformation functions (log, lag, as.factor, poly, etc)
  # test regex at https://regex101.com/r/tknU6o/4

  # define our own classes to simplify the regular expresions for parsing
  regex_classes <- c(
    id     = '[[:alpha:]][[:alnum:]_\\\\.]*', # an identifier
    op     = '[+\\\\-\\\\/\\\\*\\\\:^%]',     # an operator
    num    = '[[:digit:]\\\\.]*',             # a number
    punc   = ',',                             # punctuations
    lparen = '\\\\(',                         # left paren
    rparen = '\\\\)'                          # right paren
  )

  regex_classes <- sapply(regex_classes, paste0, "[[:space:]]*")
  regex_classes <- stats::setNames(regex_classes,
                                   paste0("<", names(regex_classes), ">"))

  regex_expr <- "^(<id>)(?:(?:<op><id>)*|<lparen>(<id>|<num>)(?:(?:<punc>|<op>)(?:<id>|<num>))*<rparen>){0,1}$"
  regex_expr <- stringr::str_replace_all(regex_expr, regex_classes)

  # for debugging print out the regex and test on regex101
  # cat(paste0("\n", regex_expr, "\n\n"))

  regex_matches <- regmatches(term_labels, regexec(regex_expr, term_labels))

  regex_matches <- lapply(regex_matches, trimws)                    # trim whitespace
  regex_matches <- lapply(regex_matches, setdiff, '')               # remove empty strings
  regex_matches <- regex_matches[lapply(regex_matches, length) > 0] # remove 0-length vectors
  return(sapply(regex_matches, utils::tail, n=1))  # we only care about the last captured group
}

