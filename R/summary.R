#' Summarizing Regression Simulations
#'
#' summary method for class "regsim"
#'
#' @param object an object of class \code{regsim}, usually obtained by calling the
#' \link{regsim} function.
#' @param detail print detail format
#' @param long print summary in long format (default is TRUE in detail mode)
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
#' summary(sim)
#' @export
summary.regsim <- function(object, detail = TRUE, long = detail, ...) {
  x <- object$x

  qi <- calc_summary(object$ev)

  if (!detail)
    return(cbind(x, qi)) # return as data.frame, don't use print in this mode

  profile_id <- paste("Profile", rownames(x))
  profile_id_short <- paste0("p", rownames(x))

  rownames(x) <- ifelse(rep(long, nrow(x)), profile_id, profile_id_short)
  rownames(qi) <- profile_id_short

  for (i in 1:nrow(x)) {
    profile <- x[i,, drop = FALSE]
    if (long)
      print(t(profile), quote = FALSE)
    else
      print(profile, quote = FALSE)

    qi_str <- utils::capture.output(print(qi[i,]))
    len <- nchar(qi_str[2])
    cat(paste0("\n", paste(qi_str, collapse = "\n")))
    cat(paste0("\n", paste(rep("-", len), collapse = ""), "\n\n"))
  }

  if (nrow(x) == 2) {
    fd <- object$ev[,2] - object$ev[,1]
    cat("First Differences:\n\n")
    print(calc_summary(as.data.frame(fd)))
    cat("\n")
  }
}

calc_summary <- function(obj) {
  as.data.frame(t(apply(obj, 2, stats::quantile, probs = c(.025, .5, .975))))
}

