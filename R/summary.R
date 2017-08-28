#' Summarizing Regression Simulations
#'
#' summary method for class "regsim"
#'
#' @param object an object of class "regsim", usually obtained by calling the
#' \code{regsim} function.
#' @param intercept include intercept in output
#' @param detail print detail format
#' @param rotate rotate covariates vertically
#' @param ... additional arguments passed to class-specific functions
#' @examples
#' library(regsim)
#'
#' model <- lm(mpg ~ wt + cyl, data = mtcars)
#' sim <- regsim(model, list(wt = seq(1, 5), cyl = mean(mtcars$cyl)))
#' summary(sim, detail = TRUE, rotate = TRUE)
#' @export
summary.regsim <- function(object, intercept = FALSE, detail = FALSE, rotate = FALSE, ...) {
  x <- as.data.frame(object$x)
  if (!intercept)
    x <- x[, names(x) != "(Intercept)"]

  qi <- calc_summary(object$ev)

  if (detail) {
    for (i in 1:nrow(x)) {
      profile <- x[i,]
      if (rotate)
        r.profile <- t(profile)
        colnames(r.profile) <- paste("Profile",i)
        print(r.profile)
      else
        rownames(profile) <- paste("Profile",i)
        print(profile)

      cat("\n")
      print(qi[i,])
      cat(paste0(paste(rep("-", 48), collapse = ""), "\n\n"))
    }

    if (nrow(x) == 2) {
      fd <- object$ev[,2] - object$ev[,1]
      cat("First Differences:\n\n")
      print(calc_summary(as.data.frame(fd)))
      cat("\n")
    }

  } else {
    # return as data.frame, don't use print in this mode
    return(cbind(x, qi))
  }
}

calc_summary <- function(obj) {
  as.data.frame(t(apply(obj, 2, function(x) {
    c(mean = mean(x),
      sd = stats::sd(x),
      stats::quantile(x, probs = c(.025, .5, .975))
    )
  })))
}

