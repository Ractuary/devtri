#' tail
#'
#' class for ldf tail factor
#'
#' @param type type of tail factor
#' @param args list of arguments to pass to fit that type of tail
#'
#'
#'
tail <- function(type, args) {
#  structure(
#
#  )
}

#' tail_linear
#'
#' fit a linear tail factor to ldfs
#'
#' @param ldf an object of class \code{idf} or \code{cdf}
#' @param n_points number of ldf points to fit.  T
#' he most mature points will be used in the fit
#' @param cutoff last age that should have a none 1.0 development factor.  All
#' loss development factors with an age greater than \code{cutoff} will be set to
#' 1.0
#'
#' @import dplyr
#'
#' @export
#'
#' @examples
#'
#' ldf <- idf(ldfs = c(1.75, 1.25, 1.15, 1.1, 1.04, 1.03), first_age = 1)
#'
tail_linear <- function(ldf, n_points = 2, cutoff = 25) {

  l <- nrow(ldf)

  trimmed <- ldf[(l - n_points + 1):l, ]

  fit <- lm(trimmed$ldf ~ trimmed$age)
  co <- coef(fit)

  first_tail_age <- ldf$age[l]

  ages <- (first_tail_age + 1):cutoff
  tail_ldfs <- co[1] + co[2] * ages
  tail_ldfs <- pmax(tail_ldfs, 1.0)

  out <- idf(
    ldfs = tail_ldfs,
    first_age = first_tail_age
  )

  attr(ldf, "tail") <- out

  ldf
}
