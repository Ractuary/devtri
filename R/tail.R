

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
#' ldfs <- idf(ldfs = c(1.75, 1.25, 1.15, 1.1, 1.04, 1.03), first_age = 1)
#'
#' test <- tail_linear(ldfs)
#'
tail_linear <- function(ldfs, n_points = 2, cutoff = 25) {

  l <- nrow(ldfs)

  # extract only the ldfs that will be used to fit the tail
  trimmed <- ldfs[(l - n_points + 1):l, ]

  # fit the tail and apply fit to calculate tail
  fit <- lm(trimmed$ldf ~ trimmed$age)
  co <- coef(fit)

  first_tail_age <- ldfs$age[l] + 1

  ages <- first_tail_age:cutoff
  tail_ldfs <- co[1] + co[2] * ages
  tail_ldfs <- pmax(tail_ldfs, 1.0)

  # create new idf which will be the tail
  # to the original idf passed to the `ldfs` argument
  out <- idf(
    ldfs = tail_ldfs,
    first_age = first_tail_age
  )

  attr(ldfs, "tail") <- out

  ldfs
}
