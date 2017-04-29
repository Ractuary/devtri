#' tail_selected
#'
#' manually select your own \code{idf} values to use as a tail
#'
#' @param idf object of class \code{idf}
#' @param ldfs idf tail values
#'
#' @import dplyr
#'
#' @export
#'
#' @examples
#'
#' my_idf <- idf(ldfs = c(1.75, 1.25, 1.15, 1.1, 1.04, 1.03), first_age = 1)
#'
#' test <- tail_selected(my_idf, c(1.02, 1.01, 1.01, 1.0))
#'
#' # replace existing tail
#' test2 <- tail_selected(test, c(1.01, 1.0))
#'
tail_selected <- function(.idf, ldfs) {

  if (!is.na(attr(.idf, "tail_first_age"))) {
    .idf <- .idf %>% dplyr::filter(age < attr(.idf, "tail_first_age"))
  }

  l <- nrow(.idf)

  first_tail_age <- .idf$age[l] + 1

  # create new idf with a tail
  out <- idf(
    ldfs = c(.idf$ldf, ldfs),
    first_age = min(.idf$age)
  )

  attr(out, "tail_call") <- match.call()
  attr(out, "tail_first_age") <- first_tail_age

  out
}



#' tail_linear
#'
#' fit a linear tail factor to ldfs
#'
#' @param ldfs an object of class \code{idf} or \code{cdf}
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
#' test <- tail_linear(ldfs, cutoff = 10)
#'
tail_linear <- function(.idf, n_points = 2, cutoff = 25) {

  if (!is.na(attr(.idf, "tail_first_age"))) {
    .idf <- .idf %>% dplyr::filter(age < attr(.idf, "tail_first_age"))
  }

  l <- nrow(.idf)
  # extract only the ldfs that will be used to fit the tail
  trimmed <- .idf[(l - n_points + 1):l, ]

  # fit the tail and apply fit to calculate tail
  fit <- lm(trimmed$ldf ~ trimmed$age)
  co <- coef(fit)

  first_tail_age <- .idf$age[l] + 1

  ages <- first_tail_age:cutoff
  tail_ldfs <- co[1] + co[2] * ages
  tail_ldfs <- pmax(tail_ldfs, 1.0)

  # create new idf with a tail
  out <- idf(
    ldfs = c(.idf$ldf, tail_ldfs),
    first_age = min(.idf$age)
  )

  attr(out, "tail_call") <- match.call()
  attr(out, "tail_first_age") <- first_tail_age

  out
}
