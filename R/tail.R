#' tail_selected
#'
#' manually select your own idf values to use as a tail
#'
#' @param idf_ object of class \code{idf_}
#' @param idfs incremental development factors to make up tail
#'
#' @import dplyr
#'
#' @export
#'
#' @examples
#'
#' my_idf <- idf(idfs = c(1.75, 1.25, 1.15, 1.1, 1.04, 1.03), first_age = 1)
#'
#' test <- tail_selected(my_idf, c(1.02, 1.01, 1.01, 1.0))
#' test
#'
#' # replace existing tail
#' tail_selected(test, c(1.01, 1.0))
#'
tail_selected <- function(idf_, idfs) {

  stopifnot(inherits(idf_, "idf_"))

  # remove previous tail if it exists
  if (!is.na(attr(idf_, "tail_first_age"))) {
    idf_ <- idf_ %>% dplyr::filter(age < attr(idf_, "tail_first_age"))
  }

  l <- nrow(idf_)

  # create new idf with a tail
  out <- idf(
    idfs = c(idf_$idfs, idfs),
    first_age = min(idf_$age)
  )

  attr(out, "tail_first_age") <- idf_$age[l] + 1

  out
}



#' tail_linear
#'
#' fit a linear tail factor to ldfs
#'
#' @param idf_ an object of class \code{idf_}
#' @param n_points number of idf points to fit.  The most mature points will be
#' used in the fit
#' @param cutoff last age that should have a none 1.0 development factor.  All
#' loss development factors with an age greater than \code{cutoff} will be set to
#' 1.0.
#'
#' @import dplyr
#'
#' @export
#'
#' @examples
#'
#' my_idf <- idf(idfs = c(1.75, 1.25, 1.15, 1.1, 1.04, 1.03), first_age = 1)
#'
#' test <- tail_linear(my_idf, cutoff = 10)
#'
tail_linear <- function(idf_, n_points = 2, cutoff = 25) {

  if (!is.na(attr(idf_, "tail_first_age"))) {
    idf_ <- idf_ %>% dplyr::filter(age < attr(idf_, "tail_first_age"))
  }

  l <- nrow(idf_)
  # extract only the ldfs that will be used to fit the tail
  trimmed <- idf_[(l - n_points + 1):l, ]

  # fit the tail and apply fit to calculate tail
  fit <- lm(trimmed$idfs ~ trimmed$age)
  co <- coef(fit)

  first_tail_age <- idf_$age[l] + 1

  ages <- first_tail_age:cutoff
  tail_ldfs <- co[1] + co[2] * ages
  tail_ldfs <- pmax(tail_ldfs, 1.0)

  # create new idf with a tail
  out <- idf(
    idfs = c(idf_$idfs, tail_ldfs),
    first_age = min(idf_$age)
  )

  attr(out, "tail_first_age") <- first_tail_age

  out
}
