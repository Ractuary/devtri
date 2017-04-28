#' idf
#'
#' Create an object of class \code{idf}
#'
#' @param ldfs the incemental loss development factor to develop lossed 1 period into the
#' future.
#' @param first_age the first development age.  This must be a number between 0 and 1.
#' @param tail function specifying the tail values.  Not yet implemented.
#'
#' @export
#'
#' @import tibble
#' @import dplyr
#'
#' @examples
#' idf(ldfs = c(1.75, 1.25, 1.15, 1.1, 1.05, 1.03), first_age = 1)
#' idf(ldfs = c(1.75, 1.25, 1.15, 1.1, 1.05, 1.03), first_age = 0.5)
idf <- function(ldfs, first_age = 1, tail = NA) {

  l <- length(ldfs)

  stopifnot(is.numeric(first_age) && length(first_age) == 1L)
  stopifnot(first_age > 0)
  stopifnot(is.numeric(ldfs) && l > 0)

  tib <- tibble(
    "age" = first_age:l,
    "ldf" = ldfs)

  tib <- tib %>%
    dplyr::mutate(earned_ratio = pmin(age / 1, 1))

  out <- structure(
    tib,
    earn_pattern = "linear",
    tail = tail,
    class = c("idf", class(tib))
  )

  if (!is.na(tail)) {
    out <- tail_linear(out)
  }

  out
}

#' cdf
#'
#' Create an object of class \code{cdf}
#'
#' @param ldfs the cumulative loss development factors.
#' @param first_age the first development age.  This must be a number between 0 and 1.
#' @param tail function specifying the tail values.  Not yet implemented.
#'
#' @export
#'
#' @import tibble
#' @import dplyr
#'
#' @examples
#' test <- cdf(ldfs = c(2.40, 1.5, 1.2, 1.15, 1.08, 1.03), first_age = 1)
#' cdf(ldfs = c(2.70, 1.5, 1.2, 1.15, 1.08, 1.03), first_age = 0.5)
cdf <- function(ldfs, first_age = 1, tail = NA) {

  l <- length(ldfs)

  stopifnot(is.numeric(first_age) && length(first_age) == 1L)
  stopifnot(first_age > 0)
  stopifnot(is.numeric(ldfs) && l > 0)

  tib <- tibble(
    "age" = first_age:l,
    "ldf" = ldfs
  )

  tib <- tib %>%
    dplyr::mutate(earned_ratio = pmin(age / 1, 1))

  out <- structure(
    tib,
    earn_pattern = "linear",
    tail_call = tail,
    tail = NA,
    class = c("cdf", class(tib))
  )

  if (!is.na(tail)) {
    out <- tail_linear(out)
  }

  out
}

#' idf2cdf
#'
#' convert \link{\code{idf}} object to \link{\code{cdf}} object
#'
#' @param idf object of class \code{idf}
#'
#' @import dplyr
#'
#' @export
#'
#' @examples
#'
#' my_idf <- idf(ldfs = c(1.75, 1.25, 1.15, 1.1, 1.05, 1.03), first_age = 1)
#'
#' idf2cdf(idf = my_idf)
idf2cdf <- function(idf) {

  stopifnot(inherits(idf, "idf"))

  ldfs_new <- idf

  ldfs_new$ldf <- ldfs_new$ldf %>%
    rev() %>%
    cumprod() %>%
    rev()

  # adjust any cdfs with age less 1 full development period
  ldfs_new <- ldfs_new %>% dplyr::mutate(ldf = ldf * earned_ratio)

  cdf(ldfs = ldfs_new$ldf,
      first_age = ldfs_new$age[1])
}
