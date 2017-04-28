#' interpolate_linear
#'
#' interpolate a \code{cdf} object to future ages
#'
#' @param cdf cdf object
#' @param dev development time to interpolate for
#'
#' @import dplyr
#'
#' @export
#'
#' @examples
#'
#' my_cdf <- cdf(ldfs = c(2.70, 1.5, 1.2, 1.15, 1.08, 1.03), first_age = 0.5)
#'
#' interpolate_linear(my_cdf)
interpolate_linear <- function(cdf, dev= 0.5) {

  stopifnot(dev > 0 && dev < 1)

  # cannot interpolate 1 ldf
  stopifnot(nrow(cdf) > 1)

  # TODO: remove tail here


  hold <- cdf %>%
    mutate(
      # adjust ldfs to account for ldfs that have not reached the first
      # development age.  These ldfs need to be increased to represent an ldf
      # for the full development period
      ldf = ldf * (1 / earned_ratio),
      # percentage reported
      pct = 1 / ldf,
      # percentage reported at future period
      pct_lead = lead(pct, order_by = age),
      # linearly interpolate between percentages
      interp_ldfs = 1 / (pct * dev + (1.0 - dev) * pct_lead),
      # adjust age
      age = age + dev,
      # adjust earned ratio
      earned_ratio = pmin(earned_ratio + dev, 1),
      # rebase cdf based on new earned ratio
      ldf = interp_ldfs * earned_ratio) %>%
    #filter(!is.na(ldf)) %>%
    select(age, ldf, earned_ratio)


  # TODO: reapply new tail here if possible

  hold
}
