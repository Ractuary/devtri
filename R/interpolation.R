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
#' interpolate_linear(my_cdf, dev = 0.25)
interpolate_linear <- function(ldfs, dev= 0.5) {

  stopifnot(dev > 0 && dev < 1)

  # cannot interpolate 1 ldf
  stopifnot(nrow(ldfs) > 1)

  # TODO: remove tail here


  hold <- ldfs %>%
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

  # TODO: need to update this to handle the tail properly
  if (is.na(attr(ldfs, "tail_first_age"))) {
    hold$ldf[hold$age == max(hold$age)] <- 1.0
    #warning("No tail: setting final development factor to 1.0")
  }

  # create new cdf object with new tail
  cdf(
    ldfs = hold$ldf,
    first_age = min(hold$age, na.rm = TRUE)
  ) #%>% TDOD: recalculate tail here


}
