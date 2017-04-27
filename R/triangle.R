#' tidy_tri
#'
#' \code{tidy_tri} class constructor.
#'
#' @param origin origin
#' @param age age
#' @param value value
#'
#' @import tibble
#'
#' @export
#'
#' @examples
#'
#' origin_data <- c(2015, 2016, 2017, 2015, 2016, 2015)
#' age_data <- c(1, 1, 1, 2, 2, 3)
#' value_data <- c(10, 11, 10, 15, 16, 17)
#'
#' my_tri <- triangle(origin = origin_data,
#'                    age = age_data,
#'                    value = value_data)
#'
tidy_tri <- function(origin, age, value) {

  tib <- tibble::tibble(
    "origin" = origin,
    "age" = age,
    "value" = value
  )

  structure(
    tib,
    class = c("tidy_tri", class(tib))
  )
}

#' @export
spread_tri <- function(tri) {
  tri %>%
    tidyr::spread(key = age, value = value)
}

#' tidy_ata
#'
#' creates age-to-age development triangles
#'
#' @param object object to turn into exhibit
#' @param ... additional arguments
#'
#' @import dplyr
#'
#' @export
#'
#' @examples
#'
#' origin_data <- c(2015, 2016, 2017, 2015, 2016, 2015)
#' age_data <- c(1, 1, 1, 2, 2, 3)
#' value_data <- c(10, 11, 10, 15, 16, 17)
#'
#' tidy_tri <- triangle(origin = origin_data,
#'                    age = age_data,
#'                    value = value_data)
#'
#' tidy_ata(my_tri)
#'
#' tri <- my_tri
#'
tidy_ata <- function(tri, ...) {
  stopifnot(inherits(tri, "tidy_tri"))

  out <- tri %>%
    dplyr::group_by(origin) %>%
    dplyr::mutate(
      value_lead = dplyr::lead(value, by = age),
      value = value_lead / value) %>%
    ungroup() %>%
    dplyr::select(origin, age, value)

  structure(
    out,
    class = c("ata", class(out))
  )
}


#' ldf_avg
#'
#' straight average of eacg age in a \code{tidy_tri}
#'
#' @param tri \code{tidy_tri} object
#'
#' @import dplyr
#'
#' @export
#'
#' @examples
#'
#'
ldf_avg <- function(tri) {
  out <- tri %>%
    dplyr::group_by(age) %>%
    dplyr::summarise(ldfs = mean(value, na.rm = TRUE))

  # assuming tail factor = to 1.0 for placeholder
  ldfs <- out$ldfs
  ldfs[is.na(ldfs)] <- 1.0

  idf(ldfs, first_age = min(tri$age))
}

#' latest
#'
#' get the latest value from a \code{tidy_tri}
#'
#' @param tri
#'
#' @import dplyr
#'
#' @export
#'
#' @examples
#'
latest <- function(tri) {
  tri %>%
    dplyr::mutate(cal = origin + age) %>%
    dplyr::filter(cal == max(cal)) %>%
    dplyr::select(-cal)
}

