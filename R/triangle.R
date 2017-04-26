#' triangle
#'
#' \code{triangle} class constructor
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
triangle <- function(origin, age, value) {

  tib <- tibble::tibble(
    "origin" = origin,
    "age" = age,
    "value" = value
  )

  structure(
    tib,
    class = c("triangle", class(tib))
  )
}

#' @export
print.triangle <- function(tri) {
  tri %>%
    tidyr::spread(key = age, value = value) %>%
    print()
}

#' ata
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
#' my_tri <- triangle(origin = origin_data,
#'                    age = age_data,
#'                    value = value_data)
#'
#' ata(my_tri)
#'
ata <- function(tri, ...) {
  stopifnot(inherits(tri, "triangle"))

  out <- tri %>%
    dplyr::mutate(
      value_lead = dplyr::lead(value, by = age),
      value = value_lead / value) %>%
    dplyr::select(origin, age, value)

  structure(
    out,
    class = c("ata", class(out))
  )
}

#' @export
print.ata <- function(ata) {
  ata %>% print.triangle()
}
