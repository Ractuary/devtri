#' dev_tri
#'
#' \code{dev_tri} class constructor.
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
#' my_tri <- dev_tri(origin = origin_data,
#'               age = age_data,
#'               value = value_data)
#'
dev_tri <- function(origin, age, value) {

  tib <- tibble::tibble(
    "origin" = origin,
    "age" = age,
    "value" = value
  )

  structure(
    tib,
    class = c("dev_tri", class(tib))
  )
}

#' @export
spread_tri <- function(tri) {
  tri %>%
    tidyr::spread(key = age, value = value)
}

#' ata_tri
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
#' my_tri <- dev_tri(origin = origin_data,
#'                   age = age_data,
#'                   value = value_data)
#'
#' ata_tri(my_tri)
#'
ata_tri <- function(tri, ...) {
  stopifnot(inherits(tri, "dev_tri"))

  out <- tri %>%
    dplyr::group_by(origin) %>%
    dplyr::mutate(
      value_lead = dplyr::lead(value, by = age),
      value = value_lead / value) %>%
    ungroup() %>%
    dplyr::select(origin, age, value)

  # if ata value is Inf (i.e value at age is 0 and value_lead > 0)
  # then set ata value to NA
  out <- out %>% mutate(value = ifelse(value == Inf, NA, value))

  structure(
    out,
    class = c("ata", class(out))
  )
}


#' ldf_avg
#'
#' straight average of eacg age in a \code{dev_tri}
#'
#' @param tri \code{dev_tri} object
#'
#' @import dplyr
#'
#' @export
#'
#' @examples
#'
#'
ldf_avg <- function(tri, tail = 1.0) {
  ata <- ata_tri(tri)

  out <- ata %>%
    dplyr::group_by(age) %>%
    dplyr::summarise(ldfs = mean(value, na.rm = TRUE))

  # assuming tail factor = to 1.0 for placeholder
  ldfs <- out$ldfs
  ldfs[is.na(ldfs)] <- tail

  idf(ldfs, first_age = min(tri$age))
}

#' ldf_avg_wtd
#'
#' dollar weighted average of eacg age in a \code{tri}
#'
#' @param tri \code{tri} object
#'
#' @import dplyr
#'
#' @export
#'
#' @examples
#'
#'
ldf_avg_wtd <- function(tri) {

  out <- tri %>%
    dplyr::mutate(value_lead = dplyr::lead(value, by = age)) %>%
    dplyr::filter(!is.na(value), !is.na(value_lead)) %>%
    dplyr::group_by(age) %>%
    dplyr::summarise(value_total = sum(value),
                     value_lead_total = sum(value_lead)) %>%
    dplyr::mutate(ldfs = value_lead_total / value_total)

  # assuming tail factor = to 1.0 for placeholder
  ldfs <- c(out$ldfs, 1.0)

  idf(ldfs, first_age = min(tri$age))
}

#' latest
#'
#' get the latest value from a \code{tri}
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

