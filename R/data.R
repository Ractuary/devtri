#' Data for making example triangles
#'
#' This data is a subset of the the \code{losses} data frame from the
#' \code{casdata} package available on Github \code{https://github.com/Ractuary/casdata}
#'
#' @format A data frame with 21 rows and 7 variables:
#' \describe{
#'   \item{lob}{Line of Business}
#'   \item{gr_code}{Unique company code}
#'   \item{gr_name}{Company name}
#'   \item{origin}{the origin period of the losses}
#'   \item{dev}{the development age of the losses}
#'   \item{incurred}{the cumulative incurred losses}
#'   \item{paid}{the cumulative paid losses}
#' }
#'
#' @examples
#' # code used to create this dataset
#' if (interactive()) {
#'   library(casdata)
#'   library(dplyr)
#'   library(devtools)
#'
#'   tri_data <- losses %>% filter(lob == "wkcomp", gr_code == 671, origin >= 1992) %>%
#'                 mutate(cal = origin + dev - 1) %>%
#'                 filter(cal <= 1997) %>%
#'                 select(-bulk, -cal)
#'
#'    use_data(tri_data, overwrite = TRUE)
#' }
#'
#'
"tri_data"

#' Premium data to go alonf with \code{tri_data}
#'
#' This data is a subset of the the \code{premium} data frame from the
#' \code{casdata} package available on Github \code{https://github.com/Ractuary/casdata}
#'
#' @format A data frame with 6 rows and 4 variables:
#' \describe{
#'   \item{lob}{Line of Business}
#'   \item{gr_code}{Unique company code}
#'   \item{gr_name}{Company name}
#'   \item{premium_net}{Premium net of ceded reinsurance}
#' }
#'
#' @examples
#' # code used to create this dataset
#' if (interactive()) {
#'   library(casdata)
#'   library(dplyr)
#'   library(devtools)
#'
#'   premium_data <- premium %>% filter(lob == "wkcomp", gr_code == 671, origin >= 1992) %>%
#'                                 select(lob, gr_code, gr_name, premium_net)
#'
#'   use_data(premium_data)
#' }
#'
#'
"premium_data"
