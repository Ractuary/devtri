#' idf_picker
#'
#' Rstudio addin that allows you to select ldfs for a triangle
#'
#' @param .dev_tri
#'
#'
#' @export
#'
#' @import shiny
#' @import miniUI
#' @import DT
#' @import tidyr
#' @import dplyr
#'
idf_picker <- function(.dev_tri) {

  idf_names <- tibble("Type" = c("Straight AVG", "Weighted AVG"))
  s_avg <- ldf_avg(.dev_tri) %>%
             dplyr::select(-earned_ratio) %>%
             tidyr::spread(key = age, value = idfs)
  w_avg <- ldf_avg_wtd(.dev_tri) %>%
             dplyr::select(-earned_ratio) %>%
             tidyr::spread(key = age, value = idfs)

  idfs <- rbind(s_avg, w_avg)
  idfs <- cbind(idf_names, idfs)

  tail_vals <- tibble("tails" = rep(NA_real_, times = nrow(idfs)))
  idfs <- cbind(idfs, tail_vals)

  tri_show <- spread_ata(ata_tri(.dev_tri)) %>%
                filter(origin != max(origin))

  names(idfs) <- names(tri_show)

  selected <- as.list(c("Selected", rep(NA, times = length(w_avg) + 1)))
  names(selected) <- names(idfs)
  selected <- as.tibble(selected)

  selected[[2]] <- as.character(numericInput(
    inputId = "sel_idf_1",
    label = NULL,
    value = 1.0,
    min = 0
  ))
  names(selected) <- names(w_avg)

  ui <- miniPage(
    gadgetTitleBar("Development Triangle"),
    miniTabstripPanel(
      miniTabPanel("Triangle", icon = icon("table"),
        miniContentPanel(
          fillCol(
            flex = c(2, 1, 1),
            column(
              width = 12,

              DT::datatable(
                tri_show,
                rownames = FALSE,
                options = list(
                  dom = "t",
                  columnDefs = list(
                    list(targets = 0:(length(tri_show) - 1), orderable = FALSE),
                    list(targets = 0, class = "dt-center"),
                    list(targets = 0, width = "100px")
                  )
                )
              ) %>%
                formatRound(
                  columns = 2:length(tri_show),
                  digits = 3
                )
            ),
            column(
              width = 12,
              DT::datatable(
                idfs,
                rownames = FALSE,
                options = list(
                  dom = "t",
                  columnDefs = list(
                    list(targets = 0:(length(tri_show) - 1), orderable = FALSE),
                    list(targets = 0, width = "100px")
                  )
                )
              ) %>%
                formatRound(
                  columns = 2:length(tri_show),
                  digits = 3
                )
            ),
            column(
              width = 12,
              DT::datatable(
                selected,
                rownames = FALSE,
                options = list(
                  dom = "t",
                  columnDefs = list(
                    list(targets = 0:(length(tri_show) - 1), orderable = FALSE),
                    list(targets = 0, width = "100px")
                  )
                ),
                escape = FALSE,
                selection = "none"
              ) %>%
                formatRound(
                  columns = 2:length(tri_show),
                  digits = 3
                )
            )
          )
        )
      )
    )
  )

  server <- function(input, output, session) {

  }

  runGadget(ui, server, viewer = browserViewer())
              #dialogViewer("LDF Picker", width = 1200, height = 800))
}
