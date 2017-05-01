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
#'
ldf_picker <- function(.dev_tri) {
  tri_show <- spread_ata(ata_tri(.dev_tri)) %>%
                filter(origin != max(origin))

  ui <- miniPage(
    gadgetTitleBar("Development Triangle"),
    miniContentPanel(
      DT::datatable(
        tri_show,
        rownames = FALSE,
        options = list(
          dom = "t",
          columnDefs = list(
            list(targets = 0:(length(tri_show) - 1), orderable = FALSE)
          )
        )
      ) %>%
        formatRound(
          columns = 2:length(tri_show),
          digits = 3
        )
    )
  )

  server <- function(input, output, session) {

  }

  runGadget(ui, server, viewer = dialogViewer("LDF Picker", width = 1200, height = 800))
}
