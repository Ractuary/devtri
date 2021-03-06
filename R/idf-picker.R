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
#' @import plotly
#' @import tibble
#'
idf_picker <- function(.dev_tri) {

  idf_names <- tibble::tibble("Type" = c("Straight AVG", "Weighted AVG"))
  s_avg <- ldf_avg(.dev_tri) %>%
             dplyr::select(-earned_ratio) %>%
             tidyr::spread(key = age, value = idfs)
  sel_idf <- ldf_avg_wtd(.dev_tri)
  n_idfs <- nrow(sel_idf)
  w_avg <- sel_idf %>%
             dplyr::select(-earned_ratio) %>%
             tidyr::spread(key = age, value = idfs)

  sel_idf <- sel_idf %>%
    tail_selected(1.0)

  idfs <- rbind(s_avg, w_avg)
  idfs <- cbind(idf_names, idfs)

  tail_vals <- tibble("tails" = rep(NA_real_, times = nrow(idfs)))
  idfs <- cbind(idfs, tail_vals)

  tri_show <- spread_ata(ata_tri(.dev_tri)) %>%
                filter(origin != max(origin))

  names(idfs) <- names(tri_show)
  names(idfs)[1] <- "Estimates"

  selected <- as.list(c("Selected IDF", rep(NA, times = length(w_avg) + 1)))
  names(selected) <- names(idfs)
  selected <- tibble::as.tibble(selected)
  selected[[length(selected)]] <- "Tail Selection"

  for (i in 2:(n_idfs + 1)) {
    selected[[i]] <- as.character(
      shiny::numericInput(
        inputId = paste0("sel_idf_", i - 1),
        label = NULL,
        value = round(w_avg[[i - 1]], 3),
        min = 0,
        width = "90px",
        step = 0.001
      )
    )
  }

  ui <- miniPage(
    tags$head(
      tags$style(
        ".form-group {
          margin: 0 !important;
          float: right;
        }"
      )
    ),
    gadgetTitleBar("Development Triangle"),
    miniTabstripPanel(
      miniTabPanel("Triangle", icon = icon("table"),
        miniContentPanel(
          fluidRow(
            column(
              width = 12,
              DT::DTOutput("tri")
            ),
            column(
              width = 12,
              DT::DTOutput("idf_est")
            ),
            column(
              width = 12,
              DT::DTOutput("sel_tbl")
            ),
            column(
              width = 12,
              DT::DTOutput("ind_cdf"),
              verbatimTextOutput("idf_1")
            )
          )
        )
      ),
      miniTabPanel("Tail", icon = icon("piggy-bank", lib="glyphicon"),
        miniContentPanel(
          fluidRow(
            column(
              width = 12,
              plotly::plotlyOutput("idf_plot")
            )
          ),
          fluidRow(
            column(
              width = 4,
              shiny::selectInput(
                "tail_fit",
                "Tail Fit",
                choices = c(
                  "Manual Selections" = "manual",
                  "Linear" = "linear"
                )
              )
            ),
            column(
              width = 4,
              uiOutput("cutoff_input")
            ),
            column(
              width = 4,
              conditionalPanel(
                condition = "input.tail_fit === 'manual'",
                uiOutput("manual_idfs")
              ),
              conditionalPanel(
                condition = "input.tail_fit === 'linear'",
                uiOutput("n_points_input")
              )
            )
          )
        )
      )#,
      # miniTabPanel("Code", icon = icon("code"),
      #   miniContentPanel(
      #     fluidRow(
      #       column(
      #         width = 12,
      #         h3("This is the code that will be returned by the gadget when you hit the 'done' button in the top right corner"),
      #         div(
      #           style = "border-style: solid; border-width: 2px;"#,
      #           #textOutput("code_text")
      #         )
      #       )
      #     )
      #   )
      # )
    )
  )

  server <- function(input, output, session) {
    sel <- reactiveValues(
      idf = sel_idf,
      cdf = idf2cdf(sel_idf)
    )

    # code_out <- reactive({
    #   idf_call <- deparse(attr(sel$idf, "idf_call"))
    #   tail_call <- deparse(attr(sel$idf, "tail_call"))
    #   paste0("# idf_picker() # ", idf_call, " %>% ", tail_call, "\n")
    # })

    # output$code_text <- renderText({
    #   code_out()
    # })

    output$cutoff_input <- renderUI({
      selectInput(
        "cutoff",
        "Cutoff Age",
        choices = attr(isolate({sel$idf}), "tail_first_age"):100
      )
    })

    output$n_points_input <- renderUI({
      selectInput(
        "n_points",
        "Number of Points in Fit",
        choices = 2:(attr(isolate({sel$idf}), "tail_first_age") - 1),
        selected = "2"
      )
    })

    output$manual_idfs <-renderUI({
      out <- lapply(attr(isolate({sel$idf}), "tail_first_age"):as.numeric(input$cutoff), function(i) {
        fluidRow(
          hr(style = "margin: 0;"),
          column(
            width = 6,
            h3(
              style = "text-align: center;",
              paste0(i, " to ", i + 1)
            )
          ),
          column(
            width = 6,
            div(
              style = "padding-top: 15px;",
              numericInput(
                inputId = paste0("tail_ldf_", i),
                label = NULL,
                value = 1.0,
                min = 0
              )
            )
          )
        )
      })
      div(
        h2(
          style = "text-align: center",
          "Tail IDF Selections"
        ),
        out
      )
    })

    # Manually selected tail IDFs
    observe({
      tail_age <- attr(sel$idf, "tail_first_age")
      req(input[[paste0("tail_ldf_", tail_age)]])
      hold_tail_selections <- vector(mode = "numeric", length = as.numeric(input$cutoff) - tail_age + 1)

      for (j in tail_age:as.numeric(input$cutoff)) {
        req(input[[paste0("tail_ldf_", j)]])
        hold_tail_selections[j - tail_age + 1] <- input[[paste0("tail_ldf_", j)]]
      }

      sel$idf <- sel$idf %>% tail_selected(hold_tail_selections)
      sel$cdf <- idf2cdf(sel$idf)
    })

    # Linear IDF extrapolation
    observeEvent({
      input$n_points
      input$cutoff
    }, {
      req(input$n_points, input$cutoff)
      sel$idf <- sel$idf %>% tail_linear(n_points = as.numeric(input$n_points),
                                         cutoff = as.numeric(input$cutoff))
      sel$cdf <- idf2cdf(sel$idf)
    })

    observeEvent(input$done, {
      #rstudioapi::insertText(code_out())
      stopApp(sel$idf)
    })


    output$idf_1 <- renderPrint({
      input$sel_idf_1
      #sel$cdf
    })

    sapply(seq_len(n_idfs), function(j) {
      observeEvent(input[[paste0("sel_idf_", j)]], {
        req(input[[paste0("sel_idf_", j)]])
        sel$idf$idfs[j] <- input[[paste0("sel_idf_", j)]]
        sel$cdf <- idf2cdf(sel$idf)
      }, ignoreInit = TRUE)
    })

    cdf_out <- reactive({
      out <- sel$cdf %>%
        dplyr::filter(age <= attr(sel$cdf, "tail_first_age")) %>%
        dplyr::select(-earned_ratio) %>%
        tidyr::spread(key = age, value = cdfs)
      cdf_name_col <- tibble::tibble("origin" = "Indicated CDF")
      out <- cbind(cdf_name_col, out)
      out
    })

    output$tri <- DT::renderDT({
      DT::datatable(
        tri_show,
        rownames = FALSE,
        options = list(
          dom = "t",
          ordering = FALSE,
          columnDefs = list(
            list(targets = 0, class = "dt-center"),
            list(targets = 0, width = "100px")
          )
        )
      ) %>%
        DT::formatRound(
          columns = 2:length(tri_show),
          digits = 3
        )
    })

    output$idf_est <- DT::renderDT({
      DT::datatable(
        idfs,
        rownames = FALSE,
        options = list(
          dom = "t",
          ordering = FALSE,
          columnDefs = list(
            list(targets = 0, width = "100px")
          )
        )
      ) %>%
        DT::formatRound(
          columns = 2:length(tri_show),
          digits = 3
        )
    })

    output$sel_tbl <- DT::renderDT({
      DT::datatable(
        selected,
        rownames = FALSE,
        colnames = rep("", times = length(selected)),
        options = list(
          dom = "t",
          ordering = FALSE,
          columnDefs = list(
            list(targets = 0, width = "100px"),
            list(targets = 1:(length(selected) - 1), class = "dt-right")
          ),
          preDrawCallback = DT::JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
          drawCallback = DT::JS('function() { Shiny.bindAll(this.api().table().node()); }')
        ),
        escape = FALSE,
        selection = "none"
      ) %>%
        DT::formatRound(
          columns = 2:length(tri_show),
          digits = 3
        )
    })

    output$ind_cdf <- DT::renderDT({
      DT::datatable(
        cdf_out(),
        rownames = FALSE,
        colnames = rep("", times = length(selected)),
        options = list(
          dom = "t",
          ordering = FALSE,
          columnDefs = list(
            list(targets = 0, width = "100px")
          )
        ),
        selection = "none"
      ) %>%
        DT::formatRound(
          columns = 2:length(tri_show),
          digits = 3
        )
    })

    output$idf_plot <- plotly::renderPlotly({
      plot(sel$idf)
    })
  }

  runGadget(ui, server, viewer = #browserViewer())
              dialogViewer("LDF Picker", width = 1200, height = 800))
}
