library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(plotly)

# =============================================================================
# QC Module  –  Step 2
# UI:     QCUI(id)
# Server: QCServer(id, tables)
# =============================================================================

#' QCUI
#'
#' Executes QCUI.
#'
#' @param id Argument for QCUI.
#'
#' @return Return value produced by QCUI.
QCUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("qcPanelWrapper"))
  )
}

#' QCServer
#'
#' Executes QCServer.
#'
#' @param id Argument for QCServer.
#' @param tables Argument for QCServer.
#'
#' @return Return value produced by QCServer.
QCServer <- function(id, tables) {
  moduleServer(id, function(input, output, session) {
    # ---- Conditional wrapper: hide all QC panels when ERCC mode is on ----
    output$qcPanelWrapper <- renderUI({
      if (!isTRUE(tables$inputValidated)) {
        return(NULL)
      }
      if (isTRUE(tables$erccDetected) && identical(tables$erccDecision, "pending")) {
        return(NULL)
      }
      if (isTRUE(tables$erccMode)) {
        return(NULL)
      }
      ns <- session$ns
      tagList(
        fluidRow(
          box(width = 6, shinycssloaders::withSpinner(uiOutput(ns("libSizeBar")))),
          box(width = 6, shinycssloaders::withSpinner(uiOutput(ns("RLEplot"))))
        ),
        fluidRow(
          box(
            width = 12,
            title = "Principal Component Analysis (PCA) — All Samples (TMM log2CPM)",
            solidHeader = TRUE, closable = TRUE, collapsible = TRUE, status = "primary",
            div(shinycssloaders::withSpinner(
              plotlyOutput(ns("qcPCAplot"), height = "500px", width = "500px")
            ), align = "center")
          )
        )
      )
    })

    # ---- Library size plot -----------------------------------------------
    output$libSizeBar <- renderUI({
      fluidRow(
        box(
          width = 12, title = "Sample library size",
          solidHeader = TRUE, closable = TRUE, collapsible = TRUE, status = "primary",
          div(plotlyOutput(session$ns("libSizePlot1"), height = "100%"), align = "center")
        )
      )
    })

    observe({
      if (!is.null(tables$libSizePlot)) {
        output$libSizePlot1 <- tables$libSizePlot
      }
    })

    # ---- RLE plot --------------------------------------------------------
    output$RLEplot <- renderUI({
      fluidRow(
        box(
          width = 12, title = "Relative log expression",
          solidHeader = TRUE, closable = TRUE, collapsible = TRUE, status = "primary",
          div(plotOutput(session$ns("rlePlot1"), height = "400px"), align = "center")
        )
      )
    })

    observe({
      if (!is.null(tables$rlePlot)) {
        output$rlePlot1 <- tables$rlePlot
      }
    })

    # ---- PCA (all samples, TMM log2CPM) ----------------------------------
    output$qcPCAplot <- renderPlotly({
      req(tables$norm.data, tables$meta_all, isTRUE(tables$inputValidated))
      fig <- ggplotly(
        PCA2d(
          dat = tables$norm.data, meta = tables$meta_all,
          scale = TRUE, topn = 3000, label = FALSE
        ),
        width = 500, height = 500, tooltip = "text"
      )
      config(fig, toImageButtonOptions = list(
        format = "svg", filename = "QC_PCA_TMM",
        height = 500, width = 500, scale = 1
      ))
    })
  })
}
