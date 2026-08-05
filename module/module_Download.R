library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(zip)

# =============================================================================
# Download Module  –  Step 6
# UI:     DownloadUI(id)
# Server: DownloadServer(id, tables, projSpace, visualizeVals)
# =============================================================================

#' Download UI
#'
#' Executes DownloadUI.
#'
#' @param id Argument for DownloadUI.
#'
#' @return Return value produced by DownloadUI.
DownloadUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        width = 12, title = "Download results",
        solidHeader = TRUE, closable = FALSE, collapsible = TRUE, status = "primary",
        uiOutput(ns("downloadView")),
        column(6, actionButton(ns("btnRefresh"), "Refresh",
                               icon = icon("sync"), class = "btn-success")),
        column(6, downloadButton(ns("btnDownload"), "Download", class = "btn-warning"))
      )
    )
  )
}

#' Download Server
#'
#' Executes DownloadServer.
#'
#' @param id Argument for DownloadServer.
#' @param tables Argument for DownloadServer.
#' @param projSpace Argument for DownloadServer.
#' @param visualizeVals Argument for DownloadServer.
#'
#' @return Return value produced by DownloadServer.
DownloadServer <- function(id, tables, projSpace, visualizeVals = NULL) {
  moduleServer(id, function(input, output, session) {
    # ---- Build the per-analysis checkbox grid ---------------------------
    
    #' Render Download View
    #'
    #' Executes renderDownloadView.
    #'
    #' @return Return value produced by renderDownloadView.
    renderDownloadView <- function() {
      output$downloadView <- renderUI({
        outdirs <- tables$outdirs
        if (is.null(outdirs) || length(outdirs) == 0) {
          return(
            box(
              width = 12,
              title = "Analysis folders",
              solidHeader = FALSE, closable = FALSE, collapsible = FALSE, status = "warning",
              HTML("No analysis output folders detected yet. Download will include files currently present in the project folder.")
            )
          )
        }

        myPanels <- lapply(seq_along(outdirs), function(i) {
          d     <- outdirs[i]
          files <- if (dir.exists(d)) {
            list.files(d, full.names = FALSE, recursive = TRUE, include.dirs = FALSE)
          } else {
            character(0)
          }
          box(
            width = 6,
            title = paste0("Analysis #", i),
            solidHeader = FALSE, closable = FALSE, collapsible = FALSE, status = "warning",
            HTML("<b>", basename(d), "</b><br/><br/>"),
            div(
              style = "max-height: 200px; overflow-y: auto; overflow-x: auto; white-space: nowrap; font-size: 12px; background: #f8f8f8; border: 1px solid #ddd; padding: 4px; border-radius: 4px;",
              HTML(paste(if (length(files)) files else "(no files)", collapse = "<br/>"))
            ),
            checkboxInput(session$ns(paste0("download_", i)), "Download", value = TRUE)
          )
        })
        do.call(fluidRow, myPanels)
      })
    }

    # Render on load and whenever outdirs changes
    observe({ renderDownloadView() })

    # ---- Refresh button -------------------------------------------------
    observeEvent(input$btnRefresh, {
      renderDownloadView()
      checked <- vapply(seq_along(tables$outdirs),
                        function(i) isTRUE(input[[paste0("download_", i)]]),
                        logical(1))
      output$info4 <- renderUI({
        HTML(paste0("You chose: <b>", sum(checked), "</b> folder(s)."))
      })

      # Log visualize cutoffs if available
      if (!is.null(visualizeVals)) {
        logIt(tables$logFile, action = "Visualize", parameter = "log2FC",  value = visualizeVals$inputLogFC())
        logIt(tables$logFile, action = "Visualize", parameter = "P.Value",value = visualizeVals$inputPval())
        logIt(tables$logFile, action = "Visualize", parameter = "FDR",    value = visualizeVals$inputFDR())
        logIt(tables$logFile, action = "Visualize", parameter = "topn",   value = visualizeVals$inputTopN())
      }
    })

    output$info4 <- renderUI({ HTML("Ready") })

    # ---- Download handler -----------------------------------------------
    output$btnDownload <- downloadHandler(
      filename = function() {
        ts <- gsub("[ :]", ".", strtrim(Sys.time(), 22))
        paste0("CAB_RNAseq_", ts, ".zip")
      },
      content = function(filename) {
        tryCatch({
          req(tables$projDir)
          if (!dir.exists(tables$projDir)) {
            stop("Project directory does not exist: ", tables$projDir)
          }

          # Copy session log into project folder when available.
          if (!is.null(tables$prefix) &&
              !is.null(tables$logFile) &&
              nzchar(tables$logFile) &&
              file.exists(tables$logFile)) {
            file.copy(tables$logFile, dirname(tables$prefix), overwrite = TRUE)
          }

          excluded <- character(0)
          if (!is.null(tables$outdirs) && length(tables$outdirs) > 0) {
            checked  <- vapply(seq_along(tables$outdirs),
                               function(i) isTRUE(input[[paste0("download_", i)]]),
                               logical(1))
            excluded <- basename(tables$outdirs[!checked])
          }

          # Build archive from project root without changing process-wide working dir.
          files2zip <- list.files(tables$projDir, full.names = FALSE, all.files = FALSE, no.. = TRUE)
          if (length(excluded) > 0) {
            files2zip <- setdiff(files2zip, excluded)
          }
          files2zip <- files2zip[file.exists(file.path(tables$projDir, files2zip))]

          # zip::zipr cannot create a truly empty archive; include a note file when
          # all folders are excluded to keep the download endpoint valid.
          if (length(files2zip) == 0) {
            note <- tempfile(pattern = "download_selection_", fileext = ".txt")
            writeLines("No project folders were selected for download.", con = note)
            zip::zipr(zipfile = filename, files = note)
          } else {
            zip::zipr(zipfile = filename, files = files2zip, root = tables$projDir, recurse = TRUE)
          }
        }, error = function(e) {
          note <- tempfile(pattern = "download_error_", fileext = ".txt")
          writeLines(c(
            "Download could not be assembled.",
            paste0("Reason: ", conditionMessage(e))
          ), con = note)
          zip::zipr(zipfile = filename, files = note)
        })
      },
      contentType = "application/zip"
    )
  })
}
