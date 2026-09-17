library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(plotly)
library(heatmaply)
library(DT)

# =============================================================================
# Visualize Module  –  Step 4
# UI:     VisualizeUI(id)
# Server: VisualizeServer(id, tables, analyzeVals)
#         analyzeVals = list(Exp, Ctl) returned by AnalyzeServer
# =============================================================================

#' Visualize UI
#'
#' Executes VisualizeUI.
#'
#' @param id Argument for VisualizeUI.
#'
#' @return Return value produced by VisualizeUI.
VisualizeUI <- function(id) {
  ns <- NS(id)
  tagList(
    # ---- Control panel ---------------------------------------------------
    fluidRow(
      box(
        width = 12, title = "Choose panels and cutoffs",
        solidHeader = TRUE, closable = FALSE, collapsible = TRUE, status = "primary",
        column(
          6,
          column(12, HTML("<b>Cutoffs for DEGs</b>")),
          column(4, selectInput(ns("inputLogFC"), "log2FC",
            choices = c(0.1, 0.25, 0.585, 1, 2, 4), selected = 1
          )),
          column(4, selectInput(ns("inputPval"), "P.Value",
            choices = c(0.001, 0.01, 0.05, 0.5, 1), selected = 0.05
          )),
          column(4, selectInput(ns("inputFDR"), "FDR",
            choices = c(0.001, 0.01, 0.05, 0.1, 0.2, 0.5, 1), selected = 0.05
          )),
          column(
            8,
            HTML("<b>TopN for up/down DEG labels</b>"),
            selectInput(ns("inputTopN"), NULL, choices = c(0, 5, 10, 15, 20, 50, 100), selected = 10)
          ),
          column(4, shinycssloaders::withSpinner(uiOutput(ns("infoNumDEGs")))),
          column(12, actionButton(ns("btnApplyCutoffs"), "Apply cutoffs",
            icon = icon("sync"), class = "btn-warning"
          ))
        ),
        column(
          6,
          column(
            4,
            checkboxInput(ns("chkDEGsStat"), "Summary", value = TRUE),
            checkboxInput(ns("chkDEGsTable"), "DEGs table", value = TRUE),
            radioButtons(ns("rbDEGsTable"), "Preview mode",
              choices  = c(TopN = "TopN", DEGs = "DEGs", "All Genes" = "All"),
              selected = "All"
            )
          ),
          column(
            4,
            checkboxInput(ns("chkPCA"), "PCA", value = TRUE),
            checkboxInput(ns("chkVolcano"), "Volcano", value = FALSE),
            checkboxInput(ns("chkAveExpr"), "AveExpr", value = FALSE)
          ),
          column(
            4,
            checkboxInput(ns("chkMAplot"), "MAplot", value = FALSE),
            checkboxInput(ns("chkBoxplot"), "Boxplot", value = FALSE),
            checkboxInput(ns("chkHeatmap"), "DEGs Heatmap", value = FALSE)
          )
        )
      )
    ),
    # ---- Summary + table ------------------------------------------------
    uiOutput(ns("vizDEGsStat")),
    shinycssloaders::withSpinner(uiOutput(ns("vizDEGsTable"))),
    # ---- Plot panels ----------------------------------------------------
    uiOutput(ns("vizTabs"))
  )
}

#' Visualize Server
#'
#' Executes VisualizeServer.
#'
#' @param id Argument for VisualizeServer.
#' @param tables Argument for VisualizeServer.
#' @param analyzeVals Argument for VisualizeServer.
#'
#' @return Return value produced by VisualizeServer.
VisualizeServer <- function(id, tables, analyzeVals) {
  moduleServer(id, function(input, output, session) {
    # Mark an output as always-active only after it is created.
    #' Keep Output Active
    #'
    #' Executes keepOutputActive.
    #'
    #' @param id Argument for keepOutputActive.
    #'
    #' @return Return value produced by keepOutputActive.
    keepOutputActive <- function(id) {
      try(outputOptions(output, id, suspendWhenHidden = FALSE), silent = TRUE)
    }

    heatmapInfo <- reactiveValues(
      genesNotFound = "None",
      validNumGenes = 0,
      topn          = 0,
      topDegText    = "",
      usingTopDEGs  = FALSE
    )

    # Helper: species-aware default gene symbol(s)
    #' Dflt Genes
    #'
    #' Executes dfltGenes.
    #'
    #' @return Return value produced by dfltGenes.
    dfltGenes <- function() if (identical(tables$species, "Homo sapiens")) "GAPDH,TOP1" else "Gapdh,Top1"
    #' Dflt Gene
    #'
    #' Executes dfltGene.
    #'
    #' @return Return value produced by dfltGene.
    dfltGene <- function() if (identical(tables$species, "Homo sapiens")) "GAPDH" else "Gapdh"

    #' Split Gene List
    #'
    #' Executes splitGeneList.
    #'
    #' @param x Argument for splitGeneList.
    #'
    #' @return Return value produced by splitGeneList.
    splitGeneList <- function(x) {
      unique(Filter(nzchar, unlist(strsplit(as.character(x), "[, ]"))))
    }

    #' Collapse Gene List
    #'
    #' Executes collapseGeneList.
    #'
    #' @param x Argument for collapseGeneList.
    #'
    #' @return Return value produced by collapseGeneList.
    collapseGeneList <- function(x) {
      paste(unique(Filter(nzchar, as.character(x))), collapse = ",")
    }

    #' Heatmap Height Px
    #'
    #' Executes heatmapHeightPx.
    #'
    #' @param n_genes Argument for heatmapHeightPx.
    #'
    #' @return Return value produced by heatmapHeightPx.
    heatmapHeightPx <- function(n_genes) {
      n_genes <- suppressWarnings(as.integer(n_genes))
      if (!is.finite(n_genes) || is.na(n_genes) || n_genes < 1L) n_genes <- 1L
      max(650, n_genes * 18 + 260)
    }

    #' Heatmap Subplot Heights
    #'
    #' Executes heatmapSubplotHeights.
    #'
    #' @param total_height Argument for heatmapSubplotHeights.
    #' @param Colv Argument for heatmapSubplotHeights.
    #' @param has_col_side Argument for heatmapSubplotHeights.
    #'
    #' @return Return value produced by heatmapSubplotHeights.
    heatmapSubplotHeights <- function(total_height, Colv = TRUE, has_col_side = TRUE) {
      total_height <- suppressWarnings(as.numeric(total_height))
      if (!is.finite(total_height) || is.na(total_height) || total_height < 1) {
        total_height <- 650
      }

      parts <- numeric(0)
      if (isTRUE(Colv)) parts <- c(parts, 90)
      if (isTRUE(has_col_side)) parts <- c(parts, 34)
      body_height <- max(300, total_height - sum(parts))
      parts <- c(parts, body_height)
      parts / sum(parts)
    }

    #' Current Top Heatmap Genes
    #'
    #' Executes currentTopHeatmapGenes.
    #'
    #' @param diff_table Argument for currentTopHeatmapGenes.
    #' @param cutoff_choice Argument for currentTopHeatmapGenes.
    #'
    #' @return Return value produced by currentTopHeatmapGenes.
    currentTopHeatmapGenes <- function(diff_table, cutoff_choice) {
      cutoff_Pval <- if (identical(cutoff_choice, "P.Value")) as.numeric(input$inputPval) else 1
      cutoff_FDR <- if (identical(cutoff_choice, "FDR")) as.numeric(input$inputFDR) else 1
      res <- GetDEGs(diff_table,
        cutoff_logFC = as.numeric(input$inputLogFC),
        cutoff_Pval  = cutoff_Pval,
        cutoff_FDR   = cutoff_FDR,
        topn         = as.numeric(input$inputTopN)
      )
      genes <- unique(as.character(unlist(res[["topnDEGs"]], use.names = FALSE)))
      genes <- genes[!is.na(genes) & nzchar(genes)]
      list(
        genes = genes,
        count = sum(lengths(res[["topnDEGs"]]))
      )
    }

    #' Set Heatmap Top Genes
    #'
    #' Executes setHeatmapTopGenes.
    #'
    #' @param inputId Argument for setHeatmapTopGenes.
    #' @param info Argument for setHeatmapTopGenes.
    #' @param diff_table Argument for setHeatmapTopGenes.
    #' @param cutoff_choice Argument for setHeatmapTopGenes.
    #'
    #' @return Return value produced by setHeatmapTopGenes.
    setHeatmapTopGenes <- function(inputId, info, diff_table, cutoff_choice) {
      top <- currentTopHeatmapGenes(diff_table, cutoff_choice)
      info$topDegText <- collapseGeneList(top$genes)
      info$usingTopDEGs <- TRUE
      info$validNumGenes <- top$count
      info$genesNotFound <- "None"
      updateTextAreaInput(session, inputId, value = info$topDegText)
    }

    #' Current Plot Top DEGs
    #'
    #' Executes currentPlotTopDEGs.
    #'
    #' @param diff_table Argument for currentPlotTopDEGs.
    #' @param fallback Argument for currentPlotTopDEGs.
    #'
    #' @return Return value produced by currentPlotTopDEGs.
    currentPlotTopDEGs <- function(diff_table, fallback = NULL) {
      tryCatch(
        {
          res <- GetDEGs(diff_table,
            cutoff_logFC = as.numeric(input$inputLogFC),
            cutoff_Pval  = as.numeric(input$inputPval),
            cutoff_FDR   = as.numeric(input$inputFDR),
            topn         = as.numeric(input$inputTopN)
          )
          res[["topnDEGs"]]
        },
        error = function(e) fallback
      )
    }

    output$vizTabs <- renderUI({
      tabs <- list()

      if (isTRUE(input$chkPCA)) {
        tabs[[length(tabs) + 1]] <- tabPanel(
          tags$b("PCA"),
          uiOutput(session$ns("vizPCA1"))
        )
      }
      if (isTRUE(input$chkVolcano)) {
        tabs[[length(tabs) + 1]] <- tabPanel(
          tags$b("Volcano"),
          shinycssloaders::withSpinner(uiOutput(session$ns("vizVolcano")))
        )
      }
      if (isTRUE(input$chkAveExpr)) {
        tabs[[length(tabs) + 1]] <- tabPanel(
          tags$b("AveExpr"),
          shinycssloaders::withSpinner(uiOutput(session$ns("vizAveExpr")))
        )
      }
      if (isTRUE(input$chkMAplot)) {
        tabs[[length(tabs) + 1]] <- tabPanel(
          tags$b("MA Plot"),
          shinycssloaders::withSpinner(uiOutput(session$ns("vizMAplot")))
        )
      }
      if (isTRUE(input$chkBoxplot)) {
        tabs[[length(tabs) + 1]] <- tabPanel(
          tags$b("Boxplot"),
          uiOutput(session$ns("vizBoxplot"))
        )
      }
      if (isTRUE(input$chkHeatmap)) {
        erccMode <- !is.null(tables$DEG_TMM)
        if (erccMode) {
          tabs[[length(tabs) + 1]] <- tabPanel(
            tags$b("Heatmap"),
            box(
              width = 12, title = "Heatmap (ERCC)",
              solidHeader = TRUE, closable = FALSE,
              collapsible = TRUE, collapsed = FALSE, status = "primary",
              uiOutput(session$ns("vizHeatmapSetting")),
              uiOutput(session$ns("vizHeatmap1"))
            ),
            box(
              width = 12, title = "Heatmap (TMM)",
              solidHeader = TRUE, closable = FALSE,
              collapsible = TRUE, collapsed = TRUE, status = "primary",
              uiOutput(session$ns("vizHeatmapSettingTMM")),
              uiOutput(session$ns("vizHeatmapTMM1"))
            )
          )
        } else {
          tabs[[length(tabs) + 1]] <- tabPanel(
            tags$b("Heatmap"),
            uiOutput(session$ns("vizHeatmapSetting")),
            uiOutput(session$ns("vizHeatmap1"))
          )
        }
      }

      if (length(tabs) == 0) {
        return(
          fluidRow(
            box(
              width = 12,
              title = "Visualization Tabs",
              solidHeader = TRUE,
              closable = FALSE,
              collapsible = TRUE,
              status = "info",
              p("Select one or more panels above to open them in separate tabs.")
            )
          )
        )
      }

      fluidRow(
        do.call(tabBox, c(list(
          id = session$ns("vizTabset"),
          width = 12,
          title = "Visualizations"
        ), tabs))
      )
    })

    # ---- Apply cutoffs --------------------------------------------------
    observeEvent(input$btnApplyCutoffs, {
      req(tables$DEG)
      resDEGs <- GetDEGs(tables$DEG,
        cutoff_logFC = as.numeric(input$inputLogFC),
        cutoff_Pval  = as.numeric(input$inputPval),
        cutoff_FDR   = as.numeric(input$inputFDR),
        topn         = as.numeric(input$inputTopN)
      )
      tables$numDEGs <- resDEGs[["numDEGs"]]
      tables$topnDEGs <- resDEGs[["topnDEGs"]]

      output$DEGsStat <- DT::renderDT(
        DT::datatable(tables$numDEGs,
          class = "display nowrap compact",
          rownames = FALSE, options = list(dom = "t")
        )
      )
    })

    output$infoNumDEGs <- renderText({
      n <- if (sum(lengths(tables$topnDEGs)) <= 1) {
        as.numeric(input$inputTopN) * 2
      } else {
        sum(lengths(tables$topnDEGs))
      }
      HTML(paste0("<br/><br/>Selected top DEGs: <b>", n, "</b>"))
    })

    # ---- Summary table --------------------------------------------------
    output$vizDEGsStat <- renderUI({
      req(input$chkDEGsStat)
      fluidRow(box(
        width = 12, title = "Summary", solidHeader = TRUE,
        closable = FALSE, collapsible = TRUE, status = "primary",
        div(DT::dataTableOutput(session$ns("DEGsStat"), width = "80%"))
      ))
    })
    output$DEGsStat <- DT::renderDT(
      DT::datatable(tables$numDEGs,
        class = "display nowrap compact",
        rownames = FALSE, options = list(dom = "t")
      )
    )

    # ---- DEGs table -----------------------------------------------------
    output$vizDEGsTable <- renderUI({
      req(input$chkDEGsTable)
      fluidRow(box(
        width = 12, title = "Differential Expressed Genes",
        solidHeader = TRUE, closable = FALSE, collapsible = TRUE, status = "primary",
        div(
          style = "overflow-x: scroll",
          DT::dataTableOutput(session$ns("DEGsTable"))
        )
      ))
    })

    #' Render DEGs Table
    #'
    #' Executes renderDEGsTable.
    #'
    #' @param df Argument for renderDEGsTable.
    #'
    #' @return Return value produced by renderDEGsTable.
    renderDEGsTable <- function(df) {
      output$DEGsTable <- DT::renderDT(
        df,
        class = "display nowrap compact", filter = "top", rownames = FALSE,
        options = list(search = list(regex = TRUE, caseInsensitive = TRUE))
      )
    }

    observe({
      req(tables$DEG)
      renderDEGsTable(as.data.frame(tables$DEG))
    })

    observeEvent(input$rbDEGsTable, {
      req(tables$DEG)
      df <- as.data.frame(tables$DEG)
      if (input$rbDEGsTable == "All") {
        renderDEGsTable(df)
      } else {
        topn <- if (input$rbDEGsTable == "TopN") as.numeric(input$inputTopN) else nrow(df)
        res <- GetDEGs(df,
          cutoff_logFC = as.numeric(input$inputLogFC),
          cutoff_Pval  = as.numeric(input$inputPval),
          cutoff_FDR   = as.numeric(input$inputFDR),
          topn         = topn
        )
        degs <- unlist(res[["topnDEGs"]])
        renderDEGsTable(df[as.character(df$gene) %in% degs, ])
      }
    })

    # ---- PCA ------------------------------------------------------------
    output$vizPCA1 <- renderUI({
      req(input$chkPCA)
      hasTMM <- !is.null(tables$DEG_TMM)
      fluidRow(box(
        width = 12, title = tables$PCA_title1,
        solidHeader = TRUE, closable = FALSE, collapsible = FALSE, status = "primary",
        fluidRow(
          column(
            6,
            selectInput(session$ns("inputPCAVar"), "Number of features",
              choices = c(500, 1000, 2000, 3000, 5000), selected = 3000
            )
          ),
          column(
            6,
            checkboxInput(session$ns("chkPCAlabel"), "Show label", value = FALSE),
            checkboxInput(session$ns("chkPCAallSamples"), "Include all samples", value = FALSE),
            checkboxInput(session$ns("chkPCAscale"), "Scale data", value = TRUE)
          )
        ),
        fluidRow(column(
          12,
          actionButton(session$ns("btnPCAplot"), "Refresh", icon = icon("sync"), class = "btn-success"),
          tags$hr()
        )),
        if (hasTMM) {
          fluidRow(
            column(
              6,
              tags$h4(tags$b("TMM Normalization")),
              plotlyOutput(session$ns("PCAplot1_tmm"), height = "500px", width = "500px")
            ),
            column(
              6,
              tags$h4(tags$b("ERCC Normalization")),
              plotlyOutput(session$ns("PCAplot1"), height = "500px", width = "500px")
            )
          )
        } else {
          plotlyOutput(session$ns("PCAplot1"), height = "500px")
        }
      ))
    })

    #' Calc Ercc Norm Log2 CPM
    #'
    #' Executes calcErccNormLog2CPM.
    #'
    #' @param meta Argument for calcErccNormLog2CPM.
    #'
    #' @return Return value produced by calcErccNormLog2CPM.
    calcErccNormLog2CPM <- function(meta) {
      req(tables$counts, tables$counts_ERCC, meta)

      counts <- as.data.frame(tables$counts)
      counts_ercc <- as.data.frame(tables$counts_ERCC)

      if (ncol(counts) > 0 && tolower(colnames(counts)[1]) == "gene") {
        counts <- counts[, -1, drop = FALSE]
      }
      if (ncol(counts_ercc) > 0 && tolower(colnames(counts_ercc)[1]) == "gene") {
        counts_ercc <- counts_ercc[, -1, drop = FALSE]
      }

      common_ids <- intersect(meta$ID, colnames(counts))
      common_ids <- intersect(common_ids, colnames(counts_ercc))
      req(length(common_ids) >= 2)

      counts <- counts[, common_ids, drop = FALSE]
      counts_ercc <- counts_ercc[, common_ids, drop = FALSE]
      counts <- counts[, unlist(lapply(counts, is.numeric)), drop = FALSE]
      counts_ercc <- counts_ercc[, unlist(lapply(counts_ercc, is.numeric)), drop = FALSE]
      req(ncol(counts) >= 2, ncol(counts_ercc) >= 2)

      dge <- edgeR::DGEList(counts = counts)
      dge <- edgeR::calcNormFactors(dge)
      sf_ercc <- edgeR::calcNormFactors(counts_ercc, lib.size = dge$samples$lib.size)
      dge$samples$norm.factors <- sf_ercc
      edgeR::cpm(dge, log = TRUE)
    }

    #' Plot PCA
    #'
    #' Executes plotPCA.
    #'
    #' @param dat Argument for plotPCA.
    #' @param meta Argument for plotPCA.
    #' @param outFile Argument for plotPCA.
    #' @param outputId Argument for plotPCA.
    #' @param plotHeight Argument for plotPCA.
    #' @param plotWidth Argument for plotPCA.
    #' @param exportWidth Argument for plotPCA.
    #'
    #' @return Return value produced by plotPCA.
    plotPCA <- function(dat, meta, outFile, outputId, plotHeight = 700, plotWidth = NULL, exportWidth = 700) {
      req(dat, meta)
      topn <- if (!is.null(input$inputPCAVar)) as.numeric(input$inputPCAVar) else 3000
      scale <- if (!is.null(input$chkPCAscale)) input$chkPCAscale else TRUE
      label <- if (!is.null(input$chkPCAlabel)) input$chkPCAlabel else FALSE
      output[[outputId]] <- renderPlotly({
        fig <- ggplotly(
          PCA2d(
            dat = dat, meta = meta,
            scale = scale, topn = topn, outFile = outFile,
            coordFile = paste0(tools::file_path_sans_ext(outFile), "_coordinates.tsv"),
            label = label
          ),
          width = plotWidth, height = plotHeight, tooltip = "text"
        )
        config(fig, toImageButtonOptions = list(
          format = "svg", filename = basename(outFile),
          height = plotHeight, width = exportWidth, scale = 1
        ))
      })
      keepOutputActive(outputId)
    }

    #' Render PCA
    #'
    #' Executes renderPCA.
    #'
    #' @return Return value produced by renderPCA.
    renderPCA <- function() {
      req(tables$norm.data)
      hasTMM <- !is.null(tables$DEG_TMM)
      useAll <- isTRUE(input$chkPCAallSamples)

      if (hasTMM) {
        meta_tmm <- if (useAll) tables$meta_all else if (!is.null(tables$meta_used_TMM)) tables$meta_used_TMM else tables$meta_used
        meta_ercc <- if (useAll) tables$meta_all else tables$meta_used
        req(meta_tmm, meta_ercc)

        outFile_tmm <- if (useAll) paste0(tables$prefix, "_TMM_PCA_all.pdf") else paste0(tables$prefix, "_TMM_PCA.pdf")
        outFile_ercc <- if (useAll) paste0(tables$prefix, "_ERCC_PCA_all.pdf") else paste0(tables$prefix, "_ERCC_PCA.pdf")

        plotPCA(tables$norm.data, meta_tmm, outFile_tmm, "PCAplot1_tmm",
          plotHeight = 500, exportWidth = 700
        )

        ercc_norm_data <- tryCatch(
          calcErccNormLog2CPM(meta_ercc),
          error = function(e) NULL
        )
        if (!is.null(ercc_norm_data)) {
          plotPCA(ercc_norm_data, meta_ercc, outFile_ercc, "PCAplot1",
            plotHeight = 500, exportWidth = 700
          )
        }

        logIt(tables$logFile, action = "Output", parameter = "PCA_plot_TMM", value = basename(outFile_tmm))
        if (!is.null(ercc_norm_data)) {
          logIt(tables$logFile, action = "Output", parameter = "PCA_plot_ERCC", value = basename(outFile_ercc))
        }
      } else {
        meta_tmm <- if (useAll) tables$meta_all else tables$meta_used
        req(meta_tmm)
        outFile_tmm <- if (useAll) paste0(tables$prefix, "_PCA_all.pdf") else paste0(tables$prefix, "_PCA.pdf")
        plotPCA(tables$norm.data, meta_tmm, outFile_tmm, "PCAplot1",
          plotHeight = 700, plotWidth = 700, exportWidth = 700
        )
        logIt(tables$logFile, action = "Output", parameter = "PCA_plot", value = basename(outFile_tmm))
      }
    }

    observeEvent(input$chkPCA, {
      req(tables$norm.data)
      tables$PCA_title1 <- "Principal Component Analysis (PCA)"
      renderPCA()
    })

    observeEvent(input$btnPCAplot, {
      renderPCA()
    })

    # ---- Volcano --------------------------------------------------------
    output$vizVolcano <- renderUI({
      req(input$chkVolcano)
      hasTMM <- !is.null(tables$DEG_TMM)
      fluidRow(box(
        width = 12, title = "Volcano plot",
        solidHeader = TRUE, closable = FALSE, collapsible = FALSE, status = "primary",
        HTML("<b>Enter genes [optional]</b>"),
        textInput(session$ns("inputGeneVp"), NULL, dfltGenes()),
        actionButton(session$ns("btnVp"), "Refresh", icon = icon("sync"), class = "btn-success"),
        tags$hr(),
        htmlOutput(session$ns("warningVolcanoplot1")),
        if (hasTMM) {
          fluidRow(
            column(
              6, tags$h4(tags$b("TMM Normalization")),
              plotlyOutput(session$ns("volcanoplot1_tmm"), height = "500px", width = "100%")
            ),
            column(
              6, tags$h4(tags$b("ERCC Normalization")),
              plotlyOutput(session$ns("volcanoplot1"), height = "500px", width = "100%")
            )
          )
        } else {
          plotlyOutput(session$ns("volcanoplot1"), height = "700px")
        }
      ))
    })

    #' Render Volcano
    #'
    #' Executes renderVolcano.
    #'
    #' @param genes Argument for renderVolcano.
    #'
    #' @return Return value produced by renderVolcano.
    renderVolcano <- function(genes) {
      req(tables$DEG)
      hasTMM <- !is.null(tables$DEG_TMM)
      pW <- if (hasTMM) NULL else 700
      pH <- if (hasTMM) 500 else 700
      erccTag <- if (hasTMM) "ERCC_" else ""
      outFile <- paste0(tables$prefix, "_", erccTag, "volcano.pdf")
      output$volcanoplot1 <- renderPlotly({
        topnERCC <- currentPlotTopDEGs(tables$DEG, fallback = tables$topnDEGs)
        fig <- ggplotly(
          PlotVolcano(tables$DEG,
            cutoff_logFC = as.numeric(input$inputLogFC),
            cutoff_FDR = as.numeric(input$inputFDR),
            cols = NA, topnDEGs = topnERCC,
            GOI = genes, outFile = outFile,
            strLogFC = "log2FC", strPval = "P.Value",
            strAveExpr = "AveExpr", strPadj = "adj.P.Val"
          ),
          width = pW, height = pH, tooltip = "text"
        )
        config(fig, toImageButtonOptions = list(
          format = "svg", filename = basename(paste0(tables$prefix, "_", erccTag, "volcano")),
          height = pH, width = 700, scale = 1
        ))
      })
      keepOutputActive("volcanoplot1")
      # TMM version
      if (hasTMM) {
        outFile_tmm <- paste0(tables$prefix, "_TMM_volcano.pdf")
        output$volcanoplot1_tmm <- renderPlotly({
          topnTMM <- currentPlotTopDEGs(tables$DEG_TMM, fallback = tables$topnDEGs)
          fig <- ggplotly(
            PlotVolcano(tables$DEG_TMM,
              cutoff_logFC = as.numeric(input$inputLogFC),
              cutoff_FDR = as.numeric(input$inputFDR),
              cols = NA, topnDEGs = topnTMM,
              GOI = genes, outFile = outFile_tmm,
              strLogFC = "log2FC", strPval = "P.Value",
              strAveExpr = "AveExpr", strPadj = "adj.P.Val"
            ),
            width = NULL, height = 500, tooltip = "text"
          )
          config(fig, toImageButtonOptions = list(
            format = "svg", filename = basename(paste0(tables$prefix, "_TMM_volcano")),
            height = 500, width = 700, scale = 1
          ))
        })
        keepOutputActive("volcanoplot1_tmm")
      }
      logIt(tables$logFile, action = "Output", parameter = "volcano_plot", value = basename(outFile))
    }

    observeEvent(input$chkVolcano, {
      gene <- rownames(tables$DEG)[1]
      updateTextInput(session, "inputGeneVp", value = gene)
      renderVolcano(NULL)
    })

    observeEvent(input$btnVp, {
      genes0 <- unique(Filter(nzchar, unlist(strsplit(input$inputGeneVp, "[, ]"))))
      genes <- intersect2(genes0, rownames(tables$DEG))
      if (length(genes) > 0) {
        renderVolcano(genes)
        invalid <- setdiff(genes0, genes)
        output$warningVolcanoplot1 <- renderUI({
          if (length(invalid) > 0) {
            HTML(paste0('<font color="#FF0000">Invalid gene(s): <b>', paste(invalid, collapse = " "), "</b></font>"))
          } else {
            HTML("")
          }
        })
        logIt(tables$logFile, action = "Volcano", parameter = "genes", value = genes)
      } else {
        output$warningVolcanoplot1 <- renderUI({
          HTML(paste0('<font color="#FF0000">Invalid gene symbol(s): <b>', input$inputGeneVp, "</b></font>"))
        })
      }
    })

    # ---- AveExpr --------------------------------------------------------
    output$vizAveExpr <- renderUI({
      req(input$chkAveExpr)
      hasTMM <- !is.null(tables$DEG_TMM)
      fluidRow(box(
        width = 12, title = "AveExpr plot",
        solidHeader = TRUE, closable = FALSE, collapsible = FALSE, status = "primary",
        HTML("<b>Enter genes [optional]</b>"),
        textInput(session$ns("inputGeneAp"), NULL, dfltGenes()),
        actionButton(session$ns("btnAveExprPlot"), "Refresh", icon = icon("sync"), class = "btn-success"),
        tags$hr(),
        htmlOutput(session$ns("warningAveExprPlot1")),
        if (hasTMM) {
          fluidRow(
            column(
              6, tags$h4(tags$b("TMM Normalization")),
              plotlyOutput(session$ns("aveExprPlot1_tmm"), height = "500px", width = "100%")
            ),
            column(
              6, tags$h4(tags$b("ERCC Normalization")),
              plotlyOutput(session$ns("aveExprPlot1"), height = "500px", width = "100%")
            )
          )
        } else {
          plotlyOutput(session$ns("aveExprPlot1"), height = "700px")
        }
      ))
    })

    #' Render Ave Expr
    #'
    #' Executes renderAveExpr.
    #'
    #' @param genes Argument for renderAveExpr.
    #'
    #' @return Return value produced by renderAveExpr.
    renderAveExpr <- function(genes) {
      req(tables$DEG)
      hasTMM <- !is.null(tables$DEG_TMM)
      pW <- if (hasTMM) NULL else 700
      pH <- if (hasTMM) 500 else 700
      erccTag <- if (hasTMM) "ERCC_" else ""
      outFile <- paste0(tables$prefix, "_", erccTag, "AveExpr.pdf")
      output$aveExprPlot1 <- renderPlotly({
        topnERCC <- currentPlotTopDEGs(tables$DEG, fallback = tables$topnDEGs)
        fig <- ggplotly(
          PlotAveExpr(tables$DEG,
            cols = NA, topnDEGs = topnERCC,
            GOI = genes, outFile = outFile,
            strLogFC = "log2FC", strPval = "P.Value",
            strAveExpr = "AveExpr", strPadj = "adj.P.Val"
          ),
          width = pW, height = pH, tooltip = "text"
        )
        config(fig, toImageButtonOptions = list(
          format = "svg", filename = basename(paste0(tables$prefix, "_", erccTag, "aveExpr")),
          height = pH, width = 700, scale = 1
        ))
      })
      keepOutputActive("aveExprPlot1")
      # TMM version
      if (hasTMM) {
        outFile_tmm <- paste0(tables$prefix, "_TMM_AveExpr.pdf")
        output$aveExprPlot1_tmm <- renderPlotly({
          topnTMM <- currentPlotTopDEGs(tables$DEG_TMM, fallback = tables$topnDEGs)
          fig <- ggplotly(
            PlotAveExpr(tables$DEG_TMM,
              cols = NA, topnDEGs = topnTMM,
              GOI = genes, outFile = outFile_tmm,
              strLogFC = "log2FC", strPval = "P.Value",
              strAveExpr = "AveExpr", strPadj = "adj.P.Val"
            ),
            width = NULL, height = 500, tooltip = "text"
          )
          config(fig, toImageButtonOptions = list(
            format = "svg", filename = basename(paste0(tables$prefix, "_TMM_AveExpr")),
            height = 500, width = 700, scale = 1
          ))
        })
        keepOutputActive("aveExprPlot1_tmm")
      }
      logIt(tables$logFile, action = "Output", parameter = "AveExpr_plot", value = basename(outFile))
    }

    observeEvent(input$chkAveExpr, {
      gene <- rownames(tables$DEG)[1]
      updateTextInput(session, "inputGeneAp", value = gene)
      renderAveExpr(NULL)
    })

    observeEvent(input$btnAveExprPlot, {
      genes0 <- unique(Filter(nzchar, unlist(strsplit(input$inputGeneAp, "[, ]"))))
      genes <- intersect2(genes0, rownames(tables$DEG))
      if (length(genes) > 0) {
        renderAveExpr(genes)
        invalid <- setdiff(genes0, genes)
        output$warningAveExprPlot1 <- renderUI({
          if (length(invalid) > 0) {
            HTML(paste0('<font color="#FF0000">Invalid gene(s): <b>', paste(invalid, collapse = " "), "</b></font>"))
          } else {
            HTML("")
          }
        })
        logIt(tables$logFile, action = "AveExpr_setting", parameter = "genes", value = genes)
      } else {
        output$warningAveExprPlot1 <- renderUI({
          HTML(paste0('<font color="#FF0000">Invalid gene symbol(s): <b>', input$inputGeneAp, "</b></font>"))
        })
      }
    })

    # ---- MA plot --------------------------------------------------------
    output$vizMAplot <- renderUI({
      req(input$chkMAplot)
      hasTMM <- !is.null(tables$DEG_TMM)
      fluidRow(box(
        width = 12, title = "MA plot",
        solidHeader = TRUE, closable = FALSE, collapsible = FALSE, status = "primary",
        HTML("<b>Enter genes [optional]</b>"),
        textInput(session$ns("inputGeneMp"), NULL, dfltGenes()),
        actionButton(session$ns("btnMp"), "Refresh", icon = icon("sync"), class = "btn-success"),
        tags$hr(),
        htmlOutput(session$ns("warningMAplot1")),
        if (hasTMM) {
          fluidRow(
            column(
              6, tags$h4(tags$b("TMM Normalization")),
              plotlyOutput(session$ns("MAplot1_tmm"), height = "500px", width = "100%")
            ),
            column(
              6, tags$h4(tags$b("ERCC Normalization")),
              plotlyOutput(session$ns("MAplot1"), height = "500px", width = "100%")
            )
          )
        } else {
          plotlyOutput(session$ns("MAplot1"), height = "700px")
        }
      ))
    })

    #' Render MAplot
    #'
    #' Executes renderMAplot.
    #'
    #' @param genes Argument for renderMAplot.
    #'
    #' @return Return value produced by renderMAplot.
    renderMAplot <- function(genes) {
      req(tables$DEG)
      hasTMM <- !is.null(tables$DEG_TMM)
      pW <- if (hasTMM) NULL else 700
      pH <- if (hasTMM) 500 else 700
      erccTag <- if (hasTMM) "ERCC_" else ""
      outFile <- paste0(tables$prefix, "_", erccTag, "MAplot.pdf")
      output$MAplot1 <- renderPlotly({
        topnERCC <- currentPlotTopDEGs(tables$DEG, fallback = tables$topnDEGs)
        fig <- ggplotly(
          PlotMA(tables$DEG,
            cutoff_logFC = as.numeric(input$inputLogFC),
            cols = NA, topnDEGs = topnERCC, GOI = genes, outFile = outFile,
            strLogFC = "log2FC", strPval = "P.Value",
            strAveExpr = "AveExpr", strPadj = "adj.P.Val"
          ),
          width = pW, height = pH, tooltip = "text"
        )
        config(fig, toImageButtonOptions = list(
          format = "svg", filename = basename(paste0(tables$prefix, "_", erccTag, "MAplot")),
          height = pH, width = 700, scale = 1
        ))
      })
      keepOutputActive("MAplot1")
      # TMM version
      if (hasTMM) {
        outFile_tmm <- paste0(tables$prefix, "_TMM_MAplot.pdf")
        output$MAplot1_tmm <- renderPlotly({
          topnTMM <- currentPlotTopDEGs(tables$DEG_TMM, fallback = tables$topnDEGs)
          fig <- ggplotly(
            PlotMA(tables$DEG_TMM,
              cutoff_logFC = as.numeric(input$inputLogFC),
              cols = NA, topnDEGs = topnTMM, GOI = genes, outFile = outFile_tmm,
              strLogFC = "log2FC", strPval = "P.Value",
              strAveExpr = "AveExpr", strPadj = "adj.P.Val"
            ),
            width = NULL, height = 500, tooltip = "text"
          )
          config(fig, toImageButtonOptions = list(
            format = "svg", filename = basename(paste0(tables$prefix, "_TMM_MAplot")),
            height = 500, width = 700, scale = 1
          ))
        })
        keepOutputActive("MAplot1_tmm")
      }
      logIt(tables$logFile, action = "Output", parameter = "MAplot", value = basename(outFile))
    }

    observeEvent(input$chkMAplot, {
      gene <- rownames(tables$DEG)[1]
      updateTextInput(session, "inputGeneMp", value = gene)
      renderMAplot(NULL)
    })

    observeEvent(input$btnMp, {
      genes0 <- unique(Filter(nzchar, unlist(strsplit(input$inputGeneMp, "[, ]"))))
      genes <- intersect2(genes0, rownames(tables$DEG))
      if (length(genes) > 0) {
        renderMAplot(genes)
        invalid <- setdiff(genes0, genes)
        output$warningMAplot1 <- renderUI({
          if (length(invalid) > 0) {
            HTML(paste0('<font color="#FF0000">Invalid gene(s): <b>', paste(invalid, collapse = " "), "</b></font>"))
          } else {
            HTML("")
          }
        })
        logIt(tables$logFile, action = "MAplot_setting", parameter = "genes", value = genes)
      } else {
        output$warningMAplot1 <- renderUI({
          HTML(paste0('<font color="#FF0000">Invalid gene symbol(s): <b>', input$inputGeneMp, "</b></font>"))
        })
      }
    })

    # ---- Boxplot --------------------------------------------------------
    #' Enforce Black Boxplot Lines
    #'
    #' Executes enforceBlackBoxplotLines.
    #'
    #' @param fig Argument for enforceBlackBoxplotLines.
    #'
    #' @return Return value produced by enforceBlackBoxplotLines.
    enforceBlackBoxplotLines <- function(fig) {
      #' Set Box Line
      #'
      #' Executes setBoxLine.
      #'
      #' @param trace Argument for setBoxLine.
      #'
      #' @return Return value produced by setBoxLine.
      setBoxLine <- function(trace) {
        if (identical(trace$type, "box") || inherits(trace, "plotly_boxplot")) {
          line <- trace$line
          if (is.null(line)) line <- list()
          line$color <- "black"
          trace$line <- line
        }
        trace
      }
      if (!inherits(fig, "plotly")) {
        return(fig)
      }
      if (!is.null(fig$x$data)) {
        for (i in seq_along(fig$x$data)) {
          fig$x$data[[i]] <- setBoxLine(fig$x$data[[i]])
        }
      }
      if (!is.null(fig$x$attrs)) {
        for (i in seq_along(fig$x$attrs)) {
          fig$x$attrs[[i]] <- setBoxLine(fig$x$attrs[[i]])
        }
      }
      fig
    }

    #' Viz Boxplotly
    #'
    #' Executes vizBoxplotly.
    #'
    #' @param ... Argument for vizBoxplotly.
    #'
    #' @return Return value produced by vizBoxplotly.
    vizBoxplotly <- function(...) {
      enforceBlackBoxplotLines(doBoxplotly(...))
    }

    output$vizBoxplot <- renderUI({
      req(input$chkBoxplot)
      hasTMM <- !is.null(tables$DEG_TMM)
      fluidRow(box(
        width = 12, title = "Boxplot",
        solidHeader = TRUE, closable = FALSE, collapsible = FALSE, status = "primary",
        fluidRow(
          column(
            6, HTML("<b>Enter a gene</b>"),
            textInput(session$ns("inputGeneBp"), NULL, dfltGene())
          ),
          column(12, checkboxInput(session$ns("chkBoxplotAllSamples"),
            "Include all samples",
            value = FALSE
          ))
        ),
        fluidRow(column(
          12,
          actionButton(session$ns("btnBoxplot"), "Refresh",
            icon = icon("sync"), class = "btn-success"
          ),
          tags$hr()
        )),
        htmlOutput(session$ns("warningBoxplot1")),
        if (hasTMM) {
          fluidRow(
            column(
              6, tags$h4(tags$b("TMM Normalization")),
              plotlyOutput(session$ns("Boxplot1_tmm"), height = "500px", width = "500px")
            ),
            column(
              6, tags$h4(tags$b("ERCC Normalization")),
              plotlyOutput(session$ns("Boxplot1"), height = "500px", width = "500px")
            )
          )
        } else {
          plotlyOutput(session$ns("Boxplot1"), height = "700px")
        }
      ))
    })

    observeEvent(input$chkBoxplot, {
      genes <- rownames(tables$DEG)
      genes <- genes[!grepl("^Gm", genes)]
      gene <- genes[1]
      hasTMM <- !is.null(tables$DEG_TMM)
      pW <- if (hasTMM) NULL else 700
      pH <- if (hasTMM) 500 else 700
      updateTextInput(session, "inputGeneBp", value = gene)
      output$Boxplot1 <- renderPlotly({
        req(tables$DEG, tables$meta_used)
        vizBoxplotly(tables$DEG, tables$meta_used,
          gene = gene, outFile = NA,
          width = pW, height = pH
        )
      })
      keepOutputActive("Boxplot1")
      if (hasTMM) {
        output$Boxplot1_tmm <- renderPlotly({
          req(tables$DEG_TMM, tables$meta_used)
          vizBoxplotly(tables$DEG_TMM, tables$meta_used,
            gene = gene, outFile = NA,
            width = NULL, height = 500
          )
        })
        keepOutputActive("Boxplot1_tmm")
      }
    })

    observeEvent(input$btnBoxplot, {
      req(tables)
      gene <- input$inputGeneBp
      hasTMM <- !is.null(tables$DEG_TMM)
      erccTag <- if (hasTMM) "ERCC_" else ""
      if (gene %in% rownames(tables$DEG) || gene %in% rownames(tables$log2CPM)) {
        if (isTRUE(input$chkBoxplotAllSamples)) {
          outFile <- paste0(tables$prefix, "_", erccTag, "boxplot_", gene, "_all.pdf")
          data1 <- tables$log2CPM
          meta1 <- tables$meta_all
          showStat <- FALSE
        } else {
          outFile <- paste0(tables$prefix, "_", erccTag, "boxplot_", gene, ".pdf")
          data1 <- tables$DEG
          meta1 <- tables$meta_used
          showStat <- TRUE
        }
        output$Boxplot1 <- renderPlotly({
          req(data1, meta1)
          hasTMM <- !is.null(tables$DEG_TMM)
          pW <- if (hasTMM) NULL else 700
          pH <- if (hasTMM) 500 else 700
          vizBoxplotly(data1, meta1,
            gene = gene, outFile = outFile,
            showStat = showStat, width = pW, height = pH
          )
        })
        keepOutputActive("Boxplot1")
        # TMM version
        if (!is.null(tables$DEG_TMM)) {
          if (isTRUE(input$chkBoxplotAllSamples)) {
            data1_tmm <- tables$log2CPM
            meta1_tmm <- tables$meta_all
            showStat_tmm <- FALSE
          } else {
            data1_tmm <- tables$DEG_TMM
            meta1_tmm <- tables$meta_used
            showStat_tmm <- TRUE
          }
          outFile_tmm <- paste0(tables$prefix, "_TMM_boxplot_", gene, ".pdf")
          output$Boxplot1_tmm <- renderPlotly({
            req(data1_tmm, meta1_tmm)
            vizBoxplotly(data1_tmm, meta1_tmm,
              gene = gene,
              outFile = outFile_tmm, showStat = showStat_tmm,
              width = 500, height = 500
            )
          })
          keepOutputActive("Boxplot1_tmm")
        }
        output$warningBoxplot1 <- renderUI({
          HTML("")
        })
        logIt(tables$logFile, action = "Output", parameter = "Boxplot", value = basename(outFile))
      } else {
        output$warningBoxplot1 <- renderUI({
          HTML(paste0('<font color="#FF0000">Invalid gene symbol: <b>', gene, "</b></font>"))
        })
      }
    })

    observeEvent(input$chkBoxplotAllSamples, {
      shinyjs::click("btnBoxplot")
    })

    # ---- Heatmap --------------------------------------------------------
    output$vizHeatmapSetting <- renderUI({
      req(input$chkHeatmap)
      fluidRow(box(
        width = 12, title = "Heatmap setting",
        solidHeader = TRUE, closable = FALSE, collapsible = TRUE, status = "primary",
        column(
          12,
          column(
            12,
            column(
              2,
              radioButtons(session$ns("cutoff4Heatmap"), "Minimum Cutoffs",
                choices = c(P.Value = "P.Value", FDR = "FDR"), selected = "FDR"
              ),
              actionButton(session$ns("btnUseTopnDEGs4Heatmap"), "Use top DEGs",
                icon = icon("arrow-right"), class = "btn-warning"
              )
            ),
            column(
              6,
              HTML("<b>Input genes [delimited by comma]</b>"),
              textAreaInput(session$ns("inputGenesHm"), NULL, "",
                width = "160%", height = "100px"
              )
            ),
            column(4, uiOutput(session$ns("infoHeatmap")))
          ),
          column(12, tags$hr()),
          column(
            2,
            radioButtons(session$ns("dendrogram"), "Dendrogram",
              choices  = c(Row = "Row", Column = "Column", Both = "Both", None = "None"),
              selected = "Both"
            )
          ),
          column(
            6,
            column(6, checkboxInput(session$ns("chkHeatmapAllSamples"),
              "Include all samples",
              value = FALSE
            )),
            column(6, selectInput(session$ns("palette"), "Palette",
              choices  = c("BlueToRed", "PurpleToRed", "BlueToBrown", "BlueToOrange"),
              selected = "BlueToRed"
            ))
          ),
          column(12, actionButton(session$ns("btnHeatmap"), "Generate Heatmap",
            icon = icon("check"), class = "btn-warning"
          ))
        )
      ))
    })

    heatmapInfoTMM <- reactiveValues(
      genesNotFound = "None",
      validNumGenes = 0,
      topDegText    = "",
      usingTopDEGs  = FALSE
    )

    output$vizHeatmapSettingTMM <- renderUI({
      req(input$chkHeatmap, tables$DEG_TMM)
      fluidRow(box(
        width = 12, title = "Heatmap setting",
        solidHeader = TRUE, closable = FALSE, collapsible = TRUE, status = "primary",
        column(
          12,
          column(
            12,
            column(
              2,
              radioButtons(session$ns("cutoff4HeatmapTMM"), "Minimum Cutoffs",
                choices = c(P.Value = "P.Value", FDR = "FDR"), selected = "FDR"
              ),
              actionButton(session$ns("btnUseTopnDEGs4HeatmapTMM"), "Use top DEGs",
                icon = icon("arrow-right"), class = "btn-warning"
              )
            ),
            column(
              6,
              HTML("<b>Input genes [delimited by comma]</b>"),
              textAreaInput(session$ns("inputGenesHmTMM"), NULL, "",
                width = "160%", height = "100px"
              )
            ),
            column(4, uiOutput(session$ns("infoHeatmapTMM")))
          ),
          column(12, tags$hr()),
          column(
            2,
            radioButtons(session$ns("dendrogramTMM"), "Dendrogram",
              choices  = c(Row = "Row", Column = "Column", Both = "Both", None = "None"),
              selected = "Both"
            )
          ),
          column(
            6,
            column(6, checkboxInput(session$ns("chkHeatmapAllSamplesTMM"),
              "Include all samples",
              value = FALSE
            )),
            column(6, selectInput(session$ns("paletteTMM"), "Palette",
              choices  = c("BlueToRed", "PurpleToRed", "BlueToBrown", "BlueToOrange"),
              selected = "BlueToRed"
            ))
          ),
          column(12, actionButton(session$ns("btnHeatmapTMM"), "Generate Heatmap",
            icon = icon("check"), class = "btn-warning"
          ))
        )
      ))
    })

    output$infoHeatmap <- renderText({
      HTML(paste0(
        "<br/><br/>Input genes [valid]: <b>", heatmapInfo$validNumGenes, "</b><br/>",
        "Input genes [absent]: <b>", paste(heatmapInfo$genesNotFound, collapse = ", "), "</b>"
      ))
    })

    output$infoHeatmapTMM <- renderText({
      HTML(paste0(
        "<br/><br/>Input genes [valid]: <b>", heatmapInfoTMM$validNumGenes, "</b><br/>",
        "Input genes [absent]: <b>", paste(heatmapInfoTMM$genesNotFound, collapse = ", "), "</b>"
      ))
    })

    observeEvent(input$btnUseTopnDEGs4Heatmap, {
      req(tables$DEG, input$cutoff4Heatmap)
      setHeatmapTopGenes("inputGenesHm", heatmapInfo, tables$DEG, input$cutoff4Heatmap)
    })

    observeEvent(input$btnUseTopnDEGs4HeatmapTMM, {
      req(tables$DEG_TMM, input$cutoff4HeatmapTMM)
      setHeatmapTopGenes("inputGenesHmTMM", heatmapInfoTMM, tables$DEG_TMM, input$cutoff4HeatmapTMM)
    })

    observeEvent(input$inputGenesHm,
      {
        currentText <- collapseGeneList(splitGeneList(input$inputGenesHm))
        if (!identical(currentText, heatmapInfo$topDegText)) {
          heatmapInfo$usingTopDEGs <- FALSE
        }
      },
      ignoreInit = TRUE
    )

    observeEvent(input$inputGenesHmTMM,
      {
        currentText <- collapseGeneList(splitGeneList(input$inputGenesHmTMM))
        if (!identical(currentText, heatmapInfoTMM$topDegText)) {
          heatmapInfoTMM$usingTopDEGs <- FALSE
        }
      },
      ignoreInit = TRUE
    )

    observeEvent(
      list(input$inputTopN, input$inputLogFC, input$inputPval, input$inputFDR, input$cutoff4Heatmap),
      {
        req(input$chkHeatmap, tables$DEG, input$cutoff4Heatmap)
        if (isTRUE(heatmapInfo$usingTopDEGs)) {
          setHeatmapTopGenes("inputGenesHm", heatmapInfo, tables$DEG, input$cutoff4Heatmap)
        }
      },
      ignoreInit = TRUE
    )

    observeEvent(
      list(input$inputTopN, input$inputLogFC, input$inputPval, input$inputFDR, input$cutoff4HeatmapTMM),
      {
        req(input$chkHeatmap, tables$DEG_TMM, input$cutoff4HeatmapTMM)
        if (isTRUE(heatmapInfoTMM$usingTopDEGs)) {
          setHeatmapTopGenes("inputGenesHmTMM", heatmapInfoTMM, tables$DEG_TMM, input$cutoff4HeatmapTMM)
        }
      },
      ignoreInit = TRUE
    )

    df_heatmap <- eventReactive(input$btnHeatmap, {
      geneList <- splitGeneList(input$inputGenesHm)
      valid <- intersect(geneList, as.character(tables$DEG$gene))
      invalid <- setdiff(geneList, as.character(tables$DEG$gene))
      heatmapInfo$genesNotFound <- if (length(invalid) > 0) invalid else "None"
      heatmapInfo$validNumGenes <- length(valid)
      if (length(valid) > 0) {
        if (!isTRUE(input$chkHeatmapAllSamples)) {
          res <- tables$DEG
          hm <- res[match(valid, as.character(res$gene)), -c(1:8), drop = FALSE]
          rownames(hm) <- valid
          hm
        } else {
          valid_all <- valid[valid %in% rownames(tables$log2CPM)]
          if (length(valid_all) > 0) tables$log2CPM[valid_all, , drop = FALSE] else NULL
        }
      } else {
        NULL
      }
    })

    df_heatmap_tmm <- eventReactive(input$btnHeatmapTMM, {
      req(tables$DEG_TMM)
      geneList <- splitGeneList(input$inputGenesHmTMM)
      valid <- intersect(geneList, as.character(tables$DEG_TMM$gene))
      invalid <- setdiff(geneList, as.character(tables$DEG_TMM$gene))
      heatmapInfoTMM$genesNotFound <- if (length(invalid) > 0) invalid else "None"
      heatmapInfoTMM$validNumGenes <- length(valid)
      if (length(valid) > 0) {
        if (!isTRUE(input$chkHeatmapAllSamplesTMM)) {
          res <- tables$DEG_TMM
          hm <- res[match(valid, as.character(res$gene)), -c(1:8), drop = FALSE]
          rownames(hm) <- valid
          hm
        } else {
          valid_all <- valid[valid %in% rownames(tables$log2CPM)]
          if (length(valid_all) > 0) tables$log2CPM[valid_all, , drop = FALSE] else NULL
        }
      } else {
        NULL
      }
    })

    #' Render Heatmap Plot
    #'
    #' Executes renderHeatmapPlot.
    #'
    #' @param df_hm Argument for renderHeatmapPlot.
    #' @param outFile Argument for renderHeatmapPlot.
    #' @param outputId Argument for renderHeatmapPlot.
    #' @param hmHeight Argument for renderHeatmapPlot.
    #' @param Rowv Argument for renderHeatmapPlot.
    #' @param Colv Argument for renderHeatmapPlot.
    #' @param colors Argument for renderHeatmapPlot.
    #'
    #' @return Return value produced by renderHeatmapPlot.
    renderHeatmapPlot <- function(df_hm, outFile, outputId, hmHeight, Rowv, Colv, colors) {
      req(df_hm)
      df_hm <- as.data.frame(df_hm)
      row_ids <- rownames(df_hm)
      df_hm <- as.data.frame(lapply(df_hm, function(x) suppressWarnings(as.numeric(as.character(x)))),
        check.names = FALSE
      )
      rownames(df_hm) <- row_ids
      n_bad_hm <- sum(!is.finite(as.matrix(df_hm)), na.rm = TRUE)
      if (n_bad_hm > 0) {
        cat("\nWarning[Visualize Heatmap]: filtering data affected by ",
          n_bad_hm, " non-numeric/NA/Inf value(s).\n",
          sep = ""
        )
      }
      keep_cols <- vapply(df_hm, function(x) any(is.finite(x)), logical(1))
      if (any(!keep_cols)) {
        cat("\nWarning[Visualize Heatmap]: excluding ", sum(!keep_cols),
          " sample column(s) without finite numeric values.\n",
          sep = ""
        )
      }
      df_hm <- df_hm[, keep_cols, drop = FALSE]
      keep_rows <- stats::complete.cases(df_hm)
      if (any(!keep_rows)) {
        cat("\nWarning[Visualize Heatmap]: excluding ", sum(!keep_rows),
          " gene row(s) with non-finite expression values.\n",
          sep = ""
        )
      }
      df_hm <- df_hm[keep_rows, , drop = FALSE]
      req(nrow(df_hm) > 0, ncol(df_hm) > 0)

      # If clustering is disabled, enforce deterministic alphabetical ordering.
      if (!Rowv && nrow(df_hm) > 0) {
        row_idx <- order(rownames(df_hm), na.last = TRUE)
        df_hm <- df_hm[row_idx, , drop = FALSE]
      }
      if (!Colv && ncol(df_hm) > 0) {
        col_idx <- order(colnames(df_hm), na.last = TRUE)
        df_hm <- df_hm[, col_idx, drop = FALSE]
      }

      meta <- tables$meta_all
      idx1 <- intersect(meta$ID, colnames(df_hm))
      idx2 <- intersect(colnames(df_hm), meta$NEWNAME)
      if (length(idx1) >= length(idx2)) {
        rownames(meta) <- meta$ID
        idx <- idx1
      } else {
        rownames(meta) <- meta$NEWNAME
        idx <- idx2
      }
      col_side <- data.frame(GROUP = meta[idx, "GROUP"])
      hmHeights <- heatmapSubplotHeights(
        total_height = max(500, hmHeight),
        Colv = Colv,
        has_col_side = !is.null(col_side) && nrow(col_side) > 0
      )

      hm <- heatmaply(df_hm,
        scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(
          low = tolower(colors[1]), high = tolower(colors[2])
        ),
        Rowv = Rowv, Colv = Colv,
        col_side_colors = col_side,
        margins = c(80, 150, 60, 20),
        fontsize_row = 10,
        fontsize_col = 10,
        height = max(500, hmHeight),
        subplot_heights = hmHeights,
        file = NULL, scale = "row"
      )

      output[[outputId]] <- renderPlotly({
        ggplotly(hm, width = NULL, height = max(500, hmHeight))
      })
      keepOutputActive(outputId)

      # Save heatmap as PDF using base R heatmap
      tryCatch(
        {
          hmData <- as.matrix(df_hm)
          hmData_scaled <- t(scale(t(hmData)))
          pdf(outFile,
            width = max(8, ncol(hmData) * 0.5 + 4),
            height = max(8, nrow(hmData) * 0.2 + 4)
          )
          heatmap(hmData_scaled,
            Rowv = if (Rowv) NULL else NA, Colv = if (Colv) NULL else NA,
            scale = "none", col = colorRampPalette(c(tolower(colors[1]), "white", tolower(colors[2])))(100),
            margins = c(10, 12)
          )
          dev.off()
          cat("[Visualize]: Heatmap PDF saved:", basename(outFile), "\n")
        },
        error = function(e) {
          cat("[Visualize]: Could not save heatmap PDF:", conditionMessage(e), "\n")
        }
      )
    }

    observeEvent(input$btnHeatmap, {
      Rowv <- input$dendrogram %in% c("Row", "Both")
      Colv <- input$dendrogram %in% c("Column", "Both")
      hasTMM <- !is.null(tables$DEG_TMM)

      df_hm <- df_heatmap()
      req(df_hm)
      hmHeight <- heatmapHeightPx(nrow(df_hm))
      output$vizHeatmap1 <- renderUI({
        req(input$chkHeatmap)
        fluidRow(
          box(
            width = 12,
            title = "Heatmap view",
            solidHeader = TRUE,
            closable = FALSE,
            collapsible = FALSE,
            status = "primary",
            plotlyOutput(session$ns("heatmap1"),
              height = paste0(hmHeight, "px")
            )
          )
        )
      })

      colors <- unlist(strsplit(input$palette, "To"))

      # ERCC (or standard) heatmap
      outFile <- if (hasTMM) {
        paste0(tables$prefix, "_ERCC_heatmap.pdf")
      } else {
        paste0(tables$prefix, "_heatmap.pdf")
      }
      renderHeatmapPlot(df_hm, outFile, "heatmap1", hmHeight, Rowv, Colv, colors)
      logIt(tables$logFile, action = "Output", parameter = "DEGs_heatmap", value = basename(outFile))
    })

    observeEvent(input$btnHeatmapTMM, {
      req(tables$DEG_TMM)
      df_hm_tmm <- df_heatmap_tmm()
      req(df_hm_tmm)
      hmHeight <- heatmapHeightPx(nrow(df_hm_tmm))
      Rowv <- input$dendrogramTMM %in% c("Row", "Both")
      Colv <- input$dendrogramTMM %in% c("Column", "Both")

      output$vizHeatmapTMM1 <- renderUI({
        req(input$chkHeatmap, tables$DEG_TMM)
        fluidRow(
          box(
            width = 12, title = "Heatmap view",
            solidHeader = TRUE, closable = FALSE, collapsible = FALSE, status = "primary",
            plotlyOutput(session$ns("heatmap1_tmm"),
              height = paste0(hmHeight, "px")
            )
          )
        )
      })

      colors_tmm <- unlist(strsplit(input$paletteTMM, "To"))
      outFile_tmm <- paste0(tables$prefix, "_TMM_heatmap.pdf")
      renderHeatmapPlot(df_hm_tmm, outFile_tmm, "heatmap1_tmm", hmHeight, Rowv, Colv, colors_tmm)
      logIt(tables$logFile, action = "Output", parameter = "DEGs_heatmap_TMM", value = basename(outFile_tmm))
    })

    # Return selected cutoffs so Download module can log them
    return(list(
      inputLogFC = reactive(input$inputLogFC),
      inputPval  = reactive(input$inputPval),
      inputFDR   = reactive(input$inputFDR),
      inputTopN  = reactive(input$inputTopN)
    ))
  })
}
