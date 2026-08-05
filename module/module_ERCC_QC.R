library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(plotly)
library(ggplot2)

# =============================================================================
# ERCC QC Module  –  Step 2 (ERCC-specific panels)
# UI:     ERCC_QCUI(id)
# Server: ERCC_QCServer(id, tables)
#
# Layout:
#   1. Tab 1: QC plots (library-size boxplots + spike-in level boxplot)
#   2. Tab 2: ERCC QC plot (QC1 table)
#   3. Tab 3: ERCC QC plot 2 (QC2 table)
# =============================================================================

#' ERCC QCUI
#'
#' Executes ERCC_QCUI.
#'
#' @param id Argument for ERCC_QCUI.
#'
#' @return Return value produced by ERCC_QCUI.
ERCC_QCUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("erccQCpanel"))
  )
}

#' ERCC QCServer
#'
#' Executes ERCC_QCServer.
#'
#' @param id Argument for ERCC_QCServer.
#' @param tables Argument for ERCC_QCServer.
#'
#' @return Return value produced by ERCC_QCServer.
ERCC_QCServer <- function(id, tables) {
  moduleServer(id, function(input, output, session) {
    #' Get Ercc Group Colors
    #'
    #' Executes get_ercc_group_colors.
    #'
    #' @param groups Argument for get_ercc_group_colors.
    #'
    #' @return Return value produced by get_ercc_group_colors.
    get_ercc_group_colors <- function(groups) {
      groups <- unique(as.character(groups))
      groups <- groups[!is.na(groups) & nzchar(groups)]
      if (length(groups) == 0) {
        return(setNames(character(0), character(0)))
      }

      map_fun <- get0("themeGroupColorMap", envir = .GlobalEnv, mode = "function")
      if (!is.null(map_fun)) {
        cmap <- map_fun(groups)
        if (!is.null(cmap) && length(cmap) > 0) {
          return(cmap)
        }
      }

      tc <- getOption("shinyRNAseq.theme.colors", list())
      pal <- unique(c(
        tc$primary, tc$info, tc$success, tc$warning, tc$danger,
        "#2FA4E7", "#5bc0de", "#73A839", "#DD5600", "#C71C22"
      ))
      pal <- pal[!is.na(pal) & nzchar(pal)]
      if (length(pal) == 0) pal <- c("#2FA4E7")
      setNames(rep(pal, length.out = length(groups)), groups)
    }

    #' Get Ercc Point Color
    #'
    #' Executes get_ercc_point_color.
    #'
    #' @return Return value produced by get_ercc_point_color.
    get_ercc_point_color <- function() {
      tc <- getOption("shinyRNAseq.theme.colors", list())
      if (!is.null(tc$danger) && nzchar(tc$danger)) tc$danger else "#C71C22"
    }

    output$erccQCpanel <- renderUI({
      req(
        isTRUE(tables$inputValidated), isTRUE(tables$erccDetected),
        tables$counts_ERCC, tables$meta_all, tables$counts
      )
      ns <- session$ns
      decision <- if (is.null(tables$erccDecision)) "pending" else as.character(tables$erccDecision)

      # Compute spike-in plot CSS width from current group count so the
      # plotlyOutput container is constrained rather than filling the full box.
      spike_dat <- tryCatch(erccData(), error = function(e) NULL)
      n_grps <- if (!is.null(spike_dat)) length(unique(spike_dat$stat$GROUP)) else 4L
      spike_w_css <- paste0(max(350L, min(980L, n_grps * 180L + 220L)), "px")

      choiceBox <- fluidRow(
        box(
          width = 12,
          title = "Choose Normalization Strategy",
          solidHeader = TRUE, collapsible = TRUE, status = "primary",
          p("ERCC spike-ins are detected. Choose one normalization method before analysis."),
          actionButton(ns("btnProceedERCC"), "Proceed with ERCC normalization",
            icon = icon("vial"), class = "btn-warning"
          ),
          tags$span(" "),
          actionButton(ns("btnUseTMM"), "Use TMM normalization",
            icon = icon("chart-bar"), class = "btn-warning"
          ),
          tags$hr(),
          tags$p(
            tags$b("Current choice: "),
            if (identical(decision, "ercc")) {
              "ERCC normalization"
            } else if (identical(decision, "tmm")) {
              "TMM normalization"
            } else {
              "Not selected"
            }
          )
        )
      )

      tagList(
        choiceBox,
        fluidRow(
          tabBox(
            width = 12, id = ns("erccQcTabs"),
            title = NULL,
            tabPanel(
              tags$b("Library Stats"),
              fluidRow(
                box(
                  width = 6, title = "Library Size – TMM Normalization",
                  solidHeader = TRUE, collapsible = TRUE, status = "primary",
                  plotlyOutput(ns("libSizeGeneNorm"), height = "550px")
                ),
                box(
                  width = 6, title = "Library Size – ERCC Normalisation",
                  solidHeader = TRUE, collapsible = TRUE, status = "primary",
                  plotlyOutput(ns("libSizeErccNorm"), height = "550px")
                )
              ),
              fluidRow(
                box(
                  width = 12, title = "ERCC Spike-in Level by Group",
                  solidHeader = TRUE, collapsible = TRUE, status = "primary",
                  div(
                    plotlyOutput(ns("spikeInBoxplot"), height = "620px", width = spike_w_css),
                    align = "left"
                  )
                )
              )
            ),
            tabPanel(
              tags$b("PCA Plots"),
              fluidRow(
                box(
                  width = 6, title = "PCA \u2014 TMM Normalization (All Samples)",
                  solidHeader = TRUE, collapsible = TRUE, status = "primary",
                  shinycssloaders::withSpinner(
                    plotlyOutput(ns("erccPCAplot_tmm"), height = "500px", width = "500px")
                  )
                ),
                box(
                  width = 6, title = "PCA \u2014 ERCC Normalization (All Samples)",
                  solidHeader = TRUE, collapsible = TRUE, status = "primary",
                  shinycssloaders::withSpinner(
                    plotlyOutput(ns("erccPCAplot_ercc"), height = "500px", width = "500px")
                  )
                )
              )
            ),
            tabPanel(
              tags$b("ERCC QC Table 1"),
              fluidRow(
                box(
                  width = 12,
                  title = "ERCC QC1 — Level, Correlation & Normalisation Factor",
                  solidHeader = TRUE, collapsible = FALSE, status = "primary",
                  p(
                    "This step calculates ERCC spike-in level, normalisation factor (nf) and",
                    "correlation to check the Dose-Response of ERCC transcripts."
                  ),
                  shinycssloaders::withSpinner(DT::DTOutput(ns("erccStatTable"))),
                  tags$div(
                    style = "margin-top:10px; font-size:13px; color:#555;",
                    tags$b("Note:"),
                    tags$ul(
                      tags$li(
                        tags$b("pct_ERCC:"), "ERCC spike-in percentage (%) should be between",
                        tags$b("0.5% to 5%"), "in normal condition (control group), ideally 1%-2%."
                      ),
                      tags$li(tags$b("nf_ERCC:"), "ERCC normalisation factor used to adjust sample library size during differential analysis."),
                      tags$li(
                        tags$b("ERCC_COR:"), "Pearson correlation between known ERCC concentration and observed counts should be",
                        tags$b("> 0.9"), ", ideally > 0.95."
                      ),
                      tags$li(tags$b("STATUS:"), "PASS if ERCC_COR > 0.9, otherwise WARN.")
                    )
                  )
                )
              )
            ),
            tabPanel(
              tags$b("ERCC QC Table 2"),
              fluidRow(
                box(
                  width = 12,
                  title = "ERCC QC2 — Consistency Across Replicates",
                  solidHeader = TRUE, collapsible = FALSE, status = "primary",
                  p(
                    "Before using ERCCs to normalise between treatment groups, you must ensure",
                    "they are consistent within the same group."
                  ),
                  shinycssloaders::withSpinner(DT::DTOutput(ns("erccCVtable"))),
                  tags$div(
                    style = "margin-top:10px; font-size:13px; color:#555;",
                    tags$b("Note:"),
                    tags$ul(
                      tags$li(tags$b("ERCC_mean:"), "Acceptable range of mean ERCC spike-in ratio should be 0.5%-5% in normal condition (control group), ideally 1%-2%."),
                      tags$li(
                        tags$b("ERCC_cv:"), "Coefficient of Variation (CV%) of ERCC spike-in ratio across biological replicates should be low (typically",
                        tags$b("< 15%"), ")."
                      ),
                      tags$li(tags$b("STATUS:"), "PASS if ERCC_cv < 15%, otherwise WARN.")
                    )
                  )
                )
              )
            )
          )
        )
      )
    })

    # ---- Compute ERCC statistics ----------------------------------------
    erccData <- reactive({
      req(
        isTRUE(tables$inputValidated), isTRUE(tables$erccDetected),
        tables$counts, tables$counts_ERCC, tables$meta_all
      )
      # Keep ERCC QC upstream totals aligned with stdRNAseq ERCC scripts.
      counts <- if (!is.null(tables$counts_unfiltered)) tables$counts_unfiltered else tables$counts
      counts_ERCC <- tables$counts_ERCC
      meta <- tables$meta_all

      if (tolower(colnames(counts)[1]) == "gene") counts <- counts[, -1]
      if (tolower(colnames(counts_ERCC)[1]) == "gene") counts_ERCC <- counts_ERCC[, -1]

      commonIDs <- intersect(meta$ID, colnames(counts))
      counts <- counts[, commonIDs, drop = FALSE]
      counts_ERCC <- counts_ERCC[, commonIDs, drop = FALSE]
      meta <- meta[meta$ID %in% commonIDs, ]

      stat <- CalcERCCnormFactor(counts, counts_ERCC, meta, txtFile = NULL)
      correlations <- ErccQC(counts_ERCC, meta)
      stat$ERCC_COR <- correlations[stat$ID]
      stat$STATUS <- ifelse(!is.na(stat$ERCC_COR) & stat$ERCC_COR > 0.9, "PASS", "WARN")

      cv <- as.data.frame(ErccCV(stat))
      cv$STATUS <- ifelse(cv$ERCC_cv < 15, "PASS", "WARN")

      list(stat = stat, cv = cv)
    })

    observeEvent(input$btnProceedERCC, {
      tables$erccDecision <- "ercc"
      tables$erccMode <- TRUE
    })

    observeEvent(input$btnUseTMM, {
      tables$erccDecision <- "tmm"
      tables$erccMode <- FALSE
    })

    # ---- QC1 Table (per sample) ------------------------------------------
    output$erccStatTable <- DT::renderDT({
      req(erccData())
      dat <- erccData()$stat
      display <- data.frame(
        ID = dat$ID,
        GROUP = dat$GROUP,
        gene_libSize = formatC(dat$gene_libSize, format = "d", big.mark = ","),
        ERCC_libSize = formatC(dat$ERCC_libSize, format = "d", big.mark = ","),
        pct_ERCC = round(dat$pct_ERCC, 7),
        nf_libSize = round(dat$nf_libSize, 7),
        nf_ERCC = round(dat$nf_ERCC, 7),
        ERCC_COR = round(dat$ERCC_COR, 7),
        STATUS = dat$STATUS,
        check.names = FALSE, stringsAsFactors = FALSE
      )
      DT::datatable(
        display,
        rownames = FALSE,
        class = "stripe hover compact nowrap",
        options = list(
          dom = "tip",
          pageLength = 50,
          scrollX = TRUE,
          columnDefs = list(
            list(className = "dt-right", targets = 2:7)
          )
        )
      ) |>
        DT::formatStyle("STATUS",
          color           = DT::styleEqual(c("PASS", "WARN"), c("green", "#DD5600")),
          fontWeight      = "bold"
        )
    })

    # ---- QC2 Table (per group CV) ----------------------------------------
    output$erccCVtable <- DT::renderDT({
      req(erccData())
      cv <- erccData()$cv
      display <- data.frame(
        GROUP = cv$GROUP,
        ERCC_mean = round(cv$ERCC_mean, 7),
        ERCC_sd = round(cv$ERCC_sd, 9),
        ERCC_cv = round(cv$ERCC_cv, 7),
        STATUS = cv$STATUS,
        check.names = FALSE, stringsAsFactors = FALSE
      )
      DT::datatable(
        display,
        rownames = FALSE,
        class = "stripe hover compact nowrap",
        options = list(
          dom = "t",
          pageLength = 50,
          scrollX = TRUE,
          columnDefs = list(
            list(className = "dt-right", targets = 1:3)
          )
        )
      ) |>
        DT::formatStyle("STATUS",
          color      = DT::styleEqual(c("PASS", "WARN"), c("green", "#DD5600")),
          fontWeight = "bold"
        )
    })

    # ---- 2a. Gene-count normalised library size boxplot ------------------
    output$libSizeGeneNorm <- renderPlotly({
      req(erccData())
      dat <- erccData()$stat
      grp_cols <- get_ercc_group_colors(dat$GROUP)
      pt_col <- get_ercc_point_color()
      ptSize <- if (nrow(dat) > 40) 1.2 else if (nrow(dat) > 20) 1.7 else 2.5
      # Effective library size = gene_libSize * nf_libSize
      dat$eff_libSize_gene <- dat$gene_libSize * dat$nf_libSize / 1e6
      dat$label <- paste0(
        dat$ID, "\n", dat$GROUP,
        "\nEff. lib size: ", round(dat$eff_libSize_gene, 1), "M"
      )
      p <- ggplot(dat, aes(
        x = GROUP, y = eff_libSize_gene, fill = GROUP,
        text = label
      )) +
        geom_boxplot(width = 0.5, outlier.shape = NA, color = "black") +
        geom_jitter(width = 0.15, size = ptSize, alpha = 0.7, color = pt_col) +
        scale_fill_manual(values = grp_cols, drop = FALSE) +
        theme_classic(base_size = 14) +
        labs(
          y = "Effective Library Size (millions)", x = NULL,
          title = "TMM Normalization"
        ) +
        theme(
          axis.text.x = element_text(angle = 90, hjust = 1),
          axis.text.y = element_text(size = 10),
          legend.position = "none"
        )
      fig <- ggplotly(p, tooltip = "text")
      config(fig, toImageButtonOptions = list(
        format = "svg",
        filename = "LibSize_gene_norm", height = 550, width = 600
      ))
    })

    # ---- 2b. ERCC normalised library size boxplot -----------------------
    output$libSizeErccNorm <- renderPlotly({
      req(erccData())
      dat <- erccData()$stat
      grp_cols <- get_ercc_group_colors(dat$GROUP)
      pt_col <- get_ercc_point_color()
      ptSize <- if (nrow(dat) > 40) 1.2 else if (nrow(dat) > 20) 1.7 else 2.5
      # Effective library size = gene_libSize * nf_ERCC
      dat$eff_libSize_ercc <- dat$gene_libSize * dat$nf_ERCC / 1e6
      dat$label <- paste0(
        dat$ID, "\n", dat$GROUP,
        "\nEff. lib size: ", round(dat$eff_libSize_ercc, 1), "M"
      )
      p <- ggplot(dat, aes(
        x = GROUP, y = eff_libSize_ercc, fill = GROUP,
        text = label
      )) +
        geom_boxplot(width = 0.5, outlier.shape = NA, color = "black") +
        geom_jitter(width = 0.15, size = ptSize, alpha = 0.7, color = pt_col) +
        scale_fill_manual(values = grp_cols, drop = FALSE) +
        theme_classic(base_size = 14) +
        labs(
          y = "Effective Library Size (millions)", x = NULL,
          title = "ERCC Normalisation"
        ) +
        theme(
          axis.text.x = element_text(angle = 90, hjust = 1),
          axis.text.y = element_text(size = 10),
          legend.position = "none"
        )
      fig <- ggplotly(p, tooltip = "text")
      config(fig, toImageButtonOptions = list(
        format = "svg",
        filename = "LibSize_ERCC_norm", height = 550, width = 600
      ))
    })

    # ---- ERCC-normalized log2CPM reactive --------------------------------
    erccNormLog2CPM <- reactive({
      req(tables$counts, tables$counts_ERCC, tables$meta_all)
      counts <- as.data.frame(tables$counts)
      counts_ercc <- as.data.frame(tables$counts_ERCC)
      if (ncol(counts) > 0 && tolower(colnames(counts)[1]) == "gene") counts <- counts[, -1, drop = FALSE]
      if (ncol(counts_ercc) > 0 && tolower(colnames(counts_ercc)[1]) == "gene") counts_ercc <- counts_ercc[, -1, drop = FALSE]
      meta <- tables$meta_all
      common_ids <- intersect(meta$ID, intersect(colnames(counts), colnames(counts_ercc)))
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
    })

    # ---- PCA – TMM (all samples) -----------------------------------------
    output$erccPCAplot_tmm <- renderPlotly({
      req(tables$norm.data, tables$meta_all, isTRUE(tables$inputValidated))
      fig <- ggplotly(
        PCA2d(
          dat = tables$norm.data, meta = tables$meta_all,
          scale = TRUE, topn = 3000, label = FALSE
        ),
        width = 500, height = 500, tooltip = "text"
      )
      config(fig, toImageButtonOptions = list(
        format = "svg", filename = "QC_PCA_TMM", height = 500, width = 500, scale = 1
      ))
    })

    # ---- PCA – ERCC normalization (all samples) --------------------------
    output$erccPCAplot_ercc <- renderPlotly({
      req(erccNormLog2CPM(), tables$meta_all, isTRUE(tables$inputValidated))
      fig <- ggplotly(
        PCA2d(
          dat = erccNormLog2CPM(), meta = tables$meta_all,
          scale = TRUE, topn = 3000, label = FALSE
        ),
        width = 500, height = 500, tooltip = "text"
      )
      config(fig, toImageButtonOptions = list(
        format = "svg", filename = "QC_PCA_ERCC", height = 500, width = 500, scale = 1
      ))
    })

    # ---- 3. Spike-in level boxplot (pct_ERCC per group) -----------------
    output$spikeInBoxplot <- renderPlotly({
      req(erccData())
      dat <- erccData()$stat
      grp_cols <- get_ercc_group_colors(dat$GROUP)
      pt_col <- get_ercc_point_color()
      ptSize <- if (nrow(dat) > 40) 1.2 else if (nrow(dat) > 20) 1.7 else 2.5
      n_groups <- length(unique(dat$GROUP))
      spike_w <- max(350L, min(980L, n_groups * 180L + 220L))
      dat$label <- paste0(
        dat$ID, "\n", dat$GROUP,
        "\nSpike-in: ", round(dat$pct_ERCC, 3), "%"
      )
      p <- ggplot(dat, aes(x = GROUP, y = pct_ERCC, fill = GROUP, text = label)) +
        geom_boxplot(width = 0.5, outlier.shape = NA, color = "black") +
        geom_jitter(width = 0.15, size = ptSize, alpha = 0.7, color = pt_col) +
        scale_fill_manual(values = grp_cols, drop = FALSE) +
        theme_classic(base_size = 14) +
        labs(
          y = "Spike-in Level\n(ERCC_libSize / gene_libSize × 100)", x = NULL,
          title = "ERCC Spike-in Ratio by Group"
        ) +
        theme(
          axis.text.x = element_text(angle = 90, hjust = 1),
          axis.title.y = element_text(size = 10.5, lineheight = 0.95),
          axis.text.y = element_text(size = 10),
          legend.position = "none"
        )
      fig <- ggplotly(p, width = spike_w, height = 620, tooltip = "text")
      config(fig, toImageButtonOptions = list(
        format = "svg",
        filename = "ERCC_spikein_boxplot", height = 620, width = spike_w
      ))
    })

    # ---- Store in tables for downstream modules -------------------------
    observe({
      req(erccData())
      tables$erccStat <- erccData()$stat
      tables$erccCV <- erccData()$cv

      # Persist ERCC QC outputs to the current analysis directory when possible.
      try(
        {
          if (!is.null(tables$prefix) && nzchar(tables$prefix)) {
            ercc_log2cpm <- tryCatch(erccNormLog2CPM(), error = function(e) NULL)
            saveErccQcBundle(
              stat = erccData()$stat,
              cv = erccData()$cv,
              shift = NULL,
              tmmLogCPM = tables$norm.data,
              erccLogCPM = ercc_log2cpm,
              meta = tables$meta_all,
              outPrefix = tables$prefix
            )
          }
        },
        silent = TRUE
      )
    })
  })
}
