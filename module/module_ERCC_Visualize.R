library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(plotly)
library(ggplot2)
library(DT)

# =============================================================================
# ERCC Visualize Module  –  Step 4 (ERCC-specific visualisation panels)
# UI:     ERCC_VisualizeUI(id)
# Server: ERCC_VisualizeServer(id, tables)
# =============================================================================

#' ERCC Visualize UI
#'
#' Executes ERCC_VisualizeUI.
#'
#' @param id Argument for ERCC_VisualizeUI.
#'
#' @return Return value produced by ERCC_VisualizeUI.
ERCC_VisualizeUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("erccVizPanel"))
  )
}

#' ERCC Visualize Server
#'
#' Executes ERCC_VisualizeServer.
#'
#' @param id Argument for ERCC_VisualizeServer.
#' @param tables Argument for ERCC_VisualizeServer.
#'
#' @return Return value produced by ERCC_VisualizeServer.
ERCC_VisualizeServer <- function(id, tables) {
  moduleServer(id, function(input, output, session) {
    output$erccVizPanel <- renderUI({
      req(isTRUE(tables$erccMode), tables$erccShift, tables$erccStat_DE)
      ns <- session$ns
      compName <- as.character(tables$erccShift$comp)
      tagList(
        fluidRow(
          box(
            width = 12,
            title = paste0("ERCC QC Summary — ", compName),
            solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, status = "primary",
            tabsetPanel(
              id = ns("erccQCSummaryTabs"),
              tabPanel(
                tags$b("Library Stats"),
                fluidRow(
                  column(
                    4,
                    h4("Library Size: TMM"),
                    plotlyOutput(ns("tmmLibSizeBoxplot"), height = "500px")
                  ),
                  column(
                    4,
                    h4("Library Size: ERCC"),
                    plotlyOutput(ns("erccLibSizeBoxplot"), height = "500px")
                  ),
                  column(
                    4,
                    h4("Shift Test"),
                    DT::DTOutput(ns("erccShiftSummary")),
                    br(),
                    tags$div(
                      style = "font-size: 12px; color: #555;",
                      tags$strong("Criteria for Robust Global Shift:"),
                      tags$ul(
                        tags$li(
                          tags$b("SHIFT:"), "PASS when both AUC and Shift_FC indicate a robust directional shift: ",
                          "AUC >= 0.9 with Shift_FC >= 1.5, or AUC <= 0.1 with Shift_FC <= 0.667."
                        ),
                        tags$li(tags$b("AUC:"), "Rank-based separation of ERCC spike-in ratios between group1 and group2. AUC >= 0.9 supports a forward shift; AUC <= 0.1 supports an opposite-direction shift."),
                        tags$li(tags$b("Shift_FC:"), "Fold change of mean ERCC spike-in ratio between the two groups."),
                        tags$li(tags$b("Shift_PVAL:"), "Two-sample t-test P value for group mean difference; report as supportive evidence."),
                        tags$li(tags$b("group_CV:"), "Coefficient of Variation (CV%) of ERCC spike-in ratio across biological replicates should be low (< 15%) for stable within-group behavior.")
                      )
                    )
                  )
                )
              ),
              tabPanel(
                tags$b("QC1"),
                tags$div(
                  style = "padding-top: 12px;",
                  p(
                    "This step calculates ERCC spike-in level, normalisation factor (nf) and",
                    "correlation to check the Dose-Response of ERCC transcripts."
                  ),
                  shinycssloaders::withSpinner(DT::DTOutput(ns("erccQC1table"))),
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
              ),
              tabPanel(
                tags$b("QC2"),
                tags$div(
                  style = "padding-top: 12px;",
                  p(
                    "Before using ERCCs to normalise between treatment groups, you must ensure",
                    "they are consistent within the same group."
                  ),
                  shinycssloaders::withSpinner(DT::DTOutput(ns("erccQC2table"))),
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

    # ---- QC1 Table (per sample, comparison only) ------------------------
    output$erccQC1table <- DT::renderDT({
      req(tables$erccStat_DE)
      dat <- tables$erccStat_DE
      dat$STATUS <- ifelse(!is.na(dat$ERCC_COR) & dat$ERCC_COR > 0.9, "PASS", "WARN")
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
          dom = "t", pageLength = 50, scrollX = TRUE,
          columnDefs = list(list(className = "dt-right", targets = 2:7))
        )
      ) |>
        DT::formatStyle("STATUS",
          color = DT::styleEqual(c("PASS", "WARN"), c("green", "#DD5600")),
          fontWeight = "bold"
        )
    })

    # ---- QC2 Table (per group CV, comparison only) ----------------------
    output$erccQC2table <- DT::renderDT({
      req(tables$erccStat_DE)
      cv <- as.data.frame(ErccCV(tables$erccStat_DE))
      cv$STATUS <- ifelse(cv$ERCC_cv < 15, "PASS", "WARN")
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
          dom = "t", pageLength = 50, scrollX = TRUE,
          columnDefs = list(list(className = "dt-right", targets = 1:3))
        )
      ) |>
        DT::formatStyle("STATUS",
          color = DT::styleEqual(c("PASS", "WARN"), c("green", "#DD5600")),
          fontWeight = "bold"
        )
    })

    # Shift test summary
    output$erccShiftSummary <- DT::renderDT({
      req(tables$erccShift)
      shift <- tables$erccShift
      df <- data.frame(
        Metric = c(
          "SHIFT", "AUC", "Fold Shift", "P-value",
          "CV% (Group 1)", "CV% (Group 2)",
          "Shift"
        ),
        Value = c(
          as.character(shift$SHIFT),
          round(shift$AUC, 4),
          round(shift$Shift_FC, 4),
          format.pval(shift$Shift_PVAL, digits = 3),
          round(shift$CV_group1, 2),
          round(shift$CV_group2, 2),
          as.character(shift$SHIFT)
        ),
        stringsAsFactors = FALSE
      )
      DT::datatable(df,
        rownames = FALSE, class = "display compact",
        options = list(dom = "t")
      )
    })

    #' Build Lib Size Plot
    #'
    #' Executes buildLibSizePlot.
    #'
    #' @param method_name Argument for buildLibSizePlot.
    #' @param output_filename Argument for buildLibSizePlot.
    #'
    #' @return Return value produced by buildLibSizePlot.
    buildLibSizePlot <- function(method_name, output_filename) {
      req(tables$erccStat_DE)
      dat <- tables$erccStat_DE
      if (is.null(dat) || !is.data.frame(dat)) {
        return(NULL)
      }

      eff_lib <- if (identical(method_name, "ERCC")) {
        dat$gene_libSize * dat$nf_ERCC / 1e6
      } else {
        dat$gene_libSize * dat$nf_libSize / 1e6
      }

      df <- data.frame(
        ID = dat$ID,
        GROUP = dat$GROUP,
        Method = method_name,
        EffLibSize = eff_lib,
        stringsAsFactors = FALSE
      )
      df$GROUP <- factor(df$GROUP, levels = unique(dat$GROUP))
      df$label <- paste0(
        df$ID, "\nGroup: ", df$GROUP,
        "\nMethod: ", df$Method,
        "\nEff. lib size: ", round(df$EffLibSize, 1), "M"
      )

      theme_cols <- getOption("shinyRNAseq.theme.colors", list())
      groups_ordered <- levels(df$GROUP)
      base_pal <- c(
        if (!is.null(theme_cols$primary)) theme_cols$primary else "#2c7bb6",
        if (!is.null(theme_cols$success)) theme_cols$success else "#73A839",
        if (!is.null(theme_cols$warning)) theme_cols$warning else "#DD5600",
        if (!is.null(theme_cols$danger)) theme_cols$danger else "#C71C22",
        if (!is.null(theme_cols$info)) theme_cols$info else "#5bc0de"
      )
      group_colors <- setNames(rep_len(base_pal, length(groups_ordered)), groups_ordered)

      p <- ggplot(df, aes(x = GROUP, y = EffLibSize, fill = GROUP, text = label)) +
        geom_boxplot(width = 0.5, outlier.shape = NA, color = "black") +
        geom_jitter(width = 0.15, size = 2, alpha = 0.7, color = "black") +
        scale_fill_manual(values = group_colors) +
        theme_classic(base_size = 14) +
        labs(y = "Effective Library Size (millions)", x = "GROUP") +
        theme(
          axis.text.x = element_text(angle = 90, hjust = 1),
          axis.text.y = element_text(size = 10),
          legend.position = "none",
          strip.text = element_text(size = 14, face = "bold")
        )
      fig <- ggplotly(p, tooltip = "text")
      config(fig, toImageButtonOptions = list(
        format = "svg",
        filename = output_filename, height = 500, width = 600
      ))
    }

    output$tmmLibSizeBoxplot <- renderPlotly({
      buildLibSizePlot(method_name = "TMM", output_filename = "TMM_LibSize")
    })

    # ERCC library size normalization boxplot
    output$erccLibSizeBoxplot <- renderPlotly({
      buildLibSizePlot(method_name = "ERCC", output_filename = "ERCC_LibSize")
    })
  })
}
