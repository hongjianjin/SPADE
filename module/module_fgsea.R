library(shiny)
library(rtracklayer)
library(dplyr)
library(fgsea)
library(data.table)
library(shinyWidgets)
library(openxlsx)
# library(plotly)


#' Shiny Msg Box
#'
#' Executes ShinyMsgBox.
#'
#' @param title Argument for ShinyMsgBox.
#' @param text Argument for ShinyMsgBox.
#' @param size Argument for ShinyMsgBox.
#' @param easyClose Argument for ShinyMsgBox.
#' @param logicClose Argument for ShinyMsgBox.
#' @param timer Argument for ShinyMsgBox.
#'
#' @return Return value produced by ShinyMsgBox.
ShinyMsgBox <- function(title = "Running analysis", text = "Processing, please wait...", size = "m", easyClose = FALSE, logicClose = TRUE, timer = 10) {
  # size:    adjusts modal size as needed # m, s,l,xl
  # easyClose: controls whether the modal dialog can be dismissed by clicking outside the dialog or by pressing the Escape key.
  # logicClose: controls whether the modal dialog can be dismissed by an external logical value
  # timer:  controls whether the modal dialog can be dismissed by a timer by second.

  showModal(modalDialog(
    title = tagList(icon("hourglass"), title),
    size = size, # Adjust size as needed # m, s,l,xl
    easyClose = easyClose,
    footer = NULL, # No footer
    div(
      # Your modal content here
      h4(text),
      # p(text)
    ),
    tags$div(class = "elapsed-time", "Loading time..."),
    tags$script(HTML("(function() {
      if (window.__fgseaElapsedTimer) {
        clearInterval(window.__fgseaElapsedTimer);
      }

      var openedAt = Date.now();
      var tick = function() {
        var nodes = document.querySelectorAll('.modal .elapsed-time');
        var elapsedDiv = nodes.length ? nodes[nodes.length - 1] : null;
        if (!elapsedDiv) {
          clearInterval(window.__fgseaElapsedTimer);
          window.__fgseaElapsedTimer = null;
          return;
        }

        var elapsedSeconds = Math.floor((Date.now() - openedAt) / 1000);
        var mins = Math.floor(elapsedSeconds / 60);
        var secs = elapsedSeconds % 60;
        var secLabel = (secs === 1) ? 'second' : 'seconds';

        if (mins > 0) {
          elapsedDiv.textContent = 'Elapsed time: ' + mins + ' min ' + secs + ' ' + secLabel;
        } else {
          elapsedDiv.textContent = 'Elapsed time: ' + elapsedSeconds + ' seconds';
        }
      };

      tick();
      window.__fgseaElapsedTimer = setInterval(tick, 1000);
    })();"))
  ))

  introModal_opened_at <- as.numeric(Sys.time())
  is_introModal_open <- reactiveVal(TRUE)

  selfDestructiveObserver <- observe({
    invalidateLater(500)
    # removes introModal after given seconds only if it is still existing and logicClose is TRUE
    if (is_introModal_open() && logicClose && as.numeric(Sys.time()) > introModal_opened_at + timer) {
      removeModal()
      selfDestructiveObserver$destroy()
    }
  })
}

msigdb_choices <- c(
  "Hallmark (H)" = "H",
  "Reactome Pathways (C2:CP:REACTOME)" = "C2:CP:REACTOME",
  "KEGG Legacy Pathways (C2:CP:KEGG_LEGACY)" = "C2:CP:KEGG_LEGACY",
  "Gene Ontology: Biological Process (C5:GO:BP)" = "C5:GO:BP",
  "Gene Ontology: Molecular Function (C5:GO:MF)" = "C5:GO:MF",
  "Gene Ontology: Cellular Compartment (C5:GO:CC)" = "C5:GO:CC",
  "Oncogenic Signatures (C6)" = "C6",
  "Immunologic Signatures (C7)" = "C7",
  "Cell-type Signatures (C8)" = "C8"
)

#' Mod Fgsea Ui
#'
#' Executes mod_fgsea_ui.
#'
#' @param id Argument for mod_fgsea_ui.
#'
#' @return Return value produced by mod_fgsea_ui.
mod_fgsea_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      box(
        id = ns("fgsea_run_box"),
        title = "Run fGSEA",
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        collapsible = TRUE,
        collapsed = FALSE,
        # Ranking metric selector
        radioButtons(
          inputId = ns("rank_type"),
          label = "Ranking metric:",
          choices = c(
            "t-statistic" = "t",
            "sign(log2FC) × −log10(P-value)" = "sign.log2FCxNegLog10Pval"
          ),
          selected = "t",
          inline = TRUE
        ),
        tabBox(
          id = ns("gmt_source_tab"),
          width = 12,
          title = "Gene Set Source",
          tabPanel(
            title = tags$b("Standard"),
            value = "standard",
            fluidRow(
              column(
                6,
                tags$div(
                  style = "padding-right: 6px;",
                  checkboxGroupInput(
                    inputId = ns("msigdb_selection"),
                    label = "Select standard MSigDB gene sets",
                    choices = msigdb_choices,
                    selected = "H"
                  ),
                  pickerInput(
                    inputId = ns("other_msigdb_selection"),
                    label = "Other MsigDB database",
                    choices = character(0),
                    selected = character(0),
                    multiple = TRUE,
                    options = list(
                      `actions-box` = TRUE,
                      `live-search` = TRUE,
                      `selected-text-format` = "count > 0",
                      `count-selected-text` = "{0} of {1} selected",
                      `auto-close` = FALSE,
                      `container` = "body"
                    )
                  ),
                  tags$br(),
                  tags$br(),
                  tags$div(
                    style = "font-size: 13px; color: #555; margin-top: 8px;",
                    "Numbers in parentheses indicate the total number of terms in that database."
                  )
                )
              ),
              column(
                6,
                tags$div(
                  style = "padding-left: 6px;",
                  tableOutput(ns("other_msigdb_reference"))
                )
              )
            )
          ),
          tabPanel(
            title = tags$b("Custom GMT"),
            value = "custom",
            fluidRow(
              column(
                6,
                fileInput(ns("gmt_file"), "Upload .gmt file", accept = ".gmt"),
                textInput(ns("gmt_name"), "Save as name (optional)", value = "custom_gene_sets"),
                actionButton(ns("save_gmt"), "Save Locally", icon = icon("save"), class = "btn-warning")
              ),
              column(
                6,
                selectInput(ns("custom_gmt_selection"), "Select saved GMT", choices = NULL),
                tags$div(style = "margin-top: 8px;"),
                tags$b("GMT Status"),
                verbatimTextOutput(ns("gmt_status"))
              )
            )
          )
        ),
        actionButton(ns("run_button"), "Run fGSEA", icon = icon("play"), class = "btn-warning"),
        tags$div(style = "margin-bottom: 12px;"),
        uiOutput(ns("fgsea_run_stats_ui"))
      )
    ),
    fluidRow(
      box(
        id = ns("fgsea_plot_settings_box"),
        title = "Plot Settings",
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        collapsible = TRUE,
        collapsed = TRUE,
        fluidRow(
          column(
            6,
            pickerInput(
              inputId = ns("plot_select_db"),
              label = "Select/Deselect MSigDB(s) to plot",
              choices = msigdb_choices,
              selected = unname(msigdb_choices),
              multiple = TRUE,
              options = list(
                `actions-box` = TRUE, `live-search` = TRUE,
                `selected-text-format` = "count > 0",
                `count-selected-text` = "{0} of {1} selected",
                `auto-close` = FALSE,
                `container` = "body"
              )
            )
          ),
          column(4, numericInput(ns("top_n"), "Top N Pathways per Direction:", 10, min = 5, max = 25)),
          column(
            8,
            tags$b("Generate Plot Types & select P-value Metrics:"),
            tags$div(
              style = "display: flex; gap: 24px; align-items: flex-start; flex-wrap: wrap; margin-top: 4px;",
              tags$div(
                checkboxInput(ns("plot_type_dot"), "Dot Plot", value = TRUE),
                tags$div(
                  style = "margin-left: 20px; margin-top: -8px;",
                  radioButtons(ns("dot_pval_type"),
                    label = NULL,
                    choices = c("-log10(Pval)" = "logPval", "-log10(Padj)" = "logPadj"),
                    selected = "logPval", inline = FALSE
                  )
                )
              ),
              tags$div(
                checkboxInput(ns("plot_type_bar"), "Bar Plot", value = TRUE),
                tags$div(
                  style = "margin-left: 20px; margin-top: -8px;",
                  radioButtons(ns("bar_pval_type"),
                    label = NULL,
                    choices = c("-log10(Pval)" = "logPval", "-log10(Padj)" = "logPadj"),
                    selected = "logPval", inline = FALSE
                  )
                )
              )
            )
          )
        ),
        fluidRow(
          column(
            12,
            tags$div(
              style = "font-size: 13px; color: #555; margin-top: 4px; margin-bottom: 12px;",
              tags$strong("Ranking: "), "by",
              tags$em("NES"),
            )
          )
        ),
        fluidRow(
          column(
            4,
            actionButton(ns("generate_plot_button"), "Generate Plot", class = "btn-warning")
          )
        ),
        tags$div(style = "margin-top: 6px;")
      )
    ),
    fluidRow(
      box(
        id = ns("fgsea_plots_box"),
        title = "fGSEA Plots",
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        collapsible = TRUE,
        collapsed = TRUE,
        fluidRow(
          column(12,
            style = "display: flex; align-items: center; gap: 10px; flex-wrap: wrap;",
            radioButtons(ns("plot_display"), "Display Plot Type:",
              choices = c("Dot Plot" = "dot", "Bar Plot" = "bar"),
              selected = "dot", inline = TRUE
            ),
            div(
              style = "margin-top: 25px; max-width: 600px; ",
              uiOutput(ns("plot_display_desc"))
            )
          )
        ),
        uiOutput(ns("plot_result_tabs")),
        uiOutput(ns("plots_ui")),
        tags$hr(),
        tags$b("Results Table Preview"),
        DT::DTOutput(ns("fgsea_table_preview"))
      )
    )
  )
}


#' Mod Fgsea Server
#'
#' Executes mod_fgsea_server.
#'
#' @param id Argument for mod_fgsea_server.
#' @param tables Argument for mod_fgsea_server.
#' @param run_fgsea Argument for mod_fgsea_server.
#'
#' @return Return value produced by mod_fgsea_server.
mod_fgsea_server <- function(id, tables, run_fgsea) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    raw_fgsea_results_rv <- reactiveVal(NULL)
    plot_objects_rv <- reactiveVal(NULL) # list keyed by "result_key__plottype"
    plot_heights_rv <- reactiveVal(list())
    plot_widths_rv <- reactiveVal(list())
    generated_result_keys_rv <- reactiveVal(character(0))
    fgsea_run_stats_rv <- reactiveVal(NULL)
    proceed_with_fgsea <- reactiveVal(FALSE) # Track user confirmation for large DB selections
    fgsea_db_term_counts_rv <- reactiveVal(setNames(integer(0), character(0)))

    #' Has Text
    #'
    #' Executes hasText.
    #'
    #' @param x Argument for hasText.
    #'
    #' @return Return value produced by hasText.
    hasText <- function(x) {
      !is.null(x) && length(x) > 0 && !is.na(x[[1]]) && nzchar(as.character(x[[1]]))
    }

    #' Box Collapsed
    #'
    #' Executes boxCollapsed.
    #'
    #' @param id Argument for boxCollapsed.
    #' @param default Argument for boxCollapsed.
    #'
    #' @return Return value produced by boxCollapsed.
    boxCollapsed <- function(id, default = FALSE) {
      state <- input[[id]]
      if (is.null(state) || is.null(state$collapsed)) {
        return(default)
      }
      isTRUE(state$collapsed)
    }

    #' Expand Box
    #'
    #' Executes expandBox.
    #'
    #' @param id Argument for expandBox.
    #' @param default Argument for expandBox.
    #'
    #' @return Return value produced by expandBox.
    expandBox <- function(id, default = TRUE) {
      if (boxCollapsed(id, default = default)) {
        shinydashboardPlus::updateBox(id, action = "toggle", session = session)
      }
    }

    #' Collapse Box
    #'
    #' Executes collapseBox.
    #'
    #' @param id Argument for collapseBox.
    #' @param default Argument for collapseBox.
    #'
    #' @return Return value produced by collapseBox.
    collapseBox <- function(id, default = FALSE) {
      if (!boxCollapsed(id, default = default)) {
        shinydashboardPlus::updateBox(id, action = "toggle", session = session)
      }
    }

    #' Active Output Dirs
    #'
    #' Executes active_output_dirs.
    #'
    #' @return Return value produced by active_output_dirs.
    active_output_dirs <- function() {
      if (!is.null(tables$currentOutDir) &&
        nzchar(as.character(tables$currentOutDir)) &&
        dir.exists(tables$currentOutDir)) {
        return(as.character(tables$currentOutDir))
      }
      if (!is.null(tables$prefix) && nzchar(as.character(tables$prefix))) {
        d <- dirname(as.character(tables$prefix))
        if (dir.exists(d)) {
          return(d)
        }
      }
      if (!is.null(tables$outdirs)) {
        d <- unlist(tables$outdirs, use.names = FALSE)
        d <- d[!is.na(d) & nzchar(d) & file.exists(d)]
        if (length(d) > 0) {
          return(tail(d, 1))
        }
      }
      character(0)
    }

    #' First Nonempty
    #'
    #' Executes first_nonempty.
    #'
    #' @param v Argument for first_nonempty.
    #'
    #' @return Return value produced by first_nonempty.
    first_nonempty <- function(v) {
      v <- as.character(v)
      v <- trimws(v[!is.na(v) & nzchar(v)])
      if (length(v) == 0) {
        return(NA_character_)
      }
      v[1]
    }

    #' Strip Trailing Code
    #'
    #' Executes strip_trailing_code.
    #'
    #' @param label Argument for strip_trailing_code.
    #'
    #' @return Return value produced by strip_trailing_code.
    strip_trailing_code <- function(label) {
      trimws(sub("\\s*\\([^)]*\\)\\s*$", "", as.character(label), perl = TRUE))
    }

    #' Extract Msigdb Term Counts
    #'
    #' Executes extract_msigdb_term_counts.
    #'
    #' @param cols_df Argument for extract_msigdb_term_counts.
    #'
    #' @return Return value produced by extract_msigdb_term_counts.
    extract_msigdb_term_counts <- function(cols_df) {
      if (is.null(cols_df) || !is.data.frame(cols_df) || nrow(cols_df) == 0) {
        return(setNames(integer(0), character(0)))
      }

      built_key <- ifelse(
        is.na(cols_df$gs_subcollection) | cols_df$gs_subcollection == "",
        cols_df$gs_collection,
        paste(cols_df$gs_collection, cols_df$gs_subcollection, sep = ":")
      )

      candidate_cols <- c("num_genesets", "num_gene_sets", "numGenesets", "num_terms", "numTerms")
      count_col <- candidate_cols[candidate_cols %in% colnames(cols_df)][1]
      if (is.na(count_col) || !nzchar(count_col)) {
        low_cols <- tolower(colnames(cols_df))
        idx <- grep("num.*(geneset|term|pathway)|(geneset|term|pathway).*num", low_cols)
        if (length(idx) > 0) {
          count_col <- colnames(cols_df)[idx[1]]
        }
      }
      if (is.na(count_col) || !nzchar(count_col)) {
        return(setNames(integer(0), character(0)))
      }

      counts <- suppressWarnings(as.integer(cols_df[[count_col]]))
      keep <- !is.na(built_key) & nzchar(built_key) & !is.na(counts) & counts > 0
      if (!any(keep)) {
        return(setNames(integer(0), character(0)))
      }

      keys <- as.character(built_key[keep])
      vals <- counts[keep]
      agg <- tapply(vals, keys, max)
      as.integer_map <- as.integer(agg)
      names(as.integer_map) <- names(agg)
      as.integer_map
    }

    #' Db Label with Count
    #'
    #' Executes db_label_with_count.
    #'
    #' @param label Argument for db_label_with_count.
    #' @param db_key Argument for db_label_with_count.
    #' @param count_map Argument for db_label_with_count.
    #'
    #' @return Return value produced by db_label_with_count.
    db_label_with_count <- function(label, db_key, count_map) {
      cnt <- suppressWarnings(as.integer(count_map[db_key]))
      if (!is.na(cnt) && cnt > 0) {
        return(paste0(label, " (", format(cnt, big.mark = ","), ")"))
      }
      label
    }

    #' Pretty Msigdb Label
    #'
    #' Executes pretty_msigdb_label.
    #'
    #' @param key Argument for pretty_msigdb_label.
    #' @param matching Argument for pretty_msigdb_label.
    #'
    #' @return Return value produced by pretty_msigdb_label.
    pretty_msigdb_label <- function(key, matching) {
      desc <- if ("gs_description" %in% colnames(matching)) first_nonempty(matching$gs_description) else NA_character_
      if (!is.na(desc) && !identical(desc, key)) {
        return(desc)
      }

      coll_name <- if ("gs_collection_name" %in% colnames(matching)) first_nonempty(matching$gs_collection_name) else NA_character_
      sub_name <- if ("gs_subcollection_name" %in% colnames(matching)) first_nonempty(matching$gs_subcollection_name) else NA_character_
      if (!is.na(coll_name) && !is.na(sub_name)) {
        return(paste(coll_name, sub_name, sep = " - "))
      }
      if (!is.na(coll_name)) {
        return(coll_name)
      }

      parts <- strsplit(key, ":", fixed = TRUE)[[1]]
      coll_map <- c(
        "H" = "Hallmark",
        "C1" = "Positional Gene Sets",
        "C2" = "Curated Gene Sets",
        "C3" = "Regulatory Target Gene Sets",
        "C4" = "Computational Gene Sets",
        "C5" = "Gene Ontology Gene Sets",
        "C6" = "Oncogenic Signatures",
        "C7" = "Immunologic Signatures",
        "C8" = "Cell Type Signature Gene Sets"
      )
      coll_pretty <- if (!is.na(coll_map[parts[1]])) unname(coll_map[parts[1]]) else parts[1]
      if (length(parts) > 1) {
        sub_pretty <- gsub("_", " ", paste(parts[-1], collapse = " : "), fixed = TRUE)
        return(paste(coll_pretty, sub_pretty, sep = " - "))
      }
      coll_pretty
    }

    #' Fgsea Db Long Label
    #'
    #' Executes fgsea_db_long_label.
    #'
    #' @param db_key Argument for fgsea_db_long_label.
    #'
    #' @return Return value produced by fgsea_db_long_label.
    fgsea_db_long_label <- function(db_key) {
      std_idx <- match(db_key, unname(msigdb_choices))
      if (!is.na(std_idx)) {
        std_label <- names(msigdb_choices)[std_idx]
        std_label <- trimws(sub("\\s*\\([^)]*\\)\\s*$", "", std_label, perl = TRUE))
        return(std_label)
      }

      if (requireNamespace("msigdbr", quietly = TRUE)) {
        cols <- tryCatch(msigdbr::msigdbr_collections(), error = function(e) NULL)
        if (!is.null(cols) && nrow(cols) > 0) {
          cols$built_key <- ifelse(
            is.na(cols$gs_subcollection) | cols$gs_subcollection == "",
            cols$gs_collection,
            paste(cols$gs_collection, cols$gs_subcollection, sep = ":")
          )
          matching <- cols[cols$built_key == db_key, , drop = FALSE]
          if (nrow(matching) > 0) {
            return(as.character(pretty_msigdb_label(db_key, matching)))
          }
        }
      }

      gsub("_", " ", gsub(":", " - ", db_key, fixed = TRUE), fixed = TRUE)
    }

    current_rnk_files <- reactive({
      req(!is.null(tables$currentOutDir) || !is.null(tables$prefix) || !is.null(tables$outdirs))
      rank_type <- input$rank_type
      req(rank_type)
      current_comp <- tables$comp
      req(hasText(current_comp))

      outdirs <- active_output_dirs()
      if (length(outdirs) == 0) {
        return(character(0))
      }

      pattern <- paste0("_", rank_type, "\\.rnk$")
      rnk_files <- list.files(outdirs,
        pattern = pattern,
        full.names = TRUE, recursive = TRUE
      )
      rnk_files <- rnk_files[vapply(rnk_files, function(p) {
        d <- basename(dirname(p))
        f <- basename(p)
        identical(d, current_comp) || grepl(current_comp, f, fixed = TRUE)
      }, logical(1))]

      unique(rnk_files)
    })

    #' Flash Message
    #'
    #' Executes flashMessage.
    #'
    #' @param title Argument for flashMessage.
    #' @param text Argument for flashMessage.
    #' @param type Argument for flashMessage.
    #' @param timer Argument for flashMessage.
    #'
    #' @return Return value produced by flashMessage.
    flashMessage <- function(title, text, type = "message", timer = 0) {
      theme_cols <- getOption("shinyRNAseq.theme.colors", list())
      btnCol <- switch(type,
        "success" = if (!is.null(theme_cols$success)) theme_cols$success else "#73A839",
        "error"   = if (!is.null(theme_cols$danger)) theme_cols$danger else "#C71C22",
        "warning" = if (!is.null(theme_cols$warning)) theme_cols$warning else "#DD5600",
        if (!is.null(theme_cols$info)) theme_cols$info else "#5bc0de"
      )
      shinyalert::shinyalert(
        title = title,
        text = text,
        type = type,
        timer = timer,
        confirmButtonCol = btnCol
      )
    }

    #' Build Fgsea Run Stats
    #'
    #' Executes build_fgsea_run_stats.
    #'
    #' @param all_results Argument for build_fgsea_run_stats.
    #'
    #' @return Return value produced by build_fgsea_run_stats.
    build_fgsea_run_stats <- function(all_results) {
      if (is.null(all_results) || length(all_results) == 0) {
        return(NULL)
      }

      rows <- lapply(names(all_results), function(key) {
        entry <- all_results[[key]]
        db_name <- sub("^.*\\| ", "", key)
        count <- if (!is.null(entry$result) && is.data.frame(entry$result)) nrow(entry$result) else 0L
        db_total_terms <- suppressWarnings(as.integer(fgsea_db_term_counts_rv()[db_name]))
        pct <- round(((count / db_total_terms) * 100), 1)
        data.frame(
          `Gene set` = fgsea_db_long_label(db_name),
          `Total terms in DB` = if (!is.na(db_total_terms) && db_total_terms > 0) db_total_terms else NA_integer_,
          `Significant pathways count` = count,
          `Significant pathways percent` = paste0(pct, " %"),
          check.names = FALSE
        )
      })

      do.call(rbind, rows)
    }

    output$fgsea_run_stats_ui <- renderUI({
      stats <- fgsea_run_stats_rv()
      if (is.null(stats) || !is.data.frame(stats) || nrow(stats) == 0) {
        return(NULL)
      }
      fluidRow(box(
        width = 12, title = "Summary", solidHeader = TRUE,
        closable = FALSE, collapsible = TRUE, status = "primary",
        div(DT::dataTableOutput(session$ns("fgsea_run_stats_table"), width = "80%")),
        tags$br(),
        tags$div(
          style = "margin-top:6px; font-size:13px; color:#555;",
          tags$strong("Note: "),
          tags$em("Total terms in DB"), " indicates the total number of pathways available in each MSigDB collection. ",
          "Significant pathway counts are based on the filtered fGSEA results table using ",
          tags$em("pval < 0.05"), "."
        )
      ))
    })

    output$fgsea_run_stats_table <- DT::renderDT({
      stats <- fgsea_run_stats_rv()
      req(!is.null(stats), is.data.frame(stats), nrow(stats) > 0)
      DT::datatable(
        stats,
        rownames = FALSE,
        class = "display nowrap compact",
        options = list(
          dom = "t",
          columnDefs = list(
            list(className = "dt-left", targets = 0),
            list(className = "dt-center", targets = 1:3)
          )
        )
      )
    })

    #' Plot Has Data
    #'
    #' Executes plot_has_data.
    #'
    #' @param p Argument for plot_has_data.
    #'
    #' @return Return value produced by plot_has_data.
    plot_has_data <- function(p) {
      inherits(p, "gg") && is.data.frame(p$data) && nrow(p$data) > 0
    }

    #' Compute Plot Height Px
    #'
    #' Executes compute_plot_height_px.
    #'
    #' @param p Argument for compute_plot_height_px.
    #' @param top_n Argument for compute_plot_height_px.
    #' @param min_height Argument for compute_plot_height_px.
    #'
    #' @return Return value produced by compute_plot_height_px.
    compute_plot_height_px <- function(p, top_n, min_height = 450) {
      base_h <- max(min_height, top_n * 35 + 180)
      if (!plot_has_data(p)) {
        return(base_h)
      }

      label_col <- if ("pathway_label" %in% colnames(p$data)) {
        as.character(p$data$pathway_label)
      } else if ("pathway" %in% colnames(p$data)) {
        as.character(p$data$pathway)
      } else {
        character(0)
      }
      if (length(label_col) == 0) {
        return(base_h)
      }

      label_col <- unique(label_col[!is.na(label_col)])
      line_counts <- vapply(strsplit(label_col, "\n", fixed = TRUE), length, integer(1))
      total_lines <- sum(line_counts)

      # Allocate vertical space by total wrapped text lines so multi-line
      # pathway labels do not collide in interactive plot display.
      dynamic_h <- 220 + total_lines * 18
      max(base_h, dynamic_h)
    }

    #' Compute Plot Width Px
    #'
    #' Executes compute_plot_width_px.
    #'
    #' @param p Argument for compute_plot_width_px.
    #' @param top_n Argument for compute_plot_width_px.
    #'
    #' @return Return value produced by compute_plot_width_px.
    compute_plot_width_px <- function(p, top_n) {
      base_w <- max(760, top_n * 18 + 620)
      if (!plot_has_data(p)) {
        return(base_w)
      }

      label_col <- if ("pathway_label" %in% colnames(p$data)) {
        as.character(p$data$pathway_label)
      } else if ("pathway" %in% colnames(p$data)) {
        as.character(p$data$pathway)
      } else {
        character(0)
      }
      if (length(label_col) == 0) {
        return(base_w)
      }

      label_col <- unique(label_col[!is.na(label_col)])
      if (length(label_col) == 0) {
        return(base_w)
      }

      wrapped_lines <- unlist(strsplit(label_col, "\n", fixed = TRUE), use.names = FALSE)
      wrapped_lines <- wrapped_lines[nzchar(wrapped_lines)]
      if (length(wrapped_lines) == 0) {
        return(base_w)
      }

      max_chars <- max(nchar(wrapped_lines), na.rm = TRUE)
      # Estimate width from longest wrapped label line, then clamp to a practical range.
      dynamic_w <- 620 + max_chars * 8
      as.integer(max(760, min(1400, max(base_w, dynamic_w))))
    }

    # ---- Helper: active plot types from individual checkboxes ---------------
    active_plot_types <- reactive({
      types <- character(0)
      if (isTRUE(input$plot_type_dot)) types <- c(types, "dot")
      if (isTRUE(input$plot_type_bar)) types <- c(types, "bar")
      types
    })

    # ---- Keep display radio buttons in sync with plot type checkboxes ---------
    observeEvent(active_plot_types(), ignoreNULL = FALSE, {
      sel <- active_plot_types()
      all_choices <- c("Dot Plot" = "dot", "Bar Plot" = "bar")
      if (length(sel) == 0) {
        updateRadioButtons(session, "plot_display",
          choices = all_choices, selected = "dot"
        )
        return()
      }
      display_choices <- all_choices[all_choices %in% sel]
      current <- input$plot_display
      default <- if ("dot" %in% sel) "dot" else sel[1]
      selected <- if (!is.null(current) && current %in% sel) current else default
      updateRadioButtons(session, "plot_display",
        choices = display_choices, selected = selected
      )
    })

    # ---- Custom GMT save -------------------------------------------------
    observeEvent(input$save_gmt, {
      req(input$gmt_file, tables$projDir)
      gmt_dir <- file.path(tables$projDir, "gmt_files")
      if (!dir.exists(gmt_dir)) dir.create(gmt_dir, recursive = TRUE, showWarnings = FALSE)
      # Sanitize: keep only alphanumerics, hyphens, underscores, dots; strip path separators
      safe_name <- gsub("[^A-Za-z0-9._-]", "_", basename(input$gmt_name))
      if (nchar(safe_name) == 0L) safe_name <- "custom"
      file_name <- paste0(safe_name, ".gmt")
      dest_path <- file.path(gmt_dir, file_name)
      file.copy(input$gmt_file$datapath, dest_path, overwrite = TRUE)
      output$gmt_status <- renderText({
        paste("Saved GMT file to:", dest_path)
      })
      updateSelectInput(session, "custom_gmt_selection",
        choices = list.files(gmt_dir,
          pattern = "\\.gmt$",
          full.names = TRUE
        )
      )
    })

    observe({
      req(tables$projDir)
      gmt_dir <- file.path(tables$projDir, "gmt_files")
      if (!dir.exists(gmt_dir)) dir.create(gmt_dir, recursive = TRUE, showWarnings = FALSE)
      updateSelectInput(session, "custom_gmt_selection",
        choices = list.files(gmt_dir,
          pattern = "\\.gmt$",
          full.names = TRUE
        )
      )
    })

    observe({
      # Always-excluded keys:
      # - C7:IMMUNESIGDB  — redundant: the full C7 collection is already in the standard checkbox list
      # - C5:HPO          — Human Phenotype Ontology: human-specific, no cross-species orthologs
      # - C7:VAX          — HIPC Vaccine Response: human-specific
      always_exclude <- c("C5:HPO", "C7:VAX", "C7:IMMUNESIGDB")

      other_choices <- character(0)
      term_count_map <- setNames(integer(0), character(0))
      if (requireNamespace("msigdbr", quietly = TRUE)) {
        cols <- tryCatch(msigdbr::msigdbr_collections(), error = function(e) NULL)
        if (!is.null(cols) && nrow(cols) > 0) {
          term_count_map <- extract_msigdb_term_counts(cols)
          keys <- ifelse(
            is.na(cols$gs_subcollection) | cols$gs_subcollection == "",
            cols$gs_collection,
            paste(cols$gs_collection, cols$gs_subcollection, sep = ":")
          )
          keys <- unique(as.character(keys))
          other_choices <- sort(setdiff(keys, unname(msigdb_choices)))
          # Remove always-excluded keys unconditionally
          other_choices <- setdiff(other_choices, always_exclude)
        }
      }
      fgsea_db_term_counts_rv(term_count_map)

      selected_standard <- isolate(input$msigdb_selection)
      if (is.null(selected_standard)) selected_standard <- "H"
      std_values <- unname(msigdb_choices)
      std_labels <- vapply(seq_along(std_values), function(i) {
        raw_label <- strip_trailing_code(names(msigdb_choices)[i])
        db_label_with_count(raw_label, std_values[i], term_count_map)
      }, character(1))
      updateCheckboxGroupInput(
        session,
        "msigdb_selection",
        choices = setNames(std_values, std_labels),
        selected = intersect(selected_standard, std_values)
      )

      selected_other <- isolate(input$other_msigdb_selection)
      if (is.null(selected_other)) selected_other <- character(0)

      other_labels <- other_choices
      if (requireNamespace("msigdbr", quietly = TRUE)) {
        cols <- tryCatch(msigdbr::msigdbr_collections(), error = function(e) NULL)
        if (!is.null(cols) && nrow(cols) > 0 && length(other_choices) > 0) {
          cols$built_key <- ifelse(
            is.na(cols$gs_subcollection) | cols$gs_subcollection == "",
            cols$gs_collection,
            paste(cols$gs_collection, cols$gs_subcollection, sep = ":")
          )
          other_labels <- vapply(other_choices, function(key) {
            matching <- cols[cols$built_key == key, , drop = FALSE]
            pretty <- if (nrow(matching) > 0) as.character(pretty_msigdb_label(key, matching)) else key
            db_label_with_count(pretty, key, term_count_map)
          }, character(1))
        }
      }

      updatePickerInput(
        session,
        "other_msigdb_selection",
        choices = setNames(other_choices, other_labels),
        selected = intersect(selected_other, other_choices)
      )
    })

    output$other_msigdb_reference <- renderTable(
      {
        species_specific_exclude <- c("C5:HPO", "C7:VAX", "C7:IMMUNESIGDB")
        full_std_collections <- unname(msigdb_choices)[!grepl(":", unname(msigdb_choices))]
        ref_table <- data.frame(
          `Short Name` = character(0),
          `Long Name` = character(0),
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
        if (requireNamespace("msigdbr", quietly = TRUE)) {
          cols <- tryCatch(msigdbr::msigdbr_collections(), error = function(e) NULL)
          if (!is.null(cols) && nrow(cols) > 0) {
            # Build keys from cols
            cols$built_key <- ifelse(
              is.na(cols$gs_subcollection) | cols$gs_subcollection == "",
              cols$gs_collection,
              paste(cols$gs_collection, cols$gs_subcollection, sep = ":")
            )
            keys <- unique(as.character(cols$built_key))
            other_keys <- sort(setdiff(keys, unname(msigdb_choices)))
            # Apply the same exclusions as the picker
            is_sub_of_std <- grepl(":", other_keys) &
              sub(":.*$", "", other_keys) %in% full_std_collections
            other_keys <- other_keys[!is_sub_of_std]
            other_keys <- setdiff(other_keys, species_specific_exclude)

            if (length(other_keys) > 0) {
              long_names <- sapply(other_keys, function(key) {
                # Find matching row(s) in cols using the built_key column
                matching <- cols[cols$built_key == key, ]
                if (nrow(matching) > 0) {
                  return(as.character(pretty_msigdb_label(key, matching)))
                }
                return(as.character(key))
              }, USE.NAMES = FALSE)

              ref_table <- data.frame(
                `Short Name` = other_keys,
                `Long Name` = long_names,
                check.names = FALSE,
                stringsAsFactors = FALSE
              )
            }
          }
        }

        ref_table
      },
      striped = TRUE,
      hover = TRUE,
      spacing = "xs"
    )

    #' Perform Fgsea Run
    #'
    #' Executes perform_fgsea_run.
    #'
    #' @return Return value produced by perform_fgsea_run.
    perform_fgsea_run <- function() {
      tryCatch(
        {
          proceed_with_fgsea(FALSE)
          fgsea_run_stats_rv(NULL)

          # Count total databases selected
          standard_sel <- input$msigdb_selection
          other_sel <- input$other_msigdb_selection
          if (is.null(standard_sel)) standard_sel <- character(0)
          if (is.null(other_sel)) other_sel <- character(0)

          current_comp <- tables$comp
          if (!hasText(current_comp)) {
            flashMessage("fGSEA Error",
              "No active comparison found. Please complete differential analysis first.",
              type = "error"
            )
            return(invisible(NULL))
          }

          ShinyMsgBox(
            title = "Running fGSEA",
            text = "Depending on number of selected db, this may take a moment...",
            size = "l",
            logicClose = FALSE
          )
          on.exit(removeModal(), add = TRUE)

          rnk_files <- current_rnk_files()
          if (length(rnk_files) == 0) {
            flashMessage("fGSEA Error",
              "No ranking file found for the current comparison and selected ranking metric.",
              type = "error"
            )
            return(invisible(NULL))
          }
          sample_name <- if (hasText(input$sample_name)) as.character(input$sample_name[[1]]) else NULL

          use_custom_gmt <- identical(input$gmt_source_tab, "custom")
          if (!use_custom_gmt) {
            if (!is.null(tables$species) && identical(tables$species, "Other")) {
              flashMessage("fGSEA Error",
                "Species 'Other' is only supported with Custom GMT. Switch to Custom GMT and rerun.",
                type = "error"
              )
              return(invisible(NULL))
            }
          } else {
            req(input$custom_gmt_selection)
          }

          all_results <- list()

          withProgress(message = "Running fGSEA", value = 0, {
            db_list <- if (use_custom_gmt) {
              input$custom_gmt_selection
            } else {
              unique(c(standard_sel, other_sel))
            }
            if (length(db_list) == 0) {
              flashMessage("fGSEA Error", "Please select at least one gene set collection.", type = "error")
              return(invisible(NULL))
            }
            total_steps <- length(rnk_files) * length(db_list)
            step <- 0

            for (rnk_path in rnk_files) {
              rnk_data <- tryCatch(
                {
                  read.table(rnk_path, header = FALSE, sep = "\t", stringsAsFactors = FALSE)
                },
                error = function(e) {
                  showNotification(paste0("Failed to read RNK file [", basename(rnk_path), "]: ", conditionMessage(e)),
                    type = "error", duration = 15
                  )
                  NULL
                }
              )
              if (is.null(rnk_data) || ncol(rnk_data) < 2) next

              rnk_data <- rnk_data[!duplicated(rnk_data[[1]]), ]
              ranks_vec <- suppressWarnings(as.numeric(rnk_data[[2]]))
              keep <- is.finite(ranks_vec) & !is.na(rnk_data[[1]]) & nzchar(as.character(rnk_data[[1]]))
              ranks_vec <- setNames(ranks_vec[keep], as.character(rnk_data[[1]][keep]))
              if (length(ranks_vec) == 0) next

              rnk_label <- tools::file_path_sans_ext(basename(rnk_path))
              out_dir <- dirname(rnk_path)

              for (sel in db_list) {
                step <- step + 1
                incProgress(step / total_steps,
                  detail = paste("Processing", sel, "for", rnk_label)
                )

                res <- tryCatch(
                  {
                    if (use_custom_gmt) {
                      run_fgsea(
                        ranks = ranks_vec,
                        custom_gmt = sel,
                        sname = sample_name,
                        run_only = TRUE
                      )
                    } else {
                      sp <- strsplit(sel, ":", fixed = TRUE)[[1]]
                      db <- sp[1]
                      subcat <- if (length(sp) > 1) paste(sp[-1], collapse = ":") else NULL
                      run_fgsea(
                        ranks = ranks_vec,
                        db = db,
                        subcat = subcat,
                        species = tables$species,
                        sname = sample_name,
                        run_only = TRUE
                      )
                    }
                  },
                  error = function(e) {
                    showNotification(paste0("Error for [", sel, "]: ", conditionMessage(e)),
                      type = "error", duration = 15
                    )
                    NULL
                  }
                )
                if (is.null(res)) next

                result_key <- paste0(rnk_label, " | ", sel)
                all_results[[result_key]] <- list(
                  result = res,
                  out_dir = out_dir,
                  rnk_label = rnk_label
                )
              }
            }
          })

          raw_fgsea_results_rv(all_results)
          plot_objects_rv(NULL)
          generated_result_keys_rv(character(0))

          fgsea_stats <- build_fgsea_run_stats(all_results)
          fgsea_run_stats_rv(fgsea_stats)

          if (length(all_results) == 0) {
            flashMessage("fGSEA Error", "No fGSEA results were produced. Check your ranking file and selected gene sets.", type = "error")
            return(invisible(NULL))
          }

          # ---- Save results into analysis directories ----
          withProgress(message = "Saving results", value = 0, {
            # Group by rnk file so each gets one Excel workbook
            by_rnk <- list()
            for (key in names(all_results)) {
              entry <- all_results[[key]]
              rnk_key <- paste0(entry$out_dir, "/", entry$rnk_label)
              if (is.null(by_rnk[[rnk_key]])) {
                by_rnk[[rnk_key]] <- list(
                  entries = list(),
                  out_dir = entry$out_dir,
                  rnk_label = entry$rnk_label
                )
              }
              db_name <- sub("^.*\\| ", "", key)
              by_rnk[[rnk_key]]$entries[[db_name]] <- entry$result
            }

            #' Flatten Results
            #'
            #' Executes flatten_results.
            #'
            #' @param res Argument for flatten_results.
            #'
            #' @return Return value produced by flatten_results.
            flatten_results <- function(res) {
              res <- as.data.frame(res)
              if ("leadingEdge" %in% colnames(res)) {
                res$leadingEdge <- vapply(
                  res$leadingEdge,
                  function(x) paste(x, collapse = ";"),
                  character(1)
                )
              }
              res
            }

            n_rnk <- length(by_rnk)
            for (rk in names(by_rnk)) {
              info <- by_rnk[[rk]]
              wb <- createWorkbook()
              for (db_name in names(info$entries)) {
                flat <- flatten_results(info$entries[[db_name]])
                sheet_name <- substr(gsub("[\\[\\]:*?/\\\\]", "_", db_name, perl = TRUE), 1, 31)
                addWorksheet(wb, sheet_name)
                writeData(wb, sheet_name, flat)
              }
              xlsx_file <- file.path(
                file.path(info$out_dir, "fGSEA"),
                paste0(info$rnk_label, "_fgsea_results.xlsx")
              )
              dir.create(dirname(xlsx_file), recursive = TRUE, showWarnings = FALSE)
              saveWorkbook(wb, xlsx_file, overwrite = TRUE)
              incProgress(1 / n_rnk)
            }
          })

          # ---- Populate plot-filter pickers after run completes ---------------
          dbs_raw <- unique(vapply(names(all_results), function(k) {
            sub("^.*\\| ", "", k)
          }, character(1)))
          db_count_map <- fgsea_db_term_counts_rv()
          db_labels <- vapply(dbs_raw, function(db_key) {
            pretty <- fgsea_db_long_label(db_key)
            db_label_with_count(pretty, db_key, db_count_map)
          }, character(1))
          db_display <- setNames(dbs_raw, db_labels)
          updatePickerInput(session, "plot_select_db",
            choices = db_display,
            selected = if (length(db_display) > 0) unname(db_display) else character(0)
          )

          flashMessage("fGSEA Complete",
            "Analysis finished. Results saved to analysis directories.",
            type = "success",
            timer = 3000
          )
          expandBox("fgsea_plot_settings_box", default = TRUE)
          collapseBox("fgsea_plots_box", default = TRUE)
        },
        error = function(e) {
          removeModal()
          cat("\n[fGSEA] run_button error:", conditionMessage(e), "\n")
          flashMessage("fGSEA Error",
            paste0("Run failed: ", conditionMessage(e)),
            type = "error"
          )
        }
      )
    }


    # ---- Run fGSEA -------------------------------------------------------
    observeEvent(input$run_button, {
      standard_sel <- input$msigdb_selection
      other_sel <- input$other_msigdb_selection
      if (is.null(standard_sel)) standard_sel <- character(0)
      if (is.null(other_sel)) other_sel <- character(0)
      total_dbs <- length(unique(c(standard_sel, other_sel)))

      # If warning is required, user confirms and that confirmation triggers run.
      if (total_dbs >= 4 && !isTRUE(proceed_with_fgsea())) {
        showModal(modalDialog(
          title = "Processing Time Warning",
          size = "m",
          easyClose = FALSE,
          footer = tagList(
            modalButton("Cancel"),
            actionButton(session$ns("confirm_fgsea_proceed"), "Proceed", class = "btn-warning")
          ),
          div(
            h4(paste0("Selecting many databases (", total_dbs, " total) will increase processing time significantly.")),
            p("Click 'Proceed' to run fGSEA now.")
          )
        ))
        return(invisible(NULL))
      }

      perform_fgsea_run()
    })

    # Handle "Proceed" button confirmation
    observeEvent(input$confirm_fgsea_proceed, {
      removeModal()
      proceed_with_fgsea(TRUE)
      perform_fgsea_run()
    })
    # ---- Generate / refresh plots ----------------------------------------
    observeEvent(input$generate_plot_button, {
      req(raw_fgsea_results_rv())
      current_comp <- tables$comp
      if (!hasText(current_comp)) {
        flashMessage("fGSEA Error",
          "No active comparison found. Please complete differential analysis first.",
          type = "error"
        )
        return(invisible(NULL))
      }

      all_results <- raw_fgsea_results_rv()
      plot_title <- if (hasText(input$sample_name)) as.character(input$sample_name[[1]]) else NULL

      # Filter keys by selected comparisons and DBs
      sel_dbs <- input$plot_select_db
      if (hasText(current_comp)) {
        all_results <- all_results[vapply(
          names(all_results),
          function(k) grepl(current_comp, sub(" \\| .*$", "", k), fixed = TRUE), logical(1)
        )]
      }
      if (!is.null(sel_dbs) && length(sel_dbs) > 0) {
        all_results <- all_results[vapply(
          names(all_results),
          function(k) sub("^.*\\| ", "", k) %in% sel_dbs, logical(1)
        )]
      }

      if (length(all_results) == 0) {
        generated_result_keys_rv(character(0))
        flashMessage("fGSEA Error", "No fGSEA result matches the current filters.", type = "error")
        return(invisible(NULL))
      }

      current_result <- input$plot_result_tab
      selected_result <- if (!is.null(current_result) && current_result %in% names(all_results)) current_result else names(all_results)[1]
      generated_result_keys_rv(names(all_results))
      updateTabsetPanel(session, "plot_result_tab", selected = selected_result)

      plots <- list()
      sel_types <- active_plot_types()
      if (length(sel_types) == 0) {
        flashMessage("fGSEA Error", "Please select at least one plot type.", type = "error")
        return(invisible(NULL))
      }

      #' Pval Type for
      #'
      #' Executes pval_type_for.
      #'
      #' @param ptype Argument for pval_type_for.
      #'
      #' @return Return value produced by pval_type_for.
      pval_type_for <- function(ptype) {
        key <- paste0(ptype, "_pval_type")
        v <- input[[key]]
        if (is.null(v) || !nzchar(v)) "logPval" else v
      }

      top_n_plot <- suppressWarnings(as.integer(input$top_n))
      if (!is.finite(top_n_plot) || is.na(top_n_plot)) top_n_plot <- 10L
      top_n_plot <- max(5L, min(25L, top_n_plot))
      if (!identical(top_n_plot, input$top_n)) {
        updateNumericInput(session, "top_n", value = top_n_plot)
      }

      withProgress(message = "Generating Plots", value = 0, {
        n_results <- length(all_results)
        for (i in seq_along(all_results)) {
          key <- names(all_results)[i]
          entry <- all_results[[key]]
          incProgress(1 / n_results, detail = paste("Creating plot for", key))
          for (ptype in sel_types) {
            pval_type_val <- pval_type_for(ptype)
            plot_key <- paste0(key, "__", ptype)
            result <- run_fgsea(
              fgsea_res_tab = entry$result,
              sname = plot_title,
              top_n = top_n_plot,
              plot_type = ptype,
              pval_type = pval_type_val
            )
            # split plots return a named list(up=, down=)
            if (is.list(result) && !inherits(result, "gg")) {
              if (!is.null(result$up)) {
                plots[[paste0(key, "__", ptype, "_up")]] <- result$up
              }
              if (!is.null(result$down)) {
                plots[[paste0(key, "__", ptype, "_down")]] <- result$down
              }
            } else {
              plots[[plot_key]] <- result
            }
          }
        }
      })

      plot_objects_rv(plots)

      # ---- Save PNGs to analysis directories for selected types ----------
      withProgress(message = "Saving plots", value = 0, {
        n_results <- length(all_results)
        for (key in names(all_results)) {
          entry <- all_results[[key]]
          db_name <- sub("^.*\\| ", "", key)
          db_long_name <- fgsea_db_long_label(db_name)
          plot_out_dir <- file.path(entry$out_dir, "fGSEA")
          dir.create(plot_out_dir, recursive = TRUE, showWarnings = FALSE)
          for (ptype in sel_types) {
            # dot plots have up/down files, bar plots have one combined file
            save_pairs <- if (ptype %in% c("dot")) {
              list(
                list(suffix = paste0(ptype, "_up"), label = paste0(ptype, "_up")),
                list(suffix = paste0(ptype, "_down"), label = paste0(ptype, "_down"))
              )
            } else {
              list(list(suffix = ptype, label = ptype))
            }

            for (pair in save_pairs) {
              db_token <- gsub("[^[:alnum:]]+", "_", db_long_name)
              db_token <- gsub("^_+|_+$", "", db_token)
              p_file <- file.path(
                plot_out_dir,
                paste0(
                  entry$rnk_label, "_",
                  db_token, "_", pair$label, ".pdf"
                )
              )
              plot_key <- paste0(key, "__", pair$suffix)
              p_save <- plots[[plot_key]]
              has_data <- plot_has_data(p_save)
              if (has_data) {
                save_height <- max(5, ceiling(top_n_plot * 0.4) + 2)
                ggsave(p_file,
                  plot = p_save, width = 10,
                  height = save_height, units = "in", dpi = 300,
                  create.dir = TRUE
                )
              } else if (file.exists(p_file)) {
                file.remove(p_file)
              }
            }
          }
          incProgress(1 / n_results)
        }
      })
      expandBox("fgsea_plots_box", default = TRUE)
    })

    # ---- Render interactive plots ----------------------------------------
    # Store stable plot IDs so renderUI can reference them without
    # re-creating render functions (which crashes the R session).
    plot_ids_rv <- reactiveVal(character(0))

    output$plot_display_desc <- renderUI({
      desc <- switch(input$plot_display,
        "bar" = tagList(
          "View", tags$em("NES"), "or Normalized Enrichment Score (",
          tags$strong("x axis"), ") and ", tags$em("NES"), " (",
          tags$strong("bar color"), ") for top pathways"
        ),
        "dot" = tagList(
          " View ", tags$em("GeneRatio"), " (",
          tags$strong("color gradient"), "), significance as ",
          tags$em("-log10Pval"), " or ", tags$em("-log10Padj"), " (", tags$strong("x axis"),
          ") and number of DEGs overlapping with pathway as Gene Count (",
          tags$strong("dot size"), ") for top upregulated and downregulated pathways"
        )
      )
      tags$span(style = "font-size: 15px; color:navy; font-weight: 500", desc)
    })

    observeEvent(plot_objects_rv(), {
      plots <- plot_objects_rv()
      if (is.null(plots) || length(plots) == 0) {
        plot_ids_rv(character(0))
        plot_heights_rv(list())
        plot_widths_rv(list())
        return()
      }

      ids <- setNames(paste0("plot_", seq_along(plots)), names(plots))
      heights <- list()
      widths <- list()

      for (i in seq_along(plots)) {
        local({
          local_id <- ids[[i]]
          local_plot <- plots[[i]]
          local_name <- names(plots)[[i]]
          local_is_static <- grepl("__dot(_up|_down)?$", local_name)
          local_is_dot <- grepl("__dot(_up|_down)?$", local_name)
          local_top_n <- suppressWarnings(as.integer(input$top_n))
          if (!is.finite(local_top_n) || is.na(local_top_n)) local_top_n <- 10L
          local_top_n <- max(5L, min(25L, local_top_n))
          local_h_num <- if (isTRUE(local_is_dot)) {
            compute_plot_height_px(local_plot, local_top_n, min_height = 280)
          } else {
            compute_plot_height_px(local_plot, local_top_n)
          }
          local_h <- paste0(local_h_num, "px")
          heights[[local_id]] <<- local_h
          if (isTRUE(local_is_static)) {
            if (isTRUE(local_is_dot)) {
              local_w_num <- compute_plot_width_px(local_plot, local_top_n)
              widths[[local_id]] <<- paste0(local_w_num, "px")
              output[[local_id]] <- renderPlot(
                {
                  print(local_plot)
                },
                height = function() local_h_num,
                width = function() local_w_num,
                res = 96
              )
            } else {
              output[[local_id]] <- renderPlot(
                {
                  print(local_plot)
                },
                height = function() local_h_num,
                res = 96
              )
            }
          } else {
            output[[local_id]] <- renderPlotly({
              fig <- ggplotly(local_plot, tooltip = "text")
              config(fig, toImageButtonOptions = list(
                format   = "png",
                filename = paste0(local_name, "_fgsea_plot"),
                width    = 800,
                height   = local_h_num
              ))
            })
          }
        })
      }

      plot_ids_rv(ids)
      plot_heights_rv(heights)
      plot_widths_rv(widths)
    })

    output$plots_ui <- renderUI({
      ids <- plot_ids_rv()
      if (length(ids) == 0) {
        return(NULL)
      }

      display_type <- input$plot_display
      if (!hasText(display_type)) {
        return(NULL)
      }

      selected_result <- input$plot_result_tab
      if (!hasText(selected_result)) {
        return(NULL)
      }

      ns <- session$ns
      plots <- plot_objects_rv()
      heights <- plot_heights_rv()
      widths <- plot_widths_rv()

      if (display_type %in% c("dot")) {
        # Split plots return up + down sub-plots
        up_key <- paste0(selected_result, "__", display_type, "_up")
        down_key <- paste0(selected_result, "__", display_type, "_down")
        up_id <- ids[up_key]
        down_id <- ids[down_key]

        #' Make Panel
        #'
        #' Executes make_panel.
        #'
        #' @param pid Argument for make_panel.
        #'
        #' @return Return value produced by make_panel.
        make_panel <- function(pid) {
          h <- if (!is.null(heights[[pid]])) heights[[pid]] else "500px"
          w <- if (!is.null(widths[[pid]])) widths[[pid]] else "100%"
          div(
            style = "overflow-x: auto;",
            plotOutput(ns(pid), height = h, width = w)
          )
        }

        tagList(
          if (length(up_id) > 0) make_panel(up_id[1]),
          if (length(up_id) > 0 && length(down_id) > 0) {
            div(style = "height: 2.2em;")
          },
          if (length(down_id) > 0) make_panel(down_id[1])
        )
      } else {
        # fallback: single plot
        suffix <- paste0("__", display_type)
        plot_name <- paste0(selected_result, suffix)
        i <- which(names(plots) == plot_name)
        if (length(i) == 0) {
          return(NULL)
        }
        i <- i[1]

        tagList(
          div(
            style = "overflow-x: auto;",
            plotlyOutput(ns(ids[[i]]),
              height = if (!is.null(heights[[ids[[i]]]])) heights[[ids[[i]]]] else "500px"
            )
          )
        )
      }
    })

    output$fgsea_table_preview <- DT::renderDT({
      req(input$plot_result_tab)
      all_results <- raw_fgsea_results_rv()
      req(!is.null(all_results), length(all_results) > 0)
      req(input$plot_result_tab %in% names(all_results))

      entry <- all_results[[input$plot_result_tab]]
      req(!is.null(entry$result))
      tbl <- as.data.frame(entry$result)
      if ("leadingEdge" %in% colnames(tbl)) {
        tbl$leadingEdge <- vapply(tbl$leadingEdge, function(x) {
          if (is.null(x)) {
            return("")
          }
          if (length(x) == 0) {
            return("")
          }
          paste(as.character(x), collapse = ";")
        }, character(1))
      }
      DT::datatable(
        tbl,
        rownames = FALSE,
        class = "stripe hover compact nowrap",
        options = list(pageLength = 20, scrollX = TRUE)
      )
    })

    output$plot_result_tabs <- renderUI({
      keys <- generated_result_keys_rv()
      if (length(keys) == 0) {
        return(NULL)
      }

      current <- input$plot_result_tab
      selected <- if (!is.null(current) && current %in% keys) current else keys[1]

      #' Make Short Tab Titles
      #'
      #' Executes make_short_tab_titles.
      #'
      #' @param x Argument for make_short_tab_titles.
      #'
      #' @return Return value produced by make_short_tab_titles.
      make_short_tab_titles <- function(x) {
        labels <- vapply(x, function(k) {
          parts <- strsplit(k, " \\| ", perl = TRUE)[[1]]
          if (length(parts) >= 2) {
            paste(parts[-1], collapse = " | ")
          } else {
            k
          }
        }, character(1))

        dup <- duplicated(labels) | duplicated(labels, fromLast = TRUE)
        if (any(dup)) {
          idx <- ave(seq_along(labels), labels, FUN = seq_along)
          tot <- ave(rep(1L, length(labels)), labels, FUN = sum)
          labels[dup] <- paste0(labels[dup], " (", idx[dup], "/", tot[dup], ")")
        }
        labels
      }

      tab_labels <- make_short_tab_titles(keys)

      tabs <- lapply(seq_along(keys), function(i) {
        tabPanel(title = tags$span(title = keys[i], tab_labels[i]), value = keys[i])
      })

      do.call(tabsetPanel, c(list(
        id = session$ns("plot_result_tab"),
        type = "tabs",
        selected = selected
      ), tabs))
    })
  })
}
