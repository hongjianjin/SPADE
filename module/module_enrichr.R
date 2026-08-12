library(shiny)
library(shinydashboard)
library(shinyWidgets)

#' Mod Enrichr Ui
#'
#' Executes mod_enrichr_ui.
#'
#' @param id Argument for mod_enrichr_ui.
#'
#' @return Return value produced by mod_enrichr_ui.
mod_enrichr_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      box(
        id = ns("enrichr_run_box"),
        width = 12,
        title = "Run EnrichR",
        solidHeader = TRUE,
        status = "primary",
        collapsible = TRUE,
        collapsed = FALSE,
        fluidRow(
          column(
            12,
            radioButtons(
              ns("rank_metric"),
              label = "Ranking metric:",
              choices = c(
                "log2FC \u00d7 \u2212log10(P-value)" = "log2FCxNegLog10Pval",
                "log2FC" = "log2FC",
                "t-statistic" = "t"
              ),
              selected = "log2FCxNegLog10Pval",
              inline = TRUE
            ),
            radioButtons(
              ns("enrichr_top_genes"),
              label = "Genes submitted per direction:",
              choices = c(
                "Top 100" = 100,
                "Top 250" = 250,
                "Top 500" = 500
              ),
              selected = 250,
              inline = TRUE
            ),
            tags$div(
              style = "font-size: 13px; color: #555; margin-top: 4px; margin-bottom: 12px;",
              HTML("Genes are ranked by the selected metric. The selected number of <b>Up</b> and <b>Down</b> ranked genes is submitted to EnrichR. If fewer genes are available in either direction, all genes in that direction are used.")
            )
          )
        ),
        HTML("<hr style='margin-top:5px; margin-bottom:5px; border:1px solid #ddd;'>"),
        fluidRow(
          column(
            12,
            pickerInput(
              inputId = ns("enrichr_db_all"),
              label = "Select EnrichR geneset database(s)",
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
            tags$div(
              style = "font-size: 13px; color: #555; margin-top: 8px; margin-bottom: 12px;",
              "Numbers in parentheses indicate the total number of terms available in that EnrichR database."
            )
          )
        ),
        actionButton(ns("run_button"), "Run EnrichR", icon = icon("play"), class = "btn-warning"),
        tags$div(style = "margin-top: 10px;"),
        uiOutput(ns("enrichr_run_status")),
        tags$br(),
        uiOutput(ns("enrichr_run_stats_ui"))
      )
    ),
    fluidRow(
      box(
        id = ns("enrichr_plot_settings_box"),
        width = 12,
        title = "Plot Settings",
        solidHeader = TRUE,
        status = "primary",
        collapsible = TRUE,
        collapsed = TRUE,
        fluidRow(
          column(
            7,
            pickerInput(
              inputId = ns("plot_result_keys"),
              label = "Select/Deselect enrichR geneset DB(s) to plots",
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
            )
          ),
          column(
            3,
            numericInput(ns("top_n"), "Top N Pathways per Direction:", value = 10, min = 5, max = 25, step = 1)
          )
        ),
        fluidRow(
          column(
            12,
            tags$b("Generate Plot Type(s) & P-value Metric:"),
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
              tags$em("Combined.Score")
            )
          )
        ),
        fluidRow(
          column(
            2,
            tags$br(),
            actionButton(ns("generate_plot"), "Generate Plot", icon = icon("chart-bar"), class = "btn-warning")
          )
        )
      )
    ),
    fluidRow(
      box(
        id = ns("enrichr_plots_box"),
        width = 12,
        title = "EnrichR Plots",
        solidHeader = TRUE,
        status = "primary",
        collapsible = TRUE,
        collapsed = TRUE,
        fluidRow(
          column(
            12,
            style = "display: flex; align-items: center; gap: 10px; flex-wrap: wrap;",
            radioButtons(
              ns("plot_type_display"), "Display Plot Type:",
              choices = c("Dot Plot" = "dot", "Bar Plot" = "bar"),
              selected = "dot", inline = TRUE
            ),
            div(
              style = "margin-top: 25px; max-width: 600px; ",
              uiOutput(ns("plot_display_desc"))
            )
          )
        ),
        uiOutput(ns("result_tabs")),
        uiOutput(ns("enrichr_plot_ui")),
        tags$hr(),
        tags$b("Results Table Preview"),
        DT::DTOutput(ns("enrichr_table"))
      )
    )
  )
}

#' Mod Enrichr Server
#'
#' Executes mod_enrichr_server.
#'
#' @param id Argument for mod_enrichr_server.
#' @param tables Argument for mod_enrichr_server.
#'
#' @return Return value produced by mod_enrichr_server.
mod_enrichr_server <- function(id, tables) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    enrichr_results_rv <- reactiveVal(list())
    proceed_with_enrichr <- reactiveVal(FALSE)
    enrichr_run_status_rv <- reactiveVal(NULL)
    enrichr_run_stats_rv <- reactiveVal(NULL)
    generated_plot_state_rv <- reactiveVal(list(
      keys          = character(0),
      top_up        = 10L,
      top_down      = 10L,
      plot_types    = c("dot", "bar"),
      bar_pval_type = "logPval",
      dot_pval_type = "logPval"
    ))

    active_plot_types_enrichr <- reactive({
      types <- character(0)
      if (isTRUE(input$plot_type_dot)) types <- c(types, "dot")
      if (isTRUE(input$plot_type_bar)) types <- c(types, "bar")
      types
    })

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
      if (is.null(state)) {
        return(default)
      }
      if (is.list(state) && !is.null(state$collapsed)) {
        return(isTRUE(state$collapsed))
      }
      if (is.logical(state) && length(state) == 1L && !is.na(state)) {
        return(isTRUE(state))
      }
      default
    }

    #' Js String
    #'
    #' Executes jsString.
    #'
    #' @param x Argument for jsString.
    #'
    #' @return Return value produced by jsString.
    jsString <- function(x) {
      x <- as.character(x)
      x <- gsub("\\", "\\\\", x, fixed = TRUE)
      x <- gsub("'", "\\'", x, fixed = TRUE)
      paste0("'", x, "'")
    }

    #' Ensure Box Collapsed
    #'
    #' Executes ensureBoxCollapsed.
    #'
    #' @param id Argument for ensureBoxCollapsed.
    #' @param collapsed Argument for ensureBoxCollapsed.
    #'
    #' @return Return value produced by ensureBoxCollapsed.
    ensureBoxCollapsed <- function(id, collapsed) {
      dom_id <- session$ns(id)
      should_collapse <- if (isTRUE(collapsed)) "true" else "false"
      shinyjs::runjs(sprintf(
        paste0(
          "(function(){",
          "var el=document.getElementById(%s);",
          "if(!el||!window.jQuery){return;}",
          "var box=window.jQuery(el);",
          "var isCollapsed=box.hasClass('collapsed-box');",
          "var shouldCollapse=%s;",
          "if(isCollapsed!==shouldCollapse){",
          "var btn=box.find('[data-widget=\"collapse\"]').first();",
          "if(btn.length){btn.trigger('click');}",
          "}",
          "})();"
        ),
        jsString(dom_id),
        should_collapse
      ))
    }

    #' Set Box Collapsed
    #'
    #' Executes setBoxCollapsed.
    #'
    #' @param id Argument for setBoxCollapsed.
    #' @param collapsed Argument for setBoxCollapsed.
    #' @param default Argument for setBoxCollapsed.
    #'
    #' @return Return value produced by setBoxCollapsed.
    setBoxCollapsed <- function(id, collapsed, default = FALSE) {
      if (!identical(boxCollapsed(id, default = default), isTRUE(collapsed))) {
        shinydashboardPlus::updateBox(id, action = "toggle", session = session)
      }
      session$onFlushed(function() {
        ensureBoxCollapsed(id, collapsed)
      }, once = TRUE)
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
      setBoxCollapsed(id, collapsed = FALSE, default = default)
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
      setBoxCollapsed(id, collapsed = TRUE, default = default)
    }

    observeEvent(active_plot_types_enrichr(), ignoreNULL = FALSE, {
      sel <- active_plot_types_enrichr()
      all_choices <- c("Dot Plot" = "dot", "Bar Plot" = "bar")
      if (length(sel) == 0) {
        updateRadioButtons(session, "plot_type_display",
          choices = all_choices, selected = "dot"
        )
        return()
      }
      display_choices <- all_choices[all_choices %in% sel]
      current <- input$plot_type_display
      selected <- if (!is.null(current) && current %in% sel) current else sel[1]
      updateRadioButtons(session, "plot_type_display", choices = display_choices, selected = selected)
    })

    #' Collapse Latest Db Versions
    #'
    #' Executes collapse_latest_db_versions.
    #'
    #' @param db_names Argument for collapse_latest_db_versions.
    #'
    #' @return Return value produced by collapse_latest_db_versions.
    collapse_latest_db_versions <- function(db_names) {
      db_names <- unique(as.character(db_names))
      db_names <- db_names[!is.na(db_names) & nzchar(db_names)]
      if (length(db_names) == 0) {
        return(character(0))
      }

      #' Extract Year
      #'
      #' Executes extract_year.
      #'
      #' @param x Argument for extract_year.
      #'
      #' @return Return value produced by extract_year.
      extract_year <- function(x) {
        m <- regexpr("(19|20)[0-9]{2}$", x, perl = TRUE)
        ok <- m > 0
        out <- rep(NA_integer_, length(x))
        if (any(ok)) {
          out[ok] <- as.integer(substr(x[ok], m[ok], m[ok] + attr(m, "match.length")[ok] - 1L))
        }
        out
      }

      #' Strip Year Suffix
      #'
      #' Executes strip_year_suffix.
      #'
      #' @param x Argument for strip_year_suffix.
      #'
      #' @return Return value produced by strip_year_suffix.
      strip_year_suffix <- function(x) {
        trimws(sub("([_\\-[:space:]]?)(19|20)[0-9]{2}$", "", x, perl = TRUE))
      }

      years <- extract_year(db_names)
      bases <- strip_year_suffix(db_names)
      groups <- split(seq_along(db_names), bases)

      chosen_idx <- vapply(groups, function(idx) {
        y <- years[idx]
        if (any(!is.na(y))) {
          max_y <- max(y, na.rm = TRUE)
          cands <- idx[which(y == max_y)]
        } else {
          cands <- idx
        }
        cands[order(db_names[cands], decreasing = TRUE)][1]
      }, integer(1))

      sort(unique(db_names[chosen_idx]))
    }

    #' Extract Enrichr Term Counts
    #'
    #' Executes extract_enrichr_term_counts.
    #'
    #' @param dbs_df Argument for extract_enrichr_term_counts.
    #'
    #' @return Return value produced by extract_enrichr_term_counts.
    extract_enrichr_term_counts <- function(dbs_df) {
      if (is.null(dbs_df) || !is.data.frame(dbs_df) || nrow(dbs_df) == 0 ||
        !"libraryName" %in% colnames(dbs_df)) {
        return(setNames(integer(0), character(0)))
      }

      candidate_cols <- c("numTerms", "num_terms", "numTerm", "numGeneSets", "num_genesets")
      count_col <- candidate_cols[candidate_cols %in% colnames(dbs_df)][1]
      if (is.na(count_col) || !nzchar(count_col)) {
        low_cols <- tolower(colnames(dbs_df))
        idx <- grep("num.*(term|geneset|pathway)|(term|geneset|pathway).*num", low_cols)
        if (length(idx) > 0) {
          count_col <- colnames(dbs_df)[idx[1]]
        }
      }
      if (is.na(count_col) || !nzchar(count_col)) {
        return(setNames(integer(0), character(0)))
      }

      db_names <- as.character(dbs_df$libraryName)
      counts <- suppressWarnings(as.integer(dbs_df[[count_col]]))
      keep <- !is.na(db_names) & nzchar(db_names) & !is.na(counts) & counts > 0
      if (!any(keep)) {
        return(setNames(integer(0), character(0)))
      }
      out <- counts[keep]
      names(out) <- db_names[keep]
      out
    }

    #' Db Label with Count
    #'
    #' Executes db_label_with_count.
    #'
    #' @param db_name Argument for db_label_with_count.
    #' @param count_map Argument for db_label_with_count.
    #'
    #' @return Return value produced by db_label_with_count.
    db_label_with_count <- function(db_name, count_map) {
      cnt <- suppressWarnings(as.integer(count_map[db_name]))
      if (!is.na(cnt) && cnt > 0) {
        return(paste0(db_name, " (", format(cnt, big.mark = ","), ")"))
      }
      db_name
    }

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

    #' Active Output Dir
    #'
    #' Executes active_output_dir.
    #'
    #' @return Return value produced by active_output_dir.
    active_output_dir <- function() {
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
      tempdir()
    }

    #' Entry Significant Count
    #'
    #' Executes entry_significant_count.
    #'
    #' @param entry Argument for entry_significant_count.
    #'
    #' @return Return value produced by entry_significant_count.
    entry_significant_count <- function(entry) {
      n_up <- if (!is.null(entry$up_result) && is.data.frame(entry$up_result)) nrow(entry$up_result) else 0L
      n_down <- if (!is.null(entry$down_result) && is.data.frame(entry$down_result)) nrow(entry$down_result) else 0L
      n_up + n_down
    }

    #' Build Enrichr Run Stats
    #'
    #' Executes build_enrichr_run_stats.
    #'
    #' @param all_results Argument for build_enrichr_run_stats.
    #'
    #' @return Return value produced by build_enrichr_run_stats.
    build_enrichr_run_stats <- function(all_results) {
      if (is.null(all_results) || length(all_results) == 0) {
        return(NULL)
      }

      rows <- lapply(names(all_results), function(key) {
        entry <- all_results[[key]]
        db_name <- if (!is.null(entry$db_name) && nzchar(entry$db_name)) entry$db_name else sub("^.*\\| ", "", key)
        count <- entry_significant_count(entry)
        db_total_terms <- suppressWarnings(as.integer(enrichr_db_term_counts_rv()[db_name]))
        pct <- round(((count / db_total_terms) * 100), 1)
        data.frame(
          `Gene set` = db_name,
          `Total terms in DB` = if (!is.na(db_total_terms) && db_total_terms > 0) db_total_terms else NA_integer_,
          `Significant pathways count` = count,
          `Significant pathways percent` = paste0(pct, " %"),
          check.names = FALSE
        )
      })

      do.call(rbind, rows)
    }

    #' Find Deg Cols
    #'
    #' Executes find_deg_cols.
    #'
    #' @param deg Argument for find_deg_cols.
    #'
    #' @return Return value produced by find_deg_cols.
    find_deg_cols <- function(deg) {
      nm <- tolower(colnames(deg))
      list(
        gene = which(nm %in% c("gene", "genesymbol", "symbol", "gene_symbol"))[1],
        lfc  = which(nm %in% c("logfc", "log2fc", "log2foldchange", "fc"))[1],
        pval = which(nm %in% c("p.value", "pvalue", "pval", "p_val"))[1],
        fdr  = which(nm %in% c("adj.p.val", "fdr", "padj", "adj_pval", "p.adj"))[1],
        t    = which(nm %in% c("t", "t_stat", "tstat", "stat", "statistic"))[1]
      )
    }

    #' Compute Rank Scores
    #'
    #' Executes compute_rank_scores.
    #'
    #' @param df Argument for compute_rank_scores.
    #' @param rank_metric Argument for compute_rank_scores.
    #' @param cols Argument for compute_rank_scores.
    #'
    #' @return Return value produced by compute_rank_scores.
    compute_rank_scores <- function(df, rank_metric, cols) {
      lfc <- suppressWarnings(as.numeric(df[[cols$lfc]]))
      pval <- if (!is.na(cols$pval)) suppressWarnings(as.numeric(df[[cols$pval]])) else rep(NA_real_, nrow(df))
      tstat <- if (!is.na(cols$t)) suppressWarnings(as.numeric(df[[cols$t]])) else rep(NA_real_, nrow(df))
      neglogp <- -log10(pmax(pval, .Machine$double.xmin))

      score <- switch(rank_metric,
        "log2FCxNegLog10Pval" = lfc * neglogp,
        "t" = ifelse(is.finite(tstat), tstat, lfc),
        lfc
      )
      list(score = score, lfc = lfc)
    }

    # --- Curated patterns for the Standard picker (Hallmark + all GO + KEGG + Reactome) ---
    standard_enrichr_patterns <- list(
      hallmark = "^MSigDB_Hallmark",
      go_bp    = "^GO_Biological_Process",
      go_mf    = "^GO_Molecular_Function",
      go_cc    = "^GO_Cellular_Component",
      kegg     = "KEGG",
      reactome = "Reactome"
    )

    # --- Curated patterns for the Other picker ---
    other_enrichr_patterns <- list(
      cellmarker   = "^CellMarker",
      chea         = "^ChEA",
      drug_pert    = "^Drug_Perturbations",
      dsigdb       = "^DSigDB",
      elsevier     = "^Elsevier",
      encode_tf    = "ENCODE_TF_ChIP",
      gene_pert    = "^Gene_Perturbations",
      gtex         = "^GTEx",
      oncogene_sig = "Oncogene_Signatures|MSigDB_Oncogenic_Signatures",
      wikipathways = "^WikiPathways"
    )

    #' Pick Latest Enrichr Db
    #'
    #' Executes pick_latest_enrichr_db.
    #'
    #' @param matches Argument for pick_latest_enrichr_db.
    #'
    #' @return Return value produced by pick_latest_enrichr_db.
    pick_latest_enrichr_db <- function(matches) {
      matches <- unique(as.character(matches))
      matches <- matches[!is.na(matches) & nzchar(matches)]
      if (length(matches) == 0L) {
        return(NULL)
      }

      year_pos <- regexpr("(19|20)[0-9]{2}", matches, perl = TRUE)
      years <- rep(NA_integer_, length(matches))
      has_year <- year_pos > 0
      if (any(has_year)) {
        year_len <- attr(year_pos, "match.length")
        year_text <- substring(
          matches[has_year],
          year_pos[has_year],
          year_pos[has_year] + year_len[has_year] - 1L
        )
        years[has_year] <- suppressWarnings(as.integer(year_text))
      }
      if (any(!is.na(years))) {
        max_year <- max(years, na.rm = TRUE)
        matches <- matches[years == max_year]
      }
      sort(matches, decreasing = TRUE)[[1L]]
    }

    # Pick best DB for a pattern: prefer species-neutral, fall back to detected species.
    #' Pick Best Enrichr Db
    #'
    #' Executes pick_best_enrichr_db.
    #'
    #' @param pattern Argument for pick_best_enrichr_db.
    #' @param pool Argument for pick_best_enrichr_db.
    #' @param species Argument for pick_best_enrichr_db.
    #'
    #' @return Return value produced by pick_best_enrichr_db.
    pick_best_enrichr_db <- function(pattern, pool, species = NULL) {
      matches <- grep(pattern, pool, ignore.case = TRUE, value = TRUE)
      if (length(matches) == 0L) {
        return(NULL)
      }
      neutral <- matches[!grepl("human|mouse|homo|_mus|_hsa|_mmu", matches, ignore.case = TRUE)]
      if (length(neutral) > 0L) {
        return(pick_latest_enrichr_db(neutral))
      }
      if (!is.null(species) && nzchar(species)) {
        sp_pat <- if (grepl("sapiens", species, ignore.case = TRUE)) "human|homo|_hsa" else if (grepl("musculus", species, ignore.case = TRUE)) "mouse|_mus|_mmu" else NULL
        if (!is.null(sp_pat)) {
          sp_m <- matches[grepl(sp_pat, matches, ignore.case = TRUE)]
          if (length(sp_m) > 0L) {
            return(pick_latest_enrichr_db(sp_m))
          }
        }
      }
      pick_latest_enrichr_db(matches)
    }

    # Return ALL non-redundant matches for a pattern (used for multi-DB families like GTEx).
    #' Pick all Enrichr Dbs
    #'
    #' Executes pick_all_enrichr_dbs.
    #'
    #' @param pattern Argument for pick_all_enrichr_dbs.
    #' @param pool Argument for pick_all_enrichr_dbs.
    #' @param species Argument for pick_all_enrichr_dbs.
    #'
    #' @return Return value produced by pick_all_enrichr_dbs.
    pick_all_enrichr_dbs <- function(pattern, pool, species = NULL) {
      matches <- grep(pattern, pool, ignore.case = TRUE, value = TRUE)
      if (length(matches) == 0L) {
        return(NULL)
      }
      neutral <- matches[!grepl("human|mouse|homo|_mus|_hsa|_mmu", matches, ignore.case = TRUE)]
      if (length(neutral) > 0L) {
        return(neutral)
      }
      if (!is.null(species) && nzchar(species)) {
        sp_pat <- if (grepl("sapiens", species, ignore.case = TRUE)) "human|homo|_hsa" else if (grepl("musculus", species, ignore.case = TRUE)) "mouse|_mus|_mmu" else NULL
        if (!is.null(sp_pat)) {
          sp_m <- matches[grepl(sp_pat, matches, ignore.case = TRUE)]
          if (length(sp_m) > 0L) {
            return(sp_m)
          }
        }
      }
      matches
    }

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
    #'
    #' @return Return value produced by compute_plot_height_px.
    compute_plot_height_px <- function(p, top_n) {
      base_h <- max(450, top_n * 35 + 180)
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

      label_col <- if ("term_label" %in% colnames(p$data)) {
        as.character(p$data$term_label)
      } else if ("pathway_label" %in% colnames(p$data)) {
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

    #' Build Enrichr Plot Bundle Safe
    #'
    #' Executes build_enrichr_plot_bundle_safe.
    #'
    #' @param entry Argument for build_enrichr_plot_bundle_safe.
    #' @param plot_type Argument for build_enrichr_plot_bundle_safe.
    #' @param top_up Argument for build_enrichr_plot_bundle_safe.
    #' @param top_down Argument for build_enrichr_plot_bundle_safe.
    #' @param pval_type Argument for build_enrichr_plot_bundle_safe.
    #'
    #' @return Return value produced by build_enrichr_plot_bundle_safe.
    build_enrichr_plot_bundle_safe <- function(entry, plot_type, top_up, top_down, pval_type = "logPval") {
      get("build_enrichr_plot_bundle", mode = "function")(
        entry = entry,
        plot_type = plot_type,
        top_up = top_up,
        top_down = top_down,
        pval_type = pval_type
      )
    }

    #' Save Enrichr Plot Bundle
    #'
    #' Executes save_enrichr_plot_bundle.
    #'
    #' @param entry Argument for save_enrichr_plot_bundle.
    #' @param plot_type Argument for save_enrichr_plot_bundle.
    #' @param top_up Argument for save_enrichr_plot_bundle.
    #' @param top_down Argument for save_enrichr_plot_bundle.
    #' @param pval_type Argument for save_enrichr_plot_bundle.
    #' @param out_dir Argument for save_enrichr_plot_bundle.
    #' @param comp_label Argument for save_enrichr_plot_bundle.
    #'
    #' @return Return value produced by save_enrichr_plot_bundle.
    save_enrichr_plot_bundle <- function(entry, plot_type, top_up, top_down, pval_type, out_dir, comp_label) {
      if (is.null(entry) || is.null(out_dir) || !nzchar(out_dir)) {
        return(invisible(NULL))
      }

      if (!identical(tolower(basename(out_dir)), "enrichr")) {
        out_dir <- file.path(out_dir, "enrichR")
      }
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
      bundle <- build_enrichr_plot_bundle_safe(entry, plot_type, top_up, top_down, pval_type = pval_type)
      db_name <- if (!is.null(entry$db_name) && nzchar(entry$db_name)) entry$db_name else "enrichr"
      file_prefix <- if (!is.null(entry$rnk_label) && nzchar(entry$rnk_label)) entry$rnk_label else comp_label
      comp_stub <- gsub("[^[:alnum:]_]+", "_", file_prefix)
      comp_stub <- gsub("^_+|_+$", "", comp_stub)
      db_stub <- gsub("[^[:alnum:]_]+", "_", db_name)
      db_stub <- gsub("^_+|_+$", "", db_stub)
      plot_stub <- if (identical(plot_type, "dot")) "dot" else "bar"

      #' Save One
      #'
      #' Executes save_one.
      #'
      #' @param plot_obj Argument for save_one.
      #' @param direction Argument for save_one.
      #'
      #' @return Return value produced by save_one.
      save_one <- function(plot_obj, direction) {
        if (!plot_has_data(plot_obj)) {
          return(invisible(NULL))
        }

        out_file <- file.path(
          out_dir,
          paste0(comp_stub, "_", db_stub, "_", plot_stub, "_", direction, ".pdf")
        )
        save_height <- max(5, compute_plot_height_px(plot_obj, max(top_up, top_down)) / 96)
        tryCatch(
          ggplot2::ggsave(
            filename = out_file,
            plot = plot_obj,
            width = 10,
            height = save_height,
            units = "in",
            dpi = 300,
            create.dir = TRUE
          ),
          error = function(e) NULL
        )
        invisible(NULL)
      }

      save_one(bundle$up, "up")
      save_one(bundle$down, "down")
      invisible(NULL)
    }

    output$current_comp_run <- renderText({
      comp <- tables$comp
      if (!hasText(comp)) "No active comparison yet" else comp
    })

    enrichr_db_choices_rv <- reactiveVal(character(0))
    enrichr_db_term_counts_rv <- reactiveVal(setNames(integer(0), character(0)))

    observe({
      if (!require("enrichR", quietly = TRUE, warn.conflicts = FALSE)) {
        return()
      }

      # Initialize the Enrichr connection (required before listEnrichrDbs works)
      tryCatch(enrichR::setEnrichrSite("Enrichr"), error = function(e) NULL)

      dbs <- tryCatch(enrichR::listEnrichrDbs(), error = function(e) NULL)
      if (is.null(dbs) || !is.data.frame(dbs) || nrow(dbs) == 0 ||
        !"libraryName" %in% colnames(dbs)) {
        return()
      }

      all_dbs <- collapse_latest_db_versions(as.character(dbs$libraryName))
      if (length(all_dbs) == 0) {
        return()
      }

      enrichr_db_choices_rv(all_dbs)
      enrichr_db_term_counts_rv(extract_enrichr_term_counts(dbs))
    })

    observe({
      all_dbs <- enrichr_db_choices_rv()
      if (length(all_dbs) == 0) {
        return()
      }

      species <- tables$species

      # Build standard picker: Hallmark + all GO + KEGG (latest, species-neutral preferred)
      std_dbs <- unique(unlist(Filter(Negate(is.null), lapply(standard_enrichr_patterns, function(pat) {
        pick_best_enrichr_db(pat, all_dbs, species)
      })), use.names = FALSE))
      std_dbs <- intersect(std_dbs, all_dbs)

      # Build other picker: curated functional/regulatory categories
      oth_dbs <- unique(unlist(Filter(Negate(is.null), lapply(other_enrichr_patterns, function(pat) {
        pick_best_enrichr_db(pat, all_dbs, species)
      })), use.names = FALSE))
      # GTEx has multiple non-redundant databases — include all of them
      gtex_all <- pick_all_enrichr_dbs("^GTEx", all_dbs, species)
      oth_dbs <- unique(c(oth_dbs, gtex_all))
      oth_dbs <- setdiff(intersect(oth_dbs, all_dbs), std_dbs)

      all_curated_dbs <- sort(c(std_dbs, oth_dbs))

      current_selection <- isolate(input$enrichr_db_all)
      if (is.null(current_selection)) current_selection <- character(0)
      selected_dbs_picker <- intersect(current_selection, all_curated_dbs)
      if (length(selected_dbs_picker) == 0L) {
        hallmark_db <- grep("^MSigDB_Hallmark", all_curated_dbs, ignore.case = TRUE, value = TRUE)
        selected_dbs_picker <- if (length(hallmark_db) > 0L) hallmark_db[[1L]] else character(0)
      }

      db_count_map <- enrichr_db_term_counts_rv()
      db_labels <- vapply(all_curated_dbs, function(db) {
        db_label_with_count(db, db_count_map)
      }, character(1))

      updatePickerInput(session, "enrichr_db_all",
        choices  = setNames(all_curated_dbs, db_labels),
        selected = selected_dbs_picker
      )
    })

    #' Perform Enrichr Run
    #'
    #' Executes perform_enrichr_run.
    #'
    #' @return Return value produced by perform_enrichr_run.
    perform_enrichr_run <- function() {
      proceed_with_enrichr(FALSE)
      enrichr_run_stats_rv(NULL)

      if (!require("enrichR", quietly = TRUE, warn.conflicts = FALSE)) {
        flashMessage("EnrichR Error",
          "Package 'enrichR' is not installed. Please install it and rerun.",
          type = "error"
        )
        return(invisible(NULL))
      }

      selected_dbs <- input$enrichr_db_all
      if (is.null(selected_dbs)) selected_dbs <- character(0)
      selected_dbs <- unique(selected_dbs)
      if (is.null(selected_dbs) || length(selected_dbs) == 0) {
        flashMessage("EnrichR Error", "Please select at least one EnrichR database.", type = "error")
        return(invisible(NULL))
      }

      deg <- tables$DEG
      if (is.null(deg) || !is.data.frame(deg) || nrow(deg) == 0) {
        flashMessage("EnrichR Error", "No DEG table is available to run EnrichR.", type = "error")
        return(invisible(NULL))
      }

      cols_all <- find_deg_cols(deg)
      if (is.na(cols_all$gene) || is.na(cols_all$lfc)) {
        flashMessage("EnrichR Error", "Required gene/log2FC columns were not found in DEG table.", type = "error")
        return(invisible(NULL))
      }

      rank_metric <- if (!is.null(input$rank_metric) && nzchar(input$rank_metric)) {
        input$rank_metric
      } else {
        "log2FCxNegLog10Pval"
      }

      top_gene_n <- suppressWarnings(as.integer(input$enrichr_top_genes))
      if (!is.finite(top_gene_n) || is.na(top_gene_n) || !top_gene_n %in% c(100L, 250L, 500L)) {
        top_gene_n <- 250L
      }

      scores <- compute_rank_scores(deg, rank_metric, cols_all)
      lfc <- scores$lfc
      score <- scores$score
      genes <- as.character(deg[[cols_all$gene]])

      up_idx <- which(is.finite(lfc) & lfc > 0 & !is.na(score))
      down_idx <- which(is.finite(lfc) & lfc < 0 & !is.na(score))
      if (length(up_idx) > 0) up_idx <- up_idx[order(score[up_idx], decreasing = TRUE)]
      if (length(down_idx) > 0) down_idx <- down_idx[order(score[down_idx], decreasing = FALSE)]

      top_n <- top_gene_n
      up_keep <- head(up_idx, top_n)
      down_keep <- head(down_idx, top_n)

      genes_up <- unique(genes[up_keep])
      genes_down <- unique(genes[down_keep])
      genes_up <- genes_up[!is.na(genes_up) & nzchar(genes_up)]
      genes_down <- genes_down[!is.na(genes_down) & nzchar(genes_down)]

      if (length(genes_up) == 0 && length(genes_down) == 0) {
        enrichr_run_status_rv(list(
          n_up = 0L, n_down = 0L,
          top_gene_n = top_gene_n,
          warnings = list(
            list(type = "none", msg = "No Up genes found."),
            list(type = "none", msg = "No Down genes found.")
          )
        ))
        flashMessage("EnrichR Error", "No genes available for EnrichR. Check that the DEG table has valid log2FC values.", type = "error")
        return(invisible(NULL))
      }

      # Build per-direction gene-count warnings for inline dashboard display
      run_warnings <- list()
      #' Check Direction
      #'
      #' Executes check_direction.
      #'
      #' @param g Argument for check_direction.
      #' @param label Argument for check_direction.
      #'
      #' @return Return value produced by check_direction.
      check_direction <- function(g, label) {
        n <- length(g)
        if (n == 0) {
          run_warnings[[length(run_warnings) + 1L]] <<- list(
            type = "none",
            msg  = paste0("No ", label, " genes found \u2014 EnrichR will not be run for this direction.")
          )
        } else if (n < 20L) {
          run_warnings[[length(run_warnings) + 1L]] <<- list(
            type = "unreliable",
            msg  = paste0(label, " genes: <b>", n, "</b> \u2014 test is unreliable with fewer than 20 genes.")
          )
        } else if (n < 50L) {
          run_warnings[[length(run_warnings) + 1L]] <<- list(
            type = "underpowered",
            msg  = paste0(label, " genes: <b>", n, "</b> \u2014 test may be underpowered with fewer than 50 genes.")
          )
        }
      }
      check_direction(genes_up, "Up")
      check_direction(genes_down, "Down")

      enrichr_run_status_rv(list(
        n_up = length(genes_up),
        n_down = length(genes_down),
        top_gene_n = top_gene_n,
        warnings = run_warnings
      ))

      comp_label <- if (hasText(tables$comp)) as.character(tables$comp) else "comparison"
      file_prefix <- paste0("RNAseq_", comp_label, "_", rank_metric)
      out_dir <- active_output_dir()

      all_results <- NULL
      run_err <- NULL

      withProgress(message = "Running EnrichR", value = 0, {
        completed_steps <- 0L
        all_results <- tryCatch(
          run_enrichr(
            deg = deg,
            selected_dbs = selected_dbs,
            genes_up = genes_up,
            genes_down = genes_down,
            file_prefix = file_prefix,
            comp_label = comp_label,
            out_dir = out_dir,
            progress_callback = function(step, total, db_name) {
              completed_steps <<- completed_steps + 1L
              incProgress(1 / total,
                detail = paste0("Querying ", db_name, " (", completed_steps, " of ", total, ")")
              )
            }
          ),
          error = function(e) {
            run_err <<- conditionMessage(e)
            list()
          }
        )
      })

      if (!is.null(run_err)) {
        flashMessage("EnrichR Error", run_err, type = "error")
        return(invisible(NULL))
      }

      enrichr_results_rv(all_results)
      enrichr_stats <- build_enrichr_run_stats(all_results)
      enrichr_run_stats_rv(enrichr_stats)

      if (length(all_results) == 0) {
        updatePickerInput(session, "plot_result_keys", choices = character(0), selected = character(0))
        flashMessage("EnrichR Error", "No EnrichR results were produced.", type = "error")
        return(invisible(NULL))
      }

      keys <- names(all_results)
      updatePickerInput(session, "plot_result_keys",
        choices = setNames(keys, keys),
        selected = keys
      )
      generated_plot_state_rv(list(
        keys = character(0),
        top_up = 100L,
        top_down = 100L,
        plot_types = c("dot", "bar"),
        pval_type = "logPval"
      ))
      expandBox("enrichr_plot_settings_box", default = TRUE)
      collapseBox("enrichr_plots_box", default = TRUE)
      flashMessage("EnrichR Complete", "Analysis finished. Results saved to analysis directories.",
        type = "success", timer = 3000
      )
    }

    observeEvent(input$run_button, {
      selected_dbs_btn <- input$enrichr_db_all
      if (is.null(selected_dbs_btn)) selected_dbs_btn <- character(0)
      total_dbs <- length(unique(selected_dbs_btn))

      run_warnings <- list()
      if (total_dbs >= 8) {
        run_warnings[[length(run_warnings) + 1L]] <- paste0(
          "Selecting many databases (", total_dbs, " total) will increase processing time significantly."
        )
      }
      if (!is.null(tables$species) && identical(tables$species, "Other")) {
        run_warnings[[length(run_warnings) + 1L]] <- paste0(
          "Current species is 'Other'. EnrichR databases are identifier- and species-dependent; ",
          "results may be empty or misleading unless your gene identifiers match the selected EnrichR libraries."
        )
      }

      if (length(run_warnings) > 0 && !isTRUE(proceed_with_enrichr())) {
        showModal(modalDialog(
          title = "EnrichR Run Warning",
          size = "m",
          easyClose = FALSE,
          footer = tagList(
            modalButton("Cancel"),
            actionButton(session$ns("confirm_enrichr_proceed"), "Proceed", class = "btn-warning")
          ),
          div(
            h4("Review before running EnrichR"),
            tags$ul(lapply(run_warnings, tags$li)),
            p("Click 'Proceed' to run EnrichR now.")
          )
        ))
        return(invisible(NULL))
      }

      perform_enrichr_run()
    })

    output$enrichr_run_stats_ui <- renderUI({
      stats <- enrichr_run_stats_rv()
      if (is.null(stats) || !is.data.frame(stats) || nrow(stats) == 0) {
        return(NULL)
      }
      fluidRow(box(
        width = 12, title = "Summary", solidHeader = TRUE,
        closable = FALSE, collapsible = TRUE, status = "primary",
        div(DT::dataTableOutput(session$ns("enrichr_run_stats_table"), width = "80%")),
        tags$br(),
        tags$div(
          style = "margin-top:6px; margin-bottom:10px; font-size:13px; color:#555;",
          tags$strong("Note: "),
          tags$em("Total terms in DB"), " indicates the total number of terms available in each EnrichR library. ",
          "Significant pathway counts are based on the filtered EnrichR results table using ",
          tags$em("P.value < 0.05"), "."
        )
      ))
    })

    output$enrichr_run_stats_table <- DT::renderDT({
      stats <- enrichr_run_stats_rv()
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

    # Inline gene-count summary + warnings shown in the EnrichR Settings box
    output$enrichr_run_status <- renderUI({
      status <- enrichr_run_status_rv()
      if (is.null(status)) {
        return(NULL)
      }

      #' Make Enrichr Msg
      #'
      #' Executes make_enrichr_msg.
      #'
      #' @param icon_name Argument for make_enrichr_msg.
      #' @param icon_color Argument for make_enrichr_msg.
      #' @param bg Argument for make_enrichr_msg.
      #' @param border Argument for make_enrichr_msg.
      #' @param text_html Argument for make_enrichr_msg.
      #'
      #' @return Return value produced by make_enrichr_msg.
      make_enrichr_msg <- function(icon_name, icon_color, bg, border, text_html) {
        div(
          style = paste0(
            "display:flex; align-items:flex-start; gap:8px; ",
            "padding:8px 12px; background:", bg, "; border-left:4px solid ", border,
            "; border-radius:4px; margin:4px 0; font-size:14px; line-height:1.5;"
          ),
          tags$i(class = paste("fa fa-", icon_name), style = paste0("color:", icon_color, "; margin-top:2px; flex-shrink:0;")),
          HTML(text_html)
        )
      }

      items <- list()

      # Gene count summary line
      items[[1L]] <- make_enrichr_msg(
        icon_name = "dna",
        icon_color = "#1a7a55",
        bg = "#eaf7f2",
        border = "#1a7a55",
        text_html = paste0(
          "<b>Genes submitted to EnrichR &mdash;</b> ",
          "Requested: <b>Top ", status$top_gene_n, "</b> per direction&nbsp;&nbsp;|&nbsp;&nbsp;",
          "Up: <b>", status$n_up, "</b>&nbsp;&nbsp;|&nbsp;&nbsp;",
          "Down: <b>", status$n_down, "</b>"
        )
      )

      type_styles <- list(
        none         = list(icon = "info-circle", icon_col = "#757575", bg = "#f5f5f5", border = "#9e9e9e"),
        underpowered = list(icon = "exclamation-triangle", icon_col = "#b36b00", bg = "#fffbe6", border = "#f0a500"),
        unreliable   = list(icon = "exclamation-triangle", icon_col = "#b03a00", bg = "#fff3ee", border = "#d9500a")
      )

      for (w in status$warnings) {
        sty <- type_styles[[w$type]]
        items[[length(items) + 1L]] <- make_enrichr_msg(
          icon_name  = sty$icon,
          icon_color = sty$icon_col,
          bg         = sty$bg,
          border     = sty$border,
          text_html  = w$msg
        )
      }

      do.call(tagList, items)
    })

    # Create description
    output$plot_display_desc <- renderUI({
      desc <- switch(input$plot_type_display,
        "bar" = tagList(
          "View ", tags$em("-log10Pval"), " or ", tags$em("-log10Padj"),
          " (", tags$strong("x axis"), "), and ", tags$em("Gene Ratio"),
          " (", tags$strong("color gradient"), ") for top pathways"
        ),
        "dot" = tagList(
          "View ", tags$em("-log10Pval"), " or ", tags$em("-log10Padj"),
          " (", tags$strong("x axis"), "), ", tags$em("Gene Ratio"),
          " (", tags$strong("color gradient"), "), and number of DEGs overlapping with pathway genes as Gene Count (",
          tags$strong("dot size"), ") for top pathways"
        )
      )
      tags$span(style = "font-size: 15px; color:navy; font-weight: 500", desc)
    })


    observeEvent(input$confirm_enrichr_proceed, {
      removeModal()
      proceed_with_enrichr(TRUE)
      perform_enrichr_run()
    })

    output$result_tabs <- renderUI({
      plot_state <- generated_plot_state_rv()
      keys <- plot_state$keys
      if (is.null(keys) || length(keys) == 0) {
        return(NULL)
      }

      current <- input$result_key_tab
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
        id = session$ns("result_key_tab"),
        type = "tabs",
        selected = selected
      ), tabs))
    })

    output$enrichr_table <- DT::renderDT({
      req(input$result_key_tab)
      all_results <- enrichr_results_rv()
      req(length(all_results) > 0)
      entry <- all_results[[input$result_key_tab]]
      tbl <- enrichr_bind_direction_results(entry$up_result, entry$down_result)

      DT::datatable(
        tbl,
        rownames = FALSE,
        class = "stripe hover compact nowrap",
        options = list(pageLength = 20, scrollX = TRUE)
      )
    })

    observeEvent(input$generate_plot, {
      all_results <- enrichr_results_rv()
      req(length(all_results) > 0)

      selected_keys <- input$plot_result_keys
      if (is.null(selected_keys) || length(selected_keys) == 0) {
        flashMessage("EnrichR Error", "Select at least one result to generate plot(s).", type = "error")
        return(invisible(NULL))
      }

      available_keys <- intersect(selected_keys, names(all_results))
      if (length(available_keys) == 0) {
        flashMessage("EnrichR Error", "Selected results are not available. Please re-run EnrichR.", type = "error")
        return(invisible(NULL))
      }

      plot_types <- active_plot_types_enrichr()
      if (is.null(plot_types) || length(plot_types) == 0) {
        flashMessage("EnrichR Error", "Select at least one plot type to generate.", type = "error")
        return(invisible(NULL))
      }

      top_n <- suppressWarnings(as.integer(input$top_n))
      if (!is.finite(top_n) || is.na(top_n)) top_n <- 10L
      top_n <- max(5L, min(25L, top_n))

      bar_pv <- if (!is.null(input$bar_pval_type)) input$bar_pval_type else "logPval"
      dot_pv <- if (!is.null(input$dot_pval_type)) input$dot_pval_type else "logPval"

      generated_plot_state_rv(list(
        keys          = available_keys,
        top_up        = top_n,
        top_down      = top_n,
        plot_types    = plot_types,
        bar_pval_type = bar_pv,
        dot_pval_type = dot_pv
      ))
      expandBox("enrichr_plots_box", default = TRUE)

      comp_label <- if (hasText(tables$comp)) as.character(tables$comp) else "comparison"
      for (key in available_keys) {
        entry <- all_results[[key]]
        for (ptype in plot_types) {
          pv <- if (identical(ptype, "dot")) dot_pv else bar_pv
          save_enrichr_plot_bundle(
            entry = entry,
            plot_type = ptype,
            top_up = top_n,
            top_down = top_n,
            pval_type = pv,
            out_dir = entry$out_dir,
            comp_label = comp_label
          )
        }
      }

      all_choices <- c("Dot Plot" = "dot", "Bar Plot" = "bar")
      display_choices <- all_choices[all_choices %in% plot_types]
      current_display <- input$plot_type_display
      selected_display <- if (!is.null(current_display) && current_display %in% plot_types) {
        current_display
      } else {
        plot_types[1]
      }
      updateRadioButtons(session, "plot_type_display", choices = display_choices, selected = selected_display)

      current <- input$result_key_tab
      if (is.null(current) || !current %in% available_keys) current <- available_keys[1]
      updateTabsetPanel(session, "result_key_tab", selected = current)
    })

    current_enrichr_plots <- reactive({
      req(input$result_key_tab)
      all_results <- enrichr_results_rv()
      req(length(all_results) > 0)

      plot_state <- generated_plot_state_rv()
      req(!is.null(plot_state$keys), length(plot_state$keys) > 0)
      req(input$result_key_tab %in% plot_state$keys)

      entry <- all_results[[input$result_key_tab]]
      req(!is.null(entry))

      top_up <- plot_state$top_up
      top_down <- plot_state$top_down
      generated_types <- if (!is.null(plot_state$plot_types) && length(plot_state$plot_types) > 0) {
        plot_state$plot_types
      } else {
        c("bar")
      }
      plot_type <- input$plot_type_display
      if (is.null(plot_type) || !plot_type %in% generated_types) {
        plot_type <- generated_types[1]
      }
      pval_type <- if (identical(plot_type, "dot")) {
        if (!is.null(plot_state$dot_pval_type)) plot_state$dot_pval_type else "logPval"
      } else {
        if (!is.null(plot_state$bar_pval_type)) plot_state$bar_pval_type else "logPval"
      }
      build_enrichr_plot_bundle_safe(entry, plot_type, top_up, top_down, pval_type = pval_type)
    })

    output$enrichr_plot_ui <- renderUI({
      plot_state <- generated_plot_state_rv()
      display_type <- input$plot_type_display
      use_plotly <- !identical(display_type, "dot")
      fallback_topn <- 10L
      if (!is.null(plot_state$top_up) && is.finite(plot_state$top_up)) {
        fallback_topn <- as.integer(plot_state$top_up)
      }
      if (!is.null(plot_state$top_down) && is.finite(plot_state$top_down)) {
        fallback_topn <- max(fallback_topn, as.integer(plot_state$top_down))
      }

      bundle <- tryCatch(current_enrichr_plots(), error = function(e) NULL)

      tagList(
        if (!is.null(bundle$up)) {
          if (use_plotly) {
            plotly::plotlyOutput(
              session$ns("enrichr_plot_up"),
              height = paste0(compute_plot_height_px(bundle$up, fallback_topn), "px"),
              width = "100%"
            )
          } else {
            up_width_px <- compute_plot_width_px(bundle$up, fallback_topn)
            plotOutput(
              session$ns("enrichr_plot_up_static"),
              height = paste0(compute_plot_height_px(bundle$up, fallback_topn), "px"),
              width = paste0(up_width_px, "px")
            )
          }
        },
        if (!is.null(bundle$up) && !is.null(bundle$down)) {
          div(style = "height: 2.2em;")
        },
        if (!is.null(bundle$down)) {
          if (use_plotly) {
            plotly::plotlyOutput(
              session$ns("enrichr_plot_down"),
              height = paste0(compute_plot_height_px(bundle$down, fallback_topn), "px"),
              width = "100%"
            )
          } else {
            down_width_px <- compute_plot_width_px(bundle$down, fallback_topn)
            plotOutput(
              session$ns("enrichr_plot_down_static"),
              height = paste0(compute_plot_height_px(bundle$down, fallback_topn), "px"),
              width = paste0(down_width_px, "px")
            )
          }
        }
      )
    })

    observe({
      plot_bundle <- current_enrichr_plots()
      plot_state <- generated_plot_state_rv()

      fallback_topn <- 10L

      if (!is.null(plot_state$top_up) && is.finite(plot_state$top_up)) {
        fallback_topn <- as.integer(plot_state$top_up)
      }

      if (!is.null(plot_state$top_down) && is.finite(plot_state$top_down)) {
        fallback_topn <- max(fallback_topn, as.integer(plot_state$top_down))
      }

      if (!is.null(plot_bundle$up)) {
        local({
          p <- plot_bundle$up
          h <- compute_plot_height_px(p, fallback_topn)
          w <- compute_plot_width_px(p, fallback_topn)
          is_dot <- identical(input$plot_type_display, "dot")

          if (is_dot) {
            output$enrichr_plot_up_static <- renderPlot(
              {
                print(p)
              },
              height = function() h,
              width = function() w,
              res = 100
            )
          } else {
            output$enrichr_plot_up <- plotly::renderPlotly({
              fig <- make_enrichr_bar_plotly(p, height = h)
              plotly::config(fig, toImageButtonOptions = list(
                format = "png",
                filename = "enrichr_plot_up",
                width = max(800, w),
                height = h
              ))
            })
          }
        })
      }

      if (!is.null(plot_bundle$down)) {
        local({
          p <- plot_bundle$down
          h <- compute_plot_height_px(p, fallback_topn)
          w <- compute_plot_width_px(p, fallback_topn)
          is_dot <- identical(input$plot_type_display, "dot")

          if (is_dot) {
            output$enrichr_plot_down_static <- renderPlot(
              {
                print(p)
              },
              height = function() h,
              width = function() w,
              res = 100
            )
          } else {
            output$enrichr_plot_down <- plotly::renderPlotly({
              fig <- make_enrichr_bar_plotly(p, height = h)
              plotly::config(fig, toImageButtonOptions = list(
                format = "png",
                filename = "enrichr_plot_down",
                width = max(800, w),
                height = h
              ))
            })
          }
        })
      }
    })
  })
}
