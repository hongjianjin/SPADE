library(ggplot2)

#' Build range-aware x-axis breaks for EnrichR plots
#'
#' Uses pretty breaks so tick spacing adapts to the plotted -log10(P-value)
#' range rather than relying on fixed-width intervals.
#'
#' @return A breaks function suitable for ggplot2 scale_x_continuous().
enrichr_x_breaks <- function() {
  scales::pretty_breaks(n = 6)
}

#' Check Whether a ggplot Contains Plot Data
#'
#' Distinguishes real pathway plots from placeholder ggplots used to display
#' missing-direction messages.
#'
#' @param p Plot object.
#'
#' @return Logical flag indicating whether `p$data` is a non-empty data frame.
enrichr_plot_has_data <- function(p) {
  inherits(p, "gg") && is.data.frame(p$data) && nrow(p$data) > 0
}

#' Create an Empty EnrichR Placeholder Plot
#'
#' @param label Message displayed in the empty plot canvas.
#'
#' @return A ggplot object with a centered message and void theme.
enrichr_empty_plot <- function(label = "No enriched pathways found.") {
  p <- ggplot() +
    annotate("text",
      x = 0.5, y = 0.5,
      label = stringr::str_wrap(label, width = 55),
      size = 5.5,
      color = "grey35",
      fontface = "italic"
    ) +
    xlim(0, 1) +
    ylim(0, 1) +
    theme_void()
  attr(p, "shinyRNAseq_empty_plot") <- TRUE
  p
}

#' Bind EnrichR up/down tables with a Direction column
#'
#' @param up_df EnrichR result table for upregulated genes.
#' @param down_df EnrichR result table for downregulated genes.
#'
#' @return Data frame combining both directions when available.
enrichr_bind_direction_results <- function(up_df, down_df) {
  up_tbl <- if (!is.null(up_df) && is.data.frame(up_df) && nrow(up_df) > 0) {
    cbind(Direction = "Up", up_df, stringsAsFactors = FALSE)
  } else {
    NULL
  }
  down_tbl <- if (!is.null(down_df) && is.data.frame(down_df) && nrow(down_df) > 0) {
    cbind(Direction = "Down", down_df, stringsAsFactors = FALSE)
  } else {
    NULL
  }

  if (is.null(up_tbl) && is.null(down_tbl)) {
    return(data.frame())
  }
  if (is.null(up_tbl)) {
    return(down_tbl)
  }
  if (is.null(down_tbl)) {
    return(up_tbl)
  }

  all_cols <- union(colnames(up_tbl), colnames(down_tbl))
  for (nm in setdiff(all_cols, colnames(up_tbl))) up_tbl[[nm]] <- NA
  for (nm in setdiff(all_cols, colnames(down_tbl))) down_tbl[[nm]] <- NA
  up_tbl <- up_tbl[, all_cols, drop = FALSE]
  down_tbl <- down_tbl[, all_cols, drop = FALSE]
  rbind(up_tbl, down_tbl)
}

#' Filter EnrichR Results by Nominal P-value
#'
#' Applies the saved-table cutoff used before EnrichR plotting. EnrichR output
#' tables are expected to contain a `P.value` column.
#'
#' @param df EnrichR result table.
#' @param pval_cutoff Nominal p-value cutoff. Use `Inf` to keep all rows.
#'
#' @return EnrichR table filtered to `P.value < pval_cutoff`.
filter_enrichr_results_by_pvalue <- function(df, pval_cutoff = 0.05) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
    return(df)
  }
  if (!"P.value" %in% colnames(df)) {
    return(df)
  }

  pval_cutoff <- suppressWarnings(as.numeric(pval_cutoff))
  if (!is.finite(pval_cutoff)) {
    return(df)
  }

  df$P.value <- suppressWarnings(as.numeric(df$P.value))
  df[!is.na(df$P.value) & df$P.value < pval_cutoff, , drop = FALSE]
}

#' Build the zero-result EnrichR threshold message
#'
#' @param pval_cutoff Nominal p-value cutoff used for the result table.
#'
#' @return Character message for empty filtered result tables.
enrichr_empty_result_message <- function(pval_cutoff = 0.05) {
  paste0("No pathways passed significance threshold of pvalue<", pval_cutoff)
}

#' Add an explanatory row to empty EnrichR saved tables
#'
#' Keeps empty filtered results usable for plotting/counting while writing a
#' visible message to disk when no term passes the significance threshold.
#'
#' @param df Filtered EnrichR result table.
#' @param pval_cutoff Nominal p-value cutoff used for filtering.
#'
#' @return Data frame suitable for `write.table()`.
format_enrichr_table_for_save <- function(df, pval_cutoff = 0.05) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) > 0) {
    return(df)
  }

  if (ncol(df) == 0) {
    return(data.frame(Message = enrichr_empty_result_message(pval_cutoff), check.names = FALSE))
  }

  out <- as.data.frame(lapply(df, function(x) NA), stringsAsFactors = FALSE, check.names = FALSE)
  names(out) <- colnames(df)
  out$Message <- enrichr_empty_result_message(pval_cutoff)
  out
}

#' Run EnrichR for selected databases
#'
#' @param deg Differential expression data frame.
#' @param selected_dbs Character vector of EnrichR database names.
#' @param cut_lfc Absolute log2FC cutoff.
#' @param cut_pval P-value cutoff.
#' @param cut_fdr FDR cutoff.
#' @param enrichment_pval_cutoff Nominal EnrichR result p-value cutoff applied
#'   to saved tables and plots.
#' @param top_up Top-N setting used to cap input up genes to EnrichR.
#' @param top_down Top-N setting used to cap input down genes to EnrichR.
#' @param genes_up Optional explicit upregulated gene vector to query EnrichR.
#' @param genes_down Optional explicit downregulated gene vector to query EnrichR.
#' @param comp_label Comparison label used in output filenames and result keys.
#' @param out_dir Output directory for EnrichR TSV tables.
#' @param progress_callback Optional callback called as fn(step, total, db_name).
#'
#' @return Named list of EnrichR results, one element per comparison/database key.
run_enrichr <- function(deg, selected_dbs,
                        cut_lfc = 1, cut_pval = 0.05, cut_fdr = 0.05,
                        enrichment_pval_cutoff = 0.05,
                        top_up = 100L, top_down = 100L,
                        genes_up = NULL, genes_down = NULL,
                        file_prefix = NULL,
                        comp_label = "comparison", out_dir = tempdir(),
                        progress_callback = NULL) {
  if (is.null(selected_dbs) || length(selected_dbs) == 0) {
    stop("Please select at least one EnrichR database.")
  }
  if (is.null(deg) || !is.data.frame(deg) || nrow(deg) == 0) {
    stop("No DEG results found. Please complete differential expression analysis first.")
  }

  # Identify gene symbol column
  gene_col <- which(tolower(colnames(deg)) %in% c("gene", "genesymbol", "symbol", "gene_symbol"))[1]
  if (is.na(gene_col)) gene_col <- 1L

  # Identify logFC, pval, FDR columns (case-insensitive)
  lfc_col <- which(tolower(colnames(deg)) %in% c("logfc", "log2fc", "log2foldchange", "fc"))[1]
  pval_col <- which(tolower(colnames(deg)) %in% c("p.value", "pvalue", "pval", "p_val"))[1]
  fdr_col <- which(tolower(colnames(deg)) %in% c("adj.p.val", "fdr", "padj", "adj_pval", "p.adj"))[1]

  if (is.na(lfc_col)) {
    stop("Could not find a log2FC column in the DEG table.")
  }

  cut_lfc <- suppressWarnings(as.numeric(cut_lfc))
  cut_pval <- suppressWarnings(as.numeric(cut_pval))
  cut_fdr <- suppressWarnings(as.numeric(cut_fdr))
  enrichment_pval_cutoff <- suppressWarnings(as.numeric(enrichment_pval_cutoff))
  if (!is.finite(cut_lfc)) cut_lfc <- 1
  if (!is.finite(cut_pval)) cut_pval <- 0.05
  if (!is.finite(cut_fdr)) cut_fdr <- 0.05
  if (!is.finite(enrichment_pval_cutoff)) enrichment_pval_cutoff <- 0.05

  top_up <- suppressWarnings(as.integer(top_up))
  top_down <- suppressWarnings(as.integer(top_down))
  if (!is.finite(top_up) || is.na(top_up)) top_up <- 100L
  if (!is.finite(top_down) || is.na(top_down)) top_down <- 100L
  top_up <- max(10L, min(500L, top_up))
  top_down <- max(10L, min(500L, top_down))

  if (is.null(genes_up) && is.null(genes_down)) {
    genes_all <- as.character(deg[[gene_col]])
    lfc_vals <- suppressWarnings(as.numeric(deg[[lfc_col]]))
    pval_vals <- if (!is.na(pval_col)) suppressWarnings(as.numeric(deg[[pval_col]])) else rep(0, nrow(deg))
    fdr_vals <- if (!is.na(fdr_col)) suppressWarnings(as.numeric(deg[[fdr_col]])) else rep(0, nrow(deg))

    pass_pval <- is.finite(pval_vals) & pval_vals <= cut_pval
    pass_fdr <- is.finite(fdr_vals) & fdr_vals <= cut_fdr
    pass_lfc <- is.finite(lfc_vals) & abs(lfc_vals) >= cut_lfc
    pass_all <- pass_lfc & pass_pval & pass_fdr

    genes_up <- genes_all[pass_all & lfc_vals > 0]
    genes_down <- genes_all[pass_all & lfc_vals < 0]
    genes_up <- head(unique(genes_up[!is.na(genes_up) & nzchar(genes_up)]), top_up * 5L)
    genes_down <- head(unique(genes_down[!is.na(genes_down) & nzchar(genes_down)]), top_down * 5L)
  } else {
    if (is.null(genes_up)) genes_up <- character(0)
    if (is.null(genes_down)) genes_down <- character(0)
    genes_up <- unique(as.character(genes_up))
    genes_down <- unique(as.character(genes_down))
    genes_up <- genes_up[!is.na(genes_up) & nzchar(genes_up)]
    genes_down <- genes_down[!is.na(genes_down) & nzchar(genes_down)]
  }

  if (length(genes_up) == 0 && length(genes_down) == 0) {
    stop("No genes pass the current DEG cutoffs. Try relaxing log2FC or p-value thresholds.")
  }

  out_dir <- file.path(out_dir, "enrichR")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  if (is.null(file_prefix) || !nzchar(file_prefix)) {
    file_prefix <- comp_label
  }
  file_prefix <- gsub("[^[:alnum:]_]+", "_", file_prefix)
  file_prefix <- gsub("^_+|_+$", "", file_prefix)

  all_results <- list()
  total_steps <- length(selected_dbs)

  for (i in seq_along(selected_dbs)) {
    db_name <- selected_dbs[[i]]
    if (!is.null(progress_callback) && is.function(progress_callback)) {
      progress_callback(i, total_steps, db_name)
    }

    file_stub <- gsub("[^[:alnum:]_]", "_", db_name)
    res_up <- NULL
    res_down <- NULL

    if (length(genes_up) > 0) {
      enr_up <- tryCatch(enrichR::enrichr(genes_up, databases = db_name), error = function(e) NULL)
      if (!is.null(enr_up) && !is.null(enr_up[[db_name]]) &&
        is.data.frame(enr_up[[db_name]]) && nrow(enr_up[[db_name]]) > 0) {
        res_up <- filter_enrichr_results_by_pvalue(enr_up[[db_name]],
          pval_cutoff = enrichment_pval_cutoff
        )
      }
    }

    if (length(genes_down) > 0) {
      enr_down <- tryCatch(enrichR::enrichr(genes_down, databases = db_name), error = function(e) NULL)
      if (!is.null(enr_down) && !is.null(enr_down[[db_name]]) &&
        is.data.frame(enr_down[[db_name]]) && nrow(enr_down[[db_name]]) > 0) {
        res_down <- filter_enrichr_results_by_pvalue(enr_down[[db_name]],
          pval_cutoff = enrichment_pval_cutoff
        )
      }
    }

    if (is.null(res_up) && is.null(res_down)) next

    key <- paste0(comp_label, " | ", db_name)
    all_results[[key]] <- list(
      up_result = res_up,
      down_result = res_down,
      out_dir = out_dir,
      rnk_label = file_prefix,
      db_name = db_name,
      top_n_up = top_up,
      top_n_down = top_down
    )
  }

  # Consolidate UP and DOWN results into separate Excel workbooks
  if (length(all_results) > 0) {
    wb_up <- openxlsx::createWorkbook()
    wb_down <- openxlsx::createWorkbook()
    has_up <- FALSE
    has_down <- FALSE
    for (key in names(all_results)) {
      entry <- all_results[[key]]
      sheet <- substr(entry$db_name, 1, 31)
      if (!is.null(entry$up_result) && is.data.frame(entry$up_result) && nrow(entry$up_result) > 0) {
        openxlsx::addWorksheet(wb_up, sheet)
        openxlsx::writeData(wb_up, sheet, entry$up_result)
        has_up <- TRUE
      }
      if (!is.null(entry$down_result) && is.data.frame(entry$down_result) && nrow(entry$down_result) > 0) {
        openxlsx::addWorksheet(wb_down, sheet)
        openxlsx::writeData(wb_down, sheet, entry$down_result)
        has_down <- TRUE
      }
    }
    if (has_up) {
      tryCatch(openxlsx::saveWorkbook(wb_up,
        file.path(out_dir, paste0(file_prefix, "_UP_enrichR.xlsx")),
        overwrite = TRUE
      ), error = function(e) NULL)
    }
    if (has_down) {
      tryCatch(openxlsx::saveWorkbook(wb_down,
        file.path(out_dir, paste0(file_prefix, "_DOWN_enrichR.xlsx")),
        overwrite = TRUE
      ), error = function(e) NULL)
    }
  }

  all_results
}

#' Prepare EnrichR Table for Plotting
#'
#' Parses overlap ratios, computes plotting scores, and formats wrapped pathway
#' labels for bar/dot plots. The input table is expected to already be filtered
#' by nominal EnrichR p-value.
#'
#' @param df EnrichR result table.
#' @param N Number of top terms to keep.
#' @param pval_type Significance metric to plot: `"logPval"` or `"logPadj"`.
#' @param keep_all If `TRUE`, return all ranked candidate terms before
#'   final top-N truncation.
#'
#' @return Data frame ready for plotting, or `NULL` when input is not plottable.
build_enrichr_plot_data <- function(df, N, pval_type = "logPval", keep_all = FALSE) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
    return(NULL)
  }
  if (!"P.value" %in% colnames(df) || !"Term" %in% colnames(df) || !"Overlap" %in% colnames(df)) {
    return(NULL)
  }

  N <- max(1L, as.integer(N))

  d <- df

  ov <- strsplit(as.character(d$Overlap), "/", fixed = TRUE)
  count <- suppressWarnings(as.numeric(vapply(ov, function(x) if (length(x) >= 1) x[1] else NA_character_, character(1))))
  denom <- suppressWarnings(as.numeric(vapply(ov, function(x) if (length(x) >= 2) x[2] else NA_character_, character(1))))
  gene_ratio <- ifelse(is.finite(count) & is.finite(denom) & denom > 0, count / denom, NA_real_)

  d$GeneCount <- count
  d$GeneSetSize <- denom
  d$GeneRatio <- gene_ratio
  d$P.value <- suppressWarnings(as.numeric(d$P.value))
  if (!"Adjusted.P.value" %in% colnames(d)) {
    d$Adjusted.P.value <- NA_real_
  }
  d$Adjusted.P.value <- suppressWarnings(as.numeric(d$Adjusted.P.value))
  if (!"Combined.Score" %in% colnames(d)) {
    d$Combined.Score <- NA_real_
  }
  d$Combined.Score <- suppressWarnings(as.numeric(d$Combined.Score))
  d$logPval <- (-1) * log10(d$P.value)
  d$logPadj <- (-1) * log10(d$Adjusted.P.value)

  if (nrow(d) == 0) {
    return(NULL)
  }
  d$filter_stage <- "pvalue_filtered"

  sig_col <- if (identical(pval_type, "logPadj")) "logPadj" else "logPval"
  d$logP <- d[[sig_col]]

  overlap_label <- ifelse(
    is.finite(d$GeneCount) & is.finite(d$GeneSetSize),
    paste0(" (", as.integer(d$GeneCount), "/", as.integer(d$GeneSetSize), ")"),
    ""
  )
  d$term_label <- paste0(
    stringr::str_wrap(substr(as.character(d$Term), 1, 100), width = 50),
    overlap_label
  )
  d <- d[order(-d$Combined.Score, na.last = TRUE), , drop = FALSE]
  if (!isTRUE(keep_all)) {
    d <- head(d, N)
    d$term_label <- factor(d$term_label, levels = rev(unique(d$term_label)))
  }
  d
}

#' Finalize EnrichR plot data
#'
#' Truncates a ranked candidate table to the requested size and restores factor
#' levels for top-to-bottom plotting.
#'
#' @param df Candidate EnrichR plotting data.
#' @param N Number of terms to keep.
#'
#' @return Data frame ready for plotting, or `NULL` when no rows remain.
finalize_enrichr_plot_data <- function(df, N) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
    return(NULL)
  }
  N <- max(1L, as.integer(N))
  df <- head(df, N)
  if (nrow(df) == 0) {
    return(NULL)
  }
  df$term_label <- factor(df$term_label, levels = rev(unique(df$term_label)))
  df
}

#' Draw EnrichR Bar Plot for One Direction
#'
#' @param df Plot-ready table from [build_enrichr_plot_data()].
#' @param direction_label Either `"Up"` or `"Down"`.
#' @param pval_type Significance metric used on the x-axis.
#' @param fill_low Low color for gene-ratio fill scale.
#' @param fill_high High color for gene-ratio fill scale.
#'
#' @return A ggplot object.
make_enrichr_bar_plot <- function(df, direction_label, pval_type = "logPval", fill_low = "white", fill_high = NULL) {
  p_label <- if (identical(pval_type, "logPadj")) "-log10(Adjusted P-value)" else "-log10(P-value)"

  df <- df |>
    dplyr::arrange(dplyr::desc(.data$Combined.Score)) |>
    dplyr::mutate(term_label = factor(.data$term_label, levels = rev(unique(.data$term_label))))

  if (direction_label == "Up") {
    title_color <- "#F92A2A"
  } else {
    title_color <- "#2727FA"
  }

  ggplot(df, aes(
    x = .data$logP,
    y = .data$term_label,
    fill = .data$GeneRatio
  )) +
    geom_col(color = "black", linewidth = 0.2) +
    scale_fill_gradient(
      low = fill_low,
      high = fill_high,
      name = "Gene Ratio"
    ) +
    labs(x = p_label, y = NULL, title = paste0(direction_label, "regulated")) +
    scale_x_continuous(
      breaks = enrichr_x_breaks(),
      labels = scales::label_number(accuracy = 0.1, trim = TRUE)
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5, color = title_color),
      axis.line.x = element_line(color = "black"),
      axis.line.y = element_line(color = "black"),
      axis.title.x = element_text(size = 10),
      axis.text.y = element_text(size = 9),
      axis.text.x = element_text(size = 9),
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8),
      plot.margin = margin(0.3, 0.5, 0.3, 0.2, "cm"),
      panel.grid.major.x = element_line(color = "grey85", linewidth = 0.3, linetype = "dashed"),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
    )
}

#' Render an EnrichR Bar Plot as Native Plotly
#'
#' Plotly's ggplot converter can drop horizontal `geom_col()` bars when multiple
#' rows share the same plotted p-value. This renderer uses the same plot-ready
#' data as the saved ggplot PNG, but builds the interactive bar chart directly.
#'
#' @param p EnrichR bar ggplot from [make_enrichr_bar_plot()].
#' @param width Optional Plotly width.
#' @param height Optional Plotly height.
#'
#' @return Plotly object, or `NULL` when input is not renderable.
make_enrichr_bar_plotly <- function(p, width = NULL, height = NULL) {
  if (!inherits(p, "gg")) {
    return(NULL)
  }
  if (!enrichr_plot_has_data(p)) {
    fig <- plotly::ggplotly(p, tooltip = "none", width = width, height = height)
    return(plotly::layout(
      fig,
      xaxis = list(visible = FALSE, showgrid = FALSE, zeroline = FALSE),
      yaxis = list(visible = FALSE, showgrid = FALSE, zeroline = FALSE),
      showlegend = FALSE,
      margin = list(l = 20, r = 20, t = 20, b = 20)
    ))
  }

  d <- p$data
  if (!all(c("logP", "term_label", "GeneRatio") %in% colnames(d))) {
    return(NULL)
  }

  d <- d[is.finite(d$logP) & !is.na(d$term_label), , drop = FALSE]
  if (nrow(d) == 0) {
    return(NULL)
  }

  term_labels <- as.character(d$term_label)
  term_levels <- if (is.factor(d$term_label)) {
    levels(d$term_label)
  } else {
    rev(unique(term_labels))
  }

  plot_title <- if (!is.null(p$labels$title)) p$labels$title else "EnrichR pathways"
  x_label <- if (!is.null(p$labels$x)) p$labels$x else "-log10(P-value)"
  high_col <- if (grepl("^Up", plot_title, ignore.case = TRUE)) "#F92A2A" else "#2727FA"

  ratio_range <- range(d$GeneRatio, finite = TRUE, na.rm = TRUE)
  if (!all(is.finite(ratio_range))) {
    ratio_range <- c(0, 1)
  } else if (isTRUE(all.equal(ratio_range[1], ratio_range[2]))) {
    ratio_range <- c(0, max(ratio_range[2], 1e-6))
  }

  hover_text <- paste0(
    "<b>", d$Term, "</b>",
    "<br>Overlap: ", d$Overlap,
    "<br>", x_label, ": ", signif(d$logP, 4),
    "<br>Gene Ratio: ", signif(d$GeneRatio, 4)
  )

  max_x <- max(d$logP, na.rm = TRUE)
  x_range <- c(0, if (is.finite(max_x) && max_x > 0) max_x * 1.08 else 1)

  fig <- plotly::plot_ly(
    x = d$logP,
    y = term_labels,
    type = "bar",
    orientation = "h",
    hovertext = hover_text,
    hoverinfo = "text",
    textposition = "none",
    width = width,
    height = height,
    marker = list(
      color = d$GeneRatio,
      cmin = ratio_range[1],
      cmax = ratio_range[2],
      colorscale = list(c(0, "#FFFFFF"), c(1, high_col)),
      colorbar = list(title = "Gene Ratio"),
      line = list(color = "black", width = 0.6)
    )
  )

  plotly::layout(
    fig,
    title = list(text = plot_title, x = 0.5, font = list(color = high_col, size = 16)),
    xaxis = list(title = x_label, range = x_range, tickformat = ".1f"),
    yaxis = list(
      title = "",
      categoryorder = "array",
      categoryarray = term_levels,
      automargin = TRUE
    ),
    margin = list(l = 20, r = 80, t = 55, b = 55),
    showlegend = FALSE
  )
}

#' Draw EnrichR Dot Plot for One Direction
#'
#' @param df Plot-ready table from [build_enrichr_plot_data()].
#' @param direction_label Either `"Up"` or `"Down"`.
#' @param pval_type Significance metric used on the x-axis.
#' @param col_low Low color for gene-ratio scale.
#' @param col_high High color for gene-ratio scale.
#'
#' @return A ggplot object.
make_enrichr_dot_plot <- function(df, direction_label, pval_type = "logPval", col_low = "white", col_high = NULL) {
  p_label <- if (identical(pval_type, "logPadj")) "-log10(Adjusted P-value)" else "-log10(P-value)"
  full_title <- paste0(direction_label, "regulated")

  df <- df |>
    dplyr::arrange(dplyr::desc(.data$Combined.Score)) |>
    dplyr::mutate(term_label = factor(.data$term_label, levels = rev(unique(.data$term_label))))

  if (direction_label == "Up") {
    title_color <- "#F92A2A"
  } else {
    title_color <- "#2727FA"
  }

  ggplot(df, aes(x = .data$logP, y = .data$term_label)) +
    geom_point(
      aes(fill = .data$GeneRatio, size = .data$GeneCount),
      shape = 21,
      color = "grey30",
      stroke = 0.35,
      alpha = 0.95,
      show.legend = TRUE
    ) +
    scale_fill_gradient(low = col_low, high = col_high, name = "Gene Ratio") +
    scale_size_continuous(name = "Gene Count", range = c(4, 10)) +
    guides(
      fill = guide_colorbar(order = 1),
      size = guide_legend(
        order = 2,
        override.aes = list(
          shape = 21,
          fill = "white",
          color = "grey30",
          alpha = 1,
          stroke = 0.35
        )
      )
    ) +
    labs(x = p_label, y = NULL, title = full_title) +
    scale_x_continuous(
      breaks = enrichr_x_breaks(),
      labels = scales::label_number(accuracy = 0.1, trim = TRUE)
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5, color = title_color),
      axis.line.x = element_line(color = "black"),
      axis.line.y = element_line(color = "black"),
      axis.title.x = element_text(size = 10, color = "black"),
      axis.text.y = element_text(size = 9, color = "black"),
      axis.text.x = element_text(size = 9, color = "black"),
      legend.title = element_text(size = 9, color = "black"),
      legend.text = element_text(size = 8, color = "black"),
      plot.margin = margin(0.3, 0.5, 0.3, 0.2, "cm"),
      panel.grid.major.x = element_line(color = "grey85", linewidth = 0.3, linetype = "dashed"),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank()
    )
}

#' Build EnrichR Plot Bundle for Up/Down Directions
#'
#' @param entry Single result entry produced by [run_enrichr()].
#' @param plot_type Plot style: `"bar"` or `"dot"`.
#' @param top_up Number of upregulated pathways to show.
#' @param top_down Number of downregulated pathways to show.
#' @param pval_type Significance metric used for plotting scores.
#'
#' @return Named list with `up` and `down` ggplot objects.
build_enrichr_plot_bundle <- function(entry, plot_type = "bar", top_up = 10L, top_down = 10L,
                                      pval_type = "logPval") {
  d_up <- build_enrichr_plot_data(entry$up_result, top_up, pval_type = pval_type, keep_all = TRUE)
  d_down <- build_enrichr_plot_data(entry$down_result, top_down, pval_type = pval_type, keep_all = TRUE)
  d_up <- finalize_enrichr_plot_data(d_up, top_up)
  d_down <- finalize_enrichr_plot_data(d_down, top_down)

  empty_up <- enrichr_empty_plot("No upregulated pathways passed pvalue<0.05.")
  empty_down <- enrichr_empty_plot("No downregulated pathways passed pvalue<0.05.")

  if (identical(plot_type, "dot")) {
    list(
      up = if (!is.null(d_up)) make_enrichr_dot_plot(d_up, "Up", pval_type = pval_type, col_high = "#F92A2A") else empty_up,
      down = if (!is.null(d_down)) make_enrichr_dot_plot(d_down, "Down", pval_type = pval_type, col_high = "#2727FA") else empty_down
    )
  } else {
    list(
      up = if (!is.null(d_up)) make_enrichr_bar_plot(d_up, "Up", pval_type = pval_type, fill_high = "#F92A2A") else empty_up,
      down = if (!is.null(d_down)) make_enrichr_bar_plot(d_down, "Down", pval_type = pval_type, fill_high = "#2727FA") else empty_down
    )
  }
}

#' Render EnrichR Plots for a Result Entry
#'
#' Builds up/down EnrichR plots and renders them as a vertical stack.
#'
#' @param entry Single result entry produced by [run_enrichr()].
#' @param top_up Number of upregulated pathways to show.
#' @param top_down Number of downregulated pathways to show.
#' @param plot_type Plot style: `"bar"` or `"dot"`.
#' @param pval_type Significance metric used for plotting scores.
#'
#' @return Invisibly returns `NULL` after plotting.
plot_enrichr_results <- function(entry, top_up = 10L, top_down = 10L,
                                 plot_type = "bar", pval_type = "logPval") {
  bundle <- build_enrichr_plot_bundle(
    entry = entry,
    plot_type = plot_type,
    top_up = top_up,
    top_down = top_down,
    pval_type = pval_type
  )
  p_up <- bundle$up
  p_down <- bundle$down

  if (is.null(p_up) && is.null(p_down)) {
    plot.new()
    title("Unable to render EnrichR plot for this result.")
    return(invisible(NULL))
  }

  plot_list <- Filter(Negate(is.null), list(p_up, p_down))

  if (requireNamespace("patchwork", quietly = TRUE)) {
    if (length(plot_list) > 1) {
      print(patchwork::wrap_plots(plot_list, ncol = 1))
    } else {
      print(plot_list[[1]])
    }
  } else if (requireNamespace("gridExtra", quietly = TRUE)) {
    do.call(gridExtra::grid.arrange, c(plot_list, ncol = 1))
  } else {
    print(plot_list[[1]])
  }

  invisible(NULL)
}
