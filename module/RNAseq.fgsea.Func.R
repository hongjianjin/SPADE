library(msigdbr)
library(fgsea)
library(dplyr)
library(ggplot2)
library(tibble)
library(ggtext)
library(grid)
library(tidyverse)

#' Build range-aware x-axis breaks for fGSEA plots
#'
#' Uses pretty breaks so tick spacing adapts to the plotted range and avoids
#' crowded labels on wider spans.
#'
#' @return A breaks function for ggplot2 continuous scales.
fgsea_x_breaks <- function() {
  scales::pretty_breaks(n = 6)
}

#' Return ordered fGSEA significance levels
#'
#' Defines the factor order used to label fGSEA pathway significance.
#'
#' @return Character vector of significance level labels.
fgsea_sig_levels <- function() {
  c("FDR<0.05", "FDR<0.1", "Pval<0.05", "NS")
}

#' Return fGSEA significance colors
#'
#' Provides the color mapping used for significance categories in fGSEA plots.
#'
#' @return Named character vector mapping significance labels to colors.
fgsea_sig_colors <- function() {
  c(
    "FDR<0.05" = "red",
    "FDR<0.1" = "pink",
    "Pval<0.05" = "grey60",
    "NS" = "grey90"
  )
}

#' Filter fGSEA Results by Nominal P-value
#'
#' Applies the same nominal p-value cutoff used for saved fGSEA tables and
#' downstream plots.
#'
#' @param fgsea_res_tab fGSEA result table containing a `pval` column.
#' @param pval_cutoff Nominal p-value cutoff. Use `Inf` to keep all rows.
#'
#' @return fGSEA result table filtered to `pval < pval_cutoff`.
filter_fgsea_results_by_pvalue <- function(fgsea_res_tab, pval_cutoff = 0.05) {
  if (is.null(fgsea_res_tab) || !is.data.frame(fgsea_res_tab) || nrow(fgsea_res_tab) == 0) {
    return(fgsea_res_tab)
  }
  if (!"pval" %in% colnames(fgsea_res_tab)) {
    return(fgsea_res_tab)
  }

  pval_cutoff <- suppressWarnings(as.numeric(pval_cutoff))
  if (!is.finite(pval_cutoff)) {
    return(fgsea_res_tab)
  }

  fgsea_res_tab |>
    dplyr::mutate(pval = suppressWarnings(as.numeric(.data$pval))) |>
    dplyr::filter(!is.na(.data$pval), .data$pval < pval_cutoff)
}

#' Select Top Enriched Pathways by Direction
#'
#' Selects upregulated and downregulated pathways independently using NES only.
#' The input table is expected to already be filtered by nominal p-value before
#' this plotting helper is called.
#'
#' @param fgsea_res_tab fGSEA result table containing `NES`, `pval`, `padj`,
#'   `size`, and `pathway_size` columns.
#' @param top_n Number of pathways to keep per direction.
#'
#' @return Data frame with pathway-level annotations used by plotting helpers.
select_top_pathways <- function(fgsea_res_tab = NULL, top_n = 10) {
  if (is.null(fgsea_res_tab)) {
    stop("No fGSEA result table provided to generate a plot.")
  }

  top_n <- suppressWarnings(as.integer(top_n))
  if (!is.finite(top_n) || is.na(top_n)) top_n <- 10L
  top_n <- max(1L, top_n)

  fgseaResTidy <- fgsea_res_tab |>
    dplyr::mutate(
      NES = suppressWarnings(as.numeric(.data$NES)),
      pval = suppressWarnings(as.numeric(.data$pval)),
      padj = suppressWarnings(as.numeric(.data$padj)),
      size = suppressWarnings(as.numeric(.data$size)),
      absNES = abs(.data$NES),
      logPval = (-1) * log10(.data$pval),
      logPadj = (-1) * log10(.data$padj),
      Direction = dplyr::case_when(
        .data$NES > 0 ~ "Upregulated",
        .data$NES < 0 ~ "Downregulated",
        TRUE ~ NA_character_
      ),
      sig_group = dplyr::case_when(
        !is.na(.data$padj) & .data$padj < 0.05 ~ "FDR<0.05",
        !is.na(.data$padj) & .data$padj < 0.1 ~ "FDR<0.1",
        !is.na(.data$pval) & .data$pval < 0.05 ~ "Pval<0.05",
        TRUE ~ "NS"
      ),
      sig_group = factor(.data$sig_group, levels = fgsea_sig_levels())
    ) |>
    dplyr::filter(is.finite(.data$NES), .data$NES != 0, !is.na(.data$Direction))

  up_top <- fgseaResTidy |>
    dplyr::filter(.data$Direction == "Upregulated") |>
    dplyr::arrange(dplyr::desc(.data$NES)) |>
    dplyr::slice_head(n = top_n)

  down_top <- fgseaResTidy |>
    dplyr::filter(.data$Direction == "Downregulated") |>
    dplyr::arrange(.data$NES) |>
    dplyr::slice_head(n = top_n)

  fgseaResTidy_top <- if (nrow(up_top) == 0 && nrow(down_top) == 0) {
    fgseaResTidy[0, , drop = FALSE]
  } else {
    dplyr::bind_rows(up_top, down_top) |>
      dplyr::mutate(filter_stage = "pvalue_filtered")
  }

  # Wrap long pathway names so they fit on the axis
  #' Wrap Labels
  #'
  #' Executes wrap_labels.
  #'
  #' @param x Argument for wrap_labels.
  #' @param width Argument for wrap_labels.
  #'
  #' @return Return value produced by wrap_labels.
  wrap_labels <- function(x, width = 40) {
    sapply(x, function(s) {
      paste(strwrap(gsub("_", " ", s), width = width), collapse = "\n")
    })
  }

  # Add annotations for plot
  fgseaResTidy_top <- fgseaResTidy_top %>%
    dplyr::mutate(
      gene_count = suppressWarnings(as.numeric(.data$size)),
      gene_ratio = round(suppressWarnings(as.numeric(.data$gene_ratio)), 2),
      gene_ratio_label = dplyr::if_else(
        !is.na(.data$size) & !is.na(.data$pathway_size),
        paste0(" (", .data$size, "/", .data$pathway_size, ")"),
        ""
      ),
      pathway_label = paste0(wrap_labels(.data$pathway, width = 45), .data$gene_ratio_label)
    ) %>%
    dplyr::arrange(
      .data$Direction,
      dplyr::if_else(.data$Direction == "Upregulated", -.data$NES, .data$NES)
    )

  return(fgseaResTidy_top)
}


#' Create an Empty fGSEA Placeholder Plot
#'
#' @param label Message displayed in the empty plot canvas.
#'
#' @return A ggplot object with a centered message and void theme.
fgsea_empty_plot <- function(label = "No enriched pathways found.") {
  p <- ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = label, size = 5.5, color = "grey35", fontface = "italic") +
    xlim(0, 1) +
    ylim(0, 1) +
    theme_void()
  attr(p, "shinyRNAseq_empty_plot") <- TRUE
  p
}

#' Compute rounded gene-count scale limits
#'
#' Converts values to numeric and expands the observed range to clean multiples
#' of `step` for plot size legends.
#'
#' @param x Numeric-like vector of gene counts.
#' @param step Rounding increment for lower and upper limits.
#'
#' @return Numeric vector of length two containing lower and upper limits.
compute_gene_count_limits <- function(x, step = 10) {
  vals <- suppressWarnings(as.numeric(x))
  vals <- vals[is.finite(vals)]
  if (length(vals) == 0) {
    return(c(0, step))
  }

  lo <- floor(min(vals, na.rm = TRUE) / step) * step
  hi <- ceiling(max(vals, na.rm = TRUE) / step) * step
  if (!is.finite(lo) || !is.finite(hi) || identical(lo, hi)) {
    hi <- lo + step
  }
  c(lo, hi)
}

# Single lollipop plot (Up + Down together): x=NES, y=pathway,
# color uses a bidirectional gradient by Direction and significance.
#' Draw Combined fGSEA Lollipop Plot
#'
#' @param fgseaResTidy_top Annotated fGSEA table from [select_top_pathways()].
#' @param pval_type Significance metric to color by: `"logPval"` or
#'   `"logPadj"`.
#' @param plot_title Optional plot title.
#'
#' @return A ggplot object. Returns an empty placeholder plot if input is empty.
fgsea_lollipop_plot <- function(fgseaResTidy_top = NULL, pval_type = "logPval", plot_title = "") {
  color_label <- if (pval_type == "logPadj") "-log10(padj)" else "-log10(p-value)"

  if (is.null(fgseaResTidy_top) || nrow(fgseaResTidy_top) == 0) {
    return(fgsea_empty_plot("No enriched pathways found."))
  }

  df <- fgseaResTidy_top |>
    dplyr::mutate(
      color_value = ifelse(.data$Direction == "Upregulated", .data[[pval_type]], -(.data[[pval_type]]))
    ) |>
    dplyr::arrange(.data$NES) |>
    dplyr::mutate(pathway_label = factor(.data$pathway_label, levels = unique(.data$pathway_label)))

  df$gene_count <- suppressWarnings(as.numeric(df$gene_count))
  size_limits <- compute_gene_count_limits(df$gene_count, step = 10)

  color_limits <- range(df$color_value, na.rm = TRUE)
  if (!all(is.finite(color_limits))) {
    color_limits <- c(-1, 1)
  }
  if (identical(color_limits[1], color_limits[2])) {
    color_limits <- c(color_limits[1] - 1, color_limits[2] + 1)
  }

  full_title <- if (nzchar(plot_title)) plot_title else ""

  ggplot(df, aes(
    x    = .data$NES,
    y    = .data$pathway_label
  )) +
    scale_x_continuous(
      breaks = fgsea_x_breaks(),
      labels = scales::label_number(accuracy = 0.1, trim = TRUE)
    ) +
    # Draw a black stem first, then overlay the gradient stem for a bordered look.
    geom_segment(aes(
      x = 0, xend = .data$NES,
      y = .data$pathway_label, yend = .data$pathway_label
    ), color = "black", linewidth = 1.8, alpha = 0.95) +
    geom_segment(aes(
      x = 0, xend = .data$NES,
      y = .data$pathway_label, yend = .data$pathway_label,
      color = .data$color_value
    ), linewidth = 1.1) +
    geom_point(aes(fill = .data$color_value, size = .data$gene_count),
      shape = 21, color = "black", stroke = 0.45, alpha = 0.95,
      show.legend = TRUE
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.5) +
    scale_color_gradient2(
      low = "#2727FA",
      mid = "white",
      high = "#F92A2A",
      midpoint = 0,
      limits = color_limits,
      guide = "none"
    ) +
    scale_fill_gradient2(
      low = "#2727FA",
      mid = "white",
      high = "#F92A2A",
      midpoint = 0,
      limits = color_limits,
      breaks = scales::pretty_breaks(n = 5),
      name = color_label
    ) +
    scale_size_continuous(
      name = "Gene Count",
      range = c(4, 8),
      limits = size_limits,
      breaks = scales::pretty_breaks(n = 4)
    ) +
    guides(
      fill = guide_colorbar(order = 1),
      size = guide_legend(order = 2, override.aes = list(shape = 21, fill = "white", color = "black", alpha = 1))
    ) +
    labs(
      x     = "Normalized Enrichment Score (NES)",
      y     = NULL,
      title = full_title
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
      axis.line.x = element_line(color = "black"),
      axis.line.y = element_line(color = "black"),
      axis.title.x = element_text(size = 10),
      axis.text.y = element_text(size = 9),
      axis.text.x = element_text(size = 9),
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "right",
      plot.margin = margin(0.3, 0.5, 0.3, 0.2, "cm")
    )
}

# Two separate dot plots (Up + Down), each colored by selected p-value metric
#' Draw Direction-Specific fGSEA Dot Plots
#'
#' @param fgseaResTidy_top Annotated fGSEA table from [select_top_pathways()].
#' @param pval_type Significance metric for the x-axis: `"logPval"` or
#'   `"logPadj"`.
#' @param plot_title Optional title prefix.
#'
#' @return Named list with `up` and `down` ggplot objects.
fgsea_dotplot <- function(fgseaResTidy_top = NULL, pval_type = "logPval", plot_title = "") {
  x_label <- if (pval_type == "logPadj") "-log10(padj)" else "-log10(p-value)"

  #' Make One Dotplot
  #'
  #' Executes make_one_dotplot.
  #'
  #' @param df Argument for make_one_dotplot.
  #' @param direction Argument for make_one_dotplot.
  #' @param fill_low Argument for make_one_dotplot.
  #' @param fill_high Argument for make_one_dotplot.
  #' @param title_color Argument for make_one_dotplot.
  #'
  #' @return Return value produced by make_one_dotplot.
  make_one_dotplot <- function(df, direction, fill_low, fill_high, title_color) {
    if (nrow(df) == 0) {
      return(fgsea_empty_plot(paste0("No ", tolower(direction), " pathways passed pvalue <0.05.")))
    }
    df <- df |>
      dplyr::arrange(dplyr::if_else(.data$Direction == "Upregulated", -.data$NES, .data$NES)) |>
      dplyr::mutate(pathway_label = factor(.data$pathway_label, levels = rev(unique(.data$pathway_label))))

    if (!"gene_count" %in% colnames(df)) {
      df$gene_count <- suppressWarnings(as.numeric(df$size))
    } else {
      df$gene_count <- suppressWarnings(as.numeric(df$gene_count))
    }
    size_limits <- compute_gene_count_limits(df$gene_count, step = 10)

    full_title <- if (nzchar(plot_title)) {
      paste0(plot_title, " - ", direction)
    } else {
      direction
    }

    ggplot(df, aes(
      x    = .data[[pval_type]],
      y    = .data$pathway_label,
      fill = .data$gene_ratio,
      size = .data$gene_count
    )) +
      geom_point(shape = 21, alpha = 0.95, color = "grey30") +
      scale_fill_gradient(low = fill_low, high = fill_high, name = "Gene Ratio") +
      scale_size_continuous(
        name = "Gene Count",
        range = c(3, 10),
        limits = size_limits,
        breaks = scales::pretty_breaks(n = 4)
      ) +
      guides(
        fill = guide_colorbar(order = 1),
        size = guide_legend(order = 2, override.aes = list(shape = 21, fill = "white", color = "grey30", alpha = 1))
      ) +
      scale_x_continuous(
        breaks = fgsea_x_breaks(),
        labels = scales::label_number(accuracy = 0.1, trim = TRUE)
      ) +
      labs(x = x_label, y = NULL, title = full_title) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5, color = title_color),
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.title.x = element_text(size = 10),
        axis.text.y = ggtext::element_markdown(size = 9),
        axis.text.x = element_text(size = 9),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8),
        plot.margin = margin(0.3, 0.5, 0.3, 0.2, "cm"),
        panel.grid.major.x = element_line(color = "grey85", linewidth = 0.3, linetype = "dashed"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank()
      )
  }

  if (is.null(fgseaResTidy_top) || nrow(fgseaResTidy_top) == 0) {
    return(list(
      up = fgsea_empty_plot("No upregulated pathways passed pvalue <0.05."),
      down = fgsea_empty_plot("No downregulated pathways passed pvalue <0.05.")
    ))
  }

  up_df <- fgseaResTidy_top |> dplyr::filter(.data$Direction == "Upregulated")
  down_df <- fgseaResTidy_top |> dplyr::filter(.data$Direction == "Downregulated")

  plots <- list(
    up = make_one_dotplot(up_df, "Upregulated",
      fill_low = "white", fill_high = "#F92A2A",
      title_color = "#F92A2A"
    ),
    down = make_one_dotplot(down_df, "Downregulated",
      fill_low = "white", fill_high = "#2727FA",
      title_color = "#2727FA"
    )
  )
  return(plots)
}

# Single bar plot: x=NES, y=pathway, fill=NES.
#' Draw Combined fGSEA Bar Plot
#'
#' @param fgseaResTidy_top Annotated fGSEA table from [select_top_pathways()].
#' @param pval_type Significance metric to include in hover text: `"logPval"`
#'   or `"logPadj"`. Bar plot fill uses NES.
#' @param plot_title Optional plot title.
#'
#' @return A ggplot object. Returns an empty placeholder plot if input is empty.
fgsea_barplot <- function(fgseaResTidy_top = NULL, pval_type = "logPval", plot_title = "") {
  if (is.null(fgseaResTidy_top) || nrow(fgseaResTidy_top) == 0) {
    return(fgsea_empty_plot("No enriched pathways found."))
  }

  allowed_pval_types <- c("logPval", "logPadj")
  if (is.null(pval_type) || !pval_type %in% allowed_pval_types || !pval_type %in% colnames(fgseaResTidy_top)) {
    pval_type <- "logPval"
  }
  pval_label <- if (pval_type == "logPadj") "-log10(padj)" else "-log10(p-value)"

  df <- fgseaResTidy_top |>
    dplyr::mutate(
      NES = suppressWarnings(as.numeric(.data$NES)),
      hover_pval_metric = suppressWarnings(as.numeric(.data[[pval_type]]))
    ) |>
    dplyr::arrange(dplyr::desc(.data$NES)) |>
    dplyr::mutate(
      pathway_label = factor(.data$pathway_label, levels = rev(unique(.data$pathway_label))),
      text = paste0(
        "Pathway: ", .data$pathway,
        "<br>NES: ", signif(.data$NES, 4),
        "<br>", pval_label, ": ", signif(.data$hover_pval_metric, 4),
        "<br>Overlap: ", .data$size, "/", .data$pathway_size
      )
    )

  finite_color_values <- df$NES[is.finite(df$NES)]
  color_max <- if (length(finite_color_values) > 0) max(abs(finite_color_values), na.rm = TRUE) else NA_real_
  if (!is.finite(color_max) || color_max <= 0) {
    color_max <- 1
  }
  color_limits <- c(-color_max, color_max)

  ggplot(df, aes(
    x = .data$NES,
    y = .data$pathway_label,
    fill = .data$NES,
    text = .data$text
  )) +
    geom_col(color = "black", linewidth = 0.2) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.1) +
    scale_fill_gradient2(
      low = "#2727FA",
      mid = "white",
      high = "#F92A2A",
      midpoint = 0,
      limits = color_limits,
      oob = scales::squish,
      breaks = scales::pretty_breaks(n = 5),
      name = "NES"
    ) +
    scale_x_continuous(
      breaks = fgsea_x_breaks(),
      labels = scales::label_number(accuracy = 0.1, trim = TRUE)
    ) +
    labs(
      x = "Normalized Enrichment Score (NES)",
      y = NULL,
      title = if (nzchar(plot_title)) plot_title else ""
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
      axis.line.x = element_line(color = "black"),
      axis.line.y = element_line(color = "black"),
      axis.title.x = element_text(size = 10),
      axis.text.y = element_text(size = 9),
      axis.text.x = element_text(size = 9),
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8),
      panel.grid.major.y = element_blank(),
      plot.margin = margin(0.3, 0.5, 0.3, 0.2, "cm")
    )
}

#' Run fgsea and build an enrichment plot
#'
#' Runs fgsea from a named ranking vector or formats an existing fgsea result
#' table into a bar or dot plot of the top enriched pathways.
#'
#' @param ranks Named numeric vector of ranked genes. When supplied, fgsea is
#'   executed before plotting.
#' @param fgsea_res_tab Existing fgsea result table used for plotting when
#'   `ranks` is `NULL`.
#' @param db MSigDB collection code used with `msigdbr`.
#' @param subcat Optional MSigDB subcollection.
#' @param species Species name passed to `msigdbr`.
#' @param pval_cutoff Nominal p-value cutoff applied to saved fGSEA tables and
#'   plots. Use `Inf` to keep all rows.
#' @param fdr_cutoff Optional adjusted p-value cutoff reserved for downstream
#'   filtering.
#' @param sname Plot title.
#' @param gene_col Column name reserved for gene identifiers in external result
#'   tables.
#' @param pval_col Column name reserved for p-values in external result tables.
#' @param custom_gmt Optional path to a GMT file. When provided it is used
#'   instead of `msigdbr` collections.
#' @param top_n Number of positive and negative NES pathways to display.
#' @param plot_type Plot style, one of `"bar"`, `"dot"`, or `"lollipop"`.
#' @param pval_type Significance metric used by plot helpers: `"logPval"` or
#'   `"logPadj"`.
#' @param run_only Logical flag indicating whether to return only the fgsea
#'   result table.
#'
#' @return A tibble when `run_only` is `TRUE`; otherwise a ggplot object or a
#'   named list of ggplot objects (dot mode).

run_fgsea <- function(ranks = NULL,
                      fgsea_res_tab = NULL,
                      db = NULL, subcat = NULL, species = "Homo sapiens",
                      pval_cutoff = 0.05, fdr_cutoff = NULL, sname = "",
                      gene_col = "gene", pval_col = "p_val",
                      custom_gmt = NULL, top_n = 10, plot_type = "bar",
                      pval_type = "logPval",
                      run_only = FALSE) {
  # Part 1: Run fGSEA analysis and return result table
  if (!is.null(ranks)) {
    # Load gene sets
    if (!is.null(custom_gmt)) {
      fgsea_sets <- gmtPathways(custom_gmt)
      pathway_sizes <- tibble(
        gs_name = names(fgsea_sets),
        pathway_size = vapply(fgsea_sets, function(x) length(unique(x)), integer(1))
      )
    } else {
      if (is.null(db)) {
        stop("'db' must be specified when no custom GMT is provided.")
      }
      m_df <- msigdbr(species = species, collection = db, subcollection = subcat)

      # Auto-detect Ensembl IDs: if >10% of rank names start with ENSG/ENSMUSG
      use_ensembl <- mean(grepl("^ENS[A-Z]*G[0-9]+", names(ranks))) > 0.1
      if (use_ensembl) {
        m_df <- m_df[!is.na(m_df$ensembl_gene), ]
        fgsea_sets <- split(x = m_df$ensembl_gene, f = m_df$gs_name)
        pathway_sizes <- m_df %>%
          dplyr::filter(!is.na(.data$gs_name), !is.na(.data$ensembl_gene)) %>%
          dplyr::distinct(.data$gs_name, .data$ensembl_gene) %>%
          dplyr::count(.data$gs_name, name = "pathway_size")
      } else {
        fgsea_sets <- split(x = m_df$gene_symbol, f = m_df$gs_name)
        pathway_sizes <- m_df %>%
          dplyr::filter(!is.na(.data$gs_name), !is.na(.data$gene_symbol)) %>%
          dplyr::distinct(.data$gs_name, .data$gene_symbol) %>%
          dplyr::count(.data$gs_name, name = "pathway_size")
      }
    }

    # Sanitize ranks: strip Ensembl version suffixes
    names(ranks) <- sub("[.][0-9]+$", "", names(ranks))
    ranks <- ranks[!duplicated(names(ranks))]

    # Run fgsea analysis
    fgseaRes <- fgsea(fgsea_sets, stats = ranks, minSize = 15)


    # Part2: Add pathway size and gene ratio to the result table, and sort by NES
    fgseaResTidy <- fgseaRes %>%
      as_tibble() %>%
      dplyr::left_join(pathway_sizes, by = c("pathway" = "gs_name")) %>%
      arrange(desc(.data$NES)) %>%
      dplyr::mutate(gene_ratio = .data$size / .data$pathway_size)

    fgseaResTidy <- filter_fgsea_results_by_pvalue(fgseaResTidy, pval_cutoff = pval_cutoff)

    if (run_only) {
      return(fgseaResTidy)
    }
  }

  # Part 3: Generate plot from a result table

  # Use fgsea_res_tab when ranks were not provided
  if (is.null(ranks)) {
    if (is.null(fgsea_res_tab)) {
      stop("Either 'ranks' or 'fgsea_res_tab' must be provided.")
    }
    fgseaResTidy <- fgsea_res_tab
  }

  fgseaResTidy <- filter_fgsea_results_by_pvalue(fgseaResTidy, pval_cutoff = pval_cutoff)

  # Select top pathways to plot
  fgseaResTidy_top <- select_top_pathways(fgsea_res_tab = fgseaResTidy, top_n = top_n)

  plot_title <- if (!is.null(sname) && nzchar(sname)) sname else ""

  if (plot_type == "bar") {
    # Bar Plot: single plot, sorted by decreasing NES, no color gradient
    return(fgsea_barplot(fgseaResTidy_top, pval_type = pval_type, plot_title = plot_title))
  } else if (plot_type == "dot") {
    # Dot Plot: two separate plots (Up + Down), x=logPval/logPadj, fill=NES
    return(fgsea_dotplot(fgseaResTidy_top, pval_type = pval_type, plot_title = plot_title))
  } else if (plot_type == "lollipop") {
    # Lollipop Plot: single combined plot with directional significance gradient
    return(fgsea_lollipop_plot(fgseaResTidy_top, pval_type = pval_type, plot_title = plot_title))
  }
}
