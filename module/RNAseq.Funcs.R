jscode <- "
shinyjs.disableTab = function(name) {
  var tab = $('.nav li a[data-value=' + name + ']');
  tab.bind('click.tab', function(e) {
    e.preventDefault();
    return false;
  });
  tab.addClass('disabled');
  tab.attr('title', 'You need to validate inputs to proceed');
  tab.attr('aria-label', 'You need to validate inputs to proceed');
}

shinyjs.enableTab = function(name) {
  var tab = $('.nav li a[data-value=' + name + ']');
  tab.unbind('click.tab');
  tab.removeClass('disabled');
  tab.removeAttr('title');
  tab.removeAttr('aria-label');
}
"

css <- "
.nav li a.disabled {
  background-color: #aaa !important;
  color: #333 !important;
  cursor: not-allowed !important;
  border-color: #aaa !important;
}"
################################################################################
#' Return the default color palette
#'
#' Provides the standard set of color names used throughout metadata handling
#' and plotting helpers.
#'
#' @return Character vector of color names.
standardColors <- function() {
  colors <- c(
    "turquoise", "blue", "brown", "grey10", "green", "red", "black", "pink", "magenta", "purple", "greenyellow", "tan", "salmon", "cyan", "midnightblue", "lightcyan",
    "grey60", "lightgreen", "lightyellow", "royalblue", "darkred", "darkgreen", "darkturquoise", "darkgrey", "orange", "darkorange", "white", "skyblue",
    "saddlebrown", "steelblue", "paleturquoise", "violet", "darkolivegreen", "darkmagenta", "sienna3", "yellowgreen", "skyblue3", "plum1", "orangered4", "mediumpurple3",
    "lightsteelblue1", "lightcyan1", "ivory", "floralwhite", "darkorange2", "brown4", "bisque4", "darkslateblue", "plum2", "thistle2", "thistle1", "salmon4",
    "palevioletred3", "navajowhite2", "maroon", "lightpink4", "lavenderblush3", "honeydew1", "darkseagreen4", "coral1", "antiquewhite4", "coral2", "mediumorchid", "skyblue2",
    "yellow4", "skyblue1", "plum", "orangered3", "mediumpurple2", "lightsteelblue", "lightcoral", "indianred4", "firebrick4", "darkolivegreen4", "brown2", "blue2",
    "darkviolet", "plum3", "thistle3", "thistle", "salmon2", "palevioletred2", "navajowhite1", "magenta4", "lightpink3", "lavenderblush2", "honeydew", "darkseagreen3",
    "coral", "antiquewhite2", "coral3", "mediumpurple4", "skyblue4", "yellow3", "sienna4", "pink4", "orangered1", "mediumpurple1", "lightslateblue", "lightblue4",
    "indianred3", "firebrick3", "darkolivegreen2", "blueviolet", "blue4", "deeppink", "plum4", "thistle4", "tan4", "salmon1", "palevioletred1", "navajowhite",
    "magenta3", "lightpink2", "lavenderblush1", "green4", "darkseagreen2", "chocolate4", "antiquewhite1", "coral4", "mistyrose", "slateblue", "yellow2", "sienna2",
    "pink3", "orangered", "mediumpurple", "lightskyblue4", "lightblue3", "indianred2", "firebrick2", "darkolivegreen1", "blue3", "brown1", "deeppink1", "powderblue",
    "tomato", "tan3", "royalblue3", "palevioletred", "moccasin", "magenta2", "lightpink1", "lavenderblush", "green3", "darkseagreen1", "chocolate3", "aliceblue",
    "cornflowerblue", "navajowhite3", "slateblue1", "whitesmoke", "sienna1", "pink2", "orange4", "mediumorchid4", "lightskyblue3", "lightblue2", "indianred1", "firebrick",
    "darkgoldenrod4", "blue1", "brown3", "deeppink2", "purple2", "tomato2", "tan2", "royalblue2", "paleturquoise4", "mistyrose4", "magenta1", "lightpink",
    "lavender", "green2", "darkseagreen", "chocolate2", "antiquewhite", "cornsilk", "navajowhite4", "slateblue2", "wheat3", "sienna", "pink1", "orange3",
    "mediumorchid3", "lightskyblue2", "lightblue1", "indianred", "dodgerblue4", "darkgoldenrod3", "blanchedalmond", "burlywood", "deepskyblue", "red1", "tomato4", "tan1",
    "rosybrown4", "paleturquoise3", "mistyrose3", "linen", "lightgoldenrodyellow", "khaki4", "green1", "darksalmon", "chocolate1", "antiquewhite3", "cornsilk2", "oldlace",
    "slateblue3", "wheat1", "seashell4", "peru", "orange2", "mediumorchid2", "lightskyblue1", "lightblue", "hotpink4", "dodgerblue3", "darkgoldenrod1", "bisque3",
    "burlywood1", "deepskyblue4", "red4", "turquoise2", "steelblue4", "rosybrown3", "paleturquoise1", "mistyrose2", "limegreen", "lightgoldenrod4", "khaki3", "goldenrod4",
    "darkorchid4", "chocolate", "aquamarine", "cyan1", "orange1", "slateblue4", "violetred4", "seashell3", "peachpuff4", "olivedrab4", "mediumorchid1", "lightskyblue",
    "lemonchiffon4", "hotpink3", "dodgerblue1", "darkgoldenrod", "bisque2", "burlywood2", "dodgerblue2", "rosybrown2", "turquoise4", "steelblue3", "rosybrown1", "palegreen4",
    "mistyrose1", "lightyellow4", "lightgoldenrod3", "khaki2", "goldenrod3", "darkorchid3", "chartreuse4", "aquamarine1", "cyan4", "orangered2", "snow", "violetred2",
    "seashell2", "peachpuff3", "olivedrab3", "mediumblue", "lightseagreen", "lemonchiffon3", "hotpink2", "dodgerblue", "darkblue", "bisque1", "burlywood3", "firebrick1",
    "royalblue1", "violetred1", "steelblue1", "rosybrown", "palegreen3", "mintcream", "lightyellow3", "lightgoldenrod2", "khaki1", "goldenrod2", "darkorchid2", "chartreuse3",
    "aquamarine2", "darkcyan", "orchid", "snow2", "violetred", "seashell1", "peachpuff2", "olivedrab2", "mediumaquamarine", "lightsalmon4", "lemonchiffon2", "hotpink1",
    "deepskyblue3", "cyan3", "bisque", "burlywood4", "forestgreen", "royalblue4", "violetred3", "springgreen3", "red3", "palegreen1", "mediumvioletred", "lightyellow2",
    "lightgoldenrod1", "khaki", "goldenrod1", "darkorchid1", "chartreuse2", "aquamarine3", "darkgoldenrod2", "orchid1", "snow4", "turquoise3", "seashell", "peachpuff1",
    "olivedrab1", "maroon4", "lightsalmon3", "lemonchiffon1", "hotpink", "deepskyblue2", "cyan2", "beige", "cadetblue", "gainsboro", "salmon3", "wheat",
    "springgreen2", "red2", "palegreen", "mediumturquoise", "lightyellow1", "lightgoldenrod", "ivory4", "goldenrod", "darkorchid", "chartreuse1", "aquamarine4", "darkkhaki",
    "orchid3", "springgreen1", "turquoise1", "seagreen4", "peachpuff", "olivedrab", "maroon3", "lightsalmon2", "lemonchiffon", "honeydew4", "deepskyblue1", "cornsilk4",
    "azure4", "cadetblue1", "ghostwhite", "sandybrown", "wheat2", "springgreen", "purple4", "palegoldenrod", "mediumspringgreen", "lightsteelblue4", "lightcyan4", "ivory3",
    "gold3", "darkorange4", "chartreuse", "azure", "darkolivegreen3", "palegreen2", "springgreen4", "tomato3", "seagreen3", "papayawhip", "navyblue", "maroon2",
    "lightsalmon1", "lawngreen", "honeydew3", "deeppink4", "cornsilk3", "azure3", "cadetblue2", "gold", "seagreen", "wheat4", "snow3", "purple3",
    "orchid4", "mediumslateblue", "lightsteelblue3", "lightcyan3", "ivory2", "gold2", "darkorange3", "cadetblue4", "azure1", "darkorange1", "paleturquoise2", "steelblue2",
    "tomato1", "seagreen2", "palevioletred4", "navy", "maroon1", "lightsalmon", "lavenderblush4", "honeydew2", "deeppink3", "cornsilk1", "azure2", "cadetblue3",
    "gold4", "seagreen1", "yellow1", "snow1", "purple1", "orchid2", "mediumseagreen", "lightsteelblue2", "lightcyan2", "ivory1", "gold1"
  )

  colors0 <- c(
    "turquoise", "blue", "brown", "yellow", "green", "red", "black", "pink", "magenta", "purple", "greenyellow", "tan", "salmon", "cyan", "midnightblue", "lightcyan",
    "grey60", "lightgreen", "lightyellow", "royalblue", "darkred", "darkgreen", "darkturquoise", "darkgrey", "orange", "darkorange", "white", "skyblue",
    "saddlebrown", "steelblue", "paleturquoise", "violet", "darkolivegreen", "darkmagenta", "sienna3", "yellowgreen", "skyblue3", "plum1", "orangered4", "mediumpurple3",
    "lightsteelblue1", "lightcyan1", "ivory", "floralwhite", "darkorange2", "brown4", "bisque4", "darkslateblue", "plum2", "thistle2", "thistle1", "salmon4",
    "palevioletred3", "navajowhite2", "maroon", "lightpink4", "lavenderblush3", "honeydew1", "darkseagreen4", "coral1", "antiquewhite4", "coral2", "mediumorchid", "skyblue2",
    "yellow4", "skyblue1", "plum", "orangered3", "mediumpurple2", "lightsteelblue", "lightcoral", "indianred4", "firebrick4", "darkolivegreen4", "brown2", "blue2",
    "darkviolet", "plum3", "thistle3", "thistle", "salmon2", "palevioletred2", "navajowhite1", "magenta4", "lightpink3", "lavenderblush2", "honeydew", "darkseagreen3",
    "coral", "antiquewhite2", "coral3", "mediumpurple4", "skyblue4", "yellow3", "sienna4", "pink4", "orangered1", "mediumpurple1", "lightslateblue", "lightblue4",
    "indianred3", "firebrick3", "darkolivegreen2", "blueviolet", "blue4", "deeppink", "plum4", "thistle4", "tan4", "salmon1", "palevioletred1", "navajowhite",
    "magenta3", "lightpink2", "lavenderblush1", "green4", "darkseagreen2", "chocolate4", "antiquewhite1", "coral4", "mistyrose", "slateblue", "yellow2", "sienna2",
    "pink3", "orangered", "mediumpurple", "lightskyblue4", "lightblue3", "indianred2", "firebrick2", "darkolivegreen1", "blue3", "brown1", "deeppink1", "powderblue",
    "tomato", "tan3", "royalblue3", "palevioletred", "moccasin", "magenta2", "lightpink1", "lavenderblush", "green3", "darkseagreen1", "chocolate3", "aliceblue",
    "cornflowerblue", "navajowhite3", "slateblue1", "whitesmoke", "sienna1", "pink2", "orange4", "mediumorchid4", "lightskyblue3", "lightblue2", "indianred1", "firebrick",
    "darkgoldenrod4", "blue1", "brown3", "deeppink2", "purple2", "tomato2", "tan2", "royalblue2", "paleturquoise4", "mistyrose4", "magenta1", "lightpink",
    "lavender", "green2", "darkseagreen", "chocolate2", "antiquewhite", "cornsilk", "navajowhite4", "slateblue2", "wheat3", "sienna", "pink1", "orange3",
    "mediumorchid3", "lightskyblue2", "lightblue1", "indianred", "dodgerblue4", "darkgoldenrod3", "blanchedalmond", "burlywood", "deepskyblue", "red1", "tomato4", "tan1",
    "rosybrown4", "paleturquoise3", "mistyrose3", "linen", "lightgoldenrodyellow", "khaki4", "green1", "darksalmon", "chocolate1", "antiquewhite3", "cornsilk2", "oldlace",
    "slateblue3", "wheat1", "seashell4", "peru", "orange2", "mediumorchid2", "lightskyblue1", "lightblue", "hotpink4", "dodgerblue3", "darkgoldenrod1", "bisque3",
    "burlywood1", "deepskyblue4", "red4", "turquoise2", "steelblue4", "rosybrown3", "paleturquoise1", "mistyrose2", "limegreen", "lightgoldenrod4", "khaki3", "goldenrod4",
    "darkorchid4", "chocolate", "aquamarine", "cyan1", "orange1", "slateblue4", "violetred4", "seashell3", "peachpuff4", "olivedrab4", "mediumorchid1", "lightskyblue",
    "lemonchiffon4", "hotpink3", "dodgerblue1", "darkgoldenrod", "bisque2", "burlywood2", "dodgerblue2", "rosybrown2", "turquoise4", "steelblue3", "rosybrown1", "palegreen4",
    "mistyrose1", "lightyellow4", "lightgoldenrod3", "khaki2", "goldenrod3", "darkorchid3", "chartreuse4", "aquamarine1", "cyan4", "orangered2", "snow", "violetred2",
    "seashell2", "peachpuff3", "olivedrab3", "mediumblue", "lightseagreen", "lemonchiffon3", "hotpink2", "dodgerblue", "darkblue", "bisque1", "burlywood3", "firebrick1",
    "royalblue1", "violetred1", "steelblue1", "rosybrown", "palegreen3", "mintcream", "lightyellow3", "lightgoldenrod2", "khaki1", "goldenrod2", "darkorchid2", "chartreuse3",
    "aquamarine2", "darkcyan", "orchid", "snow2", "violetred", "seashell1", "peachpuff2", "olivedrab2", "mediumaquamarine", "lightsalmon4", "lemonchiffon2", "hotpink1",
    "deepskyblue3", "cyan3", "bisque", "burlywood4", "forestgreen", "royalblue4", "violetred3", "springgreen3", "red3", "palegreen1", "mediumvioletred", "lightyellow2",
    "lightgoldenrod1", "khaki", "goldenrod1", "darkorchid1", "chartreuse2", "aquamarine3", "darkgoldenrod2", "orchid1", "snow4", "turquoise3", "seashell", "peachpuff1",
    "olivedrab1", "maroon4", "lightsalmon3", "lemonchiffon1", "hotpink", "deepskyblue2", "cyan2", "beige", "cadetblue", "gainsboro", "salmon3", "wheat",
    "springgreen2", "red2", "palegreen", "mediumturquoise", "lightyellow1", "lightgoldenrod", "ivory4", "goldenrod", "darkorchid", "chartreuse1", "aquamarine4", "darkkhaki",
    "orchid3", "springgreen1", "turquoise1", "seagreen4", "peachpuff", "olivedrab", "maroon3", "lightsalmon2", "lemonchiffon", "honeydew4", "deepskyblue1", "cornsilk4",
    "azure4", "cadetblue1", "ghostwhite", "sandybrown", "wheat2", "springgreen", "purple4", "palegoldenrod", "mediumspringgreen", "lightsteelblue4", "lightcyan4", "ivory3",
    "gold3", "darkorange4", "chartreuse", "azure", "darkolivegreen3", "palegreen2", "springgreen4", "tomato3", "seagreen3", "papayawhip", "navyblue", "maroon2",
    "lightsalmon1", "lawngreen", "honeydew3", "deeppink4", "cornsilk3", "azure3", "cadetblue2", "gold", "seagreen", "wheat4", "snow3", "purple3",
    "orchid4", "mediumslateblue", "lightsteelblue3", "lightcyan3", "ivory2", "gold2", "darkorange3", "cadetblue4", "azure1", "darkorange1", "paleturquoise2", "steelblue2",
    "tomato1", "seagreen2", "palevioletred4", "navy", "maroon1", "lightsalmon", "lavenderblush4", "honeydew2", "deeppink3", "cornsilk1", "azure2", "cadetblue3",
    "gold4", "seagreen1", "yellow1", "snow1", "purple1", "orchid2", "mediumseagreen", "lightsteelblue2", "lightcyan2", "ivory1", "gold1"
  )

  return(colors)
}

#' Build the active theme group-color palette
#'
#' Reads the app-level theme palette option and falls back to the default
#' Bootstrap status colors when the option is not set.
#'
#' @param n Number of colors required.
#'
#' @return Character vector of colors recycled to length `n`.
themeGroupPalette <- function(n = 1) {
  pal <- getOption("shinyRNAseq.fresh_palette", NULL)
  if (is.null(pal) || length(pal) == 0) {
    tc <- getOption("shinyRNAseq.theme.colors", list())
    pal <- unique(c(
      tc$primary, tc$info, tc$success, tc$warning, tc$danger,
      "#2FA4E7", "#5bc0de", "#73A839", "#DD5600", "#C71C22"
    ))
    pal <- pal[!is.na(pal) & nzchar(pal)]
  }
  if (length(pal) == 0) pal <- c("#2FA4E7")
  rep(pal, length.out = max(1, n))
}

#' Map group labels to theme colors
#'
#' Creates a named color vector for unique non-empty group labels using the
#' active theme palette.
#'
#' @param groups Vector of group labels.
#'
#' @return Named character vector mapping group labels to colors.
themeGroupColorMap <- function(groups) {
  groups <- unique(as.character(groups))
  groups <- groups[!is.na(groups) & nzchar(groups)]
  if (length(groups) == 0) {
    return(setNames(character(0), character(0)))
  }
  cols <- themeGroupPalette(length(groups))
  setNames(cols, groups)
}

#' Apply theme colors to metadata groups
#'
#' Replaces or creates the `COLOR` column in metadata using the current theme
#' group color map.
#'
#' @param meta Metadata data frame containing a `GROUP` column.
#'
#' @return Metadata data frame with updated `COLOR` values, or input unchanged.
applyThemeColorsToMeta <- function(meta) {
  if (is.null(meta) || !is.data.frame(meta) || !"GROUP" %in% colnames(meta)) {
    return(meta)
  }
  cmap <- themeGroupColorMap(meta$GROUP)
  meta$COLOR <- unname(cmap[as.character(meta$GROUP)])
  meta
}
################################
#' Write or update an analysis log entry
#'
#' Appends a tab-delimited log record or replaces an existing record for the
#' same action and parameter combination.
#'
#' @param file Path to the log file.
#' @param action Action label to record.
#' @param parameter Parameter name to record.
#' @param value Parameter value to record.
#'
#' @return Invisibly writes the updated log table to disk.
logIt <- function(file = logFile, action = NA, parameter = NA, value = NA) {
  # mode: insert - insert a record; update - update; delete - delete
  # DF:  timestamp, section, parameter, value
  options(stringsAsFactors = FALSE)
  ts <- paste0("[", strtrim(Sys.time(), 19), "]")
  res <- data.frame(time = ts, action = action, parameter = parameter, value = value)
  if (file.exists(file)) {
    DF <- read.table(file, sep = "\t", header = TRUE, fill = TRUE, quote = "", row.names = NULL, check.names = F)
    if (tolower(parameter) == "boxplot") { # allow multiple values
      ind <- which(DF$action == action & DF$parameter == parameter & DF$value == value)[1] # assume that there is no duplicate parameter in the same section
    } else {
      ind <- which(DF$action == action & DF$parameter == parameter)[1] # assume that there is no duplicate parameter in the same section
    }
    if (!is.na(ind) & length(ind) == 1) {
      DF[ind, ] <- res
    } else {
      DF <- rbind(DF, res)
      DF <- DF[order(DF[, "action"], DF[, "parameter"], DF[, "value"]), ]
    }
  } else {
    DF <- res
  }
  write.table(DF, file, sep = "\t", quote = F, row.names = F, col.names = T)
}

####################################
#' Clean gene identifiers and drop invalid rows
#'
#' Trims gene names, fills missing identifiers from an optional fallback vector,
#' and removes rows that still lack usable identifiers.
#'
#' @param counts Count or annotation data frame whose rows correspond to `genes`.
#' @param genes Vector of gene identifiers.
#' @param fallback Optional fallback identifier vector.
#' @param context Label used in warning messages.
#'
#' @return List with cleaned `counts` and `genes`.
CleanGeneIdentifiers <- function(counts, genes, fallback = NULL, context = "ReadCount") {
  genes <- trimws(as.character(genes))
  missing <- is.na(genes) | !nzchar(genes)

  if (!is.null(fallback)) {
    fallback <- trimws(as.character(fallback))
    use_fallback <- missing & !is.na(fallback) & nzchar(fallback)
    genes[use_fallback] <- fallback[use_fallback]
    missing <- is.na(genes) | !nzchar(genes)
  }

  if (any(missing)) {
    cat("\nWarning[", context, "]: dropping ", sum(missing),
      " row(s) with missing gene identifiers.\n",
      sep = ""
    )
    counts <- counts[!missing, , drop = FALSE]
    genes <- genes[!missing]
  }

  if (nrow(counts) == 0) {
    stop("No rows with valid gene identifiers were found in the count matrix.")
  }

  list(counts = counts, genes = genes)
}

#' Read and standardize an RNA-seq count table
#'
#' Loads a count matrix, applies optional gene annotation filters, normalizes the
#' identifier columns, and returns counts together with annotation metadata.
#'
#' @param countFile Path to the count matrix file.
#' @param fill Passed to `read.table`.
#' @param check.names Passed to `read.table`.
#' @param header Passed to `read.table`.
#' @param sep Field separator used in the count file.
#' @param quote Quote character used in the count file.
#' @param row.names Row names argument passed to `read.table`.
#' @param filterFlag Logical flag to filter by biotype and annotation level.
#'
#' @return Invisible list with `counts`, `annotation`, `counts_total`, and `raw`.
ReadCount <- function(countFile, fill = TRUE, check.names = FALSE,
                      header = TRUE, sep = "\t", quote = "", row.names = NULL,
                      filterFlag = T) {
  counts <- read.table(countFile,
    fill = fill, check.names = FALSE,
    stringsAsFactors = F, row.names = row.names,
    header = header, sep = sep, quote = quote
  )
  counts <- na.omit(counts)
  if ("geneSymbol" %in% colnames(counts)) {
    idx_empty <- is.na(counts$geneSymbol) | counts$geneSymbol %in% ""
    if (any(idx_empty) && "geneID" %in% colnames(counts)) {
      counts$geneSymbol[idx_empty] <- counts$geneID[idx_empty]
      idx_empty <- is.na(counts$geneSymbol) | counts$geneSymbol %in% ""
    }
    if (any(idx_empty)) {
      counts$geneSymbol[idx_empty] <- paste0("noname_", seq_len(sum(idx_empty)))
    }
  }
  raw <- counts
  if (filterFlag == T) {
    if ("bioType" %in% colnames(counts)) {
      counts <- counts[counts$bioType == "protein_coding", ]
    }
    if ("annotationLevel" %in% colnames(counts)) {
      counts <- counts[counts$annotationLevel <= 3, ]
    }
  }

  if ("geneSymbol" %in% colnames(counts)) {
    genes <- counts[, "geneSymbol"]
  } else if ("gene" %in% tolower(colnames(counts))) {
    genes <- counts[, match("gene", tolower(colnames(counts)))]
  } else if ("symbol" %in% tolower(colnames(counts))) {
    genes <- counts[, match("symbol", tolower(colnames(counts)))]
  } else {
    cat("\n Warning in RNAseqDE function: gene, symbol or geneSymbol column was not found in count table!")
    cat("\n Unique values of the first column will be used as gene column\n\n")
    genes <- counts[, 1]
  }
  fallback_gene_ids <- if ("geneID" %in% colnames(counts)) counts$geneID else NULL
  cleaned <- CleanGeneIdentifiers(counts, genes, fallback = fallback_gene_ids, context = "ReadCount")
  counts <- cleaned$counts
  genes <- cleaned$genes

  if ("geneID" %in% colnames(counts)) {
    # multiple ENSEMBL IDs could be assigned to one gene symbol.
    if (!"geneSymbol" %in% colnames(counts)) {
      counts$geneSymbol <- genes
    }
    missing_symbols <- is.na(counts$geneSymbol) | !nzchar(trimws(as.character(counts$geneSymbol)))
    counts$geneSymbol[missing_symbols] <- genes[missing_symbols]
    annotation <- counts[, c("geneID", "geneSymbol")] # keep intact ensembl id and gene name
    ind_nonUniqGenes <- duplicated(genes)
    genes[ind_nonUniqGenes] <- paste(genes[ind_nonUniqGenes], "_", counts$geneID[ind_nonUniqGenes], sep = "")
    genes <- make.unique(genes)
    annotation$geneSymbol.ID <- genes
    rownames(annotation) <- genes
    rownames(counts) <- genes
  } else {
    keep_unique <- !duplicated(genes)
    if (any(!keep_unique)) {
      cat("\nWarning[ReadCount]: dropping ", sum(!keep_unique),
        " duplicated gene identifier row(s) in no-annotation input.\n",
        sep = ""
      )
    }
    counts <- counts[keep_unique, , drop = FALSE] # make sure rownames are unique
    genes <- genes[keep_unique]
    rownames(counts) <- genes
    annotation <- NA
  }
  kept <- !tolower(colnames(counts)) %in% tolower(c("geneID", "gene", "symbol", "geneSymbol", "bioType", "annotationLevel"))
  counts <- counts[, colnames(counts)[kept]]
  counts <- cbind(gene = genes, counts)
  return(invisible(list(counts = counts, annotation = annotation, counts_total = NULL, raw = raw)))
}

######################################################
#' Read and validate sample metadata
#'
#' Standardizes metadata column names, validates required fields, and adds plot
#' aesthetics such as colors and symbols when missing.
#'
#' @param metaFile Path to the metadata file.
#' @param header Passed to `read.table`.
#' @param sep Field separator used in the metadata file.
#' @param quote Quote character used in the metadata file.
#' @param fill Passed to `read.table`.
#' @param check.names Passed to `read.table`.
#' @param allColors Character vector of colors used when `COLOR` is absent.
#'
#' @return List with `used`, `raw`, `valid`, `errors`, and `warnings`.
ReadMeta <- function(metaFile, header = TRUE, sep = "\t", quote = "", fill = TRUE, check.names = FALSE, allColors = standardColors()) {
  # validation of format and add COLOR column
  meta <- read.table(metaFile,
    fill = fill, check.names = FALSE,
    header = header,
    sep = sep,
    quote = quote
  )
  colnames(meta) <- toupper(colnames(meta))
  colnames(meta) <- gsub(" ", "", colnames(meta))
  raw <- meta
  anyErrors <- 0
  msg <- NULL
  anyWarnings <- 0
  if (!all(c("ID", "NEWNAME", "GROUP") %in% colnames(meta))) {
    anyErrors <- anyErrors + 1
    msg <- paste0(msg, "\nError #", anyErrors, ": ID, NEWNAME or GROUP column is missing!")
  }
  if (any(duplicated(meta$ID))) {
    anyErrors <- anyErrors + 1
    msg <- paste0(msg, "\nError #", anyErrors, ": Found non-unique IDs!")
  }
  if (any(duplicated(meta$NEWNAME))) {
    anyErrors <- anyErrors + 1
    msg <- paste0(msg, "\nError #", anyErrors, ": Found non-unique NEWNAME!")
  }

  if (any(str_detect(meta$ID, "[[^[:alnum:]^[-_.]]]"))) {
    anyErrors <- anyErrors + 1
    msg <- paste0(msg, "\nError #", anyErrors, ": Found non alphanumeric characters in IDs!")
  }
  if (any(str_detect(meta$NEWNAME, "[[^[:alnum:]^[-_.]]]"))) {
    anyErrors <- anyErrors + 1
    msg <- paste0(msg, "\nError #", anyErrors, ": Found non alphanumeric characters in NEWNAMEs!")
  }
  if (sum(is.na(meta$GROUP)) > 0 | sum(meta$GROUP == "") > 0) {
    anyErrors <- anyErrors + 1
    msg <- paste0(msg, "\nError #", anyErrors, ": detect NA(s) in GROUP column!")
  } else {
    if (any(str_detect(meta$GROUP, "[[^[:alnum:]^[-_.]]]"))) {
      anyErrors <- anyErrors + 1
      msg <- paste0(msg, "\nError #", anyErrors, ": Found non alphanumeric characters in GROUPs!")
    }
    if (length(unique(meta$GROUP)) == 1) {
      anyErrors <- anyErrors + 1
      msg <- paste0(msg, "\nError #", anyErrors, ": Found only one GROUP in meta data file. Need at least two for a pairwise comparison!")
    }
    if (min(table(meta$GROUP)) < 2) {
      anyWarnings <- anyWarnings + 1
      msg <- paste0(msg, "\nWarning #", anyErrors, ": techinically need at least two replicates for each group!")
    }
  }
  if (anyErrors > 0) {
    cat("\n** Invalid meta data file. [", anyErrors, " errors detected.] **")
    if (requireNamespace("shiny", quietly = TRUE) &&
      !is.null(shiny::getDefaultReactiveDomain()) &&
      exists("shinyalert", mode = "function")) {
      shinyalert(title = "Warning!", text = paste0(msg, "\n\nPlease upload a new valid meta data file or \nclick [Edit Metadata in QuickFile] or \n click [Create metadata From Matrix in QuickFile] to create a valid metadata file. \n\nOnly alphanumeric characters and [-_.] are acceptable in ID/NEWNAME/GROUP columns."), type = "warning", confirmButtonCol = "#DD5600")
    }
    return(list(used = meta, raw = raw, valid = FALSE, errors = msg, warnings = anyWarnings))
  }

  meta$ID <- gsub(" ", "", meta$ID)
  rownames(meta) <- meta$ID
  meta$GROUP <- gsub(" ", "", meta$GROUP)
  meta$NEWNAME <- gsub(" ", "", meta$NEWNAME)
  if (!"COLOR" %in% colnames(meta)) {
    meta$COLOR <- allColors[as.integer(as.factor(meta$GROUP))]
  }
  if ("SHAPE" %in% colnames(meta)) {
    meta$SHAPE[meta$SHAPE == "" | is.na(meta$SHAPE)] <- "NA"
    if (length(unique(meta$SHAPE)) <= 5) {
      shapes <- c(21, 23, 24, 22, 25)
    } else {
      shapes <- c(19, 17, 15, 18, 16, 14:0)
    }
    meta$SYMBOL <- shapes[as.integer(as.factor(meta$SHAPE))]
  }
  return(list(used = meta, raw = raw, valid = TRUE, errors = NULL, warnings = anyWarnings))
}

######################################################
#' Match metadata sample IDs to count columns safely.
#'
#' Exact matches are preferred. Fixed-string substring fallback is accepted only
#' when each accepted metadata ID maps to one unique count column.
#'
#' @param meta Metadata data frame containing sample identifiers.
#' @param count_cols Character vector of count-table column names.
#' @param id_col Metadata column containing sample IDs.
#'
#' @return List with logical `kept` sample flags and optional column `mapping`.
MatchMetaToCountColumns <- function(meta, count_cols, id_col = "ID") {
  if (!is.data.frame(meta) || !id_col %in% colnames(meta)) {
    stop("Metadata must contain column: ", id_col)
  }
  ids <- as.character(meta[[id_col]])
  count_cols <- as.character(count_cols)
  kept <- ids %in% count_cols
  mapping <- NULL

  if (length(ids) > 0 && sum(kept) < length(ids) / 2) {
    candidate_mapping <- lapply(ids, function(mid) {
      which(grepl(mid, count_cols, fixed = TRUE))
    })
    valid <- vapply(candidate_mapping, function(m) length(m) == 1L, logical(1))
    hit_cols <- unlist(candidate_mapping[valid], use.names = FALSE)
    if (length(hit_cols) > 0) {
      unique_hits <- !duplicated(hit_cols) & !duplicated(hit_cols, fromLast = TRUE)
      valid[valid] <- unique_hits
    }

    if (sum(valid) > sum(kept)) {
      kept <- valid
      mapping <- candidate_mapping
    }
  }

  list(kept = kept, mapping = mapping)
}

#' Rename count columns from metadata mapping
#'
#' Applies the unique substring mapping produced by
#' [MatchMetaToCountColumns()] so count columns use metadata sample IDs.
#'
#' @param counts Count data frame or matrix.
#' @param meta Metadata data frame.
#' @param match_info Result from [MatchMetaToCountColumns()].
#' @param id_col Metadata column containing sample IDs.
#'
#' @return Count object with selected columns renamed.
ApplySampleColumnMapping <- function(counts, meta, match_info, id_col = "ID") {
  if (is.null(match_info$mapping)) {
    return(counts)
  }
  ids <- as.character(meta[[id_col]])
  for (i in which(match_info$kept)) {
    hit <- match_info$mapping[[i]]
    if (length(hit) == 1L && hit <= ncol(counts)) {
      colnames(counts)[hit] <- ids[i]
    }
  }
  counts
}

#' Align metadata rows to count matrix columns
#'
#' Matches metadata samples to count columns, applies safe column renaming when
#' needed, and filters metadata to matched samples.
#'
#' @param meta Metadata data frame.
#' @param counts Count data frame or matrix.
#' @param id_col Metadata column containing sample IDs.
#'
#' @return List with aligned `meta`, renamed `counts`, kept flags, and match info.
AlignMetaToCounts <- function(meta, counts, id_col = "ID") {
  match_info <- MatchMetaToCountColumns(meta, colnames(counts), id_col = id_col)
  counts <- ApplySampleColumnMapping(counts, meta, match_info, id_col = id_col)
  meta <- meta[match_info$kept, , drop = FALSE]
  list(meta = meta, counts = counts, kept = match_info$kept, match_info = match_info)
}

#' Align count columns to metadata samples
#'
#' Renames count columns according to metadata IDs when a unique safe mapping is
#' available.
#'
#' @param counts Count data frame or matrix.
#' @param meta Metadata data frame.
#' @param id_col Metadata column containing sample IDs.
#'
#' @return Count object with matched columns renamed when needed.
AlignCountsToMeta <- function(counts, meta, id_col = "ID") {
  match_info <- MatchMetaToCountColumns(meta, colnames(counts), id_col = id_col)
  ApplySampleColumnMapping(counts, meta, match_info, id_col = id_col)
}

######################################################
#' Count genes passing a cutoff within each group
#'
#' For each metadata group, flags rows that exceed the supplied cutoff in all
#' samples from that group.
#'
#' @param dat Numeric matrix or data frame of expression values.
#' @param meta Metadata table containing at least `ID` and `GROUP`.
#' @param cutoff Numeric cutoff applied per sample.
#'
#' @return Logical data frame indicating pass/fail status by group.
CountIfsByGrp <- function(dat, meta, cutoff = 1) {
  res <- NULL
  cn <- NULL
  colnames(meta) <- toupper(colnames(meta))
  for (GROUP in unique(meta$GROUP)) {
    cols_grp <- colnames(dat)[colnames(dat) %in% meta$ID[meta$GROUP == GROUP]]
    group_size <- length(cols_grp)
    if (length(cols_grp) == 1) {
      tmp <- dat[, cols_grp] > cutoff
    } else {
      tmp <- rowSums(dat[, cols_grp] > cutoff) == group_size
    }
    tmpDF <- data.frame(tmp)
    if (is.null(res)) {
      res <- tmpDF
      cn <- GROUP
    } else {
      cn <- c(cn, GROUP)
      res <- cbind(res, tmpDF)
    }
  }
  colnames(res) <- cn
  rownames(res) <- rownames(dat)
  return(res)
}


###############################################################
######################################################
#  10/17/2023 generate three TMM normalized matrix and CLS file for standard GSEA
#######################################################
#' Generate TMM and CLS files for classic GSEA
#'
#' Produces a TMM-normalized expression matrix and matching phenotype label file
#' for downstream Broad GSEA runs.
#'
#' @param counts Count matrix with genes by samples.
#' @param annotation Optional annotation table with gene identifiers.
#' @param meta Metadata table containing sample IDs and groups.
#' @param prefix Output prefix for generated files.
#'
#' @return Logical flag indicating whether files were generated.
GenerateTMMnCLS4GSEA <- function(counts, annotation = NULL, meta = NA, prefix = NA) {
  if (sum(table(meta$GROUP) > 2) < 2) { # don't generate any file if there are less than 2 groups with sample size >=3
    return(FALSE)
  }

  aligned <- AlignMetaToCounts(meta, counts)
  meta <- aligned$meta
  counts <- aligned$counts
  if (nrow(meta) == 0) {
    cat("\nError[GenerateTMMnCLS4GSEA]: no metadata samples matched count matrix columns.\n")
    return(FALSE)
  }
  counts <- counts[, meta$ID]

  dge <- DGEList(counts = counts, group = meta$GROUP)
  median.lib.size <- median(dge$samples$lib.size)
  cutoff_auto <- median.lib.size / 10e6 # Keep genes with raw read counts >  1 per 10 million reads (  equivalent to a  CUTOFF_CPM of 0.1
  cutoff <- as.vector(cpm(cutoff_auto, median.lib.size))
  keep <- rowSums(cpm(dge) > cutoff) >= min(table(meta$GROUP))
  dge <- dge[keep, keep.lib.sizes = FALSE]
  dge <- calcNormFactors(dge)
  cpm_result <- as.data.frame(cpm(dge, log = FALSE))
  cpm_result <- round(cpm_result, 2)
  if (!is.null(annotation) && all(c("geneID", "geneSymbol") %in% colnames(annotation))) {
    dat_TMM <- cbind(annotation[rownames(cpm_result), c("geneID", "geneSymbol")], cpm_result)
    dat_TMM$geneID <- str_split_fixed(dat_TMM$geneID, "\\.", 2)[, 1]
    colnames(dat_TMM)[1:2] <- c("NAME", "DESCRIPTION")
  } else {
    dat_TMM <- cbind(NAME = rownames(cpm_result), DESCRIPTION = NA, cpm_result)
  }

  outFile <- paste0(prefix, "_GSEAinput_TMM.txt")
  write.table(dat_TMM, file = outFile, sep = "\t", col.names = TRUE, row.names = F, quote = F)
  #-----------------------------------------------------------------------------
  groups <- unique(meta$GROUP)
  phenoLblFile <- paste0(prefix, "_GSEAinput.cls")
  phenoLbls <- paste0(
    paste(nrow(meta), length(groups), 1, sep = " "),
    "\n# ", paste(groups, collapse = " "),
    "\n", paste(as.character(meta$GROUP), collapse = " "), "\n"
  )

  cat(file = phenoLblFile, text = phenoLbls, append = F)
  cat("\n", basename(phenoLblFile), "[saved]\n")
  return(TRUE)
}
#--------------------------------------------------------------

#' Run two-group RNA-seq differential expression
#'
#' Matches samples to metadata, filters low-expression genes, applies
#' limma-voom modeling, and optionally writes a result table to disk.
#'
#' @param counts Count table used for differential expression.
#' @param annotation Optional annotation table.
#' @param meta Metadata table with IDs and group labels.
#' @param comp Comparison string in the form `Exp_VS_Ctl`.
#' @param outFile Optional output file path.
#' @param cutoff_count Raw-count filtering threshold.
#' @param logFile Optional log file path.
#'
#' @return Invisible list with `DEGs` table and aligned `meta` table.
RNAseqDE <- function(counts, annotation = NULL, meta = meta, comp = comp, outFile = NA, cutoff_count = 0, logFile = NA) {
  if (!is.data.frame(counts)) {
    cat("\n** Error in RNAseqDE function: parameter [counts] must be a data.frame! **\n\n")
    stop("Exit...")
  }
  if (!is.data.frame(meta)) {
    cat("\n** Error in RNAseqDE function: parameter [meta] must be a data.frame! **\n\n")
    stop("Exit...")
  }

  comp <- gsub(" ", "", comp)
  comp <- gsub("_[vV][Ss]_", "_VS_", comp)
  keywords <- unlist(strsplit(comp, "_VS_")) # Exp_vs_Ctl ==> Exp, Ctl
  ExpLabel <- keywords[1]
  CtlLabel <- keywords[2]

  aligned <- AlignMetaToCounts(meta, counts)
  meta <- aligned$meta
  counts <- aligned$counts
  if (nrow(meta) == 0) {
    stop("No metadata samples matched count matrix columns.")
  }

  usedNewName <- FALSE
  partialMatchFlag <- FALSE
  if (usedNewName & "NEWNAME" %in% colnames(meta)) { # complete match
    Exp <- meta[toupper(meta$GROUP) == toupper(ExpLabel), "NEWNAME"]
    Ctl <- meta[toupper(meta$GROUP) == toupper(CtlLabel), "NEWNAME"]
    rownames(meta) <- meta$NEWNAME
  } else {
    Exp <- meta[toupper(meta$GROUP) == toupper(ExpLabel), "ID"]
    Ctl <- meta[toupper(meta$GROUP) == toupper(CtlLabel), "ID"]
    rownames(meta) <- meta$ID
  }
  if (min(length(Exp), length(Ctl)) < 2) { # partial match, 8/28/2020
    cat("\nWarning - no matched comparison. Trying partial match. ")
    if (usedNewName & "NEWNAME" %in% colnames(meta)) {
      Exp <- meta[grepl(ExpLabel, meta$GROUP, ignore.case = T), "NEWNAME"]
      Ctl <- meta[grepl(CtlLabel, meta$GROUP, ignore.case = T), "NEWNAME"]
      rownames(meta) <- meta$NEWNAME
    } else {
      Exp <- meta[grepl(ExpLabel, meta$GROUP, ignore.case = T), "ID"]
      Ctl <- meta[grepl(CtlLabel, meta$GROUP, ignore.case = T), "ID"]
      rownames(meta) <- meta$ID
    }
    nonUniqMatch <- length(intersect(Exp, Ctl))
    if (nonUniqMatch > 0) {
      cat(paste0("\n\t Error: non-unique partial match between comparison string and meta$GROUP [n = ", nonUniqMatch, "]"))
      stop(paste0("Non-unique partial match between comparison and meta$GROUP [n = ", nonUniqMatch, "]"))
    }
    if (min(length(Exp), length(Ctl)) < 2) {
      cat("\n Error: no /not enough matched samples for comparison.")
      msg <- paste0("[", ExpLabel, ", n = ", length(Exp), "; ", CtlLabel, ", n = ", length(Ctl), "]")
      print(msg)
      cat("\n\tPlease check whether meta$GROUP matches with  comparisons.lst.\n")
      stop(paste0("Not enough matched samples for comparison. ", msg))
    }
    cat(paste0("[looks good]\n"))
    partialMatchFlag <- TRUE
  }
  grp <- c(Exp, Ctl)
  meta <- meta[grp, ]

  rownames(counts) <- counts$gene
  counts <- counts[, meta$ID]

  if ("NEWNAME" %in% colnames(meta) & usedNewName) {
    colnames(counts) <- meta[match(colnames(counts), meta$ID), "NEWNAME"]
  }

  # =========TMM normalization and voom transformation===========
  # cat("\n\nTMM normalization...\n")
  if (partialMatchFlag) { # change group label if using partial match
    usedGroup <- meta$GROUP
    usedGroup[grepl(ExpLabel, meta$GROUP)] <- ExpLabel
    usedGroup[grepl(CtlLabel, meta$GROUP)] <- CtlLabel
  } else {
    usedGroup <- meta$GROUP
  }
  # print(usedGroup)
  dge <- DGEList(counts = counts, group = usedGroup)
  if (F) {
    cutoff <- as.vector(cpm(cutoff_count, mean(dge$samples$lib.size))) # version 1
    cat("\ncutoff=", cutoff_count)
    keep <- rowSums(cpm(dge) > cutoff) >= min(length(Exp), length(Ctl))
    cat("\n\t1.mean.lib.size filteration: Keep ", sum(keep), " out of ", dim(dge)[1], " genes\n")
    # group_cpm_flag <- CountIfsByGrp(cpm(dge),meta,cutoff)         # version 2 keep genes with CPM > cutoff in at least one sample group 2/3/2022
    # keep <- rowSums(group_cpm_flag)>0
    cutoff <- as.vector(cpm(cutoff_count, median(dge$samples$lib.size))) # version 3
  }
  # version 4
  median.lib.size <- median(dge$samples$lib.size)
  if (cutoff_count == 0) {
    if (median.lib.size > 100e6) {
      cutoff_auto <- 10
    } else if (median.lib.size < 10e6) {
      cutoff_auto <- 1
    } else {
      cutoff_auto <- median.lib.size / 10e6
    }
    cutoff <- as.vector(cpm(cutoff_auto, median.lib.size)) # version 4
  } else {
    cutoff_auto <- cutoff_count
    cutoff <- as.vector(cpm(cutoff_count, median.lib.size)) # version 4
  }
  cat("\n1. cutoff_auto=", cutoff_auto)

  keep <- rowSums(cpm(dge) > cutoff) >= min(length(Exp), length(Ctl))
  cat("\n2. median.lib.size filteration: Keep ", sum(keep), " out of ", dim(dge)[1], " genes\n")
  dge <- dge[keep, keep.lib.sizes = FALSE]

  TS <- c(rep("Exp", length(Exp)), rep("Ctl", length(Ctl)))
  TS <- factor(TS, levels = c("Exp", "Ctl"))
  if ("BATCH" %in% colnames(meta)) {
    BATCH <- factor(meta$BATCH)
    if (length(levels(BATCH)) > 1) { # 8/28/2020
      design <- model.matrix(~ 0 + TS + BATCH)
      colnames(design)[1:2] <- c(levels(TS)) # 4/5/2018 revised
    } else {
      design <- model.matrix(~ 0 + TS)
      colnames(design) <- c(levels(TS))
    }
  } else if ("GENDER" %in% colnames(meta)) {
    GENDER <- factor(meta$GENDER)
    if (length(levels(GENDER)) > 1) { # 8/28/2020
      design <- model.matrix(~ 0 + TS + GENDER) # GENDER correction?
      colnames(design)[1:2] <- c(levels(TS)) # 4/5/2018 revised
    } else {
      design <- model.matrix(~ 0 + TS + GENDER) # GENDER correction?
      colnames(design) <- c(levels(TS))
    }
  } else {
    design <- model.matrix(~ 0 + TS)
    colnames(design) <- c(levels(TS))
  }
  # print(design)

  dge <- calcNormFactors(dge)
  cpm_result <- as.data.frame(cpm(dge, log = T)) # HJ added 2017/11/20
  cpm_result <- round(cpm_result, 2)

  # cat("\nRunning Voom transformation...\n")
  v <- voom(dge, design, plot = F)

  Exp.mean <- apply(cpm_result[, Exp], 1, mean)
  Ctl.mean <- apply(cpm_result[, Ctl], 1, mean)

  # ===========voom=============
  fit <- lmFit(v, design)
  cont.matrix <- makeContrasts(Diff = (Exp - Ctl), levels = design)
  fitcon <- contrasts.fit(fit, cont.matrix)
  fitcon <- eBayes(fitcon)
  comb <- topTable(fitcon, n = nrow(dge))

  comb <- comb[, !colnames(comb) %in% c("B")] #  "t"
  comb$logFC <- round(comb$logFC, 2)
  comb$AveExpr <- round(comb$AveExpr, 2)
  colnames(comb) <- gsub("^t$", "t.Statistic", colnames(comb))
  colnames(comb) <- gsub("logFC", "log2FC", colnames(comb))

  # comb$P.Value <- format.pval(comb$P.Value)
  # comb$adj.P.Val <- format.pval(comb$adj.P.Val)
  comb_ncol <- ncol(comb)
  used <- cbind(comb, round(Exp.mean[rownames(comb)], 2), round(Ctl.mean[rownames(comb)], 2))
  colnames(used)[(comb_ncol + 1):(comb_ncol + 2)] <- paste(keywords, ".AveExpr", sep = "")
  res <- cbind(used, round(v$E[rownames(comb), ], 2))
  diff_table <- cbind(gene = rownames(res), res)

  if (!is.na(outFile)) {
    finalTable <- res
    # colnames(finalTable) <- gsub("logFC", "log2FC", colnames(finalTable))
    colnames(finalTable) <- gsub("adj.P.Val", "FDR", colnames(finalTable))

    if (!is.null(annotation) && all(c("geneID", "geneSymbol") %in% colnames(annotation))) {
      finalTable <- cbind(annotation[rownames(finalTable), c("geneID", "geneSymbol")], finalTable)
    } else {
      finalTable <- cbind(gene = rownames(finalTable), finalTable)
    }
    write.table(finalTable, file = outFile, sep = "\t", col.names = TRUE, row.names = F, quote = F)
  }
  return(invisible(list(DEGs = diff_table, meta = meta)))
}
######################################################
#  8/16/2023, 10/17/2023 generate three rnk files for GSEA
#######################################################
#' Generate ranked gene lists for GSEA
#'
#' Builds multiple RNK files from a differential expression table using signed
#' effect-size and significance based ranking schemes.
#'
#' @param diff_table Differential expression table.
#' @param annotation Optional annotation table containing `geneID`.
#' @param prefix Output prefix for RNK files.
#'
#' @return Logical flag indicating whether all RNK files were written.
GenerateRank4GSEA <- function(diff_table, annotation = NULL, prefix = NA) {
  ind_P <- grep("^P.Value$", colnames(diff_table), ignore.case = T)
  ind_FC <- grep("log.*FC$", colnames(diff_table), ignore.case = T)
  ind_t <- grep("t.Statistic$", colnames(diff_table), ignore.case = T)
  if (any(length(ind_P) == 0, length(ind_FC) == 0, length(ind_t) == 0)) {
    cat("\nError[GenerateRanks4GSEA]: wrong format in diff_table.")
    return(FALSE)
  }
  pvals <- as.numeric(diff_table[, ind_P[1]])
  pvals[is.finite(pvals) & pvals <= 0] <- .Machine$double.xmin
  negLog10pval <- -log10(pvals)
  log2FC <- as.numeric(diff_table[, ind_FC[1]])
  tStat <- as.numeric(diff_table[, ind_t[1]])
  if (!is.null(annotation) && inherits(annotation, "data.frame") && "geneID" %in% colnames(annotation)) {
    annotation$geneID <- str_split_fixed(annotation$geneID, "\\.", 2)[, 1]
    diff_table <- cbind(annotation[rownames(diff_table), c("geneID", "geneSymbol")], diff_table)
    genes <- diff_table$geneID
  } else {
    genes <- rownames(diff_table)
  }

  rnk1 <- data.frame(gene = genes, rank = round(sign(log2FC) * negLog10pval, 5))
  rnk3 <- data.frame(gene = genes, rank = round(tStat, 5))
  print(head(rnk3, n = 2))
  #' Save Rnk File
  #'
  #' Executes SaveRnkFile.
  #'
  #' @param rnk Argument for SaveRnkFile.
  #' @param outFile Argument for SaveRnkFile.
  #'
  #' @return Return value produced by SaveRnkFile.
  SaveRnkFile <- function(rnk, outFile) {
    rnk$gene <- as.character(rnk$gene)

    # Keep Inf rows: map +Inf/-Inf to extreme finite values for fgsea compatibility
    posInf <- is.infinite(rnk$rank) & rnk$rank > 0
    negInf <- is.infinite(rnk$rank) & rnk$rank < 0
    if (any(posInf | negInf)) {
      finiteRanks <- rnk$rank[is.finite(rnk$rank)]
      if (length(finiteRanks) > 0) {
        maxRank <- max(finiteRanks, na.rm = TRUE)
        minRank <- min(finiteRanks, na.rm = TRUE)
      } else {
        maxRank <- 1
        minRank <- -1
      }
      rnk$rank[posInf] <- maxRank * 10
      rnk$rank[negInf] <- minRank * 10
    }

    rnk <- rnk[is.finite(rnk$rank) & !is.na(rnk$gene) & nzchar(rnk$gene), , drop = FALSE]
    if (nrow(rnk) == 0) {
      cat("\nWarning[GenerateRanks4GSEA]: no finite ranks for ", basename(outFile), "\n", sep = "")
      return(FALSE)
    }
    rnk <- rnk[order(rnk[, 2], decreasing = TRUE), , drop = FALSE]
    rnk <- rnk[!duplicated(rnk$gene), , drop = FALSE]
    write.table(rnk, outFile, sep = "\t", quote = FALSE, row.names = FALSE, col.names = FALSE)
    return(TRUE)
  }
  outFile1 <- paste0(prefix, "_sign.log2FCxNegLog10Pval.rnk")
  ok1 <- SaveRnkFile(rnk1, outFile1)
  outFile3 <- paste0(prefix, "_t.rnk")
  ok3 <- SaveRnkFile(rnk3, outFile3)
  return(all(c(ok1, ok3)))
}

#####################################################
#' Locate key columns in a differential expression table
#'
#' Finds log fold change, p-value, FDR, and average expression columns using
#' configurable name patterns.
#'
#' @param diff_table Differential expression table.
#' @param strLogFC Pattern for log fold change column.
#' @param strPval Pattern for p-value column.
#' @param strAveExpr Pattern for average expression columns.
#' @param strPadj Pattern for adjusted p-value column.
#' @param caseInSensitive Logical flag for case-insensitive matching.
#' @param wholeWordOnly Logical flag for exact-name matching.
#'
#' @return List of matched column indices.
SearchColNames <- function(diff_table, strLogFC = "log2FC", strPval = "P.Value", strAveExpr = "AveExpr", strPadj = "adj.P.Val", caseInSensitive = T, wholeWordOnly = T) {
  ind_logFC <- NA
  ind_AveExpr <- NA
  ind_Padj <- NULL
  ind_Pval <- NA
  if (wholeWordOnly) {
    ind_logFC <- grep(paste0("^", strLogFC, "$"), colnames(diff_table), ignore.case = caseInSensitive)
    ind_AveExpr <- grep(paste0("^", strAveExpr, "$"), colnames(diff_table), ignore.case = caseInSensitive)
    ind_Pval <- grep(paste0("^", strPval, "$"), colnames(diff_table), ignore.case = caseInSensitive)
    if (!is.na(strPadj)) {
      ind_Padj <- grep(paste0(strPadj), colnames(diff_table), ignore.case = caseInSensitive)
    }
  } else {
    ind_logFC <- grep(strLogFC, colnames(diff_table), ignore.case = caseInSensitive)
    ind_AveExpr <- grep(strAveExpr, colnames(diff_table), ignore.case = caseInSensitive)
    ind_Pval <- grep(strPval, colnames(diff_table), ignore.case = caseInSensitive)
    if (!is.null(strPadj)) {
      ind_Padj <- grep(strPadj, colnames(diff_table), ignore.case = caseInSensitive)
    }
  }
  if (any(c(length(ind_logFC) == 0, length(ind_AveExpr) == 0, length(ind_Pval) == 0))) {
    cat("\n** Error: invalid DEG table or wrong pattern strings for matching log2FC, P.Value, AveExpr\n\n")
    stop("Exit...")
  }
  if (!is.null(strPadj)) {
    return(list(iLogFC = ind_logFC, iAveExpr = ind_AveExpr, iPval = ind_Pval, iPadj = ind_Padj))
  } else {
    return(list(iLogFC = ind_logFC, iAveExpr = ind_AveExpr, iPval = ind_Pval))
  }
}

#' Plot Numeric
#'
#' Executes plotNumeric.
#'
#' @param x Argument for plotNumeric.
#' @param field Argument for plotNumeric.
#' @param context Argument for plotNumeric.
#'
#' @return Return value produced by plotNumeric.
plotNumeric <- function(x, field = "value", context = "plot") {
  raw <- x
  out <- suppressWarnings(as.numeric(as.character(x)))
  raw_chr <- trimws(as.character(raw))
  bad_numeric <- is.na(out) & !is.na(raw) & nzchar(raw_chr)
  bad_finite <- !is.na(out) & !is.finite(out)
  n_bad <- sum(bad_numeric | bad_finite, na.rm = TRUE)
  if (n_bad > 0) {
    cat("\nWarning[", context, "]: ", n_bad, " non-numeric/NA/Inf value(s) in ", field,
      " were excluded from plot range calculations.\n",
      sep = ""
    )
  }
  out[!is.finite(out)] <- NA_real_
  out
}

#' Plot Finite Rows
#'
#' Executes plotFiniteRows.
#'
#' @param df Argument for plotFiniteRows.
#' @param fields Argument for plotFiniteRows.
#' @param context Argument for plotFiniteRows.
#'
#' @return Return value produced by plotFiniteRows.
plotFiniteRows <- function(df, fields = c("x", "y"), context = "plot") {
  keep <- rep(TRUE, nrow(df))
  for (field in fields) {
    keep <- keep & is.finite(df[[field]])
  }
  n_drop <- sum(!keep)
  if (n_drop > 0) {
    cat("\nWarning[", context, "]: excluding ", n_drop,
      " row(s) with non-finite plotting coordinates.\n",
      sep = ""
    )
  }
  df[keep, , drop = FALSE]
}

#' Plot Range
#'
#' Executes plotRange.
#'
#' @param x Argument for plotRange.
#' @param include Argument for plotRange.
#' @param symmetric Argument for plotRange.
#' @param lower_zero Argument for plotRange.
#' @param pad Argument for plotRange.
#' @param fallback Argument for plotRange.
#'
#' @return Return value produced by plotRange.
plotRange <- function(x, include = NULL, symmetric = FALSE, lower_zero = FALSE,
                      pad = 0.05, fallback = c(0, 1)) {
  vals <- c(x, include)
  vals <- vals[is.finite(vals)]
  if (length(vals) == 0) {
    if (symmetric) {
      return(c(-1, 1))
    }
    return(fallback)
  }
  if (symmetric) {
    lim <- max(abs(vals), na.rm = TRUE)
    if (!is.finite(lim) || lim <= 0) lim <- 1
    lim <- lim * (1 + pad)
    return(c(-lim, lim))
  }
  rng <- range(vals, na.rm = TRUE)
  if (lower_zero) rng[1] <- min(0, rng[1])
  span <- diff(rng)
  if (!is.finite(span) || span <= 0) {
    pad_abs <- max(abs(rng), 1, na.rm = TRUE) * 0.1
  } else {
    pad_abs <- span * pad
  }
  rng <- c(rng[1] - pad_abs, rng[2] + pad_abs)
  if (lower_zero) rng[1] <- 0
  rng
}
######################################################
#' Summarize differentially expressed genes by threshold
#'
#' Counts up- and down-regulated genes under common p-value, FDR, and fold
#' change cutoffs and optionally returns top-ranked genes.
#'
#' @param diff_table Differential expression table.
#' @param cutoff_logFC Absolute log2 fold-change threshold.
#' @param cutoff_Pval P-value threshold.
#' @param cutoff_FDR FDR threshold.
#' @param topn Optional number of top up/down genes to return.
#' @param strLogFC Pattern for log fold change column.
#' @param strPval Pattern for p-value column.
#' @param strAveExpr Pattern for average expression columns.
#' @param strPadj Pattern for adjusted p-value column.
#'
#' @return List with `numDEGs` summary and `topnDEGs` gene sets.
GetDEGs <- function(diff_table, cutoff_logFC = 1, cutoff_Pval = 0.05, cutoff_FDR = 0.05, topn = NULL,
                    strLogFC = "log2FC", strPval = "P.Value", strAveExpr = "AveExpr", strPadj = "adj.P.Val") {
  # input file is a default voom topTable.
  # cutoffs in default:  P.Value <0.05, FDR <0.05 or/and logFC>1 or < -1,
  # topn: will take topn of up, down genes respectively. So total topDEGs actually is topn*2
  if (!is.data.frame(diff_table)) {
    cat("\n** Error in GetDEGs function: parameter [diff_table] must be a data.frame! **\n\n")
    cat("\n", class(diff_table), "\n")
    stop("Exit...")
  }
  ind_gene <- grep("^gene$", colnames(diff_table), ignore.case = T)[1]
  if (!is.na(ind_gene) & length(ind_gene) > 0) {
    rownames(diff_table) <- diff_table[, ind_gene]
    diff_table <- diff_table[, -ind_gene]
  } else {
    cat("\n Warning in GetDEGs function:  no gene column found in [diff_table]! \n\n")
  }

  # dat <- apply(diff_table, 2,  as.numeric)
  dat <- diff_table
  rownames(dat) <- rownames(diff_table)

  inds <- SearchColNames(dat, strLogFC = strLogFC, strPval = strPval, strAveExpr = strAveExpr, strPadj = "adj.P.Val", caseInSensitive = T, wholeWordOnly = F)
  inds_logFC <- inds$iLogFC[1]
  inds_Pval <- inds$iPval[1]
  inds_Padj <- inds$iPadj[1]
  dat <- dat[order(dat[, inds_logFC], decreasing = T), ] # order by logFC from larow_grpe to small
  genes <- rownames(dat)

  # cutoff1  - pvalue <0.05
  up1 <- unlist(dat[, inds_logFC] > 0 & dat[, inds_Pval] < cutoff_Pval)
  down1 <- unlist(dat[, inds_logFC] < 0 & dat[, inds_Pval] < cutoff_Pval)

  # cutoff2  - FRD <0.05
  up2 <- unlist(dat[, inds_logFC] > 0 & dat[, inds_Padj] < cutoff_FDR)
  down2 <- unlist(dat[, inds_logFC] < 0 & dat[, inds_Padj] < cutoff_FDR)


  # cutoff3  - logFC >1 & pvalue <0.05
  up3 <- unlist(dat[, inds_logFC] > cutoff_logFC & dat[, inds_Pval] < cutoff_Pval)
  down3 <- unlist(dat[, inds_logFC] < -cutoff_logFC & dat[, inds_Pval] < cutoff_Pval)

  # cutoff4  - logFC >1 & FDR <0.05
  up4 <- unlist(dat[, inds_logFC] > cutoff_logFC & dat[, inds_Padj] < cutoff_FDR)
  down4 <- unlist(dat[, inds_logFC] < -cutoff_logFC & dat[, inds_Padj] < cutoff_FDR)

  cutoffs <- c(
    paste0("P.Value<", cutoff_Pval),
    paste0("FDR<", cutoff_FDR),
    paste0("|log2FC|>", cutoff_logFC, " & P.Value<", cutoff_Pval),
    paste0("|log2FC|>", cutoff_logFC, " & FDR<", cutoff_FDR)
  )
  nums_up <- c(sum(up1), sum(up2), sum(up3), sum(up4))
  nums_down <- c(sum(down1), sum(down2), sum(down3), sum(down4))

  numDEGs <- data.frame(cutoff = cutoffs, up = nums_up, down = nums_down)
  numDEGs$total <- numDEGs$up + numDEGs$down
  topUpGenes <- NULL
  topDownGenes <- NULL
  topnDEGs <- list(topUpGenes = NULL, topDownGenes = NULL)
  if (!is.null(topn)) {
    topn_up <- ifelse(sum(up2) >= topn, topn, sum(up2))
    topn_down <- ifelse(sum(down2) >= topn, topn, sum(down2))
    if (sum(topn_up) == 0) {
      topUpGenes <- NULL
    } else {
      topUpGenes <- head(genes[up2], topn_up)
    }
    if (sum(topn_down) == 0) {
      topDownGenes <- NULL
    } else {
      topDownGenes <- tail(genes[down2], topn_down)
    }
    topnDEGs <- list(topUpGenes = topUpGenes, topDownGenes = topDownGenes)
  }

  results <- list(numDEGs = numDEGs, topnDEGs = topnDEGs)
  return(results)
}
######################################
#' Plot library sizes by sample
#'
#' Summarizes numeric count columns into per-sample library sizes and returns a
#' grouped bar chart.
#'
#' @param counts Count matrix.
#' @param meta Metadata table with `ID` and `GROUP`.
#' @param outFile Optional path to save the figure.
#'
#' @return `ggplot` object.
PlotLibSize <- function(counts = NA, meta = NA, outFile = NA) {
  numeric_cols <- apply(counts, 2, function(x) suppressWarnings(all(!is.na(as.numeric(as.character(x))))))
  counts <- counts[, numeric_cols]
  libSize <- colSums(counts)
  used <- data.frame(ID = colnames(counts), lib.Size = libSize, GROUP = meta$GROUP[match(colnames(counts), meta$ID)])
  used$GROUP <- factor(used$GROUP, levels = unique(used$GROUP))
  used$ID <- factor(used$ID, levels = unique(used$ID))
  grp_cols <- themeGroupColorMap(used$GROUP)
  pg_base <- ggplot(used, aes(ID, lib.Size, fill = GROUP, text = libSize)) +
    geom_bar(aes(fill = GROUP), position = "dodge", stat = "identity") +
    labs(x = NULL, y = "Library Size") +
    scale_fill_manual(values = grp_cols) +
    scale_y_continuous(labels = scales::label_number(suffix = " M", scale = 1e-6))

  # Smaller text in the interactive UI panel.
  pg_ui <- pg_base +
    theme_classic(base_size = 12) +
    theme(
      axis.title = element_text(size = 10),
      axis.text.x = element_text(size = 7, angle = 90, hjust = 1),
      axis.text.y = element_text(size = 7),
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8),
      legend.position = "right"
    )

  # Slightly larger text in saved outputs for readability.
  pg_save <- pg_base +
    theme_classic(base_size = 12) +
    theme(
      axis.title = element_text(size = 11),
      axis.text.x = element_text(size = 9, angle = 90, hjust = 1),
      axis.text.y = element_text(size = 9),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9),
      legend.position = "right"
    )

  if (!is.na(outFile)) {
    ggsave(file = outFile, pg_save, width = 8, height = 6, units = "in")
    cat("\n\t[library size plot saved]\n")
  }
  return(pg_ui)
}
######################################
#' Plot relative log expression values
#'
#' Computes median-centered log expression values per sample and renders an RLE
#' boxplot for quality control.
#'
#' @param logCPM Log2 CPM expression matrix.
#' @param meta Metadata table with sample grouping.
#' @param outFile Optional path to save the figure.
#'
#' @return `ggplot` object or `NULL` for invalid input.
PlotRLE <- function(logCPM = NA, meta = NA, outFile = NA) {
  # version 8/8/2024
  # Gij: log2cpm for gene i in sample j
  # 1.For gene i, calculate its median across all samples (median i)
  # 2. Gij’ =  Gij- median i
  # Do  a boxplot using  Gj’ (median adjusted expression values for all the genes in sample j) for each sample.
  #  add a horizontal line at 0. you can group samples by phenotypes.
  #
  if (is.null(logCPM) | nrow(logCPM) == 0) {
    cat("\n Warn: invalid or empty logCPM table.\n")
    return(NULL)
  } else {
    cat("\n dim:", dim(logCPM))
  }
  rmedian <- apply(logCPM, 1, median) # row median
  dat <- as.data.frame(apply(logCPM, 2, FUN = function(x) {
    x - rmedian
  }))

  cat("\nInfo: dim(dat)", dim(dat))
  used <- reshape2::melt(dat, id.vars = NULL)
  colnames(used) <- c("ID", "log2CPM")
  # Calculate the median for each group
  bp <- boxplot(log2CPM ~ ID, data = used, plot = FALSE)
  bp_df <- as.data.frame(t(bp$stats))
  colnames(bp_df) <- c("min", "lower", "median", "upper", "max")
  bp_df$ID <- factor(bp$names, levels = bp$names) # critical # fixed bug
  bp_df$GROUP <- meta$GROUP[match(bp_df$ID, meta$ID)]
  bp_df$x <- 1:length(bp_df$ID)
  grp_cols <- themeGroupColorMap(bp_df$GROUP)
  library(ggplot2)
  medians <- data.frame(x = bp_df$ID, value = 1:length(bp_df$ID), median = bp_df$median)
  # Generate the boxplot and add median bars using geom_linerange
  pg_base <- ggplot(bp_df, aes(fill = GROUP)) +
    geom_boxplot(aes(x = ID, ymin = min, lower = lower, middle = median, upper = upper, ymax = max, group = ID, fill = GROUP),
      width = 0.8, size = 0, stat = "identity", colour = "black"
    ) +
    geom_errorbar(aes(x = ID, ymin = min, ymax = max, group = ID),
      colour = "black", size = 0.3, width = 0.5
    ) +
    geom_hline(yintercept = 0, color = "grey30", linetype = "dashed") +
    theme_classic(base_size = 12) +
    scale_fill_manual(values = grp_cols) +
    geom_linerange(aes(x = ID, xmin = x - 0.4, xmax = x + 0.4, group = ID, y = median), colour = "black", size = 1) +
    labs(x = NULL, y = "Relative log expression")

  # Keep current dashboard/UI appearance unchanged.
  pg_ui <- pg_base +
    theme(
      axis.title = element_text(size = 13),
      axis.text.x = element_text(size = 11, angle = 90, hjust = 1),
      axis.text.y = element_text(size = 11),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 11),
      legend.position = "right"
    )

  # Match saved font sizes to saved library-size plot.
  pg_save <- pg_base +
    theme(
      axis.title = element_text(size = 11),
      axis.text.x = element_text(size = 9, angle = 90, hjust = 1),
      axis.text.y = element_text(size = 9),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9),
      legend.position = "right"
    )

  if (!is.na(outFile)) {
    ggsave(file = outFile, pg_save, width = 8, height = 6, units = "in")
    cat("\n\t[RLEplot saved]\n")
  }
  return(pg_ui)
}

#' Qc Output Prefix
#'
#' Executes qcOutputPrefix.
#'
#' @param outPrefix Argument for qcOutputPrefix.
#'
#' @return Return value produced by qcOutputPrefix.
qcOutputPrefix <- function(outPrefix) {
  if (is.null(outPrefix) || length(outPrefix) == 0 ||
    is.na(outPrefix) || !nzchar(as.character(outPrefix))) {
    return(NULL)
  }
  outDir <- dirname(as.character(outPrefix))
  qcDir <- file.path(outDir, "QC")
  dir.create(qcDir, recursive = TRUE, showWarnings = FALSE)
  file.path(qcDir, basename(as.character(outPrefix)))
}

#' Qc Write Table
#'
#' Executes qcWriteTable.
#'
#' @param dat Argument for qcWriteTable.
#' @param outFile Argument for qcWriteTable.
#'
#' @return Return value produced by qcWriteTable.
qcWriteTable <- function(dat, outFile) {
  if (is.null(dat) || !is.data.frame(dat) || nrow(dat) == 0) {
    return(FALSE)
  }
  write.table(dat,
    file = outFile, sep = "\t", quote = FALSE,
    row.names = FALSE, col.names = TRUE
  )
  TRUE
}

#' Qc Meta Column
#'
#' Executes qcMetaColumn.
#'
#' @param meta Argument for qcMetaColumn.
#' @param col Argument for qcMetaColumn.
#'
#' @return Return value produced by qcMetaColumn.
qcMetaColumn <- function(meta, col) {
  if (is.null(meta) || !is.data.frame(meta)) {
    return(NULL)
  }
  idx <- match(toupper(col), toupper(colnames(meta)))
  if (is.na(idx)) {
    return(NULL)
  }
  meta[[idx]]
}

#' Calc Ercc Qc Log2 CPM
#'
#' Executes calcErccQcLog2CPM.
#'
#' @param counts Argument for calcErccQcLog2CPM.
#' @param counts_ERCC Argument for calcErccQcLog2CPM.
#' @param meta Argument for calcErccQcLog2CPM.
#'
#' @return Return value produced by calcErccQcLog2CPM.
calcErccQcLog2CPM <- function(counts, counts_ERCC, meta) {
  if (is.null(counts) || is.null(counts_ERCC) || is.null(meta)) {
    return(NULL)
  }
  counts <- as.data.frame(counts, check.names = FALSE)
  counts_ERCC <- as.data.frame(counts_ERCC, check.names = FALSE)
  if (ncol(counts) > 0 && tolower(colnames(counts)[1]) == "gene") {
    counts <- counts[, -1, drop = FALSE]
  }
  if (ncol(counts_ERCC) > 0 && tolower(colnames(counts_ERCC)[1]) == "gene") {
    counts_ERCC <- counts_ERCC[, -1, drop = FALSE]
  }

  meta_ids <- qcMetaColumn(meta, "ID")
  if (is.null(meta_ids)) {
    return(NULL)
  }
  common_ids <- intersect(meta_ids, intersect(colnames(counts), colnames(counts_ERCC)))
  if (length(common_ids) < 2) {
    return(NULL)
  }

  counts <- counts[, common_ids, drop = FALSE]
  counts_ERCC <- counts_ERCC[, common_ids, drop = FALSE]
  counts <- as.data.frame(lapply(counts, function(x) {
    suppressWarnings(as.numeric(as.character(x)))
  }), check.names = FALSE)
  counts_ERCC <- as.data.frame(lapply(counts_ERCC, function(x) {
    suppressWarnings(as.numeric(as.character(x)))
  }), check.names = FALSE)
  valid_counts <- vapply(counts, function(x) all(is.finite(x)), logical(1))
  valid_ercc <- vapply(counts_ERCC, function(x) all(is.finite(x)), logical(1))
  valid_ids <- intersect(names(valid_counts)[valid_counts], names(valid_ercc)[valid_ercc])
  if (length(valid_ids) < 2) {
    return(NULL)
  }

  counts <- counts[, valid_ids, drop = FALSE]
  counts_ERCC <- counts_ERCC[, valid_ids, drop = FALSE]
  dge <- edgeR::DGEList(counts = counts)
  dge <- edgeR::calcNormFactors(dge)
  sf_ercc <- edgeR::calcNormFactors(counts_ERCC, lib.size = dge$samples$lib.size)
  dge$samples$norm.factors <- sf_ercc
  edgeR::cpm(dge, log = TRUE)
}

#' Build Lib Size Qc Table
#'
#' Executes buildLibSizeQcTable.
#'
#' @param counts Argument for buildLibSizeQcTable.
#' @param meta Argument for buildLibSizeQcTable.
#'
#' @return Return value produced by buildLibSizeQcTable.
buildLibSizeQcTable <- function(counts, meta) {
  if (is.null(counts) || !is.data.frame(counts) || ncol(counts) == 0) {
    return(NULL)
  }
  numeric_cols <- vapply(counts, function(x) {
    vals <- suppressWarnings(as.numeric(as.character(x)))
    all(!is.na(vals))
  }, logical(1))
  if (!any(numeric_cols)) {
    return(NULL)
  }
  counts_num <- as.data.frame(lapply(counts[, numeric_cols, drop = FALSE], function(x) {
    as.numeric(as.character(x))
  }), check.names = FALSE)
  ids <- colnames(counts_num)
  meta_ids <- qcMetaColumn(meta, "ID")
  meta_groups <- qcMetaColumn(meta, "GROUP")
  groups <- if (!is.null(meta_ids) && !is.null(meta_groups)) {
    meta_groups[match(ids, meta_ids)]
  } else {
    rep(NA_character_, length(ids))
  }
  data.frame(
    ID = ids,
    GROUP = groups,
    lib.Size = unname(colSums(counts_num)),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

#' Build Rle Qc Table
#'
#' Executes buildRleQcTable.
#'
#' @param logCPM Argument for buildRleQcTable.
#' @param meta Argument for buildRleQcTable.
#'
#' @return Return value produced by buildRleQcTable.
buildRleQcTable <- function(logCPM, meta) {
  if (is.null(logCPM) || nrow(logCPM) == 0 || ncol(logCPM) == 0) {
    return(NULL)
  }
  dat_raw <- as.data.frame(logCPM, check.names = FALSE)
  dat <- as.data.frame(lapply(dat_raw, function(x) {
    suppressWarnings(as.numeric(as.character(x)))
  }), check.names = FALSE)
  rownames(dat) <- rownames(dat_raw)
  row_median <- apply(dat, 1, function(x) {
    vals <- x[is.finite(x)]
    if (length(vals) == 0) NA_real_ else stats::median(vals)
  })
  keep <- is.finite(row_median)
  if (!any(keep)) {
    return(NULL)
  }
  adj <- sweep(dat[keep, , drop = FALSE], 1, row_median[keep], "-")
  ids <- colnames(adj)
  meta_ids <- qcMetaColumn(meta, "ID")
  meta_groups <- qcMetaColumn(meta, "GROUP")
  groups <- if (!is.null(meta_ids) && !is.null(meta_groups)) {
    meta_groups[match(ids, meta_ids)]
  } else {
    rep(NA_character_, length(ids))
  }
  stats <- lapply(adj, function(x) {
    vals <- x[is.finite(x)]
    if (length(vals) == 0) {
      return(rep(NA_real_, 5))
    }
    as.numeric(stats::quantile(vals,
      probs = c(0, 0.25, 0.5, 0.75, 1),
      na.rm = TRUE, names = FALSE
    ))
  })
  stats <- do.call(rbind, stats)
  colnames(stats) <- c("min", "lower", "median", "upper", "max")
  data.frame(
    ID = ids,
    GROUP = groups,
    stats,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

#' Save Standard Qc Bundle
#'
#' Executes saveStandardQcBundle.
#'
#' @param counts Argument for saveStandardQcBundle.
#' @param logCPM Argument for saveStandardQcBundle.
#' @param meta Argument for saveStandardQcBundle.
#' @param outPrefix Argument for saveStandardQcBundle.
#'
#' @return Return value produced by saveStandardQcBundle.
saveStandardQcBundle <- function(counts, logCPM, meta, outPrefix) {
  qcPrefix <- qcOutputPrefix(outPrefix)
  if (is.null(qcPrefix)) {
    return(invisible(NULL))
  }

  tryCatch(
    {
      PlotLibSize(
        counts = counts, meta = meta,
        outFile = paste0(qcPrefix, "_QC_libSize.pdf")
      )
    },
    error = function(e) {
      cat("\n[QC]: Could not save library size plot:", conditionMessage(e), "\n")
    }
  )
  tryCatch(
    {
      PlotRLE(
        logCPM = logCPM, meta = meta,
        outFile = paste0(qcPrefix, "_QC_RLEplot.pdf")
      )
    },
    error = function(e) {
      cat("\n[QC]: Could not save RLE plot:", conditionMessage(e), "\n")
    }
  )
  tryCatch(
    {
      PCA2d(
        dat = logCPM, meta = meta, scale = TRUE, topn = 3000,
        outFile = paste0(qcPrefix, "_QC_PCA_TMM.pdf"), label = FALSE
      )
    },
    error = function(e) {
      cat("\n[QC]: Could not save PCA plot:", conditionMessage(e), "\n")
    }
  )

  tryCatch(
    {
      qcWriteTable(
        buildLibSizeQcTable(counts, meta),
        paste0(qcPrefix, "_QC_libSize.tsv")
      )
    },
    error = function(e) {
      cat("\n[QC]: Could not save library size table:", conditionMessage(e), "\n")
    }
  )
  tryCatch(
    {
      qcWriteTable(
        buildRleQcTable(logCPM, meta),
        paste0(qcPrefix, "_QC_RLE_summary.tsv")
      )
    },
    error = function(e) {
      cat("\n[QC]: Could not save RLE summary table:", conditionMessage(e), "\n")
    }
  )

  invisible(dirname(qcPrefix))
}

#' Save Ercc Qc Bundle
#'
#' Executes saveErccQcBundle.
#'
#' @param stat Argument for saveErccQcBundle.
#' @param cv Argument for saveErccQcBundle.
#' @param shift Argument for saveErccQcBundle.
#' @param tmmLogCPM Argument for saveErccQcBundle.
#' @param erccLogCPM Argument for saveErccQcBundle.
#' @param meta Argument for saveErccQcBundle.
#' @param outPrefix Argument for saveErccQcBundle.
#'
#' @return Return value produced by saveErccQcBundle.
saveErccQcBundle <- function(stat, cv = NULL, shift = NULL,
                             tmmLogCPM = NULL, erccLogCPM = NULL,
                             meta = NULL, outPrefix) {
  qcPrefix <- qcOutputPrefix(outPrefix)
  if (is.null(qcPrefix) || is.null(stat) || !is.data.frame(stat) || nrow(stat) == 0) {
    return(invisible(NULL))
  }

  grp_cols <- themeGroupColorMap(stat$GROUP)
  pt_col <- getOption("shinyRNAseq.theme.colors", list())$danger
  if (is.null(pt_col) || !nzchar(pt_col)) pt_col <- "#C71C22"

  dat <- stat
  dat$eff_libSize_gene <- dat$gene_libSize * dat$nf_libSize / 1e6
  dat$eff_libSize_ercc <- dat$gene_libSize * dat$nf_ERCC / 1e6

  p_gene <- ggplot(dat, aes(x = GROUP, y = eff_libSize_gene, fill = GROUP)) +
    geom_boxplot(width = 0.5, outlier.shape = NA, color = "black") +
    geom_jitter(width = 0.15, size = 2, alpha = 0.7, color = pt_col) +
    scale_fill_manual(values = grp_cols, drop = FALSE) +
    theme_classic(base_size = 12) +
    labs(
      y = "Effective Library Size (millions)", x = NULL,
      title = "TMM Normalization"
    ) +
    theme(
      axis.title = element_text(size = 12),
      axis.text.x = element_text(size = 10, angle = 90, hjust = 1),
      axis.text.y = element_text(size = 10),
      plot.title = element_text(size = 12),
      legend.position = "none"
    )

  p_ercc <- ggplot(dat, aes(x = GROUP, y = eff_libSize_ercc, fill = GROUP)) +
    geom_boxplot(width = 0.5, outlier.shape = NA, color = "black") +
    geom_jitter(width = 0.15, size = 2, alpha = 0.7, color = pt_col) +
    scale_fill_manual(values = grp_cols, drop = FALSE) +
    theme_classic(base_size = 12) +
    labs(
      y = "Effective Library Size (millions)", x = NULL,
      title = "ERCC Normalisation"
    ) +
    theme(
      axis.title = element_text(size = 12),
      axis.text.x = element_text(size = 10, angle = 90, hjust = 1),
      axis.text.y = element_text(size = 10),
      plot.title = element_text(size = 12),
      legend.position = "none"
    )

  p_spike <- ggplot(dat, aes(x = GROUP, y = pct_ERCC, fill = GROUP)) +
    geom_boxplot(width = 0.5, outlier.shape = NA, color = "black") +
    geom_jitter(width = 0.15, size = 2, alpha = 0.7, color = pt_col) +
    scale_fill_manual(values = grp_cols, drop = FALSE) +
    theme_classic(base_size = 12) +
    labs(
      y = "Spike-in Level (ERCC_libSize / gene_libSize x 100)", x = NULL,
      title = "ERCC Spike-in Ratio by Group"
    ) +
    theme(
      axis.title = element_text(size = 12),
      axis.text.x = element_text(size = 10, angle = 90, hjust = 1),
      axis.text.y = element_text(size = 10),
      plot.title = element_text(size = 12),
      legend.position = "none"
    )

  tryCatch(
    {
      ggsave(
        filename = paste0(qcPrefix, "_ERCC_QC_libsize_TMM.pdf"),
        plot = p_gene, width = 7, height = 6, units = "in"
      )
      ggsave(
        filename = paste0(qcPrefix, "_ERCC_QC_libsize_ERCC.pdf"),
        plot = p_ercc, width = 7, height = 6, units = "in"
      )
      ggsave(
        filename = paste0(qcPrefix, "_ERCC_QC_spikein_boxplot.pdf"),
        plot = p_spike, width = 9.5, height = 6.5, units = "in"
      )
    },
    error = function(e) {
      cat("\n[QC]: Could not save ERCC QC plots:", conditionMessage(e), "\n")
    }
  )

  if (!is.null(tmmLogCPM) && !is.null(meta)) {
    tryCatch(
      {
        PCA2d(
          dat = tmmLogCPM, meta = meta, scale = TRUE, topn = 3000,
          outFile = paste0(qcPrefix, "_ERCC_QC_PCA_TMM.pdf"), label = FALSE
        )
      },
      error = function(e) {
        cat("\n[QC]: Could not save ERCC QC TMM PCA:", conditionMessage(e), "\n")
      }
    )
  }
  if (!is.null(erccLogCPM) && !is.null(meta)) {
    tryCatch(
      {
        PCA2d(
          dat = erccLogCPM, meta = meta, scale = TRUE, topn = 3000,
          outFile = paste0(qcPrefix, "_ERCC_QC_PCA_ERCC.pdf"), label = FALSE
        )
      },
      error = function(e) {
        cat("\n[QC]: Could not save ERCC QC ERCC PCA:", conditionMessage(e), "\n")
      }
    )
  }

  tryCatch(
    {
      qcWriteTable(stat, paste0(qcPrefix, "_ERCC_QC_stat.tsv"))
    },
    error = function(e) {
      cat("\n[QC]: Could not save ERCC QC stat table:", conditionMessage(e), "\n")
    }
  )
  if (!is.null(cv)) {
    tryCatch(
      {
        qcWriteTable(cv, paste0(qcPrefix, "_ERCC_QC_group_CV.tsv"))
      },
      error = function(e) {
        cat("\n[QC]: Could not save ERCC QC CV table:", conditionMessage(e), "\n")
      }
    )
  }
  if (!is.null(shift)) {
    tryCatch(
      {
        shift_df <- as.data.frame(shift, check.names = FALSE, stringsAsFactors = FALSE)
        qcWriteTable(shift_df, paste0(qcPrefix, "_ERCC_QC_shift.tsv"))
      },
      error = function(e) {
        cat("\n[QC]: Could not save ERCC QC shift table:", conditionMessage(e), "\n")
      }
    )
  }

  invisible(dirname(qcPrefix))
}

######################################
#' Create a volcano plot from differential results
#'
#' Displays log fold change versus significance and optionally highlights top
#' genes and user-specified genes of interest.
#'
#' @param diff_table Differential expression table.
#' @param cutoff_logFC Absolute log2 fold-change threshold.
#' @param cutoff_FDR FDR threshold used for reference lines.
#' @param cols Optional named color vector.
#' @param topnDEGs Optional list containing top up/down genes.
#' @param GOI Optional vector of genes of interest.
#' @param outFile Optional path to save the figure.
#' @param strLogFC Pattern for log fold change column.
#' @param strPval Pattern for p-value column.
#' @param strAveExpr Pattern for average expression columns.
#' @param strPadj Pattern for adjusted p-value column.
#'
#' @return `ggplot` object.
PlotVolcano <- function(diff_table, cutoff_logFC = 1, cutoff_FDR = 0.05, cols = NA, topnDEGs = NULL, GOI = NULL,
                        outFile = NA, strLogFC = "log2FC", strPval = "P.Value", strAveExpr = "AveExpr", strPadj = "adj.P.Val") {
  # cols: a vector of 5 elements like
  if (is.na(cols)) { # default color setting
    cols <- c("#F9ABAB", "#ADADF9", "#F92A2A", "#2727FA", "purple")
    names(cols) <- c("fill_up", "fill_down", "color_up", "color_down", "GOI")
  }

  # GOI: genes of user's interest
  #-----------------------------------------------------

  #-----------------------------------------------------
  if (!is.data.frame(diff_table)) {
    cat("\n** Error in PlotAveExpr function: parameter [diff_table] must be a data.frame! **\n\n")
    stop("Exit...")
  }
  ind_gene <- grep("^gene$", colnames(diff_table), ignore.case = T)[1]
  if (!is.na(ind_gene) & length(ind_gene) > 0) {
    rownames(diff_table) <- diff_table[, ind_gene]
    diff_table <- diff_table[, -ind_gene]
  } else {
    cat("\n Warning in VolcanoPlot function:  no gene column found in [diff_table]! \n\n")
  }
  # dat <- apply(diff_table, 2,  function(x) as.numeric(as.character(x)))
  dat <- diff_table
  rownames(dat) <- rownames(diff_table)
  genes <- rownames(diff_table)

  GOI <- intersect(GOI, genes)

  inds <- SearchColNames(dat, strLogFC = strLogFC, strPval = strPval, strAveExpr = strAveExpr, strPadj = strPadj, caseInSensitive = T, wholeWordOnly = F)
  inds_logFC <- inds$iLogFC
  inds_Pval <- inds$iPval[1]
  inds_Padj <- inds$iPadj[1]
  inds_AveExpr <- inds$iAveExpr

  x <- plotNumeric(dat[, inds_logFC[1]], field = "log2FC", context = "PlotVolcano")
  pval <- plotNumeric(dat[, inds_Pval], field = "P.Value", context = "PlotVolcano")
  pval[is.finite(pval) & pval <= 0] <- .Machine$double.xmin
  y <- -log10(pval)
  FDR <- plotNumeric(dat[, inds_Padj[1]], field = "FDR", context = "PlotVolcano")
  Exp <- gsub(paste0(".", strAveExpr), "", colnames(dat)[inds_AveExpr[2]])
  Ctl <- gsub(paste0(".", strAveExpr), "", colnames(dat)[inds_AveExpr[3]])


  isUp <- rep(FALSE, nrow(dat))
  isDown <- rep(FALSE, nrow(dat))
  isTopGenes <- rep(FALSE, nrow(dat))
  if (is.list(topnDEGs)) {
    if (all(c("topUpGenes", "topDownGenes") %in% names(topnDEGs))) {
      if (length(topnDEGs$topUpGenes) > 0) {
        isUp <- genes %in% topnDEGs$topUpGenes
      }
      if (length(topnDEGs$topDownGenes) > 0) {
        isDown <- genes %in% topnDEGs$topDownGenes
      }
      isTopGenes <- isUp | isDown
    }
  }
  if (length(GOI) > 0) {
    isGOI <- genes %in% GOI
    isTopGenes <- isTopGenes | isGOI # add genes of interest to the gene list to be labeled
  } else {
    isGOI <- rep(FALSE, nrow(dat))
  }

  dat1 <- data.frame(x = x, y = y, gene = genes, FDR = FDR, pval = pval, upText = isUp, downText = isDown, selText = isTopGenes, goiText = isGOI)
  dat1 <- plotFiniteRows(dat1, fields = c("x", "y"), context = "PlotVolcano")

  labelx <- "log2FC"
  labely <- "-log10(P.Value)"
  scalex <- plotRange(dat1$x, include = c(-cutoff_logFC, cutoff_logFC), symmetric = TRUE)
  fdr_y <- NA_real_
  fdr_pass <- which(!is.na(dat1$FDR) & dat1$FDR <= cutoff_FDR)
  if (length(fdr_pass) > 0) {
    FDR_cutoff_y <- max(dat1$pval[fdr_pass], na.rm = TRUE)
    if (is.finite(FDR_cutoff_y) && FDR_cutoff_y > 0) fdr_y <- -log10(FDR_cutoff_y)
  }
  scaley <- plotRange(dat1$y, include = fdr_y, lower_zero = TRUE)
  myTitle <- paste0(Exp, " vs ", Ctl)
  pg <- ggplot(data = dat1, aes(x = x, y = y, text = paste(gene, "\nlog2FC:", x, "\nP.Value:", pval, "\nFDR:", FDR))) +
    geom_point(size = 1.2, shape = 21, fill = NA, col = "grey") +
    theme_bw() +
    theme_classic(base_size = 12) + # scale_color_identity()+
    theme(axis.text = element_text(size = 12)) +
    theme(panel.border = element_rect(colour = "grey20", fill = NA, size = 1)) +
    theme(plot.title = element_text(size = 8)) +
    theme(aspect.ratio = 1) +
    labs(y = labely, x = labelx) +
    coord_cartesian(xlim = scalex, ylim = scaley, clip = "off") +
    ggtitle(myTitle)
  pg1 <- pg + geom_vline(xintercept = c(-cutoff_logFC, cutoff_logFC), colour = c("blue", "brown"), linetype = "dashed", size = 0.2)
  if (sum(dat1$selText) > 0 && is.finite(fdr_y)) {
    pg1 <- pg1 + geom_hline(yintercept = fdr_y, colour = "grey30", linetype = "dashed", size = 0.2)
  }
  pg2 <- pg1
  if (sum(dat1$upText) > 0) {
    pg2 <- pg2 + geom_point(data = subset(dat1, upText == T), aes(x = x, y = y), shape = 21, fill = cols["fill_up"], col = cols["color_up"], size = 1.2)
  }
  if (sum(dat1$downText) > 0) {
    pg2 <- pg2 + geom_point(data = subset(dat1, downText == T), aes(x = x, y = y), shape = 21, fill = cols["fill_down"], col = cols["color_down"], size = 1.2)
  }
  if (sum(dat1$goiText) > 0) {
    pg2 <- pg2 + geom_point(data = subset(dat1, goiText == T), aes(x = x, y = y), shape = 21, fill = cols["GOI"], col = cols["GOI"], size = 1.2)
  }
  if (sum(dat1$goiText) > 0) { # sum(dat1$selText) >0 |
    pg3 <- pg2 + geom_text(data = subset(dat1, goiText == T), aes(label = gene), check_overlap = TRUE, size = 3, hjust = 0, nudge_x = 0.5)
  } else {
    pg3 <- pg2
  }
  if (!is.na(outFile)) {
    if (sum(dat1$selText) > 0 | sum(dat1$goiText) > 0) {
      pg2 <- pg2 + geom_text_repel(data = subset(dat1, selText == T | goiText == T), aes(label = gene), size = 3)
    }
    ggsave(file = outFile, pg2, width = 7, height = 7, units = "in")
    cat("\n\t", outFile, "[Volcano plot saved]\n")
  }
  return(pg3)
}
##########################################
##########################################
#' Plot average expression by comparison group
#'
#' Creates a paired average-expression scatter plot and can annotate top genes
#' or genes of interest.
#'
#' @param diff_table Differential expression table.
#' @param cols Optional named color vector.
#' @param topnDEGs Optional list containing top up/down genes.
#' @param GOI Optional vector of genes of interest.
#' @param outFile Optional path to save the figure.
#' @param strLogFC Pattern for log fold change column.
#' @param strPval Pattern for p-value column.
#' @param strAveExpr Pattern for average expression columns.
#' @param strPadj Pattern for adjusted p-value column.
#'
#' @return `ggplot` object.
PlotAveExpr <- function(diff_table, cols = NA, topnDEGs = NULL, GOI = NULL, outFile = NA,
                        strLogFC = "log2FC", strPval = "P.Value", strAveExpr = "AveExpr", strPadj = "adj.P.Val") {
  # cols: a vector of 5 elements like
  #-----------------------------------------------------
  if (is.na(cols)) { # default color setting
    cols <- c("#F9ABAB", "#ADADF9", "#F92A2A", "#2727FA", "purple")
    names(cols) <- c("fill_up", "fill_down", "color_up", "color_down", "GOI")
  }
  # GOI: genes of user's interest

  #-----------------------------------------------------
  if (!is.data.frame(diff_table)) {
    cat("\n** Error in PlotAveExpr function: parameter [diff_table] must be a data.frame! **\n\n")
    stop("Exit...")
  }
  ind_gene <- grep("^gene$", colnames(diff_table), ignore.case = T)[1]
  if (!is.na(ind_gene) & length(ind_gene) > 0) {
    rownames(diff_table) <- diff_table[, ind_gene]
    diff_table <- diff_table[, -ind_gene]
  } else {
    cat("\n Warning in PlotAveExpr function:  no gene column found in [diff_table]! \n\n")
  }

  # dat <- apply(diff_table, 2,  function(x) as.numeric(as.character(x)))
  dat <- diff_table
  rownames(dat) <- rownames(diff_table)

  inds <- SearchColNames(dat, strLogFC = strLogFC, strPval = strPval, strAveExpr = strAveExpr, strPadj = strPadj, caseInSensitive = T, wholeWordOnly = F)
  inds_logFC <- inds$iLogFC[1]
  inds_Pval <- inds$iPval[1]
  inds_Padj <- inds$iPadj[1]
  inds_AveExpr <- inds$iAveExpr
  genes <- rownames(dat)
  GOI <- intersect(GOI, genes)
  x <- plotNumeric(dat[, inds_AveExpr[2]], field = colnames(dat)[inds_AveExpr[2]], context = "PlotAveExpr")
  y <- plotNumeric(dat[, inds_AveExpr[3]], field = colnames(dat)[inds_AveExpr[3]], context = "PlotAveExpr")
  FDR <- plotNumeric(dat[, inds_Padj[1]], field = "FDR", context = "PlotAveExpr")
  pval <- plotNumeric(dat[, inds_Pval], field = "P.Value", context = "PlotAveExpr")

  AveExpr <- plotNumeric(dat[, inds_AveExpr[1]], field = colnames(dat)[inds_AveExpr[1]], context = "PlotAveExpr")
  Exp <- gsub(paste0(".", strAveExpr), "", colnames(dat)[inds_AveExpr[2]])
  Ctl <- gsub(paste0(".", strAveExpr), "", colnames(dat)[inds_AveExpr[3]])

  logFC <- plotNumeric(dat[, inds_logFC[1]], field = "log2FC", context = "PlotAveExpr")

  isUp <- rep(FALSE, nrow(dat))
  isDown <- rep(FALSE, nrow(dat))
  isTopGenes <- rep(FALSE, nrow(dat))
  if (is.list(topnDEGs)) {
    if (all(c("topUpGenes", "topDownGenes") %in% names(topnDEGs))) {
      if (length(topnDEGs$topUpGenes) > 0) {
        isUp <- genes %in% topnDEGs$topUpGenes
      }
      if (length(topnDEGs$topDownGenes) > 0) {
        isDown <- genes %in% topnDEGs$topDownGenes
      }
      isTopGenes <- isUp | isDown
    }
  }

  if (length(GOI) > 0) {
    isGOI <- genes %in% GOI
    isTopGenes <- isTopGenes | isGOI # add genes of interest to the gene list to be labeled
  } else {
    isGOI <- rep(FALSE, nrow(dat))
  }

  dat1 <- data.frame(x = x, y = y, gene = genes, FDR = FDR, pval = pval, upText = isUp, downText = isDown, selText = isTopGenes, goiText = isGOI)
  dat1 <- plotFiniteRows(dat1, fields = c("x", "y"), context = "PlotAveExpr")
  scale_rng <- plotRange(c(dat1$x, dat1$y), pad = 0.01)
  r <- if (nrow(dat1) >= 2 && stats::sd(dat1$x) > 0 && stats::sd(dat1$y) > 0) {
    round(cor(dat1$x, dat1$y, method = "pearson"), 5)
  } else {
    NA_real_
  }
  subTitle <- paste0("pearson r=", r)
  myTitle <- paste0("log2AveExpr ", Exp, " vs ", Ctl, "\n", subTitle)
  cat(subTitle)
  pg <- ggplot(data = dat1, aes(x = x, y = y, text = paste(gene, "\nx: ", x, "\ny: ", y, "\nP.Value:", pval, "\nFDR:", FDR))) +
    geom_point(size = 1.2, shape = 21, fill = NA, col = "grey") +
    theme_bw() +
    theme_classic(base_size = 12) + # scale_color_identity()+
    theme(axis.text = element_text(size = 12)) +
    theme(panel.border = element_rect(colour = "grey20", fill = NA, size = 1)) +
    theme(plot.title = element_text(size = 8), plot.subtitle = element_text(size = 7)) +
    theme(aspect.ratio = 1) +
    labs(y = Ctl, x = Exp) +
    coord_fixed(ratio = 1, xlim = scale_rng, ylim = scale_rng, clip = "off") +
    ggtitle(label = myTitle)
  pg1 <- pg + geom_abline(intercept = 0, slope = 1, colour = "grey10", linetype = "dashed", size = 0.5)
  pg2 <- pg1
  if (sum(dat1$upText) > 0) {
    pg2 <- pg2 + geom_point(data = subset(dat1, upText == T), aes(x = x, y = y), shape = 21, fill = cols["fill_up"], col = cols["color_up"], size = 1.2)
  }
  if (sum(dat1$downText) > 0) {
    pg2 <- pg2 + geom_point(data = subset(dat1, downText == T), aes(x = x, y = y), shape = 21, fill = cols["fill_down"], col = cols["color_down"], size = 1.2)
  }
  if (sum(dat1$goiText) > 0) {
    pg2 <- pg2 + geom_point(data = subset(dat1, goiText == T), aes(x = x, y = y), shape = 21, fill = cols["GOI"], col = cols["GOI"], size = 1.2)
  }
  if (sum(dat1$goiText) > 0) { # sum(dat1$selText) >0 |
    pg3 <- pg2 + geom_text(data = subset(dat1, goiText == T), aes(label = gene), check_overlap = TRUE, size = 3, hjust = 0, nudge_x = 0.5)
  } else {
    pg3 <- pg2
  }
  if (!is.na(outFile)) {
    if (sum(dat1$selText) > 0 | sum(dat1$goiText) > 0) {
      pg2 <- pg2 + geom_text_repel(data = subset(dat1, selText == T | goiText == T), aes(label = gene), size = 3)
    }
    ggsave(file = outFile, pg2, width = 7, height = 7, units = "in")
    cat("\n\t", outFile, "[AveExpr plot saved]\n")
  }
  return(pg3)
}

######################################################
#' Create an MA plot from differential results
#'
#' Plots average expression against log fold change and can highlight selected
#' genes for downstream review.
#'
#' @param diff_table Differential expression table.
#' @param cutoff_logFC Absolute log2 fold-change threshold.
#' @param cols Optional named color vector.
#' @param topnDEGs Optional list containing top up/down genes.
#' @param GOI Optional vector of genes of interest.
#' @param outFile Optional path to save the figure.
#' @param strLogFC Pattern for log fold change column.
#' @param strPval Pattern for p-value column.
#' @param strPadj Pattern for adjusted p-value column.
#' @param strAveExpr Pattern for average expression columns.
#'
#' @return `ggplot` object.
PlotMA <- function(diff_table, cutoff_logFC = 1, cols = NA, topnDEGs = NULL, GOI = NULL,
                   outFile = NA, strLogFC = "log2FC", strPval = "P.Value", strPadj = "adj.P.Val", strAveExpr = "AveExpr") {
  # cols: a vector of 5 elements like
  if (is.na(cols)) { # default color setting
    cols <- c("#F9ABAB", "#ADADF9", "#F92A2A", "#2727FA", "purple")
    names(cols) <- c("fill_up", "fill_down", "color_up", "color_down", "GOI")
  }

  # GOI: genes of user's interest
  #-----------------------------------------------------

  #-----------------------------------------------------
  if (!is.data.frame(diff_table)) {
    cat("\n** Error in PlotMA function: parameter [diff_table] must be a data.frame! **\n\n")
    stop("Exit...")
  }
  ind_gene <- grep("^gene$", colnames(diff_table), ignore.case = T)[1]
  if (!is.na(ind_gene) & length(ind_gene) > 0) {
    rownames(diff_table) <- diff_table[, ind_gene]
    diff_table <- diff_table[, -ind_gene]
  } else {
    cat("\n Warning in PlotMA function:  no gene column found in [diff_table]! \n\n")
  }
  dat <- diff_table
  rownames(dat) <- rownames(diff_table)
  genes <- rownames(diff_table)
  GOI <- intersect(GOI, genes)
  inds <- SearchColNames(dat, strLogFC = strLogFC, strPval = strPval, strAveExpr = strAveExpr, strPadj = strPadj, caseInSensitive = T, wholeWordOnly = F)
  inds_logFC <- inds$iLogFC
  inds_Pval <- inds$iPval
  inds_Padj <- inds$iPadj
  inds_AveExpr <- inds$iAveExpr

  x <- plotNumeric(dat[, inds_AveExpr[1]], field = colnames(dat)[inds_AveExpr[1]], context = "PlotMA")
  y <- plotNumeric(dat[, inds_logFC[1]], field = "log2FC", context = "PlotMA")
  FDR <- plotNumeric(dat[, inds_Padj[1]], field = "FDR", context = "PlotMA")
  pval <- plotNumeric(dat[, inds_Pval[1]], field = "P.Value", context = "PlotMA")
  Exp <- gsub(paste0(".", strAveExpr), "", colnames(dat)[inds_AveExpr[2]])
  Ctl <- gsub(paste0(".", strAveExpr), "", colnames(dat)[inds_AveExpr[3]])

  isUp <- rep(FALSE, nrow(dat))
  isDown <- rep(FALSE, nrow(dat))
  isTopGenes <- rep(FALSE, nrow(dat))
  if (is.list(topnDEGs)) {
    if (all(c("topUpGenes", "topDownGenes") %in% names(topnDEGs))) {
      if (length(topnDEGs$topUpGenes) > 0) {
        isUp <- genes %in% topnDEGs$topUpGenes
      }
      if (length(topnDEGs$topDownGenes) > 0) {
        isDown <- genes %in% topnDEGs$topDownGenes
      }
      isTopGenes <- isUp | isDown
    }
  }
  if (length(GOI) > 0) {
    isGOI <- genes %in% GOI
    isTopGenes <- isTopGenes | isGOI # add genes of interest to the gene list to be labeled
  } else {
    isGOI <- rep(FALSE, nrow(dat))
  }

  # print(cat("\nx:",length(x),",y:",length(y),", FDR:",length(FDR),  ", pval=",length(pval),", genes:",length(genes),", isUp:",length(isUp), ",down:",length(isDown)," selText:", length(isTopGenes),"\n"))

  dat1 <- data.frame(x = x, y = y, gene = genes, FDR = FDR, pval = pval, upText = isUp, downText = isDown, selText = isTopGenes, goiText = isGOI)
  dat1 <- plotFiniteRows(dat1, fields = c("x", "y"), context = "PlotMA")
  labelx <- "log2AveExpr"
  labely <- "log2FC"
  x_rng <- plotRange(dat1$x)
  # Extra headroom on the right so GOI labels (nudge_x = 0.5) aren't clipped by plotly.
  x_rng[2] <- x_rng[2] + max(0.5, diff(x_rng) * 0.05)
  y_rng <- plotRange(dat1$y, include = c(0, -cutoff_logFC, cutoff_logFC))
  myTitle <- paste0(Exp, " vs ", Ctl)
  pg <- ggplot(data = dat1, aes(x = x, y = y, text = paste(gene, "\nlog2AveExpr:", x, "\nlog2FC:", y, "\nP.Value:", pval, "\nFDR:", FDR))) +
    geom_point(size = 1.2, shape = 21, fill = NA, col = "grey") +
    theme_bw() +
    theme_classic(base_size = 12) + # scale_color_identity()+
    theme(axis.text = element_text(size = 12)) +
    theme(panel.border = element_rect(colour = "grey20", fill = NA, size = 1)) +
    theme(plot.title = element_text(size = 8)) +
    labs(y = labely, x = labelx) +
    coord_cartesian(xlim = x_rng, ylim = y_rng, clip = "off") +
    ggtitle(myTitle)
  pg1 <- pg + geom_hline(yintercept = 0, colour = "grey30", linetype = "dashed", size = 0.5) +
    geom_hline(yintercept = c(-cutoff_logFC, cutoff_logFC), colour = c("blue", "brown"), linetype = "dashed", size = 0.2)
  pg2 <- pg1
  if (sum(dat1$upText) > 0) {
    pg2 <- pg2 + geom_point(data = subset(dat1, upText == T), aes(x = x, y = y), shape = 21, fill = cols["fill_up"], col = cols["color_up"], size = 1.2)
  }
  if (sum(dat1$downText) > 0) {
    pg2 <- pg2 + geom_point(data = subset(dat1, downText == T), aes(x = x, y = y), shape = 21, fill = cols["fill_down"], col = cols["color_down"], size = 1.2)
  }
  if (sum(dat1$goiText) > 0) {
    pg2 <- pg2 + geom_point(data = subset(dat1, goiText == T), aes(x = x, y = y), shape = 21, fill = cols["GOI"], col = cols["GOI"], size = 1.2)
  }
  if (sum(dat1$goiText) > 0) { # sum(dat1$selText) >0 |
    pg3 <- pg2 + geom_text(data = subset(dat1, goiText == T), aes(label = gene), check_overlap = TRUE, size = 3, hjust = 0, nudge_x = 0.5)
  } else {
    pg3 <- pg2
  }

  if (!is.na(outFile)) {
    if (sum(dat1$selText) > 0) {
      pg2 <- pg2 + geom_text_repel(data = subset(dat1, selText == T | goiText == T), aes(label = gene), size = 3)
    }
    ggsave(file = outFile, pg2, width = 7, height = 7, units = "in")
    cat("\n\t", outFile, "[MAplot saved]\n")
  }
  return(pg3)
}


############################################
#' Plot one gene across metadata groups
#'
#' Extracts a single gene from a differential result matrix and draws a grouped
#' boxplot with optional statistics in the title.
#'
#' @param diff_table Differential expression table or expression matrix.
#' @param meta_used Metadata table used for grouping.
#' @param gene Target gene symbol.
#' @param outFile Optional path to save the figure.
#' @param showStat Logical flag to include DE stats in title.
#'
#' @return `ggplot` object or `NULL` when gene is not found.
doBoxplot <- function(diff_table, meta_used, gene = "Gapdh", outFile = NA, showStat = TRUE) {
  if (is.null(diff_table) || !is.data.frame(diff_table) || nrow(diff_table) == 0) {
    return(NULL)
  }
  if (is.null(meta_used) || !is.data.frame(meta_used) || nrow(meta_used) == 0) {
    return(NULL)
  }
  if ("gene" %in% colnames(diff_table)) {
    rownames(diff_table) <- diff_table$gene
  }

  dat <- diff_table
  # validate input
  tarow_grpets <- meta_used
  colnames(tarow_grpets) <- toupper(colnames(tarow_grpets))
  if (!"ID" %in% colnames(tarow_grpets) | !"GROUP" %in% colnames(tarow_grpets)) {
    cat("\nInvalid meta file. ID or/and Group column(s) not found.\n")
    stop("exit.")
  }
  keptSamples <- intersect(tarow_grpets$ID, colnames(dat))
  if (length(keptSamples) == 0) {
    cat("\nID in metadata table doesn't match colnames in input matrix.\n")
    stop(paste0("Invalid metadata file", basename(opt$metaData), "\n"))
  }
  rownames(tarow_grpets) <- tarow_grpets$ID
  meta <- tarow_grpets[keptSamples, , drop = FALSE]
  dat <- dat[, meta$ID, drop = FALSE]
  ind0 <- match(tolower(gene), tolower(rownames(diff_table)))[1]
  ind <- match(tolower(gene), tolower(rownames(dat)))[1]
  if (length(ind) == 0) {
    return(NULL)
  }
  logFC <- diff_table[ind0, grep("log2FC", colnames(diff_table))[1]]
  pval <- diff_table[ind0, grep("P.Value", colnames(diff_table))[1]]
  if (showStat) {
    myTitle <- paste0(gene, "\nlog2FC=", logFC, "; P.Value=", pval, "\n")
  } else {
    myTitle <- gene
  }
  titleSize <- if (showStat) 9 else 12
  # Use unname() + as.numeric() to strip element names from the extracted row so
  # data.frame() does not create conflicting/duplicate column names (which causes
  # ggplotly's internal merge.data.frame to fail with 'by' must specify uniquely
  # valid columns).
  logCPM_vals <- unname(as.numeric(unlist(dat[ind, , drop = FALSE])))
  meta_clean <- meta
  rownames(meta_clean) <- NULL
  if ("NEWNAME" %in% colnames(meta_clean)) {
    DF <- data.frame(
      logCPM = logCPM_vals, meta_clean,
      INFO = paste(meta_clean$ID, meta_clean$NEWNAME, meta_clean$GROUP, sep = "\n"),
      stringsAsFactors = FALSE
    )
  } else {
    DF <- data.frame(
      logCPM = logCPM_vals, meta_clean,
      INFO = paste(meta_clean$ID, meta_clean$GROUP, sep = "\n"),
      stringsAsFactors = FALSE
    )
  }
  keep_box <- is.finite(DF$logCPM)
  if (any(!keep_box)) {
    cat("\nWarning[doBoxplot]: excluding ", sum(!keep_box),
      " sample(s) with non-numeric/NA/Inf expression values.\n",
      sep = ""
    )
  }
  DF <- DF[keep_box, , drop = FALSE]
  if (nrow(DF) == 0) {
    return(NULL)
  }

  cmap <- themeGroupColorMap(DF$GROUP)
  DF$COLOR <- unname(cmap[as.character(DF$GROUP)])

  uniqCombs <- DF[, c("COLOR", "GROUP")]
  uniqCombs$comb <- paste(DF$COLOR, DF$GROUP, sep = "_")
  uniqCombs <- uniqCombs[!duplicated(uniqCombs$comb), c("COLOR", "GROUP")]
  GROUPS <- factor(DF$GROUP, levels = unique(DF$GROUP))

  bp <- ggplot(DF, aes(GROUP, logCPM, fill = GROUPS, text = INFO)) +
    stat_boxplot(geom = "errorbar", width = 0.2, color = "black") +
    geom_boxplot(outlier.shape = NA, color = "black", alpha = 0.5) +
    theme_bw() +
    theme_classic(base_size = 12) +
    scale_fill_manual(name = "GROUP", labels = uniqCombs$GROUP, values = uniqCombs$COLOR) +
    geom_jitter(
      shape = 21, color = "black", stroke = 0.35, size = 2,
      alpha = 0.85, position = position_jitter(width = 0.2, height = 0)
    ) +
    ggtitle(myTitle) +
    theme(
      legend.position = "right",
      plot.title = element_text(size = titleSize)
    )
  attr(bp, "boxplotData") <- list(
    DF = DF,
    title = myTitle,
    titleSize = titleSize,
    colors = setNames(uniqCombs$COLOR, uniqCombs$GROUP)
  )


  if (!is.na(outFile)) {
    ggsave(file = outFile, bp, width = 6, height = 6, units = "in")
    cat("\n\t", basename(outFile), "[saved]\n")
  }
  return(bp)
}

#' Create an interactive gene expression boxplot
#'
#' Converts the static boxplot data from [doBoxplot()] into Plotly traces with
#' group-colored fills, sample points, and black box outlines.
#'
#' @param diff_table Differential expression or logCPM table.
#' @param meta_used Metadata table for the samples being plotted.
#' @param gene Gene symbol or row name to plot.
#' @param outFile Optional output path passed to [doBoxplot()].
#' @param showStat Logical flag passed to [doBoxplot()] for title statistics.
#' @param width Optional Plotly widget width.
#' @param height Optional Plotly widget height.
#'
#' @return Plotly object, ggplotly fallback, or `NULL` if the gene is unavailable.
doBoxplotly <- function(diff_table, meta_used, gene = "Gapdh", outFile = NA,
                        showStat = TRUE, width = NULL, height = NULL) {
  bp <- doBoxplot(diff_table, meta_used,
    gene = gene, outFile = outFile,
    showStat = showStat
  )
  if (is.null(bp)) {
    return(NULL)
  }

  boxData <- attr(bp, "boxplotData")
  if (is.null(boxData) || is.null(boxData$DF)) {
    return(plotly::ggplotly(bp, width = width, height = height, tooltip = "text"))
  }

  DF <- boxData$DF
  DF$GROUP <- as.character(DF$GROUP)
  groups <- unique(DF$GROUP)
  colors <- boxData$colors
  fig <- plotly::plot_ly(width = width, height = height)

  for (i in seq_along(groups)) {
    grp <- groups[i]
    rows <- DF$GROUP == grp
    grpColor <- unname(colors[grp])
    if (is.na(grpColor) || length(grpColor) == 0) grpColor <- "#2FA4E7"
    fig <- plotly::add_boxplot(
      fig,
      x = rep(i, sum(rows)),
      y = DF$logCPM[rows],
      name = grp,
      text = DF$INFO[rows],
      boxpoints = "all",
      jitter = 0.35,
      pointpos = 0,
      marker = list(
        color = grpColor,
        opacity = 0.85,
        line = list(color = "black", width = 1)
      ),
      line = list(color = "black"),
      fillcolor = grpColor,
      opacity = 0.55,
      hovertemplate = "%{text}<br>logCPM=%{y:.2f}<extra></extra>"
    )
  }

  plotly::layout(
    fig,
    title = list(text = boxData$title, font = list(size = boxData$titleSize + 3)),
    xaxis = list(
      title = "GROUP", tickmode = "array",
      tickvals = seq_along(groups), ticktext = groups
    ),
    yaxis = list(title = "logCPM"),
    boxmode = "group",
    legend = list(
      orientation = "v", x = 1.02, y = 1,
      xanchor = "left", yanchor = "top",
      font = list(size = 13)
    ),
    margin = list(b = 70, r = 120),
    showlegend = TRUE
  )
}

################################
#' Generate random alphanumeric strings
#'
#' Returns one or more random identifiers of the requested length.
#'
#' @param n Number of random strings.
#' @param length Length of each string.
#'
#' @return Character vector of random strings.
RandomString <- function(n = 1, length = 12) {
  randomString <- c(1:n) # initialize vector
  for (i in 1:n)
  {
    randomString[i] <- paste(
      sample(c(0:9, letters, LETTERS),
        length,
        replace = TRUE
      ),
      collapse = ""
    )
  }
  return(randomString)
}

################################
#' Intersect vectors while preserving reference values
#'
#' Computes a case-aware intersection and returns matched elements using the
#' original values from the reference vector.
#'
#' @param query Query vector.
#' @param ref Reference vector.
#' @param ignore.case Logical flag for case-insensitive matching.
#'
#' @return Vector of matched values from `ref`.
intersect2 <- function(query = q, ref = r, ignore.case = T) {
  # submit a query vector, find matched elements from ref vector
  if (ignore.case) {
    kept <- intersect(toupper(query), toupper(ref))
    res <- ref[match(kept, toupper(ref))]
  } else {
    kept <- intersect(query, ref)
    res <- ref[match(kept, ref)]
  }
  return(res)
}

###################################
######################################################
#' Detect SLAM-seq count matrix columns
#'
#' Checks whether a count table contains paired total and Tc-read columns in the
#' expected slamdunk naming convention.
#'
#' @param counts Count table.
#' @param totalCountKeyword Suffix used for total count columns.
#' @param tcCountKeyword Suffix used for Tc read count columns.
#'
#' @return Logical flag indicating SLAM-seq column format detection.
IsSLAMseqCount <- function(counts, totalCountKeyword = "_ReadCount", tcCountKeyword = "_TcReadCount") {
  idx_total <- grep(paste0(totalCountKeyword, "$"), colnames(counts))
  idx_tc <- grep(paste0(tcCountKeyword, "$"), colnames(counts))
  if (length(idx_total) == 0 || length(idx_tc) == 0 || length(idx_total) != length(idx_tc)) {
    cat("\nInfo: invalid slamdunk format!\n")
    cat(length(idx_total), length(idx_tc), "\n")
    return(FALSE)
  } else {
    cat("\nInfo: valid slamdunk format!\n")
    return(TRUE)
  }
}

# =====================================================
#' Read RNA-seq or SLAM-seq count matrices
#'
#' Loads a count table, applies optional annotation filters, and returns either
#' standard RNA-seq counts or paired Tc/total count matrices for SLAM-seq data.
#'
#' @param countFile Path to the count matrix file.
#' @param filterFlag Logical flag to filter by biotype and annotation level.
#' @param fill Passed to `read.table`.
#' @param check.names Passed to `read.table`.
#' @param header Passed to `read.table`.
#' @param sep Field separator used in the count file.
#' @param quote Quote character used in the count file.
#' @param row.names Row names argument passed to `read.table`.
#' @param totalCountKeyword Suffix used for total count columns.
#' @param tcCountKeyword Suffix used for Tc read count columns.
#' @param aggregateId Logical flag to aggregate duplicated gene IDs.
#'
#' @return Invisible list with `counts`, `annotation`, `counts_total`, and `raw`.
ReadCountSLAM <- function(countFile, filterFlag = F, fill = TRUE, check.names = FALSE,
                          header = TRUE, sep = "\t", quote = "", row.names = NULL, totalCountKeyword = "_ReadCount", tcCountKeyword = "_TcReadCount", aggregateId = T) {
  counts0 <- read.table(countFile,
    fill = fill, check.names = FALSE,
    stringsAsFactors = F, row.names = row.names,
    header = header, sep = sep, quote = quote
  )
  # counts <- na.omit(counts)
  raw <- counts0
  geneColumns <- c("gene", "geneID", "geneSymbol")
  if (all(!geneColumns %in% tolower(colnames(counts0)))) {
    cat("\nError: missing required column [gene,  geneID or geneSymbol] in your count matrix file.\n")
    shinyalert(title = "Warning!", text = paste0("\n\nProbably the required column [gene, geneID or geneSymbol] is missing in your count matrix file."), type = "warning", confirmButtonCol = "#DD5600")
  }
  if (filterFlag == T) {
    if ("bioType" %in% colnames(counts0)) {
      counts0 <- counts0[counts0$bioType == "protein_coding", ]
    }
    if ("annotationLevel" %in% colnames(counts0)) {
      counts0 <- counts0[counts0$annotationLevel <= 3, ]
    }
  }
  kept <- !tolower(colnames(counts0)) %in% tolower(c("Chromosome", "Start", "End", "Strand", "Length", "bioType", "annotationLevel"))
  counts <- counts0[, kept]
  if (aggregateId) {
    if (sum(duplicated(counts$geneID))) {
      counts <- aggregate(x = counts[, !colnames(counts) %in% geneColumns], by = counts[, geneColumns], FUN = sum, na.rm = TRUE)
    }
  } else {
    counts$geneID <- make.names(counts$geneID, unique = T)
    counts$geneSymbol <- make.names(counts$geneSymbol, unique = T)
  }
  if ("geneSymbol" %in% colnames(counts)) {
    genes <- counts[, "geneSymbol"]
  } else if ("gene" %in% tolower(colnames(counts))) {
    genes <- counts[, match("gene", tolower(colnames(counts)))]
  } else if ("symbol" %in% tolower(colnames(counts))) {
    genes <- counts[, match("symbol", tolower(colnames(counts)))]
  } else {
    cat("\n Warning in ReadCountSLAM function: gene, symbol or geneSymbol column was not found in count table!")
    cat("\n Unique values of the first column will be used as gene column\n\n")
    genes <- counts[, 1]
  }
  if ("geneID" %in% colnames(counts)) {
    # multiple ENSEMBL IDs could be assigned to one gene symbol.
    annotation <- counts[, c("geneID", "geneSymbol")] # keep intact ensembl id and gene name
    ind_nonUniqGenes <- duplicated(genes)
    genes[ind_nonUniqGenes] <- paste(genes[duplicated(genes)], "_", counts$geneID[ind_nonUniqGenes], sep = "")
    annotation$geneSymbol.ID <- genes
    rownames(annotation) <- genes
    rownames(counts) <- genes
  } else {
    counts <- counts[!duplicated(genes), ] # make sure rownames are unique
    rownames(counts) <- genes[!duplicated(genes)]
    genes <- genes[!duplicated(genes)]
    annotation <- NA
  }
  if (!IsSLAMseqCount(counts, totalCountKeyword = totalCountKeyword, tcCountKeyword = tcCountKeyword)) {
    cat("\nInfo: RNAseq count file!\n")
    kept <- !tolower(colnames(counts)) %in% tolower(c("geneID", "gene", "symbol", "geneSymbol", "bioType", "annotationLevel"))
    counts <- counts[, colnames(counts)[kept]]
    counts <- cbind(gene = genes, counts)
    return(invisible(list(counts = counts, annotation = annotation, counts_total = NULL, raw = raw)))
  } else {
    cat("\nInfo: SLAMseq count file!\n")
    idx_total <- grep(paste0(totalCountKeyword, "$"), colnames(counts))
    idx_tc <- grep(paste0(tcCountKeyword, "$"), colnames(counts))
    counts_total <- counts[, idx_total]
    counts_total <- cbind(gene = genes, counts_total)
    colnames(counts_total) <- gsub(totalCountKeyword, "", colnames(counts_total))
    counts_tc <- counts[, idx_tc]
    counts_tc <- cbind(gene = genes, counts_tc)
    colnames(counts_tc) <- gsub(tcCountKeyword, "", colnames(counts_tc))
    return(invisible(list(counts = counts_tc, annotation = annotation, counts_total = counts_total, raw = raw)))
  }
}

######################################################
#' Run two-group SLAM-seq differential expression
#'
#' Fits a limma-voom model to Tc read counts while normalizing with paired total
#' counts and optionally writes the result table.
#'
#' @param counts Tc read-count matrix.
#' @param counts_total Matched total read-count matrix.
#' @param annotation Optional annotation table.
#' @param meta Metadata table with sample IDs and groups.
#' @param comp Comparison string in the form `Exp_VS_Ctl`.
#' @param outFile Optional output file path.
#' @param cutoff_count Raw-count filtering threshold.
#' @param cutoff_cpm Reserved CPM cutoff argument for compatibility.
#' @param logFile Optional log file path.
#'
#' @return Invisible list with `DEGs` table and aligned `meta` table.
SLAMseqDE <- function(counts = counts, counts_total = NULL, annotation = NULL, meta = meta, comp = comp, outFile = NA, cutoff_count = 0, cutoff_cpm = 0, logFile = NA) {
  ## created on 9/16/2022 for SLAMseq
  if (!is.data.frame(counts)) {
    cat("\n** Error in SLAMseqDE function: parameter [counts] must be a data.frame! **\n\n")
    stop("Exit...")
  }
  if (!is.data.frame(meta)) {
    cat("\n** Error in SLAMseqDE function: parameter [meta] must be a data.frame! **\n\n")
    stop("Exit...")
  }

  comp <- gsub(" ", "", comp)
  comp <- gsub("_[vV][Ss]_", "_VS_", comp)
  keywords <- unlist(strsplit(comp, "_VS_")) # Exp_vs_Ctl ==> Exp, Ctl
  ExpLabel <- keywords[1]
  CtlLabel <- keywords[2]

  aligned <- AlignMetaToCounts(meta, counts)
  meta <- aligned$meta
  counts <- aligned$counts
  if (!is.null(counts_total)) {
    counts_total <- AlignCountsToMeta(counts_total, meta)
  }
  if (nrow(meta) == 0) {
    stop("No metadata samples matched count matrix columns.")
  }

  usedNewName <- FALSE
  partialMatchFlag <- FALSE
  if (usedNewName & "NEWNAME" %in% colnames(meta)) { # complete match
    Exp <- meta[toupper(meta$GROUP) == toupper(ExpLabel), "NEWNAME"]
    Ctl <- meta[toupper(meta$GROUP) == toupper(CtlLabel), "NEWNAME"]
    rownames(meta) <- meta$NEWNAME
  } else {
    Exp <- meta[toupper(meta$GROUP) == toupper(ExpLabel), "ID"]
    Ctl <- meta[toupper(meta$GROUP) == toupper(CtlLabel), "ID"]
    rownames(meta) <- meta$ID
  }
  msg <- paste0("\n[SLAMseqDE]: ", ExpLabel, ", n = ", length(Exp), "; ", CtlLabel, ", n = ", length(Ctl), "\n")
  cat(msg)
  if (min(length(Exp), length(Ctl)) < 2) { # partial match, 8/28/2020
    cat("\nWarning - no matched comparison. Trying partial match. ")
    if (usedNewName & "NEWNAME" %in% colnames(meta)) {
      Exp <- meta[grepl(ExpLabel, meta$GROUP, ignore.case = T), "NEWNAME"]
      Ctl <- meta[grepl(CtlLabel, meta$GROUP, ignore.case = T), "NEWNAME"]
      rownames(meta) <- meta$NEWNAME
    } else {
      Exp <- meta[grepl(ExpLabel, meta$GROUP, ignore.case = T), "ID"]
      Ctl <- meta[grepl(CtlLabel, meta$GROUP, ignore.case = T), "ID"]
      rownames(meta) <- meta$ID
    }
    nonUniqMatch <- length(intersect(Exp, Ctl))
    if (nonUniqMatch > 0) {
      cat(paste0("\n\t Error: non-unique partial match between comparison string and meta$GROUP [n = ", nonUniqMatch, "]"))
      stop(paste0("Non-unique partial match between comparison and meta$GROUP [n = ", nonUniqMatch, "]"))
    }

    if (min(length(Exp), length(Ctl)) < 2) {
      cat("\n Error: no /not enough matched samples for comparison.")
      cat("\n\t[SLAMseqDE]: Please check whether meta$GROUP matches with  comparisons.lst.\n")
      stop(paste0("Not enough matched samples. ", ExpLabel, ": n=", length(Exp), "; ", CtlLabel, ": n=", length(Ctl)))
    }
    cat(paste0("[looks good]\n"))
    partialMatchFlag <- TRUE
  }
  grp <- c(Exp, Ctl)
  meta <- meta[grp, ]


  counts <- counts[, rownames(meta)]
  counts_total <- counts_total[, rownames(meta)]

  if ("NEWNAME" %in% colnames(meta) & usedNewName) {
    colnames(counts) <- meta[match(colnames(counts), meta$ID), "NEWNAME"]
  }

  # =========TMM normalization and voom transformation===========
  # cat("\n\nTMM normalization...\n")
  if (partialMatchFlag) { # change group label if using partial match
    usedGroup <- meta$GROUP
    usedGroup[grepl(ExpLabel, meta$GROUP)] <- ExpLabel
    usedGroup[grepl(CtlLabel, meta$GROUP)] <- CtlLabel
    meta$GROUP <- usedGroup # update group label 5/13/2022 9:44 AM
  } else {
    usedGroup <- meta$GROUP
  }
  # print(usedGroup)
  dge <- DGEList(counts = counts, group = usedGroup)

  keep <- rowSums(counts > cutoff_count) >= min(table(meta$GROUP))
  cat("\nfilteration (Tc_ReadCount): Keep ", sum(keep), " genes\n")
  counts <- counts[keep, ]
  counts_total <- counts_total[keep, ]

  dge <- dge[keep, keep.lib.sizes = FALSE]

  TS <- c(rep("Exp", length(Exp)), rep("Ctl", length(Ctl)))
  TS <- factor(TS, levels = c("Exp", "Ctl"))
  if ("BATCH" %in% colnames(meta)) {
    BATCH <- factor(meta$BATCH)
    if (length(levels(BATCH)) > 1) { # 8/28/2020
      design <- model.matrix(~ 0 + TS + BATCH)
      colnames(design)[1:2] <- c(levels(TS)) # 4/5/2018 revised
    } else {
      design <- model.matrix(~ 0 + TS)
      colnames(design) <- c(levels(TS))
    }
  } else if ("GENDER" %in% colnames(meta)) {
    GENDER <- factor(meta$GENDER)
    if (length(levels(GENDER)) > 1) { # 8/28/2020
      design <- model.matrix(~ 0 + TS + GENDER) # GENDER correction?
      colnames(design)[1:2] <- c(levels(TS)) # 4/5/2018 revised
    } else {
      design <- model.matrix(~ 0 + TS + GENDER) # GENDER correction?
      colnames(design) <- c(levels(TS))
    }
  } else {
    design <- model.matrix(~ 0 + TS)
    colnames(design) <- c(levels(TS))
  }

  # dge <- calcNormFactors(dge)
  dge_total <- DGEList(counts = counts_total, group = usedGroup)
  dge_total <- calcNormFactors(dge_total, method = "TMM")
  dge$samples$norm.factors <- dge_total$samples$norm.factors

  # dge$samples$norm.factors <- colSums(counts_total)/colSums(counts)
  cpm_result <- as.data.frame(cpm(dge, log = T)) # Yiping suggested 9/28/2022
  cpm_result <- round(cpm_result, 2)

  # cat("\nRunning Voom transformation...\n")
  v <- voom(dge, design, plot = F)

  Exp.mean <- apply(cpm_result[, Exp], 1, mean)
  Ctl.mean <- apply(cpm_result[, Ctl], 1, mean)

  # ===========voom=============
  fit <- lmFit(v, design)
  cont.matrix <- makeContrasts(Diff = (Exp - Ctl), levels = design)
  fitcon <- contrasts.fit(fit, cont.matrix)
  fitcon <- eBayes(fitcon)
  comb <- topTable(fitcon, n = nrow(dge))

  comb <- comb[, !colnames(comb) %in% c("B")] #  add back "t" on 9/28/2022
  comb$logFC <- round(comb$logFC, 2)
  comb$AveExpr <- round(comb$AveExpr, 2)
  colnames(comb) <- gsub("^t$", "t.Statistic", colnames(comb))
  colnames(comb) <- gsub("logFC", "log2FC", colnames(comb))

  # comb$P.Value <- format.pval(comb$P.Value)
  # comb$adj.P.Val <- format.pval(comb$adj.P.Val)
  comb_ncol <- ncol(comb)
  used <- cbind(comb, round(Exp.mean[rownames(comb)], 2), round(Ctl.mean[rownames(comb)], 2))
  colnames(used)[(comb_ncol + 1):(comb_ncol + 2)] <- paste(keywords, ".AveExpr", sep = "")
  res <- cbind(used, round(v$E[rownames(comb), ], 2))
  diff_table <- cbind(gene = rownames(res), res)

  if (!is.na(outFile)) {
    finalTable <- res
    # colnames(finalTable) <- gsub("logFC", "log2FC", colnames(finalTable))
    colnames(finalTable) <- gsub("adj.P.Val", "FDR", colnames(finalTable))
    if (!is.null(annotation) && all(c("geneID", "geneSymbol") %in% colnames(annotation))) {
      finalTable <- cbind(annotation[rownames(finalTable), c("geneID", "geneSymbol")], finalTable)
    } else {
      finalTable <- cbind(gene = rownames(finalTable), finalTable)
    }
    write.table(finalTable, file = outFile, sep = "\t", col.names = TRUE, row.names = F, quote = F)
  }
  return(invisible(list(DEGs = diff_table, meta = meta)))
}
######################################################
# 9/30/2022 for SLAMseq
######################################################
#' Transform count matrices for visualization
#'
#' Applies log2, log2 CPM, or DESeq2 VST normalization to RNA-seq or SLAM-seq
#' count data.
#'
#' @param count Count matrix.
#' @param meta Metadata table with sample annotations.
#' @param norm.method Normalization method (`log2CPM`, `log2`, or `vst`).
#' @param total_count Optional total-count matrix for SLAM-seq scaling.
#'
#' @return Normalized numeric matrix.
DataTransform <- function(count, meta = NULL, norm.method = "log2CPM", total_count = NULL) {
  # norm.method:
  #   log2: log2 (count+1)
  #   log2CPM: log2 (CPM)
  #   vst:
  # total_count, required for SLAMseq
  #   count=Tc_Readcount
  #   total_count= total_Readcount

  cat("\nInfo: counts ", dim(count))
  cat("\nInfo: meta ", dim(meta))
  print(meta)
  norm.method <- tolower(norm.method)
  commonIds <- intersect(colnames(count), meta$ID)
  meta <- meta[commonIds, ]
  count <- count[, commonIds]
  count <- count[, unlist(lapply(count, is.numeric))]
  cat("\nInfo: counts ", dim(count))
  if (!is.null(total_count)) {
    cat("\nscaled by total counts, ")
    total_count <- total_count[, commonIds]
    # rownames(total_count) <- genes
    cat("\ntotal_counts\n")
    # print(colnames(total_count))
    cat(dim(total_count))
  }
  if (norm.method == "vst") {
    is_DESeq2_available <- suppressPackageStartupMessages(require("DESeq2"))
    if (!is_DESeq2_available) {
      cat("\nError: DESeq2 is not installed.")
      stop("DESeq2 is not installed. Please install it to use VST normalisation.")
    }
    cat("[vst]")

    meta$GROUP <- factor(meta$GROUP, levels = unique(meta$GROUP))
    dds <- DESeqDataSetFromMatrix(countData = count, colData = meta, design = ~GROUP)
    dds <- estimateSizeFactors(dds)
    if (!is.null(total_count)) {
      print(dim(total_count))
      dds_total <- DESeqDataSetFromMatrix(countData = total_count, colData = meta, design = ~GROUP)
      dds_total <- estimateSizeFactors(dds_total)
      sizeFactors(dds) <- sizeFactors(dds_total)
    }
    vsd <- varianceStabilizingTransformation(dds, blind = T)
    return(assay(vsd))
  } else if (norm.method == "log2") {
    cat("[log2]")
    return(log2(count + 0.5))
  } else if (norm.method == "log2cpm") {
    cat("[log2CPM]")
    is_edgeR_available <- suppressPackageStartupMessages(require("edgeR"))
    cat("\nInfo: log2CPM transformtion.")
    cat("\ninfo: calss(counts), ", class(count))
    if (!is.null(total_count)) {
      total_count <- total_count[, unlist(lapply(total_count, is.numeric))]
      log2CPM <- cpm(count, log = T, lib.size = colSums(total_count))
    } else {
      log2CPM <- cpm(count, log = T)
    }
    cat("\nInfo: dim(log2CPM)", dim(log2CPM), "\n")
    return(log2CPM)
  } else {
    stop(paste0("Invalid norm.method in DataTransform: '", norm.method, "'. Use log2CPM, log2, or vst."))
  }
}
#######################################
#' Plot PCA for expression data
#'
#' Selects the most variable features, performs PCA, and returns a grouped
#' sample scatter plot.
#'
#' @param df Expression matrix.
#' @param meta Metadata table with IDs, groups, and colors.
#' @param scale Logical flag passed to `prcomp`.
#' @param topn Number of most variable features to use.
#' @param outFile Optional path to save the figure.
#' @param label Logical flag to label sample points.
#' @param title Optional custom title.
#' @param varMethod Placeholder for variability method selection.
#'
#' @return `ggplot` object.
PlotPCA <- function(df, meta, scale = T, topn = 3000, outFile = NA, label = F, title = NA, varMethod = "mad") {
  cat("\n[PlotPCA]: scale=", scale, "; topn=", topn, "; method=", varMethod, "\n")

  # rownames(diff) <- diff_table$gene
  if (!is.data.frame(df)) {
    cat("\n** Error in PlotPCA function: parameter [df] must be a data.frame! **\n\n")
    stop("Exit...")
  }
  cat("\nPCA #features ", topn, " from", nrow(df), "\n")
  cat("\n")
  if (!is.data.frame(meta)) {
    cat("\n** Error in PlotPCA function: parameter [meta] must be a data.frame! **\n\n")
    stop("Exit...")
  }
  # validate input
  colnames(meta) <- toupper(colnames(meta))
  if (!all(c("ID", "GROUP", "COLOR") %in% colnames(meta))) {
    cat("\n [PlotPCA] Invalid meta file. ID,GROUP or COLOR column(s) not found.\n")
    stop("exit.")
  }
  isNewName <- length(intersect(colnames(df), meta$NEWNAME)) > length(intersect(colnames(df), meta$ID))
  if (isNewName) {
    cat("\n [PlotPCA]Use NEWNAME in df table..\n")
    keptSamples <- intersect(colnames(df), meta$NEWNAME)
    idx1 <- match(keptSamples, colnames(df))
    idx2 <- match(keptSamples, meta$ID)
    colnames(df)[idx1] <- meta$NEWNAME[idx2]
    rownames(meta) <- meta$NEWNAME
    # meta$ID <- meta$NEWNAME
  } else {
    cat("\n[PlotPCA]Use ID in df table..\n")
    rownames(meta) <- meta$ID
    keptSamples <- intersect(rownames(meta), colnames(df))
  }
  if (length(keptSamples) == 0) {
    cat("\n ** [PlotPCA]Error: ID in metadata table doesn\'t match colnames in input matrix! **\n")
    stop(" [PlotPCA] Invalid metadata file!\n")
  }
  meta <- meta[keptSamples, ]
  dat <- df[, keptSamples]
  dat_raw <- dat
  dat <- as.data.frame(lapply(dat, function(x) suppressWarnings(as.numeric(as.character(x)))),
    check.names = FALSE
  )
  rownames(dat) <- rownames(dat_raw)
  n_bad_pca <- sum(!is.finite(as.matrix(dat)), na.rm = TRUE)
  if (n_bad_pca > 0) {
    cat("\nWarning[PlotPCA]: excluding feature row(s) affected by ",
      n_bad_pca, " non-numeric/NA/Inf expression value(s).\n",
      sep = ""
    )
  }
  keep_finite_features <- stats::complete.cases(dat)
  dat <- dat[keep_finite_features, , drop = FALSE]
  if (nrow(dat) == 0) {
    stop("No finite numeric features are available for PCA after filtering NA/non-numeric values.")
  }
  topn <- min(topn, nrow(dat))

  # calculate Median Absolute Deviation for each row
  rmads <- apply(dat, 1, mad)
  kept <- names(sort(rmads, decreasing = TRUE))[1:topn] # descending
  print(head(rownames(dat)))
  used <- dat[rownames(dat) %in% as.character(kept), ]

  cat("\ntopn=", topn, " kept features:", nrow(used), "\n", "dim=", dim(used), "\n")
  print(head(kept))

  pcs <- prcomp(t(used), center = TRUE, scale = scale)
  pctVar <- round(((pcs$sdev)^2 / sum((pcs$sdev)^2) * 100), 2)
  ######################################################
  if (is.na(title)) {
    myTitle <- paste0("PCA \nTop ", topn, " most variable genes in ", nrow(meta), " samples")
    # myTitle <- paste0("PCA\n", pctVar[1]+pctVar[2], "% of variance were explained by the first two PCs" )
  } else {
    myTitle <- paste0(title, "\nTop ", topn, " most variable genes in ", nrow(meta), " samples")
    # myTitle <- paste0(title, "\n", pctVar[1]+pctVar[2], "% of total variance were explained by the first two principal components" )
  }

  options(warn = -1)
  sampleSize <- nrow(meta)
  dotSize <- 5 - log2(nrow(meta)) / 2
  dotSize <- ifelse(dotSize < 0, 1, dotSize)
  options(stringsAsFactors = F)
  DF <- as.data.frame(pcs$x)
  if ("NEWNAME" %in% colnames(meta) & !all(meta$ID == meta$NEWNAME)) {
    DF <- cbind(DF, ID = meta$ID, GROUP = meta$GROUP, INFO = paste(meta$ID, meta$NEWNAME, meta$GROUP, sep = "\n"), COLOR = meta$COLOR)
  } else {
    DF <- cbind(DF, ID = meta$ID, GROUP = meta$GROUP, INFO = paste(meta$ID, meta$GROUP, sep = "\n"), COLOR = meta$COLOR)
  }
  pcaGroups <- unique(as.character(DF$GROUP))
  cmap <- setNames(rep(standardColors(), length.out = length(pcaGroups)), pcaGroups)
  DF$COLOR <- unname(cmap[as.character(DF$GROUP)])
  if (length(unique(DF$GROUP)) != length(unique(DF$COLOR))) { # update color
    for (grp in unique(DF$GROUP)) {
      DF$COLOR[DF$GROUP == grp] <- head(DF$COLOR[DF$GROUP == grp], n = 1)
    }
  }

  if ("SHAPE" %in% colnames(meta)) {
    DF <- cbind(DF, SHAPE = meta$SHAPE, SYMBOL = meta$SYMBOL)
    # DF$SHAPE[DF$SHAPE=="" | is.na(DF$SHAPE) ] <- "NA"
    if (length(unique(DF$SHAPE)) <= 5) {
      # shapes<- c(21,23,24,22,25)  # move to ReadMeta()
      fill_manual_status <- TRUE
    } else {
      # shapes<- c(19, 17, 15, 18, 16, 14:0)
      fill_manual_status <- FALSE
    }
    uniqCombs <- DF[, c("COLOR", "GROUP", "SYMBOL", "SHAPE")]
    uniqCombs$comb <- paste(DF$COLOR, DF$GROUP, DF$SHAPE, sep = "_")
    uniqCombs <- uniqCombs[!duplicated(uniqCombs$comb), c("COLOR", "GROUP", "SYMBOL", "SHAPE")]
    SHAPES <- factor(DF$SHAPE, levels = unique(DF$SHAPE))
  } else {
    # when multiple groups use same color, need to put up all groups.
    uniqCombs <- DF[, c("COLOR", "GROUP")]
    uniqCombs$comb <- paste(DF$COLOR, DF$GROUP, sep = "_")
    uniqCombs <- uniqCombs[!duplicated(uniqCombs$comb), c("COLOR", "GROUP")]
  }
  GROUPS <- factor(DF$GROUP, levels = unique(DF$GROUP))
  # mutliple groups may share same shape
  if ("SHAPE" %in% colnames(DF)) {
    if (fill_manual_status) {
      pg <- ggplot(DF, aes(x = PC1, y = PC2, text = INFO)) +
        xlab(paste0("PC1 (", pctVar[1], "%)")) +
        ylab(paste0("PC2 (", pctVar[2], "%)")) +
        geom_hline(yintercept = 0, colour = "grey70", linetype = "dashed", size = 0.25) +
        geom_vline(xintercept = 0, colour = "grey70", linetype = "dashed", size = 0.25) +
        geom_point(size = dotSize, aes(fill = GROUPS, shape = SHAPES), color = "grey50", alpha = 0.6, lwd = 2) +
        scale_fill_manual(name = "GROUP", labels = unique(DF$GROUP), values = unique(DF$COLOR)) +
        scale_shape_manual(name = "SHAPE", labels = unique(uniqCombs$SHAPE), values = unique(uniqCombs$SYMBOL)) +
        guides(fill = guide_legend(override.aes = list(shape = 21))) +
        theme_bw() +
        theme_classic(base_size = 10) +
        theme(aspect.ratio = 1) +
        theme(panel.border = element_rect(colour = "grey10", fill = NA, size = 1.5)) +
        theme(
          legend.text = element_text(size = 7),
          legend.title = element_text(size = 8, colour = "grey10", face = "bold"),
          plot.title = element_text(size = 10),
          axis.title = element_text(size = 10),
          axis.text.x = element_text(size = 10, color = "grey10"),
          axis.text.y = element_text(size = 10, color = "grey10")
        ) +
        ggtitle(myTitle)
    } else {
      pg <- ggplot(DF, aes(x = PC1, y = PC2, text = INFO)) +
        xlab(paste0("PC1 (", pctVar[1], "%)")) +
        ylab(paste0("PC2 (", pctVar[2], "%)")) +
        geom_hline(yintercept = 0, colour = "grey70", linetype = "dashed", size = 0.25) +
        geom_vline(xintercept = 0, colour = "grey70", linetype = "dashed", size = 0.25) +
        geom_point(size = dotSize, aes(color = GROUPS, shape = SHAPES), alpha = 0.6, lwd = 2) +
        scale_color_manual(name = "GROUP", labels = uniqCombs$GROUP, values = uniqCombs$COLOR) +
        scale_shape_manual(name = "SHAPE", labels = unique(uniqCombs$SHAPE), values = unique(uniqCombs$SYMBOL)) +
        guides(color = guide_legend(override.aes = list(shape = 21))) +
        theme_bw() +
        theme_classic(base_size = 10) +
        theme(aspect.ratio = 1) +
        theme(panel.border = element_rect(colour = "grey10", fill = NA, size = 1.5)) +
        theme(
          legend.text = element_text(size = 7),
          legend.title = element_text(size = 8, colour = "grey10", face = "bold"),
          plot.title = element_text(size = 10),
          axis.title = element_text(size = 10),
          axis.text.x = element_text(size = 10, color = "grey10"),
          axis.text.y = element_text(size = 10, color = "grey10")
        ) +
        ggtitle(myTitle)
    }
  } else {
    pg <- ggplot(DF, aes(x = PC1, y = PC2, text = INFO)) +
      xlab(paste0("PC1 (", pctVar[1], "%)")) +
      ylab(paste0("PC2 (", pctVar[2], "%)")) +
      geom_hline(yintercept = 0, colour = "grey70", linetype = "dashed", size = 0.25) +
      geom_vline(xintercept = 0, colour = "grey70", linetype = "dashed", size = 0.25) +
      geom_point(size = dotSize, aes(fill = GROUPS), colour = "grey50", lwd = 2, alpha = 0.6, shape = 21) +
      scale_fill_manual(name = "GROUP", labels = uniqCombs$GROUP, values = uniqCombs$COLOR) +
      theme_bw() +
      theme_classic(base_size = 10) +
      theme(aspect.ratio = 1) +
      theme(panel.border = element_rect(colour = "grey20", fill = NA, size = 1.5)) +
      theme(
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 8, colour = "grey10", face = "bold"),
        plot.title = element_text(size = 10),
        axis.title = element_text(size = 10),
        axis.text.x = element_text(size = 10, color = "grey10"),
        axis.text.y = element_text(size = 10, color = "grey10")
      ) +
      ggtitle(label = myTitle)
  }
  pg <- pg + theme(
    legend.position = "bottom",
    legend.title = element_text(size = 11, colour = "grey10", face = "bold"),
    legend.text = element_text(size = 10),
    legend.key.size = grid::unit(0.55, "cm")
  )
  if (label) {
    pg1 <- pg + geom_text(data = DF, aes(x = PC1, y = PC2, label = ID), hjust = 0, nudge_x = 0.2, size = 2.5)
  } else {
    pg1 <- pg
  }
  if (!is.na(outFile)) {
    if (nrow(meta) <= 20) {
      pg2 <- pg + ggrepel::geom_text_repel(aes(label = ID), size = 2.5)
    } else {
      pg2 <- pg
    }
    ggsave(file = outFile, pg2, width = 7, height = 6, units = "in")
    cat("\n\t", basename(outFile), "[saved]")
  }
  return(pg1)
}
############################################
#' Create a two-dimensional PCA plot wrapper
#'
#' Aligns metadata to a matrix-like input and delegates PCA plotting to
#' `PlotPCA`.
#'
#' @param dat Expression matrix.
#' @param meta Metadata table with sample labels.
#' @param dataType Data type label retained for compatibility.
#' @param scale Logical flag passed to PCA.
#' @param topn Number of most variable features to use.
#' @param outFile Optional path to save the figure.
#' @param label Logical flag to label points.
#'
#' @return `ggplot` object.
PCA2d <- function(dat, meta = NULL, dataType = "RNAseq", scale = T, topn = 3000, outFile = NA, label = F) {
  # meta <- tables$meta_used
  # dat <- tables$counts
  # dat_total <- tables$counts_total
  colnames(meta) <- toupper(colnames(meta))
  if (!"ID" %in% colnames(meta) | !"GROUP" %in% colnames(meta)) {
    cat("\nInvalid meta file. ID or/and Group column(s) not found.\n")
    stop("exit.")
  }
  isNewName <- length(intersect(colnames(dat), meta$NEWNAME)) > length(intersect(colnames(dat), meta$ID))
  if (isNewName) {
    # keptSamples <-  intersect(meta$NEWNAME,colnames(log2CPM))
    keptSamples <- intersect(colnames(dat), meta$NEWNAME)
    idx1 <- match(keptSamples, colnames(dat))
    idx2 <- match(keptSamples, meta$ID)
    colnames(dat)[idx1] <- meta$NEWNAME[idx2]
    # colnames(dat_total)[idx1] <- meta$NEWNAME[idx2]
    rownames(meta) <- meta$NEWNAME
  } else {
    rownames(meta) <- meta$ID
    keptSamples <- intersect(colnames(dat), meta$ID)
  }

  if (length(keptSamples) == 0) {
    cat("\nID in metadata table doesn't match colnames in input matrix.\n")
    stop(paste0("Invalid metadata file\n"))
  }
  # rownames(tarow_grpets) <- tarow_grpets$ID
  meta <- meta[keptSamples, ]
  used <- dat[, keptSamples]
  used <- as.data.frame(used)

  varMethod <- "mad"

  pcaPlot <- PlotPCA(used, meta, scale = scale, topn = topn, outFile = outFile, label = label, varMethod = varMethod)
  return(pcaPlot)
}

##### =====================================================================
#' Create a metadata template from matrix columns
#'
#' Builds an ID, NEWNAME, and GROUP table from matrix sample columns for manual
#' metadata editing.
#'
#' @param mat Input matrix whose columns are sample IDs.
#' @param matIdFile Output path for the metadata template.
#'
#' @return Logical `TRUE` after writing the file.
CreateMatIdFile <- function(mat, matIdFile) {
  columns_excluded <- c("geneID", "gene", "symbol", "geneSymbol", "bioType", "annotationLevel", "log2FC", "FDR", "AveExpr")
  idx_used <- !tolower(colnames(mat)) %in% tolower(columns_excluded)
  mat <- data.frame(ID = colnames(mat)[idx_used], NEWNAME = colnames(mat)[idx_used], GROUP = "NA")
  write.table(mat, matIdFile, sep = "\t", quote = FALSE, row.names = FALSE, col.names = TRUE)
  return(TRUE)
}


##### =====================================================================


## TODO: add this section to main script or load these libraries along with other libraries ##
library(enrichplot)
library(fgsea)
library(msigdbr)
library(ggplot2)

#' Run fgsea from a differential marker file
#'
#' Converts a differential result file into a ranked list and runs fgsea against
#' an MSigDB collection.
#'
#' @param diff_file Path to differential marker file.
#' @param db MSigDB category code.
#' @param sig_only Logical flag reserved for filtering significant terms.
#' @param out Output path or prefix used by downstream steps.
#'
#' @return Object generated by the fgsea workflow.
run_fgsea_v2 <- function(diff_file, db, sig_only = FALSE, out) {
  ## TODO: Add code to allow loading custom gmt file ##

  # Extract gene set from msig db
  m_df <- msigdbr(species = "Homo sapiens", category = db)
  fgsea_sets <- m_df %>% split(x = .$gene_symbol, f = .$gs_name)


  diff_tab <- fread(diff_file)
  prefix <- gsub("_diff_markers.tsv", "", basename(diff_file))

  # Sort

  diff_tab$rank <- (diff_tab$avg_log2FC * (-log10(diff_tab$p_val)))
  diff_tab <- diff_tab %>%
    arrange(desc(rank))


  # Check for infinite values

  max_ranking <- max(diff_tab$rank[is.finite(diff_tab$rank)])
  min_ranking <- min(diff_tab$rank[is.finite(diff_tab$rank)])
  diff_tab$rank <- replace(diff_tab$rank, diff_tab$rank > max_ranking, max_ranking * 10)
  diff_tab$rank <- replace(diff_tab$rank, diff_tab$rank < min_ranking, min_ranking * 10)

  diff_tab <- diff_tab %>%
    arrange(desc(rank))

  # correct<-which(is.infinite(diff_tab$rank)==TRUE)
  # if(length(correct)>0)
  #   {
  #    diff_tab$rank[correct]<-9999
  #   }

  cluster.genes <- diff_tab %>% dplyr::select(gene, rank)
  ranks <- deframe(cluster.genes)

  eplot <- plotEnrichment(fgsea_sets, ranks)

  # Run GSEA
  fgseaRes <- fgsea(fgsea_sets, stats = ranks)

  # Organize results
  fgseaResTidy <- fgseaRes %>%
    as_tibble() %>%
    arrange(desc(NES))


  # write.xlsx(x = fgseaResTidy,file = paste0(out,prefix,'_',db,'_pathways.xlsx'))
  if (sig_only) {
    fgseaResTidy_sig <- fgseaResTidy[fgseaResTidy$padj <= 0.05, ]
  }
  pos <- fgseaResTidy_sig[which(fgseaResTidy_sig$NES > 0), ]
  neg <- fgseaResTidy_sig[which(fgseaResTidy_sig$NES < 0), ]
  neg <- neg %>% arrange(NES)

  ## TODO: add topn parameter to control number of pathways to plot ##
  if (nrow(pos) < 10) {
    pos <- pos
  } else {
    pos <- pos[1:10, ]
  }

  if (nrow(neg) < 10) {
    neg <- neg
  } else {
    neg <- neg[1:10, ]
  }

  fgseaRes_top <- rbind(pos, neg)

  ## Barplot
  fgsea_cols <- getOption("shinyRNAseq.theme.colors", list())
  neg_col <- if (!is.null(fgsea_cols$danger)) fgsea_cols$danger else "#C71C22"
  pos_col <- if (!is.null(fgsea_cols$success)) fgsea_cols$success else "#73A839"
  p <- ggplot(fgseaRes_top, aes(reorder(pathway, NES), NES)) +
    geom_col(aes(fill = NES < 0)) +
    scale_fill_manual(values = c("TRUE" = neg_col, "FALSE" = pos_col)) +
    coord_flip() +
    labs(
      x = "Pathway", y = "Normalized Enrichment Score",
      title = paste0(prefix)
    ) +
    theme_minimal()


  return(list(p, fgseaResTidy, fgseaResTidy_sig, ranks, prefix))
}
