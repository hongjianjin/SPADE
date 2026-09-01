######################################################

#' Read a count table while preserving ERCC spike-ins
#'
#' Loads a count matrix, applies optional gene-level filters, keeps ERCC rows,
#' and separates endogenous and ERCC counts in the return value.
#'
#' @param countFile Path to the count matrix file.
#' @param filterFlag Logical flag to filter by biotype and annotation level.
#' @param fill Passed to `read.table`.
#' @param check.names Passed to `read.table`.
#' @param header Passed to `read.table`.
#' @param sep Field separator used in the count file.
#' @param quote Quote character used in the count file.
#'
#' @return Invisible list with `counts`, `annotation`, and `ERCC` tables.
ReadCountERCC <- function(countFile, filterFlag = T, fill = TRUE, check.names = FALSE,
                          header = TRUE, sep = "\t", quote = "", row.names = NULL) {
  counts <- read.table(countFile,
    fill = fill, check.names = check.names,
    stringsAsFactors = F, row.names = row.names, header = header,
    sep = sep, quote = quote
  )
  counts <- na.omit(counts)

  # Helper to detect ERCC rows by gene name column
  .detectERCC <- function(df) {
    if ("geneSymbol" %in% colnames(df)) {
      return(grepl("^ERCC-", df[, "geneSymbol"]))
    }
    if ("gene" %in% tolower(colnames(df))) {
      return(grepl("^ERCC-", df[, match("gene", tolower(colnames(df)))]))
    }
    if ("symbol" %in% tolower(colnames(df))) {
      return(grepl("^ERCC-", df[, match("symbol", tolower(colnames(df)))]))
    }
    return(grepl("^ERCC-", df[, 1]))
  }

  if (filterFlag == T) {
    if ("bioType" %in% colnames(counts)) {
      isERCC <- .detectERCC(counts)
      counts <- counts[counts$bioType == "protein_coding" | isERCC, ]
    }
    if ("annotationLevel" %in% colnames(counts)) {
      isERCC <- .detectERCC(counts)
      counts <- counts[counts$annotationLevel <= 3 | isERCC, ]
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
  cleaned <- CleanGeneIdentifiers(counts, genes, fallback = fallback_gene_ids, context = "ReadCountERCC")
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
      cat("\nWarning[ReadCountERCC]: dropping ", sum(!keep_unique),
        " duplicated gene identifier row(s) in no-annotation input.\n",
        sep = ""
      )
    }
    counts <- counts[keep_unique, , drop = FALSE] # make sure rownames are unique
    genes <- genes[keep_unique]
    rownames(counts) <- genes
    annotation <- NULL
  }
  kept <- !tolower(colnames(counts)) %in% tolower(c("geneID", "gene", "symbol", "geneSymbol", "bioType", "annotationLevel"))
  cat("\n")
  counts <- counts[, colnames(counts)[kept]]
  counts <- cbind(gene = genes, counts)
  idx_ERCCs <- grep("^ERCC-", genes)
  idx_genes <- grep("^ERCC-", genes, invert = TRUE)
  counts_ERCCs <- counts[idx_ERCCs, ]
  counts_genes <- counts[idx_genes, ]
  if (!is.null(annotation)) {
    annotation <- annotation[idx_genes, ]
  }
  return(invisible(list(counts = counts_genes, annotation = annotation, ERCC = counts_ERCCs)))
}

######################################################
#' Extract group labels from a contrast string
#'
#' Parses comparison expressions and returns the group names referenced in the
#' contrast.
#'
#' @param string Contrast expression string.
#'
#' @return Character vector of parsed group names.
ExtractGroups <- function(string) {
  # 1. Remove the part before the '='
  formula_only <- gsub("^.*=", "", string)

  # 2. Split by any non-alphanumeric character (except underscores)
  # This targets: - ( ) + / etc.
  groups <- unlist(strsplit(formula_only, "[^a-zA-Z0-9_]+"))
  groups <- groups[groups != ""]
  # 3. Remove empty strings and return unique names
  # return(unique(groups))
  return(groups)
}
######################################################
#' Read matched metadata, counts, and ERCC counts
#'
#' Harmonizes metadata with a count table processed by `ReadCountERCC` and
#' returns aligned sample-level inputs for downstream analysis.
#'
#' @param metaFile Path to metadata file.
#' @param countFile Path to count matrix file.
#' @param filterFlag Logical flag passed to `ReadCountERCC`.
#'
#' @return Invisible list with `meta`, `counts`, `annotation`, and `counts_ERCC`.
ReadMetaAndCountsERCC <- function(metaFile, countFile, filterFlag = TRUE) {
  # remove useless rows in meta or useless columns in counts
  metaObj <- ReadMeta(metaFile)
  if (!isTRUE(metaObj[["valid"]])) {
    stop("Invalid metadata file. Please fix metadata validation errors before ERCC analysis.")
  }
  meta <- metaObj[["used"]]
  #-------------------------------------------
  rawData <- ReadCountERCC(countFile, filterFlag)
  counts <- rawData[["counts"]]
  counts_ERCC <- rawData[["ERCC"]]
  annotation <- rawData[["annotation"]]
  aligned <- AlignMetaToCounts(meta, counts)
  meta <- aligned$meta
  counts <- aligned$counts
  if (nrow(meta) == 0) {
    stop("No metadata samples matched count matrix columns.")
  }
  counts_ERCC <- AlignCountsToMeta(counts_ERCC, meta)
  commonSamples <- meta$ID
  if (tolower(colnames(counts)[1]) == "gene") {
    counts <- counts[, c(colnames(counts)[1], commonSamples)]
    counts_ERCC <- counts_ERCC[, c(colnames(counts)[1], commonSamples)]
  } else {
    counts <- counts[, commonSamples]
    counts_ERCC <- counts_ERCC[, commonSamples]
  }
  return(invisible(list(meta = meta, counts = counts, annotation = annotation, counts_ERCC = counts_ERCC)))
}

##################################################################
#' Run ERCC-aware two-group differential expression
#'
#' Performs limma-voom differential expression with optional ERCC-based
#' normalization diagnostics and returns both DE and ERCC summaries.
#'
#' @param counts Count matrix for endogenous genes.
#' @param annotation Optional annotation table.
#' @param meta Metadata table with sample IDs and groups.
#' @param comp Comparison string in the form `Exp_VS_Ctl`.
#' @param outFile Optional output file path.
#' @param cutoff_count Raw-count filtering threshold.
#' @param cutoff_cpm CPM filtering threshold (`-1` disables filtering).
#' @param logFile Optional log file path.
#' @param counts_ERCC Optional ERCC count matrix.
#'
#' @return Invisible list containing DE table, aligned metadata, and ERCC stats.
RNAseqDE2 <- function(counts, annotation = NULL, meta = meta, comp = comp, outFile = NA, cutoff_count = 0, cutoff_cpm = 0, logFile = NA, counts_ERCC = NULL) {
  if (class(counts) != "data.frame") {
    cat("\n** Error in RNAseqDE function: parameter [counts] must be a data.frame! **\n\n")
    stop("Exit...")
  }
  if (class(meta) != "data.frame") {
    cat("\n** Error in RNAseqDE function: parameter [meta] must be a data.frame! **\n\n")
    stop("Exit...")
  }

  comp0 <- gsub(" ", "", comp)
  comp <- gsub("_[vV][Ss]_", "_VS_", comp0)
  keywords <- unlist(strsplit(comp, "_VS_")) # Exp_vs_Ctl ==> Exp, Ctl
  ExpLabel <- keywords[1]
  CtlLabel <- keywords[2]
  aligned <- AlignMetaToCounts(meta, counts)
  meta <- aligned$meta
  counts <- aligned$counts
  if (!is.null(counts_ERCC)) {
    counts_ERCC <- AlignCountsToMeta(counts_ERCC, meta)
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
      stop("Non-unique partial match between comparison string and meta$GROUP")
    }
    if (min(length(Exp), length(Ctl)) < 2) {
      cat("\n Error: no /not enough matched samples for comparison.")
      msg <- paste0("[", ExpLabel, ", n = ", length(Exp), "; ", CtlLabel, ", n = ", length(Ctl), "]")
      cat("\n\t", msg, "\n")
      cat("\n\tPlease check whether meta$GROUP matches with  comparisons.lst.\n")
      stop("Not enough matched samples for comparison")
    }
    cat(paste0("[looks good]\n"))
    partialMatchFlag <- TRUE
  }
  grp <- c(Exp, Ctl)
  meta <- meta[grp, ]
  if (!is.na(logFile)) {
    msg <- paste0("[", ExpLabel, ", n = ", length(Exp), "; ", CtlLabel, ", n = ", length(Ctl), "]")
    cat("\n\t", msg, "\n")
  }
  counts <- counts[, meta$ID]


  # =========TMM normalization and voom transformation===========
  if (partialMatchFlag) { # change group label if using partial match
    usedGroup <- meta$GROUP
    usedGroup[grepl(ExpLabel, meta$GROUP)] <- ExpLabel
    usedGroup[grepl(CtlLabel, meta$GROUP)] <- CtlLabel
    meta$GROUP <- usedGroup # update group label 5/13/2022 9:44 AM
  } else {
    usedGroup <- meta$GROUP
  }
  dge <- DGEList(counts = counts, group = usedGroup)

  if ("NEWNAME" %in% colnames(meta) & usedNewName) {
    colnames(counts) <- meta[match(colnames(counts), meta$ID), "NEWNAME"]
  }

  if (cutoff_cpm == -1) {
    cat("\n Disabled filteration of lowly expressed genes\n")
    cutoff <- cutoff_cpm
  } else {
    if (cutoff_cpm > 0) {
      cutoff <- cutoff_cpm
    } else { # version 3,  10/21/2022-
      median.lib.size <- median(dge$samples$lib.size)
      if (cutoff_count == 0) {
        if (median.lib.size > 100e6) {
          cutoff_auto <- 10
        } else if (median.lib.size < 10e6) {
          cutoff_auto <- 1
        } else {
          cutoff_auto <- median.lib.size / 10e6
        }
        cat("\n1. cutoff_auto=", cutoff_auto, "\n")
        cutoff <- as.vector(cpm(cutoff_auto, median.lib.size))
      } else {
        cutoff <- as.vector(cpm(cutoff_count, median.lib.size))
      }
    }
    keep <- rowSums(cpm(dge) > cutoff) >= min(table(meta$GROUP))
    cat("\n Filteration: Keep ", sum(keep), " out of ", dim(dge)[1], " genes\n")
    dge <- dge[keep, keep.lib.sizes = FALSE]
  }

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
  dge <- calcNormFactors(dge)
  cat("\ndim(dge$counts)")
  print(dim(dge$counts))
  shift_result <- NULL
  ErccStat <- NULL
  if (!is.null(counts_ERCC)) {
    cat("\n Info: enable ERCC normalisation.\n")
    counts_ERCC <- counts_ERCC[, meta$ID]
    library_sizes <- dge$samples$lib.size
    if (sum(colSums(counts_ERCC) == 0) > length(meta$ID) / 2) {
      cat("\nWarning: no ERCC read counts? disabled this step.")
      sf_ercc <- 1
    } else {
      cat("\n[ERCC counts look ok].")
      sf_libSize <- dge$samples$norm.factors
      # sf_ercc1 <- calcNormFactors(ERCC, method="TMM")
      sf_ercc <- calcNormFactors(counts_ERCC, lib.size = library_sizes)
      cat("\n")
      # txtFile2 <- paste0(prefix,"_",comp0,"_ERCC_used_stat.txt")
      ErccStat <- CalcERCCnormFactor(counts, counts_ERCC, meta, txtFile = NULL)
      correlations <- ErccQC(counts_ERCC, meta)
      ErccStat$ERCC_COR <- correlations[ErccStat$ID]
      shift_result <- ErccShiftTest(stat = ErccStat, comp = comp, ratio = "pct_ERCC", group = "GROUP")
      dge$samples$norm.factors <- sf_ercc
    }
  }
  cpm_result <- as.data.frame(cpm(dge, log = T)) # HJ added 2017/11/20
  cpm_result <- round(cpm_result, 2)
  v <- voom(dge, design, plot = F)


  Exp.mean <- apply(cpm_result[, Exp], 1, mean)
  Ctl.mean <- apply(cpm_result[, Ctl], 1, mean)

  # ===========voom=============
  fit <- lmFit(v, design)
  cont.matrix <- makeContrasts(Diff = (Exp - Ctl), levels = design)
  fitcon <- contrasts.fit(fit, cont.matrix)
  fitcon <- eBayes(fitcon)
  comb <- topTable(fitcon, n = nrow(dge))

  comb <- comb[, !colnames(comb) %in% c("B")] # add back "t" on 9/28/2022
  comb$logFC <- round(comb$logFC, 2)
  comb$AveExpr <- round(comb$AveExpr, 2)
  colnames(comb) <- gsub("^t$", "t.Statistic", colnames(comb))
  colnames(comb) <- gsub("logFC", "log2FC", colnames(comb))

  comb_ncol <- ncol(comb)
  used <- cbind(comb, round(Exp.mean[rownames(comb)], 2), round(Ctl.mean[rownames(comb)], 2))
  colnames(used)[(comb_ncol + 1):(comb_ncol + 2)] <- paste(keywords, ".AveExpr", sep = "")
  res <- cbind(used, round(v$E[rownames(comb), ], 2))
  diff_table <- cbind(gene = rownames(res), res)
  if (!is.na(outFile)) {
    finalTable <- res
    # colnames(finalTable) <- gsub("logFC", "log2FC", colnames(finalTable))
    colnames(finalTable) <- gsub("adj.P.Val", "FDR", colnames(finalTable))

    if (!is.null(annotation) & all(c("geneID", "geneSymbol") %in% colnames(annotation))) {
      finalTable <- cbind(annotation[rownames(finalTable), c("geneID", "geneSymbol")], finalTable)
    } else {
      finalTable <- cbind(gene = rownames(finalTable), finalTable)
    }
    write.table(finalTable, file = outFile, sep = "\t", col.names = TRUE, row.names = F, quote = F)
  }
  return(invisible(list(DE = diff_table, meta = meta, counts_ERCC = counts_ERCC, ErccStat = ErccStat, ErccShift = shift_result)))
}

##################################################################
#' Run complex-contrast ERCC-aware differential expression
#'
#' Fits limma-voom models for multi-group contrasts and can incorporate ERCC
#' normalization metrics during analysis.
#'
#' @param counts Count matrix for endogenous genes.
#' @param annotation Optional annotation table.
#' @param meta Metadata table with sample IDs and groups.
#' @param comp Complex contrast expression string.
#' @param outFile Optional output file path.
#' @param cutoff_count Raw-count filtering threshold.
#' @param cutoff_cpm CPM filtering threshold (`-1` disables filtering).
#' @param logFile Optional log file path.
#' @param counts_ERCC Optional ERCC count matrix.
#'
#' @return Invisible list containing DE table, aligned metadata, and ERCC stats.
RNAseqDEadv2 <- function(counts, annotation = NULL, meta = meta, comp = comp, outFile = NA, cutoff_count = 0, cutoff_cpm = 0, logFile = NA, counts_ERCC = NULL) {
  if (class(counts) != "data.frame") {
    cat("\n** Error in RNAseqDE function: parameter [counts] must be a data.frame! **\n\n")
    stop("Exit...")
  }
  if (class(meta) != "data.frame") {
    cat("\n** Error in RNAseqDE function: parameter [meta] must be a data.frame! **\n\n")
    stop("Exit...")
  }
  groups <- ExtractGroups(comp) # return groups involved in contrast
  # complete match
  kept <- meta$ID %in% colnames(counts) & meta$GROUP %in% groups
  meta <- meta[kept, ]
  meta0 <- meta
  counts <- counts[, meta$ID]
  TS <- factor(meta$GROUP)
  if ("BATCH" %in% colnames(meta)) {
    BATCH <- factor(meta$BATCH)
    if (length(levels(BATCH)) > 1) { # 8/28/2020
      design <- model.matrix(~ 0 + TS + BATCH, meta)
      colnames(design)[1:2] <- c(levels(TS)) # 4/5/2018 revised
    } else {
      design <- model.matrix(~ 0 + TS, meta)
      colnames(design) <- c(levels(TS))
    }
  } else if ("GENDER" %in% colnames(meta)) {
    GENDER <- factor(meta$GENDER)
    if (length(levels(GENDER)) > 1) { # 8/28/2020
      design <- model.matrix(~ 0 + TS + GENDER, meta) # GENDER correction?
      colnames(design)[1:2] <- c(levels(TS)) # 4/5/2018 revised
    } else {
      design <- model.matrix(~ 0 + TS + GENDER, meta) # GENDER correction?
      colnames(design) <- c(levels(TS))
    }
  } else {
    design <- model.matrix(~ 0 + TS, meta)
    colnames(design) <- c(levels(TS))
  }
  colnames(design) <- gsub("TS", "", colnames(design))
  cont.matrix <- GetContrastMatrix(comp, design)
  groups <- rownames(cont.matrix)[cont.matrix[, 1] != 0]
  groupsIncluded <- paste(groups, ".AveExpr", sep = "")

  samplesIncluded <- meta$ID[meta$GROUP %in% groups]

  if ("NEWNAME" %in% colnames(meta)) {
    usedColumns <- intersect(colnames(counts), meta$ID)
    idx1 <- match(usedColumns, colnames(counts))
    idx2 <- match(usedColumns, meta$ID)
    colnames(counts)[idx1] <- meta$NEWNAME[idx2]
    samplesIncluded <- meta$NEWNAME[meta$GROUP %in% groups]
    meta <- meta[meta$NEWNAME %in% samplesIncluded, ]
  } else {
    meta <- meta[meta$ID %in% samplesIncluded, ]
  }

  counts <- counts[, samplesIncluded]
  usedGroup <- meta$GROUP
  dge <- DGEList(counts = counts, group = usedGroup)
  if (cutoff_cpm == -1) {
    cat("\n Disabled filteration of lowly expressed genes\n")
    cutoff <- cutoff_cpm
  } else {
    if (cutoff_cpm > 0) {
      cutoff <- cutoff_cpm
    } else { # version 3,  10/21/2022
      median.lib.size <- median(dge$samples$lib.size)
      if (cutoff_count == 0) {
        if (median.lib.size > 100e6) {
          cutoff_auto <- 10
        } else if (median.lib.size < 10e6) {
          cutoff_auto <- 1
        } else {
          cutoff_auto <- median.lib.size / 10e6
        }
        cutoff <- as.vector(cpm(cutoff_auto, median.lib.size))
      } else {
        cutoff <- as.vector(cpm(cutoff_count, median.lib.size))
      }
    }
    keep <- rowSums(cpm(dge) > cutoff) >= min(table(meta$GROUP))
    if (!is.na(logFile)) {
      countSamples <- table(meta$GROUP)[groups]
      msg <- paste(names(countSamples), "=", countSamples, collapse = ",", sep = "")
      msg <- paste0(msg, " [cutoff_cpm=", round(cutoff, 2), ", kept=", sum(keep), "]")
      cat("\n\t", msg, "\n")
    }

    dge <- dge[keep, keep.lib.sizes = FALSE]
  }
  shift_result <- NULL
  print(design)
  dge <- calcNormFactors(dge)
  if (!is.null(counts_ERCC)) {
    cat("\n Info: enable ERCC normalisation.\n")
    if ("NEWNAME" %in% colnames(meta)) {
      usedColumns <- intersect(colnames(counts_ERCC), meta$ID)
      idx1 <- match(usedColumns, colnames(counts_ERCC))
      idx2 <- match(usedColumns, meta$ID)
      colnames(counts_ERCC)[idx1] <- meta$NEWNAME[idx2]
      counts_ERCC <- counts_ERCC[, meta$NEWNAME]
    } else {
      counts_ERCC <- counts_ERCC[, meta$ID]
    }
    library_sizes <- dge$samples$lib.size
    if (sum(colSums(counts_ERCC) == 0) > length(meta$ID) / 2) {
      cat("\nWarning: no ERCC read counts? disabled this step.")
      sf_ercc <- 1
    } else {
      cat("\n[ERCC counts look ok].")
      sf_libSize <- dge$samples$norm.factors
      sf_ercc <- calcNormFactors(counts_ERCC, lib.size = library_sizes)
      cat("\n")
      # txtFile2 <- paste0(prefix,"_",comp0,"_ERCC_used_stat.txt")
      ErccStat <- CalcERCCnormFactor(counts, counts_ERCC, meta, txtFile = NULL)
      correlations <- ErccQC(counts_ERCC, meta)
      ErccStat$ERCC_COR <- correlations[ErccStat$ID]
      shift_result <- ErccShiftTest(stat = ErccStat, comp = comp, ratio = "pct_ERCC", group = "GROUP")
      dge$samples$norm.factors <- sf_ercc
    }
  }
  cpm_result <- as.data.frame(cpm(dge, log = T)) # HJ added 2017/11/20
  cpm_result <- round(cpm_result, 2)
  if ("NEWNAME" %in% colnames(meta)) {
    cpm_avg <- CalAvgByGrp(cpm_result, meta, id = "NEWNAME")
  } else {
    cpm_avg <- CalAvgByGrp(cpm_result, meta, id = "ID")
  }

  # ===========voom=============
  v <- voom(dge, design, plot = F)

  fit <- lmFit(v, design)
  fitcon <- contrasts.fit(fit, cont.matrix)
  fitcon <- eBayes(fitcon)
  comb <- topTable(fitcon, n = nrow(dge))

  comb <- comb[, !colnames(comb) %in% c("B")] #  add back "t" on 9/28/2022
  comb$logFC <- round(comb$logFC, 2)
  comb$AveExpr <- round(comb$AveExpr, 2)
  colnames(comb) <- gsub("^t$", "t.Statistic", colnames(comb))
  colnames(comb) <- gsub("logFC", "log2FC", colnames(comb))

  comb_ncol <- ncol(comb)
  res <- cbind(comb, cpm_avg[rownames(comb), groupsIncluded], cpm_result[rownames(comb), samplesIncluded])
  diff_table <- cbind(gene = rownames(res), res)
  if (!is.na(outFile)) {
    finalTable <- res
    colnames(finalTable) <- gsub("adj.P.Val", "FDR", colnames(finalTable))
    if (!is.null(annotation) & all(c("geneID", "geneSymbol") %in% colnames(annotation))) {
      finalTable <- cbind(annotation[rownames(finalTable), c("geneID", "geneSymbol")], finalTable)
    } else {
      finalTable <- cbind(gene = rownames(finalTable), finalTable)
    }
    write.table(finalTable, file = outFile, sep = "\t", col.names = TRUE, row.names = F, quote = F)
  }
  meta <- meta[meta$GROUP %in% groups, ]
  return(invisible(list(DE = diff_table, meta = meta, counts_ERCC = counts_ERCC, ErccStat = ErccStat, ErccShift = shift_result)))
}

##################################################################
#' Run multiple ERCC differential comparisons in parallel
#'
#' Executes `RNAseqDE2` or `RNAseqDEadv2` across a vector of comparisons using a
#' parallel worker cluster.
#'
#' @param counts Count matrix.
#' @param annotation Optional annotation table.
#' @param meta Metadata table.
#' @param comps Character vector of comparisons.
#' @param cutoff_count Raw-count filtering threshold.
#' @param cutoff_cpm CPM filtering threshold.
#' @param logFile Optional log file path.
#' @param ncores Number of parallel workers.
#' @param counts_ERCC Optional ERCC count matrix.
#'
#' @return List of per-comparison differential analysis results.
ParallelDE2 <- function(counts = counts, annotation = annotation, meta = meta, comps = comps,
                        cutoff_count = cutoff_count, cutoff_cpm = cutoff_cpm, logFile = logFile, ncores = 8, counts_ERCC = NULL) {
  library(parallel)
  #' Run DE
  #'
  #' Executes runDE.
  #'
  #' @param counts Argument for runDE.
  #' @param annotation Argument for runDE.
  #' @param meta Argument for runDE.
  #' @param comp Argument for runDE.
  #' @param cutoff_count Argument for runDE.
  #' @param cutoff_cpm Argument for runDE.
  #' @param outFile Argument for runDE.
  #' @param logFile Argument for runDE.
  #' @param counts_ERCC Argument for runDE.
  #'
  #' @return Return value produced by runDE.
  runDE <- function(counts, annotation, meta, comp, cutoff_count, cutoff_cpm, outFile, logFile, counts_ERCC) {
    suppressMessages(library(edgeR))
    suppressMessages(library(limma))
    if (grepl(".*=\\(.*-.*\\)", comp)) { # implemented March, 2026
      compName <- unlist(strsplit(comp, "="))[1]
      outFile <- paste0(prefix, compName, "_log2CPM.tsv")
      res <- RNAseqDEadv2(
        counts = counts, annotation = annotation, meta = meta, comp = comp,
        cutoff_count = cutoff_count, cutoff_cpm = cutoff_cpm, outFile = outFile, logFile = logFile, counts_ERCC = counts_ERCC
      )
    } else {
      outFile <- paste0(prefix, comp, "_log2CPM.tsv")
      res <- RNAseqDE2(
        counts = counts, annotation = annotation, meta = meta, comp = comp,
        cutoff_count = cutoff_count, cutoff_cpm = cutoff_cpm, outFile = outFile, logFile = logFile, counts_ERCC = counts_ERCC
      )
    }
    msg <- paste0(basename(outFile), "[saved]")
    cat("\n\t", msg, "\n")
    res
  }
  if (length(comps) == 1) {
    res <- runDE(counts, annotation, meta, comps, cutoff_count, cutoff_cpm, outFile, logFile, counts_ERCC)
    return(list(C1 = res))
  }

  avai_cores <- detectCores() - 1
  ncores <- ifelse(ncores > avai_cores, avai_cores, ncores)
  ncores <- ifelse(ncores > length(comps), length(comps), ncores)
  tryCatch(
    {
      # Initiate cluster
      cl <- makeCluster(ncores, outfile = "") # output error message to console
      clusterExport(cl,
        varlist = c(
          "counts", "annotation", "meta", "comps", "logFile", "cutoff_count", "cutoff_cpm", "prefix",
          "runDE", "RNAseqDE2", "RNAseqDEadv2", "GetContrastMatrix", "CalAvgByGrp",
          "counts_ERCC", "CalcERCCnormFactor", "ErccQC", "ErccShiftTest", "ExtractGroups"
        ),
        envir = environment()
      )
      # differential analysis
      res <- parLapply(
        cl, 1:length(comps),
        function(x) runDE(counts, annotation, meta, comps[x], cutoff_count, cutoff_cpm, outFile, logFile, counts_ERCC)
      )
      names(res) <- paste("C", 1:length(comps), sep = "")
      stopCluster(cl)
    },
    error = function(e) print(e)
  ) #-------end tryCatch
  return(res)
}


###############################################
#' Calculate ERCC-based normalization statistics
#'
#' Compares endogenous and ERCC library sizes and derives normalization factors
#' and spike-in proportions per sample.
#'
#' @param counts Endogenous count matrix.
#' @param counts_ERCC ERCC count matrix.
#' @param meta Metadata table.
#' @param txtFile Optional output path to save ERCC statistics.
#'
#' @return Data frame of ERCC normalization statistics by sample.
CalcERCCnormFactor <- function(counts, counts_ERCC, meta, txtFile = NULL) {
  cn <- colnames(counts)
  ids1 <- intersect(meta$NEWNAME, cn)
  ids2 <- intersect(meta$ID, cn)
  ids <- if (length(ids1) >= length(ids2)) ids1 else ids2
  counts <- counts[, ids]
  counts_ERCC <- counts_ERCC[, ids]
  if ("GROUP" %in% meta) {
    dge <- DGEList(counts = counts, group = meta$GROUP)
  } else {
    dge <- DGEList(counts = counts)
  }
  dge <- calcNormFactors(dge)
  libSizes <- dge$samples$lib.size
  ercc_libSizes <- colSums(counts_ERCC)
  nf_libSize <- dge$samples$norm.factors
  nf_ercc <- calcNormFactors(counts_ERCC, lib.size = libSizes)
  pct_ERCC <- ercc_libSizes / libSizes * 100

  res <- data.frame(ID = colnames(dge$counts), GROUP = meta$GROUP, gene_libSize = libSizes, ERCC_libSize = ercc_libSizes, pct_ERCC, nf_libSize = nf_libSize, nf_ERCC = nf_ercc)
  if (!is.null(txtFile)) {
    write.table(res, txtFile, sep = "\t", quote = FALSE, row.names = F, col.names = TRUE)
    cat("\n", basename(txtFile), "[saved]\n")
  }
  return(res)
}


###############################################

#' Visualize ERCC spike-in summary statistics
#'
#' Creates paired plots for ERCC spike-in ratios and ERCC-derived normalization
#' factors across groups.
#'
#' @param dat ERCC statistics data frame.
#' @param outFile Optional output path for combined figure.
#'
#' @return Combined plot object when `outFile` is `NULL`.
VizErccStat <- function(dat, outFile = NULL) {
  plot1 <- ggplot(dat, aes(x = GROUP, y = pct_ERCC, fill = GROUP)) +
    geom_boxplot(aes(x = GROUP, y = pct_ERCC),
      width = 0.5, colour = "black", outlier.colour =
        "grey30", outlier.alpha = 0.1, outlier.shape = 1, position = position_dodge(0.6)
    ) +
    geom_jitter(color = "black", width = 0.2, size = 2, alpha = 0.5) +
    theme_classic(base_size = 15) +
    labs(y = "Spike-in level (ERCC_libSize/gene_libSize*100)", x = "Group") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    theme(plot.title = element_text(size = 8), plot.subtitle = element_text(size = 8)) +
    ggtitle(label = "spike-in ratio boxplot")

  plot2 <- ggplot(dat, aes(x = nf_ERCC, y = pct_ERCC)) +
    geom_point(size = 2, aes(color = GROUP)) +
    theme_bw()
  plots <- patchwork::wrap_plots(list(plot1, plot2))

  if (is.null(outFile)) {
    return(plots)
  } else {
    ggsave(file = outFile, plots, width = 16, height = 8, units = "in", limitsize = TRUE)
    cat(paste0("\n\t", basename(outFile), "[saved]"))
  }
}

###############################################
#' Plot ERCC counts against expected concentrations
#'
#' Compares observed ERCC counts with the reference spike-in concentrations and
#' saves merged and faceted QC plots.
#'
#' @param ERCC_counts ERCC count matrix.
#' @param meta Metadata table with sample IDs.
#' @param outPrefix Prefix used for saved plot files.
#' @param ercc_control_file Optional path to ERCC control concentration file.
#'
#' @return Invisibly saves ERCC concentration QC plots.
VizErccConc <- function(ERCC_counts, meta, outPrefix, ercc_control_file = NULL) {
  if (tolower(colnames(ERCC_counts)[1]) == "gene") {
    ercc_ids <- ERCC_counts[, 1]
    ERCC_counts <- ERCC_counts[, -1, drop = FALSE]
    rownames(ERCC_counts) <- ercc_ids
  }
  ERCC_counts <- ERCC_counts[, meta$ID, drop = FALSE]
  used <- log2(ERCC_counts)
  rownames(used) <- rownames(ERCC_counts)
  if (nrow(used) < 5 | sum(colSums(ERCC_counts) == 0) > nrow(meta) / 2) {
    cat("\nError:[No valid ERCC found in this dataset.]\n\n")
    stop("No valid ERCC found in this dataset")
  }

  myColors <- ggplotColours(ncol(used))
  used$id <- row.names(used) # add id to each row
  used2 <- reshape2::melt(used, id.vars = "id")
  if (is.null(ercc_control_file)) {
    candidates <- c(
      file.path("data", "ERCC_Controls_Analysis.txt"),
      "ERCC_Controls_Analysis.txt",
      file.path("extdata", "ERCC_Controls_Analysis.txt")
    )
    ercc_control_file <- candidates[file.exists(candidates)][1]
  }
  if (is.na(ercc_control_file) || !file.exists(ercc_control_file)) {
    stop("ERCC Controls file not found.")
  }
  conc <- read.table(ercc_control_file, sep = "\t", header = TRUE, fill = TRUE, stringsAsFactors = FALSE, quote = "", row.names = 1, check.names = F, comment.char = "")
  conc <- conc[, c(1, 3)]
  rownames(conc) <- conc$ERCC
  colnames(conc)[2] <- "conc"
  used3 <- data.frame(used2, conc = log2(conc[used2$id, "conc"]))
  pg2 <- ggplot(
    data = used3,
    aes(x = conc, y = value, color = variable, group = variable)
  ) +
    geom_point() +
    geom_smooth(method = "lm", fill = NA) +
    xlab("log2(conc.)") +
    ylab("log2(count)") +
    theme_classic()
  outFile <- paste0(outPrefix, "_ERCC_log2count_vs_log2conc_merged.png")
  ggsave(file = outFile, pg2, width = 15, height = 10, units = "in", limitsize = TRUE)
  cat("\n", basename(outFile), "[saved]\n")

  pg3 <- ggscatter(used3,
    x = "conc", y = "value",
    color = "variable", palette = myColors,
    add = "reg.line"
  ) + xlab("log2(conc.)") + ylab("log2(count)") +
    facet_wrap(~variable) +
    stat_cor(label.y = 11) +
    stat_regline_equation(label.y = 14)

  outFile <- paste0(outPrefix, "_ERCC_log2count_vs_log2conc_facet.png")
  ggsave(file = outFile, pg3, width = 15, height = 15, units = "in", limitsize = TRUE)
  cat("\n", basename(outFile), "[saved]\n")
}

###############################################
#' Generate evenly spaced ggplot colors
#'
#' Returns a vector of HCL colors suitable for discrete plotting.
#'
#' @param n Number of colors to generate.
#' @param h Hue range used to generate colors.
#'
#' @return Character vector of color values.
ggplotColours <- function(n = 6, h = c(0, 360) + 15) {
  if ((diff(h) %% 360) < 1) h[2] <- h[2] - 360 / n
  hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65)
}

###############################################
#' Assess ERCC concentration concordance by sample
#'
#' Matches ERCC counts to the control concentration file and returns per-sample
#' Pearson correlations.
#'
#' @param counts_ERCC ERCC count matrix.
#' @param meta Metadata table.
#' @param ercc_control_file Optional path to ERCC control concentration file.
#'
#' @return Named numeric vector of Pearson correlations by sample.
ErccQC <- function(counts_ERCC, meta, ercc_control_file = NULL) {
  cn <- colnames(counts_ERCC)
  ids1 <- intersect(meta$NEWNAME, cn)
  ids2 <- intersect(meta$ID, cn)
  ids <- if (length(ids1) >= length(ids2)) ids1 else ids2
  counts_ERCC <- counts_ERCC[, ids]
  used <- log2(counts_ERCC)
  rownames(used) <- rownames(counts_ERCC)
  colnames(used) <- colnames(counts_ERCC)
  if (nrow(used) < 5 | sum(colSums(counts_ERCC) == 0) > nrow(meta) / 2) {
    cat("\nWarning:[No valid ERCC found in this dataset.]\n\n")
    return(setNames(rep(NA_real_, length(ids)), ids))
  }

  used$id <- row.names(used) # add id to each row
  # Locate ERCC controls file
  if (is.null(ercc_control_file)) {
    candidates <- c(
      file.path("data", "ERCC_Controls_Analysis.txt"),
      "ERCC_Controls_Analysis.txt",
      file.path("extdata", "ERCC_Controls_Analysis.txt")
    )
    ercc_control_file <- candidates[file.exists(candidates)][1]
  }
  if (is.na(ercc_control_file) || !file.exists(ercc_control_file)) {
    cat("\nWarning: ERCC Controls file not found. Skipping correlation QC.\n")
    return(setNames(rep(NA_real_, length(ids)), ids))
  }
  conc <- read.table(ercc_control_file, sep = "\t", header = TRUE, fill = TRUE, stringsAsFactors = FALSE, quote = "", row.names = 1, check.names = F, comment.char = "")
  conc <- conc[, c(1, 3)]
  colnames(conc) <- c("ERCC", "conc")
  rownames(conc) <- conc$ERCC
  conc$conc <- log2(conc$conc)

  common_id <- intersect(conc$ERCC, rownames(used))
  used <- used[common_id, ]
  used[used == -Inf] <- 0 # replace -Inf with 0
  conc <- conc[common_id, ]
  if (length(common_id) < 80) {
    cat("\nWarning:[Fewer than 80 ERCC spike-ins matched. Correlation may be unreliable.]\n\n")
  }
  if (length(common_id) < 5) {
    return(setNames(rep(NA_real_, length(ids)), ids))
  }
  pearson_r <- NULL
  for (id in ids) {
    pearson_r <- c(pearson_r, cor(as.numeric(used[, id]), as.numeric(conc$conc)))
  }
  names(pearson_r) <- ids
  return(pearson_r)
}

###############################################

#' Summarize ERCC coefficient of variation by group
#'
#' Aggregates ERCC spike-in proportions within each group and reports mean,
#' standard deviation, and coefficient of variation.
#'
#' @param ErccStat ERCC statistics data frame.
#'
#' @return Group-level summary data frame with mean, SD, and CV.
ErccCV <- function(ErccStat) {
  suppressMessages(suppressWarnings(library(dplyr)))
  # Calculate CV per group
  result <- ErccStat %>%
    mutate(pct_ERCC = as.numeric(as.character(pct_ERCC))) %>%
    group_by(GROUP) %>%
    summarise(
      ERCC_mean = mean(pct_ERCC),
      ERCC_sd = sd(pct_ERCC),
      ERCC_cv = (ERCC_sd / ERCC_mean) * 100
    )
  return(result)
}

###############################################
# for each comparison
#' Test ERCC spike-in shifts between groups
#'
#' Computes a t-test, fold shift, and replicate variability metrics for ERCC
#' summary ratios across a comparison.
#'
#' @param stat ERCC statistics data frame.
#' @param comp Comparison string.
#' @param ratio Column name used as spike-in ratio metric.
#' @param group Column name containing group labels.
#'
#' @return List with fold shift, p-value, group means, and CV metrics.
ErccShiftTest <- function(stat, comp = NULL, ratio = "pct_ERCC", group = "GROUP") {
  if (all(grepl(".*=\\(.*-.*\\)", comp))) { # complex comparison
    keywords <- ExtractGroups(comp)
    if (length(keywords) != 2) {
      return(list(comp = comp, Shift_FC = NA, AUC = NA, Shift_PVAL = NA, mean_group1 = NA, mean_group2 = NA, CV_group1 = NA, CV_group2 = NA, SHIFT = NA))
    }
  } else {
    comp <- gsub("_[vV][Ss]_", "_VS_", gsub(" ", "", comp))
    keywords <- unlist(strsplit(comp, "_VS_")) # Exp_vs_Ctl ==> Exp, Ctl
  }
  group1 <- keywords[1]
  group2 <- keywords[2]
  used <- stat[stat[[group]] %in% c(group1, group2), c(group, ratio)]
  used[, ratio] <- as.numeric(used[, ratio])
  formula_str <- paste(ratio, "~", group)
  # 3. Calculate Significance and Effect Size
  # We use a t-test to see if the mean ratio differs between groups
  t_res <- t.test(as.formula(formula_str), data = used)

  # Calculate Fold Shift
  mean_grp1 <- mean(used[used[[group]] == group1, ratio])
  mean_grp2 <- mean(used[used[[group]] == group2, ratio])
  fold_shift <- mean_grp1 / mean_grp2

  values_grp1 <- used[used[[group]] == group1, ratio]
  values_grp2 <- used[used[[group]] == group2, ratio]
  n_grp1 <- length(values_grp1)
  n_grp2 <- length(values_grp2)
  if (n_grp1 > 0 && n_grp2 > 0) {
    ranks <- rank(c(values_grp1, values_grp2), ties.method = "average")
    u_grp1 <- sum(ranks[seq_len(n_grp1)]) - n_grp1 * (n_grp1 + 1) / 2
    auc <- u_grp1 / (n_grp1 * n_grp2)
  } else {
    auc <- NA
  }
  shift_call <- !is.na(auc) && !is.na(fold_shift) &&
    ((auc >= 0.9 && fold_shift >= 1.5) || (auc <= 0.1 && fold_shift <= 0.667))

  sd_grp1 <- sd(used[used[[group]] == group1, ratio])
  sd_grp2 <- sd(used[used[[group]] == group2, ratio])
  # Consistency Across Replicates
  cv_grp1 <- (sd_grp1 / mean_grp1) * 100
  cv_grp2 <- (sd_grp2 / mean_grp2) * 100
  # Established Benchmarks: Most scientific literature and manufacturer protocols use percentage thresholds.
  # Good: < 10%
  # Acceptable: 10%-20%
  # High Noise: 25%
  if (F) { # for debug only
    # 4. Results Summary
    cat("--- Statistical Summary ---\n")
    cat("P-value:", t_res$p.value, "\n")
    cat("Fold Shift:", round(fold_shift, 3), "\n")
    cat("AUC:", round(auc, 3), "\n")
    cat("CV% -", group1, " :", round(cv_grp1, 3), "%\n")
    cat("CV% -", group2, " :", round(cv_grp2, 3), "%\n")
  }

  return(list(comp = comp, Shift_FC = fold_shift, AUC = auc, Shift_PVAL = t_res$p.value, mean_group1 = mean_grp1, mean_group2 = mean_grp2, CV_group1 = cv_grp1, CV_group2 = cv_grp2, SHIFT = shift_call))
}
