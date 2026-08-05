library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(shinyalert)
library(plotly)

# =============================================================================
# Analyze Module  –  Step 3
# UI:     AnalyzeUI(id)
# Server: AnalyzeServer(id, tables, projSpace, projDir, inputVals)
#         inputVals = list(sep, quote, disp) returned by InputServer
# =============================================================================

#' Analyze UI
#'
#' Executes AnalyzeUI.
#'
#' @param id Argument for AnalyzeUI.
#'
#' @return Return value produced by AnalyzeUI.
AnalyzeUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        width = 12, title = "Choose pairwise comparison",
        solidHeader = TRUE, closable = FALSE, collapsible = TRUE, status = "primary",
        selectInput(ns("Exp"), "Group1", choices = c("choose")),
        selectInput(ns("Ctl"), "Group2", choices = c("choose")),
        tags$hr(),
        disabled(actionButton(ns("btnVoom"),
          "Perform differential analysis",
          icon  = icon("check"),
          class = "btn-warning"
        ))
      )
    ),
    htmlOutput(ns("voom")),
    htmlOutput(ns("info2"))
  )
}

#' Analyze Server
#'
#' Executes AnalyzeServer.
#'
#' @param id Argument for AnalyzeServer.
#' @param tables Argument for AnalyzeServer.
#' @param projSpace Argument for AnalyzeServer.
#' @param projDir Argument for AnalyzeServer.
#' @param inputVals Argument for AnalyzeServer.
#'
#' @return Return value produced by AnalyzeServer.
AnalyzeServer <- function(id, tables, projSpace, projDir, inputVals) {
  moduleServer(id, function(input, output, session) {
    # ---- Sync group dropdowns when metadata loads -----------------------
    observe({
      req(tables$metaStatus, tables$groups)
      groups <- tables$groups
      updateSelectInput(session, "Exp", choices = groups, selected = groups[1])
      updateSelectInput(session, "Ctl", choices = groups, selected = groups[2])
    })

    # ---- Enable Voom button when inputs pass validation + ERCC choice ---
    observe({
      choiceReady <- (!isTRUE(tables$erccDetected)) ||
        (isTRUE(tables$erccDetected) && !identical(tables$erccDecision, "pending"))
      if (isTRUE(tables$inputValidated) && isTRUE(choiceReady)) {
        shinyjs::enable("btnVoom")
      } else {
        shinyjs::disable("btnVoom")
      }
    })

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

    #' Save Qc Outputs for Current Analysis
    #'
    #' Executes saveQcOutputsForCurrentAnalysis.
    #'
    #' @param comp Argument for saveQcOutputsForCurrentAnalysis.
    #'
    #' @return Return value produced by saveQcOutputsForCurrentAnalysis.
    saveQcOutputsForCurrentAnalysis <- function(comp = NULL) {
      if (is.null(tables$prefix) || !nzchar(as.character(tables$prefix))) {
        return(invisible(NULL))
      }

      qcDir <- tryCatch(
        {
          saveStandardQcBundle(
            counts = tables$counts,
            logCPM = tables$norm.data,
            meta = tables$meta_all,
            outPrefix = tables$prefix
          )
        },
        error = function(e) {
          cat("\n[AnalyzeServer]: Standard QC save failed:", conditionMessage(e), "\n")
          NULL
        }
      )

      if (isTRUE(tables$erccDetected) && !is.null(tables$counts_ERCC)) {
        ercc_log2cpm <- tryCatch(
          {
            calcErccQcLog2CPM(tables$counts, tables$counts_ERCC, tables$meta_all)
          },
          error = function(e) {
            cat("\n[AnalyzeServer]: ERCC-normalized QC matrix failed:", conditionMessage(e), "\n")
            NULL
          }
        )

        shift_for_qc <- tables$erccShift
        if (!is.null(comp) && nzchar(comp) && !is.null(tables$erccStat)) {
          shift_for_qc <- tryCatch(
            {
              ErccShiftTest(stat = tables$erccStat, comp = comp, ratio = "pct_ERCC", group = "GROUP")
            },
            error = function(e) {
              cat("\n[AnalyzeServer]: ERCC QC shift recompute failed:", conditionMessage(e), "\n")
              tables$erccShift
            }
          )
        }

        tryCatch(
          {
            saveErccQcBundle(
              stat = tables$erccStat,
              cv = tables$erccCV,
              shift = shift_for_qc,
              tmmLogCPM = tables$norm.data,
              erccLogCPM = ercc_log2cpm,
              meta = tables$meta_all,
              outPrefix = tables$prefix
            )
          },
          error = function(e) {
            cat("\n[AnalyzeServer]: ERCC QC save failed:", conditionMessage(e), "\n")
          }
        )
      }

      if (!is.null(qcDir) && !is.null(tables$logFile)) {
        logIt(tables$logFile,
          action = "Output",
          parameter = "QC_folder", value = file.path(basename(dirname(tables$prefix)), "QC")
        )
      }
      invisible(qcDir)
    }

    # ---- Status text -----------------------------------------------------
    output$info2 <- renderUI({
      if (any(is.null(c(input$Exp, input$Ctl))) ||
        "choose" %in% c(input$Exp, input$Ctl)) {
        HTML("Not chosen yet")
      } else {
        HTML(paste0(
          "You chose: <b>", input$Exp, "</b> vs <b>", input$Ctl, "</b>",
          "<br> [ <span style='color:navy'> <i>  will report genes that are upregulated/downregulated in <b>", input$Exp, "</b> with respect to <b>", input$Ctl, "</b> </i> </span> ] "
        ))
      }
    })

    # ---- Perform DE analysis --------------------------------------------
    observeEvent(input$btnVoom, {
      req(tables)

      # Show explicit running state on click to avoid repeat submissions.
      #' Restore Analyze Button
      #'
      #' Executes restoreAnalyzeButton.
      #'
      #' @return Return value produced by restoreAnalyzeButton.
      restoreAnalyzeButton <- function() {
        choiceReady <- (!isTRUE(tables$erccDetected)) ||
          (isTRUE(tables$erccDetected) && !identical(tables$erccDecision, "pending"))
        updateActionButton(session, "btnVoom",
          label = "Perform differential analysis",
          icon = icon("check")
        )
        if (isTRUE(tables$inputValidated) && isTRUE(choiceReady)) {
          shinyjs::enable("btnVoom")
        } else {
          shinyjs::disable("btnVoom")
        }
      }

      shinyjs::disable("btnVoom")
      updateActionButton(session, "btnVoom",
        label = "Running differential analysis...",
        icon = icon("spinner", class = "fa-spin")
      )
      on.exit(restoreAnalyzeButton(), add = TRUE)

      progress <- shiny::Progress$new(session = session, min = 0, max = 1)
      progress$set(
        message = "Performing differential analysis",
        detail = "Initializing...",
        value = 0
      )
      on.exit(progress$close(), add = TRUE)

      if (input$Exp == input$Ctl) {
        shinyalert("Oops!", "Please choose different groups for comparison.", type = "error", confirmButtonCol = "#C71C22")
        return()
      }

      comp <- paste0(input$Exp, "_vs_", input$Ctl)
      tables$comp <- comp
      progress$inc(0.08, detail = "Matching metadata to count matrix...")

      # Match meta to counts columns
      meta_kept <- tables$meta$ID %in% colnames(tables$counts)
      if (sum(meta_kept) < nrow(tables$meta) / 2) {
        # Fallback: fixed-string substring match, but only accept a 1-to-1 mapping
        cnt_cols <- colnames(tables$counts)
        mapping <- lapply(tables$meta$ID, function(mid) {
          which(grepl(mid, cnt_cols, fixed = TRUE))
        })
        # Reject any meta ID that matches zero or more than one count column (ambiguous)
        valid <- vapply(mapping, function(m) length(m) == 1L, logical(1))
        # Also reject any count column claimed by more than one meta ID (duplicate hit)
        hit_cols <- unlist(mapping[valid])
        unique_hits <- !duplicated(hit_cols) & !duplicated(hit_cols, fromLast = TRUE)
        valid[valid] <- unique_hits

        if (sum(valid) > sum(meta_kept)) {
          # Rename matched count columns to the canonical meta ID (safe: 1-to-1 confirmed)
          for (i in which(valid)) {
            colnames(tables$counts)[mapping[[i]]] <- tables$meta$ID[i]
          }
          meta_kept <- valid
        }
      }
      tables$meta <- tables$meta[meta_kept, ]

      msg <- if (sum(meta_kept) == 0) {
        "Sample IDs in metadata do not match the matrix."
      } else {
        paste0("Meta data: ", sum(meta_kept), " matched samples")
      }
      output$voom <- renderUI({
        HTML(msg)
      })

      # Guard: abort if no metadata samples matched the count matrix
      if (sum(meta_kept) == 0) {
        shinyalert("Sample Matching Error",
          paste0(
            "No samples in metadata matched the count matrix column names.\n\n",
            "Please verify:\n",
            "- Sample IDs in the metadata (ID column) exactly match column names ",
            "in the count matrix\n",
            "- File encoding and whitespace are correct"
          ),
          type = "error", confirmButtonCol = "#C71C22"
        )
        cat("\n[AnalyzeServer] ERROR: No metadata samples matched count matrix. Aborting.\n")
        return()
      }

      # Guard: each group in the chosen comparison must have >= 2 replicates
      progress$inc(0.12, detail = "Checking matched replicate counts...")
      comp_meta <- tables$meta[tables$meta$GROUP %in% c(input$Exp, input$Ctl), ]
      rep_counts <- table(comp_meta$GROUP)
      insufficient <- rep_counts[rep_counts < 2]
      if (length(insufficient) > 0) {
        detail <- paste(
          paste0("  - ", names(insufficient), " (n = ", as.integer(insufficient), ")"),
          collapse = "\n"
        )
        shinyalert("Insufficient Replicates",
          paste0(
            "The following group(s) have fewer than 2 matched replicates:\n",
            detail,
            "\n\nDifferential analysis requires at least 2 replicates per group."
          ),
          type = "error", confirmButtonCol = "#C71C22"
        )
        cat("\n[AnalyzeServer] ERROR: Insufficient replicates:\n")
        print(rep_counts)
        return()
      }
      cat("\n[AnalyzeServer]: Validation passed. Matched replicates per group:\n")
      print(rep_counts)

      # Create output directory
      progress$inc(0.15, detail = "Preparing output files and normalization...")
      outDir <- paste0(
        tables$projDir, "/", comp, "_",
        strtrim(gsub("[: ]", ".", Sys.time()), 19)
      )
      dir.create(outDir, showWarnings = FALSE)
      tables$outdirs <- c(tables$outdirs, outDir)
      tables$currentOutDir <- outDir

      # Rotate log file
      if (file.exists(tables$logFile) && length(tables$prefix) > 0) {
        file.copy(tables$logFile, dirname(tables$prefix), overwrite = TRUE)
      }
      uniStr <- paste0(strtrim(gsub("[: ]", ".", Sys.time()), 19), "_", RandomString(length = 10))
      tables$logFile <- paste0(tables$logDir, "/", uniStr, "_log.txt")

      # Normalisation
      cat("\n[AnalyzeServer]: generate log2CPM\n")
      tables$log2CPM <- DataTransform(tables$counts,
        meta = tables$meta_all,
        norm.method = "log2CPM",
        total_count = tables$counts_total
      )
      tables$norm.data <- tables$log2CPM
      tables$prefix <- paste0(outDir, "/RNAseq_", comp)

      outFile <- paste0(tables$prefix, "_log2CPM_diff.tsv")

      # GSEA support files
      progress$inc(0.20, detail = "Preparing GSEA support files...")
      clsFlag <- GenerateTMMnCLS4GSEA(tables$counts,
        meta = tables$meta,
        annotation = tables$annotation,
        prefix = tables$prefix
      )
      cat(if (clsFlag) "\nTMM + cls saved.\n" else "\nTMM + cls error.\n")

      # Run DE
      progress$inc(0.20, detail = "Running differential expression model...")
      tryCatch(
        {
          if (isTRUE(tables$erccMode) && !is.null(tables$counts_ERCC)) {
            # ERCC-normalised DE analysis
            cat("\n[AnalyzeServer]: Using ERCC normalisation for DE\n")

            # Prepare ERCC counts (strip gene column if present)
            ercc_for_de <- tables$counts_ERCC
            if (tolower(colnames(ercc_for_de)[1]) == "gene") {
              rn <- ercc_for_de[, 1]
              ercc_for_de <- ercc_for_de[, -1]
              rownames(ercc_for_de) <- rn
            }

            outFile_ercc <- paste0(tables$prefix, "_ERCC_log2CPM_diff.tsv")
            results <- RNAseqDE2(
              counts = tables$counts, annotation = tables$annotation,
              meta = tables$meta, comp = comp, cutoff_count = 0,
              cutoff_cpm = 0, outFile = outFile_ercc,
              logFile = tables$logFile, counts_ERCC = ercc_for_de
            )

            diff_table <- results[["DE"]]
            tables$DEG <- diff_table
            tables$meta_used <- results[["meta"]]
            canonical_ercc_stat <- results[["ErccStat"]]
            canonical_ercc_shift <- results[["ErccShift"]]

            # Align displayed and exported ERCC shift metrics with QC baseline
            # (unfiltered endogenous counts), matching stdRNAseq script behavior.
            if (!is.null(tables$erccStat) && is.data.frame(tables$erccStat)) {
              meta_ids <- qcMetaColumn(tables$meta_used, "ID")
              if (!is.null(meta_ids) && length(meta_ids) > 1) {
                stat_qc <- tables$erccStat[tables$erccStat$ID %in% meta_ids, , drop = FALSE]
                ord <- match(meta_ids, stat_qc$ID)
                ord <- ord[!is.na(ord)]
                if (length(ord) > 1) {
                  stat_qc <- stat_qc[ord, , drop = FALSE]
                  canonical_ercc_stat <- stat_qc
                  shift_qc <- tryCatch(
                    {
                      ErccShiftTest(stat = stat_qc, comp = comp, ratio = "pct_ERCC", group = "GROUP")
                    },
                    error = function(e) {
                      cat("\n[AnalyzeServer]: Using DE ERCC shift fallback:", conditionMessage(e), "\n")
                      NULL
                    }
                  )
                  if (!is.null(shift_qc)) {
                    canonical_ercc_shift <- shift_qc
                  }
                }
              }
            }

            tables$erccShift <- canonical_ercc_shift
            tables$erccStat_DE <- canonical_ercc_stat

            # Save ERCC QC stat table under QC/ for download.
            erccStatFile <- paste0(qcOutputPrefix(tables$prefix), "_ERCC_QC_stat_DE.txt")
            write.table(canonical_ercc_stat,
              file = erccStatFile,
              sep = "\t", col.names = TRUE, row.names = FALSE, quote = FALSE
            )
            cat("\n[AnalyzeServer]: ERCC QC stat saved:", basename(erccStatFile), "\n")

            logIt(tables$logFile,
              action = "Output",
              parameter = "ERCC_DEGs_table", value = basename(outFile_ercc)
            )

            # Also run standard TMM DE for side-by-side comparison
            cat("\n[AnalyzeServer]: Running parallel TMM (standard) DE for comparison\n")
            outFile_tmm <- paste0(tables$prefix, "_TMM_log2CPM_diff.tsv")
            tryCatch(
              {
                results_tmm <- RNAseqDE2(
                  counts = tables$counts, annotation = tables$annotation,
                  meta = tables$meta, comp = comp, cutoff_count = 0,
                  cutoff_cpm = 0, outFile = outFile_tmm,
                  logFile = tables$logFile, counts_ERCC = NULL
                )
                tables$DEG_TMM <- results_tmm[["DE"]]
                tables$meta_used_TMM <- results_tmm[["meta"]]
                cat("[AnalyzeServer]: TMM DE complete. Stored in tables$DEG_TMM\n")
                logIt(tables$logFile,
                  action = "Output",
                  parameter = "TMM_DEGs_table", value = basename(outFile_tmm)
                )
              },
              error = function(e) {
                cat("[AnalyzeServer]: TMM DE failed:", conditionMessage(e), "\n")
                tables$DEG_TMM <- NULL
              }
            )
          } else {
            # Standard DE analysis
            results <- RNAseqDE(
              counts = tables$counts, annotation = tables$annotation,
              meta = tables$meta, comp = comp, cutoff_count = 0,
              outFile = outFile, logFile = tables$logFile
            )

            tables$DEG <- results[["DEGs"]]
            tables$meta_used <- results[["meta"]]
            tables$DEG_TMM <- NULL
            tables$meta_used_TMM <- NULL
            tables$erccShift <- NULL
            tables$erccStat_DE <- NULL
          }

          resDEGs <- GetDEGs(tables$DEG,
            cutoff_logFC = 1,
            cutoff_Pval = 0.05, cutoff_FDR = 0.05, topn = 10
          )
          tables$numDEGs <- resDEGs[["numDEGs"]]
          tables$topnDEGs <- resDEGs[["topnDEGs"]]

          msg <- paste(msg, "Completed!", sep = "<br/>")
          output$voom <- renderUI({
            HTML(msg)
          })

          #' Sanitize for Rnk
          #'
          #' Executes sanitizeForRnk.
          #'
          #' @param df Argument for sanitizeForRnk.
          #'
          #' @return Return value produced by sanitizeForRnk.
          sanitizeForRnk <- function(df) {
            if (is.null(df) || !is.data.frame(df)) {
              return(df)
            }
            cols <- intersect(c("P.Value", "adj.P.Val", "log2FC", "t.Statistic"), colnames(df))
            for (cc in cols) {
              vv <- as.numeric(df[[cc]])
              if (cc == "P.Value") {
                vv[!is.finite(vv) | vv <= 0] <- .Machine$double.xmin
              } else if (cc %in% c("log2FC", "t.Statistic")) {
                vv[is.nan(vv)] <- NA_real_
              } else {
                vv[!is.finite(vv)] <- NA_real_
              }
              df[[cc]] <- vv
            }
            df
          }

          if (!isTRUE(tables$erccMode)) {
            rnkInput <- sanitizeForRnk(results[["DEGs"]])
            rnkFlag <- GenerateRank4GSEA(rnkInput,
              annotation = tables$annotation,
              prefix = tables$prefix
            )
            cat(if (rnkFlag) "\nrnk saved.\n" else "\nrnk error.\n")
          } else {
            rnkInput <- sanitizeForRnk(tables$DEG)
            rnkFlag <- GenerateRank4GSEA(rnkInput,
              annotation = tables$annotation,
              prefix = tables$prefix
            )
            cat(if (rnkFlag) "\nrnk saved.\n" else "\nrnk error.\n")
          }

          progress$inc(0.15, detail = "Summarizing DEGs and logging outputs...")

          # Log parameters
          logIt(tables$logFile, action = "Input", parameter = "matrix_file", value = tables$countFile)
          logIt(tables$logFile, action = "Input", parameter = "metadata_file", value = tables$metaFile)
          logIt(tables$logFile, action = "DE_setting", parameter = "cutoff_cpm", value = "0.1")
          logIt(tables$logFile, action = "DE_setting", parameter = "comparison", value = tables$comp)
          if (isTRUE(tables$erccMode)) {
            logIt(tables$logFile, action = "DE_setting", parameter = "normalization", value = "ERCC")
          } else {
            logIt(tables$logFile, action = "Output", parameter = "DEGs table", value = basename(outFile))
          }

          progress$inc(0.07, detail = "Saving QC outputs...")
          saveQcOutputsForCurrentAnalysis(comp = tables$comp)

          # Signal completion to parent (via tables flag)
          tables$analysisComplete <- TRUE
          progress$inc(0.03, detail = "Completed")
        },
        error = function(e) {
          cat("\n[AnalyzeServer] Error during DE analysis:", conditionMessage(e), "\n")
          shinyalert("Analysis Error",
            paste0(
              "Differential analysis failed:\n", conditionMessage(e),
              "\n\nPlease check that:\n",
              "- Each group has at least 2 replicates\n",
              "- Sample IDs in metadata match column names in the matrix\n",
              "- Group names in the comparison match those in the metadata"
            ),
            type = "error", confirmButtonCol = "#C71C22"
          )
        }
      )
    }) # end observeEvent(input$btnVoom)

    # Return the selected comparison inputs so Visualize can read them
    return(list(
      Exp      = reactive(input$Exp),
      Ctl      = reactive(input$Ctl)
    ))
  })
}
