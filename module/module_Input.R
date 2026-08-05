library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyalert)
library(shinyjs)
library(plotly)

# =============================================================================
# Input Module  –  Step 1
# UI:     InputUI(id)
# Server: InputServer(id, tables, projSpace, projDir, editMetaFile, matIdFile,
#                     metaFile1, countFile1, metaExample1, countExample1,
#                     metaFile2, countFile2,
#                     metaFile3, countFile3)
#         Side-effects: populates tables$counts, tables$meta, tables$log2CPM …
#         Returns:      nothing (all state via `tables` reactiveValues)
# =============================================================================

#' Input UI
#'
#' Executes InputUI.
#'
#' @param id Argument for InputUI.
#'
#' @return Return value produced by InputUI.
InputUI <- function(id) {
  ns <- NS(id)
  tagList(
    # ---- Example buttons -------------------------------------------------
    fluidRow(
      tags$style(HTML("
      .btn-i1{
      background-color: #2E5266;
      color: white;
      }
      .btn-i2{
      background-color: #6E8898;
      color: white;
      }
      .btn-i3{
      background-color: #5D737E;
      color: white;
      }
      ")),
      box(
        width = 12, title = "Example ?", solidHeader = TRUE,
        closable = FALSE, collapsible = TRUE, status = "primary",
        actionButton(ns("btnExample1"), "Try example 1 [with annotation]",
          icon = icon("check"), class = "btn-i1",
        ),
        actionButton(ns("btnExample2"), "Try example 2 [no annotation]",
          icon = icon("check"), class = "btn-i2"
        ),
        actionButton(ns("btnExample3"), "Try example 3 [with annotation & ERCC counts]",
          icon = icon("check"), class = "btn-i3"
        )
      )
    ),
    # ---- Upload box ------------------------------------------------------
    fluidRow(
      box(
        width = 12, title = "Upload files", solidHeader = TRUE,
        closable = FALSE, collapsible = TRUE, status = "primary",
        fluidRow(
          column(6, fileInput(ns("file1"), "Choose expression matrix file",
            multiple = FALSE,
            accept = c("text/tsv", "text/tab-separated-values,text/plain", ".tsv")
          )),
          column(6, actionButton(ns("btnCreateMetaFromMatrix"),
            "Create metadata From Matrix in QuickFile",
            icon  = icon("pen-to-square"),
            style = "margin-top:25px;margin-bottom:25px",
            class = "btn-warning"
          ))
        ),
        fluidRow(
          column(6, fileInput(ns("file2"), "Choose meta data file",
            multiple = FALSE,
            accept = c("text/tsv", "text/tab-separated-values,text/plain", ".tsv")
          )),
          column(6, actionButton(ns("btnLoadMetaOnServer"),
            "Edit Metadata in QuickFile",
            icon  = icon("pen-to-square"),
            style = "margin-top:25px;margin-bottom:25px",
            class = "btn-warning"
          ))
        ),
        fluidRow(
          column(3, radioButtons(ns("sep"), "Separator",
            choices  = c(Tab = "\t", Comma = ",", Semicolon = ";"),
            selected = "\t"
          )),
          column(3, radioButtons(ns("quote"), "Quote",
            choices  = c(None = "", "Double Quote" = '"', "Single Quote" = "'"),
            selected = ""
          )),
          column(3, radioButtons(ns("disp"), "Display",
            choices  = c(Head = "head", More = "more", None = "none"),
            selected = "head"
          ))
        ),
        fluidRow(
          column(6, checkboxGroupInput(ns("speciesChoice"), "Select species",
            choices = c(
              "human" = "Homo sapiens",
              "mouse" = "Mus musculus",
              "other" = "Other"
            ),
            selected = character(0),
            inline = TRUE
          ))
        ),
        fluidRow(
          column(12, checkboxInput(ns("chkGeneFilter"),
            "Exclude non-coding genes and pseudogenes",
            value = TRUE
          ))
        ),
        fluidRow(
          column(12, actionButton(ns("btnValidateInputs"), "Validate Input",
            icon = icon("check"),
            class = "btn-warning",
            style = "margin-bottom:15px"
          ))
        ),
        tags$br(),
        fluidRow(
          box(
            id = ns("inputSummaryBox"), width = 12, title = "Input Summary",
            solidHeader = TRUE, closable = FALSE, collapsible = TRUE,
            collapsed = TRUE, status = "primary",
            htmlOutput(ns("info1"))
          )
        ),
        tags$hr(),
        fluidRow(
          box(
            width = 12, title = "Preview - matrix", solidHeader = FALSE,
            closable = TRUE, collapsible = TRUE, status = "info",
            div(style = "overflow-x: scroll", tableOutput(ns("contentsMat")))
          )
        ),
        uiOutput(ns("erccPreviewBox")),
        fluidRow(
          box(
            width = 12, title = "Preview - meta data", solidHeader = FALSE,
            closable = TRUE, collapsible = TRUE, status = "info",
            div(style = "overflow-y: scroll", tableOutput(ns("contentsMeta")))
          )
        )
      )
    )
  )
}

#' Reset Analysis State
#'
#' Executes ResetAnalysisState.
#'
#' @param tables Argument for ResetAnalysisState.
#' @param removeOutputDirs Argument for ResetAnalysisState.
#'
#' @return Return value produced by ResetAnalysisState.
ResetAnalysisState <- function(tables, removeOutputDirs = FALSE) {
  if (isTRUE(removeOutputDirs) && !is.null(tables$outdirs) && length(tables$outdirs) > 0) {
    old_dirs <- unique(as.character(unlist(tables$outdirs, use.names = FALSE)))
    old_dirs <- old_dirs[!is.na(old_dirs) & nzchar(old_dirs) & file.exists(old_dirs)]
    if (length(old_dirs) > 0) {
      unlink(old_dirs, recursive = TRUE, force = TRUE)
    }
  }

  tables$inputValidated <- FALSE
  tables$analysisComplete <- FALSE
  tables$comp <- NULL
  tables$prefix <- NULL
  tables$currentOutDir <- NULL
  tables$outdirs <- NULL
  tables$DEG <- NULL
  tables$DEG_TMM <- NULL
  tables$meta_used <- NULL
  tables$meta_used_TMM <- NULL
  tables$numDEGs <- NULL
  tables$topnDEGs <- NULL
  tables$erccStat <- NULL
  tables$erccCV <- NULL
  tables$erccShift <- NULL
  tables$erccStat_DE <- NULL
  tables$erccDecision <- NULL
  tables$erccMode <- FALSE
  tables$log2CPM <- NULL
  tables$norm.data <- NULL
  tables$libSizePlot <- NULL
  tables$rlePlot <- NULL
  tables$species <- "Not validated"
  tables$inputValidationNotes <- NULL
  tables$inputValidationNextStep <- NULL
  tables$inputValidationWarnings <- NULL
  invisible(NULL)
}

#' Input Server
#'
#' Executes InputServer.
#'
#' @param id Argument for InputServer.
#' @param tables Argument for InputServer.
#' @param projSpace Argument for InputServer.
#' @param editMetaFile Argument for InputServer.
#' @param matIdFile Argument for InputServer.
#' @param metaFile1 Argument for InputServer.
#' @param countFile1 Argument for InputServer.
#' @param metaFile2 Argument for InputServer.
#' @param countFile2 Argument for InputServer.
#' @param metaFile3 Argument for InputServer.
#' @param countFile3 Argument for InputServer.
#' @param switchToQuickFile Argument for InputServer.
#' @param switchToWorkflowTab Argument for InputServer.
#'
#' @return Return value produced by InputServer.
InputServer <- function(id, tables, projSpace,
                        editMetaFile, matIdFile,
                        metaFile1, countFile1,
                        metaFile2, countFile2,
                        metaFile3, countFile3,
                        switchToQuickFile = NULL,
                        switchToWorkflowTab = NULL) {
  moduleServer(id, function(input, output, session) {
    #' Flash Message
    #'
    #' Executes flashMessage.
    #'
    #' @param title Argument for flashMessage.
    #' @param text Argument for flashMessage.
    #' @param type Argument for flashMessage.
    #' @param timer Argument for flashMessage.
    #' @param html Argument for flashMessage.
    #' @param closeOnEsc Argument for flashMessage.
    #' @param closeOnClickOutside Argument for flashMessage.
    #'
    #' @return Return value produced by flashMessage.
    flashMessage <- function(title, text, type = "error", timer = 0,
                             html = FALSE, closeOnEsc = FALSE,
                             closeOnClickOutside = FALSE) {
      theme_cols <- getOption("shinyRNAseq.theme.colors", list())
      btnCol <- switch(type,
        "success" = if (!is.null(theme_cols$success)) theme_cols$success else "#73A839",
        "error"   = if (!is.null(theme_cols$danger)) theme_cols$danger else "#C71C22",
        "warning" = if (!is.null(theme_cols$warning)) theme_cols$warning else "#DD5600",
        if (!is.null(theme_cols$info)) theme_cols$info else "#5bc0de"
      )
      shinyalert::shinyalert(
        title = title,
        text = text, type = type,
        timer = timer,
        html = html,
        closeOnEsc = closeOnEsc,
        closeOnClickOutside = closeOnClickOutside,
        confirmButtonText = "OK",
        confirmButtonCol = btnCol
      )
    }

    #' Has Non Zero ERCC
    #'
    #' Executes hasNonZeroERCC.
    #'
    #' @param countsERCC Argument for hasNonZeroERCC.
    #'
    #' @return Return value produced by hasNonZeroERCC.
    hasNonZeroERCC <- function(countsERCC) {
      if (is.null(countsERCC) || nrow(countsERCC) == 0) {
        return(FALSE)
      }
      firstCol <- tolower(colnames(countsERCC)[1])
      if (firstCol == "gene") {
        vals <- countsERCC[, -1, drop = FALSE]
      } else {
        vals <- countsERCC
      }
      valsNum <- suppressWarnings(as.matrix(data.frame(lapply(vals, as.numeric), check.names = FALSE)))
      if (length(valsNum) == 0) {
        return(FALSE)
      }
      any(is.finite(valsNum) & valsNum > 0)
    }

    #' Sync ERCCCheckbox
    #'
    #' Executes syncERCCCheckbox.
    #'
    #' @param rawCounts Argument for syncERCCCheckbox.
    #' @param countsERCC Argument for syncERCCCheckbox.
    #' @param notify Argument for syncERCCCheckbox.
    #'
    #' @return Return value produced by syncERCCCheckbox.
    syncERCCCheckbox <- function(rawCounts = NULL, countsERCC = NULL, notify = TRUE) {
      hasERCCSignal <- FALSE

      if (!is.null(countsERCC)) {
        hasERCCSignal <- hasNonZeroERCC(countsERCC)
      } else if (!is.null(rawCounts) && nrow(rawCounts) > 0) {
        lowerNames <- tolower(colnames(rawCounts))
        geneCol <- which(lowerNames %in% c("genesymbol", "gene", "symbol"))[1]
        if (is.na(geneCol)) geneCol <- 1L
        geneVals <- rawCounts[[geneCol]]
        isERCC <- grepl("^ERCC-", as.character(geneVals))

        if (any(isERCC)) {
          dataCols <- setdiff(
            seq_along(rawCounts),
            c(geneCol, which(lowerNames %in% c("geneid", "biotype", "annotationlevel")))
          )
          erccCounts <- rawCounts[isERCC, dataCols, drop = FALSE]
          erccNums <- suppressWarnings(as.matrix(data.frame(lapply(erccCounts, as.numeric), check.names = FALSE)))
          hasERCCSignal <- length(erccNums) > 0 && any(is.finite(erccNums) & erccNums > 0)
        }
      }

      if (hasERCCSignal) {
        tables$erccDetected <- TRUE
        if (isTRUE(notify)) {
          flashMessage(
            "ERCC Counts Detected",
            "ERCC spike-in counts detected.",
            type = "info",
            timer = 3000
          )
        }
      } else {
        tables$erccDetected <- FALSE
        tables$erccDecision <- NULL
        tables$erccMode <- FALSE
        tables$counts_ERCC <- NULL
      }

      invisible(hasERCCSignal)
    }

    # Tracks whether data was loaded from an example or a user upload.
    inputSource <- reactiveVal("example")

    # User-selected species is validated after count and metadata validation.
    tables$species <- "Not validated"
    tables$inputValidationNotes <- NULL
    tables$inputValidationNextStep <- NULL
    tables$inputValidationWarnings <- NULL

    speciesChoicePrevious <- reactiveVal(character(0))

    #' Set Species Choice
    #'
    #' Executes setSpeciesChoice.
    #'
    #' @param species Argument for setSpeciesChoice.
    #'
    #' @return Return value produced by setSpeciesChoice.
    setSpeciesChoice <- function(species = character(0)) {
      updateCheckboxGroupInput(session, "speciesChoice", selected = species)
      speciesChoicePrevious(as.character(species))
    }

    #' Selected Species
    #'
    #' Executes selectedSpecies.
    #'
    #' @return Return value produced by selectedSpecies.
    selectedSpecies <- function() {
      species <- as.character(input$speciesChoice)
      if (length(species) == 0 || is.na(species[1]) || !nzchar(species[1])) {
        return(NULL)
      }
      species[1]
    }

    #' Species Display Name
    #'
    #' Executes speciesDisplayName.
    #'
    #' @param species Argument for speciesDisplayName.
    #'
    #' @return Return value produced by speciesDisplayName.
    speciesDisplayName <- function(species) {
      if (identical(species, "Homo sapiens")) {
        return("Human")
      }
      if (identical(species, "Mus musculus")) {
        return("Mouse")
      }
      if (identical(species, "Other")) {
        return("Other")
      }
      species
    }

    #' Other Species Warning
    #'
    #' Executes otherSpeciesWarning.
    #'
    #' @return Return value produced by otherSpeciesWarning.
    otherSpeciesWarning <- function() {
      paste0(
        "Only Human and Mouse species can be reliably validated using geneID column. ",
        "If the species is 'Other' please use custom gmt file for the downstream enrichment analysis or skip it."
      )
    }

    #' Set Input Validation Notes
    #'
    #' Executes setInputValidationNotes.
    #'
    #' @return Return value produced by setInputValidationNotes.
    setInputValidationNotes <- function() {
      speciesLabel <- speciesDisplayName(tables$species)
      notes <- if (identical(tables$species, "Other")) {
        paste0("Species selected: ", speciesLabel)
      } else {
        paste0("Species selected: ", speciesLabel, " (Validation successful)")
      }
      warnings <- NULL
      if (isTRUE(tables$erccDetected)) {
        erccRows <- if (!is.null(tables$counts_ERCC)) nrow(tables$counts_ERCC) else 0
        notes <- c(
          notes,
          paste0(
            "ERCC counts detected: ", erccRows,
            " spike-in transcripts."
          )
        )
        nextStep <- "Please proceed to the QC section."
      } else {
        notes <- c(
          notes,
          "Input validation completed."
        )
        nextStep <- "Please proceed to the QC section."
      }
      if (identical(tables$species, "Other")) {
        warnings <- c(warnings, otherSpeciesWarning())
      }
      tables$inputValidationNotes <- notes
      tables$inputValidationNextStep <- nextStep
      tables$inputValidationWarnings <- warnings
      invisible(notes)
    }

    #' Format Input Validation Notes
    #'
    #' Executes formatInputValidationNotes.
    #'
    #' @param notes Argument for formatInputValidationNotes.
    #'
    #' @return Return value produced by formatInputValidationNotes.
    formatInputValidationNotes <- function(notes) {
      vapply(notes, function(note) {
        note <- as.character(note)
        prefix <- "Species selected: "
        successSuffix <- " (Validation successful)"
        if (startsWith(note, prefix)) {
          speciesLabel <- substring(note, nchar(prefix) + 1L)
          hasSuccess <- endsWith(speciesLabel, successSuffix)
          if (hasSuccess) {
            speciesLabel <- substring(
              speciesLabel, 1L,
              nchar(speciesLabel) - nchar(successSuffix)
            )
          }
          return(paste0(
            htmltools::htmlEscape(prefix),
            "<b>", htmltools::htmlEscape(speciesLabel), "</b>",
            if (hasSuccess) htmltools::htmlEscape(successSuffix) else ""
          ))
        }
        htmltools::htmlEscape(note)
      }, character(1), USE.NAMES = FALSE)
    }

    #' Open Input Summary Box
    #'
    #' Executes openInputSummaryBox.
    #'
    #' @return Return value produced by openInputSummaryBox.
    openInputSummaryBox <- function() {
      boxId <- gsub("\\\\", "\\\\\\\\", session$ns("inputSummaryBox"))
      boxId <- gsub("'", "\\\\'", boxId)
      shinyjs::runjs(sprintf(
        paste0(
          "var summary = $(document.getElementById('%s'));",
          "var box = summary.hasClass('box') || summary.hasClass('card') ? ",
          "summary : summary.closest('.box, .card');",
          "if (box.length && (box.hasClass('collapsed-box') || ",
          "box.hasClass('collapsed-card'))) {",
          "box.find('> .box-header [data-widget=\"collapse\"], ",
          "> .card-header [data-widget=\"collapse\"]').first().trigger('click');",
          "}"
        ),
        boxId
      ))
    }

    #' Clean Gene Values
    #'
    #' Executes cleanGeneValues.
    #'
    #' @param values Argument for cleanGeneValues.
    #'
    #' @return Return value produced by cleanGeneValues.
    cleanGeneValues <- function(values) {
      values <- trimws(as.character(values))
      values <- values[!is.na(values) & nzchar(values)]
      values[!grepl("^ERCC-", values)]
    }

    #' Score Gene Id Species
    #'
    #' Executes scoreGeneIdSpecies.
    #'
    #' @param values Argument for scoreGeneIdSpecies.
    #'
    #' @return Return value produced by scoreGeneIdSpecies.
    scoreGeneIdSpecies <- function(values) {
      genes <- cleanGeneValues(values)
      if (length(genes) == 0) {
        return(NULL)
      }
      genes <- genes[seq_len(min(length(genes), 3000))]
      list(
        human_rate = mean(grepl("^ENSG[0-9]+", genes)),
        mouse_rate = mean(grepl("^ENSMUSG[0-9]+", genes)),
        n = length(genes)
      )
    }

    #' Score Gene Symbol Species
    #'
    #' Executes scoreGeneSymbolSpecies.
    #'
    #' @param values Argument for scoreGeneSymbolSpecies.
    #'
    #' @return Return value produced by scoreGeneSymbolSpecies.
    scoreGeneSymbolSpecies <- function(values) {
      symbols <- cleanGeneValues(values)
      symbols <- symbols[!grepl("^ENS(MUS)?G[0-9]+", symbols)]
      if (length(symbols) == 0) {
        return(NULL)
      }
      symbols <- symbols[seq_len(min(length(symbols), 3000))]

      letters <- gsub("[^A-Za-z]", "", symbols)
      letters <- letters[nzchar(letters)]
      if (length(letters) == 0) {
        return(NULL)
      }

      firstLetters <- substr(letters, 1, 1)
      allUpper <- letters == toupper(letters)
      firstUpper <- firstLetters == toupper(firstLetters)
      list(
        human_rate = mean(allUpper),
        mouse_rate = mean(firstUpper & !allUpper),
        n = length(letters)
      )
    }

    #' Infer Species from Counts
    #'
    #' Executes inferSpeciesFromCounts.
    #'
    #' @param counts Argument for inferSpeciesFromCounts.
    #'
    #' @return Return value produced by inferSpeciesFromCounts.
    inferSpeciesFromCounts <- function(counts) {
      unknown <- list(species = "Unknown", method = "none", canValidate = FALSE)
      if (is.null(counts) || nrow(counts) == 0) {
        return(unknown)
      }

      lowerNames <- tolower(colnames(counts))
      idCols <- c(
        which(lowerNames %in% c(
          "geneid", "gene_id", "ensembl",
          "ensemblid", "ensembl_gene_id"
        )),
        which(lowerNames == "gene"),
        1L
      )
      idCols <- unique(idCols[idCols >= 1L & idCols <= ncol(counts)])

      bestId <- NULL
      bestIdScore <- -Inf
      for (col in idCols) {
        score <- scoreGeneIdSpecies(counts[[col]])
        if (is.null(score)) next
        combined <- score$human_rate + score$mouse_rate
        if (combined > bestIdScore) {
          bestId <- score
          bestIdScore <- combined
        }
      }

      if (!is.null(bestId)) {
        if (bestId$human_rate >= 0.1 && bestId$human_rate > bestId$mouse_rate) {
          return(list(species = "Homo sapiens", method = "gene_id", canValidate = TRUE))
        }
        if (bestId$mouse_rate >= 0.1 && bestId$mouse_rate > bestId$human_rate) {
          return(list(species = "Mus musculus", method = "gene_id", canValidate = TRUE))
        }
      }

      symbolCols <- c(
        which(lowerNames %in% c(
          "genesymbol", "symbol",
          "gene_name", "genename"
        )),
        which(lowerNames == "gene"),
        1L
      )
      symbolCols <- unique(symbolCols[symbolCols >= 1L & symbolCols <= ncol(counts)])

      bestSymbol <- NULL
      for (col in symbolCols) {
        bestSymbol <- scoreGeneSymbolSpecies(counts[[col]])
        if (!is.null(bestSymbol)) break
      }

      if (!is.null(bestSymbol)) {
        if (bestSymbol$human_rate >= 0.5 && bestSymbol$human_rate > bestSymbol$mouse_rate) {
          return(list(species = "Homo sapiens", method = "gene_symbol", canValidate = TRUE))
        }
        if (bestSymbol$mouse_rate >= 0.5 && bestSymbol$mouse_rate > bestSymbol$human_rate) {
          return(list(species = "Mus musculus", method = "gene_symbol", canValidate = TRUE))
        }
        return(list(species = "Unknown", method = "gene_symbol", canValidate = FALSE))
      }

      unknown
    }

    #' Species Evidence Label
    #'
    #' Executes speciesEvidenceLabel.
    #'
    #' @param method Argument for speciesEvidenceLabel.
    #'
    #' @return Return value produced by speciesEvidenceLabel.
    speciesEvidenceLabel <- function(method) {
      if (identical(method, "gene_id")) {
        return("Ensembl gene IDs")
      }
      if (identical(method, "gene_symbol")) {
        return("gene-symbol casing")
      }
      "available gene identifiers"
    }

    #' Validate Species Selection
    #'
    #' Executes validateSpeciesSelection.
    #'
    #' @param counts Argument for validateSpeciesSelection.
    #' @param rawCounts Argument for validateSpeciesSelection.
    #'
    #' @return Return value produced by validateSpeciesSelection.
    validateSpeciesSelection <- function(counts, rawCounts = NULL) {
      selected <- selectedSpecies()
      if (is.null(selected)) {
        tables$species <- "Not validated"
        flashMessage("Select Species",
          "Please select human, mouse or other before continuing.",
          type = "warning"
        )
        return(FALSE)
      }

      dataToCheck <- if (!is.null(rawCounts)) rawCounts else counts
      detected <- inferSpeciesFromCounts(dataToCheck)

      if (identical(selected, "Other")) {
        if (identical(detected$method, "gene_id") &&
          detected$species %in% c("Homo sapiens", "Mus musculus")) {
          tables$species <- "Not validated"
          flashMessage(
            "Species Mismatch",
            paste0(
              "Selected species is other, but the expression matrix looks like ",
              detected$species, " based on Ensembl gene IDs."
            ),
            type = "error"
          )
          return(FALSE)
        }
        tables$species <- selected
        return(TRUE)
      }

      if (!isTRUE(detected$canValidate) || identical(detected$species, "Unknown")) {
        tables$species <- "Not validated"
        flashMessage(
          "Species Validation Failed",
          paste0(
            "Selected species is ", selected,
            ", but species could not be validated from ",
            speciesEvidenceLabel(detected$method), "."
          ),
          type = "error"
        )
        return(FALSE)
      }

      if (!identical(detected$species, selected)) {
        tables$species <- "Not validated"
        flashMessage(
          "Species Mismatch",
          paste0(
            "Selected species is ", selected,
            ", but the expression matrix looks like ", detected$species,
            " based on ", speciesEvidenceLabel(detected$method), "."
          ),
          type = "error"
        )
        return(FALSE)
      }

      tables$species <- selected
      TRUE
    }

    #' Complete Input Validation
    #'
    #' Executes completeInputValidation.
    #'
    #' @return Return value produced by completeInputValidation.
    completeInputValidation <- function() {
      if (!isTRUE(tables$exprStatus) || is.null(tables$counts)) {
        return(invisible(FALSE))
      }
      if (!isTRUE(tables$metaStatus) || !isTRUE(tables$metaValid) || is.null(tables$meta_all)) {
        return(invisible(FALSE))
      }

      if (!validateSpeciesSelection(tables$counts, rawCounts = tables$rawCounts)) {
        return(invisible(FALSE))
      }

      # ERCC datasets can enter QC, but Analyze remains locked until the user
      # chooses ERCC or TMM normalization in the ERCC QC panel.
      if (isTRUE(tables$erccDetected)) {
        tables$erccDecision <- "pending"
        tables$erccMode <- FALSE
      } else {
        tables$erccDecision <- NULL
        tables$erccMode <- FALSE
      }

      tables$inputValidated <- TRUE
      setInputValidationNotes()
      openInputSummaryBox()
      if (identical(tables$species, "Other")) {
        flashMessage("Species Warning", otherSpeciesWarning(), type = "warning")
      }
      invisible(TRUE)
    }

    observeEvent(input$speciesChoice, ignoreInit = TRUE, {
      selected <- as.character(input$speciesChoice)
      previous <- speciesChoicePrevious()
      if (length(selected) > 1) {
        latest <- setdiff(selected, previous)
        selected <- if (length(latest) > 0) latest[length(latest)] else selected[length(selected)]
        updateCheckboxGroupInput(session, "speciesChoice", selected = selected)
      }
      speciesChoicePrevious(selected)
      tables$inputValidated <- FALSE
      tables$species <- "Not validated"
      tables$inputValidationNotes <- NULL
      tables$inputValidationNextStep <- NULL
      tables$inputValidationWarnings <- NULL
    })

    observeEvent(input$btnValidateInputs, {
      if (!isTRUE(tables$exprStatus) || is.null(tables$counts)) {
        flashMessage("Input Not Ready",
          "Please upload a valid expression matrix before validating input.",
          type = "warning"
        )
        return(invisible(FALSE))
      }
      if (!isTRUE(tables$metaStatus) || !isTRUE(tables$metaValid) || is.null(tables$meta_all)) {
        flashMessage("Input Not Ready",
          "Please upload valid metadata before validating input.",
          type = "warning"
        )
        return(invisible(FALSE))
      }
      completeInputValidation()
    })

    # ------------------------------------------------------------------
    # Internal helper: auto-read, validate and process a count file
    # ------------------------------------------------------------------
    #' Process Count File
    #'
    #' Executes processCountFile.
    #'
    #' @param countPath Argument for processCountFile.
    #' @param notifyERCC Argument for processCountFile.
    #'
    #' @return Return value produced by processCountFile.
    processCountFile <- function(countPath, notifyERCC = TRUE) {
      if (is.null(countPath) || !file.exists(countPath)) {
        return(invisible(FALSE))
      }
      ResetAnalysisState(tables)

      filterFlag <- isTRUE(input$chkGeneFilter)

      rawData <- ReadCount(countPath,
        fill = TRUE, check.names = FALSE,
        header = TRUE, sep = input$sep, quote = input$quote,
        filterFlag = filterFlag
      )
      # Keep an unfiltered endogenous matrix for ERCC QC parity with std script.
      rawData_unfiltered <- ReadCount(countPath,
        fill = TRUE, check.names = FALSE,
        header = TRUE, sep = input$sep, quote = input$quote,
        filterFlag = FALSE
      )
      counts <- rawData[["counts"]]
      counts_unfiltered <- rawData_unfiltered[["counts"]]
      counts_total <- rawData[["counts_total"]]
      annotation <- rawData[["annotation"]]
      counts_ERCC <- NULL
      # Keep ERCC matrix available for QC choice flow, even when TMM is selected.
      erccData <- tryCatch(
        ReadCountERCC(countPath, filterFlag = filterFlag, sep = input$sep, quote = input$quote),
        error = function(e) NULL
      )
      if (!is.null(erccData)) {
        erccCandidate <- erccData[["ERCC"]]
        if (!is.null(erccCandidate) && nrow(erccCandidate) > 0 && hasNonZeroERCC(erccCandidate)) {
          counts_ERCC <- erccCandidate
          # Match std ERCC workflow: QC baseline uses endogenous counts
          # produced by ReadCountERCC(filterFlag = FALSE), not ReadCount().
          erccData_unfiltered <- tryCatch(
            ReadCountERCC(countPath, filterFlag = FALSE, sep = input$sep, quote = input$quote),
            error = function(e) NULL
          )
          if (!is.null(erccData_unfiltered) && !is.null(erccData_unfiltered[["counts"]])) {
            counts_unfiltered <- erccData_unfiltered[["counts"]]
          }
        }
      }
      tables$erccMode <- FALSE
      tables$counts_ERCC <- counts_ERCC

      # Read raw data BEFORE validation so we can check original gene IDs
      rawCounts <- read.table(countPath,
        fill = TRUE,
        check.names = FALSE, stringsAsFactors = FALSE,
        row.names = NULL, header = TRUE,
        sep = input$sep, quote = input$quote
      )

      syncERCCCheckbox(rawCounts = rawCounts, countsERCC = counts_ERCC, notify = notifyERCC)

      if (!any(tolower(c("gene", "geneSymbol", "symbol")) %in% tolower(colnames(counts)))) {
        tables$exprStatus <- FALSE
        tables$counts <- NULL
        tables$counts_unfiltered <- NULL
        tables$rawCounts <- NULL
        tables$counts_total <- NULL
        tables$annotation <- NULL
        flashMessage("Invalid Expression Matrix", "Invalid expression matrix!", type = "error")
        return(invisible(FALSE))
      }

      tables$counts <- counts
      tables$counts_unfiltered <- counts_unfiltered
      tables$countFile <- countPath
      tables$rawCounts <- rawCounts
      tables$counts_total <- counts_total
      tables$exprStatus <- TRUE
      tables$annotation <- annotation
      output$contentsMat <- renderTable({
        if (input$disp == "head") {
          head(rawCounts)
        } else if (input$disp == "more") {
          head(rawCounts, n = 20)
        } else {
          NULL
        }
      })

      matIdFileStatus <- CreateMatIdFile(counts, matIdFile)
      if (matIdFileStatus) tables$matIdFile <- matIdFile

      if (isTRUE(tables$metaStatus) && !is.null(tables$rawMeta)) {
        rawMeta <- tables$rawMeta
        output$contentsMeta <- renderTable({
          if (input$disp == "none") NULL else rawMeta
        })
      }

      if (isTRUE(tables$metaStatus) && !is.null(tables$meta_all)) {
        tables$log2CPM <- DataTransform(counts,
          meta = tables$meta_all,
          norm.method = "log2CPM", total_count = NULL
        )
        tables$norm.data <- tables$log2CPM
        renderDiagnosticPlots(counts, tables$prefix)
      }

      invisible(TRUE)
    }

    # ------------------------------------------------------------------
    # Internal helper: load + validate metadata from a file path
    # ------------------------------------------------------------------
    #' Load Meta
    #'
    #' Executes loadMeta.
    #'
    #' @param metaFile Argument for loadMeta.
    #'
    #' @return Return value produced by loadMeta.
    loadMeta <- function(metaFile) {
      ResetAnalysisState(tables)
      tables$metaFile <- metaFile
      meta <- ReadMeta(metaFile,
        header = TRUE,
        sep = input$sep, quote = input$quote,
        allColors = standardColors()
      )
      usedMeta <- applyThemeColorsToMeta(meta[["used"]])
      rawMeta <- meta[["raw"]]

      if (isTRUE(meta[["valid"]]) &&
        all(c("ID", "NEWNAME", "GROUP", "COLOR") %in% colnames(usedMeta))) {
        tables$meta <- usedMeta
        tables$meta_all <- usedMeta
        tables$metaStatus <- TRUE
        tables$metaValid <- TRUE
        tables$rawMeta <- rawMeta
        groups <- unique(usedMeta$GROUP)
        tables$groups <- groups

        output$contentsMeta <- renderTable({
          if (input$disp == "none") NULL else rawMeta
        })

        return(invisible(TRUE))
      } else {
        tables$meta <- NULL
        tables$meta_all <- NULL
        tables$metaStatus <- FALSE
        tables$metaValid <- FALSE
        tables$rawMeta <- rawMeta
        tables$groups <- NULL
        flashMessage("Invalid Metadata", "Invalid meta data!", type = "error")
        return(invisible(FALSE))
      }
    }

    # ------------------------------------------------------------------
    # Internal helper: render lib-size + RLE plots into tables
    # ------------------------------------------------------------------
    #' Render Diagnostic Plots
    #'
    #' Executes renderDiagnosticPlots.
    #'
    #' @param counts Argument for renderDiagnosticPlots.
    #' @param prefix Argument for renderDiagnosticPlots.
    #'
    #' @return Return value produced by renderDiagnosticPlots.
    renderDiagnosticPlots <- function(counts, prefix) {
      outFile1 <- if (!is.null(prefix)) paste0(prefix, "_libSize.pdf") else NA
      outFile2 <- if (!is.null(prefix)) paste0(prefix, "_RLEplot.pdf") else NA

      tables$libSizePlot <- renderPlotly({
        fig <- ggplotly(
          PlotLibSize(counts = counts, meta = tables$meta_all, outFile = outFile1),
          width = 400, height = 400, tooltip = "text"
        )
        config(fig, toImageButtonOptions = list(
          format = "svg", filename = basename(paste0(prefix, "_libSize")),
          height = 400, width = 800, scale = 1
        ))
      })

      tables$rlePlot <- renderPlot({
        PlotRLE(logCPM = tables$log2CPM, meta = tables$meta_all, outFile = outFile2)
      })
    }

    # ------------------------------------------------------------------
    # Navigate to QuickFile tab
    # ------------------------------------------------------------------
    observeEvent(input$btnCreateMetaFromMatrix, {
      if (!is.null(switchToQuickFile)) switchToQuickFile("tabFileEditor")
    })
    observeEvent(input$btnLoadMetaOnServer, {
      if (!is.null(switchToQuickFile)) switchToQuickFile("tabFileEditor")
    })

    # ------------------------------------------------------------------
    # Example 1
    # ------------------------------------------------------------------
    observeEvent(input$btnExample1, {
      inputSource("example")
      ResetAnalysisState(tables)
      setSpeciesChoice("Homo sapiens")
      updateCheckboxInput(session, "chkGeneFilter", value = TRUE)

      rawData <- ReadCount(countFile1,
        fill = TRUE, check.names = FALSE,
        header = TRUE, sep = input$sep, quote = input$quote,
        filterFlag = input$chkGeneFilter
      )
      counts <- rawData[["counts"]]
      rawData_unfiltered <- ReadCount(countFile1,
        fill = TRUE, check.names = FALSE,
        header = TRUE, sep = input$sep, quote = input$quote,
        filterFlag = FALSE
      )
      counts_unfiltered <- rawData_unfiltered[["counts"]]
      annotation <- rawData[["annotation"]]
      rawCounts <- rawData[["raw"]]

      syncERCCCheckbox(rawCounts = rawCounts)

      tables$countFile <- countFile1
      tables$counts <- counts
      tables$counts_unfiltered <- counts_unfiltered
      tables$rawCounts <- rawCounts
      tables$exprStatus <- TRUE
      tables$annotation <- annotation

      output$contentsMat <- renderTable({
        if (input$disp == "head") {
          head(rawCounts)
        } else if (input$disp == "more") {
          head(rawCounts, n = 20)
        } else {
          NULL
        }
      })

      loadMeta(metaFile1)

      if (tables$metaStatus && tables$exprStatus) {
        tables$log2CPM <- DataTransform(counts,
          meta = tables$meta_all,
          norm.method = "log2CPM", total_count = NULL
        )
        tables$norm.data <- tables$log2CPM
        matIdFileStatus <- CreateMatIdFile(counts, matIdFile)
        if (matIdFileStatus) tables$matIdFile <- matIdFile
        renderDiagnosticPlots(counts, tables$prefix)
      }
    })

    # ------------------------------------------------------------------
    # Example 2
    # ------------------------------------------------------------------
    observeEvent(input$btnExample2, {
      inputSource("example")
      ResetAnalysisState(tables)
      setSpeciesChoice("Mus musculus")
      updateCheckboxInput(session, "chkGeneFilter", value = TRUE)

      rawData <- ReadCount(countFile2,
        fill = TRUE, check.names = FALSE,
        header = TRUE, sep = input$sep, quote = input$quote,
        filterFlag = input$chkGeneFilter
      )
      counts <- rawData[["counts"]]
      rawData_unfiltered <- ReadCount(countFile2,
        fill = TRUE, check.names = FALSE,
        header = TRUE, sep = input$sep, quote = input$quote,
        filterFlag = FALSE
      )
      counts_unfiltered <- rawData_unfiltered[["counts"]]
      annotation <- rawData[["annotation"]]
      rawCounts <- rawData[["raw"]]

      syncERCCCheckbox(rawCounts = rawCounts)

      tables$countFile <- countFile2
      tables$counts <- counts
      tables$counts_unfiltered <- counts_unfiltered
      tables$rawCounts <- rawCounts
      tables$exprStatus <- TRUE
      tables$annotation <- annotation

      output$contentsMat <- renderTable({
        if (input$disp == "head") {
          head(rawCounts)
        } else if (input$disp == "more") {
          head(rawCounts, n = 20)
        } else {
          NULL
        }
      })

      loadMeta(metaFile2)

      if (tables$metaStatus && tables$exprStatus) {
        tables$log2CPM <- DataTransform(counts,
          meta = tables$meta_all,
          norm.method = "log2CPM", total_count = NULL
        )
        tables$norm.data <- tables$log2CPM
        renderDiagnosticPlots(counts, tables$prefix)
      }
    })

    # ------------------------------------------------------------------
    # Example 3
    # ------------------------------------------------------------------
    observeEvent(input$btnExample3, {
      inputSource("example")
      ResetAnalysisState(tables)
      setSpeciesChoice("Mus musculus")
      updateCheckboxInput(session, "chkGeneFilter", value = TRUE)

      rawData <- ReadCountERCC(countFile3, filterFlag = TRUE, sep = "\t", quote = "")
      rawData_unfiltered <- ReadCountERCC(countFile3, filterFlag = FALSE, sep = "\t", quote = "")
      counts <- rawData[["counts"]]
      counts_unfiltered <- rawData_unfiltered[["counts"]]
      counts_ERCC <- rawData[["ERCC"]]
      annotation <- rawData[["annotation"]]

      # Read raw data for validation
      rawCounts <- read.delim(countFile3,
        header = TRUE, sep = "\t",
        check.names = FALSE, fill = TRUE
      )

      syncERCCCheckbox(countsERCC = counts_ERCC)

      tables$countFile <- countFile3
      tables$counts <- counts
      tables$counts_unfiltered <- counts_unfiltered
      tables$rawCounts <- rawCounts
      tables$exprStatus <- TRUE
      tables$annotation <- annotation
      tables$counts_ERCC <- counts_ERCC
      tables$erccMode <- FALSE

      # Raw preview (full file before splitting)
      rawAll <- read.delim(countFile3,
        header = TRUE, sep = "\t",
        check.names = FALSE, fill = TRUE, nrows = 20
      )
      output$contentsMat <- renderTable({
        if (input$disp == "head") {
          head(rawAll)
        } else if (input$disp == "more") {
          head(rawAll, n = 20)
        } else {
          NULL
        }
      })

      loadMeta(metaFile3)

      if (tables$metaStatus && tables$exprStatus) {
        tables$log2CPM <- DataTransform(counts,
          meta = tables$meta_all,
          norm.method = "log2CPM", total_count = NULL
        )
        tables$norm.data <- tables$log2CPM
        renderDiagnosticPlots(counts, tables$prefix)
      }
    })

    # ------------------------------------------------------------------
    # User-uploaded matrix – auto-process on upload.
    # ------------------------------------------------------------------
    observeEvent(input$file1$datapath, {
      req(input$file1)
      inputSource("user")
      setSpeciesChoice(character(0))
      tables$countFile <- input$file1$datapath
      tables$exprStatus <- FALSE
      processCountFile(input$file1$datapath)
    })

    # ------------------------------------------------------------------
    # Re-process when ERCC mode or gene-filter checkboxes change.
    # ------------------------------------------------------------------
    observeEvent(input$chkGeneFilter, ignoreInit = TRUE, {
      countPath <- tables$countFile
      if (is.null(countPath) || !file.exists(countPath)) {
        return()
      }
      processCountFile(countPath, notifyERCC = FALSE)
    })

    # ------------------------------------------------------------------
    # ERCC count preview (conditional on detected ERCC rows)
    # ------------------------------------------------------------------
    output$erccPreviewBox <- renderUI({
      req(isTRUE(tables$erccDetected), tables$counts_ERCC)
      ns <- session$ns
      fluidRow(
        box(
          width = 12, title = paste0(
            "Preview - ERCC spike-in counts (",
            nrow(tables$counts_ERCC), " transcripts)"
          ),
          solidHeader = FALSE, closable = TRUE, collapsible = TRUE, status = "success",
          div(
            style = "overflow-x: scroll",
            tableOutput(ns("contentsERCC"))
          )
        )
      )
    })

    output$contentsERCC <- renderTable(
      {
        req(isTRUE(tables$erccDetected), tables$counts_ERCC)
        ercc <- tables$counts_ERCC
        if (nrow(ercc) > 10) head(ercc, n = 10) else ercc
      },
      rownames = FALSE
    )

    # ------------------------------------------------------------------
    # User-uploaded metadata
    # ------------------------------------------------------------------
    observeEvent(input$file2$datapath, {
      req(input$file2)
      loadMeta(input$file2$datapath)
    })

    # ------------------------------------------------------------------
    # Status info panel (shared with timeline)
    # ------------------------------------------------------------------
    output$info1 <- renderUI({
      str1 <- if (tables$exprStatus) {
        paste(
          "Expression matrix: ready<br/>Dimensions:",
          nrow(tables$counts), "x", ncol(tables$counts) - 1
        )
      } else {
        "Expression matrix: not ready"
      }

      str2 <- if (tables$metaStatus) {
        paste(
          "Meta data: ready<br/>Dimensions:",
          nrow(tables$meta), "x", ncol(tables$meta) - 1
        )
      } else {
        "Meta data: not ready"
      }

      str3 <- if (isTRUE(tables$erccMode) && !is.null(tables$counts_ERCC)) {
        paste0(
          '<font color="#228B22"><b>ERCC mode: ',
          nrow(tables$counts_ERCC), " spike-in transcripts detected</b></font>"
        )
      } else {
        ""
      }

      str5 <- if (!is.null(tables$inputValidationNotes) &&
        length(tables$inputValidationNotes) > 0) {
        nextStep <- if (!is.null(tables$inputValidationNextStep) &&
          nzchar(tables$inputValidationNextStep)) {
          paste0(
            '<div style="margin-top:8px; color:navy;">',
            htmltools::htmlEscape(tables$inputValidationNextStep),
            "</div>"
          )
        } else {
          ""
        }
        paste0(
          '<div style="border-left:4px solid #73A839; padding:10px 12px; background:#f4fbf0;">',
          paste(formatInputValidationNotes(tables$inputValidationNotes), collapse = "<br/>"),
          nextStep,
          "</div>"
        )
      } else {
        ""
      }

      str6 <- if (!is.null(tables$inputValidationWarnings) &&
        length(tables$inputValidationWarnings) > 0) {
        paste0(
          '<div style="border-left:4px solid #DD5600; padding:10px 12px; background:#fff7ed;">',
          paste(htmltools::htmlEscape(tables$inputValidationWarnings), collapse = "<br/>"),
          "</div>"
        )
      } else {
        ""
      }

      parts <- c(str1, str2, str3, str5, str6)
      HTML(paste(parts[nzchar(parts)], collapse = "<br/><br/>"))
    })

    # ------------------------------------------------------------------
    # When both matrix and meta become ready, compute log2CPM and render
    # diagnostic plots.  This observer handles the manual-upload path;
    # the Example button handlers also call these directly.
    # ------------------------------------------------------------------
    observe({
      req(isTRUE(tables$metaStatus), isTRUE(tables$exprStatus))
      isolate({
        counts1 <- tables$counts
        total_count <- NULL
        if (!is.null(counts1) && !is.null(tables$meta_all)) {
          tables$log2CPM <- DataTransform(counts1,
            meta = tables$meta_all,
            norm.method = "log2CPM",
            total_count = total_count
          )
          renderDiagnosticPlots(counts1, tables$prefix)
        }
      })
    })

    # Expose  sep so parent/siblings can read them
    return(list(
      sep      = reactive(input$sep),
      quote    = reactive(input$quote),
      disp     = reactive(input$disp)
    ))
  })
}
