library(shiny)
library(shinyalert)
library(shinydashboard)
library(shinydashboardPlus)
library(fresh)
library(shinyWidgets)
library(shinyjs)
library(shinydisconnect)
library(shinycssloaders)
library(DT)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(plotly)
library(heatmaply)
library(edgeR)
library(limma)
library(DESeq2)
library(stringr)
library(scales)
library(fontawesome)
library(reshape2)
library(markdown)
library(zip)

# source Function scripts 
source("module/RNAseq.Funcs.R")
source("module/RNAseq.ERCC.Func.R")
source("module/RNAseq.fgsea.Func.R")
source("module/RNAseq.enrichr.Func.R")

# source Modules
source("module/module_Analyze.R")
source("module/module_Documentation.R")
source("module/module_Download.R")
source("module/module_enrichr.R")
source("module/module_ERCC_Analyze.R")
source("module/module_ERCC_QC.R")
source("module/module_ERCC_Visualize.R")
source("module/module_Feedback.R")
source("module/module_fgsea.R")
source("module/module_FileEditor.R")
source("module/module_Input.R")
source("module/module_QC.R")
source("module/module_TableMerger.R")
source("module/module_Visualize.R")
source("www/theme-ui.R")

options(shiny.maxRequestSize = 100 * 1024^2)
options(stringsAsFactors = FALSE)

# Global constants 
projSpace <- getwd()

metaFile1 <- file.path("data", "Example1_meta2.txt")  
countFile1 <- file.path("data", "Example1_RSEM_count2.txt")  #simulated
metaExample1 <- tryCatch(read.table(metaFile1, fill=TRUE, check.names = FALSE, header = T, sep = "\t", quote = ""),
                         error = function(e) { message("Warning: ", metaFile1, " not found"); data.frame() })
countExample1 <- tryCatch(read.table(countFile1, fill=TRUE, check.names = FALSE, header = T,sep = "\t",quote = ""),
                          error = function(e) { message("Warning: ", countFile1, " not found"); data.frame() })

metaFile2 <- file.path("data", "Example2_sample_meta.txt")  
countFile2 <- file.path("data", "Example2_RSEM_count2.txt")  

metaExample2 <- tryCatch(read.table(metaFile2, fill=TRUE, check.names = FALSE, header = T,sep = "\t",quote = ""),
                         error = function(e) { message("Warning: ", metaFile2, " not found"); data.frame() })
countExample2 <- tryCatch(read.table(countFile2, fill=TRUE, check.names = FALSE, header = T,sep = "\t",quote = ""),
                          error = function(e) { message("Warning: ", countFile2, " not found"); data.frame() })

metaFile3 <- file.path("data", "Example3_sample_meta.txt")  
countFile3 <- file.path("data", "Example3_RSEM_with_ERCC_count.txt") 

metaExample3 <- tryCatch(read.table(metaFile3, fill=TRUE, check.names = FALSE, header = T,sep = "\t",quote = ""),
                         error = function(e) { message("Warning: ", metaFile3, " not found"); data.frame() })
countExample3 <- tryCatch(read.table(countFile3, fill=TRUE, check.names = FALSE, header = T,sep = "\t",quote = ""),
                          error = function(e) { message("Warning: ", countFile3, " not found"); data.frame() })

active_theme_name <- getOption("shinyRNAseq.theme", "fresh")
active_theme <- get_app_theme(active_theme_name)
options(shinyRNAseq.theme.colors = active_theme$colors)

# =========================================================================
# UI #
# =========================================================================
ui <- shinydashboardPlus::dashboardPage(
  skin = "blue",

  header = shinydashboardPlus::dashboardHeader(
    title = "RNAseq", titleWidth = 80
  ),

  sidebar = dashboardSidebar(
    sidebarMenu(id = "sidebar",
      tags$head(tags$script(src = "getIP.js")),
      menuItem("Workflow",      tabName = "standard",  icon = icon("dna")),
      menuItem("QuickFile",     tabName = "quickfile", icon = icon("table-list")),
      menuItem("Documentation", tabName = "docs",      icon = icon("question")),
      menuItem("Feedback",      tabName = "feedback",  icon = icon("comment"))
    )
  ),

  body = dashboardBody(
    use_app_theme(active_theme_name),
    theme = active_theme$body_theme,
    tabItems(

      # ================================================================
      # Workflow tab
      # ================================================================
      tabItem(tabName = "standard",
        fluidPage(
          disconnectMessage(
            text = paste0(
              "*** R session error. Check input file format and sample size per GROUP. ",
              "Both files must use the same separator. At least two replicates required ",
              "per GROUP. See [? Documentation] for details."),
            refresh        = "[Restart Now]",
            background     = "white", 
            top = 120, # distance away from top to message box
            width = 700, # width of message box
            colour = "grey10", 
            overlayColour  = "grey",
            overlayOpacity = 0.3,
            refreshColour  = "grey40"
          ),
          useShinyjs(),
          extendShinyjs(text = jscode, functions = c("disableTab", "enableTab")),
          inlineCSS(css),

          column(12,
            tabBox(
              id = "tabset1",
              title  = tagList(shiny::icon("dna"),
                               "RNAseq Differential Expression Analysis [Ver 3.0]"),
              height = "650px", width = "400",

              tabPanel(id = "tabInput",   title = "Step1. Input",     value = "tab1",
                       InputUI("input1")),

              tabPanel(id = "tabQC",      title = "Step2. QC",        value = "tab2",
                       QCUI("qc1"),
                       ERCC_QCUI("ercc_qc1")),

              tabPanel(id = "tabAnalyze", title = "Step3. Analyze",   value = "tab3",
                       AnalyzeUI("analyze1")),

              tabPanel(id = "tabViz",     title = "Step4. Visualize", value = "tab4",
                       fluidPage(ERCC_VisualizeUI("ercc_viz1"),
                                 VisualizeUI("viz1"))),

              tabPanel(id = "tabFgsea", title = "Step5. Enrichment", value = "tab5",
                fluidRow(
                   box(
                     title = "Analysis Info",
                    width = 12,
                    solidHeader = TRUE,
                    status = "primary",
                    collapsible = FALSE,
                  tags$div(
                  style = "margin: 8px 0 12px 0; padding: 8px 12px; border: 1px solid #d9e2ef; border-radius: 6px; background: #f8fbff; display:flex; flex-direction:column; gap:8px;",
                  tags$div(
                    tags$b("Species:"),
                    tags$span(style = "margin-left:6px;", textOutput("enrichment_species", inline = TRUE))
                  ),
                  tags$div(
                    tags$b("Current comparison:"),
                    tags$span(style = "margin-left:6px;", textOutput("enrichment_current_comp", inline = TRUE))
                  )
                ))),
                tags$style(HTML("
                  #tabset_enrichment > li > a {
                    font-size: 15px;
                    font-weight: bold;
                    padding: 10px 22px;
                    color: #444;
                  }
                  #tabset_enrichment > li.active > a,
                  #tabset_enrichment > li.active > a:focus,
                  #tabset_enrichment > li.active > a:hover {
                    background-color: #2c7bb6 !important;
                    color: #ffffff !important;
                    border-color: #2c7bb6 !important;
                    border-bottom-color: transparent !important;
                  }
                  #tabset_enrichment > li > a:hover {
                    background-color: #d0e8f8 !important;
                    color: #2c7bb6 !important;
                  }
                ")),
                tabsetPanel(
                  id = "tabset_enrichment",
                  tabPanel(
                    title = tags$span(icon("chart-bar"), " fGSEA"),
                    value = "tab_fgsea",
                    mod_fgsea_ui("fgsea1")
                  ),
                  tabPanel(
                    title = tags$span(icon("database"), " EnrichR"),
                    value = "tab_enrichr",
                    mod_enrichr_ui("enrichr1")
                  )
                )
              ),

              tabPanel(id = "tabSave",    title = "Step6. Download",  value = "tab6",
                       DownloadUI("download1"))
            )
          )
        )
      ),

      # ================================================================
      # QuickFile tab
      # ================================================================
      tabItem(tabName = "quickfile",
        tabsetPanel(id = "tabset2",
          tabPanel(title = "FileEditor", value = "tabFileEditor",
            fluidPage(column(12,
              fluidRow(box(width = 12, title = "Edit File", solidHeader = TRUE,
                           closable = FALSE, collapsible = TRUE, status = "primary",
                           fluidRow(column(12, mdlEditTableUI("editableTable1"))))),
              fluidRow(uiOutput("vizPrevMeta"))
            ))
          ),
          tabPanel(title = "TableMerger", value = "tabTableMerger",
            fluidPage(column(12,
              fluidRow(box(width = 12, title = "Need examples?", solidHeader = FALSE,
                           closable = TRUE, collapsible = TRUE, status = "warning",
                           fluidRow(column(12,
                             h4(tags$a(href = "https://en.wikipedia.org/wiki/Join_(SQL)",
                                       target = "_blank", "What are common join types?")),
                             tags$img(src = "merging.types.jpg", width = 550, height = 115),
                             tags$br()
                           )),
                           fluidRow(column(12,
                             actionButton("btnCreateMergerExample", "Try tables [example]",
                                          icon = icon("sync"), class = "btn-success")
                           ))
              )),
              fluidRow(box(width = 12, title = "Merge Files", solidHeader = TRUE,
                           closable = FALSE, collapsible = TRUE, status = "primary",
                           fluidRow(column(12, mdlMergeTableUI("mergeTables1")))
              ))
            ))
          )
        )
      ),

      # ================================================================
      # Documentation tab  (Manual + Privacy)
      # ================================================================
      tabItem(tabName = "docs",
        DocumentationUI("docs1")
      ),

      # ================================================================
      # Feedback tab
      # ================================================================
      tabItem(tabName = "feedback",
        FeedbackUI("feedback1")
      )
    )
  )
)

# =========================================================================
# Server
# =========================================================================
#' Server
#'
#' Executes server.
#'
#' @param input Argument for server.
#' @param output Argument for server.
#' @param session Argument for server.
#'
#' @return Return value produced by server.
server <- function(input, output, session) {
  cat("\nStarting Session...\n")

  # ---- Shared reactive state --------------------------------------------
  tables <- reactiveValues(
    exprStatus       = FALSE,
    metaStatus       = FALSE,
    metaValid        = FALSE,
    inputValidated   = FALSE,
    comp             = NULL,
    outdirs          = NULL,
    currentOutDir    = NULL,
    prefix           = NULL,
    analysisComplete = FALSE,
    PCA_title1       = "Principal Component Analysis (PCA)",
    erccMode         = FALSE,
    counts_ERCC      = NULL,
    erccStat         = NULL,
    erccCV           = NULL,
    erccShift        = NULL,
    erccStat_DE      = NULL
  )

  # ---- Per-session directories ------------------------------------------
  uniStr       <- paste0(strtrim(gsub("[: ]", ".", Sys.time()), 19), "_",
                          RandomString(length = 10))
  setwd(projSpace)

  projDir        <- file.path("data", uniStr)
  dir.create(projDir, showWarnings = FALSE, recursive = TRUE)
  tables$projDir <- projDir

  TMPDIR       <- file.path(projDir, "editedFiles")
  dir.create(TMPDIR, showWarnings = FALSE, recursive = TRUE)
  uid<- RandomString()
  editMetaFile <- paste0(TMPDIR, "/meta_", uid, "_", Sys.Date(), ".txt")
  matIdFile    <- paste0(TMPDIR, "/mat_ID_", uid, "_", Sys.Date(), ".txt")

  updir        <- normalizePath(file.path(projDir, "uploads"), mustWork = FALSE)
  dir.create(updir, showWarnings = FALSE, recursive = TRUE)

  qfdir        <- normalizePath(file.path(projDir, "QuickFiles"), mustWork = FALSE)
  dir.create(qfdir, showWarnings = FALSE, recursive = TRUE)

  logDir         <- file.path("data", "log")
  dir.create(logDir, showWarnings = FALSE)
  tables$logDir  <- logDir
  tables$logFile <- paste0(logDir, "/", uniStr, "_RNAseq_log.txt")

  # ---- Disable workflow tabs initially ----------------------------------
  js$disableTab("tab2")
  js$disableTab("tab3")
  js$disableTab("tab4")
  js$disableTab("tab5")
  js$disableTab("tab6")

  # ==========================================================================
  # Module wiring
  # ==========================================================================

  # QuickFile navigation callback used by Input module --------------------
  #' Switch to Quick File
  #'
  #' Executes switchToQuickFile.
  #'
  #' @param innerTab Argument for switchToQuickFile.
  #'
  #' @return Return value produced by switchToQuickFile.
  switchToQuickFile <- function(innerTab = "tabFileEditor") {
    updateTabItems(session, "sidebar", selected = "quickfile")
    updateTabsetPanel(session, "tabset2", selected = innerTab)
  }

  #' Switch to Workflow Tab
  #'
  #' Executes switchToWorkflowTab.
  #'
  #' @param tab Argument for switchToWorkflowTab.
  #'
  #' @return Return value produced by switchToWorkflowTab.
  switchToWorkflowTab <- function(tab = "tab2") {
    updateTabItems(session, "sidebar", selected = "standard")
    js$enableTab(tab)
    updateTabsetPanel(session, "tabset1", selected = tab)
  }

  # Step 1 – Input --------------------------------------------------------
  inputVals <- InputServer("input1",
                           tables            = tables,
                           projSpace         = projSpace,
                           editMetaFile      = editMetaFile,
                           matIdFile         = matIdFile,
                           metaFile1         = metaFile1,  countFile1 = countFile1,
                           metaFile2         = metaFile2,  countFile2 = countFile2,
                           metaFile3         = metaFile3,  countFile3 = countFile3,
                           switchToQuickFile = switchToQuickFile,
                           switchToWorkflowTab = switchToWorkflowTab)

  # Single observer covering all tab-3 enable/disable logic so there is no
  # ordering ambiguity between inputValidated, erccDetected, and erccDecision.
  observe({
    validated     <- isTRUE(tables$inputValidated)
    ercc_detected <- isTRUE(tables$erccDetected)
    decision      <- tables$erccDecision

    if (!validated) {
      js$disableTab("tab2")
      js$disableTab("tab3")
      js$disableTab("tab4")
      js$disableTab("tab5")
      js$disableTab("tab6")
    } else {
      js$enableTab("tab2")
      # Lock Analyze when ERCC data is present but no normalization chosen yet.
      if (ercc_detected && (is.null(decision) || !decision %in% c("ercc", "tmm"))) {
        js$disableTab("tab3")
      } else {
        js$enableTab("tab3")
      }
    }
  })

  # Auto-navigate to Analyze tab once the user picks a normalization method.
  observeEvent(tables$erccDecision, ignoreNULL = TRUE, ignoreInit = TRUE, {
    if (isTRUE(tables$erccDecision %in% c("ercc", "tmm"))) {
      updateTabsetPanel(session, "tabset1", selected = "tab3")
    }
  })

  # Step 2 – QC -----------------------------------------------------------
  QCServer("qc1", tables = tables)
  ERCC_QCServer("ercc_qc1", tables = tables)

  # Step 3 – Analyze ------------------------------------------------------
  analyzeVals <- AnalyzeServer("analyze1",
                               tables    = tables,
                               projSpace = projSpace,
                               projDir   = projDir,
                               inputVals = inputVals)

  observeEvent(tables$analysisComplete, {
    req(tables$analysisComplete)
    js$enableTab("tab4")
    js$enableTab("tab5")
    updateTabsetPanel(session, "tabset1", selected = "tab4")
    # Reset visualize checkboxes to avoid stale plots
    for (chk in c("chkAveExpr","chkPCA","chkVolcano","chkMAplot","chkBoxplot","chkHeatmap"))
      updateCheckboxInput(session, paste0("viz1-", chk), value = FALSE)
    js$enableTab("tab6")
    tables$analysisComplete <- FALSE
  })

  # Step 4 – Visualize ----------------------------------------------------
  visualizeVals <- VisualizeServer("viz1",
                                   tables      = tables,
                                   analyzeVals = analyzeVals)
  ERCC_VisualizeServer("ercc_viz1", tables = tables)

  # Step 5 – Enrichment (fGSEA + EnrichR) --------------------------------
  output$enrichment_current_comp <- renderText({
    comp <- tables$comp
    if (is.null(comp) || !nzchar(as.character(comp))) "No active comparison yet" else as.character(comp)
  })

  output$enrichment_species <- renderText({
    sp <- tables$species
    if (is.null(sp) || !nzchar(as.character(sp))) "Not set" else as.character(sp)
  })

  mod_fgsea_server("fgsea1", tables = tables, run_fgsea = run_fgsea)
  mod_enrichr_server("enrichr1", tables = tables)

  # Step 6 – Download -----------------------------------------------------
  DownloadServer("download1",
                 tables        = tables,
                 projSpace     = projSpace,
                 visualizeVals = visualizeVals)

  # QuickFile – FileEditor ------------------------------------------------
  my_meta <- reactive({
    req(tables$metaFile)
    read.table(tables$metaFile, sep = "\t", header = TRUE, fill = TRUE,
               stringsAsFactors = FALSE, quote = "", row.names = NULL,
               check.names = FALSE, comment.char = "#")
  })
  my_matId <- reactive({
    req(tables$matIdFile)
    read.table(tables$matIdFile, sep = "\t", header = TRUE, fill = TRUE,
               stringsAsFactors = FALSE, quote = "", row.names = NULL,
               check.names = FALSE, comment.char = "#")
  })

  #' Load Meta Callback
  #'
  #' Executes loadMetaCallback.
  #'
  #' @param metaFile Argument for loadMetaCallback.
  #'
  #' @return Return value produced by loadMetaCallback.
  loadMetaCallback <- function(metaFile) {
    ResetAnalysisState(tables)
    tables$metaFile <- metaFile
    meta     <- ReadMeta(metaFile, header = TRUE, sep = "\t", quote = "",
                         allColors = standardColors())
    usedMeta <- applyThemeColorsToMeta(meta[["used"]])
    if (isTRUE(meta[["valid"]]) &&
        all(c("ID","NEWNAME","GROUP","COLOR") %in% colnames(usedMeta))) {
      tables$meta       <- usedMeta
      tables$meta_all   <- usedMeta
      tables$metaStatus <- TRUE
      tables$metaValid  <- TRUE
      tables$groups     <- unique(usedMeta$GROUP)
    } else {
      tables$meta       <- NULL
      tables$meta_all   <- NULL
      tables$metaStatus <- FALSE
      tables$metaValid  <- FALSE
      tables$groups     <- NULL
    }
  }

  callModule(mdlEditTable, "editableTable1",
             inMeta   = my_meta,
             inMatId  = my_matId,
             metaFile = editMetaFile,
             CallFun  = loadMetaCallback)

  callModule(mdlMergeTable, "mergeTables1",
             updir   = updir,
             projdir = projDir,
             outdir  = qfdir)

  observeEvent(input$btnCreateMergerExample, {
    rawPath  <- file.path(projSpace, "datasets")
    if (!dir.exists(rawPath)) {
      rawPath <- file.path(projSpace, "data")
    }
    filelist <- dir(rawPath, pattern = "*.*", full.names = TRUE, recursive = FALSE)
    if (length(filelist) == 0) {
      showNotification("No example tables were found to copy.", type = "warning")
      return(invisible(FALSE))
    }
    file.copy(filelist, updir, overwrite = TRUE)
    callModule(mdlMergeTable, "mergeTables1", updir = updir, projdir = projDir, outdir = qfdir)
  })

  # Documentation ---------------------------------------------------------
  DocumentationServer("docs1",
                      projSpace    = projSpace,
                      metaExample1 = metaExample1, countExample1 = countExample1,
                      metaExample2 = metaExample2, countExample2 = countExample2,
                      metaExample3 = metaExample3, countExample3 = countExample3 )
  
  # Feedback --------------------------------------------------------------
  FeedbackServer("feedback1")

  # ---- Timeline sidebar widget ------------------------------------------
  output$timeline <- renderUI({
    tab <- input$tabset1
    #' Mk Item
    #'
    #' Executes mkItem.
    #'
    #' @param ttl Argument for mkItem.
    #' @param ic Argument for mkItem.
    #' @param time Argument for mkItem.
    #' @param ... Argument for mkItem.
    #'
    #' @return Return value produced by mkItem.
    mkItem <- function(ttl, ic, time = NULL, ...) {
      timelineItem(title = ttl, icon = icon(ic, verify_fa = FALSE),
                   color = NULL, time = time, ...)
    }
    done  <- "check-circle"; active <- "circle"
    start <- timelineStart(icon = icon("circle-user", verify_fa = FALSE), color = NULL)

    if (tab == "tab1") {
      timelineBlock(width = 12, start,
        mkItem("step 1. upload files", active, time = "now",
               htmlOutput("tlInfo1"), htmlOutput("tlWarning")),
        timelineEnd(icon = icon("circle", verify_fa = FALSE), color = NULL))
    } else if (tab == "tab2") {
      timelineBlock(width = 12, start,
        mkItem("step 1. upload files", done,   htmlOutput("tlInfo1")),
        mkItem("step 2. QC", active, time = "now"),
        timelineEnd(icon = icon("circle", verify_fa = FALSE), color = NULL))
    } else if (tab == "tab3") {
      timelineBlock(width = 12, start,
        mkItem("step 1. upload files", done,   htmlOutput("tlInfo1")),
        mkItem("step 2. QC",           done),
        mkItem("step 3. analyze data", active, time = "now",
               textOutput("tlInfo2"), htmlOutput("tlVoom")),
        timelineEnd(icon = icon("circle", verify_fa = FALSE), color = NULL))
    } else if (tab == "tab4") {
      timelineBlock(width = 12, start,
        mkItem("step 1. upload files",      done,   htmlOutput("tlInfo1")),
        mkItem("step 2. QC",                done),
        mkItem("step 3. analyze data",      done,   textOutput("tlInfo2")),
        mkItem("step 4. visualize results", active, time = "now"),
        timelineEnd(icon = icon("circle", verify_fa = FALSE), color = NULL))
    } else {
      timelineBlock(width = 12, start,
        mkItem("step 1. upload files",      done, htmlOutput("tlInfo1")),
        mkItem("step 2. QC",                done),
        mkItem("step 3. analyze data",      done, textOutput("tlInfo2")),
        mkItem("step 4. visualize results", done),
        mkItem("step 5. download results",  done, time = "now", htmlOutput("tlInfo4")),
        timelineEnd(icon = icon("flag-checkered", verify_fa = FALSE), color = NULL))
    }
  })

  output$tlInfo1 <- renderUI({
    s1 <- if (isTRUE(tables$exprStatus))
      paste("Matrix: ready —", nrow(tables$counts), "×", ncol(tables$counts) - 1)
    else "Matrix: not ready"
    s2 <- if (isTRUE(tables$metaStatus))
      paste("Meta: ready —", nrow(tables$meta), "×", ncol(tables$meta) - 1)
    else "Meta: not ready"
    HTML(paste(s1, s2, sep = "<br/>"))
  })
  output$tlWarning <- renderUI({ HTML("") })
  output$tlInfo2   <- renderText({
    if (!is.null(tables$comp)) paste("Comparison:", tables$comp) else ""
  })
  output$tlVoom    <- renderUI({ HTML("") })
  output$tlInfo4   <- renderUI({ HTML("Ready") })

  # ---- Session cleanup --------------------------------------------------
  session$onSessionEnded(function() {
    cat("\nEnding Session...\n")

    # Snapshot reactiveValues fields in a safe context before cleanup.
    logFile_now <- isolate(tables$logFile)

    # Remove all per-session outputs.
    setwd(projSpace)
    unlink(projDir, recursive = TRUE, force = TRUE)

    # Remove all log files tied to this session.
    log_candidates <- unique(c(
      if (!is.null(logFile_now) && nzchar(logFile_now)) logFile_now else character(0),
      file.path(logDir, paste0(uniStr, "_RNAseq_log.txt")),
      if (dir.exists(logDir)) list.files(
        logDir,
        pattern = paste0("^", uniStr, ".*_log\\.txt$"),
        full.names = TRUE
      ) else character(0)
    ))
    log_candidates <- log_candidates[file.exists(log_candidates)]
    if (length(log_candidates) > 0) {
      unlink(log_candidates, force = TRUE)
    }

    # Disable persistent user-level logging by clearing any existing log file.
    user_log <- file.path(projSpace, "data", "users_data.log")
    if (file.exists(user_log)) {
      unlink(user_log, force = TRUE)
    }

    cat("User files deleted.\n\nCheers!\n\n")
  })
}

shinyApp(ui, server)
