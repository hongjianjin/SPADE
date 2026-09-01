library(shiny)
library(shinydashboard)
library(shinydashboardPlus)

# =============================================================================
# Documentation Module  –  Manual + Privacy
# UI:     DocumentationUI(id)
# Server: DocumentationServer(id, projSpace,
#                             metaExample1, countExample1,
#                             metaExample2, countExample2,
#                             metaExample3)
# =============================================================================

#' Documentation UI
#'
#' Executes DocumentationUI.
#'
#' @param id Argument for DocumentationUI.
#'
#' @return Return value produced by DocumentationUI.
DocumentationUI <- function(id) {
  ns <- NS(id)
  tabsetPanel(
    # ---- Manual ----------------------------------------------------------
    tabPanel(
      tags$b("Manual"),
      fluidPage(
        fluidRow(
          box(
            title = "Introduction", closable = FALSE, width = NULL,
            status = "primary", solidHeader = TRUE,
            collapsed = FALSE, collapsible = TRUE, enable_dropdown = FALSE,
            column(
              8,
              tags$br(),
              tags$p(
                "This interactive R Shiny application uses limma-voom method for differential expression",
                "(DE) analysis of bulk RNA-seq data. Users can optionally perform downstream enrichment analysis",
                "using those DE outputs. It requires a raw count matrix and metadata as inputs, and generates QC",
                "assessment and performs DE analysis. It can also generate visualizations including PCA, volcano plot, Pairwise",
                "Average Gene Expression (AveExpr) plot, heatmaps, etc."
              )
            ),
            column(
              8,
              tags$h3("1. Input tab"),
              tags$br(),
              tags$a(id = "step1_load_input_data"),
              tags$u("STEP1: Load input data"),
              tags$ul(
                tags$li("Upload expression matrix (raw counts) and metadata files; Use example for app demo"),
                tags$li("All input files must be either tab- or comma-delimited text files with a header line."),
                tags$li("Data preview is generated as soon as corectly formatted input data is loaded, else warning is generated."),
                tags$li("(Recommended): Correctly formatted metadata files can be also created using count matrix and further edited in QuickFile"),
                tags$li("For more details on file format, please refer to the section - ", tags$a(href = "#file_format", "File format")),
                tags$li("If ERCC counts are detected, user is notified; ERCC must be evaluated in QC section to proceed with ERCC normalization")
              ),
              tags$u("STEP2: Select species"),
              tags$br(),
              tags$ul(
                tags$li("User can upload data for Human, Mouse, or any other species but must specify species using checkbox selection."),
                tags$li("GeneID or gene symbol columns are used to validate species selection."),
                tags$li("If geneID suggests 'not mouse' and 'not human' then validation step defaults selection to 'other'."),
                tags$li("However, correct assignment to 'other' species is NOT possible if ONLY gene symbol column is present in the input count table!"),
                tags$li("In that scenario, user-dependent species selection is relied on with minimal validation for human (all upper case gene symbol) and mouse (first upper case)."),
                tags$li("Species selection is important to ensure species specific geneset database are used for enrichment analysis.")
              ),
              tags$u("STEP3: Click Validate Input button"),
              tags$br(),
              tags$ul(
                tags$li("Data check is performed real time for each entity - counts and metadata file format and species selection."),
                tags$li("However, all checks are colsolidated when Validated inputs button is clicked to move on to next step (QC/Analysis)")
              ),
              tags$u("Step4: Data Processing"),
              tags$br(),
              tags$ul(
                tags$li("By default, non-coding genes are excluded and low-expression genes filtered at 1 CPM per 10M reads (CUTOFF_CPM = 0.1)"),
                tags$li("Users can turn off these filters if desired (although not recommended).")
              )
            ),
            column(
              8,
              tags$br(),
              tags$h3("2. QC tab"),
              tags$ul(
                tags$li("QC plots such as Principal Component Analysis (PCA) plot, Relative Log Expression (RLE) plot and library size boxplots are generated for sample QC evaluation"),
                tags$li("Add a SHAPE column to metadata to customise point shapes"),
                tags$li("Evaluate PCA plot to check for sample level anamolies or batch effects"),
                tags$li("If batch effect exists, a BATCH column can be added to the metadata which directs limma to use BATCH as covariate during DE analysis."),
                tags$li("The medians of RLE distributions across samples are expected to be centered around 0, with similar spreads; wide distributions and shifted centers indicate poor QC, batch effect, or other technical artifacts."),
                tags$li("Library size plots are used to evaluated sample sequencing depth; for DE analysis, each sample is expected to have 30M-60M reads"),
                tags$li("ERCC mode generates additional QC plots and summary tables to assess spike-in performance and other QC metrics, confirming whether ERCC normalization worked as expected")
              )
            ),
            column(
              8,
              tags$br(),
              tags$h3("3. Analyze tab"),
              tags$ul(
                tags$li("Choose comparison group to perform DE analysis."),
                tags$li("Differential expression analysis is performed using TMM normalization, followed by voom transformation and limma fitting with lmFit and eBayes"),
                tags$li("DE tables along with ranked gene table are generated; the latter can be used to run GSEA analysis on the GSEA app."),
                tags$li("Three ranking metric are used - log2FC, (log2FC*-log10(P.value) and t.statistics)"),
                tags$li("Once analysis is completed, you will be automatically directed to Visualization tab."),
              )
            ),
            column(
              8,
              tags$h3("4. Visualize tab"),
              tags$ul(
                tags$li("Allows choosing p.value, FDR and log2FC thresholds to generate DE gene count summary table."),
                tags$li("Using checkbox selection, generate various visualization for DE results - PCA,Volcano plot, MA plot, Average expression plot, Boxplot and Heatmap "),
                tags$li("Select number of top DE genes to highlight in these plots (0,5,10,15,20,50 and 100)"),
                tags$li("Additionally, any gene of interest can be highlighted on these plots"),
                tags$li("For ERCC mode, both TMM normalized and ERCC normalized data is plotted for comaprison.")
              )
            ),
            column(
              8,
              tags$br(),
              tags$h3("5. Enrichment tab"),
              tags$ul(
                tags$li("For quick evaluation of the DE results, enrichment analysis can be optionally performed."),
                tags$li("If species selected in Input tab is 'Human' or 'Mouse', species specific geneset database (db) are loaded for enrichment analysis."),
                tags$li("If inputs correspond to any 'Other' species, the user can skip enrichment analysis or upload species specific custom GMT files to run enrichment analysis."),
                tags$li("Entire set of DE genes can be subjected to gene set enrichment analysis using ", tags$b("fGSEA")),
                tags$li("Additionally/alternately, top enrichment terms can be evaluated by running ", tags$b("enrichR"), " on top DE genes (100, 250 or 500)"),
              ),
              tags$br(),
              tags$u(tags$b("Running fGSEA")),
              tags$ul(
                tags$li("Select one or more geneset db to run fGSEA; Hallmark is selected by default."),
                tags$li("Since each run takes considerable time, a warning is generated if >=4 db are selected."),
                tags$li("Once fGSEA is completed, the enrichment table preview is available; the result is filtered for P.value < 0.05."),
                tags$li("The filtered output can be used to generate barplots and dotplots to plot various metrics related to the enriched terms."),
                tags$li("Only top terms (ranked by 'NES') are plotted; min 5 and max 25 terms can be plotted for each direction (Up and Down)")
              ),
              tags$br(),
              tags$u(tags$b("Running enrichR")),
              tags$ul(
                tags$li("Select ranking metric to use (log2FC, log2FC * -log10(P.value), or t.statistics)."),
                tags$li("Select number of top DE genes to use (100, 250, or 500)."),
                tags$li("DE are selected for Upregulated and Downregulated set separately."),
                tags$li("Select enrichR db to run enrichment analysis."),
                tags$li("Once enrichR run is completed, the enrichment table preview is available; the result is filtered for P.value < 0.05."),
                tags$li("The filtered output can be used to generate barplots and dotplots to plot various metrics related to the enriched terms."),
                tags$li("Only top terms (ranked by 'combined.score') are plotted; min 5 and max 25 terms can be plotted for each direction (Up and Down)")
              )
            ),
            column(
              8,
              tags$br(),
              tags$h3("6. Download tab"),
              tags$ul(
                tags$li("All outputs generated from Analyze, Visualize and Enrichment tabs per analysis can be downladed as zip file."),
                tags$li("Users have an option to exclude unwanted runs")
              )
            ),
            column(8, tags$br(), tags$br()),
            tags$a(id = "file_format"),
            column(
              8,
              tags$h3(tags$u("Input file format")),
              tags$p(tags$a(href = "#step1_load_input_data", "Back to STEP1: Load input data")),
              tags$br(),
              tags$b("Input expression matrix file"),
              tags$br(),
              tags$p(
                "1.1 ",
                tags$u("CAB AutoMapper format"),
                " - first four columns: ",
                tags$i("geneID"), ", ", tags$i("geneSymbol"), ", ", tags$i("bioType"),
                ", and ", tags$i("annotationLevel"), "."
              ),
              tags$p("Example 1 in the app represents this format [expression matrix file 1]:"),
              div(style = "overflow-x: scroll", tableOutput(ns("countExample1"))),
              tags$p(
                "1.2 ",
                tags$u("Minimal format"),
                " - first column named ", tags$i("gene"), " / ", tags$i("geneSymbol"),
                " / ", tags$i("symbol"), " (case-insensitive)."
              ),
              tags$p("Use example 2 in the app to understand this format [expression matrix file 2]:"),
              div(style = "overflow-x: scroll", tableOutput(ns("countExample2"))),
              tags$p(
                "1.3 ",
                tags$u("CAB AutoMapper format with ERCC counts"),
                " - first four columns: ", tags$i("geneID"), ", ", tags$i("geneSymbol"),
                ", ", tags$i("bioType"), ", and ", tags$i("annotationLevel"),
                " (additional rows denoting ERCC read counts)."
              ),
              tags$p("Example 3 illustrates this format [expression matrix file 3]:"),
              div(style = "overflow-x: scroll", tableOutput(ns("countExample3")))
            ),
            column(
              8,
              tags$br(),
              tags$b("2. Input metadata file"),
              tags$br(),
              tags$ul(
                tags$li("At least three columns: ID, NEWNAME, GROUP."),
                tags$li("NEWNAME column is used for alternate short names but can be identical to ID column."),
                tags$li("ID and NEWNAME entries must be unique."),
                tags$li(tagList("At least ", tags$b("two"), " replicates required per GROUP."))
              ),
              tags$p("2.1 Metadata file 1:"), div(tableOutput(ns("metaExample1"))),
              tags$p("2.2 Metadata file 2:"), div(tableOutput(ns("metaExample2"))),
              tags$p("2.3 Optional SHAPE column for PCA customisation:"), div(tableOutput(ns("metaExample3")))
            ),
            column(8, tags$br(), tags$br())
          )
        ),
        fluidRow(
          box(
            title = "Frequently Asked Questions", closable = FALSE, width = NULL,
            status = "primary", solidHeader = TRUE, collapsible = TRUE,
            enable_dropdown = FALSE,
            htmlOutput(ns("infoQA"))
          )
        ),
        fluidRow(
          box(
            title = "New Features", closable = FALSE, width = NULL,
            status = "primary", solidHeader = TRUE, collapsible = TRUE,
            enable_dropdown = FALSE,
            column(8, includeMarkdown("docs/RNAseqV3_history.md"))
          )
        )
      )
    ),

    # ---- Privacy ---------------------------------------------------------
    tabPanel(
      tags$b("Privacy"),
      fluidPage(
        fluidRow(
          userBox(
            title = userDescription(
              title    = "Data Privacy",
              subtitle = "Submitted data is for your eyes only.",
              type     = 2,
              image    = "dataprotection.jpg"
            ),
            status = "primary",
            paste0(
              "When you use our services, you're trusting us with your data. ",
              "We take the confidentiality of your submitted data seriously and are committed ",
              "to keeping your submissions private. The Center of Applied Bioinformatics (CAB) ",
              "does not collect or keep any user input files and relevant deliverable results ",
              "generated by the pipeline. After your session is over, all these files will be ",
              "permanently deleted from this server."
            ),
            footer = "Thank you for using CAB services!"
          )
        )
      )
    )
  )
}

#' Documentation Server
#'
#' Executes DocumentationServer.
#'
#' @param id Argument for DocumentationServer.
#' @param projSpace Argument for DocumentationServer.
#' @param metaExample1 Argument for DocumentationServer.
#' @param countExample1 Argument for DocumentationServer.
#' @param metaExample2 Argument for DocumentationServer.
#' @param countExample2 Argument for DocumentationServer.
#' @param metaExample3 Argument for DocumentationServer.
#' @param countExample3 Argument for DocumentationServer.
#'
#' @return Return value produced by DocumentationServer.
DocumentationServer <- function(id, projSpace,
                                metaExample1, countExample1,
                                metaExample2, countExample2,
                                metaExample3, countExample3) {
  moduleServer(id, function(input, output, session) {
    output$countExample1 <- renderTable(countExample1[1:5, ])
    output$countExample2 <- renderTable(countExample2[1:5, ])
    output$countExample3 <- renderTable(countExample3[1:5, ])
    output$metaExample1 <- renderTable(metaExample1)
    output$metaExample2 <- renderTable(metaExample2)
    output$metaExample3 <- renderTable(metaExample3)

    output$infoQA <- renderUI({
      includeMarkdown("docs/RNAseqV3_faq.md")
    })
  })
}
