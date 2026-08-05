
# Run these lines to install required packages

pkgs <- c(
  # Shiny framework + UI
  "shiny", "shinydashboard", "shinydashboardPlus", "shinyalert", "shinyWidgets",
  "shinyjs", "shinydisconnect", "shinycssloaders", "bslib", "fresh",
  # Plotting
  "plotly", "ggplot2", "ggrepel", "heatmaply", "patchwork", "reshape2",
  "ggpubr", "ggtext", "pheatmap",
  # Tables / data manipulation
  "DT", "dplyr", "openxlsx", "data.table", "scales", "tibble", "stringr", "tidyverse",
  # Utilities
  "fontawesome", "htmltools", "markdown", "knitr", "rmarkdown",
  "parallel", "zip", "grid", "gridExtra",
  "rlang", "optparse",
  # Enrichment (CRAN)
  "enrichR", "msigdbr"
)

pkgs2 <- c(
  # Bioconductor packages
  "edgeR", "limma", "DESeq2", "rtracklayer", "RUVSeq", "Rsamtools",
  "fgsea",
  "enrichplot"       
)

if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}

missing_cran <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_cran) > 0) {
  install.packages(missing_cran)
}

missing_bioc <- pkgs2[!vapply(pkgs2, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_bioc) > 0) {
  BiocManager::install(missing_bioc)
}
