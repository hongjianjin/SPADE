## SPADE: ERCC Spike-in Aware RNA-seq Differential Expression and Functional Enrichment Analysis

<img src="www/SPADE_logo_small.png" alt="SPADE logo" width="180"/>

## Table of Contents
- [Introduction](#introduction)
- [Features](#features)
- [Installation](#installation)
- [Method](#method)
  - [Data Preparation](#data-preparation)
  - [QC evaluation](#qc-evaluation)
  - [Principal component analysis (PCA) analysis](#principal-component-analysis-pca-analysis)
  - [Relative log expression (RLE) plot](#relative-log-expression-rle-plot)
  - [ERCC normalization](#ercc-normalization)
  - [Differential expression analysis](#differential-expression-analysis)
  - [Enrichment analysis](#enrichment-analysis)
- [History](#history)
- [Maintainers](#maintainers)
- [Contact](#contact)
- [Notes](#notes)
  - [Development enviroment](#development-enviroment)
  - [sessionInfo()](#sessioninfo)

## Introduction
SPADE (Spike-in Powered Analysis of Differential Expression and Enrichment) is an interactive R Shiny application for coding-independent RNA-seq analysis. SPADE supports differential expression analysis using limma-voom, interactive data visualization, and downstream functional enrichment analysis within a unified web interface.

Unlike most existing RNA-seq Shiny applications, SPADE incorporates ERCC spike-in controls for technical quality assessment and spike-in-based normalization. This enables users to directly compare results obtained using alternative normalization strategies, including TMM-based and ERCC-based workflows. By integrating established statistical methods with an accessible and reproducible analysis workflow, SPADE enables experimental researchers to perform comprehensive transcriptomic analyses without programming expertise.

## Features
* Coding-independent RNA-seq differential expression analysis using limma-voom.
* ERCC spike-in quality assessment and spike-in-based normalization.
* Side-by-side comparison of TMM-normalized and ERCC-normalized analysis results.
* Interactive QC and result visualizations, including PCA, RLE, volcano plots, average expression plots, boxplots, and heatmaps.
* Functional enrichment analysis using fGSEA and EnrichR.
* Reproducible output tables, plots, PCA coordinate files, QC metric tables, and downloadable analysis results.

## Installation
Create a conda environment and activate it.
```bash
conda create --name RNAseqV3_env r-base=4.3
conda activate RNAseqV3_env
```

Install core R tooling.
```r
install.packages("BiocManager")
```

Install or verify these core tools/packages (with links):
* [Conda](https://docs.conda.io/projects/conda/en/latest/user-guide/install/index.html)
* [R](https://www.r-project.org/)
* [BiocManager](https://cran.r-project.org/package=BiocManager)
* [shiny](https://cran.r-project.org/package=shiny)
* [limma](https://bioconductor.org/packages/limma)
* [edgeR](https://bioconductor.org/packages/edgeR)
* [DESeq2](https://bioconductor.org/packages/DESeq2)
* [fgsea](https://bioconductor.org/packages/fgsea)
* [enrichR](https://cran.r-project.org/package=enrichR)

If any package fails from R, try conda-forge/bioconda equivalents for that package.

Launch the app from the project root.
```r
shiny::runApp()
```

Alternative: run `Rscript install.R` to install missing CRAN and Bioconductor packages in one step.

## Method
### Data Preparation
1. Two filtration steps by default prior to analysis to ensure that the quality results are obtained:
- [x] non-coding genes are excluded.
- [x] genes with very low expression values are also removed. Only include genes that pass a median.library.size adjusted cutoff ( 1 read per 10 million reads, equivalent to a CUTOFF_CPM of 0.1 ) in the number (smallest group size) of samples involved in pairwise comparison.
2. User can choose to skip this filtration step. However, if biotype column is not present in the counts table, only low expression filtering can be turned off by unchecking this option.
3. A species selection is required - Human, Mouse or Other. This selection method is validated using geneID or/and geneSymbol column. The selected and validated species ensure correct gene set database are loaded for enrichment analysis. The app can run differential analysis for any other species if at least gene symbol column is present. However, to run enrichment analysis for any species other than Human or mouse, user must load species matched custom gmt file.
4. ERCC counts (if present), are automatically detected and user is notified. QC evaluation is imperative and non-optional in this case to allow user to switch back to TMM normalization mode in case ERCC failed.

### QC evaluation
1. Library size, RLE and PCA plots are generated to evaluate sample-level quality check. QC outputs include plot files, summary metric tables, and PCA coordinate TSV files for downstream review.
2. ERCC mode generates library size, PCA and spike-in level plots along with QC metric tables for per-sample ERCC statistics, group-level ERCC CV, and comparison-level spike-in shift when available. At minimum, ERCC correlation threshold of 0.90 and coefficient of variation per group threshold of 15% is used to determine if ERCC passed essential QC considerations and can be reliably used for normalization. User may use QC plots and tables to make additional evaluation and proceed with ERCC normalization if ERCC passed all QC considerations.

### Principal component analysis (PCA) analysis
1. Normalize raw count to log2CPM, and select top 3000 features as input for PCA.
2. It is possible to customize shapes adding a [BATCH] or [SHAPE] column to your metadata file(see 2.3 meta-data example file).
3. Saved PCA outputs include coordinate TSV files with sample IDs, groups, PC scores, color/shape metadata when available, and percent variance for each PC.

### Relative log expression (RLE) plot
We assume majority of the genes are not differentially expressed. In relative log expression(RLE) plot, unwanted variation both between and within batches are indicated by the varying position and size of the boxplots.
If there is no technical effect, the boxplots are centered around zero and become roughly the same size. An RLE plot is constructed as follows:
1. Let Gij be the log2CPM for gene i in sample j.
2. For each gene i, calculate its median expression across the all samples, i.e. Med(Gi), and calculate the deviations from this median, i.e. Gij’ = Gij − Med(Gi)
3. For each sample, generate a boxplot of all the deviations for all the genes in that sample.

### ERCC normalization
1. ERCC normalization uses ERCC-derived normalization factors from `edgeR::calcNormFactors()` with sample library sizes, then applies limma-voom to the adjusted count model.

### Differential expression analysis
1. After filtering steps, normalization factors are generated using the TMM (or ERCC) normalization method. 
2. Counts are normalized using voom. Voom normalized counts are analyzed using the lmFit and eBayes functions of the limma software package to calculate logFC and p-values. 
3. Plots are generated from voom normalized log2CPM data.

### Enrichment analysis
1. Two types of enrichment analysis options are available: (1) fGSEA and (2) enrichR
2. fGSEA is a fast implementation of GSEA analysis which utilizes ranked DE genes (.rnk files). Genes are ranked by -log10Pvalue, log2FC or t statistics (these are among the standard columns in limma DE output table). 
3. enrichR is run on DE genes extracted from DE tables. Genes are split into Upregulated and Downregulated categories based on log2FC values and enrich R is run on each category separately. Each gene set can be filtered by Pvalue, FDR or log2FC (Default filter: Pvalue <= 0.05). Genes are then ranked by one of the 3 metric (same choices as fGSEA) to select topN genes where N cannot exceed 500. (Default ranking by -log10Pvalue) 
4. Only significant outputs (P.value<0.05) are retained and used for plotting.
5. Both fGSEA and enrichR outputs can be used to generate barplots and dotplots for topN genes (min 5, max 25, default 10) in each category (Upregulated and Downregulated). Dotplots are generated using ggplot2 and barplots are generated using plotly.

## History

**08/21/2026 version 3.0**

1) Added fGSEA module to run fGSEA analysis using ranked DE gene list (.rnk file).
2) Modularized code for easy update of existing feature and easy integration of additional features.
3) Added fGSEA plots and fGSEA module files download option.
4) Created separate QC tab to evaluate RLE, library size, etc.
5) Created separate ERCC workflow to facilitate ERCC normalization
6) updated visualization to include both TMM normalized as well as ERCC normalized data visualizations.
7) added enrichR submodule
8) streamlined ERCC run mode for all tabs (QC, visualization,etc.)
9) ERCC mode now generates 2 sets of plots (TMM and ERCC normalized data plots)
10) Changed labels (TREATMENT and CONTROL) -> (GROUP1 and GROUP2)
11) added description to specify direction of change so user understand clearly how group1 and group2 choices affect the analysis/output
13) Added species validation in the data input step using geneID and gene symbol (once validated, the same species database is utilized to run enrichment analysis)
14) added standard mSigDB as well as non-standard mSigDB selection option in fGSEA module
15) warning added if >=4 database selected; added enrichR as second option for enrichment analysis
16) added theme module to change color scheme as well as to allow future seamless aesthetic updates.
17) Updated fGSEA and enrichR plots. Dotplots plot more than 2 data which can be error-prone in interactive version gerneated by plotly - converted them to static plots. Also restricted enrich db selection to 18 commonly used databases.
18) Created optimal options for selecting DE genes for running enrichR  - ranking using one of the 3 ranking metric similar to fGSEA section, followed by option to choose top 100, 250 and 500 genes Upregulated and Downregulated DE genes each for running enrichR. 
19) Added option to select topN genes (min 5, max 25, default 10) for plotting barplots and dotplots in enrichR module.
20) Filter enrichment results by P.value<0.05 followed by ranking enrichment terms by NES (for fgsea barplot), or Pvalue (for fgsea dotplot and for enrichR barplots and dotplots) to select topN terms (min 5, max 25, default 10) for plotting barplots and dotplots in both fgsea and enrichR modules. 
21) Added option to select species in the Input tab/module. This is additionally validated using geneId or gene symbol. 
22) Added geneset db size information in the enrichment section


**08/08/2024 version 2.0**
1) allow multiple DE runs from same inputs and download results individually.
2) update summary table by different cutoffs.
3) DE table preview could choose TopN_DEGs, All_DEGs, or all genes.
4) use TopN DEGs or input a custom gene list in Heatmap setting panel.
5) dynamically label user-input genes in AveExpr plot, Volcano plot, MAplots.
6) TopN_DEGs will be labeled in in AveExpr plot, Volcano plot, MAplots.
7) The DEG table is identical to the one from CAB standard RNA-seq analysis report pipeline V2.20.
8) implement a CUTOFF_CPM of 0.1 for pre-filtering step.
9) implement "include all samples" to PCA, boxplot and heatmap.
10) implement "Dendrogram" options and sample annotation to heatmap.
11) Add t.Statistic back to final output table.
12) Significantly refactored codes to be compatible with shinydashboardPlus_2.0.3
13) use same color scheme for plots (PCA, boxplot etc.)
14) enable optional static labels in PCA plot.
15) implement library size plot in step2
16) Pop up friendly message when something abnormal is detected.
17) implement plotly figures in svg format.
18) generate three rnk files ready for GSEA
19) add RLE plot 
20) generate rnk files (ENSEMBL geneID in the first column) for GSEAPreranked\
21) if at least two groups with sample size >=3, generate TMM.txt (ENSEMBL geneID in the NAME column, Gene Symbol in the DESCRIPTION column), *.cls file for standard GSEA
22) add pearson r to AveExpr plot title
23) update history in Markdown 
24) check alphanumeric characters in meta data file
25) fix bug of PCA failure: plot.marow_grpin theme element undefined in the element hierarchy [1/30/2024].
26) implement QuickFile modules FileEditor and TableMerger
27) fixed bug of the column name check in non-AutoMapper gene count matrix
28) use RNAseqV2.3x as prefix of QuickFile output files
29) optimization of the RLE plot

## Maintainers

* [Hongjian Jin]

[Hongjian Jin]: https://github.com/hongjianjin

* [Surbhi Sona]

[Surbhi Sona]: https://github.com/2019surbhi

## Contact
shiny app RNAseqV3 is co-developed by Dr. Hongjian Jin and Surbhi Sona at the St Jude Children's Research Hospital. For collaborations or any other matters, please contact hongjian.jinATstjude.org

## Notes


### Development enviroment 
* BiocManager::install(version = "3.9")
* options(repos = BiocManager::repositories())
options(repos = list(CRAN = "https://cloud.r-project.org",  
            BioCsoft = "https://bioconductor.org/packages/3.10/bioc",                                                  
	    BioCann = "https://bioconductor.org/packages/3.10/data/annotation" ,                                               
	    BioCexp ="https://bioconductor.org/packages/3.10/data/experiment" ,
            BioCworkflows = "https://bioconductor.org/packages/3.10/workflows" )) 
*Disable OneDrive syncing if you use local R Studio in Windows/St Jude deskPC or laptop.

### sessionInfo()
```R
>sessionInfo()
R version 4.3.3 (2024-02-29)
Platform: aarch64-apple-darwin20 (64-bit)
Running under: macOS 26.5

Matrix products: default
BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib 
LAPACK: /Library/Frameworks/R.framework/Versions/4.3-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.11.0

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

time zone: America/Chicago
tzcode source: internal

attached base packages:
[1] grid      stats4    stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] enrichR_3.4                 openxlsx_4.2.8.1            data.table_1.18.2.1        
 [4] rtracklayer_1.62.0          lubridate_1.9.5             forcats_1.0.1              
 [7] purrr_1.2.1                 readr_2.2.0                 tidyr_1.3.2                
[10] tidyverse_2.0.0             ggtext_0.1.2                tibble_3.3.1               
[13] msigdbr_26.1.0              fgsea_1.28.0                enrichplot_1.22.0          
[16] zip_2.3.3                   markdown_2.0                reshape2_1.4.5             
[19] fontawesome_0.5.3           scales_1.4.0                stringr_1.6.0              
[22] DESeq2_1.42.1               SummarizedExperiment_1.32.0 Biobase_2.62.0             
[25] MatrixGenerics_1.14.0       matrixStats_1.5.0           GenomicRanges_1.54.1       
[28] GenomeInfoDb_1.38.8         IRanges_2.36.0              S4Vectors_0.40.2           
[31] BiocGenerics_0.48.1         edgeR_4.0.16                limma_3.58.1               
[34] heatmaply_1.6.0             viridis_0.6.5               viridisLite_0.4.3          
[37] plotly_4.12.0               ggrepel_0.9.8               ggplot2_4.0.2              
[40] dplyr_1.2.0                 DT_0.34.0                   shinycssloaders_1.1.0      
[43] shinydisconnect_0.1.1       shinyjs_2.1.1               shinyWidgets_0.9.1         
[46] fresh_0.2.2                 shinydashboardPlus_2.0.6    shinydashboard_0.7.3       
[49] shinyalert_3.1.0            shiny_1.13.0               

loaded via a namespace (and not attached):
  [1] fs_1.6.7                      bitops_1.0-9                  HDO.db_0.99.1                
  [4] httr_1.4.8                    webshot_0.5.5                 RColorBrewer_1.1-3           
  [7] doParallel_1.0.17             backports_1.5.0               tools_4.3.3                  
 [10] R6_2.6.1                      lazyeval_0.2.2                GetoptLong_1.1.0             
 [13] litedown_0.9                  withr_3.0.2                   gridExtra_2.3                
 [16] textshaping_1.0.1             cli_3.6.5                     TSP_1.2.6                    
 [19] scatterpie_0.2.6              countToFPKM_1.2.0             labeling_0.4.3               
 [22] sass_0.4.10                   S7_0.2.1                      systemfonts_1.2.3            
 [25] commonmark_2.0.0              Rsamtools_2.18.0              yulab.utils_0.2.4            
 [28] DOSE_3.28.2                   WriteXLS_6.8.0                rstudioapi_0.18.0            
 [31] RSQLite_2.4.6                 BiocIO_1.12.0                 generics_0.1.4               
 [34] gridGraphics_0.5-1            shape_1.4.6.1                 crosstalk_1.2.2              
 [37] dendextend_1.19.1             GO.db_3.18.0                  Matrix_1.6-5                 
 [40] abind_1.4-8                   lifecycle_1.0.5               yaml_2.3.12                  
 [43] qvalue_2.34.0                 SparseArray_1.2.4             BiocFileCache_2.10.2         
 [46] blob_1.3.0                    promises_1.5.0                ExperimentHub_2.10.0         
 [49] crayon_1.5.3                  lattice_0.22-9                beachmat_2.18.1              
 [52] cowplot_1.2.0                 KEGGREST_1.42.0               pillar_1.11.1                
 [55] knitr_1.51                    ComplexHeatmap_2.18.0         rjson_0.2.23                 
 [58] codetools_0.2-20              fastmatch_1.1-8               glue_1.8.0                   
 [61] ggfun_0.2.0                   vctrs_0.7.1                   png_0.1-8                    
 [64] treeio_1.26.0                 gtable_0.3.6                  assertthat_0.2.1             
 [67] cachem_1.1.0                  xfun_0.56                     S4Arrays_1.2.1               
 [70] mime_0.13                     tidygraph_1.3.1               seriation_1.5.7              
 [73] iterators_1.0.14              statmod_1.5.0                 interactiveDisplayBase_1.40.0
 [76] nlme_3.1-168                  ggtree_3.10.1                 bit64_4.6.0-1                
 [79] filelock_1.0.3                bslib_0.10.0                  irlba_2.3.5.1                
 [82] otel_0.2.0                    colorspace_2.1-2              DBI_1.3.0                    
 [85] celldex_1.12.0                tidyselect_1.2.1              bit_4.6.0                    
 [88] compiler_4.3.3                curl_7.0.0                    xml2_1.5.2                   
 [91] DelayedArray_0.28.0           shadowtext_0.1.5              checkmate_2.3.4              
 [94] rappdirs_0.3.4                digest_0.6.39                 rmarkdown_2.30               
 [97] ca_0.71.1                     XVector_0.42.0                htmltools_0.5.9              
[100] pkgconfig_2.0.3               SingleR_2.4.1                 sparseMatrixStats_1.14.0     
[103] dbplyr_2.5.2                  fastmap_1.2.0                 rlang_1.1.7                  
[106] GlobalOptions_0.1.3           htmlwidgets_1.6.4             DelayedMatrixStats_1.24.0    
[109] farver_2.1.2                  jquerylib_0.1.4               jsonlite_2.0.0               
[112] BiocParallel_1.36.0           GOSemSim_2.28.1               BiocSingular_1.18.0          
[115] RCurl_1.98-1.17               magrittr_2.0.4                GenomeInfoDbData_1.2.11      
[118] ggplotify_0.1.3               patchwork_1.3.2               Rcpp_1.1.1                   
[121] ape_5.8-1                     babelgene_22.9                stringi_1.8.7                
[124] ggraph_2.2.2                  zlibbioc_1.48.2               MASS_7.3-60.0.1              
[127] AnnotationHub_3.10.1          plyr_1.8.9                    parallel_4.3.3               
[130] Biostrings_2.70.3             graphlayouts_1.2.3            splines_4.3.3                
[133] gridtext_0.1.6                hms_1.1.4                     circlize_0.4.17              
[136] locfit_1.5-9.12               uuid_1.2-2                    igraph_2.1.4                 
[139] ScaledMatrix_1.10.0           XML_3.99-0.22                 BiocVersion_3.18.1           
[142] evaluate_1.0.5                BiocManager_1.30.27           tzdb_0.5.0                   
[145] foreach_1.5.2                 tweenr_2.0.3                  httpuv_1.6.16                
[148] polyclip_1.10-7               clue_0.3-67                   ggforce_0.5.0                
[151] rsvd_1.0.5                    xtable_1.8-8                  restfulr_0.0.16              
[154] tidytree_0.4.7                later_1.4.8                   ragg_1.4.0                   
[157] aplot_0.2.9                   GenomicAlignments_1.38.2      memoise_2.0.1                
[160] AnnotationDbi_1.64.1          registry_0.5-1                cluster_2.1.8.2              
[163] timechange_0.4.0       
```