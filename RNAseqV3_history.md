**07/21/2026 version 3.0**

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
20) Filter enrichment results by P.value<0.05 followed by ranking enrichment terms by NES (for fgsea) or combined score (for enrichR) to select topN terms (min 5, max 25, default 10) for plotting barplots and dotplots in both fgsea and enrichR modules. 
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

