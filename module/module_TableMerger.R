library(shiny)
library(DT)
library(shinyjs)
library(data.table)
library(zip)
library(shinyalert)
#============================================================================
# Functions
#===================================================
#' Shiny Input
#'
#' Executes shinyInput.
#'
#' @param FUN Argument for shinyInput.
#' @param len Argument for shinyInput.
#' @param id Argument for shinyInput.
#' @param ns Argument for shinyInput.
#' @param ... Argument for shinyInput.
#'
#' @return Return value produced by shinyInput.
shinyInput <- function(FUN, len, id, ns, ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(paste0(id, i), ...))
  }
  inputs
}

#=====================================================
#' Merge Two Files
#'
#' Executes MergeTwoFiles.
#'
#' @param joinType Argument for MergeTwoFiles.
#' @param colNameX Argument for MergeTwoFiles.
#' @param colNameY Argument for MergeTwoFiles.
#' @param file1 Argument for MergeTwoFiles.
#' @param file2 Argument for MergeTwoFiles.
#' @param headerFlag Argument for MergeTwoFiles.
#' @param uniqColumn Argument for MergeTwoFiles.
#' @param outFile Argument for MergeTwoFiles.
#'
#' @return Return value produced by MergeTwoFiles.
MergeTwoFiles <-function(joinType, colNameX,colNameY, file1,file2, headerFlag=TRUE, uniqColumn=FALSE, outFile=NA){
  #------------10/13/2018 10:51 PM ------------
  if (!grepl(".rds$",file1) | !grepl(".xlsx$",file1)){
    con <- file(file1, "rt")
    line1 <- readLines(con, 1)
    close(con)
    ncol1 <- length(unlist(strsplit(line1, "\t")))
  }  
  if (!grepl(".rds$",file2) | !grepl(".xlsx$",file2)){
    con <- file(file2, "rt")
    line1 <- readLines(con, 1)
    close(con)
    ncol2 <- length(unlist(strsplit(line1, "\t")))
  }
  #=====================================================
  if (toupper(colNameX) == toupper("row.names") & toupper(colNameY) == toupper("row.names")) {
    if (grepl(".rds$",file1)){
      df1 <- readRDS(file1)
      ncol1 <- ncol(df1)
    }else{
      df1 <- fread(file1, sep="\t",header=headerFlag,fill=TRUE, quote="",check.names=F,data.table=FALSE )
      rownames(df1) <- df1[,1]
      df1[,1] <- NULL
    }
    if (grepl(".rds$",file2) ){
      df2 <- readRDS(file2)
      ncol2 <- ncol(df2)
    }else{
      # df2 <- read.table(file2, sep = "\t", header = headerFlag, fill = TRUE, stringsAsFactors = FALSE, quote = "", row.names = 1, check.names = F)
      df2 <- fread(file2, sep="\t",header=headerFlag,fill=TRUE, quote="",check.names=F,data.table=FALSE )
      rownames(df2) <- df2[,1]
      df2[,1] <- NULL          
    }
    x <- rownames(df1)
    y <- rownames(df2)
  }else{
    df1 <- fread(file1, sep="\t",header=headerFlag,fill=TRUE, quote="",check.names=F,data.table=FALSE )
    df2 <- fread(file2, sep="\t",header=headerFlag,fill=TRUE, quote="",check.names=F,data.table=FALSE )
    if (! colNameX %in% colnames(df1)){
      cat("\nError: column not found. [",colNameX,"]\n")
      stop("Exit.")
    }
    if (! colNameY %in% colnames(df2)){
      cat("\nError: column not found. [",colNameY,"]\n")
      stop("Exit.")
    }
    x <- df1[, colNameX]
    y <- df2[, colNameY]
  } 
  
  if (ncol1 == 1) {
    cat("There is only 1 column in table 1\n")
    df1$pseudo <- NA
  } else {
    cat("\nThere are", ncol1, "columns in table 1.\n")
  }
  
  if (ncol2 == 1) {
    cat("There is only 1 column in table 2\n")
    df2$pseudo <- NA
  } else {
    cat("\nThere are", ncol2, "columns in table 2.\n")
  }
  
  df1 <- df1[!duplicated(df1), ] # keep unique rows
  df2 <- df2[!duplicated(df2), ] # keep unique rows
  options(warn = -1)
  
  if (toupper(joinType) == "OUTER") {
    tmp <- merge(x = df1, y = df2, by.x= colNameX, by.y=colNameY, all = TRUE) 
  } else if (toupper(joinType) == "LEFT") {
    tmp <- merge(x = df1, y = df2, by.x= colNameX, by.y=colNameY, all.x = TRUE) 
  } else if (toupper(joinType) == "RIGHT") {
    tmp <- merge(x = df1, y = df2,by.x= colNameX, by.y=colNameY,all.y = TRUE) 
  #} else if (toupper(joinType) == "CROSS") {
  #  tmp <- merge(x = df1, y = df2, by = NULL)
  } else if (toupper(joinType) == "INNER") {
    comm <- intersect(x, y)
    if (ncol1 == 1) {
      tmp <- df2[match(comm, y), ]
    } else if (ncol2 == 1) {
      tmp <- df1[match(comm, x), ]
    } else { # good for tables with at least 2 columns
      tmp <- na.omit(merge(x = df1, y = df2, by.x= colNameX, by.y=colNameY, all = TRUE)) # Inner join
    }
  }
  options(warn = 1)
  DF <- tmp
  if ( toupper(colNameX) == toupper("row.names") & toupper(colNameY) == toupper("row.names")) {
    DF <- transform(tmp, row.names = Row.names, Row.names = NULL)
    DF <- data.frame(rowname = rownames(DF), DF, check.names = F)
    rownames(DF) <- 1:nrow(DF)
  }
  
  
  DF <- DF[!duplicated(DF), ] # keep unique rows
  DF <- DF[, colSums(is.na(DF)) < nrow(DF)] # remove columns where all values are NA - outer join could produce these columns
  
  #print(head(DF, n = 5))
  if (uniqColumn == TRUE){
    cat("\n Searching dupicated column names... ")
    tmp <- table(gsub("\\.[xy]$","", colnames(DF)))
    dupColNames<- names(tmp)[tmp>1]
    if (length(dupColNames) >0){
      cat("\n\tFound ",length(dupColNames),":", dupColNames)
      cat("\n\tOrig. ncol: ",ncol(DF))
      DFuniq<- DF 
      for(n in dupColNames) {
          toRename <- paste0( n, ".x")
          toDel <- paste0( n, ".y")
          idx_x <-grep(toRename, colnames(DFuniq))
          idx_y <-grep(toDel, colnames(DFuniq))
          if (all(DF[,paste0( n, ".x")] == DF[,paste0( n, ".y")])){
            colnames(DFuniq)[idx_x] <- gsub("\\.x","", toRename)            
            DFuniq <- DFuniq[, -idx_y]
          }
      }
      DF <- DFuniq
      cat("\n\tUniq. ncol: ",ncol(DF))
    }
  }
  
  write.table(DF, outFile, sep = "\t", quote = F, row.names = F, col.names = T)
  cat(paste0("\n",basename(outFile),"[",nrow(DF), "x", ncol(DF), ",saved]\n"))
  if (file.exists(outFile)){
    if (file.info(outFile)$size >0){
      return(F)
    }
  }
  
}
#####################################################

#' Join2 Files
#'
#' Executes Join2Files.
#'
#' @param joinType Argument for Join2Files.
#' @param joinBy Argument for Join2Files.
#' @param file1 Argument for Join2Files.
#' @param file2 Argument for Join2Files.
#' @param headerFlag Argument for Join2Files.
#' @param uniqColumn Argument for Join2Files.
#'
#' @return Return value produced by Join2Files.
Join2Files <-function(joinType, joinBy, file1,file2, headerFlag=TRUE, uniqColumn=FALSE){
  #------------10/13/2018 10:51 PM ------------
  if (!grepl(".rds$",file1) | !grepl(".xlsx$",file1)){
    con <- file(file1, "rt")
    line1 <- readLines(con, 1)
    close(con)
    ncol1 <- length(unlist(strsplit(line1, "\t")))
  }  
  if (!grepl(".rds$",file2) | !grepl(".xlsx$",file2)){
    con <- file(file2, "rt")
    line1 <- readLines(con, 1)
    close(con)
    ncol2 <- length(unlist(strsplit(line1, "\t")))
  }
  #=====================================================
  if (toupper(joinBy) == toupper("row.names")) {
    if (grepl(".rds$",file1)){
      df1 <- readRDS(file1)
      ncol1 <- ncol(df1)
    }else{
      # df1 <- read.table(file1, sep = "\t", header = headerFlag, fill = TRUE, stringsAsFactors = FALSE, quote = "", row.names = 1, check.names = F)
      df1 <- fread(file1, sep="\t",header=headerFlag,fill=TRUE, quote="",check.names=F,data.table=FALSE )
      rownames(df1) <- df1[,1]
      df1[,1] <- NULL
    }
    if (grepl(".rds$",file2) ){
      df2 <- readRDS(file2)
      ncol2 <- ncol(df2)
    }else{
      # df2 <- read.table(file2, sep = "\t", header = headerFlag, fill = TRUE, stringsAsFactors = FALSE, quote = "", row.names = 1, check.names = F)
      df2 <- fread(file2, sep="\t",header=headerFlag,fill=TRUE, quote="",check.names=F,data.table=FALSE )
      rownames(df2) <- df2[,1]
      df2[,1] <- NULL          
    }
    x <- rownames(df1)
    y <- rownames(df2)
  }else{
    #df1 <- read.table(file1, sep = "\t", header = headerFlag, fill = TRUE, stringsAsFactors = FALSE, quote = "", row.names = NULL, check.names = F)
    #df2 <- read.table(file2, sep = "\t", header = headerFlag, fill = TRUE, stringsAsFactors = FALSE, quote = "", row.names = NULL, check.names = F)
    df1 <- fread(file1, sep="\t",header=headerFlag,fill=TRUE, quote="",check.names=F,data.table=FALSE )
    df2 <- fread(file2, sep="\t",header=headerFlag,fill=TRUE, quote="",check.names=F,data.table=FALSE )
    if (! joinBy %in% colnames(df1)){
      cat("\nError: column not found. [",joinBy,"]\n")
      stop("Exit.")
    }
    if (! joinBy %in% colnames(df2)){
      cat("\nError: column not found. [",joinBy,"]\n")
      stop("Exit.")
    }
    x <- df1[, joinBy]
    y <- df2[, joinBy]
  } 
  
  if (ncol1 == 1) {
    cat("There is only 1 column in table 1\n")
    df1$pseudo <- NA
  } else {
    cat("\nThere are", ncol1, "columns in table 1.\n")
  }
  
  if (ncol2 == 1) {
    cat("There is only 1 column in table 2\n")
    df2$pseudo <- NA
  } else {
    cat("\nThere are", ncol2, "columns in table 2.\n")
  }
  
  df1 <- df1[!duplicated(df1), ] # keep unique rows
  df2 <- df2[!duplicated(df2), ] # keep unique rows
  options(warn = -1)
  
  if (toupper(joinType) == "OUTER") {
    tmp <- merge(x = df1, y = df2, by= joinBy , all = TRUE) 
  } else if (toupper(joinType) == "LEFT") {
    tmp <- merge(x = df1, y = df2, by= joinBy, all.x = TRUE) 
  } else if (toupper(joinType) == "RIGHT") {
    tmp <- merge(x = df1, y = df2, by= joinBy ,all.y = TRUE) 
  #} else if (toupper(joinType) == "CROSS") {
  #  tmp <- merge(x = df1, y = df2, by = NULL)
  } else if (toupper(joinType) == "INNER") {
    comm <- intersect(x, y)
    if (ncol1 == 1) {
      tmp <- df2[match(comm, y), ]
    } else if (ncol2 == 1) {
      tmp <- df1[match(comm, x), ]
    } else { # good for tables with at least 2 columns
      tmp <- na.omit(merge(x = df1, y = df2, by=joinBy , all = TRUE)) # Inner join
    }
  }
  options(warn = 1)
  DF <- tmp
  if ( toupper(joinBy) == toupper("row.names")) {
    DF <- transform(tmp, row.names = Row.names, Row.names = NULL)
    DF <- data.frame(rowname = rownames(DF), DF, check.names = F)
    rownames(DF) <- 1:nrow(DF)
  }
  
  
  DF <- DF[!duplicated(DF), ] # keep unique rows
  DF <- DF[, colSums(is.na(DF)) < nrow(DF)] # remove columns where all values are NA - outer join could produce these columns
  
  print(head(DF, n = 5))
  if (uniqColumn == TRUE){
    cat("\n Searching dupicated column names... ")
    tmp <- table(gsub("\\.[xy]$","", colnames(DF)))
    dupColNames<- names(tmp)[tmp>1]
    if (length(dupColNames) >0){
      cat("\n\tFound ",length(dupColNames),":", dupColNames)
      cat("\n\tOrig. ncol: ",ncol(DF))
      toDel <- paste( dupColNames, ".y",sep="")
      DFuniq<- DF[, colnames(DF)[! colnames(DF) %in% toDel]]
      for(n in dupColNames) {
        colnames(DFuniq) <- gsub(paste0(n,"\\.x"),n,colnames(DFuniq))
      }
      DF <- DFuniq
      cat("\n\tUniq. ncol: ",ncol(DF))
    }
  }
  return(DF)
}
#####################################################

#' Append File2 Dat
#'
#' Executes AppendFile2Dat.
#'
#' @param joinType Argument for AppendFile2Dat.
#' @param joinBy Argument for AppendFile2Dat.
#' @param dat Argument for AppendFile2Dat.
#' @param file2 Argument for AppendFile2Dat.
#' @param headerFlag Argument for AppendFile2Dat.
#' @param uniqColumn Argument for AppendFile2Dat.
#'
#' @return Return value produced by AppendFile2Dat.
AppendFile2Dat <-function(joinType, joinBy, dat,file2, headerFlag=T, uniqColumn=FALSE){
  #------------10/13/2018 10:51 PM ------------
  if (!grepl(".rds$",file2) | !grepl(".xlsx$",file2)){
    con <- file(file2, "rt")
    line1 <- readLines(con, 1)
    close(con)
    ncol2 <- length(unlist(strsplit(line1, "\t")))
  }
  #=====================================================
  if (toupper(joinBy) == toupper("row.names")) {
    df1 <- dat
    if (grepl(".rds$",file2) ){
      df2 <- readRDS(file2)
      ncol2 <- ncol(df2)
    }else{
      df2 <- fread(file2, sep="\t",header=headerFlag,fill=TRUE, quote="",check.names=F,data.table=FALSE )
      rownames(df2) <- df2[,1]
      df2[,1] <- NULL          
    }
    x <- rownames(df1)
    y <- rownames(df2)
  }else{
    df1 <- dat
    if (grepl(".rds$",file2) ){
      df2 <- readRDS(file2)
      ncol2 <- ncol(df2)
    }else{
      # df2 <- read.table(file2, sep = "\t", header = headerFlag, fill = TRUE, stringsAsFactors = FALSE, quote = "", row.names = NULL, check.names = F)
      df2 <- fread(file2, sep="\t",header=headerFlag,fill=TRUE, quote="",check.names=F,data.table=FALSE )
    }
    if (! joinBy %in% colnames(df1)){
      cat("\nError: column not found. [",joinBy,"]\n")
      stop("Exit.")
    }
    if (! joinBy %in% colnames(df2)){
      cat("\nError: column not found. [",joinBy,"]\n")
      stop("Exit.")
    }
    x <- df1[, joinBy]
    y <- df2[, joinBy]
  } 
  
  if (ncol(dat) == 1) {
    cat("There is only 1 column in table 1\n")
    df1$pseudo <- NA
  } else {
    cat("\nThere are", ncol(dat), "columns in table 1.\n")
  }
  
  if (ncol2 == 1) {
    cat("\nThere is only 1 column in table 2\n")
    df2$pseudo <- NA
  } else {
    cat("\nThere are", ncol2, "columns in table 2.\n")
  }
  
  # df1 <- df1[!duplicated(df1), ] # keep unique rows
  # df2 <- df2[!duplicated(df2), ] # keep unique rows
  options(warn = -1)
  
  if (toupper(joinType) == "OUTER") {
    tmp <- merge(x = df1, y = df2, by= joinBy , all = TRUE) 
  } else if (toupper(joinType) == "LEFT") {
    tmp <- merge(x = df1, y = df2, by= joinBy, all.x = TRUE) 
  } else if (toupper(joinType) == "RIGHT") {
    tmp <- merge(x = df1, y = df2, by= joinBy ,all.y = TRUE) 
  #} else if (toupper(joinType) == "CROSS") {
  #  tmp <- merge(x = df1, y = df2, by = NULL)
  } else if (toupper(joinType) == "INNER") {
    comm <- intersect(x, y)
    if (ncol(dat) == 1) {
      tmp <- df2[match(comm, y), ]
    } else if (ncol2 == 1) {
      tmp <- df1[match(comm, x), ]
    } else { # good for tables with at least 2 columns
      tmp <- na.omit(merge(x = df1, y = df2, by=joinBy , all = TRUE)) # Inner join
    }
  }
  options(warn = 1)
  DF <- tmp
  if ( toupper(joinBy) == toupper("row.names")) {
    DF <- transform(tmp, row.names = Row.names, Row.names = NULL)
    DF <- data.frame(rowname = rownames(DF), DF, check.names = F)
    rownames(DF) <- 1:nrow(DF)
  }
  
  DF <- DF[!duplicated(DF), ] # keep unique rows
  DF <- DF[, colSums(is.na(DF)) < nrow(DF)] # remove columns where all values are NA - outer join could produce these columns
  
  if (uniqColumn == TRUE){
    cat("\n Searching dupicated column names... ")
    tmp <- table(gsub("\\.[xy]$","", colnames(DF)))
    dupColNames<- names(tmp)[tmp>1]
    if (length(dupColNames) >0){
      cat("\n\tFound ",length(dupColNames),":", dupColNames)
      cat("\n\tOrig. ncol: ",ncol(DF))
      DFuniq<- DF 
      for(n in dupColNames) {
        toRename <- paste0( n, ".x")
        toDel <- paste0( n, ".y")
        idx_x <-grep(toRename, colnames(DFuniq))
        idx_y <-grep(toDel, colnames(DFuniq))
        if (all(DF[,paste0( n, ".x")] == DF[,paste0( n, ".y")])){
          colnames(DFuniq)[idx_x] <- gsub("\\.x","", toRename)            
          DFuniq <- DFuniq[, -idx_y]
        }
      }
      DF <- DFuniq
      cat("\n\tUniq. ncol: ",ncol(DF))
    }
  }
  return(DF)
}

#====================================================================
#' Merge Multi Files
#'
#' Executes mergeMultiFiles.
#'
#' @param filelist Argument for mergeMultiFiles.
#' @param joinType Argument for mergeMultiFiles.
#' @param joinBy Argument for mergeMultiFiles.
#' @param outFile Argument for mergeMultiFiles.
#' @param headerFlag Argument for mergeMultiFiles.
#' @param uniqColumn Argument for mergeMultiFiles.
#'
#' @return Return value produced by mergeMultiFiles.
mergeMultiFiles <- function(filelist,joinType=NA, joinBy=NA, outFile=NA, headerFlag=TRUE, uniqColumn=FALSE){
  total = length(filelist)
  if( total <2){
    return(FALSE)
  }
  
  file1 <- filelist[1]
  n <- 1 
  cat("\n", n, basename(file1))
  for(file2 in filelist[-1]) {
    n <- n+1
    cat(n, basename(file2),"\n")
    if (n==2){
      DF <- Join2Files(joinType, joinBy, file1,file2,headerFlag=headerFlag , uniqColumn=uniqColumn)
    }else{
      DF<- AppendFile2Dat(joinType, joinBy, DF,file2,headerFlag=headerFlag, uniqColumn=uniqColumn)
    }
    if (n == total){ # save intermediate file every 10-times
      write.table(DF, outFile, sep = "\t", quote = F, row.names = F, col.names = T)
      cat(paste0("\n",basename(outFile),"[",nrow(DF), "x", ncol(DF), ",saved]\n"))
    }
  }
  if (file.exists(outFile)){
    if (file.info(outFile)$size >0){
      return(F)
    }
  }
  cat("\nWarn: Failed to merge files.\n")
  return(FALSE)
}



#============================================================================

# UI
#' Mdl Merge Table UI
#'
#' Executes mdlMergeTableUI.
#'
#' @param id Argument for mdlMergeTableUI.
#'
#' @return Return value produced by mdlMergeTableUI.
mdlMergeTableUI <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    fluidRow(width=12,
             column(width=10,
                    h3(" Upload Matrix File(s)"),
                    fileInput(ns("file"), "Choose your file(s) [tab-delimited text]",                                                
                              multiple = T,
                              accept = c("text/tsv",
                                         "text/tab-separated-values,text/plain",
                                         ".tsv"))       
             )
    ), 
    fluidRow(width=12,
             column(width=10,
                    h3("Input Files"),
                    div(style = 'overflow-y: scroll', DT::dataTableOutput(ns("infileTable"), width = "80%")),
                    
                    actionButton(ns("btnPreview1"), "Preview Selected file",icon=icon("eye"),style = 'margin-top:25px;margin-bottom:25px', class = "btn-info"),
                    actionButton(ns("btnDelFiles1"), "Delete Selected file(s)",icon=icon("file-circle-minus"),style = 'margin-top:25px;margin-bottom:25px', class = "btn-danger")
                    
             )),
    fluidRow(width=12,
             column(width=10,
                    h3("Preview Window 1"),
                    div(style = 'overflow-y: scroll', DT::dataTableOutput(ns("prevTable1"), width = "80%"))
                    
             ),
             
    ),
    fluidRow(width=12,
             column(width=4,
                    fluidRow(width=12,
                             column(width=12,
                                    h3("Merge Files by colum name(s)"),
                                    radioButtons(inputId=ns("mergeOption"), label="Option",
                                                 choices = c("All files with a common column name"="common", "Two files with diferent column names"="diff"), inline=F, selected=NULL)
                             )
                      ), 
                    fluidRow(width=12, 
                             column(width=12,  
                                    selectInput(inputId = ns("joinType"),"choose join type", choices=c("left"="left","right"="right",
                                                                                                       "inner"="inner", "outer"="outer"
                                                                                                     ),selected = "inner"),#"cross"="cross",
                             )
                     ),
                    fluidRow(width=12,
                             column(width=12,
                                    checkboxInput(inputId=ns("uniqColumnFlag"), "remove duplicate columns in merged table", value=TRUE)
                                    
                             )
                     )       
             ),
             column(width=6,
                    uiOutput(ns("colNameSetting"))
             )
    ),
    fluidRow(width=12,
             column(width=6,
                    disabled(actionButton(ns("btnMergeMulti"), "MergeMulti",icon=icon("file-lines"),style = 'margin-top:25px;margin-bottom:25px', class = "btn-warning")),                
                    disabled(actionButton(ns("btnMergeTwo"), "MergeTwo",icon=icon("copy"),style = 'margin-top:25px;margin-bottom:25px', class = "btn-warning"))
             )
    ),
    fluidRow(width=12,
             column(width=10,
                    h3("Output Files"),
                    div(style = 'overflow-y: scroll', DT::dataTableOutput(ns("outfileTable"), width = "80%")),
                    
                    actionButton(ns("btnPreview2"), "Preview Selected file",icon=icon("eye"),style = 'margin-top:25px;margin-bottom:25px', class = "btn-success"),
                    downloadButton(ns("btnDownloadData"), "Download Selected file",icon=icon("download"),style = 'margin-top:25px;margin-bottom:25px', class = "btn-warning"),
                    actionButton(ns("btnRenameFile"), "Rename Selected file",icon=icon("file"),style = 'margin-top:25px;margin-bottom:25px', class = "btn-success"),
                    actionButton(ns("btnDelFiles2"), "Delete Selected file(s)",icon=icon("file-circle-minus"),style = 'margin-top:25px;margin-bottom:25px', class = "btn-danger")
                    
             )),
    fluidRow(width=12,
             column(width=10,
                    h3("Preview Window 2"),
                    div(style = 'overflow-y: scroll', DT::dataTableOutput(ns("prevTable2"), width = "80%"))
                    
             ),
             
    ),
    

  )
}
#============================================================================
# sever
#' Mdl Merge Table
#'
#' Executes mdlMergeTable.
#'
#' @param input Argument for mdlMergeTable.
#' @param output Argument for mdlMergeTable.
#' @param session Argument for mdlMergeTable.
#' @param updir Argument for mdlMergeTable.
#' @param outdir Argument for mdlMergeTable.
#' @param projdir Argument for mdlMergeTable.
#'
#' @return Return value produced by mdlMergeTable.
mdlMergeTable <- function(input, output, session, updir, outdir, projdir) {
  ns <- session$ns   # this line is required if there are any  UI elements dynamically-generated in server 
  dir.create(updir, showWarnings = FALSE, recursive = T)
  dir.create(outdir, showWarnings = FALSE, recursive = T)
  rv <- reactiveValues(infiles = data.frame(), outfiles=data.frame())
  
  updir <- normalizePath(updir)
  outdir <- normalizePath(outdir)
  projdir <- normalizePath(projdir)
  #=========================================================
  
  #' Check Files2 DF
  #'
  #' Executes CheckFiles2DF.
  #'
  #' @param path Argument for CheckFiles2DF.
  #' @param pattern Argument for CheckFiles2DF.
  #' @param recursive Argument for CheckFiles2DF.
  #'
  #' @return Return value produced by CheckFiles2DF.
  CheckFiles2DF <- function(path, pattern="*.*",recursive=FALSE){
    filelist <- dir(path, pattern = pattern,full.names=TRUE,recursive=recursive)  
    if (length(filelist)==0){ return (NULL)}
    DF <- data.frame(
      filename = basename(filelist), 
      size = file.info(filelist)$size,
      filepath = filelist,
      stringsAsFactors = FALSE
    )
    DF
  }
  
  #=========================================================
  
  rv$infiles <- CheckFiles2DF(path=updir)
  rv$outfiles <- CheckFiles2DF(path=outdir)
  
  #=========================================================
  
  # Update reactiveValues when a new file is uploaded
  observeEvent(input$file, {
    filename = input$file$name
    filepath = input$file$datapath
    new_filename = paste0(updir, "/",filename)
    new_filepath = paste0(basename(updir), "/",filename)
    file.copy(filepath, new_filename)
    rv$infiles <- CheckFiles2DF(path=updir)
    rv$infiles
  })
  #=========================================================
  
  observeEvent(input$select_button, {
    print(input$select_button)
    selectedRow <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
    cat( paste('click on ',rv$infiles[selectedRow,1]))
  })
  
  #=========================================================
  
  output$infileTable <- renderDT({
    df <- rv$infiles
    datatable(df, editable = F,      
              options = list(
                columnDefs = list(
                  list(visible = FALSE, targets = c(3))  # Hide the third column (index 3)
                )
              ))  
  }) 
  
  #=========================================================
  output$outfileTable <- renderDT({
    df <- rv$outfiles
    print(df)
    datatable(df, editable = F,      
              options = list(
                columnDefs = list(
                  list(visible = FALSE, targets = c(3))  # Hide the third column (index 3)
                )
              ))  
  }) 
  
  #=========================================================
  
  merged_data <- reactive({
    req(input$files, input$common_column)
    
    # Read files into a list of data frames
    dfs <- lapply(input$files$datapath, read.delim)
    
    # Merge data frames
    merged_df <- Reduce(function(x, y) merge(x, y, by = input$common_column), dfs)
    
    return(merged_df)
  })
  
  #=========================================================
  #=========================================================
  
  observeEvent(input$btnMergeMulti,{
    timeStamp<- substr(strtrim(gsub("[-: ]","",Sys.time()),16),5,12) #"02041842"
    outFile <- paste0(outdir,"/merged_",timeStamp,".txt")
    filelist <- rv$infiles[as.numeric(input$infileTable_rows_selected),"filepath"]
    cat("\nsIColName1:",input$sIColName1)
    cat("\n:joinType",input$joinType,"\n")
    if (!is.null(input$sIColName1) & ! is.null(input$joinType)){
      mergeMultiFiles(filelist,joinBy=input$sIColName1,joinType=input$joinType, outFile=outFile,uniqColumn=input$uniqColumnFlag)
      rv$outfiles <- CheckFiles2DF(path=outdir)
      shinyalert(title="Infomation!",timer = 5000, text=paste("Merged multiple files sucessfully.\n Save table as: ",basename(outFile)),type="info", confirmButtonCol="#5bc0de") 
    }
    
  })
  #=========================================================
  
  observeEvent(input$btnMergeTwo,{
    timeStamp<- substr(strtrim(gsub("[-: ]","",Sys.time()),16),5,12) #"02041842"
    outFile <- paste0(outdir,"/merged_",timeStamp,".txt")
    filelist <- rv$infiles[as.numeric(input$infileTable_rows_selected),"filepath"]
    if (!is.null(input$sIColName1) & !is.null(input$sIColName2)  & ! is.null(input$joinType) & length(filelist)==2) {
      MergeTwoFiles(joinType=input$joinType, colNameX = input$sIColName1,colNameY = input$sIColName2,
                    file1=filelist[1], file2=filelist[2], outFile=outFile,
                    headerFlag=TRUE,uniqColumn=input$uniqColumnFlag)
      rv$outfiles <- CheckFiles2DF(path=outdir)
      shinyalert(title="Infomation!",timer = 5000, text=paste("Merged two files sucessfully.\n Save table as: ",basename(outFile)),type="info", confirmButtonCol="#5bc0de")
    }
    
  })
  
  
  #========================================================================
  observeEvent(input$btnPreview1,{
    if (!is.null(input$infileTable_rows_selected)) {
      inFiles <- rv$infiles[as.numeric(input$infileTable_rows_selected),"filepath"]
      inFile <- unlist(inFiles)[1]
      if(length(inFile)==1){
        dat <- read.table(inFile, sep="\t",header=TRUE,fill=TRUE,stringsAsFactors = FALSE, quote="",row.names=NULL ,check.names=FALSE,comment.char = "" )
        output$prevTable1 <- renderDT({
          datatable(head(dat), editable = F)  
        }) 
      }
    }
  })
  #=================================================================
  #=================================================================
  
  observeEvent(input$btnDelFiles1, {
    selected_rows <- input$infileTable_rows_selected
    if (length(selected_rows) > 0) {
      shinyalert(
        title = "Are you sure you want to delete selected file(s)? ",
        text = "You will not be able to recover deleted file(s)!",
        type = "warning",
        showCancelButton = TRUE,
        confirmButtonCol = '#DD5600',
        confirmButtonText = 'Yes, delete it!',
        callbackR = function(x) {
          if(x){
            selected_files <- rv$infiles[as.numeric(input$infileTable_rows_selected),"filepath"]
            unlink(selected_files, force = TRUE)   
            shinyalert(title="Infomation!",timer = 5000, text=paste("File(s) deleted sucessfully.\n "),type="info", confirmButtonCol="#5bc0de") 
            rv$infiles <- rv$infiles[-selected_rows, , drop = FALSE]
          }}
      )
    }
  })
  #=================================================================
  
  observeEvent(input$btnDelFiles2, {
    selected_rows <- input$outfileTable_rows_selected
    if (length(selected_rows) > 0) {
      shinyalert(
        title = "Are you sure you want to delete selected file(s)? ",
        text = "You will not be able to recover deleted file(s)!",
        type = "warning",
        showCancelButton = TRUE,
        confirmButtonCol = '#DD5600',
        confirmButtonText = 'Yes, delete it!',
        callbackR = function(x) {
          if(x){ # if click yes, 
            selected_files <- rv$outfiles[as.numeric(input$outfileTable_rows_selected),"filepath"]
            unlink(selected_files, force = TRUE)   
            shinyalert(title="Infomation!",timer = 5000, text=paste("File(s) deleted sucessfully.\n "),type="info", confirmButtonCol="#5bc0de") 
            rv$outfiles <- rv$outfiles[-selected_rows, , drop = FALSE]
          }}
      )
    }
  })
  #==============================================================
  observeEvent(input$btnPreview2,{
    if (!is.null(input$outfileTable_rows_selected)) {
      inFiles <- rv$outfiles[as.numeric(input$outfileTable_rows_selected),"filepath"]
      inFile <- unlist(inFiles)[1]
      if(length(inFile)==1){
        dat <- read.table(inFile, sep="\t",header=TRUE,fill=TRUE,stringsAsFactors = FALSE, quote="",row.names=NULL ,check.names=FALSE,comment.char = "" )
        output$prevTable2 <- renderDT({
          datatable(head(dat), editable = F)  
        }) 
      }
    }
  })
  #====================================================================
  output$btnDownloadData <- downloadHandler (
    filename = function() {
      timestamp <- gsub("[ :]",".",strtrim(Sys.time(),22))
      fn <- paste("RNAseqV2.3x_downloaded_",timestamp,".zip", sep="")
      fn
    },
    content = function(filename) {
      selected <- input$outfileTable_rows_selected
      if (is.null(selected) || length(selected) == 0) {
        note <- tempfile(pattern = "table_merger_selection_", fileext = ".txt")
        writeLines("No output files were selected for download.", con = note)
        zip::zipr(zipfile = filename, files = note)
        return(invisible(NULL))
      }

      files2zip <- rv$outfiles[as.numeric(selected), "filepath"]
      files2zip <- unique(as.character(files2zip[file.exists(files2zip)]))

      if (length(files2zip) == 0) {
        note <- tempfile(pattern = "table_merger_missing_", fileext = ".txt")
        writeLines("Selected files are no longer available on disk.", con = note)
        zip::zipr(zipfile = filename, files = note)
      } else {
        zip::zipr(zipfile = filename, files = basename(files2zip), root = outdir, recurse = FALSE)
      }
    },
    contentType = "application/zip"
  )
  
  #====================================================================
  observeEvent(c(input$infileTable_rows_selected,input$mergeOption),{
    num_selected <- length(input$infileTable_rows_selected)    
    if (is.null(input$infileTable_rows_selected) | num_selected <2) {
      shinyjs::disable("btnMergeMulti")
      shinyjs::disable("btnMergeTwo")
    }else{
      if (input$mergeOption =="diff"){
        shinyjs::disable("btnMergeMulti")    
        if (num_selected == 2 ) {
          shinyjs::enable("btnMergeTwo")
          
        }else{
          shinyjs::disable("btnMergeTwo")
        }
      } 
      if (input$mergeOption =="common"){
        shinyjs::disable("btnMergeTwo")    
        if (num_selected >= 2 ) {
          shinyjs::enable("btnMergeMulti")
          
        }else{
          shinyjs::disable("btnMergeMulti")
        }
      } 
      inFiles <- rv$infiles[as.numeric(input$infileTable_rows_selected),"filepath"]
      inFile1 <- unlist(inFiles)[1]
      inFile2 <-  unlist(inFiles)[2]
      cat("\ninFile1:", inFile1)
      cat("\ninFile2:", inFile2)
      if (length(inFile1)==1 & length(inFile2)==1) {
        dat1 <- read.table(inFile1, sep="\t",header=TRUE,fill=TRUE,stringsAsFactors = FALSE, quote="",row.names=NULL ,check.names=FALSE,comment.char = "" )
        dat2 <- read.table(inFile2, sep="\t",header=TRUE,fill=TRUE,stringsAsFactors = FALSE, quote="",row.names=NULL ,check.names=FALSE,comment.char = "" )
  
         output$colNameSetting <- renderUI({     
          label1 <- paste0("Input file1 (left, x):",basename(inFile1))
          label2 <- paste0("Input file2 (right, y):",basename(inFile2))
          if(input$mergeOption =="diff" && exists ("dat2")){
            tagList(
              h3("Choose column name"),br(),
              label1,
              selectInput(inputId = ns("sIColName1"),NULL, choices=colnames(dat1),selected = NULL),
              label2,
              selectInput(inputId = ns("sIColName2"),NULL, choices=colnames(dat2),selected = NULL),
            )          
          }else{
            selectInput(inputId = ns("sIColName1"),"choose common column name", choices=colnames(dat1),selected = NULL)
          }
        })  
      }
    }
  })
  #========================================================================================
  observe({
    if (!is.null(input$outfileTable_rows_selected)) {
      shinyjs::enable("btnDownloadData")
    }else{
      shinyjs::disable("btnDownloadData")
    }
  })
  #===========================================================================================
  observeEvent(input$btnRenameFile,{
    if (!is.null(input$outfileTable_rows_selected)){
      inFile <- rv$outfiles[as.numeric(input$outfileTable_rows_selected),"filepath"]
      
      if(length(inFile) ==1) {
        shinyalert(
          title = "What is your new filename? ",
          type = "input",
          confirmButtonCol = "#2FA4E7",
          callbackR = function(value) {
            if (!is.null(value)) {
              new_name <- paste0(dirname(inFile), "/", value)
              file.rename(inFile, new_name)    
              shinyalert(title="Infomation!",timer = 5000, text=paste("File renamed sucessfully.\n Original name: ",basename(inFile), " \n New name: ",  basename(new_name)),type="info", confirmButtonCol="#5bc0de") 
              rv$outfiles <- CheckFiles2DF(path=outdir)
            }
          }
        )
        
      }
    }
  })
   #
}
#============================================================================
