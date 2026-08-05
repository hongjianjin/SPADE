library(shiny)
library(DT)

#' Mdl Edit Table UI
#'
#' Executes mdlEditTableUI.
#'
#' @param id Argument for mdlEditTableUI.
#'
#' @return Return value produced by mdlEditTableUI.
mdlEditTableUI <- function(id) {
  ns <- NS(id)
  tagList(
     fluidPage(
       fluidRow(width=12,
                column(width=3,
                       h3("Input"),
                )
       ),
       fluidRow(width=12,
          column(width=10,
            actionButton(ns("btnNewMeta1"), "Metadata Example1 [simple]",style = 'margin-top:15px;margin-bottom:5px'), 
            actionButton(ns("btnNewMeta2"), "Metadata Example2 [batch]",style = 'margin-top:15px;margin-bottom:5px')
          )
      ),
      fluidRow(width=12,    
          column(width=10,
                 actionButton(ns("btnLoadMetaOnServer"), "Load Your Uploaded Metadata",style = 'margin-top:5px;margin-bottom:25px'),
                 actionButton(ns("btnLoadMatIdOnServer"), "Create Metadata From Your Matrix",style = 'margin-top:5px;margin-bottom:25px')
          )
                
       ),
       fluidRow(width=12,
          column(width=6,
            fileInput(ns("infile1"), "Upload metadata file [tab-delimited text format]",
                                                   multiple = F,
                                                   accept = c("text/tsv",
                                                              "text/tab-separated-values,text/plain",
                                                              ".tsv"))
          )
        ),
       fluidRow(width=12,
          
          column(width=12,
          h3("Editable Table "),
          div(style = 'overflow-y: scroll', DT::dataTableOutput(ns("table"), width = "80%"))
         )
       ), 
      fluidRow(width=12,
               column(width=3,
                      h3("Settings"),
               )
      ),  
      fluidRow(width=12,
               column(width=4, 
                      selectInput(ns("sIseparator"), "Separator for split/cat", choices=c("comma","space","underline","dash","colon","semicolon"),selected="underline",multiple =F)
               ),
               column(width=4, 
                      selectInput(ns("sIsplit2n"), "Number of columns to return after split", choices= 2:4,selected=2,multiple =F)
               )
      ),  
      fluidRow(width=12,
                column(width=3,
                  h3("Operations"),
                )
       ),
      fluidRow(width=12,
        column(width=4,        
           actionButton(ns("btnAddRow"), "Add New Row",icon=icon("plus"),style = 'margin-top:15px;margin-bottom:25px', class = "btn-info" )
        ),
        column(width=4,
         actionButton(ns("btnDeleteRows"), "Delete Selected Row(s)",icon=icon("minus"),style = 'margin-top:15px;margin-bottom:25px', class = "btn-danger" )
        )
      ),   
      fluidRow(width=12,
        column(width=4, 
            selectizeInput(inputId=ns("szImetaColName"),label="Column Name(s)",multiple = T,choices = NULL,selected = NULL)
        ),
        column(width=8,
              actionButton(ns("btnDeleteColumns"),  "Delete Column(s)",icon=icon("minus"), style = 'margin-top:25px', class = "btn-danger" ),  
              actionButton(ns("btnSplitColumn"), "Split Column", icon=icon("table-columns"),style = 'margin-top:25px', class = "btn-info"),
              actionButton(ns("btnCatColumns"), "Concatenate Columns",icon=icon("link"),style = 'margin-top:25px', class = "btn-info"),
              actionButton(ns("btnDupColumn"),  "Duplicate Column",icon=icon("paste"), style = 'margin-top:25px', class = "btn-success" ),  
        )
      ),
      fluidRow(width=12,
       column(width=4, 
              textInput(ns("new_colname"), "New Column Name"),
       ),
       column(width=8,    
              actionButton(ns("btnAddColumn"), "Add New Column",icon=icon("plus"),style = 'margin-top:25px;margin-bottom:25px', class = "btn-info"),
              actionButton(ns("btnChgColname"), "Rename Column ",icon=icon("i-cursor"), style = 'margin-top:25px;margin-bottom:25px',class = "btn-info")
       )
      ),
     
    
    fluidRow(width=12,
         column(width=4,
              actionButton(ns("btnValData"), "Validate Metadata",icon=icon("file-circle-check"),style = 'margin-top:25px;margin-bottom:25px', class = "btn-warning")
         ),
         column(width=4,
              actionButton(ns("btnSaveData"), "Apply To Workflow",icon=icon("cloud"),style = 'margin-top:25px;margin-bottom:25px', class = "btn-warning")
        ),
        column(width=4,
             downloadButton(ns("btnDownloadData"), "Save & Download",style = 'margin-top:25px;margin-bottom:25px', class = "btn-warning")
        )
    )
  )
  )
}

#' Mdl Edit Table
#'
#' Executes mdlEditTable.
#'
#' @param input Argument for mdlEditTable.
#' @param output Argument for mdlEditTable.
#' @param session Argument for mdlEditTable.
#' @param inMeta Argument for mdlEditTable.
#' @param inMatId Argument for mdlEditTable.
#' @param metaFile Argument for mdlEditTable.
#' @param CallFun Argument for mdlEditTable.
#'
#' @return Return value produced by mdlEditTable.
mdlEditTable <- function(input, output, session,inMeta=NULL,inMatId=NULL, metaFile=NULL,CallFun=NULL) {
  
  #' Split Column
  #'
  #' Executes SplitColumn.
  #'
  #' @param data Argument for SplitColumn.
  #' @param column_name Argument for SplitColumn.
  #' @param separator Argument for SplitColumn.
  #' @param ncols Argument for SplitColumn.
  #'
  #' @return Return value produced by SplitColumn.
  SplitColumn <- function(data, column_name,separator="_", ncols=2) {
    require(stringr)
    newCols<- str_split_fixed(data[[column_name]],separator,ncols)
    colnames(newCols)  <- paste("split_col", 1:ncols,sep="")
    after_merge <- cbind(data, newCols)
    #after_merge[[column_name]] <- NULL  # to delete original column
    print(after_merge)
    after_merge
  }
  #' Get Separator
  #'
  #' Executes GetSeparator.
  #'
  #' @param sep Argument for GetSeparator.
  #'
  #' @return Return value produced by GetSeparator.
  GetSeparator <- function(sep){
    separator <- switch(sep,
                     "comma" = ",",
                     "space" = " ",
                     "underline" = "_",
                     "dash" = "-",
                     "colon"= ":",
                     "semicolon"= ";"
                     )

    return (separator)
  }
  
  data <- reactiveValues() #reactiveValues(df = df)
  df <- reactive({
      if(!is.null(inMeta())){
        inMeta() 
      }else if(!is.null(inMatId())){
        inMatId()
      }else{
        NULL
      }
    })
  #----------------------------------------------------
  observeEvent(input$btnNewMeta1,{
    data$df  = data.frame(ID = c("100_KO_rep1", "101_KO_rep2", "102_KO_rep3", "103_WT_rep1","104_WT_rep2","105_WT_rep3"), 
                     NEWNAME = c("100_KO_rep1", "101_KO_rep2", "102_KO_rep3", "103_WT_rep1","104_WT_rep2","105_WT_rep3"),
                     GROUP = c("KO", "KO", "KO", "WT","WT","WT"))
    output$table <- renderDT({
      datatable(data$df, editable = TRUE) #, rownames = FALSE
    })  
    refreshInputs()
  })
  
  #----------------------------------------------------
  observeEvent(input$btnLoadMetaOnServer,{
    if(!is.null(df())){
        data$df <- df()
        output$table <- renderDT({
            datatable(data$df, editable = TRUE) #, rownames = FALSE
         })  
        refreshInputs()
    }else{
        shinyalert(title="Information!",timer = 5000, text=paste0("No active metadata.\nPlease upload your metadata file for operations."),type="info", confirmButtonCol="#5bc0de")
    }
  })
  
  #----------------------------------------------------
  observeEvent(input$btnLoadMatIdOnServer,{
    if(!is.null(inMatId())){
      data$df <- inMatId()
      output$table <- renderDT({
        datatable(data$df, editable = TRUE) #, rownames = FALSE
      })  
      refreshInputs()
    }else{
      shinyalert(title="Information!",timer = 5000, text=paste0("No active matrix.\nPlease upload your matrix file in [workflow -> Input]."),type="info", confirmButtonCol="#5bc0de")
     }
  })
  
  #----------------------------------------------------
  #----------------------------------------------------
  proxy = dataTableProxy('table')

  updateSelectInput(session, 'szImetaColName',
                       choices = colnames(df),
                       selected = character(0))
  #----------------------------------------------------
  #' Refresh Inputs
  #'
  #' Executes refreshInputs.
  #'
  #' @return Return value produced by refreshInputs.
  refreshInputs <- function(){
    updateSelectInput(session, 'szImetaColName',
                      choices = colnames(data$df),
                      selected = character(0))
    meta_ready(TRUE)
  }
  #----------------------------------------------------
  observeEvent(input$infile1$datapath, {
    req(input$infile1)
    meta <- read.table(input$infile1$datapath,
                       fill=TRUE, check.names = FALSE,
                       header = TRUE,
                       sep = "\t",
                       quote = NULL)  
    colnames(meta) <- toupper(colnames(meta))
    data$df <- meta
    meta_ready(TRUE)
    output$table <- renderDT({
      datatable(data$df, editable = TRUE) #, rownames = FALSE
    })  
    refreshInputs()
  })
    
  #----------------------------------------------------
  observeEvent(input$table_cell_edit, {
    info = input$table_cell_edit
    i = info$row
    j = info$col
    v = info$value
    cat("\ni=",i,", j=",j,", v=",v)
    if (is.null(data$df[i, j])) {
      # Handle NULL values, e.g., assign a default value of UNKNOWN
      data$df[i, j] <- "UNKNOWN"
    } else{
      data$df[i, j] <- DT::coerceValue(v, data$df[i, j])
    }
     replaceData(proxy, data$df, resetPaging = FALSE)
  })
  #----------------------------------------------------
  observeEvent(input$btnNewMeta2,{
    data$df  = data.frame(ID = c("100_KO_batch1_rep1", "101_KO_batch1_rep2",  "102_WT_batch1_rep1","103_WT_batch1_rep2","201_KO_batch2_rep1","202_KO_batch2_rep2","203_WT_batch2_rep1","204_WT_batch2_rep2"), 
                          NEWNAME = c("100_KO_batch1_rep1", "101_KO_batch1_rep2","102_WT_batch1_rep1","103_WT_batch1_rep2", "201_KO_batch2_rep1","202_KO_batch2_rep2","203_WT_batch2_rep1","204_WT_batch2_rep2" ),
                          GROUP = c("KO", "KO", "WT","WT","KO","KO", "WT","WT"),
                          BATCH = c("b1", "b1", "b1", "b1","b2","b2","b2","b2")
                          )
    output$table <- renderDT({
      datatable(data$df, editable = TRUE) #, rownames = FALSE
    })  
    refreshInputs()
  })
  #----------------------------------------------------
  data_ready <- reactiveVal(FALSE)
  meta_ready <- reactiveVal(FALSE)
  
  observe({
    if (meta_ready()){
      shinyjs::enable("btnValData")
    }else{
      shinyjs::disable("btnValData")
    }
    if (data_ready()){
      shinyjs::enable("btnSaveData")
      shinyjs::enable("btnDownloadData")
    }else{
      shinyjs::disable("btnSaveData")
      shinyjs::disable("btnDownloadData")
    }    
  })
  
  observeEvent(input$btnSaveData,{
    if (!is.null(metaFile) && is.data.frame(data$df) && nrow(data$df) > 0) {
       write.table(data$df,metaFile, sep="\t",quote=FALSE,row.names=FALSE, col.names=TRUE)
    }
    if (file.exists(metaFile)){
      cat("\ninfo:", metaFile, "[saved]\n")
      if(!is.null(CallFun)){
        CallFun(metaFile)
        shinyalert(title="Information!",timer = 3000, text=paste0("Metadata has been saved and loaded to your Workflow."),type="success", confirmButtonCol="#73A839")
      }else{
        shinyalert(title="Information!",timer = 3000, text=paste0("Metadata has been saved on server."),type="success", confirmButtonCol="#73A839")
      }

    }
  })
  
  #----------------------------------------------------
  observeEvent(input$btnValData,{
    meta <- data$df
    anyErrors <- 0;  anyWarnings <- 0;  msg  <- NULL
    if (! all(c( "ID",	"NEWNAME",	"GROUP") %in% colnames(meta))){
      anyErrors <- anyErrors +1   
      msg <-   paste0(msg , "\nError #",anyErrors,": ID, NEWNAME or GROUP column is missing!")
    }
    if (any(duplicated(meta$ID))){
      anyErrors <- anyErrors +1
      msg <-   paste0(msg , "\nError #",anyErrors,": Found non-unique IDs!")
    }
    if (any(duplicated(meta$NEWNAME))){
      anyErrors <- anyErrors +1
      msg <-  paste0(msg ,  "\nError #",anyErrors,": Found non-unique NEWNAME!")
    }
    
    if (any(str_detect(meta$ID,'[[^[:alnum:]^[-_.]]]'))){
      anyErrors <- anyErrors +1
      msg <-   paste0(msg ,"\nError #",anyErrors,": Found non alphanumeric characters in IDs!")
    }
    if (any(str_detect(meta$NEWNAME,'[[^[:alnum:]^[-_.]]]'))){
      anyErrors <- anyErrors +1
      msg <-   paste0(msg , "\nError #",anyErrors,": Found non alphanumeric characters in NEWNAMEs!")
    }

      if (sum(is.na(meta$GROUP)) >0 |sum(meta$GROUP =="") >0){
        anyErrors <- anyErrors +1
        msg <- paste0(msg , "\nError #",anyErrors,": detect NA(s) in GROUP column!")
      }else{
        if (any(str_detect(meta$GROUP,'[[^[:alnum:]^[-_.]]]'))){
          anyErrors <- anyErrors +1
          msg <- paste0(msg , "\nError #",anyErrors,": Found non alphanumeric characters in GROUPs!")
        }     
         if (length(unique(meta$GROUP)) == 1 ){
            anyErrors <- anyErrors +1
            msg <- paste0(msg , "\nError #",anyErrors,": Found only one GROUP in meta data file. Need at least two for a pairwise comparison!")
          }
          if (min(table(meta$GROUP))<2){
            anyWarnings <- anyWarnings + 1
            msg <- paste0(msg , "\nWarning #",anyErrors,": techinically need at least two replicates for each group!")
          }     
      }

   
    if (anyErrors >0 ){
        data_ready(FALSE)
         cat("\n** Invalid meta data file?  [", anyErrors ," errors; ",anyWarnings," warnings ] **")
         shinyalert(title="Error!",timer = 5000+anyErrors*1000, text=paste0(msg, "\nPlease keep working on meta data file. \nOnly alphanumeric characters and [-_.] are acceptable in ID/NEWNAME/GROUP columns."),type="error", confirmButtonCol="#C71C22")
    }else if ( anyWarnings >0 ){
        data_ready(TRUE)
        cat("\n** meta data file is acceptable.[", anyWarnings," warnings ] **")
        shinyalert(title="Information!",timer = 5000+anyWarnings*1000, text=paste0(msg, "\nYou could download meta data file for analysis. \nOnly alphanumeric characters and [-_.] are acceptable in ID/NEWNAME/GROUP columns."),type="warning", confirmButtonCol="#DD5600")
    }else{
        data_ready(TRUE)
        cat("\n** Perfect meta data file. [no error; no warning ] **")
        shinyalert(title="Good Job!",timer = 5000, text=paste0(msg, "\nYour metadata file is ready for \n[Apply to workflow] or [Save & Download]."),type="success", confirmButtonCol="#73A839")
      
    }
  })
  
  #----------------------------------------------------
  output$btnDownloadData <- downloadHandler(
    filename = function() {
      paste("RNAseqV2.3x_metadata_", Sys.Date(), ".txt", sep="")
    },
    content = function(file) {
      write.table(data$df, file, sep="\t", row.names = FALSE,quote=FALSE, col.names=TRUE )
    }

  )
  #----------------------------------------------------
  observeEvent(input$btnAddRow, {
    data$df [nrow(data$df )+1,] <- ""
    replaceData(proxy, data$df, resetPaging = FALSE)
  })

  #----------------------------------------------------
  observeEvent(input$btnAddColumn, {
    data$df <- cbind(data$df, NewColumn = "")
    if(input$new_colname !=""){
      colnames(data$df)[ncol(data$df)] <- input$new_colname
    }
    colnames(data$df ) <- make.unique(colnames(data$df ) )
    replaceData(proxy, data$df, resetPaging = FALSE)
    refreshInputs()
  })
  #----------------------------------------------------
  observeEvent(input$btnCatColumns, {
    # Make sure the columns exist before trying to concatenate
      colNames <- intersect(input$szImetaColName, colnames(data$df))
      cat("\nuser selected:",colNames)
      sep <- GetSeparator(input$sIseparator)
      data$df$Concatenated = apply(data$df[, colNames, drop = F], MARGIN = 1, FUN = function(i) paste(i, collapse = sep))
      replaceData(proxy, data$df, resetPaging = FALSE)
      refreshInputs()
  })
  #----------------------------------------------------------------
  observeEvent(input$btnChgColname, {
    if (input$szImetaColName %in% names(data$df)) {
      names(data$df)[names(data$df) == input$szImetaColName ] <- input$new_colname
      replaceData(proxy, data$df, resetPaging = FALSE)
      refreshInputs()
    }
  })
  #----------------------------------------------------------------
  observeEvent(input$btnDeleteRows,{
    if (!is.null(input$table_rows_selected)) {
      data$df <- data$df[-as.numeric(input$table_rows_selected),]
    }
  })
  #----------------------------------------------------------------
  observeEvent(input$btnDeleteColumns,{
    if (!is.null(input$szImetaColName)) {
      data$df[input$szImetaColName] <- NULL
      replaceData(proxy, data$df, resetPaging = FALSE)
      refreshInputs()
    }

  })
  
  #----------------------------------------------------------------

  observeEvent(input$btnSplitColumn, {
    sep <- GetSeparator(input$sIseparator)
    n <-  as.integer(input$sIsplit2n)
    if(length(input$szImetaColName) ==1){
      data$df <- SplitColumn(data$df, input$szImetaColName,separator =sep, ncols=n)
      replaceData(proxy, data$df, resetPaging = FALSE)
      refreshInputs()
    } 
  })  
  #----------------------------------------------------------------
  observeEvent(input$btnDupColumn, {
    if (!is.null(input$szImetaColName) && length(input$szImetaColName) ==1){
      data$df <- cbind(data$df, NewColumn = "")
      colnames(data$df)[ncol(data$df)] <- paste0(input$szImetaColName, "_duplicate")
      data$df[,ncol(data$df)] <- data$df[,  input$szImetaColName]
      #data$df <- data$df[, paste0(input$szImetaColName, "_duplicate") := get(input$szImetaColName)]
      replaceData(proxy, data$df, resetPaging = FALSE)
      refreshInputs()
      
    }
  }) 
  
  #----------------------------------------------------------------
  
  
}
  
#--------------------------------------------------------
# for test/demo purpose
#===ui =====
# mdlEditTableUI("editableTable1")

#projSpace <- getwd()
#timeStamp<- substr(strtrim(gsub("[-: ]","",Sys.time()),16),5,12) #"11301304"
#editMetaFile= paste0(projSpace,"/meta_tmp_",timeStamp,".txt")

#===server====== 
#  callModule(mdlEditTable, "editableTable1",df=NULL,toy=NULL,metaFile=editMetaFile)
