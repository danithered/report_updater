library(shiny)
library(ssh)
library(DT)

source("functions.R")

robust.system <- function (cmd) {
  stderrFile = tempfile(pattern="R_robust.system_stderr", fileext=as.character(Sys.getpid()))
  stdoutFile = tempfile(pattern="R_robust.system_stdout", fileext=as.character(Sys.getpid()))
  
  retval = list()
  retval$exitStatus = system(paste0(cmd, " 2> ", shQuote(stderrFile), " > ", shQuote(stdoutFile)))
  retval$stdout = readLines(stdoutFile)
  retval$stderr = readLines(stderrFile)
  
  unlink(c(stdoutFile, stderrFile))
  return(retval)
}

IPext <- "148.6.202.1"
IPint <- "10.30.0.15"

#### server function ####
shinyServer(function(input, output) {
    data <- readRDS("data.Rds")
  
   
    #UI parameters
    output$parameters <- renderDataTable({
        extlink <- gsub("/var/www/html/", paste0("http://", IPext, "/"), data$jobs$targetdir)
        inlink <- gsub("/var/www/html/", paste0("http://", IPint, "/"), data$jobs$targetdir)
        applink <- paste0(
          paste0("http://", IPext, "/shiny/apps/mcrsc_examiner/?dir="), 
          data$jobs$path,
          "&ssh=", data$jobs$ssh)
        data$jobs$links <- paste0('<a href="', extlink, '" target="_blank">[external]</a>',
                                  '<a href="', inlink, '" target="_blank">[internal]</a>',
                                  '<a href="', applink, '" target="_blank">[app]</a>')
        data$jobs[,c("name", "path", "ssh", "report", "links", "targetdir", "description")]
    }
      , server = TRUE
      , escape=F
      , filter = 'top'
      , options = list( scrollX = TRUE, 
                      colReorder = TRUE, 
                      FixedHeader = TRUE,
                      keys = TRUE#,
                      #deferRender = TRUE,
                      #scrollY = 200,
                      #scroller = F
        )
      , extensions = c('ColReorder', 'FixedHeader')#, 'Scroller')
      , selection=list(mode = 'single', 
                       selected = min(which(data$jobs$targetdir %in% names(data$params))), 
                       target = 'row', 
                       selectable = which(data$jobs$targetdir %in% names(data$params)) )
      , editable=F)
    
    #UI parameters
    output$par_to_choose <- renderDataTable({
      data$sources
    }
      , server = TRUE
      , escape=F
      , filter = 'top'
      , options = list( scrollX = TRUE, 
                      colReorder = TRUE, 
                      FixedHeader = TRUE,
                      keys = TRUE#,
                      #deferRender = TRUE,
                      #scrollY = 200,
                      #scroller = F
        )
      , extensions = c('ColReorder', 'FixedHeader')#, 'Scroller')
      , selection=list(mode = 'single', 
                       selected = min(which(data$jobs$targetdir %in% names(data$params))), 
                       target = 'row', 
                       selectable = which(data$jobs$targetdir %in% names(data$params)) )
      , editable=F)
    
    #UI params for a selectin
    output$par <- renderTable({
      r <- input$parameters_rows_selected
      data$params[[ data$jobs[r,"targetdir"] ]]
    })
    
    output$updatetime <- renderPrint(data$last_updated)
    
    
    #pushing knit button
    observeEvent(input$knit, {
      system("nohup Rscript update.R")
    }) #observer
    
    #pushing git button
    observeEvent(input$git, {
      output$gitout <- renderPrint( paste(robust.system("git pull")) )
      showModal(modalDialog( title= "git pull", easyClose=T,
            verbatimTextOutput("gitout")
      ))
    }) #observer

    
})
