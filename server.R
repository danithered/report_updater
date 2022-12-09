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
    dataorig <- readRDS("data.Rds")
    data <- reactiveValues()
    for(val in seq_along(dataorig)) data[[names(dataorig)[val]]] <- dataorig[[val]]
   
    #UI parameters
    output$parameters <- renderDataTable({
        #extlink <- gsub("/var/www/html/", paste0("http://", IPext, "/"), data$jobs$targetdir)
        #inlink <- gsub("/var/www/html/", paste0("http://", IPint, "/"), data$jobs$targetdir)
        #applink <- paste0(
        #  paste0("http://", IPext, "/shiny/apps/mcrsc_examiner/?dir="), 
        #  data$jobs$path,
        #  "&ssh=", data$jobs$ssh)
        #data$jobs$links <- paste0('<a href="', extlink, '" target="_blank">[external]</a>',
        #                          '<a href="', inlink, '" target="_blank">[internal]</a>',
        #                          '<a href="', applink, '" target="_blank">[app]</a>')
        data$jobs[,c("name", "path", "ssh", "report", "targetdir", "description", "updated")]
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
    output$simacts <- renderUI({
      selected = input$parameters_rows_selected
      if(length(selected) == 1){
        fluidRow(
          actionButton("upd", "Update"),
          actionButton("force_upd", "Force update"),
          HTML(paste0('<a href="', gsub("/var/www/html/", paste0("http://", IPext, "/"), data$jobs$targetdir[selected]), '" target="_blank">[external link]</a>')),
          HTML(paste('<a href="', gsub("/var/www/html/", paste0("http://", IPint, "/"), data$jobs$targetdir[selected]), '" target="_blank">[internal link]</a>')),
          HTML(paste0('<a href="', paste0("http://", IPext, "/shiny/apps/mcrsc_examiner/?dir="), 
            data$jobs$path[selected], "&ssh=", data$jobs$ssh[selected], '" target="_blank">[app]</a>'))
        )
      } else {
        fluidRow("Select a row to access more controls")
      }
    })
    
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

    #pushing force update
    observeEvent(input$force_upd, {
      selected = input$parameters_rows_selected
      job = data$jobs[selected,]
      wheretocache = paste(job$targetdir, 
                           "report_cache", 
                           sep=ifelse(nchar(job$targetdir) > 0 & 
                                        substr(job$targetdir, 
                                               nchar(job$targetdir), 
                                               nchar(job$targetdir)) != "/",
                                      "/",
                                      ""))
      showModal(modalDialog(title = "Knitting report", 
                            paste(job$report, "on simulation", job$path, "in FORCED mode"), 
                            footer = NULL)
      )
      try({
        rmarkdown::render(paste0("reports/", job$report),
                        params = list(
                          dir = job$path,
                          ssh = job$ssh,
                          ssh_key = job$ssh_key,
                          force=T,
                          cache.path= wheretocache
                        ),
                        output_dir = job$targetdir,
                        knit_root_dir = job$targetdir,
                        intermediates_dir = job$targetdir,
                        output_file = "index.html")
        data$jobs[selected, "updated"] <- Sys.time()
        saveRDS(reactiveValuesToList(data), "data.Rds")
      })
      removeModal()
    })
    
    observeEvent(input$upd, {
      selected = input$parameters_rows_selected
      job = data$jobs[selected,]
      wheretocache = paste(job$targetdir, 
                           "report_cache", 
                           sep=ifelse(nchar(job$targetdir) > 0 & 
                                        substr(job$targetdir, 
                                               nchar(job$targetdir), 
                                               nchar(job$targetdir)) != "/",
                                      "/",
                                      ""))
      showModal(modalDialog(title = "Knitting report", 
                            paste(job$report, "on simulation", job$path, "in NOT forced mode"), 
                            footer = NULL)
                )
      try({
        rmarkdown::render(paste0("reports/", job$report),
                        params = list(
                          dir = job$path,
                          ssh = job$ssh,
                          ssh_key = job$ssh_key,
                          force=F,
                          cache.path= wheretocache
                        ),
                        output_dir = job$targetdir,
                        knit_root_dir = job$targetdir,
                        intermediates_dir = job$targetdir,
                        output_file = "index.html")
        data$jobs[selected, "updated"] <- Sys.time()
        saveRDS( reactiveValuesToList(data), "data.Rds")
      })
      removeModal()
    })
    
})
