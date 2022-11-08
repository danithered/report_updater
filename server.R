library(shiny)
library(ssh)
library(DT)

source("functions.R")

#### server function ####
shinyServer(function(input, output) {
    data <- readRDS("data.Rds")
  
   
    #UI parameters
    output$parameters <- renderDataTable({
        extlink <- gsub("/var/www/html/", "http://148.6.202.1/", data$jobs$targetdir)
        inlink <- gsub("/var/www/html/", "http://10.30.0.15/", data$jobs$targetdir)
        data$jobs$links <- paste0("<a href=", extlink, ">[external]</a><a href=", inlink, ">[internal]</a>")
        data$jobs[,c("name", "path", "ssh", "report", "links")]
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
      data$jobs
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
    
    output$reports <- renderUI({
      # fluidPage(
        selectizeInput("report",
                     label = "Which reports would you like to see?",
                     #selectize = F,
                     #options=list(maxOptions=nrow(simul_names)),
                     #size= nrow(simul_names),
                     choices= unique(data$jobs$report)
       )
      
      
      # ,DT::dataTableOutput("par_to_choose")
    })
    
    #pushing knit button
    observeEvent(input$knit, {
      system("nohup Rscript update.R")
    }) #observer
    
    #pushing git button
    observeEvent(input$knit, {
      system("git pull")
    }) #observer

    
})
