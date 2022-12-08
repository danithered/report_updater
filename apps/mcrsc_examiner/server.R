library(shiny)
library(DT)
library(ggplot2)
library(plotly)
library(RColorBrewer)
library(dplyr)
library(Polychrome)
library(ssh)

#Functions

source("../../functions.R")
source("../../rnarep_plot.R")
source("../../xml.R")
source("../../rnarep.R")

# app specific functions
get_my_data <- function(file, path="", ssh=NA, ssh_key= "~/.ssh/id_rsa"){
  out <- list()
  
  try({
    f <- get_file(file, path, ssh=ssh, ssh_key=ssh_key, fast=T, to=NA)
  
    data <- read_xml(f)
    
    d <- xml_child(data, 1) # mooving to mcrscm
    
    out$time = get_child(d, "time", "int")
    out$size = get_child(d, "sim.size", "int")
    try({
      out$no_last_splits = get_child(d, "sim.no_last_splits", "int")
      })
    
    cells <- get_child(d, "cells")
    
    # get table
    out$table = do.call(rbind, lapply(1:xml_length(cells), function(no_cell){
      cell <- xml_child(cells, no_cell)
      
      out <- as.data.frame(list( alive = get_child(cell, "cell.alive", "logical"),
                                 leftover = get_child(cell, "cell.leftover", "double"),
                                 M = get_child(cell, "metabolism", "double"),
                                 no_reps = get_child(cell, "cell.reps") |> get_child("count", "int")#,
                                 #reps = get_child(cell, "cell.reps")
      ))
      out$reps <- list(get_child(cell, "cell.reps"))
      out
    }))
    
    if(!is.na(ssh)) file.remove(f)
  }) # try
  
  return(out)
}

get_my_xmls <- function(path, ssh=NA, ssh_key="~/.ssh/id_rsa"){
  grep(".xml", get_filelist(path= mergepath(path, "SAVE/"), ssh=ifelse(nchar(ssh) == 0, NA, ssh), ssh_key=ssh_key), value=T)
}

# Get data

rules <- readRDS("rules.RDS")

# Server
shinyServer(function(input, output, session) {
    #setwd("/home/danielred/data/programs/mcrs_to_scm/OUT/A7retest.6_5/SAVE/")
  
    # inic params
    params <- reactiveValues()
    params$cache.path <- "report_cache/"
    params$dir <- "/home/danielred/data/programs/mcrs_to_scm/OUT/A7retest.6_5/"
    params$ssh <- NA
    params$ssh_key <- "~/.ssh/id_rsa"
    params$force <- FALSE
    
    # reads params from URL
    observe({
      query <- parseQueryString(session$clientData$url_search)
      if (length(query) > 0) {
        ws <- names(query)[names(query) %in% names(params)]
        for(w in ws) params[[w]] <- ifelse(nchar(query[[w]]) > 0, query[[w]], NA)
        files(get_my_xmls( params$dir, params$ssh, params$ssh_key))
      }
    })
  
    # inic other vals
    files <- reactiveVal(get_my_xmls( isolate(params$dir), isolate(params$ssh), isolate(params$ssh_key))) 
    tablev <- reactiveVal()
    rep_table <- reactiveVal()
    size <- reactiveVal()
    time <- reactiveVal()
    no_last_splits <- reactiveVal()
    typecols <- reactiveVal()
    actcols <- reactiveVal()
    no_acts <- reactiveVal(0)
    col.pattern <- reactiveVal()
      
    ## UI
    output$reports <- renderUI({
      files()
      fs <- isolate(files())
      names(fs) <- filename(fs)
      nums <- substr(names(fs), 1, nchar(names(fs))-4)
      if(all(!is.na(as.numeric(nums)))){ # if all is numeric
        #names(files) <- sapply(strsplit(names(files), ".", fixed=T), function(x) x[1])
        names(fs) <- as.character(nums)
        selectizeInput("file",
                       label = "Which report would you like to see?",
                       #selectize = F,
                       options=list(maxOptions= length(fs)),
                       choices= fs[order(as.numeric(nums))]
        )
      } else { # there are non numeric ones
        selectizeInput("file",
                       label = "Which report would you like to see?",
                       #selectize = F,
                       options=list(maxOptions= length(fs )),
                       choices= fs
        )
      }
    })
    
    popup <- function(pdir, pssh, pssh_key) modalDialog(
      textInput("path", "Path:", value=pdir),
      textInput("ssh", "SSH:", value=pssh),
      textInput("ssh_key", "PAth to SSH key:", value = pssh_key),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("submit_path", "OK")
      )
    )
    
    # Reading in data
    observeEvent(input$pop, {
      showModal(popup(params$dir, params$ssh, params$ssh_key))
    })
    
    observeEvent(input$submit_path, {
      params$dir <- input$path
      params$ssh <- ifelse(nchar(input$ssh) > 0, input$ssh, NA)
      params$ssh_key <- input$ssh_key
      
      files(get_my_xmls( params$dir, params$ssh, params$ssh_key))
      
      removeModal()
    })
    
    observeEvent(input$file, {
      data <- get_my_data(filename(input$file), path=path(input$file), ssh=params$ssh, ssh_key = params$ssh_key)
      
      if(length(data) > 0){
        no_last_splits(data$no_last_splits)
        time(data$time)
        size(data$size)
        tablev(data$table)
      }
    }) # observeEvent
    
    observeEvent(input$table_rows_selected, {
      # examine reps
      reps = tablev()[as.numeric(input$table_rows_selected),]$reps[[1]]
      
      rep_table (do.call(rbind, lapply(which(children(reps) == "item"), function(rnum){
        rep <- xml_child(reps, rnum)
        out <- list(seq = get_child(rep, "repl.seq", "string"),
                    str = get_child(rep, "str", "string"),
                    mfe = get_child(rep, "repl.mfe", "double"),
                    Pfold = get_child(rep, "repl.Pfold", "double"),
                    Pdeg = get_child(rep, "repl.Pdeg", "double"),
                    R = get_child(rep, "repl.R", "double"),
                    no_sites = get_child(rep, "repl.no_sites", "double"),
                    no_acts = get_child(rep, "repl.no_acts", "double"),
                    type = get_child(rep, "repl.type", "double"),
                    rev_type = get_child(rep, "repl.prev_type", "double")
        )
        acts <- get_child(rep, "activities")
        for(a in 1:xml_length(acts)){
          out[[paste0("act", a)]] <- xml_double(xml_child(acts, a))
        }
        return(as.data.frame(out))
      })))
      
      # create colors
      no_acts_curr <- sum(startsWith(colnames(rep_table()), "act"))
      if(no_acts_curr != no_acts()){ # number of activities changed
        # refresh no_acts
        no_acts(no_acts_curr)
        
        # refresh activity colors
        actcols( brewer.pal(no_acts_curr, "Set1") )
        
        # refresh replicator plot colorization
        pcols = list()
        for(col in actcols()) {
          pcols[[length(pcols)+1]] <- c("red", "coral")
        }
        col.pattern(pcols)
        
        # refresh type colors
        no_types = 2^no_acts_curr-1
        no_types
        set.seed(5464)
        newcols <- c(NA, col2rgb("black"), createPalette(no_types, actcols()))
        names(newcols) <- enzN(-1:no_types, as.text = T)
        newcols[!is.na(type2A(-1:no_types)) & type2A(-1:no_types)==1] <- actcols()
        typecols(newcols)
      }
      
      
    })
    
    ## Tables
    output$table <- renderDataTable( {
      tablev()[, -5] |> mutate_if(is.numeric, round, digits=2)
    }, 
    selection =list(mode = 'single', selected = 1, target = 'row', selectable = T), 
    options = list(
      order = list(list(1, "desc"), list(3, "desc")),
      scrollY = 200,
      scroller = TRUE,
      deferRender = TRUE
    ),
    extensions = "Scroller"
    )
    
    output$reps <- renderDataTable({
      out <- cbind(
        type = enzN(rep_table()$type, as.text=T),
        reverse = enzN(rep_table()$rev_type, as.text = T),
        length = nchar(rep_table()$seq),
        rep_table()[,c("mfe", "Pfold", "Pdeg", "R", "no_sites", "no_acts")],
        rep_table()[, startsWith(colnames(rep_table()), "act")]
      )
      out |> mutate_if(is.numeric, round, digits=2)
    }, 
    selection =list(mode = 'single', selected = 1, target = 'row', selectable = T), 
    options = list(
      scrollY = 200,
      scroller = TRUE,
      deferRender = TRUE
    ),
    extensions = "Scroller"
    )
    
    output$rep_props <- renderTable({
      whichone= input$reps_rows_selected
      t(as.data.frame(c(length=nchar(rep_table()[whichone, "seq"]), rep_table()[whichone, c("mfe", "Pfold", "Pdeg", "R") ]))) |> round(digits=2)
    }, rownames = T, colnames = F)
    
    ## Text outputs
    output$state_data <- renderText(paste0("Sample taken in generation: ", 
                                           time(), 
                                           "\nNumber of cells: ", size(), 
                                           "\nNumber of splitting events in last generation: ",
                                           no_last_splits() )) 
    output$seq <- renderText({
      whichone= input$reps_rows_selected
      rep_table()[whichone, c("seq") ]
    })
    output$str <- renderText({
      whichone= input$reps_rows_selected
      rep_table()[whichone, c("str") ]
    })
    
    ## Plots
    # For state
    output$plot <- renderPlotly( {
      s = input$table_rows_selected
      #plot(tablev()$M, tablev()$no_reps)
      #points(tablev()[s,"M"], tablev()[s, "no_reps"], col="red")
      
      sizes = rep(1,nrow(tablev()))
      sizes[s] <- 4
      ggplotly(ggplot(tablev(), aes(x=M, y= no_reps, color=alive))+
        geom_point(size=sizes )+
        labs(x="Metabolism", y="Number of replicators")+
        theme(legend.pos="none")
      )
    })
    
    # For cell
    output$legend <- renderPlot({
      plot.new()
    })
    
    output$hist_mfe <- renderPlot({
      
      #hist(rep_table()$mfe)
      ggplot(rep_table(), aes(x=mfe, fill=as.factor(type)))+
        geom_histogram()
      
    })
    
    output$hist_Pfold <- renderPlot({
      ggplot(rep_table(), aes(x=Pfold))+
        geom_histogram()+
        scale_fill_manual(values=typecols())+
        theme(legend.position = "none")
    })
    
    output$hist_Pdeg <- renderPlot({
      ggplot(rep_table(), aes(x=Pdeg))+
        geom_histogram()
    })
    
    output$hist_R <- renderPlot({
      ggplot(rep_table(), aes(R, fill=no_acts))+
        geom_histogram()
    })
    
    output$hist_no_sites <- renderPlot({
      ggplot(rep_table(), aes(x=no_sites))+
        geom_histogram()
    })
    
    output$hist_no_acts <- renderPlot({
      ggplot(rep_table(), aes(x=no_acts))+
        geom_histogram()
    })
    
    output$nice <- renderPlot({
      types <- unique(rep_table()$type)
      p <- list(par_noEA=7)
      
      # calculate table
      van <- rep_table()$seq != "N"
      odf <- data.frame(orig=rep_table()$type[van], rev=rep_table()$rev_type[van])
      odf$orig <- factor(odf$orig, levels = 0:(2^as.numeric(p$par_noEA)-1))
      odf$rev <- factor(odf$rev, levels = 0:(2^as.numeric(p$par_noEA)-1))
      pairs <- table(odf)
      
      tv <- as.numeric(colnames(pairs))
      
      kell <- 0:(2^as.numeric(p$par_noEA)-1)

      #reorder by numbers of activities
      kell2 <- kell[order(type2noA(kell))]
      pairs <- pairs[as.character(kell2),]
      pairs <- pairs[,as.character(kell2)]
      
      keep <- apply(pairs, 1, function(x) sum(x) > 0 ) | apply(pairs, 2, function(x) sum(x) > 0 )
      kell <- kell[keep]
      pairs <- pairs[keep, keep]
    
      image(1:sum(keep), 1:sum(keep),  
            pairs, 
            xaxt="n", yaxt="n" ,
            xlab="", ylab="",
            col= heat.colors(100, rev = TRUE)[5:100]
      )
      axis(1, at= 1:sum(keep), labels=enzN(kell), las=2)
      axis(2, at= 1:sum(keep), labels=enzN(kell), las=1)
      text(rep(1:sum(keep), length(1:sum(keep))), rep(1:sum(keep), each=length(1:sum(keep))), c(pairs), cex=0.5 )
      
    })

    # For Replicatro
    output$replicator <- renderPlot({
      par(mar=c(0,0,0,0))
      whichone= input$reps_rows_selected
      
      repl <- rep_table()[whichone, c("seq", "str") ]
      
      sink("NUL") # they made ct2coord to print everything
      coords= ct2coord( makeCt( repl$str, repl$seq) )
      sink()
      
      startofPatterns <- find_activity(repl, coords, rules)
      
      #compute 2D structure and colorisation
      colormask <- make.colormask(repl$str, 
                                  patterns =startofPatterns, 
                                  col=NA, 
                                  col.pattern = col.pattern(), 
                                  col.base = actcols()
      )
      
      plot_RNA(coords,
               border="lightblue",
               #bases
               add_letter = T,
               cex_letter = 0.6,
               col_letter = "black",
               #fill
               col=colormask,
               #rotate it
               #rot=pi/2,
               #connecting lines
               main_con = list(lwd=1, col="darkgrey", lty=1),
               side_con = list(lwd=0.5, col="purple", lty=2)
      )
      
    }) # render replicator
    
    output$acts <- renderPlot({
      par(mar=c(0,0,0,0))
      whichone= input$reps_rows_selected
      
      acts = rep_table()[whichone, ] |> select( starts_with("act"))
      compl_acts = type2vec(rep_table()[whichone, "rev_type"], no_acts())
      plot_acts(acts, compl_acts, col=actcols())
    })
    
})
