library(shiny)
library(ssh)

source("functions.R")

#### set default vals ####
online <- NULL

rootdir="/home/danielred/data/programs/mcrs_chromosome/OUT/"

remote_dirs <- data.frame(name=c("eti", "eti2"),
                          #name=c("eti", "eti2", "dani"),
                          #address = c("danielred@10.30.0.12", "danielred@10.30.0.14", "danielred@10.30.0.15"),
                          address = c("danielred@148.6.202.1:22023", "danielred@148.6.202.1:22025"),
                          key="~/.ssh/id_rsa", 
                          dir="/home/danielred/data/programs/mcrs_chromosome/OUT")

if(!dir.exists("outputs")) dir.create("outputs")






#### server function ####
shinyServer(function(input, output) {
    wheretolook <- read.table("imports.tsv", sep="\t", header=T)
    
  
    
        upd <- data.frame(id= character(), 
                          output_hash= character(), 
                          abrmax=numeric(),
                          abrmin=numeric(),
                          kompl=logical(),
                          remote=character()
                          )
    
    #get simulation IDs
    simul_names <- data.frame(
        names= list.dirs("/home/danielred/data/programs/mcrs_chromosome/OUT", recursive = F, full.names = F),
        remote=NA
        )
    for(rem in remote_dirs$name){
        simul_names <- rbind(simul_names, data.frame(names=get_remote_dirs(
                        address = remote_dirs[remote_dirs$name == rem, "address"],
                        key = remote_dirs[remote_dirs$name == rem, "key"],
                        path = remote_dirs[remote_dirs$name == rem, "dir"],
                        slash=F
                        ),
                   remote=rem
        ))
    }
    simul_names$label <- ifelse(is.na(simul_names$remote), simul_names$names, paste0("[", simul_names$remote, "] ", simul_names$names))
    
    #UI komlementer button (is it on or off)
    output$kompl <- renderUI({
        checkboxInput("kompl",
                      label= "Compute complemeters (lots of time)",
                      value= ifelse( input$report %in% upd$id, 
                                     as.logical(upd[upd$id == input$report, "kompl"]),
                                     FALSE
                             )
        )
    })
    
    #UI list of outputs 
    output$reports <- renderUI({
        selectizeInput("report",
                    label = "Which report would you like to see?",
                    #selectize = F,
                    options=list(maxOptions=nrow(simul_names)),
                    #size= nrow(simul_names),
                    choices= simul_names$label
        )
    })
    
    #UI parameters
    output$parameters <- renderTable({
      if( is.na(simul_names[simul_names$label == input$report, "remote"]) ){ # local
        read.table(paste0(rootdir, 
                         #"/", 
                         input$report, 
                         "/SAVE/parameters.txt"), header=F, sep="\t")
      } else { #remote
        try({ 
          #connecting to ssh
          ssh_con <- ssh_connect(remote_dirs[remote_dirs$name == simul_names[simul_names$label == input$report, "remote"], "address"], 
                                 keyfile = remote_dirs[remote_dirs$name == simul_names[simul_names$label == input$report, "remote"], "key"])
          
          #download
          tdir = tempdir()
          scp_download(ssh_con, 
                       files= paste0(remote_dirs[remote_dirs$name == simul_names[simul_names$label == input$report, "remote"], "dir"], 
                                      "/", 
                                     simul_names[simul_names$label == input$report, "names"], 
                                      "/SAVE/parameters.txt"),
                       to=tdir)
          ssh_disconnect(ssh_con)
          
          #process
          if(!file.exists(paste0(tdir, "/parameters.txt"))) 
            warning("parameters file does not exist\n")
          t <- readLines(paste0(tdir, "/parameters.txt"))
          t <- read.table( text = sub(" ", "\t", t) , header=F, sep="\t")
          p <- as.list(as.character(t$V2))
          names(p) <- t$V1
          
          #delete temp file
          file.remove(paste0(tdir, "/parameters.txt"))
        })
        
        #rd <- remote_dirs[remote_dirs$name == simul_names[simul_names$label == input$report, "remote"], ]
        #times <- get_remote_files(address= rd$address, 
        #                          key=rd$key, 
        #                          path=rd$dir,
        #                          path2=paste0("/", simul_names[simul_names$label == input$report, "names"], "/SAVE" ),
        #                          pattern = "*.tsv"
        #) |> strsplit(split=".", fixed=T) |> unlist()
        #paste(names(p), p, sep="=", collapse="\n\t")
        t
      }
    }, colnames = F)
    
    #UI time slider
    output$scale <- renderUI({
        #get times
        if( is.na(simul_names[simul_names$label == input$report, "remote"]) ){
            times <- unlist(strsplit(list.files(paste0(rootdir, 
                                                       "/", 
                                                       input$report, 
                                                       "/SAVE"), "*.tsv"), 
                                     ".", 
                                     fixed=T)
                            )
        } else {
            rd <- remote_dirs[remote_dirs$name == simul_names[simul_names$label == input$report, "remote"], ]
            times <- get_remote_files(address= rd$address, 
                             key=rd$key, 
                             path=rd$dir,
                             path2=paste0("/", simul_names[simul_names$label == input$report, "names"], "/SAVE" ),
                             pattern = "*.tsv"
            ) |> strsplit(split=".", fixed=T) |> unlist()
        }
        
        #make output
        if(is.null(times)){
            warning("times is NULL! (There are no snatshots in directory)\n")
            h3("No snapshot available")
        } else {
            times <- times[seq(1, length(times), 2)]
            times <- times[order(as.numeric(times))]
        
            sliderInput(
                "limits",
                label="Set minima and maxima of snapshots",
                value=ifelse( rep(input$report %in% upd$id,2), 
                             as.numeric(upd[upd$id == input$report, c("abrmin", "abrmax")]),
                             as.numeric(times[c(1, length(times))]) 
                             ),
                min= as.numeric(times[1]),
                max= as.numeric(times[length(times)]),
                step=1
            )
        }
    })
    
    #pushing knit button
    observeEvent(input$knit, {
        #times <- unlist(strsplit(list.files(paste0(rootdir, "/", input$report, "/SAVE"), "*.tsv"), ".", fixed=T))
        #times <- times[seq(1, length(times), 2)]
        #times <- times[order(as.numeric(times))]
        
        
        #check if setting or data has been changed
        if(!input$report %in% upd$id) { # it has never been created before 
            #cat("it has never been created before\n")
            
            # it doesnot even have a dir
            if( !is.na(simul_names[simul_names$label == input$report, "remote"]) ){
                #create dirs
                dir.create(paste0("outputs/", 
                                  simul_names[simul_names$label == input$report, "remote"])
                           )
                dir.create(paste0("outputs/", 
                                  simul_names[simul_names$label == input$report, "remote"], 
                                  "/", 
                                  simul_names[simul_names$label == input$report, "names"])
                           )
                
                #create report
                input2 <- reactiveValuesToList(input)
                input2$report <- simul_names[simul_names$label == input2$report, "names"]
                td = update_report(dir=remote_dirs[remote_dirs$name == simul_names[simul_names$label == input$report, "remote"], "dir"],
                                   input=input2,
                                   ssh=remote_dirs[remote_dirs$name == simul_names[simul_names$label == input$report, "remote"], "address"],
                                   key=remote_dirs[remote_dirs$name == simul_names[simul_names$label == input$report, "remote"], "key"],
                                   outdir= simul_names[simul_names$label == input$report, "remote"]
                                   ) #still needs to fixed
            } else { #it is local
                #create dir
                dir.create(paste0("outputs/", input$report)) 
                
                #create report
                td = update_report(rootdir, input)
            }
            
            

            #create new record at upd
            #browser()
            upd <- rbind(upd, data.frame(id= as.character(input$report),
                                         output_hash= as.character( get_hash( path=paste0(rootdir, input$report, "/"),
                                                                              file="output.csv",
                                                                              ssh=remote_dirs[remote_dirs$name == simul_names[simul_names$label == input$report, "remote"], "address"] ,
                                                                              key=remote_dirs[remote_dirs$name == simul_names[simul_names$label == input$report, "remote"], "key"])),
                                         #output_hash= as.character( "hfghgh" ),
                                         abrmax=as.numeric(input$limits[2]),
                                         abrmin=as.numeric(input$limits[1]),
                                         kompl=as.logical(input$kompl),
                                         remote= simul_names[simul_names$label == input$report, "remote"] 
                                )
            )
            saveRDS(upd, "last_updated.rds") # save it
        } else { # it has been created before
            if( # hash differences
                upd[ upd$id == input$report, "output_hash"] != tools::md5sum(paste0(rootdir, input$report, "/output.csv")) |
                # abrmax differs
                upd[ upd$id == input$report, "abrmax"] != input$limits[2] |
                # abrmin differs
                upd[ upd$id == input$report, "abrmin"] != input$limits[1] |
                # kompl differs
                upd[ upd$id == input$report, "kompl"] != input$kompl
               ){
                    if( !is.na(simul_names[simul_names$label == input$report, "remote"]) ){
                        #create report
                        input2 <- isolate(reactiveValuesToList(input))
                        input2$report <- simul_names[simul_names$label == input2$report, "names"]
                        td = update_report(dir=remote_dirs[remote_dirs$name == simul_names[simul_names$label == input$report, "remote"], "dir"],
                                           input=input2,
                                           ssh=remote_dirs[remote_dirs$name == simul_names[simul_names$label == input$report, "remote"], "address"],
                                           key=remote_dirs[remote_dirs$name == simul_names[simul_names$label == input$report, "remote"], "key"],
                                           outdir= simul_names[simul_names$label == input$report, "remote"]
                        )
                    } else { #it is local
                        td = update_report(rootdir, input) # update report
                    }

                    # update upd
                    upd[ upd$id == input$report, "output_hash"] = as.character( get_hash( path=paste0(rootdir, input$report, "/"),
                                                                                          file="output.csv",
                                                                                          ssh=remote_dirs[remote_dirs$name == simul_names[simul_names$label == input$report, "remote"], "address"] ,
                                                                                          key=remote_dirs[remote_dirs$name == simul_names[simul_names$label == input$report, "remote"], "key"]))
                    upd[ upd$id == input$report, "abrmax"] = input$limits[2]
                    upd[ upd$id == input$report, "abrmin"] = input$limits[1]
                    upd[ upd$id == input$report, "kompl"] = input$kompl

                    saveRDS(upd, "last_updated.rds") # save upd
            } # if it has been changed
            else { #it is the old version
              td = paste0(input$report, "/index.html")
            }
        }
        
        # serve it
        #cat( paste(getwd(), "updated\n") )
        
        if(is.null(online)){
            #message(paste("Opening", paste(getwd(), "outputs", td, sep="/")))
            browseURL(paste(getwd(), "outputs", td, sep="/"))
        } else {
        
            #addResourcePath("tmpuser", getwd())
            output$rep <- renderUI({
                "starting"
                tags$iframe(#seamless="seamless", 
                            #src= paste("tmpuser/outputs", input$report, "index.html", sep="/"),
                            src= paste(online, td, sep="/"),
                            #width="100%",
                            #, height="100hv"
                            margin= 0, 
                            paddin= 0,
                            width= "100%",
                            height= "100%",
                            border= 0,
                            #background-color="#EBEBEB"
                            #, overflow-y= "scroll"
                            )
            })
        }
        
    }) #observer

    
})
