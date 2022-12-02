library(ssh)

#### functions ####
get_mtime <- (function(target=NULL, path="~/", ssh=NA, ssh_key="~/.ssh/id_rsa"){
  file = paste(path, 
               target, 
               sep=ifelse(nchar(path) > 0 & substr(path, nchar(path), nchar(path)) != "/", "/", ""))
  
  if(is.na(ssh)){ #local file
    if( all(file.exists( file )) ){
      if(is.null(target)){
        return( system(paste( "find", file, "-type f -exec stat \\{} --printf=\"%y\n\" \\; | sort -n -r | head -n 1" ), intern=T) )
      } else {
        return( system(paste("stat -c %y", paste(file, collapse = " ") ), intern=T) )
      }
      return( system(paste("stat -c %y", paste(file, collapse = " ") ), intern=T) )
    } else {
      return(NA)
    }
  } else try({ 
    #it is in shh
    #connecting to ssh
    ssh_con <- ssh_connect(ssh, keyfile = ssh_key)
    if(!ssh_session_info(ssh_con)$connected){
      warning("Could not establish shh connection")
      return(NA)
    }
    
    #download
    type <- capture.output(ssh_exec_wait(ssh_con, paste0("tf=", 
                                                         file, 
                                                         ";if [ -f $tf ]; then echo file; else if [ -d $tf ]; then echo dir; else echo FALSE; fi; fi") 
    ))[1]
    if(!type %in% c("file", "dir")) return(NA)
    
    if(is.null(target)){
      out <- capture.output(ssh_exec_wait(ssh_con, paste( "find", file, "-type f -exec stat \\{} --printf=\"%y\n\" \\; | sort -n -r | head -n 1" ) ))[1]
    } else {
      out <- capture.output(ssh_exec_wait(ssh_con, paste("stat -c %y", paste(file, collapse = " "))))[1]
    }
    #disconnect
    ssh_disconnect(ssh_con)
    return(out)
    
  })
})

sshready_adr <- function(orig){
  unlist(lapply(strsplit(orig, ":"), function(x) paste(x, collapse=" -p ")))
}

get_file <- function(target, path="~/", ssh=NA, ssh_key="~/.ssh/id_rsa", fast=T, to=NA){
  if(is.na(ssh)){
    file = paste(path, 
                 target, 
                 sep=ifelse(nchar(path) > 0 & substr(path, nchar(path), nchar(path)) != "/", "/", ""))
    if( all(file.exists(file)) ) return(file)
    else return(NA)
  } else try({ 
    #it is in shh
    
    #check output dir
    if(is.na(to)) {
      to = tempdir()
    } else {
      if(!dir.exists(to)){
        if(!dir.create(to)) {
          warning(paste("Cannot create dir", to))
          return(NA)
        }
      }
    }
    
    #download
    tofile = paste(to, target, sep=ifelse(nchar(to) > 0 & substr(to, nchar(to), nchar(to)) != "/", "/", ""))
    if(fast){
      system(paste("ssh", sshready_adr(ssh), '"tar -C', path, '-zc', target, '" | tar zx -C', to))
      if(all(file.exists( tofile ))){
        return( tofile )
      } else {
        warning(paste("Could not download file tru shh", target))
        return(NA)
      }
    } else {
      #connecting to ssh
      ssh_con <- ssh_connect(ssh, keyfile = ssh_key)
      if(!ssh_session_info(ssh_con)$connected){
        warning("Could not establish shh connection")
        return(NA)
      }
      #download
      scp_download(ssh_con, 
                   files= paste(path, target, sep=ifelse(nchar(path) > 0 & substr(path, nchar(path), nchar(path)) != "/", "/", "")),
                   to=to
      )
      #disconnect
      ssh_disconnect(ssh_con)
      return( tofile )
    }
  })
}

get_subdirs <- Vectorize(function(path="~/", ssh=NA, ssh_key="~/.ssh/id_rsa"){
  if(nchar(path) > 0 & substr(path, nchar(path), nchar(path)) != "/" ) path=paste0(path, "/")
  
  if(is.na(ssh)){ #local dir
    if( dir.exists( path ) ){
      out <- system(paste0("ls -d ", path, "*/"), intern=T)
      if( length(out) == 0 ) return( NA) 
      else return( out )
    } else {
      return(NA)
    }
  } else try({ 
    #it is in shh
    #connecting to ssh
    ssh_con <- ssh_connect(ssh, keyfile = ssh_key)
    if(!ssh_session_info(ssh_con)$connected){
      warning("Could not establish shh connection")
      return(NA)
    }
    
    #check if dir exists
    if(!as.logical(capture.output(ssh_exec_wait(ssh_con, paste0("if [ -d ", path, " ]; then echo TRUE; else echo FALSE; fi")  ))[1])){
      return(NA)
    }
    
    #get subdirs
    out <- capture.output(ssh_exec_wait(ssh_con, paste0("ls -d ", path, "*/") ))
    if(length(out) > 1) out <- out[1:(length(out)-1)]
    else out <- NA
    
    #disconnect
    ssh_disconnect(ssh_con)
    return(out)
    
  })
})

get_filelist <- Vectorize(function(path="~/", ssh=NA, ssh_key="~/.ssh/id_rsa"){
  if(nchar(path) > 0 & substr(path, nchar(path), nchar(path)) != "/" ) path=paste0(path, "/")
  
  if(is.na(ssh)){ #local dir
    if( dir.exists( path ) ){
      out <- system(paste("find", path, "-maxdepth 1 -type f"), intern=T)
      if( length(out) == 0 ) return( NA) 
      else return( out )
    } else {
      return(NA)
    }
  } else try({ 
    #it is in shh
    #connecting to ssh
    ssh_con <- ssh_connect(ssh, keyfile = ssh_key)
    if(!ssh_session_info(ssh_con)$connected){
      warning("Could not establish shh connection")
      return(NA)
    }
    
    #check if dir exists
    if(!as.logical(capture.output(ssh_exec_wait(ssh_con, paste0("if [ -d ", path, " ]; then echo TRUE; else echo FALSE; fi")  ))[1])){
      return(NA)
    }
    
    #get subdirs
    out <- capture.output(ssh_exec_wait(ssh_con, paste("find", path, "-maxdepth 1 -type f") ))
    if(length(out) > 1) out <- out[1:(length(out)-1)]
    else out <- NA
    
    #disconnect
    ssh_disconnect(ssh_con)
    return(out)
    
  })
})


getjobs <- function(dirs){
  needed_entries = c("name", "description", "report", "target")
  
  lapply(1:nrow(dirs), function(r) {
    dir = dirs[r,]
    if( is.na(dir$ssh_key) ) dir$ssh_key = "~/.ssh/id_rsa"
    # check and get file
    #browser()
    f <- get_file("updset.R", path=dir$dir, ssh=dir$ssh, ssh_key = dir$ssh_key, fast=F)
    if(is.na(f)) return(NA)
    
    # read file
    fl <- readLines(f)
    if(!is.na(dir$ssh)) file.remove(f)
    
    # process file
    readpoints <- c(grep( c("\\[dir\\]|\\[subdir\\]"), fl), length(fl)+1)
    readpoints <- readpoints[!duplicated(readpoints)]
    
    out <- data.frame()
    for (i in 1:(length(readpoints)-1) ) try({
      #create list
      text = fl[(readpoints[i]+1):(readpoints[i+1]-1)]
      text = text[gsub("[[:space:]]", "", text) != ""] # remove lines containing only whitespaces
      lista = eval(parse(text= 
                           paste("list(", paste(text, collapse=","), ")") 
      ))
      
      if(length(lista) == 0) next
      
      # absent needed entries as NA
      missing_entries <- needed_entries[!needed_entries %in% names(lista)]
      for( miss in missing_entries) lista[[ miss ]] <- NA
      
      #create
      if( length(grep("subdir", fl[readpoints[i]])) == 0 ){ # dir
        out <- rbind(out, data.frame(path = dir$dir, 
                                     ssh= dir$ssh, 
                                     ssh_key=dir$ssh_key, 
                                     name=lista$name, 
                                     description=lista$name, 
                                     report=lista$report,
                                     targetdir=lista$target) )
        
      } else { # subdir
        #get subdirs
        subd <- suppressWarnings(get_subdirs(path=dir$dir, ssh=dir$ssh, ssh_key=dir$ssh_key))
        # remove ones containing .ignore file
        sudb <- subd[sapply(subd, function(sd) {
          length(grep(".ignore", get_filelist(path=sd, ssh=dir$ssh, ssh_key=dir$ssh_key))) != 0
        })]
        # add to out
        if( length(subd) > 0 ) {
          enddirs <- sapply(strsplit(subd, "/"), function(x) x[length(x)])
          out <- rbind(out, data.frame(path = unname(subd), 
                                       ssh= dir$ssh, 
                                       ssh_key=dir$ssh_key, 
                                       name=lista$name, 
                                       description=lista$description, 
                                       report=lista$report,
                                       targetdir=paste(lista$target, 
                                                       enddirs, 
                                                       sep=ifelse(nchar(lista$target) > 0 & 
                                                                    substr(lista$target, 
                                                                           nchar(lista$target), 
                                                                           nchar(lista$target)) != "/",
                                                                  "/",
                                                                  ""))
                                       ) 
                       )
          
        }
      }
    }) #reading in data
    
    
    return(out)
  } ) # apply
}

update_report <- function(dir, input, ssh=NA, key=NA, outdir){
  targetdir <- character()
  if(is.na(ssh) ){
    targetdir <- paste("outputs", input$report, sep="/")
    try(rmarkdown::render("/home/danielred/data/programs/mcrs_chromosome/src/output_graph.Rmd", 
                          params = list(
                            report=input$report,
                            abrmax=input$limits[2],
                            abrmin=input$limits[1],
                            kompl=input$kompl,
                            cache.path=paste0(getwd(), 
                                              #"/MCRS_max_reports/", 
                                              "/",
                                              targetdir, 
                                              "/cache/")
                          ),
                          output_dir = targetdir,
                          #knit_root_dir = paste("outputs", input$report, sep="/"),
                          intermediates_dir = targetdir,
                          output_file = "index.html"
    ))
    #cat(paste0(getwd(), "/outputs/", input$report, "/cache/\n"))
  } else { #on ssh
    #rd <- remote_dirs[remote_dirs$name == simul_names[simul_names$label == input$report, "remote"]]
    targetdir <- paste("outputs", outdir, input$report, sep="/")
    parameters <- list(
      report=input$report,
      abrmax=input$limits[2],
      abrmin=input$limits[1],
      kompl=input$kompl,
      cache.path=paste0(getwd(), 
                        #"/MCRS_max_reports/",
                        "/",
                        targetdir,
                        "/cache/"),
      ssh=TRUE,
      ssh_address=ssh,
      ssh_dir=dir,
      ssh_key=key
    )
    
    message(paste0("Rendering outputn targetdir ", targetdir, " with parameters:\n\t", paste(names(parameters), parameters, sep="=", collapse="\n\t")))
    
    try(rmarkdown::render("/home/danielred/data/programs/mcrs_chromosome/src/output_graph.Rmd", 
                          params = parameters,
                          output_dir = targetdir,
                          #knit_root_dir = paste("outputs", input$report, sep="/"),
                          intermediates_dir = targetdir,
                          output_file = "index.html"
    )) #render
  }#on ssh
  
  return( paste0(substr(targetdir,9,nchar(targetdir)), "/index.html") )
}


get_hash <- function(file, path="", ssh=NA, key){
  #browser()
  if(is.na(ssh[1])){
    return(tools::md5sum( paste0(path, file) ))
  }
  # it is online
  try({
    #connect to remote host
    if(class(address) == "ssh_session"){
      con = address
      dest = F
    } else {
      con <- ssh_connect(address, keyfile = key)
      dest = T
    }
    
    #get file
    tdir = tempdir()
    scp_download(con, paste0(path, file) , to = tdir)
    
    #get hash
    hash = tools::md5sum( paste0(tdir, file) )
    
    #close con
    if(dest) ssh_disconnect(con)
    
    return(hash)
  })
  
  return(NA)
}

get_remote <- function(address, 
                       path="data/programs/mcrs_chromosome/OUT", 
                       path2="", 
                       key= "~/.ssh/id_rsa",
                       what="all",
                       fullpath=F    
){
  if(!what %in% c("all", "dirs", "files")){
    return(-2)
  }
  
  try({
    #connect to remote host
    if(class(address) == "ssh_session"){
      con = address
      dest = F
    } else {
      con <- ssh_connect(address, keyfile = key)
      dest = T
    }
    
    
    #get data
    if(!ssh_session_info(con)$connected){
      return(-1)
    }
    if(what == "all") {
      out <- ssh_exec_internal(con, command=paste("cd", paste0(path, path2), "\nls -p"))
    } else if(what=="dirs"){
      out <- ssh_exec_internal(con, command=paste("cd", paste0(path, path2), "\nls -d */"))
    } else {
      out <- ssh_exec_internal(con, command=paste("cd", paste0(path, path2), "\nls -p | grep -v /"))
    }
    
    #disconnect
    if(dest) ssh_disconnect(con)
    
    #return data
    if(fullpath){
      return( paste( 
        paste0(path, path2), 
        ( rawToChar(out$stdout) |> strsplit("\\n") )[[1]], 
        sep="/")
      )
    } else {
      return(( rawToChar(out$stdout) |> strsplit("\\n") )[[1]])
    }
  })
  
  return(-3)
}

get_remote_dirs <- function(address, slash=T, ...) {
  out <- get_remote(address=address, what = "dirs", ...)
  if(slash) return(out)
  return( substr(out,1,nchar(out)-1) )
}

get_remote_files <- function(address, pattern=NA, ...) {
  out <- get_remote(address=address, what = "files", ...)
  if(is.na(pattern)) {
    return(out)
  }
  return( grep(pattern, out, value = T) )
} 

get_cached_files <- function(needed_files, path="./", ssh=NA, ssh_key="~/.ssh/id_rsa"){
  lapply(needed_files, function(file) {
    hash <- get_mtime(needed_files, path=params$dir, ssh=ssh, ssh_key=params$ssh_key)
    if(is.na(hash)) return(NA)
    refreshed = F
    table <- xfun::cache_rds({
      f <- get_file(needed_files, path=params$dir, ssh=ssh, ssh_key = params$ssh_key)
      refreshed <- ifelse(is.na(f), NA, T)
      message(paste("Reading file", f))
      read.table(f, sep=";", header=T)
    }, hash = list(hash))
    return(refreshed)
  })
}



####################### from mcrsc_average #####################
# get_remote <- function(address, 
#                        path="data/programs/mcrs_chromosome/OUT", 
#                        path2="", 
#                        key= "~/.ssh/id_rsa",
#                        what="all",
#                        fullpath=F    
# ){
#   if(!what %in% c("all", "dirs", "files")){
#     return(-2)
#   }
#   
#   try({
#     #connect to remote host
#     if(class(address) == "ssh_session"){
#       con = address
#       dest = F
#     } else {
#       con <- ssh_connect(address, keyfile = key)
#       dest = T
#     }
#     
#     
#     #get data
#     if(!ssh_session_info(con)$connected){
#       return(-1)
#     }
#     if(what == "all") {
#       out <- ssh_exec_internal(con, command=paste("cd", paste0(path, path2), "\nls -p"))
#     } else if(what=="dirs"){
#       out <- ssh_exec_internal(con, command=paste("cd", paste0(path, path2), "\nls -d */"))
#     } else {
#       out <- ssh_exec_internal(con, command=paste("cd", paste0(path, path2), "\nls -p | grep -v /"))
#     }
#     
#     #disconnect
#     if(dest) ssh_disconnect(con)
#     
#     #return data
#     if(fullpath){
#       return( paste( 
#         paste0(path, path2), 
#         ( rawToChar(out$stdout) |> strsplit("\\n") )[[1]], 
#         sep="/")
#       )
#     } else {
#       return(( rawToChar(out$stdout) |> strsplit("\\n") )[[1]])
#     }
#   })
#   
#   return(-3)
# }
# 
# get_remote_dirs <- function(address, ...) get_remote(address=address, what = "dirs", ...)
# 
# get_remote_files <- function(address, pattern=NA, ...) {
#   out <- get_remote(address=address, what = "files", ...)
#   if(is.na(pattern)) {
#     return(out)
#   }
#   return( grep(pattern, out, value = T) )
# }
# 
# sshready_adr <- function(orig){
#   unlist(lapply(strsplit(orig, ":"), function(x) paste(x, collapse=" -p ")))
# }
# 
# get_file <- function(target, path="~/", ssh=NA, ssh_key="~/.ssh/id_rsa", fast=T, to=NA){
#   if(is.na(ssh)){
#     file = paste(path, 
#                  target, 
#                  sep=ifelse(nchar(path) > 0 & substr(path, nchar(path), nchar(path)) != "/", "/", ""))
#     if( all(file.exists(file)) ) return(file)
#     else return(NA)
#   } else try({ 
#     #it is in shh
#     
#     #check output dir
#     if(is.na(to)) {
#       to = tempdir()
#     } else {
#       if(!dir.exists(to)){
#         if(!dir.create(to)) {
#           warning(paste("Cannot create dir", to))
#           return(NA)
#         }
#       }
#     }
#     
#     #download
#     tofile = paste(to, target, sep=ifelse(nchar(to) > 0 & substr(to, nchar(to), nchar(to)) != "/", "/", ""))
#     if(fast){
#       system(paste("ssh", sshready_adr(ssh), '"tar -C', path, '-zc', target, '" | tar zx -C', to))
#       if(all(file.exists( tofile ))){
#         return( tofile )
#       } else {
#         warning(paste("Could not download file tru shh", target))
#         return(NA)
#       }
#     } else {
#       #connecting to ssh
#       ssh_con <- ssh_connect(ssh, keyfile = ssh_key)
#       if(!ssh_session_info(ssh_con)$connected){
#         warning("Could not establish shh connection")
#         return(NA)
#       }
#       #download
#       scp_download(ssh_con, 
#                    files= paste(path, target, sep=ifelse(nchar(path) > 0 & substr(path, nchar(path), nchar(path)) != "/", "/", "")),
#                    to=to
#       )
#       #disconnect
#       ssh_disconnect(ssh_con)
#       return( tofile )
#     }
#   })
# }
# 
# get_mtime <- Vectorize(function(target, path="~/", ssh=NA, ssh_key="~/.ssh/id_rsa"){
#   file = paste(path, 
#                target, 
#                sep=ifelse(nchar(path) > 0 & substr(path, nchar(path), nchar(path)) != "/", "/", ""))
#   
#   if(is.na(ssh)){ #local file
#     if( all(file.exists( file )) ){
#       return( system(paste("stat -c %y", file), intern=T) )
#     } else {
#       return(NA)
#     }
#   } else try({ 
#     #it is in shh
#     #connecting to ssh
#     ssh_con <- ssh_connect(ssh, keyfile = ssh_key)
#     if(!ssh_session_info(ssh_con)$connected){
#       warning("Could not establish shh connection")
#       return(NA)
#     }
#     
#     #download
#     type <- capture.output(ssh_exec_wait(ssh_con, paste0("tf=", 
#                                                          file, 
#                                                          ";if [ -f $tf ]; then echo file; else if [ -d $tf ]; then echo dir; else echo FALSE; fi; fi") 
#     ))[1]
#     if(!type %in% c("file", "dir")) return(NA)
#     
#     out <- capture.output(ssh_exec_wait(ssh_con, paste("stat -c %y", file)))[1]
#     #disconnect
#     ssh_disconnect(ssh_con)
#     return(out)
#     
#   })
# })
# 
# get_cached_files <- function(needed_files, path="./", ssh=NA, ssh_key="~/.ssh/id_rsa"){
#   lapply(needed_files, function(file) {
#     hash <- get_mtime(needed_files, path=params$dir, ssh=ssh, ssh_key=params$ssh_key)
#     if(is.na(hash)) return(NA)
#     refreshed = F
#     table <- xfun::cache_rds({
#       f <- get_file(needed_files, path=params$dir, ssh=ssh, ssh_key = params$ssh_key)
#       refreshed <- ifelse(is.na(f), NA, T)
#       message(paste("Reading file", f))
#       read.table(f, sep=";", header=T)
#     }, hash = list(hash))
#     return(refreshed)
#   })
# }
