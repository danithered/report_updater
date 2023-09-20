noComposites <- function(noEA){
  # return( 2^(noEA*2) )
  return( sum(1:2^noEA) )
}

composit_type <- function(x, y, noEA=7){
  ifelse(x>y, x+bitwShiftL(y, noEA), y+bitwShiftL(x, noEA))
}

burstComposit <- function(x, noEA=7){
  return(c(
    bitwShiftR(x, noEA),
    bitwAnd(x, 2^noEA-1)
  ))
}

compositN <- Vectorize(function(x, noEA=7, as.text=F){
  if(x==0) return("parazite")
  if(x==-1) return("empty")
  
  return(paste(enzN(bitwShiftR(x, noEA), as.text = as.text), "/", enzN(bitwAnd(x, 2^noEA-1), as.text = as.text)))
})

#composit_type(c(0, 5), c(3, 4))
#enzN(c(0, 3), as.text = T)
#enzN(c(5, 4), as.text = T)
#compositN(c(3, 517), as.text = T)

#noComposites(7)

# app specific functions

get_my_data <- function(file, path="", ssh=NA, ssh_key= "~/.ssh/id_rsa"){
  out <- list()
  
  try({
    f <- get_file(file, path, ssh=ssh, ssh_key=ssh_key, fast=T, to=NA)
    if(is.na(f)) {
      warning("File (", file, ") not found", ifelse(is.na(ssh), "", "via SSH connection"), "!")
    }
    
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
      
      out <- as.data.frame(list( alive = get_child(cell, c("cell.alive", "is_alive"), "logical"),
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

getInputCompositions <- function(files, noEA=7, exclude=T, onlyalive=F){
  
  # colname <- c("time","replicators","no_par","mean_R_par","mean_length_par","mean_mfe_par",
  #              paste0(c("no_enz", "mean_R_enz", "mean_length_enz", "mean_mfe_enz","mean_a_enz" ), rep(1:noEA-1, each=5) ),
  #              paste0("no_A", 0:noEA)  
  # )
  
  colname <- c("seq", "str", "mfe", "Pfold", "Pdeg", "no_sites", "R", "M", "type", paste0("a", 1:noEA-1), "prev_type")
  
  out<-list()
  
  for(file in files){
    try({
      data <- read.table(file, sep="\t")
      colnames(data) <- colname
      if(onlyalive){
        if(!all(apply(
          do.call(rbind, lapply(data$type, function(x, noEA) type2vec(x, no=noEA), noEA=noEA)),
          2,
          any
        ))) next
      }
      if(exclude){
        out[[length(out)+1]] <- composit_type(data$type, data$prev_type) |> table()
      } else {
        out[[length(out)+1]] <- factor(composit_type(data$type, data$prev_type), levels=0:( 2^(noEA*2) )) |> table()
      }
    }, silent=T)
  }
  
  return(out)
}

getLastComposition <- function(file, noEA=7, exclude=T, onlyalive=F, ...){
  scm = get_my_data(file, ...)
  out<-list()
  
  for(row in 1:nrow(scm$table) ){
    reps = scm$table$reps[[row]]
    
    if(onlyalive){
      tt = lapply(which(children(reps) == "item"), function(rnum, noEA){
        rep <- xml_child(reps, rnum)
        out <- type2vec(get_child(rep, "repl.type", "double"), no=noEA)
        
        return(out)
      }, noEA)  
      if(!all(apply(
        do.call(rbind, tt),
        2,
        any
      ))) next
    }
    
    comps = sapply(which(children(reps) == "item"), function(rnum, noEA){
      rep <- xml_child(reps, rnum)
      if(exclude){
        out <- composit_type(get_child(rep, "repl.type", "double"), get_child(rep, "repl.prev_type", "double"), noEA=noEA  )
      } else {
        out <- factor(
          composit_type(get_child(rep, "repl.type", "double"), get_child(rep, "repl.prev_type", "double"), noEA=noEA)
          , levels=0:( 2^(noEA*2))  )
      }
      
      return(out)
    }, noEA)  
    
    try(out[[length(out)+1]] <- table(comps))
  }
  
  return(out)
}

symdiff <- function(x, y) { 
  setdiff( union(x, y), intersect(x, y))
}

compdiff <- function(x, y) { 
  a = (names(x))
  b = (names(y))
  1- length(intersect(a, b))/length(union(a, b))
}

simpson <- function(a){
  sum((a/sum(a))^2)
}

shannon <- function(a){
  a = a/sum(a)
  -sum(a * log(a))
}

evenness <- function(a, noEA=7){
  shannon(a)/log( noComposites(noEA) )
}

getUniques <- function(input){
  uniques<-list(input[[1]] != 0)
  for(v in 2:length(input)){
    temp <- input[[v]] != 0
    isnew = T
    for(comp in uniques){ # compare with previous types
      if(all(temp==comp)){ # there is a match!
        isnew=F
        break
      }
    }
    if(isnew){
      uniques[[length(uniques)+1]] <- temp
    } 
  }
  return(uniques)
}


isAlive <- function(pool, noEA=7){
  all(apply(
    sapply(as.numeric(names(pool[[1]][pool[[1]]>0])), function(x, noEA) 
      sapply(burstComposit(x), function(x, noEA) type2vec(x, no=noEA), noEA=noEA ) 
      , noEA=noEA)
    , 2, any))
}

type2sA <- Vectorize(function(x){
  if(x==0) return("PARA")
  if(x==-1) return("empty")
  
  m <- ceiling(log(x,2))
  acts <- c()
  
  for(i in m:0){
    if(x %/% 2^i != 0){
      acts <- c(acts, i)
      x <- x-2^i
      if(x == 0) break
    }
  }
  if(length(acts) > 1) return( "POLI" )
  else return( "MONO" )
  
})

comp2sA <- Vectorize(function(x, noEA=7){
  if(x==0) return("parazite")
  if(x<0) return("empty")
  
  types <- sort(type2A(burstComposit(x, noEA = noEA)))
  
  return(paste(ifelse(types==0, "PARA", ifelse(types==1, "MONO", "POLI")), collapse = "/"))
  
})
