if(!require(RRNA)){install.packages("RRNA");library(RRNA)}
if(!require(plotrix)){install.packages("plotrix");library(plotrix)}

dirto <- function(to, from=c(0,0)){
  
  return(atan2(to[2] - from[2], to[1] - from[1]) / pi)
}
rad2deg <- function(rad) {rad * 180 / pi}
pirad2deg <- function(rad) {rad * 180}
deg2rad <- function(deg) {deg * pi / 180}
deg2pirad <- function(deg) {deg / 180}
addToPlot <- function(x=0, y=0, coords, 
                      xspan=1, rot = NA, gap=0.05, 
                      main_con=NA, side_con=NA, 
                      add_letter=F, cex_letter=par("cex"), col_letter="black", col=NULL,
                      quiet=F, add=T,
                      # for plotting
                      asp=1, 
                      xlim=NULL, ylim=NULL, 
                      xpd=NA, 
                      main=NULL, sub=NULL, xlab=NULL, ylab=NULL,
                      ...){
  #cat("rot is ",rot, "\n")
  try({
  #rotate
  if(!is.na(rot)){
    midx <- mean(coords$x)
    midy <- mean(coords$y)
    dist <- sqrt(coords$x^2 + coords$y^2)
    angle <- atan2(coords$y, coords$x) + rot
    coords$y <- midy + sin(angle) * dist
    coords$x <- midx + cos(angle) * dist
    
  }
  
  #align
  coords$x <- (coords$x - min(coords$x))
  coords$y <- (coords$y - min(coords$y))
  
  #rescale
  scaling=1
  if(!is.na(xspan)){
    #compute scaling factor
    scaling = ifelse(max(coords$x) < max(coords$y),
                     xspan/(max(coords$y) - min(coords$y)),
                     xspan/(max(coords$x) - min(coords$x))
    )
    
    coords$x <- coords$x * scaling
    coords$y <- coords$y * scaling
  }
  
  #realign
  coords$x <- x + coords$x
  coords$y <- y + coords$y
  
  
  
  if(!quiet){
    ## make it a frame if needed
    if(!add){
      makeFrame(coords=coords, x=x, y=y, 
                asp=asp, 
                xlim=xlim, ylim=ylim, 
                xpd=xpd, 
                main=main, sub=sub, xlab=xlab, ylab=ylab)
    }
    
    ## set bg
    orig_bg = par("bg")
    par(bg="transparent")
  } 
  
  #connector lines
  ## MAIN_CON
  if(!is.list(main_con)) if(!is.na(main_con)){
    if(is.logical(main_con)){
      if(main_con) {
        main_con <- list(lwd=1, lty=1, col="black")
      }
    }
  }
  
  if(!quiet & is.list(main_con)) with(main_con, {
    #points(coords$x, coords$y, type="c", col=col, lwd=lwd, lty=lty)
    segments(coords[1:(nrow(coords)-1),"x"], 
             coords[1:(nrow(coords)-1),"y"], 
             coords[2:nrow(coords),"x"], 
             coords[2:nrow(coords),"y"], 
             col=col, lwd=lwd, lty=lty)
  })
  
  ##SIDE_CON
  if(!is.list(side_con)) if(!is.na(side_con)){
    if(is.logical(side_con)) if(side_con) side_con <- list(lwd=1, lty=1, col="black")
  }  
  
  if(is.list(side_con)) with(side_con, {
    segs <- data.frame()
    
    for(base in 1:nrow(coords)){
      if(coords[base, "bound"] > 0 & base < coords[base, "bound"]){
        segs <- rbind(segs, data.frame(
          x0=coords[base, "x"], 
          y0=coords[base, "y"],
          x1=coords[coords[base, "bound"], "x"],
          y1=coords[coords[base, "bound"], "y"]  ))
      }
    }
    if(!quiet) segments(segs$x0, segs$y0, segs$x1, segs$y1, lwd=lwd, col=col, lty=lty)
    
  })
  
  
  #points(coords$x, coords$y)
  # symbols(coords$x, coords$y, 
  #         circles=rep(scaling* (1/2-gap), nrow(coords)), 
  #         inches=F, add=T, ...)
  if(is.null(col)){
    cols <- rep("transparent", nrow(coords))
  } else {
    cols <- col
  }
  if(!quiet) for(ci in 1:nrow(coords) ) draw.circle(coords$x[ci], coords$y[ci],
                                         radius = scaling* (1/2-gap), nrow(coords),
                                         col=cols[ci],
                                         ...)
  #add letter
  lett <- coords[add_letter, ]
  if(!quiet & nrow(lett) > 0) {
    text(lett$x, lett$y, labels=lett$seq, cex=cex_letter, col=col_letter)
  }
  })
  
  if(!quiet) par(bg=orig_bg)
  
  return(list(xlim=range(coords$x), ylim=range(coords$y), cex=cex_letter))
}

# deprecated
plot_RNA <- function(coords, x=0, y=0, xspan=1, asp=1, xlim=NULL, ylim=NULL, xpd=NA, main=NULL, sub=NULL, xlab=NULL, ylab=NULL, quiet=F, ...){
  if(!quiet){
    plot.new()
    plot.window(asp=asp, 
                xlim=c(ifelse(is.null(xlim), 0+x, xlim[1]), ifelse(is.null(xlim), 1+x, xlim[2])), 
                ylim=c(ifelse(is.null(ylim), 0+y, ylim[1]), ifelse(is.null(ylim), 1+y, ylim[2])), 
                xpd=xpd)
    title(main=main, sub=sub, ylab=ylab, xlab=xlab)
  }
  addToPlot(x,y,coords, xspan=xspan, quiet=quiet, ...)
}

makeFrame <- function(coords, x=0, y=0, asp=1, xlim=NULL, ylim=NULL, xpd=NA, main=NULL, sub=NULL, xlab=NULL, ylab=NULL){
    plot.new()
    plot.window(asp=asp, 
                xlim=c(ifelse(is.null(xlim), min(coords$x), xlim[1]), 
                       ifelse(is.null(xlim), max(coords$x), xlim[2])), 
                ylim=c(ifelse(is.null(ylim), min(coords$y), ylim[1]), 
                       ifelse(is.null(ylim), max(coords$y), ylim[2])), 
                xpd=xpd)
    title(main=main, sub=sub, ylab=ylab, xlab=xlab)
}

plot_acts <- function(a1, a2, col="grey"){
  length =max(length(a1), length(a2)) 
  if(length(a1) < length) a1 <- c(unlist(a1), rep(0, length-length(a1)))
  if(length(a2) < length) a2 <- c(unlist(a2), rep(0, length-length(a2)))
  plot.new()
  plot.window(asp=1, xlim=c(0,length), ylim=c(0,1), xpd=NA)
  rect(0:(length-1), 0, 1:length,1, 
       col = ifelse( rep(is.logical(a2), length), ifelse(a2, col, "white"), ifelse(a2 == 0, "white", alpha(col, a1/max(a1)))))
  rect(0:(length-1), 1.5, 1:length,2.5, 
       col = ifelse( rep(is.logical(a1), length), ifelse(a1, col, "white"), ifelse(a1 == 0, "white", alpha(col, a1/max(a1)))))
  if(!is.logical(a1)) text(1:length-0.5, 2, labels = round(a1, 2) )
  if(!is.logical(a2)) text(1:length-0.5, 0.5, labels = round(a2, 2) )
  text(mean(c(0, length) ), 2.75, "Current strand")
  text(mean(c(0, length) ), 1.25, "Complementary strand")
}

find_activity <- function(repl, coords, rules){
  startofPattern <- list()
  for(rule in rules){
    where <- gregexpr(rule$str, repl$str, fixed=T)
    if(where[[1]][1] > -1){ #it has this activity
      for(w in where[[1]]){ #check every possible location
        #check subrules
        found = c()
        for(sr in rule$subrules){
          at = w + as.numeric(sr$locus)-1
          if(substr(repl$seq, at, at) == sr$base) { 
            found = c(found, at)
          }
        } # going tru subrules
        if(length(found)>0){ #everything is ok
          startofPattern[[length(startofPattern)+1]] <- list(activity = rule$activity, 
                                                             start=as.numeric(w), 
                                                             length= nchar(rule$str), 
                                                             matches=found
          )
        }
      } # checking possible locations
    }
  }
  if(length(startofPattern) == 0) return(NA)
  return(startofPattern)
}
basecol <- function(str, n, col){
  sapply(n, function(n=n, str, col){
    if(length(col) == 1){
      return(col)
    }
    if(length(col) == 2){
      base <- substr(str, n, n)
      if(base == "."){
        return(col[1])
      }
      if(base %in% c("(", ")")){
        return(col[2])
      }
    }
    return(NA)
  }, str=str, col=col)
}
masking <- function(str, n, col){
  mask <- rep(NA, nchar(str))
  mask[n] <- basecol(str, n, col)
  return(mask)
}
mask_overlap <- function(mask1, mask2, over="orange"){
  vals2 = !is.na(mask2)
  overlap = !is.na(mask1) & vals2
  mask1[vals2] <- mask2[vals2]
  if(!is.na(over)) mask1[overlap] <- over
  return(mask1)
}

make.colormask <- function(str, patterns=list(), col=NA, col.pattern=list(), col.base=c()){
  #browser()
  length = nchar(str)
  basemask = basecol(str, 1:length, col)
  if(!is.na(patterns)[1]) for(pattern in patterns){
    if(length(col.pattern) < pattern$activity) mask <- rep(NA, length)
    else mask <- masking(str, seq(pattern$start, length.out=pattern$length), col.pattern[[pattern$activity]])
    if(length(col.base) >= pattern$activity) mask[pattern$matches] <- col.base[pattern$activity]
    basemask <- mask_overlap(basemask, mask, over=NA)
  }
  return(basemask)
}


readRules <- function(dir){
  setwd(dir)
  
  rules <- list()
  ids = dir(dir)[!is.na(as.numeric(( dir(dir))))]
  ids = ids[order(as.numeric(ids))]
  
  for(id in ids){
    rule <- readLines(mergepath(dir, id, "str.txt"))
    subs <- grep(".", rule, fixed=T)
    
    for(sub in 1:length(subs) ){
      subr <- list(str = rule[subs[sub]], subrules = list(), activity = as.numeric(id))
      start = (subs[sub]+1)
      end = ifelse(sub<length(subs), subs[sub+1]-1, length(rule))
      for(b in start:end ) {
        sp <- strsplit(rule[[b]], " ")[[1]]
        subr$subrules[[length(subr$subrules)+1]] <- list(locus = as.numeric(sp[1]), base = sp[2])
      }
      rules[[length(rules)+1]] <- subr
    }
  }
  
  return(rules)
}

quick_plot_RNA <- function(seq, str, rules, A, pcols=c("red", "coral"), actcols = brewer.pal(A, "Set1"), ...){
  if(is.character(rules)) rules = readRules(rules)
  
  col.pattern = list()
  for(col in actcols) {
    col.pattern[[length(col.pattern)+1]] <- pcols
  }
  
  coords= ct2coord( makeCt( str, seq) )
  startofPatterns <- find_activity(list(seq=seq,str=str), coords, rules)
  
  colormask <- make.colormask(str, 
                              patterns =startofPatterns, 
                              col=NA, 
                              col.pattern = col.pattern, 
                              col.base = actcols
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
           side_con = list(lwd=0.5, col="purple", lty=2),
           ...
  )
}

quick_RNA <- function(x, y, seq, str, rules, A, 
                          actcols, 
                          pcols=c("red", "coral"), 
                          ncols=NA,    
                          xspan = 1, border="lightblue",
                          add_letter = T,
                          cex_letter = 0.6,
                          main=NULL, sub=NULL, xlab=NULL, ylab=NULL,
                          col_letter = "black",
                          main_con = list(lwd=1, col="darkgrey", lty=1),
                          side_con = list(lwd=0.5, col="purple", lty=2),
                          add=F,
                          ...){
  sink(nullfile())
  coords= ct2coord( makeCt( str, seq) )
  sink()
  
  if(!missing(rules)){
    # if rules are in a file
    if(is.character(rules)) rules = readRules(rules)
    
    # if no A given, A= no_rules
    if(missing(A)) A = length(unique(sapply(rules, function(x) x$activity)))
    
    # if no actcols specified, get them
    if(missing(actcols)) actcols = brewer.pal(A, "Set1")
    
    if(is.list(pcols) & length(pcols) == A){
      col.pattern = pcols
    } else {
      col.pattern = list()
      for(col in actcols) {
        col.pattern[[length(col.pattern)+1]] <- pcols
      }
    }
    
    startofPatterns <- find_activity(list(seq=seq,str=str), coords, rules)
  } else {
    startofPatterns = NA
    col.pattern = NA
    actcols = NA
  }
  
  colormask <- make.colormask(str, 
                              patterns =startofPatterns, 
                              col=ncols, 
                              col.pattern = col.pattern, 
                              col.base = actcols
  )
  
  #if(add){
    addToPlot(x=ifelse(missing(x), 0, x),y=ifelse(missing(y), 0, y), coords=coords, 
              col=colormask,
              xspan=xspan, border=border,
              add_letter=add_letter,
              cex_letter=cex_letter,
              col_letter=col_letter,
              main_con=main_con,
              side_con=side_con,
              add=add,
              main=main, sub=sub, xlab=xlab, ylab=ylab,
              ...)
  # } else {
  #   plot_RNA(coords, x=ifelse(missing(x), 0, x), y=ifelse(missing(y), 0, y), col=colormask,
  #            xspan=xspan, border=border,
  #            add_letter=add_letter,
  #            cex_letter=cex_letter,
  #            col_letter=col_letter,
  #            main_con=main_con,
  #            side_con=side_con,
  #            main=main, sub=sub, xlab=xlab, ylab=ylab,
  #            ...
  #   )
  # }
}
