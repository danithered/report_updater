type2noA <- Vectorize(function(x){
  if(x==0) return("A 0")
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
  return( paste("A",length(acts)) )
  
})

type2A <- Vectorize(function(x){
  if(x==0) return(0)
  if(x==-1) return(NA)
  
  m <- ceiling(log(x,2))
  acts <- c()
  
  for(i in m:0){
    if(x %/% 2^i != 0){
      acts <- c(acts, i)
      x <- x-2^i
      if(x == 0) break
    }
  }
  return( length(acts) )
  
})
type2vec <- (function(x, no=1){
  if(x==0) return(rep(F, no))
  if(x==-1) return(NA)
  
  m <- ceiling(log(x,2))
  acts <- c()
  
  for(i in m:0){
    if(x %/% 2^i != 0){
      acts <- c(acts, i)
      x <- x-2^i
      if(x == 0) break
    }
  }
  
  acts <- acts+1
  out <- rep(F, max(no, acts))
  out[acts] <- T
  
  return(out)
})

enzN <- Vectorize(function(x, as.text=F){
  if(x==0) return("parazite")
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
  
  if(as.text) return( paste0("E[", paste(sort(acts+1), collapse = ","), "]") )
  return( as.expression(bquote(E[.(paste(sort(acts+1), collapse = ","))])) )
})

