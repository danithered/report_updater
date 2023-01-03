plotNeigh <- function(neigh_tipus, 
                      ylim=NA,
                      xlim=NA,
                      asp=NA, 
                      xlab=NA, 
                      ylab=NA, 
                      axes=NA,
                      las=NA,
                      bty=NA,
                      main=NA,
                      ...)
{
  n_inic_x = n_inic_y = c()
  maxDist = ceiling(log2( neigh_tipus - 1))
  for(x  in -maxDist:maxDist){ 
    for(y in -maxDist:maxDist){ 
      if( 2^abs(x) + 2^abs(y) <= neigh_tipus ){
        n_inic_x <- c(n_inic_x, x)
        n_inic_y <- c(n_inic_y, y)
      }
    } 
  }
  
  k <- matrix(rep(0, (maxDist*2+1)^2 ), ncol=maxDist*2+1)
  middle <- maxDist + 1
  
  for(i in 1:length(n_inic_x)){
    k[middle + n_inic_x[i], middle +n_inic_y[i] ] <- 1
  }
  k[middle+0,middle+0] <- 2
  
  #parameters
  if(is.na(ylim)) ylim=c(-maxDist-0.5,maxDist+0.5)
  if(is.na(xlim)) xlim=c(-maxDist-0.5,maxDist+0.5)
  if(is.na(asp)) asp=1
  if(is.na(xlab)) xlab=""
  if(is.na(ylab)) ylab=""
  if(is.na(axes)) {
    axes=F
    draw.axis=T
  } else {
    draw.axis=F
  }
  if(is.na(las)) las=1
  if(is.na(bty)) bty="n"
  if(is.na(main)) main=neigh_tipus
  
  #draw grid
  image(-maxDist:maxDist, -maxDist:maxDist, k, 
        ylim=ylim,
        xlim=xlim,
        asp=asp, 
        xlab=xlab, 
        ylab=ylab, 
        axes=axes,
        las=las,
        bty=bty,
        main=main,
        ...) 
  segments(-(maxDist+1):maxDist+0.5, 
           rep(-maxDist-0.5, maxDist*4 ), 
           -(maxDist+1):maxDist+0.5, 
           rep(maxDist+0.5, maxDist*4 ),
           col="grey")
  segments( y0=-(maxDist+1):maxDist+0.5, 
            x0=rep(-maxDist-0.5, maxDist*4 ), 
            y1=-(maxDist+1):maxDist+0.5,
            x1=rep(maxDist+0.5, maxDist*4 ),
            col="grey")
  #abline(h=-(maxDist+1):maxDist+0.5, col="grey")
  #abline(v=-(maxDist+1):maxDist+0.5, col="grey")
  if(draw.axis){
    axis(1, lwd=0, at=-maxDist:maxDist)
    axis(2, lwd=0, at=-maxDist:maxDist, las=1)
  }
  return( sum(k != 0) )
}

