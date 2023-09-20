if(!require(xml2)) {install.packages("xml2"); library(xml2)}

get_child <- function(node, names, as = NA){
  w <-unlist(sapply(names, function(name, nodes) which(name == nodes), children(node) ))
  if (length(w) != 1) {
    #warning("get_child: number of correct children does ot equals to 1!")
    return(NA)
  }
  if(!is.na(as)) {
    if(as == "int") return(xml_integer(xml_child(node, w)))
    else if(as == "double") return(xml_double(xml_child(node, w)))
    else if(as == "string") return(xml_text(xml_child(node, w)))
    else if(as == "logical") return( ifelse(xml_integer(xml_child(node, w)) == 0, F, T ))
  }
  return(xml_child(node, w))
}

children <- function(node){
  xml_name(xml_children(node))
}

