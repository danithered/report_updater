# read in rules
setwd("/home/danielred/data/programs/mcrs_to_scm/IN/str/")

rules <- list()

for(id in 1:13){
  rule <- readLines(paste(id, "str.txt", sep="/"))
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

saveRDS(rules, "/home/danielred/data/programs/mcrs_to_scm/explorer/examiner/rules.RDS")
