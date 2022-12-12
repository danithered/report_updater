source("functions.R")
force = F

if(file.exists("data.Rds")) lastdata <- readRDS("data.Rds") else lastdata <- NA

#setwd("/home/danielred/data/programs/report_updater/")
wheretolook <- read.table("imports.tsv", sep="\t", header=T) 
jobs <- getjobs(wheretolook) |> do.call(what=rbind)
datas <- list()

for(jr in 1:nrow(jobs)){
  job <- jobs[jr, ]
  
  #check necessary stuff
  if(is.na(job$report) | is.na(job$targetdir) | is.na(job$path)) {
    #warning(paste("Comlulsary inforamtions do not exist for report number:", jr) )
    next
  }
  
  #check report
  if(!file.exists(paste0("reports/", job$report))) {
    warning(paste("Report file does not exist:", job$report) )
    next
  }
  
  #check targetdir
  tryCatch({
      if(!dir.exists(job$targetdir)) {
        tomake = character()
        for(tm in strsplit(job$targetdir, "/")[[1]]){
          #tm = strsplit(job$path, "/")[[1]][1]
          tomake = paste(tomake, tm, sep="/")
          if (!dir.exists(tomake)) { #does it existst so far?
            if(!dir.create(tomake)){ #if not -> tries to make it
              error( paste("Coud not create dir", job$targetdir) )
            }
          }
        }
      }
  }, error = function(e) {next})
  
  #try to compile report
  wheretocache = paste(job$targetdir, 
                  "report_cache", 
                  sep=ifelse(nchar(job$targetdir) > 0 & 
                               substr(job$targetdir, 
                                      nchar(job$targetdir), 
                                      nchar(job$targetdir)) != "/",
                             "/",
                             ""))
  # checkif it has been modified since last - if not then update!
  if(is.na(lastdata)[1]) {
    curr_mtime <- NA
    fromlast <- F
  } else {
    curr_mtime <- get_mtime(path=job$path, ssh=job$ssh, ssh_key=job$ssh_key)
    fromlast <- apply(lastdata$jobs, 1, function(x) all(x==job, na.rm = T))  
  }
  
  hastorun <- force
  if(!any(fromlast)){
    hastorun <- T
  } else if( is.na(lastdata$jobs$updated[fromlast]) ) {
    hastorun = T
  } else if(curr_mtime > lastdata$jobs$updated[fromlast]) {
    hastorun <- T
  }
  
  # run it...
  if( hastorun ) {
    try({ # or it has changed since last run
        rmarkdown::render(paste0("reports/", job$report),
                      params = list(
                        dir = job$path,
                        ssh = job$ssh,
                        ssh_key = job$ssh_key,
                        force=F,
                        cache.path= wheretocache
                      ),
                      output_dir = job$targetdir,
                      knit_root_dir = job$targetdir,
                      intermediates_dir = job$targetdir,
                      output_file = "index.html")
        jobs[jr, "updated"] = Sys.time()
    })
  } else {
    if(any(fromlast)){
      jobs[jr, "updated"] = lastdata$jobs$updated[fromlast]
    }
  }
  
  # try to get parameters
  pf <- get_file("parameters.tsv", job$targetdir)
  if(!is.na(pf)){
    datas[[job$targetdir]] <- read.table(pf, sep="\t", header=F)
    if(!is.na(job$ssh)) file.remove(pf)
  }
}

saveRDS(list(sources = wheretolook, jobs = jobs, params = datas, last_updated = Sys.time()), "data.Rds")
