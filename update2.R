source("functions.R")
force = F

#setwd("/home/danielred/data/programs/report_updater/")
wheretolook <- read.table("imports.tsv", sep="\t", header=T) 
jobs <- getjobs(wheretolook) |> do.call(what=rbind)
datas <- list()

if(file.exists("data.Rds")) {
  lastdata <- readRDS("data.Rds") 
  # sync the two
  for(newrow in 1:nrow(jobs)){
    found = F
    for(oldrow in 1:nrow(lastdata$jobs)){
      if( all( na.omit(jobs[newrow,] == lastdata$jobs[oldrow,]) ) ){
        if(found) error("duplication of rows found in data.Rds")
        
        jobs[newrow,] = lastdata$jobs[oldrow,] # owerwriting NA fields
        
        found=T
      }
    }
  }
} else {
  lastdata <- NA
}

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
  wheretocache = mergepath(job$targetdir, 
                           "report_cache/")
  check_dir(wheretocache)
  
  curr_mtime <- get_mtime(path=job$path, ssh=job$ssh, ssh_key=job$ssh_key)
  
  
  hastorun <- force
  if(is.na(job$updated)){
    hastorun <- T
  } else if(curr_mtime > job$updated) {
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
      saveRDS(list(sources = wheretolook, jobs = jobs, params = datas, last_updated = Sys.time()), "data.Rds")
    })
  } 
  
  # try to get parameters
  pf <- get_file("parameters.tsv", job$targetdir)
  if(!is.na(pf)){
    datas[[job$targetdir]] <- read.table(pf, sep="\t", header=F)
    if(!is.na(job$ssh)) file.remove(pf)
  }
}

saveRDS(list(sources = wheretolook, jobs = jobs, params = datas, last_updated = Sys.time()), "data.Rds")
