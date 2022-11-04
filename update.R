source("functions.R")

#setwd("/home/danielred/data/programs/report_updater/")
wheretolook <- read.table("imports.tsv", sep="\t", header=T) 
jobs <- getjobs(wheretolook) |> do.call(what=rbind)

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
  try({
    rmarkdown::render(paste0("reports/", job$report),
                      params = list(
                        dir = job$path,
                        ssh = job$ssh,
                        ssh_key = job$ssh_key,
                        force=F,
                        cache.path= paste(job$targetdir, 
                                          "report_cache", 
                                          sep=ifelse(nchar(job$targetdir) > 0 & 
                                                           substr(job$targetdir, 
                                                                  nchar(job$targetdir), 
                                                                  nchar(job$targetdir)) != "/",
                                                         "/",
                                                         ""))
                      ),
                      output_dir = job$targetdir,
                      knit_root_dir = job$targetdir,
                      intermediates_dir = job$targetdir,
                      output_file = "index.html")
  })
}

saveRDS(list(wheretolook, jobs, last_updated = Sys.time()), "data.Rds")
