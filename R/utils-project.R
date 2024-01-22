qrender <- function(){
  rmarkdown::render("paper/paper.Rmd")
  system("quarto render")
}

mkdirif <- function(dir){
  if(!fs::is_dir(dir)){
    fs::dir_create(dir)
  }
}

render <- function(file){
  db <- getdb()
  parent <- dirname(file)
  current_md5 <- tools::md5sum(file)
  update <- !(db$md5[db$files == names(current_md5)] == current_md5)
  if(update){
    rmarkdown::render(file)
    db$md5[db$files == names(current_md5)] <- current_md5
    saveRDS(db, ".dbcache/dbcache.rds")
  }else{
    cat("file not changed!")
  }
}

getdb <- function(){
  mkdirif(file.path(".dbcache"))
  dbpath <- here::here(".dbcache", "dbcache.rds")
  if(!fs::file_exists(dbpath)){
    files <- list.files(pattern = "Rmd", full.names = TRUE, recursive = TRUE)
    md5 <- tools::md5sum(files)
    db <- data.frame(
      file = gsub("\\./", "", names(md5)),
      md5 = md5
    )
    names(db) <- c("files", "md5")
    rownames(db) <- NULL
    saveRDS(db, dbpath)
  }
  db <- readRDS(dbpath)
}
