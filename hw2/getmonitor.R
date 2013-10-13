getmonitor <- function(id, directory, summarize = FALSE) {
    ## 'id' is a vector of length 1 indicating the monitor ID
    ## number. The user can specify 'id' as either an integer, a
    ## character, or a numeric.
    
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'summarize' is a logical indicating whether a summary of
    ## the data should be printed to the console; the default is
    ## FALSE
    if (is.character(id) && grepl(".csv$",id)) {
      # do nothing
    } else {
      if (is.numeric(id)) {
        id <- sprintf("%03d",id)
      }
      id <- paste(id, "csv", sep=".")
    }
    file <- paste(directory, id, sep="/")
    monitor <- read.csv(file, header=T)
    if (summarize) {
      print(summary(monitor))
      invisible(monitor)
    } else {
      monitor
    }
}