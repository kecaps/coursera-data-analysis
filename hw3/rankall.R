rankall <- function(outcome, num = "best", data = NULL, data_dir="",filename="outcome-of-care-measures.csv") {
  ## Read outcome data
  if (is.null(data)) {
    if (data_dir != "") {
      filename <- paste(data_dir, filename, sep="/")
    }  
    data <- read.csv(filename, colClasses="character")
  }

  outcome_idx = c(11,17,23)
  names(outcome_idx) = tolower(c("Heart Attack", "Heart Failure", "Pneumonia"))
  
  col_idx <- outcome_idx[tolower(outcome)]
  if (is.na(col_idx)) {
    stop("invalid outcome")
  }
  
  if (!is.numeric(num)) {
    num <- tolower(num)
    if (num == "best") { 
      num = 1
    } else if(num == "worst") { 
      num = -1
    } else {
      stop("invalid rank")
    }
  }
  
  state_data = split(data, data$State)
  hospital = sapply(state_data, function(data) {
    # sort by hospital name so ranking ties will be broken by name
    data <- data[order(data$Hospital.Name),]
    
    deaths = suppressWarnings(as.numeric(data[,col_idx]))
    d_sort = order(deaths, na.last=NA)
    
    if (length(num) == 1 && num < 0) {
      sort_num = length(d_sort)+1+num
    } else {
      sort_num = num
    }
    return(data[d_sort[sort_num],'Hospital.Name'])
  })
  return(data.frame(hospital,state=names(state_data)))
}

