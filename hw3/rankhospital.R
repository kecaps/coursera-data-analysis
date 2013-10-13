rankhospital <- function(state, outcome, num = "best", data = NULL, data_dir="",filename="outcome-of-care-measures.csv") {
  ## Read outcome data
  if (is.null(data)) {
    if (data_dir != "") {
      filename <- paste(data_dir, filename, sep="/")
    }  
    data <- read.csv(filename, colClasses="character")
  }
  
  ## Check that state and outcome are valid
  data <- data[data$State == state,]
  if (nrow(data) == 0) {
    stop("invalid state")
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
  
  # sort by hospital name so ranking ties will be broken by name
  data <- data[order(data$Hospital.Name),]
  
  deaths = suppressWarnings(as.numeric(data[,col_idx]))
  d_sort = order(deaths, na.last=NA)
  
  if (length(num) == 1 && num < 0) {
    num = length(d_sort)+1+num
  }
  return(data[d_sort[num],'Hospital.Name'])
}

rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
