best <- function(state, outcome, data = NULL, data_dir="",filename="outcome-of-care-measures.csv") {
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

  deaths = suppressWarnings(as.numeric(data[,col_idx]))
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  return(sort(data[deaths==min(deaths, na.rm=T),'Hospital.Name'])[1])
}
