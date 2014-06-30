rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  if (!state %in% data$State){
      stop("invalid state")
  }
  m <- tolower(paste(strsplit(outcome, " ")[[1]], collapse=""))
  if (! m %in% c("heartfailure", "heartattack", "pneumonia")) {
      stop("invalid outcome")  
  }
  
  ## Select the proper outcome columne to order
  if ( m %in% c("heartfailure")) j <- 17
  if ( m %in% c("heartattack")) j <- 11
  if ( m %in% c("pneumonia")) j <- 23
  
  ## coerce to numeric
  data[,17] <- as.numeric(data[,17])
  data[,11] <- as.numeric(data[,11])
  data[,23] <- as.numeric(data[,23])
  
  
  ## Return hospital name in that state with the given rank
  
  ## Order data by hospital's names
  
  ordered_data <- data[data$State == state & !is.na(data[,j]), ]
  ordered_data <- ordered_data[with(ordered_data, order(ordered_data[,j], Hospital.Name)),]
   
  
  last <- nrow(ordered_data)
  
  if ( num == "best") return(ordered_data[1,"Hospital.Name"])
  if ( num == "worst") return(ordered_data[last,"Hospital.Name"])
  if ( (num <- as.integer(num)) <= last ) {
    return(ordered_data[num,"Hospital.Name"])
  }else {
    return(NA)
  }
  
  ## 30-day death rate
}