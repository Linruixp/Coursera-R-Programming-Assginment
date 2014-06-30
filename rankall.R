rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that  outcome are valid
  m <- tolower(paste(strsplit(outcome, " ")[[1]], collapse=""))
  if (! m %in% c("heartfailure", "heartattack", "pneumonia")) {
      stop("invalid outcome")  
  }
  
  ## Get the proper outcome columne induex for ordering
  if ( m %in% c("heartfailure")) j <- 17
  if ( m %in% c("heartattack")) j <- 11
  if ( m %in% c("pneumonia")) j <- 23
  
  ## coerce to numeric
  data[,17] <- as.numeric(data[,17])
  data[,11] <- as.numeric(data[,11])
  data[,23] <- as.numeric(data[,23])
  
  ## Get states' names
  states <- names(table(data$State))
  
  
  ## For each state, find the hospital of the given rank
  h <- NULL
  for (s in states){
      ordered_data <- data[data$State == s & !is.na(data[,j]), ]
      ordered_data <- ordered_data[with(ordered_data, order(ordered_data[,j], Hospital.Name)),]
  
      last <- nrow(ordered_data)
      if ( num == "best") {
          h <- cbind(h,ordered_data[1,"Hospital.Name"])
          next
      }
      if ( num == "worst") {
          h <- cbind(h,ordered_data[last,"Hospital.Name"])    
          next
      }
      if ( (num <- as.integer(num)) <= last ) {
            h <- cbind(h, ordered_data[num,"Hospital.Name"])
      }else {
            h <- cbind(h, c(NA))
      }
      
  }
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  result <- data.frame(hospital=h[1,], state=states)
  row.names(result) <- states
  result
  
}