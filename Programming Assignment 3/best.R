best <- function(state, outcome){
   outcome_measures <- read.csv("outcome-of-care-measures.csv", colClasses = "character");
   if(!state %in% data$State){
      stop("Invalid State.");
   }

   outcome_measures <- outcome_measures[outcome_measures$State==state,]
}


https://github.com/liuminzhao/computing-data-analysis/blob/master/best.R