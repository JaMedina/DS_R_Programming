best <- function(state, outcome){
   outcome_measures <- read.csv("outcome-of-care-measures.csv", colClasses = "character");
   if(!state %in% outcome_measures$State){
      stop("Invalid State.");
   }

   # Filer by received state. Gets the indexes of the state and den retrieves de row of those indexes.
   outcome_measures <- outcome_measures[outcome_measures$State==state,];
   number_of_deaths <- numeric();

   if(outcome == 'heart attack'){
      # Column 11 contains information about heart attacks.
      number_of_deaths <- as.numeric(outcome_measures[,11]);
   } else if (outcome == 'heart failure'){
      # Column 17 contains information about heart failure.
      number_of_deaths <- as.numeric(outcome_measures[,17]);
   } else if (outcome == 'pneumonia'){
      # Column 23 contains information about pneumonia.
      number_of_deaths <- as.numeric(outcome_measures[,23]);
   } else {
      stop("Invalud outcome");
   }

   ## Get the msmallest number of deaths in the number of deaths array removing the NA values. and hets the 
   # rows which contains the smallest number
   smallest_rate_index <- which(number_of_deaths == min(number_of_deaths, na.rm=T));
   return (outcome_measures$Hospital.Name[smallest_rate_index]);
}