rankhospital <- function(state, outcome, num="best"){
   outcome_measures <- read.csv("outcome-of-care-measures.csv", colClasses = "character");
   if(!state %in% outcome_measures$State){
      stop("Invalid State.");
   }

   outcome_measures <- outcome_measures[outcome_measures$State==state,];
   number_of_deaths <- numeric();

   if(outcome == 'heart attack'){
      number_of_deaths <- as.numeric(outcome_measures[,11]);
   } else if (outcome == 'heart failure'){
      number_of_deaths <- as.numeric(outcome_measures[,17]);
   } else if (outcome == 'pneumonia'){
      number_of_deaths <- as.numeric(outcome_measures[,23]);
   } else {
      stop("Invalud outcome");
   }

   all_rankings <- rank(number_of_deaths, na.last = NA);

   if(num == "best"){
      ranking <- 1;
   } else if (num == "worst"){
      ranking <- length(all_rankings);
   } else if (num <= length(all_rankings)){
      ranking <- num;
   } else {
      return(NA);
   }
   
   return (outcome_measures$Hospital.Name[order(number_of_deaths, outcome_measures$Hospital.Name)[ranking]]);
}