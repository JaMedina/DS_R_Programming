rankall <- function(outcome, num = "best"){
   outcome_measures  <- read.csv("outcome-of-care-measures.csv", colClasses = "character");
   states            <- outcome_measures$State;
   states            <- sort(unique(states));

   hospital          <- rep("", length(states));

   for (i in 1:length(states)){
      state_information <- outcome_measures[outcome_measures$State == states[i],];

      if(outcome == 'heart attack'){
         number_of_deaths <- as.numeric(state_information[,11]);
      } else if (outcome == 'heart failure'){
         number_of_deaths <- as.numeric(state_information[,17]);
      } else if (outcome == 'pneumonia'){
         number_of_deaths <- as.numeric(state_information[,23]);
      } else {
         stop("Invalud outcome");
      }

      all_rankings <- rank(number_of_deaths, na.last = NA);

      if(num == "best"){
         ranking     <- 1;
      } else if (num == "worst"){
         ranking     <- length(all_rankings);
      } else if (num <= length(all_rankings)){
         ranking     <- num;
      } else {
         ranking     <- NA
      }

      if(is.na(ranking)){
         hospital[i] <- NA
      } else {
         hospital[i] <- state_information$Hospital.Name[order(number_of_deaths, state_information$Hospital.Name)[ranking]];
      }
   }

   return (data.frame(hospital=hospital, state=states));
}