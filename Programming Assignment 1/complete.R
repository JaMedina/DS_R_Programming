complete <- function(directory, id=1:332){
   result <- data.frame();

   for(file_number in rep(id)){
      location       <- paste(directory,"/",sprintf("%03d", file_number),".csv",sep ="");
      file_table     <- read.table(location, sep=",", header=TRUE);

      ## Filter complete cases
      complete_cases <- complete.cases(file_table);
      file_table     <- file_table[complete_cases,];
      
      ## Append Complete cases
      result<-rbind(result,cbind(file_number,nrow(file_table)))

   }

   colnames(result)  <- c("id", "nobs");
   result;
}