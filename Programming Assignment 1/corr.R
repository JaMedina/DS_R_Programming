corr <- function(directory, threshold = 0){
   result <- numeric();
   for (file_name in list.files(directory)){
      location       <- paste(directory,"/", file_name, sep = "");
      file_table     <- read.table(location, sep = ",", header = TRUE);
      complete_cases <- complete.cases(file_table);
      file_table     <- file_table[complete_cases,];

      number_of_rows <- nrow(file_table);
      if(number_of_rows > threshold ){
         correlation <- cor(file_table$nitrate, file_table$sulfate, use="complete.obs");
         result      <- c(result, correlation);
      }
   }

   as.numeric(result);
}