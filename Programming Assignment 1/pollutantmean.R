pollutantmean <- function(directory, pollutant, id=1:332){
   pollutantData <- data.frame();

   for(file in rep(id)){
      ## Gets the path of the file
      location      <- paste(directory, "/", sprintf("%03d", file),".csv",sep = "");

      ## Reads the file separated by commas
      file_table    <- read.table(location, sep = ",", header = TRUE);

      ## Appends the file to the pollutantData
      pollutantData <- rbind(pollutantData, file_table);
   }

   result   <- mean(as.matrix(pollutantData[pollutant]), na.rm = TRUE);
   result;
}