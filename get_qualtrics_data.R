get_qualtrics_data <- function(qualtrics_data_path){#qualtrics_data_path<-paste0(data.directory,"raw_files/exp20180521/ITCConstrual-attempt4_March+22,+2018_16.47.csv")
  #this might seem redundant but I plan on adding code to tidy
  rqd<-data.table(read.csv(qualtrics_data_path,stringsAsFactors = FALSE))
  
  return(rqd)
}