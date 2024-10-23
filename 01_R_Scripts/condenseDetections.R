library(stringr)
library(readr)
library(ggplot2)
library(SPlit)
library(dplyr)
library(tidyr)
library(dplyr)

#Manipulation to get into the folder autoUC_MedSea
current_dir <- getwd()
parent_dir <- dirname(current_dir)
setwd(parent_dir)

#Save all the different thresholds
incrementStep = 20 #Change the increment step here (default 0.05)

#Load the CSV file
data_frame <- read.table("./summaryVideos.csv", header = FALSE, sep = " ")

colnames(data_frame) <- c('file', 'class', 'x', 'y', 'w', 'h', 'conf')

#Error in older versions, maybe not needed anymore
for(i in 1:nrow(data_frame)){
  if(data_frame$class[i] == 'Sparus aurata' & !is.na(data_frame$class[i])){
    data_frame$class[i] = 'Sparus_aurata'
  }
}

for(k in 1:incrementStep){
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  names <- readLines("./class_names.txt")
  #data_frame <- data_frame[,-1]

  threshold <- k*0.05
  
  dataFrameConf <- subset(data_frame, conf > threshold)
  
  #Drop the Overlaps!
  dataFrameConf <- drop_na(dataFrameConf)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  videoName <- dataFrameConf$file
  
  cutVideos <- vapply(strsplit(videoName,"_"), `[`, 1, FUN.VALUE=character(1))
  
  sumVideoName <- list()
  
  for(i in 1:length(cutVideos)){
    if(any(sumVideoName %in% cutVideos[i]) == FALSE){
      sumVideoName <- append(sumVideoName, cutVideos[i])
    }
  }
  
  finalSummary <- data.frame(matrix(ncol = 21))
  colnames(finalSummary) <- c('file', names)
  
  for(i in 1:length(sumVideoName)){
    currentVideo <- sumVideoName[i]
    
    currentData <- dataFrameConf[grep(currentVideo, dataFrameConf$file), ]
    
    # Count occurrences of each name
    nameCounts <- currentData %>%
      group_by(class) %>%
      summarise(Count = n())
    
    transNameCounts <- as.data.frame(t(nameCounts))
    colnames(transNameCounts) <- transNameCounts[1,]
    
    transNameCounts <- transNameCounts[-1,]
    
    currentVideo <- as.data.frame(currentVideo)Z
    colnames(currentVideo) <- 'file'
    
    transNameCounts <- cbind(currentVideo, transNameCounts)
    
    finalSummary <- bind_rows(transNameCounts, finalSummary)
  }
  
  finalSummary[is.na(finalSummary)] <- 0
  
  #Delete the last row
  lastLine <- nrow(finalSummary)
  finalSummary <- finalSummary[-lastLine,]
  
  #Save the dataframe in CSV format into the directory
  write.table(finalSummary,file=paste0('./03_Datasets/01_condensedCounts_Detections/condensedVideoCount', threshold,  '.csv'),col.names = TRUE,row.names = FALSE)
  print(paste0(threshold, ' done!'))
}