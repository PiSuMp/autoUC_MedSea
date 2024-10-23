library(tidyr)
library(ggplot2)
library(reshape2)
library(tibble)
library(dplyr)
library(cowplot)
library(pracma)

#Manipulation to get into the folder autoUC_MedSea
current_dir <- getwd()
parent_dir <- dirname(current_dir)
setwd(parent_dir)

csvList <- list.files(path = "./03_Datasets/01_condensedCounts_Detections/", pattern = "csv", full.names = TRUE)

plot_list <- list()

names <- readLines("class_names.txt")

for(l in 1:length(names)){
  aucValues <- as.data.frame(matrix(nrow=length(csvList), ncol=3))
  colnames(aucValues) <- c('threshold', 'specificity', 'sensitivity')
  
  metricValues <- as.data.frame(matrix(nrow=length(csvList), ncol=3))
  colnames(metricValues) <- c('threshold', 'metrics', 'cat')
  
  specValues <- as.data.frame(matrix(nrow=length(csvList), ncol=3))
  colnames(specValues) <- c('threshold', 'metrics', 'cat')
  
  senValues <- as.data.frame(matrix(nrow=length(csvList), ncol=3))
  colnames(senValues) <- c('threshold', 'metrics', 'cat')
  
  overallSpecies <- names[l]

  #Make it for multiple files
  for(k in 1:(length(csvList)-1)){
    
    #Load the detections
    detections <- csvList[k]
    detections <- read.csv(detections, sep = " ") 
    
    #Only keep current Species
    detectionsNew <- detections
    detectionsNew <- detectionsNew[1]
    detectionsNew[overallSpecies] <- detections[overallSpecies]
    
    detections <- detectionsNew
    
    #MANUAL VIDEO COUNTS
    manualVideoCount <- read.csv("./03_Datasets/03_Manual_Video_Count/manualVideoCount.csv")
    #Only keep current Species
    manualVideoCountNew <- manualVideoCount
    manualVideoCountNew <- manualVideoCountNew[1]
    manualVideoCountNew[overallSpecies] <- manualVideoCount[overallSpecies]
    
    manualVideoCount <- manualVideoCountNew
  
    #Get rid of the class that only appear in a certain numnber of frames per video (unlikely to be a fish!)
    minFishCount <- 4
    
    for(i in 1:nrow(detections)){
      for(j in 1:ncol(detections)){
        if(detections[i,j] <= minFishCount){
          detections[i,j] = 0
        }
      }
    }
    
    # Function to convert counts to presence/absence
    convert_to_presence_absence <- function(data) {
      presence_absence <- data
      presence_absence[, -1][presence_absence[, -1] > 0] <- 1
      return(presence_absence)
    }
    
    # Convert counts to presence/absence for both files
    detections_presence_absence <- convert_to_presence_absence(detections)
  
    #MANUAL COUNTS
    for(i in 1:nrow(detections_presence_absence)){
      if (detections_presence_absence$file[i] %in% manualVideoCount$file) {
        #print(paste0('ManualCount available for ', detections_presence_absence$file[i]))
      } else{
        #print(paste0('ManualCount NOT available for ', detections_presence_absence$file[i]))
        detections_presence_absence$file[i] <- NA
      }
    }
    
    #Delete the non existing groundtruth files
    detections_presence_absence <- drop_na(detections_presence_absence)
    
    finalAccuracy <- as.data.frame(matrix(1:(nrow(manualVideoCount)*2), nrow = nrow(manualVideoCount), ncol = 2))
    colnames(finalAccuracy) <- c('file', 'accuracy')
    
    overallTP <- 0
    overallTN <- 0
    overallFP <- 0
    overallFN <- 0
    
    
      for(i in 1:nrow(manualVideoCount)){
        currentVideo <- manualVideoCount$file[i]
        
        if(currentVideo %in% detections_presence_absence$file){
          currentCount <- subset(detections_presence_absence, file == currentVideo)
          
          currentDetection <- subset(detections_presence_absence, file == currentVideo)
          currentManualCount <- subset(manualVideoCount, file == currentVideo)
          
          if(currentDetection[[overallSpecies]] == 1 & currentManualCount[[overallSpecies]] == 1){
            overallTP = overallTP + 1
          }else if(currentDetection[[overallSpecies]] == 0 & currentManualCount[[overallSpecies]] == 1){
            overallFN = overallFN + 1
          }else if(currentDetection[[overallSpecies]] == 1 & currentManualCount[[overallSpecies]] == 0){
            overallFP = overallFP + 1
          }else if(currentDetection[[overallSpecies]] == 0 & currentManualCount[[overallSpecies]] == 0){
            overallTN = overallTN + 1
          }
        
          
        }else{
          #Video of detections not available means that there were no detections!
          currentManualCount <- subset(manualVideoCount, file == currentVideo)
          
          truePositive = 0
          falsePositive = 0
          if(currentManualCount[[overallSpecies]] == 1){
            overallFN = overallFN + 1
          }else if(currentManualCount[[overallSpecies]] == 0){
            overallTN = overallTN + 1
          }
        }
      }
    
    overallAcc <- (overallTP + overallTN) / (overallTP + overallTN + overallFP + overallFN)
    overallSensitivity <- (overallTP) / (overallTP + overallFN)
    overallSpecificity <- (overallTN) / (overallTN + overallFP)

    sumFP <- sum(finalAccuracy$FP)
    
    aucValues$threshold[k] <- (k*0.5)/10
    aucValues$sensitivity[k] <- overallSensitivity
    aucValues$specificity[k] <- overallSpecificity
    
    metricValues$threshold[k] <- (k*0.5)/10
    metricValues$metrics[k] <- overallAcc
    metricValues$cat <- 'Accuracy'
    
    specValues$threshold[k] <- (k*0.5)/10
    specValues$metrics[k] <- overallSpecificity
    specValues$cat <- 'Specificity'
    
    senValues$threshold[k] <- (k*0.5)/10
    senValues$metrics[k] <- overallSensitivity
    senValues$cat <- 'Sensitivity'
  }
  
  newRow <- as.data.frame(t(c(0.00, 0, 1)))
  colnames(newRow) <- c('threshold', 'specificity', 'sensitivity')
  
  aucValues <- rbind(newRow, aucValues)
  aucValues <- drop_na(aucValues)
  
  #Calculate AUC
  
  #This is used to 'smooth' the graphs since there can be multiple Sen values per Spec value
  #We want to use the mean Sen value per Spec and only use the highest threshold for it
  
  aucValues <- aucValues %>%
    group_by(specificity) %>%
    summarise(sensitivity = mean(sensitivity), threshold = max(threshold))

  aucValues$species <- overallSpecies

  #Sort the points in ascending order of specificity
  order_index <- order(aucValues$specificity)
  sensitivity <- aucValues$sensitivity[order_index]
  specificity <- aucValues$specificity[order_index]
  
  #Calculate AUC using trapezoidal rule
  auc_value <- trapz(specificity, sensitivity)
  
  #Print the AUC value
  cat("AUC:", auc_value, "\n")
  
  #PLOT AUC Curve
  p <- print(ggplot(data=aucValues, aes(x=1-specificity, y=sensitivity, group=1)) +
    geom_line(color = 'blue')+
    ylim(0,1) +
    xlim(0,1) +
    geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color='red', linetype=2) +
    xlab('1-Specificity') +
    ylab('Sensitivity') +
    theme_bw() +
    theme(text=element_text(size=20)))
  
  aucValues$species <- overallSpecies
  aucValues$value <- round(auc_value,2)
  
  plot_list[[l]] = aucValues
  
  #aucPlot
  nameFile <- paste0('./05032024_AUC_plot_', overallSpecies, '.pdf')
  #dev.copy2pdf(file=nameFile, height = 5, width = 10)
  
  values <- rbind(specValues, senValues)
  values <- rbind(values, metricValues)
  values <- drop_na(values)
  
  #PLOT Metrics Curve
  print(ggplot(data=values, aes(x=threshold, y=metrics, color=cat)) +
    geom_line() +
    scale_color_discrete(name = 'Metric')+
    scale_x_continuous(breaks=seq(0, 1, 0.05), guide = guide_axis(angle = 45)) +
    scale_y_continuous(breaks=seq(0, 1, 0.05)) +
    xlab('Threshold') +
    ylab('') +
    theme_bw() +
    theme(text=element_text(size=20)) +
    geom_segment(aes(x = 0.05, xend = 0.05, y = 0, yend = 1), color='purple', linetype=2) +
    geom_segment(aes(x = 0.55, xend = 0.55, y = 0, yend = 1), color='purple', linetype=2) +
    geom_segment(aes(x = 0.85, xend = 0.85, y = 0, yend = 1), color='purple', linetype=2))
  
  #metricPlot
  nameFile <- paste0('./05032024_metric_plot_', overallSpecies, '.pdf')
  #dev.copy2pdf(file=nameFile, height = 5, width = 10)
}


#Acutal PLOTTING of the AUC curves per species in one big graph
dresults <- lapply(plot_list, as.data.frame) %>%
  bind_rows(.id = "id")

aucValue <- dresults[!duplicated(aucValues$species),]
aucValue <- aucValue[!duplicated(aucValue$species),]

dresults$value <- round(dresults$value, 2)

dummyVar <- as.data.frame(matrix(ncol = 1, nrow = nrow(dresults)))

for(i in 1:nrow(dresults)){
  dummyVar$V1[i] <- paste0('AUC: ', sprintf("%.2f", dresults$value[i]))
}

dresults$value <- dummyVar

xpos = c(-Inf)
ypos =  c(-Inf)

hjustvar = c(-1.67)
vjustvar = c(-0.25)

ggplot(data=dresults, aes(x=1-specificity, y=sensitivity, group=1)) +
  geom_line(color = 'blue')+
  ylim(0,1) +
  xlim(0,1) +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color='red', linetype=2) +
  xlab('1-Specificity') +
  ylab('Sensitivity') +
  theme_bw() +
  theme(text=element_text(size=12)) +
  geom_label(aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label=dresults$value$V1)) +
  facet_wrap(~species)

nameFile <- paste0('.//06032024_overall_AUC_plot.pdf')
#dev.copy2pdf(file=nameFile, height = 8, width = 12)

