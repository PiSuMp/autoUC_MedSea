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

aucValues <- as.data.frame(matrix(nrow=length(csvList), ncol=3))
colnames(aucValues) <- c('threshold', 'specificity', 'sensitivity')

metricValues <- as.data.frame(matrix(nrow=length(csvList), ncol=3))
colnames(metricValues) <- c('threshold', 'metrics', 'cat')

specValues <- as.data.frame(matrix(nrow=length(csvList), ncol=3))
colnames(specValues) <- c('threshold', 'metrics', 'cat')

senValues <- as.data.frame(matrix(nrow=length(csvList), ncol=3))
colnames(senValues) <- c('threshold', 'metrics', 'cat')

names <- readLines("class_names.txt")

#Make it for multiple files
for(k in 1:(length(csvList)-1)){
  #Load the detections
  detections <- csvList[k]
  detections <- read.csv(detections, sep = " ") 
  
  #MANUAL VIDEO COUNTS
  manualVideoCount <- read.csv("./03_Datasets/03_Manual_Video_Count/manualVideoCount.csv")
  
  if("transNameCounts" %in% colnames(detections)){
    # Clean up the dataframes
    detections <- subset(detections, select = -c(transNameCounts))
  }

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
    
    for(i in 1:nrow(manualVideoCount)){
      truePositive <- 0
      falsePositive <- 0
      trueNegative <- 0
      falseNegative <- 0
      
      currentVideo <- manualVideoCount$file[i]
      
      if(currentVideo %in% detections_presence_absence$file){
        currentCount <- subset(detections_presence_absence, file == currentVideo)
        
        for(j in 1:length(names)){
          currentDetection <- subset(detections_presence_absence, file == currentVideo)
          currentManualCount <- subset(manualVideoCount, file == currentVideo)
          
          currentSpecies <- names[j]
          
          #Get the FPs, FNs, TPs and TNs
          if(currentDetection[[currentSpecies]] == 1 & currentManualCount[[currentSpecies]] == 1){
            truePositive = truePositive + 1
          }else if(currentDetection[[currentSpecies]] == 0 & currentManualCount[[currentSpecies]] == 1){
            falseNegative = falseNegative + 1
          }else if(currentDetection[[currentSpecies]] == 1 & currentManualCount[[currentSpecies]] == 0){
            falsePositive = falsePositive + 1
          }else if(currentDetection[[currentSpecies]] == 0 & currentManualCount[[currentSpecies]] == 0){
            trueNegative = trueNegative + 1
          }
        }
        #Save the metrics
        finalAccuracy$file[i] <- currentVideo
        finalAccuracy$TP[i] <- truePositive
        finalAccuracy$FP[i] <- falsePositive
        finalAccuracy$TN[i] <- trueNegative
        finalAccuracy$FN[i] <- falseNegative
        finalAccuracy$accuracy[i] <- (truePositive + trueNegative) / (truePositive + trueNegative + falsePositive + falseNegative)
        finalAccuracy$sensitivity[i] <- (truePositive) / (truePositive + falseNegative)
        finalAccuracy$specificity[i] <- (trueNegative) / (trueNegative + falsePositive)
        
        #Cohens Kappas a bit more complicated
        UB <- finalAccuracy$accuracy[i] - ((truePositive + falsePositive)*(truePositive + falseNegative) + (falseNegative + trueNegative)*(trueNegative + falsePositive)) / (truePositive + trueNegative + falsePositive + falseNegative)**2
        LB <- 1 - ((truePositive + falsePositive)*(truePositive + falseNegative) + (falseNegative + trueNegative)*(trueNegative + falsePositive)) / (truePositive + trueNegative + falsePositive + falseNegative)**2
        finalAccuracy$KappaStat[i] <- UB / LB
        
        #TSS easier again
        finalAccuracy$TSS[i] <- finalAccuracy$sensitivity[i] + finalAccuracy$specificity[i] - 1
        
      }else{
        #Video of detections not available means that there were no detections!
        for(j in 1:length(names)){
          currentManualCount <- subset(manualVideoCount, file == currentVideo)
          
          currentSpecies <- names[j]
          
          truePositive = 0
          falsePositive = 0
          if(currentManualCount[[currentSpecies]] == 1){
            falseNegative = falseNegative + 1
          }else if(currentManualCount[[currentSpecies]] == 0){
            trueNegative = trueNegative + 1
          }
        }
        #Save the metrics
        finalAccuracy$file[i] <- currentVideo
        finalAccuracy$TP[i] <- truePositive
        finalAccuracy$FP[i] <- falsePositive
        finalAccuracy$TN[i] <- trueNegative
        finalAccuracy$FN[i] <- falseNegative
        finalAccuracy$accuracy[i] <- (truePositive + trueNegative) / (truePositive + trueNegative + falsePositive + falseNegative)
        finalAccuracy$sensitivity[i] <- (truePositive) / (truePositive + falseNegative)
        finalAccuracy$specificity[i] <- (trueNegative) / (trueNegative + falsePositive)
        
        UB <- finalAccuracy$accuracy[i] - ((truePositive + falsePositive)*(truePositive + falseNegative) + (falseNegative + trueNegative)*(trueNegative + falsePositive)) / (truePositive + trueNegative + falsePositive + falseNegative)**2
        LB <- 1 - ((truePositive + falsePositive)*(truePositive + falseNegative) + (falseNegative + trueNegative)*(trueNegative + falsePositive)) / (truePositive + trueNegative + falsePositive + falseNegative)**2
        finalAccuracy$KappaStat[i] <- UB / LB
        
        finalAccuracy$TSS[i] <- finalAccuracy$sensitivity[i] + finalAccuracy$specificity[i] - 1 
      }

    }
    
    overallAcc <- mean(finalAccuracy$accuracy)
    overallSensitivity <- mean(finalAccuracy$sensitivity)
    overallSpecificity <- mean(finalAccuracy$specificity)
    overallKappaStat <- mean(finalAccuracy$KappaStat)
    overallTSS <- mean(finalAccuracy$TSS)
    
    overallALL <- overallAcc + overallSensitivity + overallSpecificity + overallKappaStat + overallTSS
    
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
#Sort the points in ascending order of specificity
order_index <- order(aucValues$specificity)
sensitivity <- aucValues$sensitivity[order_index]
specificity <- aucValues$specificity[order_index]

#Calculate AUC using trapezoidal rule
auc_value <- trapz(specificity, sensitivity)

#Print the AUC value
cat("AUC:", auc_value, "\n")

#PLOT AUC Curve
xpos = c(-Inf)
ypos =  c(-Inf)

hjustvar = c(-8.32)
vjustvar = c(-1)

aucPlot <- ggplot(data=aucValues, aes(x=1-specificity, y=sensitivity, group=1)) +
  geom_line(color = 'blue')+
  ylim(0,1) +
  xlim(0,1) +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color='red', linetype=2) +
  xlab('1-Specificity') +
  ylab('Sensitivity') +
  theme_bw() +
  theme(text=element_text(size=20))+
  geom_label(aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label='   AUC: 0.93'))

aucPlot
#dev.copy2pdf(file='21022024_AUC_plot.pdf', height = 5, width = 10)

values <- rbind(specValues, senValues)
values <- rbind(values, metricValues)
values <- drop_na(values)

#PLOT Metrics Curve
metricPlot <- ggplot(data=values, aes(x=threshold, y=metrics, color=cat)) +
  geom_line() +
  scale_color_discrete(name = 'Metric')+
  scale_x_continuous(breaks=seq(0, 1, 0.05), guide = guide_axis(angle = 45)) +
  scale_y_continuous(breaks=seq(0, 1, 0.05)) +
  xlab('Threshold') +
  ylab('') +
  theme_bw() +
  theme(text=element_text(size=20)) +
  geom_segment(aes(x = 0.05, xend = 0.05, y = 0, yend = 1), color='purple', linetype=2) +
  geom_segment(aes(x = 0.6, xend = 0.6, y = 0, yend = 1), color='purple', linetype=2) +
  geom_segment(aes(x = 0.8, xend = 0.8, y = 0, yend = 1), color='purple', linetype=2)

metricPlot
#dev.copy2pdf(file='21022024_metric_plot.pdf', height = 5, width = 10)


