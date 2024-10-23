library(tidyr)
library(ggplot2)
library(reshape2)
library(tibble)
library(dplyr)
library(cowplot)
#library(vegan)

#FUNCTIONS
{
#Function to convert counts to presence/absence
convert_to_presence_absence <- function(data) {
  presence_absence <- data
  presence_absence[, -1][presence_absence[, -1] > 0] <- 1
  return(presence_absence)
}

# Replace duplicates in the first column with NA
replace_duplicates_with_na <- function(data) {
  duplicates <- duplicated(data$file)
  data[duplicates, ] <- NA
  return(data)
}
}


#INITIALISATION / change directories here ect.
{
#Manipulation to get into the folder autoUC_MedSea
current_dir <- getwd()
parent_dir <- dirname(current_dir)
setwd(parent_dir)

#Get into the correct path
setwd("./03_Datasets/01_condensedCounts_Detections/")

detections <- read.csv("./condensedVideoCount0.8.csv", sep = " ") 

# Read the files
setwd("./03_Datasets/02_UVC_Data/")
groundtruth <- read.csv("UVC_Oct_2023_condensed.csv") 

#Get MetaData for later
metaData <- groundtruth[,1:11]

# Clean up the dataframes
detections <- subset(detections, select = -c(transNameCounts))
groundtruth <- groundtruth[,-c(1:10)]
names <- readLines("class_names.txt")

#Get rid of the class that only appear in a certain numnber of frames per video (unlikely to be a fish!)
minFishCount <- 4

for(i in 1:nrow(detections)){
  for(j in 1:ncol(detections)){
    if(detections[i,j] <= minFishCount){
      detections[i,j] = 0
    }
  }
}

#Drop the file names that are not in the detections
for(i in 1:nrow(detections)){
  if (detections$file[i] %in% groundtruth$file) {
    #print(paste0('Groundtruth available for ', detections$file[i]))
  } else{
    #print(paste0('Groundtruth NOT available for ', detections$file[i]))
    detections$file[i] <- NA
  }
}

#Delete the non existing groundtruth & detection files 
#This has to be done since we detected on more videos than we have data for
groundtruth <- drop_na(groundtruth)
detections <- drop_na(detections)

# Convert counts to presence/absence for both files
groundtruth_presence_absence <- convert_to_presence_absence(groundtruth)
detections_presence_absence <- convert_to_presence_absence(detections)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~
#PLOT per transect!
{
# Replace duplicates in the first column with NA
groundtruth_presence_absence <- replace_duplicates_with_na(groundtruth_presence_absence)

#Delete the non existing groundtruth files
groundtruth_presence_absence <- drop_na(groundtruth_presence_absence)

#Reorder them alphabetically
groundtruth_presence_absence <- groundtruth_presence_absence[order(groundtruth_presence_absence$file, decreasing = TRUE), ]

transGround <- as.data.frame(t(groundtruth_presence_absence))
names(transGround) <- transGround[1,]
transGround <- transGround[-1,]
species <- row.names(transGround)

transGround <- sapply(transGround, as.numeric)
transGround <- as.data.frame(transGround)

transGround <- cbind(species, transGround)
rownames(transGround) <- NULL

#DETECTIONS
transDetection <- as.data.frame(t(detections_presence_absence))

names(transDetection) <- transDetection[1,]
transDetection <- transDetection[-1,]
species <- row.names(transDetection)

transDetection <- sapply(transDetection, as.numeric)
transDetection <- as.data.frame(transDetection)


transDetection <- cbind(species, transDetection)
rownames(transDetection) <- NULL

#MANUAL VIDEO COUNTS
setwd("./03_Datasets/03_Manual_Video_Count/")
manualVideoCount <- read.csv("./manualVideoCount.csv")

#MANUAL COUNTS
#Drop the file names that are not in the detections
for(i in 1:nrow(groundtruth_presence_absence)){
  if (groundtruth_presence_absence$file[i] %in% manualVideoCount$file) {
    #print(paste0('Detections available for ', groundtruth_presence_absence$file[i]))
  } else{
    #print(paste0('Detections NOT available for ', groundtruth_presence_absence$file[i]))
    groundtruth_presence_absence$file[i] <- NA
  }
}

for(i in 1:nrow(detections_presence_absence)){
  if (detections_presence_absence$file[i] %in% manualVideoCount$file) {
    #print(paste0('Groundtruth available for ', detections_presence_absence$file[i]))
  } else{
    #print(paste0('Groundtruth NOT available for ', detections_presence_absence$file[i]))
    detections_presence_absence$file[i] <- NA
  }
}

for(i in 1:nrow(manualVideoCount)){
  if (manualVideoCount$file[i] %in% groundtruth_presence_absence$file) {
    #print(paste0('Groundtruth available for ', manualVideoCount$file[i]))
  } else{
    #print(paste0('Groundtruth NOT available for ', manualVideoCount$file[i]))
    manualVideoCount$file[i] <- NA
  }
}

#Delete the non existing groundtruth files
groundtruth_presence_absence <- drop_na(groundtruth_presence_absence)
detections_presence_absence <- drop_na(detections_presence_absence)

#Get Metadata
metaDataAdvanced <- metaData
metaDataAdvanced <- drop_na(metaDataAdvanced)

#This was used for representation to find rocky information per column
for(i in 1:nrow(metaDataAdvanced)){
  #metaDataAdvanced$category[i] <- paste0(metaDataAdvanced$location[i], "_", metaDataAdvanced$substrate[i], "_", metaDataAdvanced$depth[i], "_site", metaDataAdvanced$site[i])
  #metaDataAdvanced$category[i] <- paste0(metaDataAdvanced$zone[i], "_", metaDataAdvanced$substrate[i], "_", metaDataAdvanced$location[i], "_", metaDataAdvanced$depth[i], "_site", metaDataAdvanced$site[i], "_transect", metaDataAdvanced$transect[i])
  metaDataAdvanced$category[i] <- paste0(metaDataAdvanced$substrate[i], "_", metaDataAdvanced$location[i], "_", metaDataAdvanced$depth[i], "_site", metaDataAdvanced$site[i], "_transect", metaDataAdvanced$transect[i])
  #metaDataAdvanced$arbitTransect[i] <- paste0('Transect ', i)
}

metaDataAdvanced <- metaDataAdvanced[order(metaDataAdvanced$category, decreasing = FALSE),]

#This will be displayed on the x-axis
for(i in 1:nrow(metaDataAdvanced)){
  #metaDataAdvanced$category[i] <- paste0(metaDataAdvanced$location[i], "_", metaDataAdvanced$substrate[i], "_", metaDataAdvanced$depth[i], "_site", metaDataAdvanced$site[i])
  #metaDataAdvanced$category[i] <- paste0( metaDataAdvanced$zone[i], "_", metaDataAdvanced$location[i], "_", metaDataAdvanced$depth[i], "_site", metaDataAdvanced$site[i], "_transect", metaDataAdvanced$transect[i])
  metaDataAdvanced$arbitTransect[i] <- paste0('Transect ', i)
}

# Read the files and join all the Transects together with the data
manualMeta <- left_join(metaDataAdvanced, manualVideoCount, by = "file")
manualMeta <- drop_na(manualMeta)

detectionsMeta <- left_join(metaDataAdvanced, detections_presence_absence, by = "file")
detectionsMeta <- drop_na(detectionsMeta)

groundtruthMeta <- left_join(metaDataAdvanced, groundtruth_presence_absence, by = "file")
groundtruthMeta <- drop_na(groundtruthMeta)

detectionsMeta$site <- as.factor(detectionsMeta$site)
detectionsMeta$gopro <- as.factor(detectionsMeta$gopro)
detectionsMeta$transect <- as.factor(detectionsMeta$transect)

manualMeta$site <- as.factor(manualMeta$site)
manualMeta$gopro <- as.factor(manualMeta$gopro)
manualMeta$transect <- as.factor(manualMeta$transect)

groundtruthMeta$site <- as.factor(groundtruthMeta$site)
groundtruthMeta$gopro <- as.factor(groundtruthMeta$gopro)
groundtruthMeta$transect <- as.factor(groundtruthMeta$transect)

#Setting up Groundtruth
groundtruthMetaMelted <- melt(groundtruthMeta)

for(i in 1:nrow(groundtruthMetaMelted)){
  if(groundtruthMetaMelted$value[i] == 0){
    groundtruthMetaMelted$value[i] = NA
  }
}

groundtruthMetaMelted <- drop_na(groundtruthMetaMelted)

#Re-Order to make it look nice MANUAL DATA
namesOrd <- sort(names, decreasing = TRUE)

namesOrd <- namesOrd[ !namesOrd == 'Other']

base <- manualMeta[,1:13]

#Do the other class to be first/last
Other <- manualMeta['Other']
base <- cbind(base, Other)

for(i in 1:length(namesOrd)){
  currentSpecies <- namesOrd[i]
  base <- cbind(base, manualMeta[currentSpecies])
}

manualMetaMelted <- melt(base)

#Re-Order to make it look nice DETECTION DATA
base <- detectionsMeta[,1:13]

#Do the other class to be first/last
Other <- detectionsMeta['Other']
base <- cbind(base, Other)

for(i in 1:length(namesOrd)){
  currentSpecies <- namesOrd[i]
  base <- cbind(base, detectionsMeta[currentSpecies])
}

detectionsMetaMelted <- melt(base)

transectNames <- c()

for(i in 1:nrow(manualMeta)){
  transectNames[i] <- as.character(i)
}

#Plot Species against Transects
print(ggplot() +
  geom_tile(data = manualMetaMelted, aes(category, variable, alpha = value*0.8), fill = "red") + #Manual Video Counts
  geom_tile(data = detectionsMetaMelted, aes(category, variable, alpha = value*0.4), fill = "yellow") + #Detections
  geom_text(data = groundtruthMetaMelted, aes(category, variable, label = "X"),  size=7) + #Diver Data marked by X
  coord_equal(expand = 0) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  #labs(x = "[Location]_[Substrate]_[depth]_[Site]_[Transect]", y = "Species") +
  guides(alpha = FALSE) +
  scale_alpha_continuous(range = c(0, 1)) + #To make the absent species non coloured! +
  theme(text=element_text(size=20), 
        axis.title.y=element_blank() ) + 
  scale_x_discrete(labels=transectNames) +
  labs(x = 'Transects'))

#Save the plot if needed
#dev.copy2pdf(file='21022024_transect_evaluation_8.pdf', height = 8, width = 20)


}


#PLOT per site without the transect!
{
#Get Metadata
metaDataAdvanced <- metaData
for(i in 1:nrow(metaDataAdvanced)){
  metaDataAdvanced$category[i] <- paste0(metaDataAdvanced$zone[i], "_", metaDataAdvanced$location[i], "_", metaDataAdvanced$substrate[i], "_", metaDataAdvanced$depth[i], "_site", metaDataAdvanced$site[i])
  #metaDataAdvanced$category[i] <- paste0(metaDataAdvanced$location[i], "_", metaDataAdvanced$substrate[i], "_", metaDataAdvanced$depth[i], "_site", metaDataAdvanced$site[i], "_transect", metaDataAdvanced$transect[i])
}

# Read the files
manualMeta <- left_join(metaDataAdvanced, manualVideoCount, by = "file")
manualMeta <- drop_na(manualMeta)

detectionsMeta <- left_join(metaDataAdvanced, detections_presence_absence, by = "file")
detectionsMeta <- drop_na(detectionsMeta)

groundtruthMeta <- left_join(metaDataAdvanced, groundtruth_presence_absence, by = "file")
groundtruthMeta <- drop_na(groundtruthMeta)

detectionsMeta$site <- as.factor(detectionsMeta$site)
detectionsMeta$gopro <- as.factor(detectionsMeta$gopro)
detectionsMeta$transect <- as.factor(detectionsMeta$transect)

manualMeta$site <- as.factor(manualMeta$site)
manualMeta$gopro <- as.factor(manualMeta$gopro)
manualMeta$transect <- as.factor(manualMeta$transect)

groundtruthMeta$site <- as.factor(groundtruthMeta$site)
groundtruthMeta$gopro <- as.factor(groundtruthMeta$gopro)
groundtruthMeta$transect <- as.factor(groundtruthMeta$transect)

#Setting up Groundtruth
groundtruthMetaMelted <- melt(groundtruthMeta)

for(i in 1:nrow(groundtruthMetaMelted)){
  if(groundtruthMetaMelted$value[i] == 0){
    groundtruthMetaMelted$value[i] = NA
  }
}

groundtruthMetaMelted <- drop_na(groundtruthMetaMelted)

manualMetaMelted <- melt(manualMeta)
detectionsMetaMelted <- melt(detectionsMeta)

detectionsMetaMelted <- subset(detectionsMetaMelted, select = -c(transect))
manualMetaMelted <- subset(manualMetaMelted, select = -c(transect))

detectionsMetaMelted$catSpecies <- paste0(detectionsMetaMelted$category, '_', detectionsMetaMelted$variable, '_', detectionsMetaMelted$value)
uniqueCount <- duplicated(detectionsMetaMelted$catSpecies)

for(i in 1:nrow(detectionsMetaMelted)){
  if(uniqueCount[i] == TRUE){
    detectionsMetaMelted$value[i] = NA
  }
}

detectionsMetaMelted <- drop_na(detectionsMetaMelted)


manualMetaMelted$catSpecies <- paste0(manualMetaMelted$category, '_', manualMetaMelted$variable, '_', manualMetaMelted$value)
uniqueCount <- duplicated(manualMetaMelted$catSpecies)

for(i in 1:nrow(manualMetaMelted)){
  if(uniqueCount[i] == TRUE){
    manualMetaMelted$value[i] = NA
  }
}

manualMetaMelted <- drop_na(manualMetaMelted)

#Plot Species per site instead of transects/videos
ggplot() +
  geom_tile(data = manualMetaMelted, aes(category, variable, alpha = value*0.8), fill = "red") + #Manual Video Counts
  geom_tile(data = detectionsMetaMelted, aes(category, variable, alpha = value*0.4), fill = "yellow") + #Detections
  geom_text(data = groundtruthMetaMelted, aes(x= category, y = variable, label = "X"),  size=7) + #Diver Data marked by X
  coord_equal(expand = 0) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "[Location]_[Substrate]_[depth]_[Site]",
       y = "Species") +
  guides(alpha = FALSE) +
  scale_alpha_continuous(range = c(0, 1)) #To make the absent species non coloured!

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~

#For Metrics 
{
  #~~~~~~~~~~~~~~~~~~~~~~~~~~Get quantitative numbers / ACCURACY
  #Reload Groundtruth
  setwd("./03_Datasets/02_UVC_Data/")
  groundtruth <- read.csv("./UVC_Oct_2023_condensed.csv")
  #Load names
  names <- readLines("./class_names.txt")
  #Create dataframe
  colNames <- colnames(groundtruth)
  colNames <- colNames[1:11]
  
  #Adjust the rows
  for(i in 1:nrow(detections_presence_absence)){
    if (detections_presence_absence$file[i] %in% manualVideoCount$file) {
      #print(paste0('Groundtruth available for ', detections_presence_absence$file[i]))
    } else{
      #print(paste0('Groundtruth NOT available for ', detections_presence_absence$file[i]))
      detections_presence_absence$file[i] <- NA
    }
  }
  
  # for(i in 1:nrow(manualVideoCount)){
  #   if (manualVideoCount$file[i] %in% detections_presence_absence$file) {
  #     #print(paste0('Groundtruth available for ', manualVideoCount$file[i]))
  #   } else{
  #     #print(paste0('Groundtruth NOT available for ', manualVideoCount$file[i]))
  #     manualVideoCount$file[i] <- NA
  #   }
  # }
  
  isEmptyNumeric <- function(x) {
    return(identical(x, numeric(0)))
  }
  
  #Delete the non existing groundtruth files
  groundtruth_presence_absence <- drop_na(groundtruth_presence_absence)
  detections_presence_absence <- drop_na(detections_presence_absence)
  manualVideoCount <- drop_na(manualVideoCount)
  
  finalAccuracy <- as.data.frame(matrix(1:(nrow(groundtruth_presence_absence)*2), nrow = nrow(groundtruth_presence_absence), ncol = 2))
  colnames(finalAccuracy) <- c('file', 'accuracy')
  
  for(i in 1:nrow(manualVideoCount)){
    truePositive <- 0
    falsePositive <- 0
    trueNegative <- 0
    falseNegative <- 0
    
    currentVideo <- manualVideoCount$file[i]
    
    currentCount <- subset(detections_presence_absence, file == currentVideo)
    
    for(j in 1:length(names)){
      currentDetection <- subset(detections_presence_absence, file == currentVideo)
      currentManualCount <- subset(manualVideoCount, file == currentVideo)
      
      currentSpecies <- names[j]
      
      if(isEmptyNumeric(currentDetection[[currentSpecies]]) == TRUE & currentManualCount[[currentSpecies]] == 1){
        falseNegative = falseNegative + 1
      }else if(isEmptyNumeric(currentDetection[[currentSpecies]]) == TRUE & currentManualCount[[currentSpecies]] == 0){
        trueNegative = trueNegative + 1
      }else if(currentDetection[[currentSpecies]] == 1 & currentManualCount[[currentSpecies]] == 1){
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
    finalAccuracy$detectionAccuracy[i] <- (truePositive) / (truePositive + falsePositive)
    
    UB <- finalAccuracy$accuracy[i] - ((truePositive + falsePositive)*(truePositive + falseNegative) + (falseNegative + trueNegative)*(trueNegative + falsePositive)) / (truePositive + trueNegative + falsePositive + falseNegative)**2
    LB <- 1 - ((truePositive + falsePositive)*(truePositive + falseNegative) + (falseNegative + trueNegative)*(trueNegative + falsePositive)) / (truePositive + trueNegative + falsePositive + falseNegative)**2
    finalAccuracy$KappaStat[i] <- UB / LB
    
    finalAccuracy$TSS[i] <- finalAccuracy$sensitivity[i] + finalAccuracy$specificity[i] - 1
    
  }
  
  finalAccuracy <- left_join(finalAccuracy, groundtruth, by = 'file')[,1:12]
  
  #finalAccuracy <- drop_na(finalAccuracy)
  
  overallAcc <- mean(finalAccuracy$accuracy)
  overallSensitivity <- mean(finalAccuracy$sensitivity)
  overallSpecificity <- mean(finalAccuracy$specificity)
  overallKappaStat <- mean(finalAccuracy$KappaStat)
  overallTSS <- mean(finalAccuracy$TSS)
  
  overallALL <- overallAcc + overallSensitivity + overallSpecificity + overallKappaStat + overallTSS
  
  overallDetectionAccuracy <- mean(finalAccuracy$detectionAccuracy, na.rm = TRUE)
  sumFP <- sum(finalAccuracy$FP)
  sumFN <- sum(finalAccuracy$FN)
  sumTP <- sum(finalAccuracy$TP)
  sumTN <- sum(finalAccuracy$TN)
  
  #Accuracy
  print(paste0('Accuracy: ', round(overallAcc,2), ' Sensitivity: ', round(overallSensitivity,2), ' Specificity: ', round(overallSpecificity,2), ' Kappa Statistic: ', round(overallKappaStat,2), ' TSS: ', round(overallTSS,2), ' ALL: ', round(overallALL,2), '/5'))
}


#For Biol Metrics / does not work with Linux R since package R does not exist
{
detectionsMeta$SpeciesRichness <- specnumber(detectionsMeta[,12:32])

manualMeta$SpeciesRichness <- specnumber(manualMeta[,12:32])

manualMeta$method <- 'm'
detectionsMeta$method <- 'd'

manDetMeta <- rbind(manualMeta, detectionsMeta)

ggplot(data=manDetMeta, aes(x = substrate, y = SpeciesRichness, fill = method)) +
  geom_boxplot() +
  labs(y= "Species Richness", x = "Substrate", fill = "Evaluation Method") +
  scale_fill_discrete(labels=c('Detection', 'Manual'))
}

#End




