# Towards a Fully Automated Underwater Census for Fish Assemblages in the Mediterranean Sea
## Demo
### Step by step - Transect by different Methods
1. Clone the repository
2. Open '.03_R_Scripts/analyzePresenceAbsence.R'
3. Install packages
4. Change the threshold file on line 37 if different than 0.8
5. Optional: If you want to save the graph in a PDF uncomment line 266
6. Run all the lines and enjoy the graphs
   
Result: [Transect Evaluation by Methods](https://github.com/PiSuMp/autoUC_MedSea/tree/main/04_Figures/transectData.png?raw=true)

### Step by step - AUC Thresholds Overall
1. Clone the repository
2. Open '.03_R_Scripts/aucThresholding_overall.R'
3. Install packages
4. Optional: If you want to save the graph in a PDF uncomment line 246
5. Run all the lines and enjoy the graphs

Result: [AUC Overall](https://github.com/PiSuMp/autoUC_MedSea/tree/main/04_Figures/aucOverall.png?raw=true)

### Step by step - AUC Thresholds per Species
1. Clone the repository
2. Open '.03_R_Scripts/aucThresholding_overall.R'
3. Install packages
4. Optional: If you want to save the graph in a PDF uncomment line 266
5. Run all the lines and enjoy the graph
   
Result: [AUC by Species](https://github.com/PiSuMp/autoUC_MedSea/tree/main/04_Figures/aucPerSpecies.png?raw=true)

## Workflow for custom data
0. Your label files need to have the following structure per file:
   class x_center y_center width height confidence
2. Copy all the label files into the folder ./03_Datasets/99_label_files
3. Run the python script './02_Python_Scripts/concatenate_labels.py'
   - This creates a file called './summaryVideos.csv' that contains all the labels information
4. Run the R script 'condenseDetections.R'
   Change the incrementStep for the confidence threshold at line 15
   - This creates multiple files in the folder './03_Datatsets/01_condensedCounts_Detections/' for each of the confidence threhsolds given in 'condenseDetections.R'
