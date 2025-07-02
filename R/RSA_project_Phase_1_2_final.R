### RSA project - Similarity task (phase 2) ####  
###   Alejandro Gamba - 2023 - PAC LAB      ####

############################################################################
# 0. Initial steps: Working Directory, Load data, Parameters ###############
############################################################################

## Description of the data: ##################################################################################### 
## Around 200 participants in each language (spanish,english and Japanese) provided their Similarity ratings    #
## for 8 different types of questions over 40 different categories of either animals (30) or objects (10).      #
## The data was collected online and 280 images were used based on a previous prototypical behavioral task where# 
## they chose the most representative exemplars.                                                                #  
#################################################################################################################

## Set working directory ##
setwd("C:/Users/j.gambasegovia/Documents/Papeles Alejo UF/RSA - R.Project") ## replace the location of the data file here

# Import data  - unique file #
#Simdata <- read.csv("phase-1-prototypicality-task-rsa-project-a-english_79.csv")

# Import and join data set from different files #
temp = list.files(pattern="*.csv")
myfiles = lapply(temp, read.csv)
Simdata <- do.call(rbind,myfiles)

# View the data structure. 
head(Simdata)
names(Simdata)

#############################---------------------------------------------################################ 
############################# FUNCTIONS TO BE USED DURING PRE-PROCESSING  ################################ 
#############################---------------------------------------------################################

###### **Prerequisite function** for step 15 that filters the dataframe for each blocktype category ###### 

generate_filtered_dataframes_with_names <- function(data, data_name) {
  unique_blocktypes <- unique(data$BlockType)
  filtered_dataframes <- list()
  
  # Add the entire original dataframe to the list
  filtered_dataframes[["original"]] <- data
  cat(paste("Generated dataframe:", paste0(data_name, "_original"), "\n"))
  
  for (blocktype in unique_blocktypes) {
    # Create a new dataframe excluding the current blocktype
    df_filtered <- subset(data, BlockType != blocktype)
    # Add a new column indicating which blocktype was removed
    df_filtered$Removed_BlockType <- blocktype
    # Add a new column indicating from which dataset this dataframe originated
    df_filtered$Dataset <- data_name
    
    # Append the filtered dataframe to the list
    filtered_dataframes[[blocktype]] <- df_filtered
    
    # Print the name of the generated dataframe
    formatted_blocktype <- gsub(" ", "_", tolower(blocktype))
    cat(paste("Generated dataframe:", paste0(data_name, "_no", formatted_blocktype), "\n"))
  }
  
  return(filtered_dataframes)
}

#############################---------------------------------------------################################ 
#############################     CODE FOR PRE-PROCESSING OF THE DATA     ################################ 
#############################---------------------------------------------################################

### --------------------------------------------------------------------------------------------------- ###
### ----------------- SECTION 0: 9 Dataframes per language without one category.   -------------------- ###
### --------------------------------------------------------------------------------------------------- ### 

##### Step 0: Organizing and Filtering the data #####

### 1) Filter By ID in Prolific and Date. ##
library (tidyr) #For drop_na among others  
library (dplyr)  

# Insert the NA in subject_id #
Simdata['subject_id'][Simdata['subject_id'] == ''] <- NA

# Omit the rows with NA #
Simdata2 <- Simdata %>% drop_na(subject_id)

### 2) Transform date format and take off the previous 31/01/23 (did not taken anyone because all were removed in step 1) ##
Simdata2$recorded_at <- as.Date(Simdata2$recorded_at, "%Y-%m-%d")
Simdata2 <- Simdata %>% filter(recorded_at >= "2023-04-23")

### 3) Selecting the relevant output for further organization and cleaning #
Simdata2 <- Simdata[c(6,7,10,24,25,28,29,32,34,35,36,38,39)]

# Resize the data with the interested variables (for individual files). 
# Simdata_espfilt2 <- Simdata[c(10:329),c(6,7,8,25,26,29)]

### 4) Generate the blocks variable and fill it according to the type of trials.

# extract category from text column 
library(stringr)
# English part #
Simdata2$BlockType <- str_extract(Simdata2$stimulus, "<b>similarity</b>|color|shape|animacy|scare|dissimilar|category|preference|Primer|color|forma|animación|asustan|disimilaridad|categorías|prefieres")

## Fill the values with the ones from the block. 
Simdata2 <- Simdata2 %>% fill(BlockType)

## Extract category only for attention checks 
Simdata2$BlockType <- ifelse(grepl('<h3>',Simdata2$stimulus),'attention', Simdata2$BlockType)

## Replace block 1 for general similarity
Simdata2$BlockType[Simdata2$BlockType == '<b>similarity</b>'|Simdata2$BlockType == 'Primer'] <- 'General Similarity'
    
### 4.2) Remove the stimulus that are empty.  
## Insert the NA in subject_id 
Simdata2['imagesPairs'][Simdata2['imagesPairs'] == ''] <- NA
## Drop the rows with NA stimulus.
Simdata3 <- Simdata2 %>% drop_na(imagesPairs)

### 5) Add 2 new columns for only numbers of images without extension##
Simdata3$LeftImg <- sub("([0-9]+)_([0-9])+_([0-9]+).png", "\\1_\\2_\\3", Simdata3$leftImage)
Simdata3$RightImg <- sub("([0-9]+)_([0-9])+_([0-9]+).png", "\\1_\\2_\\3", Simdata3$rightImage)


## 5.00 Checking step: Drop the rows with NA response (did not change it - so all good).
library(tidyr)
Simdata4 <- Simdata3 %>% drop_na(response)

## check the amount of trials per Run_id. 
table(Simdata4$study_id)

### 6) Convert the columns into numeric values #
  cols.num <- c('trial_index','response','run_id')   
  Simdata4[cols.num] <- sapply(Simdata4[cols.num],as.numeric)
  
  # Verify the class of each column #
  sapply(Simdata4,class)
     

### 7) Getting sex assigned more direct. ## 
  Simdata4$sexfromStu <- NA 
  Simdata4$sexfromStu[Simdata4$study_id =='6445ce098124356006df4371'] <- 'Male'     # English
  Simdata4$sexfromStu[Simdata4$study_id =='6470be16877bfb0f486b3c8b'] <- 'Male'     # English - 2
  Simdata4$sexfromStu[Simdata4$study_id =='64624ff3314096399dbc0a7b'] <- 'Male'     # Spanish
  Simdata4$sexfromStu[Simdata4$study_id =='6445ce098124356006df4372'] <- 'Female'   # English
  Simdata4$sexfromStu[Simdata4$study_id =='6470be16877bfb0f486b3c8c'] <- 'Female'     # English - 2
  Simdata4$sexfromStu[Simdata4$study_id =='64624ff3314096399dbc0a7c'] <- 'Female'   # Spanish
  # Special case - No study_id
  Simdata4$sexfromStu[Simdata4$run_id =='6'] <- 'Female'   # Spanish
  
  # Confirmation # 
  table(Simdata4$sexfromStu)
  
  
### 8) Unifying the names of the study.  ##
  Simdata4$study_id[Simdata4$study_id =='6445ce098124356006df4371'] <- 'English 2 images'
  Simdata4$study_id[Simdata4$study_id =='6445ce098124356006df4372'] <- 'English 2 images'
  Simdata4$study_id[Simdata4$study_id =='6470be16877bfb0f486b3c8b'] <- 'English 2 images'
  Simdata4$study_id[Simdata4$study_id =='6470be16877bfb0f486b3c8c'] <- 'English 2 images'
  Simdata4$study_id[Simdata4$study_id =='64624ff3314096399dbc0a7b'] <- 'Spanish 2 images'
  Simdata4$study_id[Simdata4$study_id =='64624ff3314096399dbc0a7c'] <- 'Spanish 2 images'
  # Special case - No study_id
  Simdata4$study_id[Simdata4$run_id =='6'] <- 'Spanish 2 images'
  
  
### 9) rename the participants: 2 or 4k for 2 images version; 3 or 6 for 3 image version easier manipulation and identification ## 
  ## WORKING FOR 2K ONLY, WAITING FOR THE REST OF THE 4 TASKS ##
   Simdata5 <- Simdata4 %>%
    mutate(run_id = if_else(study_id == "English 2 images", run_id + 2000,
                                  if_else(study_id == "Spanish 2 images", run_id + 4000, run_id)))
                                        #if_else(Study_id == "English 3 images", run_id + 3000,
                                             #if_else(Study_id == "Spanish 3 images", run_id + 6000)))))
   
   # Confirmation # 
   table(Simdata5$run_id)
   # To Know if there are any above the 360 count
   table(Simdata5$run_id[Simdata5$trial_index > 360])
   
   
### 10) Rename the images that start with only 1 digit in both columns
   # Define a function to add leading zeros to the label
   f_add_leading_zero <- function(label) {
     components <- strsplit(label, "_")[[1]]
     if (nchar(components[1]) == 1) {
       components[1] <- paste0("0", components[1])
     }
     return(paste(components, collapse = "_"))
   }
   
   # Apply the function to the labels column
   Simdata5$LeftImg <- sapply(Simdata5$LeftImg, f_add_leading_zero)
   Simdata5$RightImg <- sapply(Simdata5$RightImg, f_add_leading_zero)
   
   #View(Simdata5)
   
## SEPARATION OF DATA FOR ANALYSIS PURPOSES: Done before exclu criteria to see between languages data ##        
  
### 11) Separate the dataframe into 2 for each language (Spanish, English) OR for versions (2 and 3 images). 
    ## Spanish ##
    Simdataesp <- Simdata5 %>%  filter(study_id == 'Spanish 2 images'| study_id == 'Spanish 3 images')   
      # Confirmation #
      table(Simdataesp$study_id)
    
      Simdataesp %>% count(run_id)
      
    ## English ##
    Simdataeng <- Simdata5 %>%  filter(study_id == 'English 2 images'| study_id == 'English 3 images')   
      # Confirmation #
      table(Simdataeng$study_id)
    
    Simdataeng %>% count(run_id)
    
    ## 2 images ##
    Simdata2img <- Simdata5 %>%  filter(study_id == 'English 2 images'| study_id == 'Spanish 2 images')   
    # Confirmation #
    #table(Simdata2img$study_id)
    
    ## 3 images ##
    Simdata3img <- Simdata5 %>%  filter(study_id == 'English 3 images'| study_id == 'Spanish 3 images')   
    # Confirmation #
    #table(Simdata2img$study_id)
    
    ## TOTAL ##
    SimdataTot <- Simdata5 %>%  filter(study_id == 'English 2 images'| study_id == 'Spanish 2 images'|study_id =='English 3 images'| study_id == 'Spanish 3 images')   
    # Confirmation #
    #table(SimdataTot$study_id)

    
### 12: - EXCLUSION CRIT 1: Attention checks failed 
    
##12.1 English Version (First do it manually and then automatic to validate) 
## Prolific IDs from the attention code checks.
  
## English Codes from Prolific ##
Failed_eng <- c('5fb219d125498577f2aa3a6d','5e2cd786dc63ca2ff798a0ca','62a9513ae9305607fd5af01a',
             '63e69728da28e991114aad46', '63f1b6b09dd844d7802e1682', '637ea165e071484955b325f7',
             '599de70d8a6cc5000128f980','5c928a3abf2af20001363f48','63756a809455a7613a718c6f',
             '6411e53b18c91ccb3f63d6b0','63c3584c5e3d88814d1005ab','5d85910572aeb00001290de9',
             '59a88f5b321f870001d16d68')
Failed_esp <- c('6143a74ee2a59fe9ef24de02','5bc0ffa6b548c500012783cc','610afdfedb8325ac8a155d00',
                '613a83dc733097c0054602df','6148fa16224d9a3182bb1871', '5fcc576f70ce74033ee4675b',
                '5f66e131cb8b062d0ea776f0', '5aea8c59ae889e00018624c2', '613a3d980fd9796d4b302d08',
                '6148bf90e4a4620ca010c833','5c1d19c810677f0001d9d56c','6043f909d14273455e5e7436',
                '613ac677749969b288c4a7b1','6151cfe3ba49dd3ad62708ee','5fb8706de225255a48011f08',
                '5d4e592bab253e00161bc8ef','5eb06d490b554111d40b842f') 
  
Failed <- c('5fb219d125498577f2aa3a6d','5e2cd786dc63ca2ff798a0ca','62a9513ae9305607fd5af01a','63e69728da28e991114aad46', 
            '63f1b6b09dd844d7802e1682','637ea165e071484955b325f7','599de70d8a6cc5000128f980','5c928a3abf2af20001363f48',
            '63756a809455a7613a718c6f','6411e53b18c91ccb3f63d6b0','63c3584c5e3d88814d1005ab','5d85910572aeb00001290de9',
            '59a88f5b321f870001d16d68','6143a74ee2a59fe9ef24de02','5bc0ffa6b548c500012783cc','610afdfedb8325ac8a155d00',
            '613a83dc733097c0054602df','6148fa16224d9a3182bb1871','5fcc576f70ce74033ee4675b','5f66e131cb8b062d0ea776f0', 
            '5aea8c59ae889e00018624c2','613a3d980fd9796d4b302d08','6148bf90e4a4620ca010c833','5c1d19c810677f0001d9d56c',
            '6043f909d14273455e5e7436','613ac677749969b288c4a7b1','6151cfe3ba49dd3ad62708ee','5fb8706de225255a48011f08',
            '5d4e592bab253e00161bc8ef','5eb06d490b554111d40b842f')
  
NoCode <- c('5e520fe5fe2087237b1b8cd1','56e72067c89073000be77fdf','59d52d7821977e0001d63082',
            '6412f18d99aa2c8cf064ac87','6440883aa6e850dda1c0cf5c','622a2b1c561ce3cd95519fd2',
            '5f3810d3ef03d32e90b5d813','613c11ce8696b84afc0ef290') # Last 2 from spanish#
  
  
# Full exclusion values list 
exclu_list <- list(NoCode,Failed) 
  
## Excluding them from the data set. // Try also to analyze the data without excluding them to see evaluate differences. ## 
## 2 Images ## 
Simdata2img2 <- subset(Simdata2img, !(subject_id %in% Failed))
Simdata2img2.2 <- subset(Simdata2img2, !(subject_id %in% NoCode))
  
#Counting subjects remaining
Simdata2img2 %>% count(run_id) 
table(Simdata2img2$run_id)
    
    ### I am missing here the matches vector that I accidentally errase.... to verify that there are not ID subjects from the exclusion list. 
    ## Print out matches
    #if (length(matches) > 0) {
    #  print(paste("The following values are present in the column:", paste(unique(matches), collapse = ", ")))
    #} else {
    #  print("No values are present in the column.")
    #}  
  
## Only English ##
Simdataeng2 <- subset(Simdataeng, !(subject_id %in% Failed_eng))
Simdataeng2 <- subset(Simdataeng2, !(subject_id %in% NoCode))
  
#Counting subjects remaining
Simdataeng2 %>% count(run_id)
#table(Simdataeng2$run_id)
  
## Only Spanish ##
Simdataesp2 <- subset(Simdataesp, !(subject_id %in% Failed_esp))
Simdataesp2 <- subset(Simdataesp2, !(subject_id %in% NoCode))
    
#Counting subjects remaining
Simdataesp2 %>% count(run_id)
#Simdataesp %>% count(run_id)
#table(Simdataesp2$run_id)
  
### 13: - EXCLUSION CRIT 2: Excluding trials that has a RTs <1000 and >20000, >50,000, >852,000.      
  
### 2 images ###  
## Measures of central tendency ##
library (psych)
  psych::describe(Simdata2img2.2$rt)

# Excluding trials with rt <=20k (20 seconds) and >= 1000ms (1s)
  Simdata2img3_no_outlier <- subset(Simdata2img2.2, rt <= 20000 & rt >= 1000)
  psych::describe(Simdata2img3_no_outlier$rt)
  
# Graphical idea of the data #
  histresp2img <- hist(Simdata2img3_no_outlier$response, breaks = 50, density = 100,
                 col = "springgreen4", xlab = "Similarity scores (a.u.)", main = "Histogram of similarity scores")
                 #xlim = c(0,5)) 

### Only English ###  
## Measures of central tendency ##
  psych::describe(Simdataeng2$rt)
  
  # Excluding trials with rt <=20k (20 seconds) and >= 1000ms (1s)
  Simdataeng2_no_outlier <- subset(Simdataeng2, rt <= 20000 & rt >= 1000)
  psych::describe(Simdataeng2_no_outlier$rt)
  
  # Graphical idea of the data #
  histrespeng <- hist(Simdataeng2_no_outlier$response, breaks = 50, density = 100,
                   col = "springgreen4", xlab = "Similarity scores (a.u.)", main = "Histogram of similarity scores")
  #xlim = c(0,5)) 
  
### Only Spanish ###  
## Measures of central tendency ##
  psych::describe(Simdataesp2$rt)
  
  # Excluding trials with rt <=20k (20 seconds) and >= 1000ms (1s)
  Simdataesp2_no_outlier <- subset(Simdataesp2, rt <= 20000 & rt >= 1000)
  psych::describe(Simdataesp2_no_outlier$rt)
  
  # Graphical idea of the data #
  histrespesp <- hist(Simdataesp2_no_outlier$response, breaks = 50, density = 100,
                      col = "springgreen4", xlab = "Similarity scores (a.u.)", main = "Histogram of similarity scores")
  #xlim = c(0,5)) 
  
  # Outliers information (852 obs) 
  #Simdata2img3_no_outlier <- subset(Simdata2img2.2, rt >= 20000 | rt <= 100)
  
  #Define extremes for original values# ## did not use this values due to extreme skeweness of data - mean not reliable ##
  #upper_extreme<-mean(Simdataeng2$rt)+3*sd(Simdataeng2$rt)
  #lower_extreme<-mean(Simdataeng2$rt)-3*sd(Simdataeng2$rt)
  
  # Histograms of Reaction times 2 images - it is visible only if taking out >10k(best) or >20k#
  
  histRT <- hist(Simdata2img3_no_outlier$rt, breaks = 30, density = 100,
                 col = "red", xlab = "Reaction times (ms)", main = "Histogram of Reaction Times") 
  xfit1 <- seq(min(Simdata2img3_no_outlier$rt), max(Simdata2img3_no_outlier$rt), length = 40) 
  yfit1 <- dnorm(xfit1, mean = mean(Simdata2img3_no_outlier$rt), sd = sd(Simdata2img3_no_outlier$rt)) 
  yfit1 <- yfit1 * diff(histRT$mids[1:2]) * length(Simdata2img3_no_outlier$rt) 
  lines(xfit1, yfit1, col = "black", lwd = 2)
  
### 14 - EXCLUSION CRIT 3:  Remove the people that did not complete 50% of the experiment (just to evaluate reliability of the 
###       measures based on reducing the individual variability).  
    
## 14.1 Group by the subject column and calculate the number of trials for each subject
    
# 2 images #
subject_studytype_trial_counts2img <- Simdata2img3_no_outlier %>%
  group_by(run_id,study_id) %>%
  summarize(trial_count = n())
  
# Spanish #
subject_studytype_trial_countsesp <- Simdataesp2_no_outlier %>% 
  group_by(run_id,study_id) %>% 
  summarize(trial_count = n())
    
# English #
  subject_studytype_trial_countseng <- Simdataeng2_no_outlier %>%
    group_by(run_id,study_id) %>%
    summarize(trial_count = n())
    
## 14.2 Filter the database to only keep subjects with more than 180 trials 
    
# 2 images #
Simdata_2imgfilt <- Simdata2img3_no_outlier %>% 
  left_join(subject_studytype_trial_counts2img, by = c("study_id","run_id")) %>% 
  filter(trial_count > 180)
  
# Confirmation #    
table(Simdata_2imgfilt['run_id'])
  
# count final subjects #
Simdata2img3.2 <- Simdata_2imgfilt %>%
  group_by(run_id,study_id) %>%
  summarize(trial_count = n())
  
### Spanish ###  
Simdata_espfilt <- Simdataesp2_no_outlier %>% 
  left_join(subject_studytype_trial_countsesp, by = c("study_id","run_id")) %>% 
  filter(trial_count > 180)

# Confirmation #    
table(Simdata_espfilt['run_id'])
  
# count final subjects #
Simdataesp3.2 <- Simdata_espfilt %>%
  group_by(run_id,study_id) %>%
  summarize(trial_count = n())
    
# total number of trials #
total_trials_esp <- sum(Simdataesp3.2$trial_count)
    
### English ###
Simdata_engfilt <- Simdataeng2_no_outlier %>% 
  left_join(subject_studytype_trial_countseng, by = c("study_id","run_id")) %>% 
  filter(trial_count > 180)
      
# Confirmation #    
table(Simdata_engfilt['run_id'])
      
# count final subjects #
Simdataeng3.2 <- Simdata_engfilt %>%
  group_by(run_id,study_id) %>%
  summarize(trial_count = n())
      
# total number of trials #   
  total_trials_eng <- sum(Simdataeng3.2$trial_count)

### 15): Exclusion of one of the categories in the dataframes. ###
eng_filtered_dfs_names <- generate_filtered_dataframes_with_names(Simdata_engfilt, 'Simdata_engfilt')
spa_filtered_dfs_names <- generate_filtered_dataframes_with_names(Simdata_espfilt, 'Simdata_espfilt')
 
#############################################################################################################
################################## ANALYSIS FOR BEHAVIORAL RSM  #############################################
#############################################################################################################

  ##### Steps for Behavioral RSA (think on this when I reach taht point) #####
  # - Pre-processing to have column of images left, column of images right, ##
  # and column of similarity scores between them.                           ##  
  # - Apply a Z-score on the similarity scores                              ##  
  # - Generate the matrix between the images, filling the values, fill the  ##
  # diagonal for each image with itself (1 or 0), and then make the RSM to  ##
  # RDM (1-correlation)                                                     ##  
  ############################################################################

library(Matrix) # for create_similarity_matrix  function 
library(tidyr)  # for complete_matrix function
library(dplyr)  # for complete_matrix function
library(tibble) # for complete_matrix function

#############################---------------------------------------------################################ 
#############################    FUNCTIONS TO BE USED DURING ANALYSIS     ################################ 
#############################---------------------------------------------################################

###### **Prerequisite function** for step 1 that creates the similarity matrix   ######
########################################################################################

## Function for  step 1 ##
## f_create_similarity_matrix ##
f_create_similarity_matrix <- function(data, left_col, right_col, matrix_name) {
  # Make the objects unique to make the matrix
  ImageDeck <- unique(c(data[[left_col]], data[[right_col]]))
  
  # Create the matrix with the specified dimensions
  mat <- matrix(NA, nrow = length(ImageDeck), ncol = length(ImageDeck), 
                dimnames = list(ImageDeck, ImageDeck))
  
  # Re-order each of the axis - restructure the matrix
  row_order <- order(rownames(mat))
  col_order <- order(colnames(mat))
  mat_reord <- mat[row_order, col_order]
  
  # Fill the matrix with the similarity scores and mean if more than 1 value
  for (i in 1:nrow(data)) {
    LeftImg <- data[i, left_col]
    RightImg <- data[i, right_col]
    response <- data[i, "response"]
    if (is.na(mat_reord[LeftImg, RightImg])) {
      mat_reord[LeftImg, RightImg] <- response
    } else {
      mat_reord[LeftImg, RightImg] <- mean(c(mat_reord[LeftImg, RightImg], response))
    }
  }
  
  # Count the frequency of datapoints for each cell
  freq_mat <- matrix(0, nrow = nrow(mat_reord), ncol = ncol(mat_reord), dimnames = dimnames(mat_reord))
  for (i in 1:nrow(mat_reord)) {
    for (j in 1:ncol(mat_reord)) {
      freq_mat[i, j] <- sum(!is.na(mat_reord[i, j]))
    }
  }
  
  # Checking for symmetry on the matrix #
  isSymmetric(mat_reord)
  
  # Making symmetric the matrix
  mat_copy <- mat_reord
  mat_t <- t(mat_reord)
  mat_fin <- f_add_matrices(mat_copy,mat_t)
  dimnames(mat_fin) <- dimnames(mat_reord)
  
  # Total NA present in the matrix
  total_NA <- sum(is.na(mat_fin))
  # Proportion of NAs in total cells in matrix
  proportion_NA <- sum(is.na(mat_fin))/ length(mat_fin)
  
  # Adjusted print statements to include matrix_name
  print(paste("For matrix", matrix_name, "- The number of NAs is", total_NA))
  print(paste("For matrix", matrix_name, "- The percentage of NAs is", proportion_NA * 100, '%'))
  
  return(list(similarity_matrix = mat_fin, frequency_matrix = freq_mat))
}

###### **Prerequisite function** for to add matrices following 4 conditions given the NAs values   ######
######   and include condition if diff dimensions. Function used before when diff dimensions.      ###### 

## f_add_matrices_ext ##
f_add_matrices_ext <- function(mat1, mat2) {
  # get the dimensions of the matrices
  dim1 <- dim(mat1)
  dim2 <- dim(mat2)
  
  # get the maximum dimensions
  max_dim <- pmax(dim1, dim2)
  
  # create a matrix of NA values with the maximum dimensions
  mat_sum <- matrix(NA, nrow = max_dim[1], ncol = max_dim[2])
  
  # loop through each element of the matrices and apply the conditions
  for (i in 1:max_dim[1]) {
    for (j in 1:max_dim[2]) {
      # check if the indices are within the dimensions of the matrices
      if (i <= dim1[1] & j <= dim1[2] & i <= dim2[1] & j <= dim2[2]) {
        # If both values are NA, then NA.
        if (is.na(mat1[i,j]) & is.na(mat2[i,j])) {
          mat_sum[i,j] <- NA
        }
        # If value in mat1 is NA, take the other.
        else if (is.na(mat1[i,j])) {
          mat_sum[i,j] <- mat2[i,j]
        }
        # If value in mat2 is NA, take the other.
        else if (is.na(mat2[i,j])) {
          mat_sum[i,j] <- mat1[i,j]
        }
        # If both values are equal, take the value.
        else if (mat1[i,j] == mat2[i,j]) {
          mat_sum[i,j] <- mat1[i,j]
        }
        # If both values are Non-NA and not equal, compute the mean.
        else {
          mat_sum[i,j] <- mean(c(mat1[i,j], mat2[i,j]))
        }
      }
      else {
        # take the value of the matrix with the higher dimension
        if (i <= dim1[1] & j <= dim1[2]) {
          mat_sum[i,j] <- mat1[i,j]
        }
        else if (i <= dim2[1] & j <= dim2[2]) {
          mat_sum[i,j] <- mat2[i,j]
        }
      }
    }
  }
  return(mat_sum)
}

###### **Prerequisite function** step 1.2 to remove the additional dimensions in spanish #########
##################################################################################################

## Trim_matrix ##
trim_matrix <- function(list_item) {
  list_item$similarity_matrix <- list_item$similarity_matrix[-c(1,2), -c(1,2)]
  # If there are other matrices inside, you can apply the same logic. For example:
  # list_item$other_matrix <- list_item$other_matrix[-c(1,2), -c(1,2)]
  return(list_item)
}

###### **Prerequisite function** for to add matrices following 4 conditions given the NAs values   ######
###### Function used for different versions of combined in step 18!!                               ######

## f_add_matrices ## 
f_add_matrices <- function(mat1, mat2) {
  # create a matrix of NA values with the same dimensions as the input matrices
  mat_sum <- matrix(NA, nrow = nrow(mat1), ncol = ncol(mat1))
  
  # loop through each element of the matrices and apply the conditions
  for (i in 1:nrow(mat1)) {
    for (j in 1:ncol(mat1)) {
      # If both values are NA, then NA.
      if (is.na(mat1[i,j]) & is.na(mat2[i,j])) {
        mat_sum[i,j] <- NA
      } 
      # If value in mat1 is NA, take the other. 
      else if (is.na(mat1[i,j])) {
        mat_sum[i,j] <- mat2[i,j]
      } 
      # If value in mat2 is NA, take the other. 
      else if (is.na(mat2[i,j])) {
        mat_sum[i,j] <- mat1[i,j]
      } 
      # If both values are equal, take the value.
      else if (mat1[i,j] == mat2[i,j]) {
        mat_sum[i,j] <- mat1[i,j]
      } 
      # If both values are Non-NA and not equal, compute the mean.
      else {
        mat_sum[i,j] <- mean(c(mat1[i,j], mat2[i,j]))
      }
    }
  }
  
  return(mat_sum)
} 

###### **Prerequisite function** for step 2 that Completes the similarity matrix #######
########################################################################################

### Function for Step 2 ### 
## f_complete_matrix ## 
f_complete_matrix <- function(matrix, matrix_name) {
  dfeng <- as.data.frame(matrix)
  dfeng2 <- dfeng %>% rownames_to_column(var = "rowname") %>% gather(key = "colname", value = "value", -rowname)
  dfeng3 <- dfeng2 %>% separate(rowname, into = c("categoryRow", "runRow", "exemplarRow"), sep = "_") %>% 
    separate(colname, into = c("categoryCol", "runCol", "exemplarCol"), sep = "_") %>% 
    mutate(categoryRow = as.numeric(categoryRow), categoryCol = as.numeric(categoryCol))
  for (i in unique(dfeng3$categoryRow)) {
    for (j in unique(dfeng3$categoryCol)) {
      mean_value <- dfeng3 %>% filter(categoryRow == i, categoryCol == j) %>% 
        summarise(mean_value = mean(value, na.rm = TRUE)) %>% pull(mean_value)
      dfeng3 <- dfeng3 %>% mutate(value = ifelse(is.na(value) & categoryRow == i & categoryCol == j, mean_value, value))
    }
  }
  dfeng4 <- dfeng3 %>% unite(colname, categoryCol, runCol, exemplarCol, sep = "_")
  dfeng4$rowname <- dfeng2$rowname
  dfeng4$colname <- dfeng2$colname
  dfeng5 <- dfeng4[,!names(dfeng4) %in% c("categoryRow",'runRow','exemplarRow')]
  mat_reord3eng_compl_fun <- dfeng5 %>% spread(key = colname, value = value) %>% dplyr::select(-rowname) %>% as.matrix()
  
  # Rename the rows as the previous matrix
  rownames(mat_reord3eng_compl_fun) <- rownames(matrix)
  # Rename the matrix dimensions
  dimnames(mat_reord3eng_compl_fun) <- dimnames(matrix)
  # Include the 100 values for the main diagonal
  diag(mat_reord3eng_compl_fun) <- 100
  # Checking for symmetry on the matrix
  isSymmetric(mat_reord3eng_compl_fun)
  # Total NA present in the matrix
  total_NA <- sum(is.na(mat_reord3eng_compl_fun))
  # Proportion of NAs in total cells in matrix
  proportion_NA <- sum(is.na(mat_reord3eng_compl_fun))/ length(mat_reord3eng_compl_fun)
  
  # Adjusted print statements to include matrix_name
  print(paste("For matrix", matrix_name, "- The number of NAs is", total_NA))
  print(paste("For matrix", matrix_name, "- The percentage of NAs is", proportion_NA * 100, '%'))
  
  return(mat_reord3eng_compl_fun)
} 
############# **Prerequisite** for step 3 to combine matrices  #########################
########################################################################################

### Prerequisites for step 3 ###
# Define the pairs
pairs <- list(
  c("original", "original"),
  c("General Similarity", "General Similarity"),
  c("animacy", "animación"),
  c("preference", "prefieres"),
  c("scare", "asustan"),
  c("color", "color"),
  c("dissimilar", "disimilaridad"),
  c("shape", "forma"),
  c("category", "categorías")
)

# Define the pairs2 to combine the combined_obj ones #
pairs2 <- list(
  c("original", "original"),
  c("General Similarity", "General Similarity"),
  c("animacy", "animación"),
  c("preference", "prefieres"),
  c("scare", "asustan"),
  c("color", "color"),
  c("dissimilar", "disimilaridad"),
  c("shape", "forma"),
  c("category", "categorías"),
  c("combined_obj_eng","combined_obj_esp")
)

combined_matrices <- list()

###### **Prerequisite function** for step 4 that calculate the mean per category #######
########################################################################################

## f_mean_values_cat ## 
# create a sample matrix
f_Mean_Values_cat <- function(matrix) {  
  # extract the first number from the column names
  numbers <- gsub("_.*", "", colnames(matrix))
  
  # calculate the mean response for each number in the rows
  row_means <- aggregate(matrix, by = list(numbers), FUN = mean)
  
  # calculate the mean response for each number in the columns
  col_means <- aggregate(t(matrix), by = list(numbers), FUN = mean)
  
  # create a 40x40 matrix of means 
  means_mat <- matrix(NA, nrow = 40, ncol = 40)
  for (i in 1:40) {
    for (j in 1:40) {
      means_mat[i, j] <- mean(matrix[numbers == row_means$Group.1[i], numbers == col_means$Group.1[j]])
    }
  }
  # set the row and column names of the matrix
  rownames(means_mat) <- row_means$Group.1
  colnames(means_mat) <- col_means$Group.1
  
  # view the result
  return(means_mat)
}  

#############################---------------------------------------------################################ 
#############################           ANALYSIS CODE FOR THE DATA        ################################ 
#############################---------------------------------------------################################
  
### Personal Comment: Initially I started working with both languages but given differences       ###
### between dimensions, The way to go was processed them individually and then add them together  ###
### with the conditional function created to add matrices together                                ###
### ----------------------------------------------------------------------------------------------###

### --------------------------------------------------------------------------------------------------- ###
### ------------ SECTION 1: GENERAL RDMs: SPANISH, ENGLISH, COMBINED 280, COMBINED 282  --------------- ###
### --------------------------------------------------------------------------------------------------- ###      

## Relevant details/comments for the analysis here ##

## Last step output files with all the dataframes. ##
#eng_filtered_dfs_names
#spa_filtered_dfs_names

## To Access the matrices in the specific list. ##
#similarity_matrix_esp_original <- mat_reord2esp_list[['original']][['similarity_matrix']]
#view(similarity_matrix_esp_original)

# --------------------------- # ----------------------------- # -------------------------- #

### Step 1: - create symmetric similarity matrices with frequency matrix ####
## English ##
mat_reord2eng_list <- list()
for (blocktype in names(eng_filtered_dfs_names)) {
  df <- eng_filtered_dfs_names[[blocktype]]
  # Directly store the similarity matrix result into the list
  mat_reord2eng_list[[blocktype]] <- f_create_similarity_matrix(df, 'LeftImg', 'RightImg', matrix_name = blocktype)
  # Check for values of 2 in the frequency matrix
  indices_of_2 <- which(mat_reord2eng_list[[blocktype]]$frequency_matrix == 2, arr.ind = TRUE)

  if (nrow(indices_of_2) > 0) {
    print(paste("For matrix", blocktype, "cells with value of 2 are:"))
    print(indices_of_2)
  } else {
    print(paste("For matrix", blocktype, "- Matrix full binary"))
  }
}

View(mat_reord2eng_list$original[["similarity_matrix"]])  ## voy aca 
## Spanish ##
mat_reord2esp_list <- list()
for (blocktype in names(spa_filtered_dfs_names)) {
  df <- spa_filtered_dfs_names[[blocktype]]
  # Directly store the similarity matrix result into the list
  mat_reord2esp_list[[blocktype]] <- f_create_similarity_matrix(df, 'LeftImg', 'RightImg',matrix_name = blocktype)
  # Check for values of 2 in the frequency matrix
  indices_of_2 <- which(mat_reord2esp_list[[blocktype]]$frequency_matrix == 2, arr.ind = TRUE)
  
  if (nrow(indices_of_2) > 0) {
    print(paste("For matrix", blocktype, "cells with value of 2 are:"))
    print(indices_of_2)
  } else {
    print(paste("For matrix", blocktype, "- Matrix full binary"))
  }
}  

### Step 1.2 Remove the additional dimensions from the Spanish data (#example images 2 and 3 saved from category 1!!!#) ###
mat_reord2esp_list <- lapply(mat_reord2esp_list, trim_matrix)
#View(mat_reord2esp_list[["original"]][["similarity_matrix"]])

### Step 2 - fill up the matrices with mean values for each category  ###
completed_matrices_eng <- list()
for (blocktype in names(mat_reord2eng_list)) {
  matrix <- mat_reord2eng_list[[blocktype]]$similarity_matrix
  completed_matrices_eng[[blocktype]] <- f_complete_matrix(matrix, matrix_name = blocktype)
}
  
completed_matrices_esp <- list()
for (blocktype in names(mat_reord2esp_list)) {
  matrix <- mat_reord2esp_list[[blocktype]]$similarity_matrix
  completed_matrices_esp[[blocktype]] <- f_complete_matrix(matrix, matrix_name = blocktype)
}


###################################################################################################################
###################################################################################################################
############## DIFFERENT ALTERNATIVES TO COMPLETE THE MATRICES - start of options #################################

## Possible solutions: 1) Matrix factorization custome(get the low rank but save the old values), 2)  

#### Personal comment: Matrix factorization re-calculates all the values and it does not only fill up the NA values.
#### So method discarded. 

## Step 2B - fill up matrices with matrix factorization algorythm ##
# Previously cut the data in a 9/1 ratio for future comparisons #

########################################################
### Matrix factorization with my own data to compare ### 
mat_sim <- mat_reord2eng_list[["original"]][['similarity_matrix']]

# Assuming your similarity matrix is stored in mat_sim
# and that missing values are represented by NA

## Pre-processing of the matrix for Matrix factorization ##
# 1. Identify the non-missing values
non_na_indices <- which(!is.na(mat_sim), arr.ind = TRUE)

# The number of non-missing values
num_non_na <- nrow(non_na_indices)
head(num_non_na)
# 2. Randomly select 10% of the non-missing values to be in the test set
set.seed(123)  # Setting seed for reproducibility
test_size <- floor(0.1 * num_non_na)  # Size of the test set
test_indices <- sample(1:num_non_na, test_size)

# Indices for the test set
test_set_indices <- non_na_indices[test_indices, ]

# 3. Create the training set by cloning the original matrix and then setting the test set values to NA
training_set <- mat_sim
training_set[test_set_indices] <- NA

# 4. Create the test set - this will be a smaller matrix/array that stores just the test values and their locations.
test_set_values <- mat_sim[test_set_indices]

## Matrix factorization process ##
#install.packages('softImpute')
library(softImpute)

# Set seed for reproducible results
set.seed(123)

# Apply matrix factorization to fill in the missing values
# The rank.max and lambda parameters may need to be adjusted based on cross-validation or other criteria
completed_matrix <- softImpute::softImpute(training_set, rank.max = 2, lambda = 0.1, type = "als")

# The completed matrix is obtained from the U, D, V components
imputed_matrix <- completed_matrix$u %*% diag(completed_matrix$d) %*% t(completed_matrix$v)
View(imputed_matrix)


## Step 2C - fill up matrices with KNN (cluster K-means) algorythm ##
## Errased and not used due to too many NAs values ##


#######################################################  MICE MICE CODE MICE CODE MICE ######################################################
#############################################################################################################################################
### Step 2D - fill up matrices with MICE ALGORITHM ### 

#### personal comment: a Mice imputation for the matrix of 280 variables takes 6-7minutes to complete ##
#### Also: After the correlation scores of the test_set with the imputed values and test_values, they were uncorrelated #### 
#### So different alternative, try MICE but within the category matrices 7x7 and then combine them ####

## Main steps to perform with MICE as from the book and guidelines: ##   ## Also add the code-needed steps such as re-labelling ## 
# 1. have the matrix or list of matrices. 
# 2. Fill up the diagonals.
# 3. Missing data pattern.
# 4. imputed the data. 
# 5. analyze the data. 
# 6. Pool the data. 
# 7. Reconstruct the matrices


### Part 2D-1) MICE  FOR THE 7X7 diagonal subcategories to capture category substructure ###
library(mice)

# I need to extract both: all data english and spanish matrices from the list above (only those 2) #
# Make a deep copy of the input matrix to avoid altering the original matrix
#mat_copy <- matrix(as.numeric(input_matrix), nrow = nrow(input_matrix), ncol = ncol(input_matrix))
#rownames(mat_copy) <- rownames(input_matrix)
#colnames(mat_copy) <- colnames(input_matrix)

# The original 280x280 matrix
mat_sim_eng_Ad <- mat_reord2eng_list[["original"]][['similarity_matrix']] 

# Assuming you have the category_names vector as provided
category_names <- c("snake","Frog","Cockroach","Ants","spider","Worms","Bee","Butterflies","Birds","Bat","Chicken",
                    "Mouse","dog","Cat","Horses","Shark","Fish","Gecko","Turtle","Beatle","Grasshoper","Catterpillar",
                    "Fly","Peacock","Guinea_pig","Sheep","Rabbit","Giraffe","Whale","Dolphin","Airplanes","Car","Bicycle",
                    "Scissor","Hammer","Key","Guitar","Cellphone","Umbrella","Chair")


# Each group has 7 exemplars, so we repeat each category name 7 times and append a sequence from 1 to 7
labels <- unlist(lapply(category_names, function(category) paste0(category, 1:7)))

# Assuming your matrix is named mat_sim_eng_Ad2 and is 280x280
mat_sim_eng_Ad2 <- mat_sim_eng_Ad
# You can now assign these labels to the rows or columns of your matrix. For example, to assign to rows:
rownames(mat_sim_eng_Ad2) <- labels

# Similarly, if you want to assign to columns as well
colnames(mat_sim_eng_Ad2) <- labels

#### Step 2D - 1:  Extraction, imputation and reconstruction - 1-210x7 items in sub-matrices per category ####
#### (animated rows and colums) ####

## ---------- Functional approach ------------ ###
## Imput the updated 280x280 matrices per columns of category. Also, I will impute 30 first 
## of [210 rows x 7columns] because we know that the pattern of the data is quite different between
## the first 30 categories(living things) and the other 10 (unanimated objects). 

  ## Function to extract 210x7 matrices. 
# Define the function to extract 210x7 sub-matrices with correct row and column labels
extract_column_matrices <- function(input_matrix, category_names) {
  # Since the matrix is symmetrical, generate labels for the full matrix first
  full_labels <- unlist(lapply(category_names, function(category) paste0(category, 1:7)))
  
  # Initialize the list to store the 210x7 submatrices
  sub_matrices <- list()
  
  # Iterate to extract each 210x7 matrix (covering columns until 210, for the first 30 groups of categories)
  for (i in 1:30) {
    # Extract the 210x7 sub-matrix
    sub_matrix <- input_matrix[1:210, ((i-1)*7 + 1):(i*7)]
    
    # Assign row labels from the first 30 categories
    row_labels <- unlist(lapply(category_names[1:30], function(category) paste0(category, 1:7)))
    rownames(sub_matrix) <- row_labels
    
    # Assign column labels based on the specific group of 7 columns being extracted
    col_labels <- full_labels[((i-1)*7 + 1):(i*7)]
    colnames(sub_matrix) <- col_labels
    
    # Assign the sub-matrix to the list
    sub_matrices[[i]] <- sub_matrix
  }
  
  # Optionally, assign names to the list elements to reflect their original column positions
  names(sub_matrices) <- sapply(1:30, function(i) paste0("sub_matrix_cols_", ((i-1)*7 + 1), "_to_", i*7))
  
  return(sub_matrices)
}

## Step 2D - 1.1 Extract the 210x7 matrices from the original matrix. 
sub_matrices_list_columnwise <- extract_column_matrices(mat_sim_eng_Ad2, category_names)
View(sub_matrices_list_columnwise[[2]])

library(mice)
# Step 2D - 1.2: run the MICE function storing all the elements of importance #
imp_eng_ad_210 <- impute_sub_matrices(sub_matrices_list_columnwise) ## Processing duration: 1:26 started - around 20min ##
#View(imp_eng_ad_210$imputed_matrices[[1]][[1]])

# Define function to replace imputed 210x7 values on 3 different versions of the matrices.
replace_column_matrices210 <- function(original_matrix, imp_matrices) {
  # Prepare 3 versions of the original matrix to be filled with imputed values
  versions <- list(matrix_1 = as.matrix(original_matrix), 
                   matrix_2 = as.matrix(original_matrix), 
                   matrix_3 = as.matrix(original_matrix))
  
  for (i in 1:length(imp_matrices$imputed_matrices)) {
    for (j in 1:3) {
      # Extracting the specific imputed dataset
      imputed_sub_matrix <- as.matrix(imp_matrices$imputed_matrices[[i]][[j]])
      
      # Determine the starting and ending indices for the columns
      col_start <- (i - 1) * 7 + 1
      col_end <- col_start + 6  # Since each sub-matrix is 7 columns wide
      
      # Replacing the values in the corresponding version of the original matrix
      versions[[j]][1:210, col_start:col_end] <- imputed_sub_matrix
    }
  }
  return(versions)
}

## Step 2D - 1.3: reconstruct the original matrix with the imputed values for each imputation #
updated_matrices_columns210x7 <- replace_column_matrices210(mat_sim_eng_Ad2, imp_eng_ad_210)
#View(updated_matrices_columns210x7$matrix_1)

#### Step 2D - 2: Do the same process of extraction, imputation and reconstruction in the 211-280x7 items left ####
#### (unanimated rows but animated columns). ####

### IMPORTANT COMMENT: ONCE I AM DONE WITH THE IMPUTATION OF THE LAST 10 CATEGORIES, I NEED TO 
### ORDER THE CODE 1) FIRST THE 210X7, 2) THEN last rows 70X7 OTHER CATEGORIES, 3) the 211-280 columns until rows (same as 2 if symmetric but
## due to imputation, better to have average between them), AND 4) unanimated objects and 5) LASTLY DIAGONALS. 
### ONCE I HAVE DONE THAT, I WOULD HAVE MY FIRST COMPLETED IMPUTED MATRICES DATASET. ####

## ---------- Functional approach ------------ ###
## Function to extract 211-280rows x 7 matrices.

# Define the function to extract 211:280 x 7 sub-matrices with correct row and column labels
# Function to extract 70x7 sub-matrices from the bottom of a 280x280 matrix
extract_bottom_matrices <- function(input_matrix, category_names) {
  # Generate row labels for the last 10 categories since we're working with the last 70 rows
  row_labels <- unlist(lapply(category_names[31:40], function(category) paste0(category, 1:7)))
  
  # Initialize the list to store the 70x7 submatrices
  sub_matrices <- list()
  
  # Iterate to extract each 70x7 matrix (covering columns for the first 30 groups of categories)
  for (i in 1:30) {
    # Extract the 70x7 sub-matrix
    sub_matrix <- input_matrix[211:280, ((i-1)*7 + 1):(i*7)]
    
    # Assign row and column labels
    rownames(sub_matrix) <- row_labels
    col_labels <- unlist(lapply(category_names[1:30], function(category) paste0(category, 1:7)))[((i-1)*7 + 1):(i*7)]
    colnames(sub_matrix) <- col_labels
    
    # Assign the sub-matrix to the list
    sub_matrices[[i]] <- sub_matrix
  }
  
  names(sub_matrices) <- sapply(1:30, function(i) paste0("sub_matrix_bottom_cols_", ((i-1)*7 + 1), "_to_", i*7))
  
  return(sub_matrices)
}

## Step 2D - 2.1 Extract the last 70x7 matrices from the original matrix. 
sub_matrices_list_columnwise211 <- extract_bottom_matrices(mat_sim_eng_Ad2, category_names)
#View(sub_matrices_list_columnwise211[[2]])
#length(sub_matrices_list_columnwise211)

# Step 2D - 2.2: run the MICE function storing all the elements of importance #
imp_eng_ad_70 <- impute_sub_matrices(sub_matrices_list_columnwise211) ## Processing duration: 15 minutes ##
View(imp_eng_ad_70$imputed_matrices[[1]][[1]])

#sum(is.na(imp_eng_ad_70[[1]][[1]])) ## not NAs

## Function to replace the imputed 70x7 values in the already partially updated matrices
replace_bottom_matrices <- function(updated_matrices, imp_matrices_bottom) {
  # We're now expecting 'updated_matrices' to be a list of matrices already updated with the first 210 rows imputed
  
  for (i in 1:length(imp_matrices_bottom$imputed_matrices)) {
    for (j in 1:3) {
      # Extracting the specific imputed dataset
      imputed_sub_matrix_bottom <- as.matrix(imp_matrices_bottom$imputed_matrices[[i]][[j]])
      
      # Indices for the columns
      col_start <- (i - 1) * 7 + 1
      col_end <- col_start + 6
      
      # Replacing the values in the last 70 rows of the corresponding columns in each version
      updated_matrices[[j]][211:280, col_start:col_end] <- imputed_sub_matrix_bottom
    }
  }
  return(updated_matrices)
}

## Step 2D - 2.3: reconstruct the original matrix with the imputed values for each imputation #
#'updated_matrices_columns210x7' is the list of matrices with the first 210 rows imputed,
#'imp_eng_ad_70' is your list of imputed 70x7 matrices.
updated_matrices_280x210 <- replace_bottom_matrices(updated_matrices_columns210x7, imp_eng_ad_70)
View(updated_matrices_280x210$matrix_1)

#### Step 2D - 3: Do the same process of extraction, imputation and reconstruction in the 1-210x211-280 items ####
#### (animated rows but unanimated columns). ####

# Function to extract sub-matrices from the right edge (last 70 columns) of the 280x280 matrix
extract_right_edge_matrices <- function(input_matrix, category_names) {
  # Since the matrix is symmetrical, the labels for the last 70 columns will be the last 10 category names
  edge_labels <- unlist(lapply(category_names[31:40], function(category) paste0(category, 1:7)))
  
  # Initialize the list to store the 210x7 submatrices for the right edge
  edge_matrices <- list()
  
  # Iterate to extract each 210x7 matrix from the last 70 columns
  for (i in 31:40) {  # Assuming there are 40 categories in total
    # Extract the 210x7 sub-matrix
    sub_matrix <- input_matrix[1:210, ((i-1)*7 + 1):(i*7)]
    
    # Assign the original column labels from the edge labels we calculated
    colnames(sub_matrix) <- edge_labels[((i-31)*7 + 1):((i-30)*7)]
    
    # Assign row labels for the first 210 rows
    row_labels <- unlist(lapply(category_names[1:30], function(category) paste0(category, 1:7)))
    rownames(sub_matrix) <- row_labels[1:210]  # First 210 row labels
    
    # Assign the sub-matrix to the list
    edge_matrices[[i-30]] <- sub_matrix  # i-30 to start index at 1
  }
  
  # Optionally, name the list elements to reflect their original column positions
  names(edge_matrices) <- sapply(31:40, function(i) paste0("sub_matrix_edge_cols_", ((i-1)*7 + 1), "_to_", i*7))
  
  return(edge_matrices)
}

## Step 2D - 3.1 Extract the last 70x7 matrices from the original matrix. 
sub_matrices_list_last70 <- extract_right_edge_matrices(mat_sim_eng_Ad2, category_names)
#View(sub_matrices_list_last70[[2]])
#length(sub_matrices_list_last70)

## Step 2D - 3.2: run the MICE function storing all the elements of importance #
imp_eng_ad_last70cols <- impute_sub_matrices(sub_matrices_list_last70) ## Processing duration: 5:22 started minutes ##
#View(imp_eng_ad_last70cols$imputed_matrices[[1]][[1]])

## Function to replace the imputed 210x7 edge values in the already partially updated matrices
replace_right_edge_matrices <- function(updated_matrices, imp_matrices_edge) {
  for (i in 1:length(imp_matrices_edge$imputed_matrices)) {
    for (j in 1:3) {
      # Extracting the specific imputed dataset
      imputed_sub_matrix_edge <- as.matrix(imp_matrices_edge$imputed_matrices[[i]][[j]])
      
      # Indices for the columns, adjusted for the last 70 columns
      col_start <- 280 - 70 + (i - 1) * 7 + 1
      col_end <- col_start + 6
      
      # Replace the values in the first 210 rows of the corresponding columns
      updated_matrices[[j]][1:210, col_start:col_end] <- imputed_sub_matrix_edge
    }
  }
  return(updated_matrices)
}

# 'updated_matrices_columns210x7' should be the list of matrices with the first 210 columns imputed,
# and 'imp_eng_ad_210_right_edge' is your list of imputed 210x7 matrices for the right edge.

## Step 2D - 3.3: reconstruct the original matrix with the imputed values for each imputation # 
updated_matrices_210x280 <- replace_right_edge_matrices(updated_matrices_280x210, imp_eng_ad_last70cols)
View(updated_matrices_210x280$matrix_1)

#### Step 2D - 4: Same process of extraction, (complete NAs and symmetric) imputation, and reconstruction of lower diagonal ####
#### matrices (unanimated objects) 70x70 ####

# Function to extract 70x7 sub-matrices for the last 70 rows and last 10 categories
extract_bottom_right_matrices <- function(input_matrix, category_names) {
  # Labels for the last 10 categories
  row_labels <- unlist(lapply(category_names[31:40], function(category) paste0(category, 1:7)))
  
  # Initialize the list to store the 70x7 submatrices for the bottom right
  bottom_right_matrices <- list()
  
  # Start extracting from the last 70 columns, which corresponds to the last 10 categories
  for (i in 31:40) {
    # Extract the 70x7 sub-matrix
    sub_matrix <- input_matrix[211:280, ((i-1)*7 + 1):(i*7)]
    
    # Assign the original row and column labels for the last 10 categories
    rownames(sub_matrix) <- row_labels
    colnames(sub_matrix) <- row_labels[((i-31)*7 + 1):((i-30)*7)]
    
    # Assign the sub-matrix to the list
    bottom_right_matrices[[i-30]] <- sub_matrix
  }
  
  names(bottom_right_matrices) <- sapply(31:40, function(i) paste0("sub_matrix_bottom_right_", ((i-1)*7 + 1), "_to_", i*7))
  
  return(bottom_right_matrices)
}

## Step 2D- 4.1 - Subdivision of the 280x280 matrices into the 7x7 structures. ##
sub_matrices_list_Eng_last70x70 <- extract_bottom_right_matrices(mat_sim_eng_Ad2, category_names)
View(sub_matrices_list_Eng_last70x70$sub_matrix_bottom_right_211_to_217)

## Step 2D- 4.2: run the MICE function storing all the elements of importance 
imp_eng_ad_last70x70 <- impute_sub_matrices(sub_matrices_list_Eng_last70x70) ## Processing duration: 3:13 ##

## Function to replace the imputed 70x7 bottom-right values into the updated matrices
replace_bottom_right_matrices <- function(updated_matrices, imp_matrices_bottom_right) {
  for (i in 1:length(imp_matrices_bottom_right$imputed_matrices)) {
    for (j in 1:3) {
      # Extracting the specific imputed dataset
      imputed_sub_matrix_bottom_right <- as.matrix(imp_matrices_bottom_right$imputed_matrices[[i]][[j]])
      
      # Calculate the column start index for the last 10 categories (last 70 columns)
      col_start <- 280 - 70 + (i - 1) * 7 + 1
      col_end <- col_start + 6
      
      # Replace the values in the last 70 rows of the corresponding columns
      updated_matrices[[j]][211:280, col_start:col_end] <- imputed_sub_matrix_bottom_right
    }
  }
  return(updated_matrices)
}


## Step 2D - 4.3: reconstruct the original matrix with the imputed values for each imputation # 
updated_matrices_280x280 <- replace_bottom_right_matrices(updated_matrices_210x280, imp_eng_ad_last70x70)
View(updated_matrices_280x280$matrix_1)

#sum(is.na(updated_matrices_280x280$matrix_1)) 

#### Step 2D - 5: Same process of extraction, imputation, (complete NAs and symmetric extra steps) and reconstruction of the diagonal ####
#### matrices (animated objects) 210x210 #### 

#### ------- Function - approach ------- ####
#library(mice)
## Define the function to extract the 7x7 matrices
extract_sub_matrices_7x7 <- function(input_matrix, category_names) {
  # Generate labels
  labels <- unlist(lapply(category_names, function(category) paste0(category, 1:7)))
  
  # Create a copy of the input_matrix to avoid altering the original
  mat_copy <- matrix(as.numeric(input_matrix), nrow = nrow(input_matrix), ncol = ncol(input_matrix))
  rownames(mat_copy) <- labels
  colnames(mat_copy) <- labels
  
  # Fill up the diagonal with 100 values
  diag(mat_copy) <- 100
  
  # List to store the 7x7 sub-matrices
  sub_matrices <- list()
  
  # Number of categories
  num_categories <- length(category_names)
  
  # Split the matrix into 7x7 submatrices along the diagonal
  for (i in 1:num_categories) {
    # Extract the submatrix
    sub_matrix <- mat_copy[((i-1)*7 + 1):(i*7), ((i-1)*7 + 1):(i*7)]
    # Assign the submatrix to the list using a numerical 
    sub_matrices[[i]] <- sub_matrix
  }
  
  # Optionally, assign names to the list elements if needed
  names(sub_matrices) <- sapply(1:num_categories, function(i) paste0("sub_matrix_", i, "_", deparse(substitute(input_matrix))))
  
  return(sub_matrices)
}
## Step 2D-5.1 - Subdivision of the 280x280 matrices into the 7x7 structures. ##
sub_matrices_list_Eng_AllData <- extract_sub_matrices_7x7(mat_sim_eng_Ad2, category_names)
#View(sub_matrices_list_Eng_AllData$sub_matrix_1_input_matrix)

## Part 2D-5.2: Imputations over the list of matrices. 
## ---------- Functional approach ------------ ###
library(mice)
## Define a function that imputes over the list of extracted sub-matrices
impute_sub_matrices <- function(sub_matrices) {
  # Initialize the lists to store the imputed submatrices and imp objects
  imputed_matrices_7x7 <- list()
  imp_list <- list()
  
  # Iterate over the submatrices
  for (i in seq_along(sub_matrices)) {
    # Initial form of the imputation to get the predictor matrix
    imp0 <- mice(sub_matrices[[i]], maxit = 0)
    pred <- imp0$predictorMatrix
    
    # Ensure all variables are considered in the imputation process
    # This example assumes you want to modify the predictor matrix to include all variables
    # Update this part as needed to match your specific requirements
    var_names <- rownames(pred)
    pred[,] <- 1  # Consider all variables for each other
    diag(pred) <- 0  # But each variable should not predict itself
    
    # Perform MICE imputation on the current 7x7 matrix with the modified predictor matrix
    imp1 <- mice(sub_matrices[[i]], seed = 500, m = 3, method = 'pmm', maxit = 1000, predictorMatrix = pred, print = TRUE)
    
    # Store the imp object for diagnostics
    imp_list[[i]] <- imp1
    
    # Extract and store the imputed datasets
    imputed_matrices_7x7[[i]] <- lapply(1:3, function(j) complete(imp1, j))
  }
  
  return(list(imputed_matrices = imputed_matrices_7x7, imp_objects = imp_list))
}

# Step 2D - 5.2: run the MICE function storing all the elements of importance #
imp_eng_ad <- impute_sub_matrices(sub_matrices_list_Eng_AllData) ## Processing duration: around 30 min. ##
## P.C.: There were only 2 cases where the pred matrix was 0 for 2 rows and they still have NA values (in BAT). 
## So I need to average them and I will do it in the complete_NAs function.

imp_eng_ad$imputed_matrices[[1]][[10]]
imp_eng_ad$imp_objects
#complete(imp_eng_2mat$imp_objects[[2]])

## ---------- Functional approach ------------ ###
## Enhanced function to complete NAs with their symmetrical counterparts or row/column means ##
complete_NAs <- function(imp_eng_ad) {
  num_sub_matrices <- length(imp_eng_ad$imputed_matrices)
  
  for (i in 1:num_sub_matrices) {
    for (j in 1:3) {
      # Access the specific imputed dataset
      matrix <- imp_eng_ad$imputed_matrices[[i]][[j]]
      
      # Complete NA values with their symmetrical counterparts or row/column means
      for (row in 1:nrow(matrix)) {
        for (col in 1:ncol(matrix)) {  # Update to iterate through entire matrix
          if (is.na(matrix[row, col])) {
            if (!is.na(matrix[col, row])) {
              # Use symmetrical value if available
              matrix[row, col] <- matrix[col, row]
            } else {
              # Both values are NAs, use mean of the row or column
              # Exclude NAs in mean calculation
              row_mean <- mean(matrix[row, ], na.rm = TRUE)
              col_mean <- mean(matrix[, col], na.rm = TRUE)
              
              # If both means are NA (entire row/column are NAs), skip
              if (!is.na(row_mean) || !is.na(col_mean)) {
                # Prefer row mean if not NA, otherwise use column mean
                matrix[row, col] <- if (!is.na(row_mean)) row_mean else col_mean
              }
              # Note: If both row_mean and col_mean are NA, no replacement occurs
            }
          }
        }
      }
      
      # Update the original matrix directly
      imp_eng_ad$imputed_matrices[[i]][[j]] <- matrix
    }
  }
  
  # Return the modified list with completed matrices
  return(imp_eng_ad)
}

#Step 2D - 5.3: Fill out positional arguments with symmetrical counterpart in the 7x7 sub_matrices. 
imp_eng_ad_updated <- complete_NAs(imp_eng_ad)

# Simplified direct check for NA values in all imputed matrices
#na_check_direct <- lapply(imp_eng_ad_updated$imputed_matrices, function(mat_list) {
#  lapply(mat_list, function(m) {
#    any(is.na(m))  # Check directly for NAs in each matrix
#  })
#})
#na_check_direct  #will be a list of logical vectors indicating the presence of NAs in each matrix

## Step 2D - 5.4 correction due to bat3 and bat 6 NA after imputation ##
## ---------- Functional approach ------------ ###
# Function to make all the imputed matrices symmetric
make_matrices_symmetric <- function(imp_eng_ad) {
  num_sub_matrices <- length(imp_eng_ad$imputed_matrices)
  
  for (i in 1:num_sub_matrices) {
    for (j in 1:3) {
      # Access the specific imputed dataset
      mat_reord <- imp_eng_ad$imputed_matrices[[i]][[j]]
      
      # Make a copy of the matrix
      mat_copy <- mat_reord
      
      # Transpose the matrix
      mat_t <- t(mat_reord)
      
      # Add the original matrix and its transpose, then average to ensure symmetry
      mat_fin <- f_add_matrices(mat_copy, mat_t)
      
      # Assign the original dimension names to the resulting matrix
      dimnames(mat_fin) <- dimnames(mat_reord)
      
      # Update the original matrix with the symmetric one
      imp_eng_ad$imputed_matrices[[i]][[j]] <- mat_fin
    }
  }
  
  # Return the updated list with symmetric matrices
  return(imp_eng_ad)
}

#Step 2D - 5.4: Making symmetric the matrix
imp_eng_ad_symmetric <- make_matrices_symmetric(imp_eng_ad_updated)
#imp_eng_ad_symmetric$imputed_matrices[[10]]

## ---------- Functional approach ------------ ###
## Function to replace the diagonal 7x7 matrices into the updated matrices
replace_diagonal_matrices <- function(updated_matrices, imp_eng_ad_symmetric) {
  num_sub_matrices <- length(imp_eng_ad_symmetric$imputed_matrices)
  
  # Iterate over each of the updated matrices
  for (i in 1:num_sub_matrices) {
    for (j in 1:3) {
      # Calculate the starting row and column for the sub-matrix
      start_index <- (i - 1) * 7 + 1
      end_index <- i * 7
      
      # Extract the symmetric sub-matrix for the current version
      symmetric_sub_matrix <- as.matrix(imp_eng_ad_symmetric$imputed_matrices[[i]][[j]])
      
      # Replace the corresponding diagonal block in the current version of updated matrices
      updated_matrices[[j]][start_index:end_index, start_index:end_index] <- symmetric_sub_matrix
    }
  }
  
  # Return the list of further updated matrices
  return(updated_matrices)
}

## Step 2D - 5.5: replace the 7x7 imputed matrices into the imputed_integrated matrices. 
updated_matrices_280x280_wDiag <- replace_diagonal_matrices(updated_matrices_280x280, imp_eng_ad_symmetric)

## Step 2D - 6: make the final matrices symmetrical - from list.
## Function to make a list of matrices symmetric
f_make_matrices_list_symmetric <- function(matrices_list) {
  # Iterate over each matrix in the list
  for (matrix_name in names(matrices_list)) {
    # Access the specific matrix
    matrix <- matrices_list[[matrix_name]]
    
    # Make a copy of the matrix
    mat_copy <- matrix
    
    # Transpose the matrix
    mat_t <- t(matrix)
    
    # Average the original matrix and its transpose to ensure symmetry
    mat_fin <- (mat_copy + mat_t) / 2
    
    # Update the original matrix with the symmetric one in the list
    matrices_list[[matrix_name]] <- mat_fin
  }
  
  # Return the updated list with symmetric matrices
  return(matrices_list)
}

## Step 2D - 6: make the final matrices symmetrical - from list.
updated_matrices_280x280_sim <- f_make_matrices_list_symmetric(updated_matrices_280x280_wDiag)

## Step 2D - 7: Generate the category specific matrices from the imputed versions. 
## Pass to a list with final names. 
l_imp_mat_eng_AD <- list()

# Loop through the updated_matrices_280x280_sim to rename and add to the new list
for (i in 1:length(updated_matrices_280x280_sim)) {
  # Construct the new name based on the index
  new_name <- paste("Behavioral imputed matrix - all data - English", i, sep = " ")
  
  # Add the matrix to the new list with the new name as the list name
  l_imp_mat_eng_AD[[new_name]] <- updated_matrices_280x280_wDiag[[i]]
}
#View(l_imp_mat_eng_AD$`Behavioral imputed matrix - all data - English 1`)

## Plot of the general matrices after completion: 

## define the function to plot it: 
plot_correlation_distance <- function(matrices_list) {
  # Ensure the lattice package is available
  require(lattice)
  
  # Initialize a list to store the plots
  plots_list <- list()
  
  for (matrix_name in names(matrices_list)) {
    # Extract the current matrix
    matrix <- matrices_list[[matrix_name]]
    
    # Compute the correlation distance
    dmat <- 1 - cor(matrix)
    
    # Create a heatmap of the distances
    plot_result <- levelplot(dmat,
                             col.regions = colorRampPalette(c('blue', '#E69F00'))(100),
                             main = paste("Correlation Distance for", matrix_name),
                             scales = list(x = list(labels = NA), y = list(labels = NA)))
    
    # Store the plot with the matrix name as the key
    plots_list[[matrix_name]] <- plot_result
  }
  
  return(plots_list)
}

## Step 2D - 7: plot the matrices. 
pl_imp_eng_AD_corr_dist <- plot_correlation_distance(l_imp_mat_eng_AD)
pl_imp_eng_AD_corr_dist$`Behavioral imputed matrix - all data - English 1`
l_CorrDis_levelplots$completed_matrices_eng

### USE THIS TO COMPARE BEFORE AND AFTER MICE AND WITH THE <5 VALUE RULE.
## COUNT NA values in the real vs the imputed ones TO CONFIRM all of them are complete 
#sum(is.na(mat_sim_eng_Ad)) ## 13494 NA values - 17% 
#sum(is.na(updated_matrices$mat_sim_eng_Ad_imp_diag_1)) ## 12958 NA values - 16%


### Step 3. add together the matrices for objective dimensions without general similarity ###

## Define the categories you want to combine for each language ##
combine_eng_categories <- c('color','category','shape')  
combine_esp_categories <- c('color', 'categorías', 'forma') 
  
## Initialize accumulator matrices with zeros ##
accumulator_matrix_eng <- matrix(NA, nrow = 280, ncol = 280) #nrow(eng_matrix), ncol = ncol(eng_matrix))
accumulator_matrix_esp <- matrix(NA, nrow = 280, ncol = 280)
  
## Initialize variables to store labels ##
eng_labels <- NULL
esp_labels <- NULL
  
## Loop over the pairs and accumulate matrices for English and Spanish based on conditions ##
for (pair in pairs) {
  eng_blocktype <- pair[1]
  esp_blocktype <- pair[2]
    
  eng_matrix <- completed_matrices_eng[[eng_blocktype]]
  esp_matrix <- completed_matrices_esp[[esp_blocktype]]
    
## Extract labels from the first encountered matrix in the specified categories ##
if (eng_blocktype %in% combine_eng_categories && is.null(eng_labels)) {
    eng_labels <- list(row = rownames(eng_matrix), col = colnames(eng_matrix))
  }
    
if (esp_blocktype %in% combine_esp_categories && is.null(esp_labels)) {
    esp_labels <- list(row = rownames(esp_matrix), col = colnames(esp_matrix))
  }
    
## Accumulate matrices for English and Spanish based on conditions ##
if (eng_blocktype %in% combine_eng_categories) {
    accumulator_matrix_eng <- f_add_matrices(accumulator_matrix_eng, eng_matrix)
  }
    
if (esp_blocktype %in% combine_esp_categories) {
    accumulator_matrix_esp <- f_add_matrices(accumulator_matrix_esp, esp_matrix)
  }
}
  
## Assign labels to the combined matrices ##
if (!is.null(eng_labels)) {
  rownames(accumulator_matrix_eng) <- eng_labels$row
  colnames(accumulator_matrix_eng) <- eng_labels$col
}
  
if (!is.null(esp_labels)) {
  rownames(accumulator_matrix_esp) <- esp_labels$row
  colnames(accumulator_matrix_esp) <- esp_labels$col
}
  
## Store the accumulator matrices after the loop ##
completed_matrices_eng[['combined_obj_eng']] <- accumulator_matrix_eng
completed_matrices_esp[['combined_obj_esp']] <- accumulator_matrix_esp
  
### step 4. add together matrices in english and spanish with the 280 function (not the extended - 282).###

## Now, combine between languages. ##
combined_matrices_with_combined_labels <- list()

## Loop over the pairs and combine the matrices ##
for (pair in pairs2) {
  eng_blocktype <- pair[1]
  esp_blocktype <- pair[2]
  
  eng_matrix <- completed_matrices_eng[[eng_blocktype]]
  esp_matrix <- completed_matrices_esp[[esp_blocktype]]
  
  combined_matrices[[eng_blocktype]] <- f_add_matrices(eng_matrix, esp_matrix)
  
  # Generate a "combined label" if needed
  combined_labels <- mapply(function(eng_label, esp_label) {
    # Extract the three parts of the label
    eng_parts <- unlist(strsplit(eng_label, "_"))
    esp_parts <- unlist(strsplit(esp_label, "_"))
    
    # If the second and third parts don't match, combine them
    if (eng_parts[2] != esp_parts[2] || eng_parts[3] != esp_parts[3]) {
      return(paste(eng_parts[1], paste(eng_parts[2], esp_parts[2], sep = "-"), 
                   paste(eng_parts[3], esp_parts[3], sep = "-"), sep = "_"))
    } else {
      return(eng_label)
    }
  }, rownames(eng_matrix), rownames(esp_matrix))
  
  rownames(combined_matrices[[eng_blocktype]]) <- combined_labels
  colnames(combined_matrices[[eng_blocktype]]) <- combined_labels
  
  combined_matrices_with_combined_labels[[eng_blocktype]] <- combined_matrices[[eng_blocktype]]
}

## Define mean lists for step 20 ##
l_mat_means_eng <- list()
l_mat_means_esp <- list()
l_mat_means_combined <- list()

## Verify that the dimensions of all the matrices are the same for step 5 ##
dim(completed_matrices_eng[[eng_blocktype]])
dim(completed_matrices_esp[[esp_blocktype]])    
dim(combined_matrices[[eng_blocktype]])

### Step 5. calculate the mean response for each category to assess better future distances within the matrix ####
l_mean_eng_matrices <- lapply(completed_matrices_eng, f_Mean_Values_cat)
l_mean_esp_matrices <- lapply(completed_matrices_esp, f_Mean_Values_cat)
l_mean_combined_matrices <- lapply(combined_matrices, f_Mean_Values_cat)


##################################################################################################################

##!! Warning: Once you do this step for the graphs, you can´t do step 18 again because the matrix does not have labels!!## 
  ## For graphical purposes: Change labels of matrices. 
  colnames(mat_reord3eng_compl_fun) <- NULL
  colnames(mat_reord3esp_compl_fun) <- NULL
  rownames(mat_reord3eng_compl_fun) <- NULL
  rownames(mat_reord3esp_compl_fun) <- NULL
  ## For graphical purposes: Change labels of matrices.
  colnames(mat_reord3eng_compl_fun2) <- NULL
  colnames(mat_reord3esp_compl_fun2) <- NULL
  rownames(mat_reord3eng_compl_fun2) <- NULL
  rownames(mat_reord3esp_compl_fun2) <- NULL
  
#################################################################################
####  Visualization, heatmaps and correlation for each list of matrices #########
#################################################################################

library(rasterVis)
library(raster)
library(gridExtra)
library(DT)
  
## Color palettes for color blindness ##
colorBlindBlack8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                         "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  
safe_colorblind_palette <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
                               "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")
#scales::show_col(safe_colorblind_palette)
  
#install.packages("rcartocolor")
#library(rcartocolor)
#display_carto_all(colorblind_friendly = TRUE)

## Categories names to be used in the matrices. 
category_names <- c("snake","Frog","Cockroach","Ants","spider","Worms","Bee","Butterflies","Birds","Bat","Chicken",
                   "Mouse","dog","Cat","Horses","Shark","Fish","Gecko","Turtle","Beatle","Grasshoper","Catterpillar",
                   "Fly","Peacock","Guinea pig","Sheep","Rabbit","Giraffe","Whale","Dolphin","Airplanes","Car","Bicycle",
                   "Scissor","Hammer","Key","Guitar","Cellphone","Umbrella","Chair") 

### Requirements for visualization functions ###
## Extract every 4th category name ##
selected_category_names <- category_names[seq(1, 40, by=4)]

# Repeat each word 7 times to get a vector of length 280
labels <- rep(category_names, each=7)

# Positions to display the labels on the Y axis
label_positions <- seq(14, 280, by=28)  # Start at 14 (middle of the first group of 4x7) and increment by 28

# Select only every 56th label (280/5) to get 5 labels
selected_x_labels <- labels[seq(1, 280, by = 56)]

# Function to compute raster and normalize it - Function to combine plots step 23#
compute_normalized_raster <- function(matrix) {
  matrix <- matrix / max(matrix)
  return(raster(matrix))
}

## ------------------------------------------------------------------------------------------------------ ##

### Step 6.  Heatmaps - mean distance for all 6 list of matrices ###

## Define a list of lists with all the matrices ##
all_lists <- list(
  completed_matrices_eng = completed_matrices_eng,
  completed_matrices_esp = completed_matrices_esp,
  combined_matrices = combined_matrices,
  l_mean_eng_matrices = l_mean_eng_matrices,  # 
  l_mean_esp_matrices = l_mean_esp_matrices,  # Assuming you've already computed means for the Spanish matrices
  l_mean_combined_matrices = l_mean_combined_matrices  # Assuming you've already computed means for the combined matrices
)

# Define a list for the distance matrices
l_distance_matrices <- list()
# List to store the levelplot graphs
l_meanDis_levelplots <- list()

## Loop through all lists and matrices ##
for (list_name in names(all_lists)) {
  
  # Initialize list for the current list_name
  l_meanDis_levelplots[[list_name]] <- list()
  
  for (blocktype in names(all_lists[[list_name]])) {
    
    matrix <- all_lists[[list_name]][[blocktype]]
    
    # Compute means for the current matrix
    cmeans <- apply(matrix, 2, mean)
    
    # Compute the distance between means
    dmat <- as.matrix(dist(cmeans))
    
    # Store the mean distance matrix with a combined key
    l_distance_matrices[[paste(list_name, blocktype, "1", sep = "_")]] <- dmat
    
    # Create a heatmap of the distances
    plot_result <- levelplot(dmat, 
                             col.regions = colorRampPalette(c('blue','#E69F00'))(100),
                             main = paste("Mean distance without", blocktype),
                             scales = list(x=list(labels=NA), y=list(labels=NA)))
    
    # Store the plot in the inner list with blocktype as the key
    l_meanDis_levelplots[[list_name]][[blocktype]] <- plot_result
  }
}

## View plots for each list_name in a 3x4 grid ##
for (list_name in names(l_meanDis_levelplots)) {
  cat(paste("Displaying plots for", list_name, "\n"))
  grid.arrange(grobs = l_meanDis_levelplots[[list_name]], ncol = 4, top = list_name)
}

### Step 7.  Heatmaps - Euclidean distance for all 6 list of matrices ### 
# List to store the levelplot graphs
l_EuclDis_levelplots <- list()
# Adjusted function to compute and return just the Euclidean distance
f_compute_Eucl_distance <- function(matrix) {
  return(as.matrix(dist(t(matrix))))
}

## Loop through all lists and matrices for Euclidean distance ##
for (list_name in names(all_lists)) {
  
  # Initialize list for the current list_name (for plots)
  l_EuclDis_levelplots[[list_name]] <- list()
  
  for (blocktype in names(all_lists[[list_name]])) {
    
    matrix <- all_lists[[list_name]][[blocktype]]
    
    # Compute the Euclidean distance for the current matrix
    dmat <- f_compute_Eucl_distance(matrix)
    
    # Store the euclidean distance matrix with a combined key
    l_distance_matrices[[paste(list_name, blocktype, "2", sep = "_")]] <- dmat
    
    # Create a heatmap of the distances
    plot_result <- levelplot(dmat, 
                             col.regions = colorRampPalette(c('blue','orange'))(100),
                             main = paste("Euclidean Distance without", blocktype),
                             scales = list(x=list(labels=NA), y=list(labels=NA)))
    
    # Store the plot in the inner list with blocktype as the key
    l_EuclDis_levelplots[[list_name]][[blocktype]] <- plot_result
  }
}
## View plots for each list_name in a 3x4 grid ##
for (list_name in names(l_EuclDis_levelplots)) {
  cat(paste("Displaying plots for", list_name, "\n"))
  grid.arrange(grobs = l_EuclDis_levelplots[[list_name]], ncol = 4, top = list_name)
}

### Step 8.  Heatmaps - Correlation distance for all 6 list of matrices ### 
# Initialize the list of lists to store plots
l_CorrDis_levelplots <- list()

## Loop through all lists and matrices ##
for (list_name in names(all_lists)) {
  
  # Initialize list for the current list_name
  l_CorrDis_levelplots[[list_name]] <- list()
  
  for (blocktype in names(all_lists[[list_name]])) {
    
    matrix <- all_lists[[list_name]][[blocktype]]
    
    # Compute the correlation distance for the current matrix
    dmat <- 1 - cor(matrix)
    
    # Store the correlation distance matrix with a combined key
    l_distance_matrices[[paste(list_name, blocktype, "3", sep = "_")]] <- dmat
    
    # Create a heatmap of the distances
    plot_result <- levelplot(dmat, 
                             col.regions = colorRampPalette(c('blue','#E69F00'))(100),
                             main = paste("Correlation Distance without", blocktype), 
                             scales = list(x=list(labels=NA), y=list(labels=NA)))
    
    # Store the plot in the inner list with blocktype as the key
    l_CorrDis_levelplots[[list_name]][[blocktype]] <- plot_result
  }
}

## Better visualization for each of the 6 lists. ##
# View plots for each list_name in a 3x3 grid
for (list_name in names(l_CorrDis_levelplots)) {
  cat(paste("Displaying plots for", list_name, "\n"))
  grid.arrange(grobs = l_CorrDis_levelplots[[list_name]], ncol = 3, top = list_name)
}

## Sanity-check on dimensions for all matrices ##
# Loop through the list and get the dimensions of each matrix
for (i in 1:length(l_distance_matrices)) {
  matrix_dim <- dim(l_distance_matrices[[i]])
  cat("Matrix", i, "has", matrix_dim[1], "rows and", matrix_dim[2], "columns\n")
}

#### Step 9: Compute the correlations scores between specific related matrices ####

## Define a function to compute pairwise Spearman correlations between matrices ##
# f_compute_correlation_matrix #  
compute_correlation_matrix <- function(matrix_list) {
  n <- length(matrix_list)
  correlation_matrix <- matrix(0, n, n)
  
  for (i in 1:n) {
    for (j in 1:n) {
      correlation_matrix[i, j] <- cor(as.vector(matrix_list[[i]]), as.vector(matrix_list[[j]]), method = 'spearman')
    }
  }
  
  colnames(correlation_matrix) <- names(matrix_list)
  rownames(correlation_matrix) <- names(matrix_list)
  return(correlation_matrix)
}

## Define the patterns to match the names ##
completed_combined_pattern_1 <- "^(completed|combined).*_1$"
l_mean_pattern_1 <- "^l_mean_(eng|esp|combined).*_1$"
completed_combined_pattern_2 <- "^(completed|combined).*_2$"
l_mean_pattern_2 <- "^l_mean_(eng|esp|combined).*_2$"
completed_combined_pattern_3 <- "^(completed|combined).*_3$"
l_mean_pattern_3 <- "^l_mean_(eng|esp|combined).*_3$"

## Filter the matrices based on the patterns ##
l_Meandist_matrices <- l_distance_matrices[grepl(completed_combined_pattern_1, names(l_distance_matrices))]
l_Meandist_catmean_matrices <- l_distance_matrices[grepl(l_mean_pattern_1, names(l_distance_matrices))]
l_Eucldist_matrices <- l_distance_matrices[grepl(completed_combined_pattern_2, names(l_distance_matrices))]
l_Eucldist_catmean__matrices <- l_distance_matrices[grepl(l_mean_pattern_2, names(l_distance_matrices))]
l_corrdist__matrices <- l_distance_matrices[grepl(completed_combined_pattern_3, names(l_distance_matrices))]
l_corrdist_catmean__matrices <- l_distance_matrices[grepl(l_mean_pattern_3, names(l_distance_matrices))]

## compute the correlation tables ##
Meandist_corr_table <- compute_correlation_matrix(l_Meandist_matrices)
Meandist_catmean_corr_table <- compute_correlation_matrix(l_Meandist_catmean_matrices)
Eucldist_corr_table <- compute_correlation_matrix(l_Eucldist_matrices)
Eucldist_catmean_corr_table <- compute_correlation_matrix(l_Eucldist_catmean__matrices)
corrdist_corr_table <- compute_correlation_matrix(l_corrdist__matrices)
corrdist_catmean_corr_table <- compute_correlation_matrix(l_corrdist_catmean__matrices)

View(l_corrdist__matrices$completed_matrices_eng_original_3)

## Generate a list with all the table names for loop in step 10 ##
table_names <- c("Meandist_corr_table", "Meandist_catmean_corr_table", 
                 "Eucldist_corr_table", "Eucldist_catmean_corr_table", 
                 "corrdist_corr_table", "corrdist_catmean_corr_table")

#### Step 10: Visualization - Color-coding the correlation tables - lower triangle ####

## New list - lower triangle values ##
simplified_tables <- list()

## Loop over the names of the original uncolored tables and store the modified tables in the new list ##
for (name in table_names) {
  # Get the table
  table <- as.matrix(get(name))
  
  # Set the upper triangle to NA
  table[upper.tri(table, diag = FALSE)] <- NA
  
  # Update the table in the new list
  simplified_tables[[name]] <- table
}

## set the names of the list elements as the original list ##
names(simplified_tables) <- table_names

## Coloring the simplified tables ## 
# empty list for colored simplified tables #
simplified_colored_tables <- list()

## Loop over the names of the simplified tables and store the colored tables in the new list ##
for (name in table_names) {
  table <- as.data.frame(simplified_tables[[name]])
  simplified_colored_tables[[name]] <- datatable(table, options = list(pageLength = 25), rownames = TRUE) %>% 
    formatStyle(names(table),
                backgroundColor = styleInterval(c(0.3, 0.5, 0.7), 
                                                c('lightcoral', 'lightsalmon', 'yellow', 'lightgreen')))
}

# set the names of the list elements as the simplified_tables list #
names(simplified_colored_tables) <- paste("Col_", table_names, sep = "")

## Calling tables ##
simplified_colored_tables[[1]]
simplified_colored_tables$Col_corrdist_catmean_corr_table

  
### --------------------------------------------------------------------------------------------------- ###
### ------------------------- SECTION 2: RSMs Per category/Type of Question  -------------------------- ###
### --------------------------------------------------------------------------------------------------- ###      

## recap of final datasets ##
# Simdata_engfilt  ---- # English data with 3 exclusion criteria
# Simdata_espfilt  ---- # Spanish data with 3 exclusion criteria
# Simdata_2imgfilt ---- # Combined 2 images dataset with 3 exlclusion criteria
 
## Personal comment: ##
## FIRST FILTER PER LANGUAGE BECAUSE THE FILLING DIMENSIONS WOULD BE MUCH HIGHER INSTEAD OF HAVING THEM TOGETHER ##
## ONCE I PROCESSED THEM SEPARATED, THEN COMBINED THEM AS BEFORE  ## 

## Prerequisites: Datasets list and matching block types ## 
datasets <- list(Simdata_engfilt,Simdata_espfilt,Simdata_2imgfilt)
# list of blocks - matched pairs # 
Block_types <- list('General Similarity','color',c('shape','forma'),c('animacy','animación'),c('scare','asustan'),
                      c('dissimilar','disimilaridad'),c('category','categorías'), c('preference','prefieres'))


###### **Prerequisite function** for step 1 that filters the dataframes   ##############
########################################################################################

## Function that creates dataframes per category/blocktype with verified output ##
f_generate_dataframes_by_blocktype <- function(data, data_name) {
  unique_blocktypes <- unique(data$BlockType)
  filtered_dataframes <- list()
  
  # Step 1: Initial splitting of the dataframe by blocktype
  for (blocktype in unique_blocktypes) {
    df_filtered <- subset(data, BlockType == blocktype)
    df_filtered$Dataset <- data_name
    
    # Append the filtered dataframe to the list
    filtered_dataframes[[blocktype]] <- df_filtered
  }
  
  # Step 2: Iterating through each filtered dataframe to perform checks
  for (blocktype in unique_blocktypes) {
    # Select the current dataframe
    current_df <- filtered_dataframes[[blocktype]]
    
    # Check if any 'run_id' has occurred more than 45 times
    id_counts <- table(current_df$run_id)  # Assuming 'run_id' identifies your subjects
    
    if (any(id_counts > 45)) {
      # Step 3: Handle the issue. Here, we print a warning - you could also stop the function, or take other action
      warning(paste("The dataframe for blocktype '", blocktype, "' has subjects with more than 45 trials.", sep = ""))
      
      # If you want to remove the dataframe from the list, uncomment the next line
      # filtered_dataframes[[blocktype]] <- NULL
    }
    else print("The dataframe does not have subjects with more than 45 trials.")
  }
  
  # Final step: Returning the list of dataframes, which have all been checked
  return(filtered_dataframes)
}

#### Step 1: Filter the dataframes per category ####
l_df_single_cat_esp <- f_generate_dataframes_by_blocktype(Simdata_espfilt,"Simdata_espfilt")
l_df_single_cat_eng <- f_generate_dataframes_by_blocktype(Simdata_engfilt,"Simdata_engfilt")

#### Step 2: create symmetric similarity matrices for one dimension ####
### English ###
l_mat_unicat_eng <- list()

## Loop through the list of filtered dataframes ##
for (blocktype in names(l_df_single_cat_eng)) {
  # Get the current dataframe
  current_df <- l_df_single_cat_eng[[blocktype]]
  
  # Apply your function to create a similarity matrix from the current dataframe
  # Assuming 'LeftImg' and 'RightImg' are column names for your 'current_df'
  current_similarity_matrix <- f_create_similarity_matrix(current_df, 'LeftImg', 'RightImg', matrix_name = blocktype)
  
  # Store the generated similarity matrix in the list, with a name corresponding to the blocktype
  l_mat_unicat_eng[[blocktype]] <- current_similarity_matrix
  
  indices_of_2 <- which(current_similarity_matrix$frequency_matrix == 2, arr.ind = TRUE)
  
  if (nrow(indices_of_2) > 0) {
    print(paste("For matrix", blocktype, "cells with value of 2 are:"))
    print(indices_of_2)
  } else {
    print(paste("For matrix", blocktype, "- Matrix full binary"))
  }
}

### Spanish ###
l_mat_unicat_esp <- list()

## Loop through the list of filtered dataframes ##
for (blocktype in names(l_df_single_cat_esp)) {
  # Get the current dataframe
  current_df <- l_df_single_cat_esp[[blocktype]]
  
  # Apply your function to create a similarity matrix from the current dataframe
  # Assuming 'LeftImg' and 'RightImg' are column names for your 'current_df'
  current_similarity_matrix <- f_create_similarity_matrix(current_df, 'LeftImg', 'RightImg', matrix_name = blocktype)
  
  # Store the generated similarity matrix in the list, with a name corresponding to the blocktype
  l_mat_unicat_esp[[blocktype]] <- current_similarity_matrix
  
  indices_of_2 <- which(current_similarity_matrix$frequency_matrix == 2, arr.ind = TRUE)
  
  if (nrow(indices_of_2) > 0) {
    print(paste("For matrix", blocktype, "cells with value of 2 are:"))
    print(indices_of_2)
  } else {
    print(paste("For matrix", blocktype, "- Matrix full binary"))
  }
}

#### Step 3 - Opt A: Compute the mean matrix based on the current values and see the completion rate ####    
### English ###
l_com_mat_unicat_eng <- list()

for (blocktype in names(l_mat_unicat_eng)) {
  
  # Access the names per blocktype (containing similarity and frequency matrix)
  #matrix_list <- l_mat_unicat_eng[[blocktype]][["similarity_matrix"]]
  current_mat <- l_mat_unicat_eng[[blocktype]][["similarity_matrix"]]
  # Number of categories and exemplars
  num_categories <- 40  # Since you mentioned there are 40 categories
  exemplars <- 7        # Each category has 7 exemplars
  
  # Create an empty matrix for the means
  means_matrix <- matrix(0, nrow = num_categories, ncol = num_categories)
  
  # Set the row names (which seems to be automatically done given your context)
  rownames(means_matrix) <- 1:num_categories 
  
  # We will calculate the mean for each 7x7 block within the matrix.
  # These blocks are defined by the categories.
  for (i in 1:num_categories) {
    for (j in 1:num_categories) {
      # Define the rows and columns that constitute the submatrix for the current block
      row_inds <- ((i - 1) * exemplars + 1):(i * exemplars)
      col_inds <- ((j - 1) * exemplars + 1):(j * exemplars)
      
      # Extract the submatrix
      sub_mat <- current_mat[row_inds, col_inds]
      
      # Compute the mean for the submatrix
      block_mean <- mean(sub_mat, na.rm = TRUE)
      
      # Store this mean in the means matrix
      means_matrix[i, j] <- block_mean
      #Output comment: 'means_matrix' contains the mean values calculated for each 7x7 block in the original matrix.#
    }
  }
  # After filling the matrix, set the column names based on the row names
  colnames(means_matrix) <- rownames(means_matrix)  # Ensuring symmetry in labeling
  
  # Count the number of NA values in the current matrix
  na_count <- sum(is.na(means_matrix))
  cat(paste("the NA count is:", na_count,"\n"))
  
  # Calculate the proportion of NA values over the entire matrix
  total_values <- length(means_matrix)
  proportion_na <- na_count / total_values
  
  # Print the proportion of NAs
  cat(paste("In the", blocktype, "matrix, the proportion of NA values is:", proportion_na, "\n"))
  
  # Store the means matrix in the list
  l_com_mat_unicat_eng[[blocktype]] <- means_matrix
}

View(l_com_mat_unicat_eng$`General Similarity`)

### Spanish ###
l_com_mat_unicat_esp <- list()

for (blocktype in names(l_mat_unicat_esp)) {
  
  # Access the similarity matrix per blocktype 
  current_mat <- l_mat_unicat_esp[[blocktype]][["similarity_matrix"]]
  # Number of categories and exemplars
  num_categories <- 40  
  exemplars <- 7      
  
  # Create an empty matrix for the means
  means_matrix <- matrix(0, nrow = num_categories, ncol = num_categories)
  
  # Set the row names (which seems to be automatically done given your context)
  rownames(means_matrix) <- 1:num_categories 
  
  # We will calculate the mean for each 7x7 block within the matrix.
  # These blocks are defined by the categories.
  for (i in 1:num_categories) {
    for (j in 1:num_categories) {
      # Define the rows and columns that constitute the submatrix for the current block
      row_inds <- ((i - 1) * exemplars + 1):(i * exemplars)
      col_inds <- ((j - 1) * exemplars + 1):(j * exemplars)
      
      # Extract the submatrix
      sub_mat <- current_mat[row_inds, col_inds]
      
      # Compute the mean for the submatrix
      block_mean <- mean(sub_mat, na.rm = TRUE)
      
      # Store this mean in the means matrix
      means_matrix[i, j] <- block_mean
      #Output comment: 'means_matrix' contains the mean values calculated for each 7x7 block in the original matrix.#
    }
  }
  # After filling the matrix, set the column names based on the row names
  colnames(means_matrix) <- rownames(means_matrix)  # Ensuring symmetry in labeling
  
  # Count the number of NA values in the current matrix
  na_count <- sum(is.na(means_matrix))
  cat(paste("the NA count is:", na_count,"\n"))
  
  # Calculate the proportion of NA values over the entire matrix
  total_values <- length(means_matrix)
  proportion_na <- na_count / total_values
  
  # Print the proportion of NAs
  cat(paste("In the", blocktype, "matrix, the proportion of NA values is:", proportion_na, "\n"))
  
  # Store the means matrix in the list
  l_com_mat_unicat_esp[[blocktype]] <- means_matrix
}

View(l_com_mat_unicat_esp$`General Similarity`)


## Step 3 - Opt B: fill up matrices - ML algorithm   
## Step 3 - Opt C: fill up the matrices with mean values for each category   
mat_eng_gensim_compl_fun <- f_complete_matrix(mat_eng_gensim_fun) # English   
mat_esp_gensim_compl_fun <- f_complete_matrix(mat_esp_gensim_fun) # Spanish 
## Find out which are the NA values
na_values <- which(is.na(mat_reord3esp_compl_fun), arr.ind = TRUE)
(na_values)


## Step 4. add together the matrices in english and spanish with both functions of addition. 
mat_comb_gensim_fin <- f_add_matrices(mat_eng_gensim_compl_fun,mat_esp_gensim_compl_fun)   #Result 280x280mat since not specified in function if diff dimensions# 
mat_comb_gensim_fin_ext <- f_add_matrices_ext(mat_eng_gensim_compl_fun,mat_esp_gensim_compl_fun) #Result 282x282 taking the 2 extra from spanish mat # 

## 4.1 Rename the matrix dimensions. 
dimnames(mat_comb_gensim_fin) <- dimnames(mat_eng_gensim_compl_fun)
dimnames(mat_comb_gensim_fin_ext) <- dimnames(mat_reord3esp_compl_fun)
  
## Step 5. correlation distance
dmatcomb_gensim1 <- 1-cor(mat_comb_gensim_fin)
levelplot(dmatcomb_gensim1, col.regions = colorRampPalette(c('blue','#E69F00'))(100)) 
  

### Color Block ### 
Simdata_eng_block_col <- Simdata_engfilt %>%  filter(BlockType == 'color')  # English
Simdata_esp_block_col <- Simdata_espfilt %>%  filter(BlockType == 'color')  # Spanish
# Confirmation #
# English
id_counts_eng_color <- table(Simdata_eng_block_col$run_id)  
sum(id_counts_eng_color[duplicated(Simdata_eng_block_col$run_id)]) > 45
# Spanish
id_counts_esp_block1 <- table(Simdata_esp_block_1$run_id) 
sum(id_counts_esp_block1[duplicated(Simdata_esp_block_1$run_id)]) > 45 
    
    
  ## Color ##
    Simdata_color1 <- Simdata_2imgfilt %>%  filter(BlockType == 'color')   
  # Confirmation #
    table(Simdata_color1$run_id)
    sum(Simdata_color1$subject_id > 45)
    
  ## Shape / forma ##
    Simdata_shape1 <- Simdata_2imgfilt %>%  filter(BlockType == 'shape'| BlockType == 'forma')   
  # Confirmation #
    table(Simdata_shape1$run_id)
    sum(Simdata_shape1$subject_id > 45)
    
  ## Animacy / animación ##
    Simdata_animacy1 <- Simdata_2imgfilt %>%  filter(BlockType == 'animación'| BlockType == 'animacy')   
  # Confirmation #
    table(Simdata_animacy1$run_id)
    sum(Simdata_animacy1$subject_id > 45)
    
  ## dissimilar / disimilaridad ##
    Simdata_dissimilar1 <- Simdata_2imgfilt %>%  filter(BlockType == 'dissimilar'| BlockType == 'disimilaridad')   
  # Confirmation #
    table(Simdata_dissimilar1$run_id)
    sum(Simdata_dissimilar1$subject_id > 45)
    
  ## category / categorías ##
    Simdata_category1 <- Simdata_2imgfilt %>%  filter(BlockType == 'category'| BlockType == 'categorías')   
  # Confirmation #
    table(Simdata_category1$run_id)
    sum(Simdata_category1$subject_id > 45)
    
  ## scare / asustan ##
    Simdata_scare1 <- Simdata_2imgfilt %>%  filter(BlockType == 'scare'| BlockType == 'asustan')   
  # Confirmation #
    table(Simdata_scare1$run_id)
    sum(Simdata_scare1$subject_id > 45)
    
  ## preference / prefieres ##
    Simdata_preference1 <- Simdata_2imgfilt %>%  filter(BlockType == 'preference'| BlockType == 'prefieres')   
  # Confirmation #
    table(Simdata_preference1$run_id)
    sum(Simdata_preference1$subject_id > 45)


    