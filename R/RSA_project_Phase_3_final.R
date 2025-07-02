
# RSA project - Similarity task (phase 3)   
# written by Alejandro Gamba - 2024 - PAC LAB

## Description of the data: 
## Around 200 participants in each language (spanish,english and Japanese) provided their Similarity ratings    
## for 8 different types of questions over 40 different categories of either animals (30) or objects (10).      
## The data was collected online and 280 images were used based on a previous prototypical behavioral task where 
## they chose the most representative exemplars.                                                                
                                                                                                              
## The data has been analyzed such that I computed the behavioral matrices from the collected data (in phase 2) 
## from both: General and category specific matrices. Our collaborator Vincent Taschereau-Dumouchel has also    
## estimate the voxel-based RDM matrices from the fMRI data that will be imported and used here.                

----------------------------------------------------------------------------------------------------------------------
## Code Summary: (update this part!!) 


### --------------------------------------------------------------------------------------------------- ###
### -------------------------- Phase 3:  fMRI and Behavioral correlations ----------------------------- ###
### --------------------------------------------------------------------------------------------------- ### 

## ------------------------------------------------------------------------------------------
### -------------------------- SECTION 0: Pre-requisites and Uploading. ----------------- ###

##### Step 0.1: Set working directory ##### 
setwd("C:/Users/j.gambasegovia/Documents/Papeles Alejo UF/1 - RSA - R.Project/Behavioral tasks (1 and 2)/Phase 2 data/RSA_all_Behavioral_matrices/R_lists_of_matrices")

##### Step 0.2: Define category names (for 280x280 matrices) 
category_names <- c("snake","Frog","Cockroach","Ants","spider","Worms","Bee","Butterflies","Birds","Bat","Chicken",
                    "Mouse","dog","Cat","Horses","Shark","Fish","Gecko","Turtle","Beatle","Grasshoper","Catterpillar",
                    "Fly","Peacock","Guinea pig","Sheep","Rabbit","Giraffe","Whale","Dolphin","Airplanes","Car","Bicycle",
                    "Scissor","Hammer","Key","Guitar","Cellphone","Umbrella","Chair") 

##### Step 0.3: Load behavioral raw matrices ##### 
## 280x280 behavioral raw matrices ##
l_final_matrices_280x280_2 <- readRDS("l_final_matrices_280x280_2.rds")

## 40x40 behavioral raw matrices ##
l_final_matrices_40x40_2 <- readRDS("l_final_matrices_40x40_2.rds")

##### Step 0.4: Load behavioral correlational distance matrices #####

## Load the 280x280 matrices list from the file ##
l_cor_distance_matrices_280x280 <- readRDS("l_cor_distance_matrices_280x280.rds")
## Load the 40x40 matrices list from the file ##
l_cor_distance_matrices_40x40 <- readRDS("l_cor_distance_matrices_40x40.rds")

##### Step 0.5: Load fMRI matrices (Python-based) #####
#install.packages("reticulate")
library(reticulate)

## Use an isolated Anaconda Python path ##
use_python("C:/Users/j.gambasegovia/anaconda3/envs/Env_RSA_project_Rstudio/python.exe") ## Numpy package required in Python. 

## Import the necessary Python modules ##
pickle <- import("pickle")
builtins <- import_builtins()

## Define the file path (since working directory is already set, we only need the file name) ##
file_path <- "RSAs_All_40by40.pkl"

## Open the .pkl file and load the data using Python's built-in open function ##
f <- builtins$open(file_path, "rb")
l_fmri_data <- pickle$load(f)
f$close()

##### Step 0.6: Define color palette #####
palette_5colors <- colorRampPalette(c('blue', 'cyan', 'yellow', 'orange', 'red'))(100)


## P.C. the l_fmri_data consists in 6 elements: 
## RSA (40x40 - combined), RSA_eng (280x280), RSA_Spa (280x280), RSA_40_eng, RSA_40_spa, and animals order. 
## Also RSA is an array of [80 participants, 11 ROIS, 40, 40 dimensions],                                                   
## 11 ROIs: 'Occipital','fusiform','IT','MT','ParHipp','dlPFC','vmPFC','vlPFC','OFC','mPFC','Hippocampus'                                    


## ------------------------------------------------------------------------------------------
### -------------------------- SECTION 1: Pre-processing  ------------------------------- ###

##### Step 1: Extract and convert arrays into matrices #####

## Extract the matrices ##
RSA_40_comb <- l_fmri_data[[2]]  # First 40x40 matrix
RSA_280_eng <- l_fmri_data[[3]]  # First 280x280 matrix (English)
RSA_280_spa <- l_fmri_data[[4]]  # Second 280x280 matrix (Spanish)
RSA_40_spa <- l_fmri_data[[5]]   # Second 40x40 matrix (Spanish)
RSA_40_eng <- l_fmri_data[[6]]   # third 40x40 matrix (English)

## Get the dimensions using the shape attribute ##
rsa_shape <- dim(RSA_40_comb)  
print(rsa_shape)  # This should print the dimensions

## Extract the number of participants and ROIs ##
num_participants <- rsa_shape[[1]]
num_rois <- rsa_shape[[2]]

#### Function to convert a NumPy ndarray to an R matrix ####
f_convert_to_r_matrix <- function(np_array) {
  # Convert NumPy ndarray to R matrix using py_to_r
  r_matrix <- py_to_r(np_array)
  return(r_matrix)
}

#### Function to extract the inner matrix if additional dimmensions ####
f_extract_inner_matrix <- function(mat) {
  if (length(dim(mat)) == 4 && dim(mat)[1] == 1 && dim(mat)[2] == 1) {
    return(mat[1, 1, , ])
  }
  return(mat)
}

#### Function to extract matrices for all participants and ROIs (Modified to handle 5 items) ####
f_extract_all_matrices <- function(RSA_40_comb, RSA_40_eng, RSA_40_spa, RSA_280_eng, RSA_280_spa, num_participants, num_rois) {
  l_mat_fMRI_data <- list()
  
  for (participant_index in 0:(num_participants - 1)) {  # Adjusted to 0-based indexing
    for (roi_index in 0:(num_rois - 1)) {  # Adjusted to 0-based indexing
      
      # Extract and convert 40x40 matrix (combined)
      matrix_40x40_comb <- RSA_40_comb[participant_index + 1, roi_index + 1, , , drop=FALSE]
      matrix_40x40_comb_r <- f_convert_to_r_matrix(matrix_40x40_comb)
      matrix_name_40x40_comb <- paste0("part_", participant_index + 1, "_ROI_", roi_index + 1, "_40x40_comb")
      l_mat_fMRI_data[[matrix_name_40x40_comb]] <- matrix_40x40_comb_r
      
      # Extract and convert 40x40 matrix (English)
      matrix_40x40_eng <- RSA_40_eng[participant_index + 1, roi_index + 1, , , drop=FALSE]
      matrix_40x40_eng_r <- f_convert_to_r_matrix(matrix_40x40_eng)
      matrix_name_40x40_eng <- paste0("part_", participant_index + 1, "_ROI_", roi_index + 1, "_40x40_eng")
      l_mat_fMRI_data[[matrix_name_40x40_eng]] <- matrix_40x40_eng_r
      
      # Extract and convert 40x40 matrix (Spanish)
      matrix_40x40_spa <- RSA_40_spa[participant_index + 1, roi_index + 1, , , drop=FALSE]
      matrix_40x40_spa_r <- f_convert_to_r_matrix(matrix_40x40_spa)
      matrix_name_40x40_spa <- paste0("part_", participant_index + 1, "_ROI_", roi_index + 1, "_40x40_spa")
      l_mat_fMRI_data[[matrix_name_40x40_spa]] <- matrix_40x40_spa_r
      
      # Extract and convert 280x280 matrix (English)
      matrix_280x280_eng <- RSA_280_eng[participant_index + 1, roi_index + 1, , , drop=FALSE]
      matrix_280x280_eng_r <- f_convert_to_r_matrix(matrix_280x280_eng)
      matrix_name_280x280_eng <- paste0("part_", participant_index + 1, "_ROI_", roi_index + 1, "_280x280_eng")
      l_mat_fMRI_data[[matrix_name_280x280_eng]] <- matrix_280x280_eng_r
      
      # Extract and convert 280x280 matrix (Spanish)
      matrix_280x280_spa <- RSA_280_spa[participant_index + 1, roi_index + 1, , , drop=FALSE]
      matrix_280x280_spa_r <- f_convert_to_r_matrix(matrix_280x280_spa)
      matrix_name_280x280_spa <- paste0("part_", participant_index + 1, "_ROI_", roi_index + 1, "_280x280_spa")
      l_mat_fMRI_data[[matrix_name_280x280_spa]] <- matrix_280x280_spa_r
    }
  }
  return(l_mat_fMRI_data)
}

## Extract all matrices and store them in a list ##
l_mat_fMRI_data <- f_extract_all_matrices(RSA_40_comb, RSA_40_eng, RSA_40_spa, RSA_280_eng, RSA_280_spa, num_participants, num_rois)
## Check the structure of the list ##
#str(l_mat_fMRI_data)
#View(l_mat_fMRI_data)

##### Step 2: Remove participants 81–88 with NaN/patient data ######
l_filt_mat_fMRI_data <- l_mat_fMRI_data[!grepl("part_(81|82|83|84|85|86|87|88)_", names(l_mat_fMRI_data))]
## Check the structure of the filtered list ##
#str(l_filt_mat_fMRI_data)
#View(l_filt_mat_fMRI_data)

##### Step 3: Generate the average fMRI matrices across participants #####

## Separate matrices by type ##   # 880 matrices (80 participants x 11 ROIs)
l_matrices_40_comb <- list()    # Combined 40x40 matrices 
l_matrices_40_eng <- list()     # English 40x40 matrices
l_matrices_40_spa <- list()     # Spanish 40x40 matrices
l_matrices_280_eng <- list()   # English 280x280 matrices
l_matrices_280_spa <- list()   # Spanish 280x280 matrices

## Iterate over all matrices and separate them into the corresponding list ##
for (name in names(l_filt_mat_fMRI_data)) {
  if (grepl("40x40_comb", name)) {
    l_matrices_40_comb[[name]] <- l_filt_mat_fMRI_data[[name]]
  } else if (grepl("40x40_eng", name)) {
    l_matrices_40_eng[[name]] <- l_filt_mat_fMRI_data[[name]]
  } else if (grepl("40x40_spa", name)) {
    l_matrices_40_spa[[name]] <- l_filt_mat_fMRI_data[[name]]
  } else if (grepl("280x280_eng", name)) {
    l_matrices_280_eng[[name]] <- l_filt_mat_fMRI_data[[name]]
  } else if (grepl("280x280_spa", name)) {
    l_matrices_280_spa[[name]] <- l_filt_mat_fMRI_data[[name]]
  }
}

#### Function to calculate the average matrix ####
f_average_matrix <- function(matrices) {
  num_matrices <- length(matrices)
  if (num_matrices == 0) {
    return(NULL)
  }
  
  # Ensure all matrices have the same dimensions
  first_matrix <- f_extract_inner_matrix(matrices[[1]])
  matrix_dim <- dim(first_matrix)
  cat("Expected matrix dimensions:", matrix_dim, "\n")
  
  for (name in names(matrices)) {
    mat <- f_extract_inner_matrix(matrices[[name]])
    if (!all(dim(mat) == matrix_dim)) {
      cat("Matrix", name, "has different dimensions:", dim(mat), "\n")
      stop("Matrices have different dimensions")
    }
  }
  
  sum_matrix <- matrix(0, nrow = matrix_dim[1], ncol = matrix_dim[2])
  
  matrix_labels <- names(matrices)
  
  for (name in names(matrices)) {
    mat <- f_extract_inner_matrix(matrices[[name]])
    sum_matrix <- sum_matrix + mat
  }
  
  avg_matrix <- sum_matrix / num_matrices
  
  # Print the number of matrices used and their labels
  cat("Number of matrices used:", num_matrices, "\n")
  cat("Matrix labels used:\n")
  print(matrix_labels)
  
  return(avg_matrix)
}

## Calculate the average matrices ##
m_avg_part_40_comb <- f_average_matrix(l_matrices_40_comb)      # Combined 40x40
m_avg_part_40_eng <- f_average_matrix(l_matrices_40_eng)        # English 40x40
m_avg_part_40_spa <- f_average_matrix(l_matrices_40_spa)        # Spanish 40x40
m_avg_part_280_eng <- f_average_matrix(l_matrices_280_eng)      # English 280x280
m_avg_part_280_spa <- f_average_matrix(l_matrices_280_spa)      # Spanish 280x280

# Combine English and Spanish averages to get a combined matrix for 280x280
m_avg_part_280_comb <- (m_avg_part_280_eng + m_avg_part_280_spa) / 2
## View the matrices ##
#View(m_avg_part_40_comb)

##### Step 4: Separate matrices by ROI and compute per-ROI averages #####

## Define ROI names ##
roi_names <- c('Occipital', 'fusiform', 'IT', 'MT', 'ParHipp', 'dlPFC',
               'vmPFC', 'vlPFC', 'OFC', 'mPFC', 'Hippocampus')

## Separate matrices by type and ROI ##
l_roi_matrices_40_comb <- setNames(vector("list", 11), roi_names)  # Combined 40x40 matrices
l_roi_matrices_40_eng <- setNames(vector("list", 11), roi_names)   # English 40x40 matrices
l_roi_matrices_40_spa <- setNames(vector("list", 11), roi_names)   # Spanish 40x40 matrices
l_roi_matrices_280_eng <- setNames(vector("list", 11), roi_names)  # English 280x280 matrices
l_roi_matrices_280_spa <- setNames(vector("list", 11), roi_names)  # Spanish 280x280 matrices
l_roi_matrices_280_comb <- setNames(vector("list", 11), roi_names) # Combined average of 280x280 matrices (English + Spanish)

## Initialize lists for each ROI ##
for (i in 1:11) {
  l_roi_matrices_40_comb[[i]] <- list()
  l_roi_matrices_40_eng[[i]] <- list()
  l_roi_matrices_40_spa[[i]] <- list()
  l_roi_matrices_280_eng[[i]] <- list()
  l_roi_matrices_280_spa[[i]] <- list()
  l_roi_matrices_280_comb[[i]] <- list()
}

## Assign matrices to the corresponding ROI lists ##
for (name in names(l_filt_mat_fMRI_data)) {
  roi_index <- as.numeric(gsub(".*ROI_(\\d+)_.*", "\\1", name))  # Extract the ROI index from the name
  
  if (grepl("40x40_comb", name)) {
    l_roi_matrices_40_comb[[roi_index]][[name]] <- l_filt_mat_fMRI_data[[name]]
  } else if (grepl("40x40_eng", name)) {
    l_roi_matrices_40_eng[[roi_index]][[name]] <- l_filt_mat_fMRI_data[[name]]
  } else if (grepl("40x40_spa", name)) {
    l_roi_matrices_40_spa[[roi_index]][[name]] <- l_filt_mat_fMRI_data[[name]]
  } else if (grepl("280x280_eng", name)) {
    l_roi_matrices_280_eng[[roi_index]][[name]] <- l_filt_mat_fMRI_data[[name]]
  } else if (grepl("280x280_spa", name)) {
    l_roi_matrices_280_spa[[roi_index]][[name]] <- l_filt_mat_fMRI_data[[name]]
  }
}

## Combine and average English and Spanish matrices for each ROI in l_roi_matrices_280_comb ##
for (i in 1:11) {
  roi_name <- paste0("ROI_", i)
  
  # Loop through English matrices and find corresponding Spanish matrices
  for (name in names(l_roi_matrices_280_eng[[i]])) {
    # Define the expected Spanish matrix name by replacing '_eng' with '_spa'
    spanish_name <- gsub("_eng", "_spa", name)
    
    # Check if the Spanish matrix exists in l_roi_matrices_280_spa
    if (spanish_name %in% names(l_roi_matrices_280_spa[[i]])) {
      eng_matrix <- l_roi_matrices_280_eng[[i]][[name]][1, 1, , ]
      spa_matrix <- l_roi_matrices_280_spa[[i]][[spanish_name]][1, 1, , ]
      
      # Calculate the average
      combined_name <- gsub("_eng", "_comb", name)
      l_roi_matrices_280_comb[[i]][[combined_name]] <- (eng_matrix + spa_matrix) / 2
      
      # Confirm addition of the averaged matrix
      print(paste("Averaged matrix", combined_name, "added to ROI:", roi_name))
    } else {
      # If no matching Spanish matrix is found, output a message
      print(paste("No Spanish matrix found for", name, "in ROI:", roi_name))
    }
  }
}

#### Function to drop extra dimensions in matrices (from 4D to 2D) ####
f_convert_to_2d <- function(matrix_list) {
  lapply(matrix_list, function(mat) {
    lapply(mat, function(x) {
      if (length(dim(x)) == 4) {
        drop(x)  # Drop dimensions of size 1, converting [1, 1, 280, 280] to [280, 280]
      } else {
        x  # Leave matrices already in [280, 280] unchanged
      }
    })
  })
}

## Apply the conversion function to each list except l_roi_matrices_280_comb ##
l_roi_matrices_40_comb <- f_convert_to_2d(l_roi_matrices_40_comb)
l_roi_matrices_40_eng <- f_convert_to_2d(l_roi_matrices_40_eng)
l_roi_matrices_40_spa <- f_convert_to_2d(l_roi_matrices_40_spa)
l_roi_matrices_280_eng <- f_convert_to_2d(l_roi_matrices_280_eng)
l_roi_matrices_280_spa <- f_convert_to_2d(l_roi_matrices_280_spa)
# l_roi_matrices_280_comb is already in 2D.  

l_roi_matrices_fmri <- list(
  'l_roi_matrices_40_comb' = l_roi_matrices_40_comb, 
  'l_roi_matrices_40_eng' = l_roi_matrices_40_eng, 
  'l_roi_matrices_40_spa' = l_roi_matrices_40_spa,
  'l_roi_matrices_280_eng' =  l_roi_matrices_280_eng, 
  'l_roi_matrices_280_spa' = l_roi_matrices_280_spa, 
  'l_roi_matrices_280_comb' = l_roi_matrices_280_comb) 

l_roi_matrices_fmri_40 <- list(
  'l_roi_matrices_40_comb' = l_roi_matrices_40_comb, 
  'l_roi_matrices_40_eng' = l_roi_matrices_40_eng, 
  'l_roi_matrices_40_spa' = l_roi_matrices_40_spa
)
######

## Calculate the average matrices per ROI ## 
l_avg_mat_per_roi_40_comb <- setNames(lapply(l_roi_matrices_40_comb, f_average_matrix), roi_names)
l_avg_mat_per_roi_40_eng <- setNames(lapply(l_roi_matrices_40_eng, f_average_matrix), roi_names)
l_avg_mat_per_roi_40_spa <- setNames(lapply(l_roi_matrices_40_spa, f_average_matrix), roi_names)
l_avg_mat_per_roi_280_eng <- setNames(lapply(l_roi_matrices_280_eng, f_average_matrix), roi_names)
l_avg_mat_per_roi_280_spa <- setNames(lapply(l_roi_matrices_280_spa, f_average_matrix), roi_names)

## Combine English and Spanish averages to get a combined matrix per ROI for 280x280 matrices ##
l_avg_mat_per_roi_280_comb <- setNames(vector("list", 11), roi_names)
for (i in 1:11) {
  l_avg_mat_per_roi_280_comb[[roi_names[i]]] <- (l_avg_mat_per_roi_280_eng[[i]] + l_avg_mat_per_roi_280_spa[[i]]) / 2
}

##### Step 5: Behavioral Reliability estimation at the group level - 280x280 matrices ### 
### P.C. since reliability estimates are computed on raw data, applicable for all metrics in the attenuation correction: step 18 ### 

#### Step 5.1: Compute the reliability estimate across the 3 languages (English, Spanish and Combined) ####

## Define category vector ##
category_vector <- rep(1:40, each = 7)  # assuming stimuli 1–7 = category 1, 8–14 = category 2, etc.

#### function for estimate split-half reliability behavioral RDM ####
f_estimate_split_half_reliability <- function(behavioral_matrix, category_vector, n_splits = 1000, method = "spearman") {
  stopifnot(length(category_vector) == nrow(behavioral_matrix))  # matrix must be square and match category vector
  
  # Start timer
  start_time <- Sys.time()
  
  n_categories <- length(unique(category_vector))
  correlations <- numeric(n_splits)
  
  message("Starting split-half reliability estimation with ", n_splits, " iterations...")
  
  for (i in 1:n_splits) {
    # Optional: show progress every 100 iterations
    if (i %% 100 == 0 || i == 1) {
      message("Running split ", i, "/", n_splits, "...")
    }
    
    # Split the stimuli in each category
    split_A_indices <- c()
    split_B_indices <- c()
    
    for (cat in unique(category_vector)) {
      inds <- which(category_vector == cat)
      inds_split <- sample(inds, length(inds) %/% 2)
      split_A_indices <- c(split_A_indices, inds_split)
      split_B_indices <- c(split_B_indices, setdiff(inds, inds_split))
    }
    
    # Construct RDMs (40x40) for each split
    rdm_split <- function(indices) {
      rdm <- matrix(NA, n_categories, n_categories)
      for (i in 1:n_categories) {
        for (j in 1:n_categories) {
          items_i <- which(category_vector == i & 1:nrow(behavioral_matrix) %in% indices)
          items_j <- which(category_vector == j & 1:nrow(behavioral_matrix) %in% indices)
          submatrix <- behavioral_matrix[items_i, items_j]
          rdm[i, j] <- mean(submatrix, na.rm = TRUE)
        }
      }
      return(rdm)
    }
    
    rdm_A <- rdm_split(split_A_indices)
    rdm_B <- rdm_split(split_B_indices)
    
    # Vectorize and correlate
    vec_A <- rdm_A[lower.tri(rdm_A)]
    vec_B <- rdm_B[lower.tri(rdm_B)]
    correlations[i] <- suppressWarnings(cor(vec_A, vec_B, method = method, use = "complete.obs"))
  }
  
  # Spearman-Brown correction
  r_mean <- mean(correlations, na.rm = TRUE)
  r_sb <- (2 * r_mean) / (1 + r_mean)
  
  # End timer
  end_time <- Sys.time()
  duration <- difftime(end_time, start_time, units = "secs")
  message("Total execution time: ", round(duration, 2), " seconds.")
  
  return(list(
    mean_split_half = r_mean,
    spearman_brown = r_sb,
    all_correlations = correlations
  ))
}

## English matrix ##
relia_eng <- f_estimate_split_half_reliability(
  behavioral_matrix = l_final_matrices_280x280_2$Behavioral_matrix_eng_all_data_original,
  category_vector = category_vector
)
#str(res_eng)

## Spanish matrix ##
relia_spa <- f_estimate_split_half_reliability(
  behavioral_matrix = l_final_matrices_280x280_2$Behavioral_matrix_spa_all_data_original,
  category_vector = category_vector
)

## Combined matrix ##
relia_comb <- f_estimate_split_half_reliability(
  behavioral_matrix = l_final_matrices_280x280_2$Behavioral_matrix_comb_all_data_original,
  category_vector = category_vector
)

#### Step 5.2: Estimate and report confidence intervals for each ####

#### Function to estimate the confidence intervals of the reliability estimate
f_compute_reliability_CI <- function(correlations, conf_level = 0.95) {
  # Get quantiles
  alpha <- (1 - conf_level) / 2
  bounds <- quantile(correlations, probs = c(alpha, 1 - alpha), na.rm = TRUE)
  
  # Apply Spearman-Brown correction to both bounds
  sb_bounds <- (2 * bounds) / (1 + bounds)
  
  return(list(
    raw_CI = bounds,
    sb_CI = sb_bounds
  ))
}

## English ##
ci_eng <- f_compute_reliability_CI(relia_eng$all_correlations)
print(ci_eng)

## Spanish ##
ci_spa <- f_compute_reliability_CI(relia_spa$all_correlations)
print(ci_spa)

## Combined ##
ci_comb <- f_compute_reliability_CI(relia_comb$all_correlations)
print(ci_comb)

#### Step 5.3: Plot histograms to visualize distributions ####
library(ggplot2)

f_plot_reliability_distribution <- function(correlations, language = "English") {
  df <- data.frame(cor = correlations)
  ci <- f_compute_reliability_CI(correlations)$sb_CI
  
  ggplot(df, aes(x = cor)) +
    geom_histogram(aes(y = ..density..), bins = 50, fill = "skyblue", color = "white", alpha = 0.7) +
    geom_density(color = "blue", size = 1) +
    geom_vline(xintercept = ci, color = "red", linetype = "dashed", size = 1) +
    labs(title = paste("Split-Half Reliability (Spearman) -", language),
         subtitle = paste("95% CI of Spearman-Brown corrected:", round(ci[1], 3), "-", round(ci[2], 3)),
         x = "Split-half correlation",
         y = "Density") +
    theme_minimal()
}

## English ##
f_plot_reliability_distribution(relia_eng$all_correlations, language = "English")

## Spanish ##
f_plot_reliability_distribution(relia_spa$all_correlations, language = "Spanish")

## Combined ##
f_plot_reliability_distribution(relia_comb$all_correlations, language = "Combined")

###### Step 6: Transformation of matrices to pre-requisites for correlations and distances ######
#### Step 6.1 Correlation distance ####
### Step 6.1A: Transform fMRI averaged data to correlation distances ###
#### Function to convert correlation coefficients to correlation distances ####
f_convert_to_distances <- function(matrix) {
  return(1 - matrix)
}

## Transform lists of matrices (11 matrices per list; 66 total) ##
l_avg_mat_cor_dist_per_roi_40_comb <- lapply(l_avg_mat_per_roi_40_comb, f_convert_to_distances)
l_avg_mat_cor_dist_per_roi_40_eng <- lapply(l_avg_mat_per_roi_40_eng, f_convert_to_distances)
l_avg_mat_cor_dist_per_roi_40_spa <- lapply(l_avg_mat_per_roi_40_spa, f_convert_to_distances)
l_avg_mat_cor_dist_per_roi_280_eng <- lapply(l_avg_mat_per_roi_280_eng, f_convert_to_distances)
l_avg_mat_cor_dist_per_roi_280_spa <- lapply(l_avg_mat_per_roi_280_spa, f_convert_to_distances)

## Combine English and Spanish correlational distances for 280x280 matrices ##
l_avg_mat_cor_dist_per_roi_280_comb <- lapply(l_avg_mat_per_roi_280_comb, f_convert_to_distances)

### Step 6.1B: Behavioral data to Z-transformed correlational distances ###
#### Function to Z-score an individual matrix ####
f_z_score_transformation <- function(matrix) {
  return(scale(matrix, center = TRUE, scale = TRUE))
}

#### Function to convert Z-scored matrices to correlation coefficients ####
f_convert_to_correlation <- function(matrix_list) {
  lapply(matrix_list, function(matrix) {
    cor(as.matrix(matrix), use = "pairwise.complete.obs", method = "pearson")
  })
}
### raw behavioral ###
## Convert raw matrices to correlation coefficients ##
l_raw_correlation_behavioral_40x40 <- f_convert_to_correlation(l_final_matrices_40x40_2)
l_raw_correlation_behavioral_280x280 <- f_convert_to_correlation(l_final_matrices_280x280_2)

## Convert raw correlation coefficients to correlational distances ##
l_raw_cor_dist_behavioral_40x40 <- lapply(l_raw_correlation_behavioral_40x40, f_convert_to_distances)
l_raw_cor_dist_behavioral_280x280 <- lapply(l_raw_correlation_behavioral_280x280, f_convert_to_distances)

## use the raw cor dist to replace the behavioral matrices !!! ## 

## Steps for behavioral data: raw --> Z-score --> corr coeff --> corr distance! ##
## Apply Z-score transformation to raw matrices ##
z_scored_behavioral_40x40 <- lapply(l_final_matrices_40x40_2, f_z_score_transformation)
z_scored_behavioral_280x280 <- lapply(l_final_matrices_280x280_2, f_z_score_transformation)

## Convert Z-scored matrices to correlation coefficients ##
correlation_behavioral_40x40 <- f_convert_to_correlation(z_scored_behavioral_40x40)
correlation_behavioral_280x280 <- f_convert_to_correlation(z_scored_behavioral_280x280)

## Convert correlation coefficients to correlational distances ##
z_cor_dist_behavioral_40x40 <- lapply(correlation_behavioral_40x40, f_convert_to_distances)
z_cor_dist_behavioral_280x280 <- lapply(correlation_behavioral_280x280, f_convert_to_distances)
#View(z_cor_dist_behavioral_40x40$Behavioral_mean_matrix_eng_all_data_original)

### Step 6.1C: Behavioral raw data to rank-transformed correlational distances (for Spearman Rho) ###
#### Function to apply rank transformation to a matrix ####
f_rank_transformation <- function(matrix) {
  ranked_vector <- rank(as.vector(matrix))  # Rank the matrix as a single vector
  matrix(ranked_vector, nrow = nrow(matrix), ncol = ncol(matrix))  # Reshape back to original matrix dimensions
}

## Apply rank transformation to each matrix in the list ##
ranked_behavioral_40x40 <- lapply(l_final_matrices_40x40_2, f_rank_transformation)
ranked_behavioral_280x280 <- lapply(l_final_matrices_280x280_2, f_rank_transformation)

## Convert ranked matrices to correlation coefficients (using Spearman's rank correlation) ##
rank_correlation_behavioral_40x40 <- f_convert_to_correlation(ranked_behavioral_40x40)
rank_correlation_behavioral_280x280 <- f_convert_to_correlation(ranked_behavioral_280x280)

## Convert correlation coefficients to correlational distances ##
rank_cor_dist_behavioral_40x40 <- lapply(rank_correlation_behavioral_40x40, f_convert_to_distances)
rank_cor_dist_behavioral_280x280 <- lapply(rank_correlation_behavioral_280x280, f_convert_to_distances)

### rank_cor_dist_behavioral_40x40 & rank_cor_dist_behavioral_280x280 - Behav rank matrices ###  

### Step 6.1D: fMRI matrices to correlation distance ###
## Function to apply the correlational distance transformation across all nested lists ##
f_apply_to_nested_matrices <- function(nested_list, func) {
  lapply(nested_list, function(inner_list) {
    lapply(inner_list, func)
  })
}
## fMRI correlational distance - no transformation ##
l_cor_dist_matrices_40_fmri <- list(
  "l_roi_corr_matrices_40_comb_fmri" = f_apply_to_nested_matrices(l_roi_matrices_fmri$l_roi_matrices_40_comb, f_convert_to_distances),
  "l_roi_corr_matrices_40_eng_fmri" = f_apply_to_nested_matrices(l_roi_matrices_fmri$l_roi_matrices_40_eng, f_convert_to_distances),
  "l_roi_corr_matrices_40_spa_fmri" = f_apply_to_nested_matrices(l_roi_matrices_fmri$l_roi_matrices_40_spa, f_convert_to_distances)
)
l_cor_dist_matrices_280_fmri <- list(
  "l_roi_corr_matrices_280_eng_fmri" = f_apply_to_nested_matrices(l_roi_matrices_fmri$l_roi_matrices_280_eng, f_convert_to_distances),
  "l_roi_corr_matrices_280_spa_fmri" = f_apply_to_nested_matrices(l_roi_matrices_fmri$l_roi_matrices_280_spa, f_convert_to_distances),
  "l_roi_corr_matrices_280_comb_fmri" = f_apply_to_nested_matrices(l_roi_matrices_fmri$l_roi_matrices_280_comb, f_convert_to_distances)
)
# l_cor_dist_matrices_40_fmri & l_cor_dist_matrices_280_fmri : in corr distance - fMRI (no transformation added) #

## Z-transformed fMRI correlational distances ##
l_z_transformed_matrices_40_fmri <- list(
  "l_roi_z_matrices_40_comb_fmri" = f_apply_to_nested_matrices(l_roi_matrices_fmri$l_roi_matrices_40_comb, f_z_score_transformation),
  "l_roi_z_matrices_40_eng_fmri" = f_apply_to_nested_matrices(l_roi_matrices_fmri$l_roi_matrices_40_eng, f_z_score_transformation),
  "l_roi_z_matrices_40_spa_fmri" = f_apply_to_nested_matrices(l_roi_matrices_fmri$l_roi_matrices_40_spa, f_z_score_transformation)
)
## correlation distance - Z- transformed ##
l_cor_dist_z_matrices_40_fmri <- list(
  "l_roi_cor_dist_z_matrices_40_comb_fmri" = f_apply_to_nested_matrices(l_z_transformed_matrices_40_fmri$l_roi_z_matrices_40_comb_fmri, f_convert_to_distances),
  "l_roi_cor_dist_z_matrices_40_eng_fmri" = f_apply_to_nested_matrices(l_z_transformed_matrices_40_fmri$l_roi_z_matrices_40_eng_fmri, f_convert_to_distances),
  "l_roi_cor_dist_z_matrices_40_spa_fmri" = f_apply_to_nested_matrices(l_z_transformed_matrices_40_fmri$l_roi_z_matrices_40_spa_fmri, f_convert_to_distances)
)

### 280 x280 ###
l_z_transformed_matrices_280_fmri <- list(
  "l_roi_z_matrices_280_eng_fmri" = f_apply_to_nested_matrices(l_roi_matrices_fmri$l_roi_matrices_280_eng, f_z_score_transformation),
  "l_roi_z_matrices_280_spa_fmri" = f_apply_to_nested_matrices(l_roi_matrices_fmri$l_roi_matrices_280_spa, f_z_score_transformation),
  "l_roi_z_matrices_280_comb_fmri" = f_apply_to_nested_matrices(l_roi_matrices_fmri$l_roi_matrices_280_comb, f_z_score_transformation)
)
l_cor_dist_z_matrices_280_fmri <- list(
  "l_roi_cor_dist_z_matrices_280_eng_fmri" = f_apply_to_nested_matrices(l_z_transformed_matrices_280_fmri$l_roi_z_matrices_280_eng_fmri, f_convert_to_distances),
  "l_roi_cor_dist_z_matrices_280_spa_fmri" = f_apply_to_nested_matrices(l_z_transformed_matrices_280_fmri$l_roi_z_matrices_280_spa_fmri, f_convert_to_distances),
  "l_roi_cor_dist_z_matrices_280_comb_fmri" = f_apply_to_nested_matrices(l_z_transformed_matrices_280_fmri$l_roi_z_matrices_280_comb_fmri, f_convert_to_distances)
)

## Rank-transformed fMRI correlational distances ##
l_ranked_matrices_40_fmri <- list(
  "l_roi_ranked_matrices_40_comb" = f_apply_to_nested_matrices(l_roi_matrices_fmri$l_roi_matrices_40_comb, f_rank_transformation),
  "l_roi_ranked_matrices_40_eng" = f_apply_to_nested_matrices(l_roi_matrices_fmri$l_roi_matrices_40_eng, f_rank_transformation),
  "l_roi_ranked_matrices_40_spa" = f_apply_to_nested_matrices(l_roi_matrices_fmri$l_roi_matrices_40_spa, f_rank_transformation)
)
l_cor_dist_ranked_matrices_40_fmri <- list(
  "l_roi_cor_dist_ranked_matrices_40_comb_fmri" = f_apply_to_nested_matrices(l_ranked_matrices_40_fmri$l_roi_ranked_matrices_40_comb, f_convert_to_distances),
  "l_roi_cor_dist_ranked_matrices_40_eng_fmri" = f_apply_to_nested_matrices(l_ranked_matrices_40_fmri$l_roi_ranked_matrices_40_eng, f_convert_to_distances),
  "l_roi_cor_dist_ranked_matrices_40_spa_fmri" = f_apply_to_nested_matrices(l_ranked_matrices_40_fmri$l_roi_ranked_matrices_40_spa, f_convert_to_distances)
)

### 280X280 ###
l_ranked_matrices_280_fmri <- list(
  "l_roi_ranked_matrices_280_eng" = f_apply_to_nested_matrices(l_roi_matrices_fmri$l_roi_matrices_280_eng, f_rank_transformation),
  "l_roi_ranked_matrices_280_spa" = f_apply_to_nested_matrices(l_roi_matrices_fmri$l_roi_matrices_280_spa, f_rank_transformation),
  "l_roi_ranked_matrices_280_comb" = f_apply_to_nested_matrices(l_roi_matrices_fmri$l_roi_matrices_280_comb, f_rank_transformation)
)
l_cor_dist_ranked_matrices_280_fmri <- list(
  "l_roi_cor_dist_ranked_matrices_280_eng" = f_apply_to_nested_matrices(l_ranked_matrices_280_fmri$l_roi_ranked_matrices_280_eng, f_convert_to_distances),
  "l_roi_cor_dist_ranked_matrices_280_spa" = f_apply_to_nested_matrices(l_ranked_matrices_280_fmri$l_roi_ranked_matrices_280_spa, f_convert_to_distances),
  "l_roi_cor_dist_ranked_matrices_280_comb" = f_apply_to_nested_matrices(l_ranked_matrices_280_fmri$l_roi_ranked_matrices_280_comb, f_convert_to_distances)
)

#### Step 6.2 Fisher Z-transformed correlation distance ####

### Step 6.2A: Define Fisher Z-transformation and distance functions ###
#### Fisher Z-transformation function ####
f_fisher_z_transformation <- function(matrix) {
  return(0.5 * log((1 + matrix) / (1 - matrix)))
}

#### Function to set the diagonal values to 0.999 ##
f_set_diagonal_values <- function(matrix) {
  diag(matrix) <- 0.999
  return(matrix)
}

#### Function to convert correlation matrices to correlational distance ####
f_convert_to_corr_distance <- function(matrix) {
  return(1 - matrix)
}

### Step 6.2B: Apply Fisher Z-transformation to fMRI averages ###
## Initialize new lists for the Fisher Z-transformed matrices ##
l_fisher_corr_distance_40x40 <- list()
l_fisher_corr_distance_280x280 <- list()

## Apply Fisher Z-transformation and correlational distance to lists of fMRI matrices ##
l_fisher_avg_mat_per_roi_40_comb <- lapply(l_avg_mat_per_roi_40_comb, function(matrix) {
  f_convert_to_corr_distance(f_fisher_z_transformation(f_set_diagonal_values(matrix)))
})
l_fisher_avg_mat_per_roi_40_eng <- lapply(l_avg_mat_per_roi_40_eng, function(matrix) {
  f_convert_to_corr_distance(f_fisher_z_transformation(f_set_diagonal_values(matrix)))
})
l_fisher_avg_mat_per_roi_40_spa <- lapply(l_avg_mat_per_roi_40_spa, function(matrix) {
  f_convert_to_corr_distance(f_fisher_z_transformation(f_set_diagonal_values(matrix)))
})
l_fisher_avg_mat_per_roi_280_eng <- lapply(l_avg_mat_per_roi_280_eng, function(matrix) {
  f_convert_to_corr_distance(f_fisher_z_transformation(f_set_diagonal_values(matrix)))
})
l_fisher_avg_mat_per_roi_280_spa <- lapply(l_avg_mat_per_roi_280_spa, function(matrix) {
  f_convert_to_corr_distance(f_fisher_z_transformation(f_set_diagonal_values(matrix)))
})
l_fisher_avg_mat_per_roi_280_comb <- lapply(l_avg_mat_per_roi_280_comb, function(matrix) {
  f_convert_to_corr_distance(f_fisher_z_transformation(f_set_diagonal_values(matrix)))
})

### Step 6.2C: Apply Fisher Z-transformation to behavioral matrices ###
## Convert Behavioral matrices to correlation coefficients ##
l_cor_final_matrices_40x40 <- f_convert_to_correlation(l_final_matrices_40x40_2)
l_cor_final_matrices_280x280 <- f_convert_to_correlation(l_final_matrices_280x280_2)

## Apply Fisher Z-transformation and correlational distance to behavioral matrices ##
l_cordist_fisher_40x40 <- lapply(l_cor_final_matrices_40x40, function(matrix) {
  f_convert_to_corr_distance(f_fisher_z_transformation(f_set_diagonal_values(matrix)))
})
l_cordist_fisher_280x280 <- lapply(l_cor_final_matrices_280x280, function(matrix) {
  f_convert_to_corr_distance(f_fisher_z_transformation(f_set_diagonal_values(matrix)))
})

### Step 6.2D: Apply recursive Fisher Z-transformation to ROI matrices ###
#### Combined transformation function for applying the three steps sequentially ####
f_apply_full_transformation <- function(matrix) {
  f_convert_to_corr_distance(f_fisher_z_transformation(f_set_diagonal_values(matrix)))
}

#### Recursive function to apply transformations to nested list of matrices ####
f_apply_to_nested_matrices <- function(nested_list, func) {
  lapply(nested_list, function(inner_list) {
    lapply(inner_list, func)
  })
}
## Fisher Z transformation for 40x40 matrices ##
l_fisher_matrices_40 <- list(
  "l_roi_fisher_matrices_40_comb" = f_apply_to_nested_matrices(l_roi_matrices_fmri$l_roi_matrices_40_comb, f_apply_full_transformation),
  "l_roi_fisher_matrices_40_eng" = f_apply_to_nested_matrices(l_roi_matrices_fmri$l_roi_matrices_40_eng, f_apply_full_transformation),
  "l_roi_fisher_matrices_40_spa" = f_apply_to_nested_matrices(l_roi_matrices_fmri$l_roi_matrices_40_spa, f_apply_full_transformation)
)

## Fisher Z transformation for 280x280 matrices ##         # 6 hrs approx #
l_fisher_matrices_280 <- list(
  "l_roi_fisher_matrices_280_eng" = f_apply_to_nested_matrices(l_roi_matrices_fmri$l_roi_matrices_280_eng, f_apply_full_transformation),
  "l_roi_fisher_matrices_280_spa" = f_apply_to_nested_matrices(l_roi_matrices_fmri$l_roi_matrices_280_spa, f_apply_full_transformation),
  "l_roi_fisher_matrices_280_comb" = f_apply_to_nested_matrices(l_roi_matrices_fmri$l_roi_matrices_280_comb, f_apply_full_transformation)
)
#l_fisher_matrices_40 & l_fisher_matrices_280 - post fisher transformed corr dist matrices fmri #  

#### Step 6.3 Cosine Similarity ####
### Step 6.3A: Apply cosine similarity to average fMRI and behavioral matrices ###

### Function to convert cosine similarity matrix to cosine dissimilarity ###
f_cosine_to_dissimilarity <- function(matrix) {
  return(1 - matrix)
}

## Define the Cosine Similarity function ##
f_cosine_similarity <- function(matrix) {
  n <- nrow(matrix)
  sim_matrix <- matrix(0, n, n)
  for (i in 1:n) {
    for (j in i:n) {
      dot_product <- sum(matrix[i, ] * matrix[j, ])
      norm_i <- sqrt(sum(matrix[i, ]^2))
      norm_j <- sqrt(sum(matrix[j, ]^2))
      cosine_sim <- dot_product / (norm_i * norm_j)
      sim_matrix[i, j] <- cosine_sim
      sim_matrix[j, i] <- cosine_sim
    }
  }
  return(sim_matrix)
}

## Initialize new lists for the Cosine Similarity matrices ##
l_cosine_40x40 <- list()
l_cosine_280x280 <- list()

## Apply Cosine Similarity to lists of fMRI matrices ##
l_cosine_avg_mat_per_roi_40_comb <- lapply(l_avg_mat_per_roi_40_comb, function(matrix) f_cosine_similarity(matrix))
l_cosine_avg_mat_per_roi_40_eng <- lapply(l_avg_mat_per_roi_40_eng, function(matrix) f_cosine_similarity(matrix))
l_cosine_avg_mat_per_roi_40_spa <- lapply(l_avg_mat_per_roi_40_spa, function(matrix) f_cosine_similarity(matrix))
l_cosine_avg_mat_per_roi_280_eng <- lapply(l_avg_mat_per_roi_280_eng, function(matrix) f_cosine_similarity(matrix))
l_cosine_avg_mat_per_roi_280_spa <- lapply(l_avg_mat_per_roi_280_spa, function(matrix) f_cosine_similarity(matrix))
l_cosine_avg_mat_per_roi_280_comb <- lapply(l_avg_mat_per_roi_280_comb, function(matrix) f_cosine_similarity(matrix))

## Apply dissimilarity to Cosine fMRI matrices  ##
l_cosine_dissim_avg_mat_per_roi_40_comb <- lapply(l_cosine_avg_mat_per_roi_40_comb, f_cosine_to_dissimilarity)
l_cosine_dissim_avg_mat_per_roi_40_eng <- lapply(l_cosine_avg_mat_per_roi_40_eng, f_cosine_to_dissimilarity)
l_cosine_dissim_avg_mat_per_roi_40_spa <- lapply(l_cosine_avg_mat_per_roi_40_spa, f_cosine_to_dissimilarity)
l_cosine_dissim_avg_mat_per_roi_280_eng <- lapply(l_cosine_avg_mat_per_roi_280_eng, f_cosine_to_dissimilarity)
l_cosine_dissim_avg_mat_per_roi_280_spa <- lapply(l_cosine_avg_mat_per_roi_280_spa, f_cosine_to_dissimilarity)
l_cosine_dissim_avg_mat_per_roi_280_comb <- lapply(l_cosine_avg_mat_per_roi_280_comb, f_cosine_to_dissimilarity)

### Step 6.3B Cosine similarity to Behavioral matrices ### 
### order for transformation - Raw --> Z-transformed --> cosine similarity (NO corr coefficient). ### 

## Apply Cosine Similarity to raw behavioral matrices ##
l_cosine_final_matrices_40x40 <- lapply(l_final_matrices_40x40_2, function(matrix) f_cosine_similarity(matrix))
l_cosine_final_matrices_280x280 <- lapply(l_final_matrices_280x280_2, function(matrix) f_cosine_similarity(matrix))

## Apply dissimilarity to cosine matrices ##
l_cosine_dissim_behavioral_40x40 <- lapply(l_cosine_final_matrices_40x40, f_cosine_to_dissimilarity)
l_cosine_dissim_behavioral_280x280 <- lapply(l_cosine_final_matrices_280x280, f_cosine_to_dissimilarity)
### Cosine raw dissim matrices: l_cosine_dissim_behavioral_40x40 and l_cosine_dissim_behavioral_280x280


## Cosine Similarity to Z-transformed 40x40 matrices ##
z_scored_cosine_behavioral_40x40 <- lapply(z_scored_behavioral_40x40, f_cosine_similarity)
z_scored_cosine_behavioral_280x280 <- lapply(z_scored_behavioral_280x280, f_cosine_similarity)

# Apply dissimilarity to Z-transformed (== pearson correlation!!) #
l_cosine_dissim_z_behavioral_40x40 <- lapply(z_scored_cosine_behavioral_40x40, f_cosine_to_dissimilarity)
l_cosine_dissim_z_behavioral_280x280 <- lapply(z_scored_cosine_behavioral_280x280, f_cosine_to_dissimilarity)

## Rank transformation -- Raw --> rank-transformed (computed in the corr distance section) --> cosine similarity ##
## cosine similarity directly to the rank-transformed matrices ##
ranked_cosine_behavioral_40x40 <- lapply(ranked_behavioral_40x40, f_cosine_similarity)
ranked_cosine_behavioral_280x280 <- lapply(ranked_behavioral_280x280, f_cosine_similarity)

# Apply dissimilarity to Rank-transformed #
l_cosine_dissim_ranked_behavioral_40x40 <- lapply(ranked_cosine_behavioral_40x40, f_cosine_to_dissimilarity)
l_cosine_dissim_ranked_behavioral_280x280 <- lapply(ranked_cosine_behavioral_280x280, f_cosine_to_dissimilarity)
### cosine matrices after rank-transformation: l_cosine_dissim_ranked_behavioral_40x40 & l_cosine_dissim_ranked_behavioral_280x280 ###  

### Step 6.3C: Apply cosine similarity to fMRI nested matrices ###

#### Cosine similarity transformation to each matrix function ####
f_apply_cosine_similarity_to_nested <- function(nested_list) {
  lapply(seq_along(nested_list), function(i) {
    inner_list_name <- names(nested_list)[i]  # Get the name of the current outer list element
    inner_list <- nested_list[[i]]
    
    # Process each inner list
    processed_inner_list <- lapply(seq_along(inner_list), function(j) {
      inner_matrix_name <- names(inner_list)[j]  # Get the name of the current inner list element
      result <- f_cosine_similarity(inner_list[[j]])  # Apply cosine similarity
      
      # Print progress for each inner matrix
      cat("Processed:", inner_list_name, "-", inner_matrix_name, "\n")
      result
    })
    
    # Print progress after finishing an outer list
    cat("Finished processing:", inner_list_name, "\n")
    
    # Set names for the processed inner list and return
    names(processed_inner_list) <- names(inner_list)
    processed_inner_list
  }) -> processed_list
  
  # Set names for the processed list and return
  names(processed_list) <- names(nested_list)
  processed_list
}

#### Function to pass to dissimilarity Cosine matrices ####
f_convert_nested_cosine_to_dissim <- function(nested_list) {
  lapply(nested_list, function(inner_list) {
    lapply(inner_list, f_cosine_to_dissimilarity)
  })
}


## Cosine similarity for 40x40 matrices ##
l_cosine_similarity_matrices_40_fmri <- list(
  "l_roi_cosine_matrices_40_comb_fmri" = f_apply_cosine_similarity_to_nested(l_roi_matrices_fmri$l_roi_matrices_40_comb),
  "l_roi_cosine_matrices_40_eng_fmri" = f_apply_cosine_similarity_to_nested(l_roi_matrices_fmri$l_roi_matrices_40_eng),
  "l_roi_cosine_matrices_40_spa_fmri" = f_apply_cosine_similarity_to_nested(l_roi_matrices_fmri$l_roi_matrices_40_spa)
)

## Cosine similarity for 280x280 matrices ##      # approx 80 segs x ROI  (11) = 15 min approx #
l_cosine_similarity_matrices_280_fmri <- list(
  "l_roi_cosine_matrices_280_eng_fmri" = f_apply_cosine_similarity_to_nested(l_roi_matrices_fmri$l_roi_matrices_280_eng),
  "l_roi_cosine_matrices_280_spa_fmri" = f_apply_cosine_similarity_to_nested(l_roi_matrices_fmri$l_roi_matrices_280_spa),
  "l_roi_cosine_matrices_280_comb_fmri" = f_apply_cosine_similarity_to_nested(l_roi_matrices_fmri$l_roi_matrices_280_comb)
)
# l_cosine_similarity_matrices_40 & l_cosine_similarity_matrices_280  # 

### Step 6.3D: Cosine similarity to Z-transformed fMRI matrices ###      
l_cosine_z_matrices_40_fmri <- list(
  "l_roi_cosine_z_matrices_40_comb_fmri" = f_apply_cosine_similarity_to_nested(l_z_transformed_matrices_40_fmri$l_roi_z_matrices_40_comb_fmri),
  "l_roi_cosine_z_matrices_40_eng_fmri" = f_apply_cosine_similarity_to_nested(l_z_transformed_matrices_40_fmri$l_roi_z_matrices_40_eng_fmri),
  "l_roi_cosine_z_matrices_40_spa_fmri" = f_apply_cosine_similarity_to_nested(l_z_transformed_matrices_40_fmri$l_roi_z_matrices_40_spa_fmri)
) 

l_cosine_z_matrices_280_fmri <- list(
  "l_roi_cosine_z_matrices_280_eng_fmri" = f_apply_cosine_similarity_to_nested(l_z_transformed_matrices_280_fmri$l_roi_z_matrices_280_eng_fmri),
  "l_roi_cosine_z_matrices_280_spa_fmri" = f_apply_cosine_similarity_to_nested(l_z_transformed_matrices_280_fmri$l_roi_z_matrices_280_spa_fmri),
  "l_roi_cosine_z_matrices_280_comb_fmri" = f_apply_cosine_similarity_to_nested(l_z_transformed_matrices_280_fmri$l_roi_z_matrices_280_comb_fmri)
)
### l_cosine_z_matrices_40_fmri & l_cosine_z_matrices_280_fmri - cosine distance post Z-transformed - fMRI ###

### Step 6.3E: Cosine similarity to rank-transformed fMRI matrices ###
l_cosine_ranked_matrices_40_fmri <- list(
  "l_roi_cosine_ranked_matrices_40_comb_fmri" = f_apply_cosine_similarity_to_nested(l_ranked_matrices_40_fmri$l_roi_ranked_matrices_40_comb),
  "l_roi_cosine_ranked_matrices_40_eng_fmri" = f_apply_cosine_similarity_to_nested(l_ranked_matrices_40_fmri$l_roi_ranked_matrices_40_eng),
  "l_roi_cosine_ranked_matrices_40_spa_fmri" = f_apply_cosine_similarity_to_nested(l_ranked_matrices_40_fmri$l_roi_ranked_matrices_40_spa)
)

l_cosine_ranked_matrices_280_fmri <- list(
  "l_roi_cosine_ranked_matrices_280_eng" = f_apply_cosine_similarity_to_nested(l_ranked_matrices_280_fmri$l_roi_ranked_matrices_280_eng),
  "l_roi_cosine_ranked_matrices_280_spa" = f_apply_cosine_similarity_to_nested(l_ranked_matrices_280_fmri$l_roi_ranked_matrices_280_spa),
  "l_roi_cosine_ranked_matrices_280_comb" = f_apply_cosine_similarity_to_nested(l_ranked_matrices_280_fmri$l_roi_ranked_matrices_280_comb)
)
### l_cosine_ranked_matrices_40 & l_cosine_ranked_matrices_280 - cosine distance post rank-transformed - fMRI ###

### Step 6.F: Pass to dissimilarity from cosine matrices in fMRI ###
l_cosine_dissim_similarity_matrices_40_fmri <- lapply(l_cosine_similarity_matrices_40_fmri, f_convert_nested_cosine_to_dissim)
l_cosine_dissim_similarity_matrices_280_fmri <- lapply(l_cosine_similarity_matrices_280_fmri, f_convert_nested_cosine_to_dissim)

l_cosine_dissim_z_matrices_40_fmri <- lapply(l_cosine_z_matrices_40_fmri, f_convert_nested_cosine_to_dissim)
l_cosine_dissim_z_matrices_280_fmri <- lapply(l_cosine_z_matrices_280_fmri, f_convert_nested_cosine_to_dissim)

l_cosine_dissim_ranked_matrices_40_fmri <- lapply(l_cosine_ranked_matrices_40_fmri, f_convert_nested_cosine_to_dissim)
l_cosine_dissim_ranked_matrices_280_fmri <- lapply(l_cosine_ranked_matrices_280_fmri, f_convert_nested_cosine_to_dissim)

### Step 6.4: Crossnobis distance (Adapted from Walther et al., 2016) ###
###
# For behavioral matrices, pairwise distances were computed using a reference covariance 
# matrix estimated from a related condition (e.g., English matrices for Spanish and combined conditions), 
# ensuring noise normalization and robust comparisons. For neural (fMRI) data, 
# group-level covariance matrices were estimated by pooling all subject matrices 
# within each region of interest (ROI) while excluding the test subject to maintain 
# independence. Crossnobis distances were calculated for each subject's matrix using 
# the linear discriminant contrast (LDC), normalized by the standard error estimated 
# from residuals.
### 
### P.C.: For Crossnobis_distance, the Z-transformation or rank transformation interacts due to the sign of the distance  ###
### So only non-transformed processed ###


#### Step 6.4A: Behavioral matrices — Crossnobis using reference condition covariance ####
# Load required libraries
library(MASS)
library(lattice)

#### Function to compute crossnobis distance with normalization ####
f_compute_crossnobis_distance_with_norm <- function(matrix_A, matrix_B, regularization = 1e-3) {
  # Compute covariance matrix from matrix_A and add regularization
  cov_matrix_A <- cov(matrix_A) + diag(regularization, nrow(matrix_A))
  
  # Compute residuals to estimate standard error
  residuals_B <- matrix_B - matrix_A
  error_var_B <- cov(residuals_B) + diag(regularization, nrow(residuals_B))
  
  # Initialize the crossnobis distance matrix
  crossnobis_matrix <- matrix(0, nrow(matrix_A), ncol(matrix_A))
  
  # Compute distances normalized by standard error
  for (i in 1:nrow(matrix_A)) {
    for (j in 1:ncol(matrix_A)) {
      diff_vector <- matrix_B[i, ] - matrix_B[j, ]
      ldc_dist <- t(diff_vector) %*% solve(cov_matrix_A) %*% diff_vector
      norm_factor <- sqrt(t(diff_vector) %*% solve(error_var_B) %*% diff_vector)
      if (i == j) {
        crossnobis_matrix[i, j] <- 0
      } else {
        crossnobis_matrix[i, j] <- ldc_dist / norm_factor
      }
    }
  }
  return(crossnobis_matrix)
}

#### Function to process all matrices in the list and handle 2 specific cases ####
f_process_crossnobis_distances <- function(matrix_list) {
  results <- list()
  processed <- character()
  skipped <- character()
  
  for (name in names(matrix_list)) {
    
    # Handle specific known missing pair directly
    if (name == "Behavioral_mean_matrix_spa_all_data_combined_obj_esp") {
      ref_name <- "Behavioral_mean_matrix_eng_all_data_combined_obj_eng"
      if (ref_name %in% names(matrix_list)) {
        cat("Processing (manual pair):", name, "using covariance from", ref_name, "\n")
        results[[name]] <- f_compute_crossnobis_distance_with_norm(matrix_list[[ref_name]], matrix_list[[name]])
        processed <- c(processed, name)
        next
      }
    } else if (name == "Behavioral_mean_matrix_eng_all_data_combined_obj_eng") {
      ref_name <- "Behavioral_mean_matrix_spa_all_data_combined_obj_esp"
      if (ref_name %in% names(matrix_list)) {
        cat("Processing (manual pair):", name, "using covariance from", ref_name, "\n")
        results[[name]] <- f_compute_crossnobis_distance_with_norm(matrix_list[[ref_name]], matrix_list[[name]])
        processed <- c(processed, name)
        next
      }
    }
    
    # Standard pairing logic
    if (grepl("spa", name)) {
      ref_name <- sub("spa", "eng", name)
      if (ref_name %in% names(matrix_list)) {
        cat("Processing:", name, "using covariance from", ref_name, "\n")
        results[[name]] <- f_compute_crossnobis_distance_with_norm(matrix_list[[ref_name]], matrix_list[[name]])
        processed <- c(processed, name)
      } else {
        skipped <- c(skipped, name)
      }
      
    } else if (grepl("comb", name)) {
      ref_name <- sub("comb", "eng", name)
      if (ref_name %in% names(matrix_list)) {
        cat("Processing:", name, "using covariance from", ref_name, "\n")
        results[[name]] <- f_compute_crossnobis_distance_with_norm(matrix_list[[ref_name]], matrix_list[[name]])
        processed <- c(processed, name)
      } else {
        skipped <- c(skipped, name)
      }
      
    } else if (grepl("eng", name)) {
      ref_name <- sub("eng", "spa", name)
      if (ref_name %in% names(matrix_list)) {
        cat("Processing:", name, "using covariance from", ref_name, "\n")
        results[[name]] <- f_compute_crossnobis_distance_with_norm(matrix_list[[ref_name]], matrix_list[[name]])
        processed <- c(processed, name)
      } else {
        skipped <- c(skipped, name)
      }
    } else {
      skipped <- c(skipped, name)
    }
  }
  
  # Log skipped matrices
  if (length(skipped) > 0) {
    cat("\nSkipped the following matrices due to missing references:\n")
    print(skipped)
  }
  
  return(results)
}

#### Step 6.4B: Plot Behavioral Crossnobis results ####
f_process_and_plot_matrices_without_labels <- function(matrix_list, category_names, roi_names, palette) {
  plots <- list()
  for (name in names(matrix_list)) {
    cat("Plotting:", name, "\n")
    cor_dist_matrix <- matrix_list[[name]]
    
    rownames(cor_dist_matrix) <- NULL
    colnames(cor_dist_matrix) <- category_names
    
    plot <- levelplot(cor_dist_matrix, col.regions = palette, 
                      xlab = "Categories", ylab = "Categories", 
                      main = paste("Crossnobis Distance for", name),
                      scales = list(x = list(at = seq(5, length(category_names), by = 5), labels = seq(5, length(category_names), by = 5), rot = 45, cex = 0.6), 
                                    y = list(at = 1:length(category_names), labels = category_names, cex = 0.7),
                                    tck = c(0, 0)))
    
    plots[[name]] <- plot
  }
  return(plots)
}

### Crossnobis results - behavioral matrices ###
crossnobis_results <- f_process_crossnobis_distances(l_final_matrices_40x40_2) 
crossnobis_plots <- f_process_and_plot_matrices_without_labels(crossnobis_results, category_names, names(crossnobis_results), palette_5colors)
View(crossnobis_results$Behavioral_mean_matrix_eng_all_data_original)
## Print plots ##
#for (plot_name in names(crossnobis_plots)) {
#  print(crossnobis_plots[[plot_name]])
#}
View(crossnobis_results$Behavioral_mean_matrix_eng_all_data_original)
#### Step 6.4C: fMRI matrices — Crossnobis using group-level covariance ####
f_apply_crossnobis_group_cov <- function(nested_list, regularization = 1e-3) {
  lapply(seq_along(nested_list), function(i) {
    roi_name <- names(nested_list)[i]  # ROI name
    roi_regions <- nested_list[[i]]  # The list of ROIs within each type (e.g., "Occipital")
    
    cat("Processing ROI type:", roi_name, "\n")
    
    processed_roi_type <- lapply(seq_along(roi_regions), function(j) {
      roi_region_name <- names(roi_regions)[j]
      roi_list <- roi_regions[[j]]  # Extracting individual participants within the ROI region
      
      cat("Processing ROI region:", roi_region_name, "\n")
      
      # Filter valid numeric matrices from the ROI region
      valid_matrices <- Filter(function(x) is.matrix(x) && is.numeric(x), roi_list)
      
      if (length(valid_matrices) == 0) {
        cat("No valid matrices found for ROI region:", roi_region_name, "\n")
        return(NULL)
      }
      
      # Combine all subject matrices for covariance estimation
      combined_data <- do.call(rbind, valid_matrices)
      group_covariance <- cov(combined_data) + diag(regularization, ncol(combined_data))
      
      processed_participants <- lapply(seq_along(roi_list), function(k) {
        participant_name <- names(roi_list)[k]
        subject_matrix <- roi_list[[k]]
        
        if (!is.null(subject_matrix) && is.matrix(subject_matrix) && is.numeric(subject_matrix)) {
          # Compute crossnobis distance using group covariance
          crossnobis_matrix <- f_compute_crossnobis_distance_with_norm(group_covariance, subject_matrix, regularization)
          
          cat("Processed subject:", participant_name, "in ROI region:", roi_region_name, "\n")
          return(crossnobis_matrix)
        } else {
          cat("Skipping invalid matrix for participant:", participant_name, "\n")
          return(NULL)
        }
      })
      
      names(processed_participants) <- names(roi_list)
      processed_participants
    })
    
    names(processed_roi_type) <- names(roi_regions)
    processed_roi_type
  }) -> processed_list
  
  names(processed_list) <- names(nested_list)
  processed_list
}

## Crossnobis application on fMRI matrices: 1 hr approx ##
fmri_crossnobis_results <- f_apply_crossnobis_group_cov(l_roi_matrices_fmri_40)

## visualization ##
selected_matrices <- list(
  "Occipital_part_1_comb" = fmri_crossnobis_results$l_roi_matrices_40_comb$Occipital$part_1_ROI_1_40x40_comb,
  "Fusi_part_15_spa" = fmri_crossnobis_results$l_roi_matrices_40_spa$fusiform$part_15_ROI_2_40x40_spa,
  "MT_part_50_eng" = fmri_crossnobis_results$l_roi_matrices_40_eng$MT$part_50_ROI_4_40x40_eng
)
selected_plots <- f_process_and_plot_matrices_without_labels(selected_matrices, 
                                                             category_names, 
                                                             names(selected_matrices), 
                                                             palette_5colors)
#print(selected_plots$Occipital_part_1_comb)
#print(selected_plots$MT_part_50_eng)
# Check names at different levels to ensure correct access - Nested structure #
#names(l_roi_matrices_fmri_40)  # language-based distinction 
#names(l_roi_matrices_fmri_40$l_roi_matrices_40_comb) # specific ROIs (e.g. "Occipital")
#names(l_roi_matrices_fmri_40$l_roi_matrices_40_comb$Occipital)  # participant names


## ------------------------------------------------------------------------------------------
### -------------------------- SECTION 2: Main Analysis  -------------------------------- ###


##### Step 7: Compute correlations across matrices (2nd-level matrix) and plot it #####

#### Step 7.1: Define and apply matrix correlation functions ####
library(ggplot2)
library(reshape2)
library(scales)

#### Function to calculate the correlation matrix from a list of matrices ####
f_calculate_correlation_matrix <- function(matrix_list, method = "pearson") {
   # Get the number of matrices
   n <- length(matrix_list)
   
   # Initialize an empty correlation matrix
   correlation_matrix <- matrix(NA, n, n)
   
   # Set the row and column names of the correlation matrix
   rownames(correlation_matrix) <- names(matrix_list)
   colnames(correlation_matrix) <- names(matrix_list)
   
   # Loop over the matrices to calculate correlations
   for (i in 1:n) {
     for (j in i:n) {
       # Calculate the correlation between the matrices
       correlation <- cor(as.vector(matrix_list[[i]]), as.vector(matrix_list[[j]]), 
                          use = "pairwise.complete.obs", method = method)
       
       # Store the correlation in the matrix
       correlation_matrix[i, j] <- correlation
       correlation_matrix[j, i] <- correlation
     }
   }
   
   return(correlation_matrix)
 }

#### Function to convert the correlation matrix to a long format data frame for plotting ####
f_convert_corr_matrix_to_long_format <- function(corr_matrix) {
  long_format <- melt(corr_matrix, na.rm = TRUE)
  colnames(long_format) <- c("Row", "Column", "Value")
  return(long_format)
}

#### Function to create a heatmap from the long format data frame ####
f_create_heatmap <- function(long_format, title) {
  ggplot(long_format, aes(x = Row, y = Column, fill = Value)) +
    geom_tile() +
    scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, 
                         limits = c(-1, 1), na.value = "grey50") +
    labs(title = title, x = "Matrices", y = "Matrices") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

#### Function for behavior-brain correlation per subject across ROIs ####
f_compute_and_plot_2nd_level_correlation <- function(behavioral_matrices, fmri_matrices_list, 
                                                     correlation_method = "pearson", matrix_dim = 40) {
  
  # Start time
  start_time <- Sys.time()
  
  # Initialize the list to store all conditions
  all_conditions_2nd_level_matrices <- list()
  
  # Loop through each fMRI condition
  for (fmri_condition in names(fmri_matrices_list)) {
    print(paste("Processing condition:", fmri_condition))
    
    # Initialize list to store each participant's 2nd-level correlation matrix for the current condition
    condition_2nd_level_matrices <- list()
    
    # Extract the ROI list for the current condition
    fmri_rois <- fmri_matrices_list[[fmri_condition]]
    
    # Loop over all participants (assuming 80 participants)
    for (participant_num in 1:80) {
      # Define the participant identifier (e.g., "part_1")
      participant_id <- paste0("part_", participant_num)
      
      # Step 1: Initialize list to store all matrices (behavioral + fMRI ROIs for the current participant and condition)
      combined_matrices <- list()
      
      # Step 2: Add the behavioral matrices to the combined list
      for (behav_name in names(behavioral_matrices)) {
        combined_matrices[[behav_name]] <- behavioral_matrices[[behav_name]]
      }
      
      # Step 3: Add fMRI matrices for the current participant and condition
      for (roi_name in names(fmri_rois)) {
        # Find the matrix for the current participant in the current ROI
        participant_matrix <- fmri_rois[[roi_name]][grep(participant_id, names(fmri_rois[[roi_name]]))][[1]]
        
        # Define a unique name for this ROI in the combined list
        fmri_name <- paste(fmri_condition, roi_name, sep = "_")
        
        # Add this matrix to the combined list
        combined_matrices[[fmri_name]] <- participant_matrix
      }
      
      # Step 4: Initialize an empty matrix to store correlations for the current participant
      n <- length(combined_matrices)
      second_level_corr_matrix <- matrix(NA, n, n)
      rownames(second_level_corr_matrix) <- names(combined_matrices)
      colnames(second_level_corr_matrix) <- names(combined_matrices)
      
      # Step 5: Calculate pairwise correlations between all matrices in combined_matrices
      for (i in 1:n) {
        for (j in i:n) {
          # Calculate the correlation between the vectorized matrices
          corr_value <- cor(as.vector(combined_matrices[[i]]), as.vector(combined_matrices[[j]]), 
                            use = "pairwise.complete.obs", method = correlation_method)
          
          # Store the correlation in the matrix
          second_level_corr_matrix[i, j] <- corr_value
          second_level_corr_matrix[j, i] <- corr_value
        }
      }
      
      # Step 6: Store the 2nd-level correlation matrix in the main list with a participant-specific name
      participant_matrix_name <- paste0("result_2nd_level_corr_matrix_behav_fmri_", participant_id)
      condition_2nd_level_matrices[[participant_matrix_name]] <- second_level_corr_matrix
      
      # Print progress every 10 participants
      if (participant_num %% 10 == 0) {
        print(paste("Processed", participant_num, "participants for condition", fmri_condition))
      }
    }
    
    # Step 7: Average the 2nd-level correlation matrices across participants for the current condition
    avg_corr_matrix <- Reduce("+", condition_2nd_level_matrices) / length(condition_2nd_level_matrices)
    
    # Step 8: Capture the labels used in the average correlation matrix
    matrix_labels <- rownames(avg_corr_matrix)
    
    # Step 9: Convert the averaged correlation matrix to long format for plotting
    long_format_avg_corr_matrix <- f_convert_corr_matrix_to_long_format(avg_corr_matrix)
    
    # Step 10: Plot the heatmap of the average 2nd-level correlation matrix
    heatmap_plot <- f_create_heatmap(long_format_avg_corr_matrix, 
                                     title = paste("Average 2nd-Level Correlation Matrix for", fmri_condition, "-", correlation_method))
    # Display the heatmap
    print(heatmap_plot)
    
    # Store the results for the current condition in the main list
    all_conditions_2nd_level_matrices[[fmri_condition]] <- list(
      all_2nd_level_corr_matrices = condition_2nd_level_matrices,
      average_corr_matrix = avg_corr_matrix,
      heatmap_plot = heatmap_plot,
      matrix_labels = matrix_labels
    )
  }
  
  # End time and calculate duration using difftime
  end_time <- Sys.time()
  duration <- difftime(end_time, start_time, units = "secs")
  print(paste("Total execution time:", round(duration, 2), "seconds"))
  
  # Return all outputs in a list for all conditions
  return(all_conditions_2nd_level_matrices)
}

#### Step 7.2: Correlate behavioral matrices with fMRI matrices per subject across ROIs for all participants ####

#### Step 7.2A: Correlation using untransformed data ####

### 40x40 ###
## Correlation distance - Pearson - Duration time: 6.13 seconds ##
result_2nd_level_40x40_pearson <- f_compute_and_plot_2nd_level_correlation(
  behavioral_matrices = l_raw_cor_dist_behavioral_40x40,
  fmri_matrices_list = l_cor_dist_matrices_40_fmri,
  correlation_method = "pearson",
  matrix_dim = 40
)

## Correlation distance - Spearman - Duration time: 153.61 seconds  ## ## Check warning ##
result_2nd_level_40x40_spearman <- f_compute_and_plot_2nd_level_correlation(
  behavioral_matrices = l_raw_cor_dist_behavioral_40x40,
  fmri_matrices_list = l_cor_dist_matrices_40_fmri,
  correlation_method = "spearman",
  matrix_dim = 40
)

## Correlation distance - Kendall - Duration time: 5292.3 seconds ##
result_2nd_level_40x40_kendall <- f_compute_and_plot_2nd_level_correlation(
  behavioral_matrices = l_raw_cor_dist_behavioral_40x40,
  fmri_matrices_list = l_cor_dist_matrices_40_fmri,
  correlation_method = "kendall",
  matrix_dim = 40
)

#### Step 7.2B: Correlation using transformed behavioral data ####

## Correlation distance - Z-transformed data - pearson - Durantion time: 6.72 seconds ##
result_2nd_level_z_40x40_pearson <- f_compute_and_plot_2nd_level_correlation(
  behavioral_matrices = z_cor_dist_behavioral_40x40,
  fmri_matrices_list = l_cor_dist_z_matrices_40_fmri,
  correlation_method = "pearson",
  matrix_dim = 40
)
## The correlations seem slightly stronger after the Z-transformation ###

## Correlation distance - rank-transformed data - Spearman - Duration time: 148.42 seconds ##
result_2nd_level_rank_40x40_spearman <- f_compute_and_plot_2nd_level_correlation(
  behavioral_matrices = rank_cor_dist_behavioral_40x40,
  fmri_matrices_list = l_cor_dist_ranked_matrices_40_fmri,
  correlation_method = "spearman",
  matrix_dim = 40
)
## The correlations seem slightly weaker after the rank-transf ###

## Correlation distance - rank-transformed data - Kendall - Duration time: 4846.05 seconds ##
result_2nd_level_rank_40x40_kendall <- f_compute_and_plot_2nd_level_correlation(
  behavioral_matrices = rank_cor_dist_behavioral_40x40,
  fmri_matrices_list = l_cor_dist_ranked_matrices_40_fmri,
  correlation_method = "kendall",
  matrix_dim = 40
)

#### Step 7.2C: Correlation with 280x280 matrices (for visualization) #### ## run this again with the name correction ## 

## Correlation distance - Pearson - Duration time: 99 seconds ##
result_2nd_level_280x280_pearson <- f_compute_and_plot_2nd_level_correlation(
  behavioral_matrices = l_cor_distance_matrices_280x280,
  fmri_matrices_list = l_cor_dist_matrices_280_fmri,
  correlation_method = "pearson",
  matrix_dim = 280
)

## Correlation distance - Spearman - Duration time: 6022.6 seconds ## 
result_2nd_level_280x280_spearman <- f_compute_and_plot_2nd_level_correlation(
  behavioral_matrices = l_cor_distance_matrices_280x280,
  fmri_matrices_list = l_cor_dist_matrices_280_fmri,
  correlation_method = "spearman",
  matrix_dim = 280
)

## Visualization purposes ##
#print(result_2nd_level_280x280_pearson$heatmap_plot)
#print(result_2nd_level_280x280_spearman$heatmap_plot)  

#### Step 7.3: Correlation distance (c.d) Z-fisher transformed -  Behavioral matrices with fMRI matrices per subject across ROIs ####
## l_fisher_matrices_40 & l_fisher_matrices_280 - post fisher transformed corr dist matrices fmri # 
## l_cordist_fisher_40x40 & l_cordist_fisher_280x280
## P.C: I will not transform these matrices to either Z or rank due to the fisher transformation ##

#### Step 7.3A: Correlation using untransformed data ####

#### 40x40 ####
## Correlation distance post-fisher-transformation - Pearson - Duration time: 6.59 seconds ##
result_2nd_level_fisher_40x40_pearson <- f_compute_and_plot_2nd_level_correlation(
  behavioral_matrices = l_cordist_fisher_40x40,
  fmri_matrices_list = l_fisher_matrices_40,
  correlation_method = "pearson",
  matrix_dim = 40
)

## Correlation distance post-fisher-transformation - Spearman Duration time:  118.89 seconds ## 
result_2nd_level_fisher_40x40_spearman <- f_compute_and_plot_2nd_level_correlation(
  behavioral_matrices = l_cordist_fisher_40x40,
  fmri_matrices_list = l_fisher_matrices_40,
  correlation_method = "spearman",
  matrix_dim = 40
)

## Correlation distance post-fisher-transformation - Kendall Duration time: 5218.96  seconds ##
result_2nd_level_fisher_40x40_kendall <- f_compute_and_plot_2nd_level_correlation(
  behavioral_matrices = l_cordist_fisher_40x40,
  fmri_matrices_list = l_fisher_matrices_40,
  correlation_method = "kendall",
  matrix_dim = 40
)

#### Step 7.4: Cosine dissimilarity distance - Correlate behavioral matrices with fMRI matrices ####

#### Step 7.4A: Correlation using untransformed data ####
## Behavioral matrices cosine distance - no transformation: l_cosine_dissim_behavioral_40x40 & _cosine_dissim_behavioral_280x280 ##
## cosine distance matrices fmri - no transformation: l_cosine_dissim_similarity_matrices_40_fmri & l_cosine_dissim_similarity_matrices_280_fmri ##

### 40x40 ###
## Cosine distance - Pearson - Duration time: 10.65 seconds ##
result_2nd_level_cosine_40x40_pearson <- f_compute_and_plot_2nd_level_correlation(
  behavioral_matrices = l_cosine_dissim_behavioral_40x40,
  fmri_matrices_list = l_cosine_dissim_similarity_matrices_40_fmri,
  correlation_method = "pearson",
  matrix_dim = 40
)

## Cosine distance - Spearman - Duration time: 185.89 seconds ##
result_2nd_level_cosine_40x40_spearman <- f_compute_and_plot_2nd_level_correlation(
  behavioral_matrices = l_cosine_dissim_behavioral_40x40,
  fmri_matrices_list = l_cosine_dissim_similarity_matrices_40_fmri,
  correlation_method = "spearman",
  matrix_dim = 40
)

## Cosine distance - Kendall - Duration time: 7720.62 seconds ##
result_2nd_level_cosine_40x40_kendall <- f_compute_and_plot_2nd_level_correlation(
  behavioral_matrices = l_cosine_dissim_behavioral_40x40,
  fmri_matrices_list = l_cosine_dissim_similarity_matrices_40_fmri,
  correlation_method = "kendall",
  matrix_dim = 40
)

#### Step 7.4B: Correlation using transformed behavioral data ####
### 40x40 ###
## Cosine distance - Z-transformed data - Pearson - Duration time: 7.37 seconds ##
result_2nd_level_z_cosine_40x40_pearson <- f_compute_and_plot_2nd_level_correlation(
  behavioral_matrices = l_cosine_dissim_z_behavioral_40x40,
  fmri_matrices_list = l_cosine_dissim_z_matrices_40_fmri,
  correlation_method = "pearson",
  matrix_dim = 40
)

## Cosine distance - Rank-transformed data - Spearman - Duration time: 140.26 seconds ##
result_2nd_level_rank_cosine_40x40_spearman <- f_compute_and_plot_2nd_level_correlation(
  behavioral_matrices = l_cosine_dissim_ranked_behavioral_40x40,
  fmri_matrices_list = l_cosine_dissim_ranked_matrices_40_fmri,
  correlation_method = "spearman",
  matrix_dim = 40
)

## Cosine distance - Rank-transformed data - Kendall - Duration time: 5075.87 seconds ##
result_2nd_level_rank_cosine_40x40_kendall <- f_compute_and_plot_2nd_level_correlation(
  behavioral_matrices = l_cosine_dissim_ranked_behavioral_40x40,
  fmri_matrices_list = l_cosine_dissim_ranked_matrices_40_fmri,
  correlation_method = "kendall",
  matrix_dim = 40
)


#### Step 7.5: Adapted Crossnobis distance - Correlate behavioral matrices with fMRI matrices ####
## Behavioral crossnobis matrices: l_ldc_final_matrices_40x40, z_scored_ldc_behavioral_40x40, ranked_ldc_behavioral_40x40 ##
## fMRI crossnobis matrices: l_crossnobis_matrices_40_fmri, l_crossnobis_z_matrices_40_fmri, l_crossnobis_ranked_matrices_40_fmri ##

#### Step 7.5A: Correlation using untransformed data ####
typeof(l_cosine_similarity_matrices_40_fmri$l_roi_cosine_matrices_40_comb_fmri$Occipital$part_1_ROI_1_40x40_comb)
typeof(fmri_crossnobis_results$l_roi_matrices_40_comb$Occipital$part_1_ROI_1_40x40_comb)


### 40x40 ###
## Cross-nobis distance - Pearson - Duration time: 10 seconds ##
result_2nd_level_crossnobis_40x40_pearson <- f_compute_and_plot_2nd_level_correlation(
  behavioral_matrices = crossnobis_results,  # Raw behavioral Crossnobis
  fmri_matrices_list = fmri_crossnobis_results,  # Raw fMRI Crossnobis
  correlation_method = "pearson",
  matrix_dim = 40
)

## Cross-nobis distance - Spearman - Duration time: 90 seconds ##
result_2nd_level_crossnobis_40x40_spearman <- f_compute_and_plot_2nd_level_correlation(
  behavioral_matrices = crossnobis_results,  # Raw behavioral Crossnobis
  fmri_matrices_list = fmri_crossnobis_results,  # Raw fMRI Crossnobis
  correlation_method = "spearman",
  matrix_dim = 40
)

## Cross-nobis distance - Kendall - Duration time: 7321.11 seconds ##
result_2nd_level_crossnobis_40x40_kendall <- f_compute_and_plot_2nd_level_correlation(
  behavioral_matrices = crossnobis_results,  # Raw behavioral Crossnobis
  fmri_matrices_list = fmri_crossnobis_results,  # Raw fMRI Crossnobis
  correlation_method = "kendall",
  matrix_dim = 40
)

##### Step 8: Filter and plot second-level filtered correlation matrices (2nd-level correlation subject's average) #####
## P.C.: Only correlation and cosine similarity distance done for visualizing the data ## 

#### Function to filter and plot the correlation matrix for multiple conditions ####
f_filter_and_plot_per_condition <- function(result_per_condition, row_pattern, title_prefix) {
  # Initialize a list to store filtered matrices and heatmaps for all conditions
  filtered_results <- list()
  
  # Define descriptive names and column patterns for each condition
  condition_titles <- c("Combined", "English", "Spanish")
  col_patterns <- c("comb", "eng", "spa")
  
  # Ensure the number of condition titles matches the number of conditions
  if (length(condition_titles) != length(names(result_per_condition)) ||
      length(col_patterns) != length(names(result_per_condition))) {
    stop("Mismatch between the number of conditions and descriptive titles.")
  }
  
  # Loop through each condition
  condition_idx <- 1
  for (condition_name in names(result_per_condition)) {
    print(paste("Processing condition:", condition_name))
    
    # Extract the average correlation matrix and labels for the condition
    corr_matrix <- result_per_condition[[condition_name]]$average_corr_matrix
    labels <- result_per_condition[[condition_name]]$matrix_labels
    
    # Define the column pattern for the current condition
    col_pattern <- col_patterns[condition_idx]
    col_exclude_pattern <- "Behavioral"  # Exclude behavioral matrices from columns
    
    # Filter rows based on the row pattern (e.g., rows that contain "Behavioral")
    filtered_rows <- grepl(row_pattern, labels)
    print(paste("Filtered rows for condition:", condition_name))
    print(labels[filtered_rows])
    
    # Filter columns based on the column pattern for the current condition
    filtered_cols <- grepl(col_pattern, labels) & !grepl(col_exclude_pattern, labels)
    print(paste("Filtered columns for condition:", condition_name))
    print(labels[filtered_cols])
    
    # Check if filtering resulted in valid rows and columns
    if (sum(filtered_rows) == 0 || sum(filtered_cols) == 0) {
      warning(paste("No matching rows or columns found for condition:", condition_name))
      next
    }
    
    # Subset the correlation matrix based on the filtering logic
    filtered_corr_matrix <- corr_matrix[filtered_rows, filtered_cols]
    
    # Convert the filtered matrix to long format for heatmap plotting
    long_format <- f_convert_corr_matrix_to_long_format(filtered_corr_matrix)
    
    # Create and plot the heatmap
    heatmap_title <- paste(title_prefix, "-", condition_titles[condition_idx])
    heatmap_plot <- f_create_heatmap(long_format, heatmap_title)
    
    # Display the heatmap
    print(heatmap_plot)
    
    # Store the results for the condition
    filtered_results[[condition_name]] <- list(
      filtered_corr_matrix = filtered_corr_matrix,
      heatmap_plot = heatmap_plot,
      filtered_in_rows = labels[filtered_rows],
      filtered_in_cols = labels[filtered_cols]
    )
    
    # Increment the condition index
    condition_idx <- condition_idx + 1
  }
  
  return(filtered_results)
}

#### Step 8.1: Correlation distance ####

### 40x40 - No transformation ###
## Pearson ##
filtered_corr_matrix_behav_comb_40x40_pearson <- f_filter_and_plot_per_condition(result_per_condition = result_2nd_level_40x40_pearson,
                                                                                 row_pattern = "Behavioral",
                                                                                 title_prefix = "Filtered Pearson Correlation 2nd-Level Matrix 40x40"
                                                                                 )
## Spearman ##
filtered_corr_matrix_behav_comb_40x40_spearman <- f_filter_and_plot_per_condition(result_per_condition = result_2nd_level_40x40_spearman,
                                                                                  row_pattern = "Behavioral",
                                                                                  title = "Filtered Spearman Correlation 2nd-Level Matrix 40x40"
                                                                                  )
## Kendall ##
filtered_corr_matrix_behav_comb_40x40_kendall <- f_filter_and_plot_per_condition(result_per_condition = result_2nd_level_40x40_kendall,
                                                                                 row_pattern = "Behavioral",
                                                                                 title = "Filtered Kendall Correlation 2nd-Level Matrix 40x40"
                                                                                 )

### 40x40: Correlation distance - Transformed ###
## Pearson - Z-transformed ##
filtered_corr_matrix_behav_comb_z_40x40_pearson <- f_filter_and_plot_per_condition(result_per_condition = result_2nd_level_z_40x40_pearson,
                                                                                   row_pattern = "Behavioral",
                                                                                   title = "Filtered Pearson Correlation 2nd-Level Matrix Z-transformed 40x40"
                                                                                   )
## Spearman - Rank transformed ##
filtered_corr_matrix_behav_comb_rank_40x40_spearman <- f_filter_and_plot_per_condition(result_per_condition = result_2nd_level_rank_40x40_spearman,
                                                                                       row_pattern = "Behavioral",
                                                                                       title = "Filtered Spearman Correlation 2nd-Level Matrix rank-transformed 40x40"
                                                                                       )

## Filtered kendall Correlation - rank-transformed ##
filtered_corr_matrix_behav_comb_rank_40x40_kendall <- f_filter_and_plot_per_condition(result_per_condition = result_2nd_level_rank_40x40_kendall,
                                                                                      row_pattern = "Behavioral",
                                                                                      title = "Filtered Kendall Correlation 2nd-Level Matrix rank-transformed 40x40"
                                                                                      )

#### Step 8.2: Cosine similarity distance ####

### 40x40: Cosine similarity - No transformation ###
## Pearson ##
filtered_corr_matrix_cosine_behav_comb_40x40_pearson <- f_filter_and_plot_per_condition(result_per_condition = result_2nd_level_cosine_40x40_pearson,
                                                                                        row_pattern = "Behavioral",
                                                                                        title_prefix = "Filtered Pearson Correlation Cosine Distance 2nd-Level Matrix 40x40"
                                                                                        )
#print(filtered_corr_matrix_cosine_behav_comb_40x40_pearson$l_roi_cosine_matrices_40_comb_fmri$heatmap_plot)

## Spearman ##
filtered_corr_matrix_cosine_behav_comb_40x40_spearman <- f_filter_and_plot_per_condition(result_per_condition = result_2nd_level_cosine_40x40_spearman,
                                                                                         row_pattern = "Behavioral",
                                                                                         title_prefix = "Filtered Spearman Correlation Cosine Distance 2nd-Level Matrix 40x40"
                                                                                         )
## Kendall ##
filtered_corr_matrix_cosine_behav_comb_40x40_kendall <- f_filter_and_plot_per_condition(result_per_condition = result_2nd_level_cosine_40x40_kendall,
                                                                                        row_pattern = "Behavioral",
                                                                                        title_prefix = "Filtered Kendall Correlation Cosine Distance 2nd-Level Matrix 40x40"
                                                                                        )
## 40x40: Cosine similarity - Transformed ##
## Pearson - Z-transformed ##
filtered_corr_matrix_z_cosine_behav_comb_40x40_pearson <- f_filter_and_plot_per_condition(result_per_condition = result_2nd_level_z_cosine_40x40_pearson,
                                                                                          row_pattern = "Behavioral",
                                                                                          title_prefix = "Filtered Pearson Correlation Cosine Distance 2nd-Level Matrix Z-transformed 40x40"
                                                                                          )

## Spearman - Rank transformed ##
filtered_corr_matrix_rank_cosine_behav_comb_40x40_spearman <- f_filter_and_plot_per_condition(result_per_condition = result_2nd_level_rank_cosine_40x40_spearman,
                                                                                              row_pattern = "Behavioral",
                                                                                              title_prefix = "Filtered Spearman Correlation Cosine Distance 2nd-Level Matrix Rank-transformed 40x40"
                                                                                              )

## Kendall Correlation - rank-transformed ##
filtered_corr_matrix_rank_cosine_behav_comb_40x40_kendall <- f_filter_and_plot_per_condition(result_per_condition = result_2nd_level_rank_cosine_40x40_kendall,
                                                                                             row_pattern = "Behavioral",
                                                                                             title_prefix = "Filtered Kendall Correlation Cosine Distance 2nd-Level Matrix Rank-transformed 40x40"
                                                                                             )

##### Step 8.3: Crossnobis distance ##### #### 
### 40x40 - No transformation ###
## Pearson ##
filtered_corr_matrix_crossnobis_behav_comb_40x40_pearson <- f_filter_and_plot_per_condition(result_per_condition = result_2nd_level_crossnobis_40x40_pearson,
                                                                                        row_pattern = "Behavioral",
                                                                                        title_prefix = "Filtered Pearson Correlation Crossnobis Distance 2nd-Level Matrix 40x40"
)
#print(filtered_corr_matrix_crossnobis_behav_comb_40x40_pearson$l_roi_cosine_matrices_40_comb_fmri$heatmap_plot)

## Spearman ##
filtered_corr_matrix_crossnobis_behav_comb_40x40_spearman <- f_filter_and_plot_per_condition(result_per_condition = result_2nd_level_crossnobis_40x40_spearman,
                                                                                         row_pattern = "Behavioral",
                                                                                         title_prefix = "Filtered Spearman Correlation Crossnobis Distance 2nd-Level Matrix 40x40"
)
## Kendall ##
filtered_corr_matrix_crossnobis_behav_comb_40x40_kendall <- f_filter_and_plot_per_condition(result_per_condition = result_2nd_level_crossnobis_40x40_kendall,
                                                                                        row_pattern = "Behavioral",
                                                                                        title_prefix = "Filtered Kendall Correlation Crossnobis Distance 2nd-Level Matrix 40x40"
)

##### Step 9: Aggregate all 2nd-level correlation matrices ##### 

#### Step 9.1: Aggregate all 2nd-level AVERAGE correlation matrices  ####
additional_2nd_level_matrices <- list(
  result_2nd_level_40x40_comb_pearson = result_2nd_level_40x40_pearson$l_roi_corr_matrices_40_comb_fmri$average_corr_matrix,  ## correlation distance ##
  result_2nd_level_40x40_eng_pearson = result_2nd_level_40x40_pearson$l_roi_corr_matrices_40_eng_fmri$average_corr_matrix,
  result_2nd_level_40x40_spa_pearson = result_2nd_level_40x40_pearson$l_roi_corr_matrices_40_spa_fmri$average_corr_matrix,
  result_2nd_level_40x40_comb_spearman = result_2nd_level_40x40_spearman$l_roi_corr_matrices_40_comb_fmri$average_corr_matrix,
  result_2nd_level_40x40_eng_spearman = result_2nd_level_40x40_spearman$l_roi_corr_matrices_40_eng_fmri$average_corr_matrix,
  result_2nd_level_40x40_spa_spearman = result_2nd_level_40x40_spearman$l_roi_corr_matrices_40_spa_fmri$average_corr_matrix,
  result_2nd_level_40x40_comb_kendall = result_2nd_level_40x40_kendall$l_roi_corr_matrices_40_comb_fmri$average_corr_matrix,
  result_2nd_level_40x40_eng_kendall = result_2nd_level_40x40_kendall$l_roi_corr_matrices_40_eng_fmri$average_corr_matrix,
  result_2nd_level_40x40_spa_kendall = result_2nd_level_40x40_kendall$l_roi_corr_matrices_40_spa_fmri$average_corr_matrix,
  result_2nd_level_z_40x40_comb_pearson = result_2nd_level_z_40x40_pearson$l_roi_cor_dist_z_matrices_40_comb_fmri$average_corr_matrix,
  result_2nd_level_z_40x40_eng_pearson = result_2nd_level_z_40x40_pearson$l_roi_cor_dist_z_matrices_40_eng_fmri$average_corr_matrix,
  result_2nd_level_z_40x40_spa_pearson = result_2nd_level_z_40x40_pearson$l_roi_cor_dist_z_matrices_40_spa_fmri$average_corr_matrix,
  result_2nd_level_rank_40x40_comb_spearman = result_2nd_level_rank_40x40_spearman$l_roi_cor_dist_ranked_matrices_40_comb_fmri$average_corr_matrix,
  result_2nd_level_rank_40x40_eng_spearman = result_2nd_level_rank_40x40_spearman$l_roi_cor_dist_ranked_matrices_40_eng_fmri$average_corr_matrix,
  result_2nd_level_rank_40x40_spa_spearman = result_2nd_level_rank_40x40_spearman$l_roi_cor_dist_ranked_matrices_40_spa_fmri$average_corr_matrix,
  result_2nd_level_rank_40x40_comb_kendall = result_2nd_level_rank_40x40_kendall$l_roi_cor_dist_ranked_matrices_40_comb_fmri$average_corr_matrix,
  result_2nd_level_rank_40x40_eng_kendall = result_2nd_level_rank_40x40_kendall$l_roi_cor_dist_ranked_matrices_40_eng_fmri$average_corr_matrix,
  result_2nd_level_rank_40x40_spa_kendall = result_2nd_level_rank_40x40_kendall$l_roi_cor_dist_ranked_matrices_40_spa_fmri$average_corr_matrix,
  result_2nd_level_fisher_40x40_comb_pearson = result_2nd_level_fisher_40x40_pearson$l_roi_fisher_matrices_40_comb$average_corr_matrix, ## correlation distance post fisher transf ## 
  result_2nd_level_fisher_40x40_eng_pearson = result_2nd_level_fisher_40x40_pearson$l_roi_fisher_matrices_40_eng$average_corr_matrix,  
  result_2nd_level_fisher_40x40_spa_pearson = result_2nd_level_fisher_40x40_pearson$l_roi_fisher_matrices_40_spa$average_corr_matrix,  
  result_2nd_level_fisher_40x40_comb_spearman = result_2nd_level_fisher_40x40_spearman$l_roi_fisher_matrices_40_comb$average_corr_matrix,
  result_2nd_level_fisher_40x40_eng_spearman = result_2nd_level_fisher_40x40_spearman$l_roi_fisher_matrices_40_eng$average_corr_matrix,
  result_2nd_level_fisher_40x40_spa_spearman = result_2nd_level_fisher_40x40_spearman$l_roi_fisher_matrices_40_spa$average_corr_matrix,
  result_2nd_level_fisher_40x40_comb_kendall = result_2nd_level_fisher_40x40_kendall$l_roi_fisher_matrices_40_comb$average_corr_matrix,
  result_2nd_level_fisher_40x40_eng_kendall = result_2nd_level_fisher_40x40_kendall$l_roi_fisher_matrices_40_eng$average_corr_matrix,
  result_2nd_level_fisher_40x40_spa_kendall = result_2nd_level_fisher_40x40_kendall$l_roi_fisher_matrices_40_spa$average_corr_matrix,
  result_2nd_level_cosine_40x40_comb_pearson = result_2nd_level_cosine_40x40_pearson$l_roi_cosine_matrices_40_comb_fmri$average_corr_matrix, ## cosine distance ##
  result_2nd_level_cosine_40x40_eng_pearson = result_2nd_level_cosine_40x40_pearson$l_roi_cosine_matrices_40_eng_fmri$average_corr_matrix, 
  result_2nd_level_cosine_40x40_spa_pearson = result_2nd_level_cosine_40x40_pearson$l_roi_cosine_matrices_40_spa_fmri$average_corr_matrix,
  result_2nd_level_cosine_40x40_comb_spearman = result_2nd_level_cosine_40x40_spearman$l_roi_cosine_matrices_40_comb_fmri$average_corr_matrix,
  result_2nd_level_cosine_40x40_eng_spearman = result_2nd_level_cosine_40x40_spearman$l_roi_cosine_matrices_40_eng_fmri$average_corr_matrix,
  result_2nd_level_cosine_40x40_spa_spearman = result_2nd_level_cosine_40x40_spearman$l_roi_cosine_matrices_40_spa_fmri$average_corr_matrix,
  result_2nd_level_cosine_40x40_comb_kendall = result_2nd_level_cosine_40x40_kendall$l_roi_cosine_matrices_40_comb_fmri$average_corr_matrix,
  result_2nd_level_cosine_40x40_eng_kendall = result_2nd_level_cosine_40x40_kendall$l_roi_cosine_matrices_40_eng_fmri$average_corr_matrix,
  result_2nd_level_cosine_40x40_spa_kendall = result_2nd_level_cosine_40x40_kendall$l_roi_cosine_matrices_40_spa_fmri$average_corr_matrix,
  result_2nd_level_z_cosine_40x40_comb_pearson = result_2nd_level_z_cosine_40x40_pearson$l_roi_cosine_z_matrices_40_comb_fmri$average_corr_matrix,
  result_2nd_level_z_cosine_40x40_eng_pearson = result_2nd_level_z_cosine_40x40_pearson$l_roi_cosine_z_matrices_40_eng_fmri$average_corr_matrix,
  result_2nd_level_z_cosine_40x40_spa_pearson = result_2nd_level_z_cosine_40x40_pearson$l_roi_cosine_z_matrices_40_spa_fmri$average_corr_matrix,
  result_2nd_level_rank_cosine_40x40_comb_spearman = result_2nd_level_rank_cosine_40x40_spearman$l_roi_cosine_ranked_matrices_40_comb_fmri$average_corr_matrix, 
  result_2nd_level_rank_cosine_40x40_eng_spearman = result_2nd_level_rank_cosine_40x40_spearman$l_roi_cosine_ranked_matrices_40_eng_fmri$average_corr_matrix,
  result_2nd_level_rank_cosine_40x40_spa_spearman = result_2nd_level_rank_cosine_40x40_spearman$l_roi_cosine_ranked_matrices_40_spa_fmri$average_corr_matrix,
  result_2nd_level_rank_cosine_40x40_comb_kendall = result_2nd_level_rank_cosine_40x40_kendall$l_roi_cosine_ranked_matrices_40_comb_fmri$average_corr_matrix,
  result_2nd_level_rank_cosine_40x40_eng_kendall = result_2nd_level_rank_cosine_40x40_kendall$l_roi_cosine_ranked_matrices_40_eng_fmri$average_corr_matrix,
  result_2nd_level_rank_cosine_40x40_spa_kendall = result_2nd_level_rank_cosine_40x40_kendall$l_roi_cosine_ranked_matrices_40_spa_fmri$average_corr_matrix,
  result_2nd_level_crossnobis_40x40_comb_pearson = result_2nd_level_crossnobis_40x40_pearson$l_roi_matrices_40_comb$average_corr_matrix,  ## crossnobis distance ##
  result_2nd_level_crossnobis_40x40_eng_pearson = result_2nd_level_crossnobis_40x40_pearson$l_roi_matrices_40_eng$average_corr_matrix,
  result_2nd_level_crossnobis_40x40_spa_pearson = result_2nd_level_crossnobis_40x40_pearson$l_roi_matrices_40_spa$average_corr_matrix,
  result_2nd_level_crossnobis_40x40_comb_spearman = result_2nd_level_crossnobis_40x40_spearman$l_roi_matrices_40_comb$average_corr_matrix,
  result_2nd_level_crossnobis_40x40_eng_spearman = result_2nd_level_crossnobis_40x40_spearman$l_roi_matrices_40_eng$average_corr_matrix,
  result_2nd_level_crossnobis_40x40_spa_spearman = result_2nd_level_crossnobis_40x40_spearman$l_roi_matrices_40_spa$average_corr_matrix,
  result_2nd_level_crossnobis_40x40_comb_kendall = result_2nd_level_crossnobis_40x40_kendall$l_roi_matrices_40_comb$average_corr_matrix,
  result_2nd_level_crossnobis_40x40_eng_kendall = result_2nd_level_crossnobis_40x40_kendall$l_roi_matrices_40_eng$average_corr_matrix,
  result_2nd_level_crossnobis_40x40_spa_kendall = result_2nd_level_crossnobis_40x40_kendall$l_roi_matrices_40_spa$average_corr_matrix
)
#View(additional_2nd_level_matrices$result_2nd_level_40x40_comb_pearson)

#### Step 9.2: Aggregate all 2nd-level ALL subjects correlation matrices (for significance testing on non-parametric methods)  #### 
additional_2nd_level_matrices_all_subjects <- list(
  result_2nd_level_40x40_comb_pearson = result_2nd_level_40x40_pearson$l_roi_corr_matrices_40_comb_fmri$all_2nd_level_corr_matrices,  ## individuals 2nd-level matrices ##
  result_2nd_level_40x40_eng_pearson = result_2nd_level_40x40_pearson$l_roi_corr_matrices_40_eng_fmri$all_2nd_level_corr_matrices,
  result_2nd_level_40x40_spa_pearson = result_2nd_level_40x40_pearson$l_roi_corr_matrices_40_spa_fmri$all_2nd_level_corr_matrices,
  result_2nd_level_40x40_comb_spearman = result_2nd_level_40x40_spearman$l_roi_corr_matrices_40_comb_fmri$all_2nd_level_corr_matrices,
  result_2nd_level_40x40_eng_spearman = result_2nd_level_40x40_spearman$l_roi_corr_matrices_40_eng_fmri$all_2nd_level_corr_matrices,
  result_2nd_level_40x40_spa_spearman = result_2nd_level_40x40_spearman$l_roi_corr_matrices_40_spa_fmri$all_2nd_level_corr_matrices,
  result_2nd_level_40x40_comb_kendall = result_2nd_level_40x40_kendall$l_roi_corr_matrices_40_comb_fmri$all_2nd_level_corr_matrices,
  result_2nd_level_40x40_eng_kendall = result_2nd_level_40x40_kendall$l_roi_corr_matrices_40_eng_fmri$all_2nd_level_corr_matrices,
  result_2nd_level_40x40_spa_kendall = result_2nd_level_40x40_kendall$l_roi_corr_matrices_40_spa_fmri$all_2nd_level_corr_matrices,
  result_2nd_level_z_40x40_comb_pearson = result_2nd_level_z_40x40_pearson$l_roi_cor_dist_z_matrices_40_comb_fmri$all_2nd_level_corr_matrices,
  result_2nd_level_z_40x40_eng_pearson = result_2nd_level_z_40x40_pearson$l_roi_cor_dist_z_matrices_40_eng_fmri$all_2nd_level_corr_matrices,
  result_2nd_level_z_40x40_spa_pearson = result_2nd_level_z_40x40_pearson$l_roi_cor_dist_z_matrices_40_spa_fmri$all_2nd_level_corr_matrices,
  result_2nd_level_rank_40x40_comb_spearman = result_2nd_level_rank_40x40_spearman$l_roi_cor_dist_ranked_matrices_40_comb_fmri$all_2nd_level_corr_matrices,
  result_2nd_level_rank_40x40_eng_spearman = result_2nd_level_rank_40x40_spearman$l_roi_cor_dist_ranked_matrices_40_eng_fmri$all_2nd_level_corr_matrices,
  result_2nd_level_rank_40x40_spa_spearman = result_2nd_level_rank_40x40_spearman$l_roi_cor_dist_ranked_matrices_40_spa_fmri$all_2nd_level_corr_matrices,
  result_2nd_level_rank_40x40_comb_kendall = result_2nd_level_rank_40x40_kendall$l_roi_cor_dist_ranked_matrices_40_comb_fmri$all_2nd_level_corr_matrices,
  result_2nd_level_rank_40x40_eng_kendall = result_2nd_level_rank_40x40_kendall$l_roi_cor_dist_ranked_matrices_40_eng_fmri$all_2nd_level_corr_matrices,
  result_2nd_level_rank_40x40_spa_kendall = result_2nd_level_rank_40x40_kendall$l_roi_cor_dist_ranked_matrices_40_spa_fmri$all_2nd_level_corr_matrices,
  result_2nd_level_fisher_40x40_comb_pearson = result_2nd_level_fisher_40x40_pearson$l_roi_fisher_matrices_40_comb$all_2nd_level_corr_matrices, ## correlation distance post fisher transf ## 
  result_2nd_level_fisher_40x40_eng_pearson = result_2nd_level_fisher_40x40_pearson$l_roi_fisher_matrices_40_eng$all_2nd_level_corr_matrices,  
  result_2nd_level_fisher_40x40_spa_pearson = result_2nd_level_fisher_40x40_pearson$l_roi_fisher_matrices_40_spa$all_2nd_level_corr_matrices,  
  result_2nd_level_fisher_40x40_comb_spearman = result_2nd_level_fisher_40x40_spearman$l_roi_fisher_matrices_40_comb$all_2nd_level_corr_matrices,
  result_2nd_level_fisher_40x40_eng_spearman = result_2nd_level_fisher_40x40_spearman$l_roi_fisher_matrices_40_eng$all_2nd_level_corr_matrices,
  result_2nd_level_fisher_40x40_spa_spearman = result_2nd_level_fisher_40x40_spearman$l_roi_fisher_matrices_40_spa$all_2nd_level_corr_matrices,
  result_2nd_level_fisher_40x40_comb_kendall = result_2nd_level_fisher_40x40_kendall$l_roi_fisher_matrices_40_comb$all_2nd_level_corr_matrices,
  result_2nd_level_fisher_40x40_eng_kendall = result_2nd_level_fisher_40x40_kendall$l_roi_fisher_matrices_40_eng$all_2nd_level_corr_matrices,
  result_2nd_level_fisher_40x40_spa_kendall = result_2nd_level_fisher_40x40_kendall$l_roi_fisher_matrices_40_spa$all_2nd_level_corr_matrices,
  result_2nd_level_cosine_40x40_comb_pearson = result_2nd_level_cosine_40x40_pearson$l_roi_cosine_matrices_40_comb_fmri$all_2nd_level_corr_matrices, ## cosine distance ##
  result_2nd_level_cosine_40x40_eng_pearson = result_2nd_level_cosine_40x40_pearson$l_roi_cosine_matrices_40_eng_fmri$all_2nd_level_corr_matrices, 
  result_2nd_level_cosine_40x40_spa_pearson = result_2nd_level_cosine_40x40_pearson$l_roi_cosine_matrices_40_spa_fmri$all_2nd_level_corr_matrices,
  result_2nd_level_cosine_40x40_comb_spearman = result_2nd_level_cosine_40x40_spearman$l_roi_cosine_matrices_40_comb_fmri$all_2nd_level_corr_matrices,
  result_2nd_level_cosine_40x40_eng_spearman = result_2nd_level_cosine_40x40_spearman$l_roi_cosine_matrices_40_eng_fmri$all_2nd_level_corr_matrices,
  result_2nd_level_cosine_40x40_spa_spearman = result_2nd_level_cosine_40x40_spearman$l_roi_cosine_matrices_40_spa_fmri$all_2nd_level_corr_matrices,
  result_2nd_level_cosine_40x40_comb_kendall = result_2nd_level_cosine_40x40_kendall$l_roi_cosine_matrices_40_comb_fmri$all_2nd_level_corr_matrices,
  result_2nd_level_cosine_40x40_eng_kendall = result_2nd_level_cosine_40x40_kendall$l_roi_cosine_matrices_40_eng_fmri$all_2nd_level_corr_matrices,
  result_2nd_level_cosine_40x40_spa_kendall = result_2nd_level_cosine_40x40_kendall$l_roi_cosine_matrices_40_spa_fmri$all_2nd_level_corr_matrices,
  result_2nd_level_z_cosine_40x40_comb_pearson = result_2nd_level_z_cosine_40x40_pearson$l_roi_cosine_z_matrices_40_comb_fmri$all_2nd_level_corr_matrices,
  result_2nd_level_z_cosine_40x40_eng_pearson = result_2nd_level_z_cosine_40x40_pearson$l_roi_cosine_z_matrices_40_eng_fmri$all_2nd_level_corr_matrices,
  result_2nd_level_z_cosine_40x40_spa_pearson = result_2nd_level_z_cosine_40x40_pearson$l_roi_cosine_z_matrices_40_spa_fmri$all_2nd_level_corr_matrices,
  result_2nd_level_rank_cosine_40x40_comb_spearman = result_2nd_level_rank_cosine_40x40_spearman$l_roi_cosine_ranked_matrices_40_comb_fmri$all_2nd_level_corr_matrices, 
  result_2nd_level_rank_cosine_40x40_eng_spearman = result_2nd_level_rank_cosine_40x40_spearman$l_roi_cosine_ranked_matrices_40_eng_fmri$all_2nd_level_corr_matrices,
  result_2nd_level_rank_cosine_40x40_spa_spearman = result_2nd_level_rank_cosine_40x40_spearman$l_roi_cosine_ranked_matrices_40_spa_fmri$all_2nd_level_corr_matrices,
  result_2nd_level_rank_cosine_40x40_comb_kendall = result_2nd_level_rank_cosine_40x40_kendall$l_roi_cosine_ranked_matrices_40_comb_fmri$all_2nd_level_corr_matrices,
  result_2nd_level_rank_cosine_40x40_eng_kendall = result_2nd_level_rank_cosine_40x40_kendall$l_roi_cosine_ranked_matrices_40_eng_fmri$all_2nd_level_corr_matrices,
  result_2nd_level_rank_cosine_40x40_spa_kendall = result_2nd_level_rank_cosine_40x40_kendall$l_roi_cosine_ranked_matrices_40_spa_fmri$all_2nd_level_corr_matrices,
  result_2nd_level_crossnobis_40x40_comb_pearson = result_2nd_level_crossnobis_40x40_pearson$l_roi_matrices_40_comb$all_2nd_level_corr_matrices,  ## crossnobis distance ##
  result_2nd_level_crossnobis_40x40_eng_pearson = result_2nd_level_crossnobis_40x40_pearson$l_roi_matrices_40_eng$all_2nd_level_corr_matrices,
  result_2nd_level_crossnobis_40x40_spa_pearson = result_2nd_level_crossnobis_40x40_pearson$l_roi_matrices_40_spa$all_2nd_level_corr_matrices,
  result_2nd_level_crossnobis_40x40_comb_spearman = result_2nd_level_crossnobis_40x40_spearman$l_roi_matrices_40_comb$all_2nd_level_corr_matrices,
  result_2nd_level_crossnobis_40x40_eng_spearman = result_2nd_level_crossnobis_40x40_spearman$l_roi_matrices_40_eng$all_2nd_level_corr_matrices,
  result_2nd_level_crossnobis_40x40_spa_spearman = result_2nd_level_crossnobis_40x40_spearman$l_roi_matrices_40_spa$all_2nd_level_corr_matrices,
  result_2nd_level_crossnobis_40x40_comb_kendall = result_2nd_level_crossnobis_40x40_kendall$l_roi_matrices_40_comb$all_2nd_level_corr_matrices,
  result_2nd_level_crossnobis_40x40_eng_kendall = result_2nd_level_crossnobis_40x40_kendall$l_roi_matrices_40_eng$all_2nd_level_corr_matrices,
  result_2nd_level_crossnobis_40x40_spa_kendall = result_2nd_level_crossnobis_40x40_kendall$l_roi_matrices_40_spa$all_2nd_level_corr_matrices
)
#View(additional_2nd_level_matrices_all_subjects$result_2nd_level_40x40_comb_pearson$result_2nd_level_corr_matrix_behav_fmri_part_80)

## CORREGIR EL 10 ANTES DE SEGUIR !!! ##
 
##### Step 10: Generate and plot a 3rd-level matrix correlation to assess methodological convergence #####

##### Step 10A: Duplicate the original list #####
matrix_list_2nd_level_results <- additional_2nd_level_matrices

##### Function to Compute cosine dissimilarity for vectorized matrices #####
f_cosine_dissimilarity <- function(vec1, vec2) {
  dot_product <- sum(vec1 * vec2)
  norm1 <- sqrt(sum(vec1^2))
  norm2 <- sqrt(sum(vec2^2))
  return(1 - (dot_product / (norm1 * norm2)))
}

#### function to compute the 3rd-level RSA analysis with cosine modification ####
f_3rd_level_similarity <- function(matrix_list, method = "pearson", triangle = "lower", use_cosine = FALSE, remove_diagonal = TRUE) {
  triangle <- match.arg(triangle, choices = c("lower", "full"))
  n <- length(matrix_list)
  
  # Extract vector from each matrix
  vec_list <- lapply(matrix_list, function(mat) {
    if (triangle == "lower") {
      vec <- mat[lower.tri(mat)]
    } else {
      vec <- as.vector(mat)
      if (remove_diagonal) {
        # remove diagonal manually
        vec <- vec[-which(rep(1:nrow(mat), each = nrow(mat)) == rep(1:nrow(mat), times = nrow(mat)))]
      }
    }
    return(vec)
  })
  
  # Initialize result matrix
  result_mat <- matrix(NA, n, n)
  rownames(result_mat) <- names(matrix_list)
  colnames(result_mat) <- names(matrix_list)
  
  for (i in 1:n) {
    for (j in i:n) {
      sim <- if (use_cosine) {
        f_cosine_dissimilarity(vec_list[[i]], vec_list[[j]])
      } else {
        cor(vec_list[[i]], vec_list[[j]], method = method)
      }
      result_mat[i, j] <- sim
      result_mat[j, i] <- sim
    }
  }
  
  return(result_mat)
}

## Initialize storage list ##
results_3rd_level_all <- list()

## Define parameter sets ##
methods <- c("pearson", "spearman", "kendall")
triangles <- c("lower", "full")
diagonals <- c(TRUE, FALSE)

## Step 10C: Apply for both correlation distance and cosine ##
for (method in methods) {
  for (tri in triangles) {
    for (remove_diag in diagonals) {
      
      key_corr <- paste0("res_corrdist_", method, "_", tri, ifelse(remove_diag, "_nodiag", "_withdiag"))
      key_cos <- paste0("res_cosinedist_", method, "_", tri, ifelse(remove_diag, "_nodiag", "_withdiag"))
      
      # 1. Correlation distance
      results_3rd_level_all[[key_corr]] <- f_3rd_level_similarity(
        matrix_list_2nd_level_results, method = method, triangle = tri, use_cosine = FALSE, remove_diagonal = remove_diag)
      
      # 2. Cosine dissimilarity
      results_3rd_level_all[[key_cos]] <- f_3rd_level_similarity(
        matrix_list_2nd_level_results, method = method, triangle = tri, use_cosine = TRUE, remove_diagonal = remove_diag)
    }
  }
}

#### Function to plot 3rd-level RSA ####
f_plot_3rd_level_result <- function(results_list, key_name, title = NULL) {
  if (!(key_name %in% names(results_list))) {
    stop(paste("Key", key_name, "not found in the results list."))
  }
  
  mat <- results_list[[key_name]]
  long_data <- f_convert_corr_matrix_to_long_format(mat)
  
  if (is.null(title)) {
    title <- paste("3rd-Level RSA Heatmap:", key_name)
  }
  
  heatmap_plot <- f_create_heatmap(long_data, title)
  print(heatmap_plot)
  
  return(invisible(heatmap_plot))
}

## Plot all the (12) 3rd-level graphs - methods, triangle, and diagonal parameters ##
for (key in names(results_3rd_level_all)) {
  f_plot_3rd_level_result(results_3rd_level_all, key)
}

### P.C: Overall, it illustrates the unreliability of crossnobis and Fisher-pearson, and relates more 
### spearman with Kendall as expected but leaves pearson very related to it.  

##### Step 11: Rename the matrices for a better visualization on MDS plot #####

#### Function to dynamically generate short labels for 40x40 matrices ####
f_generate_short_labels_40x40 <- function(names) {
  # Initialize an empty named character vector to store short labels
  short_labels <- character(length(names))
  names(short_labels) <- names  # Assign the original names as names of the vector
  
  # Loop over each name to generate short labels dynamically
  for (i in seq_along(names)) {
    name <- names[i]
    
    # Behavioral matrices
    if (grepl("Behavioral_mean_matrix", name) || grepl("Behavioral_imputed_mean_matrix", name)) {
      # Base label for Behavioral matrices
      short_label <- "BM"
      
      # Add source (English, Spanish, Combined)
      if (grepl("eng", name)) short_label <- paste0(short_label, "E")
      if (grepl("spa", name)) short_label <- paste0(short_label, "S")
      if (grepl("comb", name)) short_label <- paste0(short_label, "C")
      
      # Add "imputed" if present
      if (grepl("imputed", name)) short_label <- paste0(short_label, "Imp")
      
      # Add specific categories if present
      if (grepl("General_Similarity", name)) short_label <- paste0(short_label, "-GS")
      if (grepl("dissimilar", name)) short_label <- paste0(short_label, "-Dis")
      if (grepl("category", name)) short_label <- paste0(short_label, "-Cat")
      if (grepl("shape", name)) short_label <- paste0(short_label, "-Sh")
      if (grepl("color", name)) short_label <- paste0(short_label, "-Co")
      if (grepl("animacy", name)) short_label <- paste0(short_label, "-An")
      if (grepl("preference", name)) short_label <- paste0(short_label, "-Pr")
      if (grepl("scare", name)) short_label <- paste0(short_label, "-Sc")
    }
    
    # fMRI ROI matrices
    else if (grepl("l_roi", name)) {
      # Extract ROI region
      roi <- ifelse(grepl("Occipital", name), "Occ",
                    ifelse(grepl("fusiform", name), "Fus",
                           ifelse(grepl("IT", name), "IT",
                                  ifelse(grepl("MT", name), "MT",
                                         ifelse(grepl("ParHipp", name), "ParHipp",
                                                ifelse(grepl("dlPFC", name), "dlPFC",
                                                       ifelse(grepl("vmPFC", name), "vmPFC",
                                                              ifelse(grepl("vlPFC", name), "vlPFC",
                                                                     ifelse(grepl("OFC", name), "OFC",
                                                                            ifelse(grepl("mPFC", name), "mPFC",
                                                                                   ifelse(grepl("Hippocampus", name), "Hippo", NA)))))))))))
      
      # Extract source (combined, English, or Spanish)
      source <- ifelse(grepl("comb", name), "Comb",
                       ifelse(grepl("eng", name), "Eng",
                              ifelse(grepl("spa", name), "Spa", NA)))
      
      # Combine ROI and source
      short_label <- paste0("ROI_", roi, "_", source)
    }
    
    # Add type of distance if present
    if (grepl("fisher", name)) {
      short_label <- paste0(short_label, "_f")
    } else if (grepl("cosine", name)) {
      short_label <- paste0(short_label, "_c")
    } else if (grepl("crossnobis", name)) {
      short_label <- paste0(short_label, "_x")
    }
    # Default case (correlation distance): no additional suffix
    
    # If no matching rule is found, set to "Unknown"
    if (short_label == "") {
      short_label <- "Unknown"
    }
    
    # Assign the generated label to the output vector
    short_labels[i] <- short_label
  }
  
  # Return the named vector of short labels
  return(short_labels)
}

## Empty vector to store all the short labels ##
all_short_labels_combined <- c()
## Iterate through each matrix in the list ##
for (matrix_name in names(additional_2nd_level_matrices)) {
  # Get the current matrix
  current_matrix <- additional_2nd_level_matrices[[matrix_name]]
  # Generate short labels for the rownames of the current matrix
  short_labels <- f_generate_short_labels_40x40(rownames(current_matrix))
  # Combine the new short labels into the overall vector
  all_short_labels_combined <- c(all_short_labels_combined, short_labels)
}
## Verify the structure ##
#print(all_short_labels_combined)


##### Step 12: Multidimensional Scaling (MDS) for 40x40 2nd-Level Matrices #####

##### Step 12.1: Load required libraries #####
library(MASS)
library(ggplot2)
library(cluster) # for silhouette
## Defined function to convert correlation matrix to dissimilarity matrix in line 509 ## 

#### Function to calculate Optimal Clustering Based on Silhouette Width ####
f_optimal_clusters <- function(dissimilarity_matrix, max_k = 10) {
  sil_width <- numeric(max_k)
  for (k in 2:max_k) {
    clustering <- hclust(as.dist(dissimilarity_matrix))
    cluster_labels <- cutree(clustering, k)
    sil <- silhouette(cluster_labels, as.dist(dissimilarity_matrix))
    sil_width[k] <- mean(sil[, 3])
  }
  optimal_k <- which.max(sil_width)
  return(optimal_k)
}

#### Function to assign categories based on short_labels ####
f_assign_label_categories <- function(short_labels) {
  categories <- rep("Other", length(short_labels))  # Default category
  for (i in seq_along(short_labels)) {
    label <- short_labels[i]
    # Assign Behavioral-Color before general Behavioral
    if (grepl("^(BMS|BMC|BME)", label) && grepl("Co$", label)) {
      categories[i] <- "Beh-Color"  
    } else if (grepl("^(BMS|BMC|BME)", label) && !grepl("(Pr|Sc)$", label)) {
      categories[i] <- "Behavioral"  
    } else if (grepl("(Pr|Sc)$", label)) {
      categories[i] <- "Subject-based"
    } else if (grepl("^ROI", label)) {
      categories[i] <- "fMRI"
    } else if (grepl("^OB-Col", label)) {
      categories[i] <- "Color(hist)"
    } else if (grepl("^OB-ColP", label)) {
      categories[i] <- "Color(prop)"
    } else if (grepl("EdgeCENG|EdgeCESP", label)) {
      categories[i] <- "Shape(Canny)"
    } else if (grepl("HED-ENG|HED-ESP", label)) {
      categories[i] <- "Shape (HED)"
    } else if (grepl("^OB-GenSim|^OB-SimENG|^OB-SimESP", label)) {  # CNN General Similarity
      categories[i] <- "CNN-General-Similarity"
    } else if (grepl("^OB-CatENG|^OB-CatESP", label)) {  # Category-Based
      categories[i] <- "Category"
    } else if (grepl("^OB-AniENG|^OB-AniESP", label)) {  # CNN Animacy
      categories[i] <- "CNN-Animacy"
    } else {
      categories[i] <- "Other"
    }
  }
  return(as.factor(categories))
}

##### Step 12.2: Define Color Mapping for MDS Categories #####
fixed_category_color_mapping <- c(
  "Behavioral" = "indianred1",
  "Subject-based" = "deepskyblue3",
  "Beh-Color" = "springgreen3",
  "fMRI" = "purple",
  "Color(hist)" = "darkorange2",
  "Color(prop)" = "darkgreen",
  "Shape(Canny)" = "navy",
  "Shape (HED)" = "saddlebrown",
  "CNN-General-Similarity" = "hotpink",  # New
  "Category" = "cyan",  # New
  "CNN-Animacy" = "goldenrod",  # New
  "Other" = "gray50"
)

#### Function to apply MDS with clustering labels ####
f_apply_mds_with_clustering <- function(corr_matrix, dims = 2, max_k = 10) {
  dissimilarity_matrix <- f_convert_to_distances(corr_matrix)
  mds_result <- cmdscale(as.dist(dissimilarity_matrix), k = dims)
  optimal_k <- f_optimal_clusters(dissimilarity_matrix, max_k)
  clustering <- hclust(as.dist(dissimilarity_matrix))
  cluster_labels <- cutree(clustering, k = optimal_k)
  return(list(mds_result = mds_result, cluster_labels = cluster_labels, optimal_k = optimal_k))
}

#### Function to plot MDS Results with Colors and Labels ####
f_plot_mds_with_label_colors <- function(mds_result, short_labels, title) {
  mds_df <- as.data.frame(mds_result)
  # Assign categories based on label patterns
  mds_df$Category <- f_assign_label_categories(short_labels)
  mds_df$Label <- short_labels  # Use short labels for text
  # Create the plot with fixed color mapping
  ggplot(mds_df, aes(x = V1, y = V2, color = Category, label = Label)) +
    geom_point(size = 3) +
    geom_text(vjust = -0.5, hjust = 1, size = 3) +
    scale_color_manual(values = fixed_category_color_mapping) +  # Use fixed colors
    labs(title = title, x = "Dimension 1", y = "Dimension 2", color = "Clusters") +
    theme_minimal() +
    theme(legend.position = "right")
}

#### Function to plot the MDS of all 2nd-level Matrices - grouped ####
f_plot_all_matrices_mds <- function(corr_matrix_list, short_labels, dims = 2, max_k = 10) {
  results_list <- list()
  for (matrix_name in names(corr_matrix_list)) {
    message("Processing matrix: ", matrix_name)
    # Extract the correlation matrix
    corr_matrix <- corr_matrix_list[[matrix_name]]
    # Check if it's square and complete
    if (!is.matrix(corr_matrix) || nrow(corr_matrix) != ncol(corr_matrix)) {
      warning(paste("Skipping", matrix_name, "- not a square matrix."))
      next
    }
    # Ensure labels match matrix dimension
    matrix_labels <- short_labels[rownames(corr_matrix)]
    # Handle potential missing labels gracefully
    if (length(matrix_labels) != nrow(corr_matrix)) {
      warning(paste("Skipping", matrix_name, "- label mismatch."))
      next
    }
    # Apply MDS + clustering + plot
    mds_result <- f_apply_mds_with_clustering(corr_matrix, dims = dims, max_k = max_k)
    plot <- f_plot_mds_with_label_colors(mds_result$mds_result, matrix_labels, matrix_name)
    # Save both results and plots into a list
    results_list[[matrix_name]] <- list(
      mds_result = mds_result,
      plot = plot
    )
  }
  return(results_list)
}

##### Step 12.3 Run MDS to All matrices #####
####  Estimate and plot the MDS ###
mds_results_all_40x40 <- f_plot_all_matrices_mds(
  corr_matrix_list = additional_2nd_level_matrices,
  short_labels = all_short_labels_combined
)

##### Step 13: MDS on Filtered 40x40 Matrices for Correlation, Cosine and crossnobis distances #####

##### Step 13.1: Filtered MDS Function #####
f_apply_mds_with_filtered_data <- function(corr_matrix, filtered_corr_matrix, short_labels, title, dims = 2, max_k = 10) {
  # Step 1: Extract the filtered row and column names (they might be different)
  filtered_row_names <- rownames(filtered_corr_matrix)
  filtered_col_names <- colnames(filtered_corr_matrix)
  # Step 2: Combine both row and column names to make a complete list for the square matrix
  combined_names <- union(filtered_row_names, filtered_col_names)  # Union of both rows and columns
  # Step 3: Subset the original correlation matrix using the combined names for both rows and columns
  filtered_corr_matrix_full <- corr_matrix[combined_names, combined_names]
  # Step 4: Apply the existing MDS function on the filtered square matrix
  mds_result <- f_apply_mds_with_clustering(filtered_corr_matrix_full, dims = dims, max_k = max_k)
  # Step 5: Plot the MDS results with the short labels
  plot <- f_plot_mds_with_label_colors(mds_result$mds_result, short_labels[combined_names], title)
  # Step 6: Return the MDS results and the plot
  return(list(mds_result = mds_result, plot = plot))
}

##### Step 13.2: Run Filtered MDS (40x40) on combined for Correlation, Cosine, and Crossnobis ##### ## voy aca corriendo corregir para guardar el workspace!!! ###

### Correlation distance ###
## Pearson ##
mds_filtered_behavioral_comb_2nd_level_pearson <- f_apply_mds_with_filtered_data(result_2nd_level_40x40_pearson$l_roi_corr_matrices_40_comb_fmri$average_corr_matrix,
                                                                      filtered_corr_matrix_behav_comb_40x40_pearson$l_roi_corr_matrices_40_comb_fmri$filtered_corr_matrix,
                                                                      all_short_labels_combined,
                                                                       "MDS Plot for Filtered 40x40 Matrices (c.d & Pearson)")
#print(mds_filtered_behavioral_comb_2nd_level_pearson)

## Spearman ##
mds_filtered_behavioral_comb_2nd_level_spearman <- f_apply_mds_with_filtered_data(result_2nd_level_40x40_spearman$l_roi_corr_matrices_40_comb_fmri$average_corr_matrix,
                                                                        filtered_corr_matrix_behav_comb_40x40_pearson$l_roi_corr_matrices_40_comb_fmri$filtered_corr_matrix,
                                                                        all_short_labels_combined,
                                                                        "MDS Plot for Filtered 40x40 Matrices (c.d & Spearman)")
#print(mds_filtered_behavioral_comb_2nd_level_spearman$plot)

## Kendall ##
mds_filtered_behavioral_comb_2nd_level_kendall <- f_apply_mds_with_filtered_data(result_2nd_level_40x40_kendall$l_roi_corr_matrices_40_comb_fmri$average_corr_matrix,
                                                                        filtered_corr_matrix_behav_comb_40x40_kendall$l_roi_corr_matrices_40_comb_fmri$filtered_corr_matrix,
                                                                        all_short_labels_combined,
                                                                        "MDS Plot for Filtered 40x40 Matrices (c.d & kendall)")
#print(mds_filtered_behavioral_comb_2nd_level_kendall$plot)

### Cosine Similarity Distance ###
## Pearson ##
mds_filtered_behavioral_comb_2nd_level_cosine_pearson <- f_apply_mds_with_filtered_data(result_2nd_level_cosine_40x40_pearson$l_roi_cosine_matrices_40_comb_fmri$average_corr_matrix,
                                                                                 filtered_corr_matrix_cosine_behav_comb_40x40_pearson$l_roi_cosine_matrices_40_comb_fmri$filtered_corr_matrix,
                                                                                 all_short_labels_combined,
                                                                                 "MDS Plot for Filtered 40x40 Matrices (cosine similarity & Pearson)")
#print(mds_filtered_behavioral_comb_2nd_level_cosine_pearson$plot)

## Spearman ##
mds_filtered_behavioral_comb_2nd_level_cosine_spearman <- f_apply_mds_with_filtered_data(result_2nd_level_cosine_40x40_spearman$l_roi_cosine_matrices_40_comb_fmri$average_corr_matrix,
                                                                                  filtered_corr_matrix_cosine_behav_comb_40x40_spearman$l_roi_cosine_matrices_40_comb_fmri$filtered_corr_matrix,
                                                                                  all_short_labels_combined,
                                                                                  "MDS Plot for Filtered 40x40 Matrices (cosine similarity & Spearman)")
#print(mds_filtered_behavioral_comb_2nd_level_cosine_spearman$plot)
## Kendall ##
mds_filtered_behavioral_comb_2nd_level_cosine_kendall <- f_apply_mds_with_filtered_data(result_2nd_level_cosine_40x40_kendall$l_roi_cosine_matrices_40_comb_fmri$average_corr_matrix,
                                                                                 filtered_corr_matrix_cosine_behav_comb_40x40_spearman$l_roi_cosine_matrices_40_comb_fmri$filtered_corr_matrix,
                                                                                 all_short_labels_combined,
                                                                                 "MDS Plot for Filtered 40x40 Matrices (cosine similarity & kendall)")
print(mds_filtered_behavioral_comb_2nd_level_cosine_kendall$plot)

### Crossnobis distance ### 
## Pearson ##
mds_filtered_behavioral_comb_2nd_level_crossnobis_pearson <- f_apply_mds_with_filtered_data(result_2nd_level_crossnobis_40x40_pearson$l_roi_matrices_40_comb$average_corr_matrix,
                                                                        filtered_corr_matrix_crossnobis_behav_comb_40x40_pearson$l_roi_matrices_40_comb$filtered_corr_matrix,
                                                                        all_short_labels_combined,
                                                                        "MDS Plot for Filtered 40x40 Matrices - 40x40 Matrices (crossnobis & Pearson)")
#print(mds_filtered_behavioral_comb_2nd_level_crossnobis_pearson$plot)
## Spearman ##
mds_filtered_behavioral_comb_2nd_level_crossnobis_spearman <- f_apply_mds_with_filtered_data(result_2nd_level_crossnobis_40x40_spearman$l_roi_matrices_40_comb$average_corr_matrix,
                                                                        filtered_corr_matrix_crossnobis_behav_comb_40x40_spearman$l_roi_matrices_40_comb$filtered_corr_matrix ,
                                                                        all_short_labels_combined,
                                                                        "MDS Plot for Filtered 40x40 Matrices (crossnobis & Spearman)")
#print(mds_filtered_behavioral_comb_2nd_level_crossnobis_spearman$plot)

## Kendall ##
mds_filtered_behavioral_comb_2nd_level_kendall <- f_apply_mds_with_filtered_data(result_2nd_level_crossnobis_40x40_kendall$l_roi_matrices_40_comb$average_corr_matrix,
                                                                                 filtered_corr_matrix_crossnobis_behav_comb_40x40_kendall$l_roi_matrices_40_comb$filtered_corr_matrix,
                                                                                 all_short_labels_combined,
                                                                                 "MDS Plot for Filtered 40x40 Matrices (crossnobis & kendall)")
#print(mds_filtered_behavioral_comb_2nd_level_kendall$plot)


##### Step 14: Visualization of the fMRI matricesin cosine Dissimilarity distance #####

#### Step 14.1 Load and set up requirements ####
library(lattice) # Load required library
palette_5colors <- colorRampPalette(c('blue', 'cyan', 'yellow', 'orange', 'red'))(100) # Define the color palette
category_names <- c("snake","Frog","Cockroach","Ants","spider","Worms","Bee","Butterflies","Birds","Bat","Chicken",
                    "Mouse","dog","Cat","Horses","Shark","Fish","Gecko","Turtle","Beatle","Grasshoper","Catterpillar",
                    "Fly","Peacock","Guinea pig","Sheep","Rabbit","Giraffe","Whale","Dolphin","Airplanes","Car","Bicycle",
                    "Scissor","Hammer","Key","Guitar","Cellphone","Umbrella","Chair") 


#### Function to process and plot cosine dissimilarity matrices ####     
f_plot_cosine_dissimilarity <- function(matrix_list, category_names, roi_names, palette) {
  plots <- list()
  
  for (roi_index in seq_along(matrix_list)) {
    roi_name <- roi_names[roi_index]
    cos_diss_matrix <- matrix_list[[roi_index]]  # Already dissimilarity
    
    # Set proper dimension names for plotting
    rownames(cos_diss_matrix) <- category_names
    colnames(cos_diss_matrix) <- category_names
    
    # Plot the precomputed cosine dissimilarity matrix
    plot <- levelplot(cos_diss_matrix,
                      col.regions = palette,
                      xlab = "Categories", ylab = "Categories",
                      main = paste("Cosine Dissimilarity Matrix -", roi_name),
                      scales = list(
                        x = list(at = seq(5, length(category_names), by = 5), 
                                 labels = seq(5, length(category_names), by = 5), 
                                 rot = 45, cex = 0.6),
                        y = list(at = 1:length(category_names),
                                 labels = category_names, cex = 0.35),
                        tck = c(0, 0)))
    
    plots[[roi_name]] <- plot
  }
  
  return(plots)
}



#### Step 14.2: Generate the fMRI cosine dissimilarity plots #####     
fmri_final_plots_cd <- f_plot_cosine_dissimilarity(l_cosine_dissim_avg_mat_per_roi_40_comb, category_names, roi_names, palette_5colors)
print(fmri_final_plots_cd$dlPFC)

#### Function to process and plot correlation distance matrices ####
f_process_and_plot_matrices_280 <- function(matrix_list, category_names, roi_names, palette) {
  cor_distance_matrices <- list()
  plots <- list()
  
  for (roi_index in seq_along(matrix_list)) {
    roi_name <- roi_names[roi_index]  # Use the provided ROI names
    
    # Select the current correlation distance matrix
    cor_dist_matrix <- matrix_list[[roi_index]]
    
    # Remove row labels, but keep column labels
    dimnames(cor_dist_matrix) <- list(NULL, category_names)
    
    # Store the correlation distance matrices in the list
    cor_distance_matrices[[roi_name]] <- cor_dist_matrix
    
    # Ensure the column and row names are properly managed for plotting
    rownames(cor_dist_matrix) <- NULL
    colnames(cor_dist_matrix) <- category_names
    
    # Create the plot without row labels, without spindles, and with specified x labels
    plot <- levelplot(cor_dist_matrix, col.regions = palette, 
                      xlab = "Exemplars", ylab = "Exemplars", main = paste("Dissimilarity Matrix of Correlational Distance -", roi_name),
                      scales = list(x = list(at = seq(20, length(category_names), by = 20), labels = seq(20, length(category_names), by = 20), rot = 45, cex = 0.6), 
                                    y = list(at = seq(20, length(category_names), by = 20), labels = seq(20, length(category_names), by = 20), cex = 0.7),
                                    tck = c(0, 0)))
    
    # Store the plot in the list
    plots[[roi_name]] <- plot
  }
  
  return(list(cor_distance_matrices = cor_distance_matrices, plots = plots))
}

#### Adjustments for 280x280 matrices #### 
category_names_280 <- rep(category_names, 7) # Extend category names to match 280x280 dimensions

#### Step 14.3: Generate the fMRI 280x280 cosine dissimilarity plots #######
fmri_final_plots_280 <- f_process_and_plot_matrices_280(l_cosine_dissim_avg_mat_per_roi_280_comb, category_names_280, roi_names, palette_5colors)
#print(fmri_final_plots_280$plots$IT)

#### Step 15: Visualization of behavioral matrices (280x280) and (40x40) ####

#### Step 15.1: Visualization of behavioral matrices 280x280 between completion methods (Fig sup 1) ####

#### Function to plot ####
f_plot_behavioral_matrices_280 <- function(matrix_list, category_names, palette, metric_name = "Cosine Dissimilarity") {
  plots <- list()
  category_names_280 <- rep(category_names, each = 7)  # 7 exemplars per category
  
  for (matrix_name in names(matrix_list)) {
    matrix_280 <- matrix_list[[matrix_name]]
    
    # Set labels
    colnames(matrix_280) <- category_names_280
    rownames(matrix_280) <- category_names_280
    
    # Create axis tick marks every 7 exemplars
    axis_ticks <- seq(7, length(category_names_280), by = 7)
    axis_labels <- category_names
    
    # Plot
    plot <- levelplot(matrix_280,
                      col.regions = palette,
                      xlab = "Exemplars", ylab = "Exemplars",
                      main = paste0(metric_name, " - ", matrix_name),
                      scales = list(
                        x = list(at = c(1, 140, 280), labels = c("1", "140", "280"), rot = 0, cex = 0.7),
                        y = list(at = axis_ticks, labels = axis_labels, cex = 0.6),
                        tck = c(0, 0)))
    
    plots[[matrix_name]] <- plot
  }
  
  return(plots)
}

## Cosine dissimilarity matrix ##
plots_behavioral_280 <- f_plot_behavioral_matrices_280(
  l_cosine_dissim_behavioral_280x280,
  category_names,
  palette_5colors,
  metric_name = "Behavioral Cosine Dissimilarity"
)
#print(plots_behavioral_280$Behavioral_imputed_matrix_spa_all_data_3)

#### Step 15.2: Visualization of all behavioral matrices across distances (Fig sup 5) ####

#### Function to plot ####
f_plot_behavioral_dissimilarity_matrices <- function(
    dissim_matrix_list,      # Named list of matrices
    category_names,          # Category labels
    metric_name,             # Label for the type of distance (for plot titles)
    palette                  # Color palette
) {
  plots <- list()
  
  for (matrix_name in names(dissim_matrix_list)) {
    dissim_matrix <- dissim_matrix_list[[matrix_name]]
    
    # Set row/col names for plotting
    rownames(dissim_matrix) <- category_names
    colnames(dissim_matrix) <- category_names
    
    # Create levelplot
    plot <- levelplot(dissim_matrix,
                      col.regions = palette,
                      xlab = "Categories", ylab = "Categories",
                      main = paste0(metric_name, " - ", matrix_name),
                      scales = list(
                        x = list(at = seq(5, length(category_names), by = 5), 
                                 labels = seq(5, length(category_names), by = 5), 
                                 rot = 45, cex = 0.6),
                        y = list(at = 1:length(category_names), 
                                 labels = category_names, cex = 0.35),
                        tck = c(0, 0)))
    
    plots[[matrix_name]] <- plot
  }
  
  return(plots)
}

## Correlation distance ##
plots_cor_dist <- f_plot_behavioral_dissimilarity_matrices(
  l_raw_cor_dist_behavioral_40x40,
  category_names,
  "Correlation Distance",
  palette_5colors
)
print(plots_cor_dist)

## Correlation distance post fisher transformation ##
plots_fisher <- f_plot_behavioral_dissimilarity_matrices(
  l_cordist_fisher_40x40,
  category_names,
  "Fisher Correlation Distance",
  palette_5colors
)
#print(plots_fisher)

## Cosine dissimilarity matrix ##
plots_cosine_diss <- f_plot_behavioral_dissimilarity_matrices(
  l_cosine_dissim_behavioral_40x40,
  category_names,
  "Cosine Dissimilarity",
  palette_5colors
)
#print(plots_cosine_diss$Behavioral_imputed_mean_matrix_eng_all_data)


## Crossnobis distance ##
plots_crossnobis <- f_plot_behavioral_dissimilarity_matrices(
  crossnobis_results,
  category_names,
  "Crossnobis Distance",
  palette_5colors
)
print(plots_crossnobis)








## ------------------------------------------------------------------------------------------
###------------- SECTION 3: Statistical Analysis Brain and Behavior --------------------- ###

##### Step 16: Significance Testing through Permutation Analysis with FDR Correction - on Average 2nd-Level Matrix #####
## P.C. Different approaches for significance testing on parametric and non-parametric correlation methods due to assumptions.
## Non-parametric: Permutation-TMAX method #####
## Rather than correcting p‑values afterward using a method like FDR or Bonferroni on cell‑wise p‑values
##,the max‑T procedure builds a null distribution based on the maximum test statistic
## (here, the maximum absolute correlation) across all cells in each permutation. 

#### Step 16.1: Load necessary libraries ####
library(boot)
library(tidyr)
library(dplyr)
library(stats)  # For cor, pt, and p.adjust functions
library(ggplot2)
library(gridExtra)

#### Step 16.2: pre-requisites functions for parametric method ####

#### Function to convert correlation matrix to long format ####
f_corr_mat_to_long_format2 <- function(corr_matrix) {
  long_format <- as.data.frame(as.table(corr_matrix))
  colnames(long_format) <- c("Row", "Column", "Value")
  # Optionally set factor levels to preserve row/col order in plots
  long_format$Row <- factor(long_format$Row, levels = rownames(corr_matrix))
  long_format$Column <- factor(long_format$Column, levels = colnames(corr_matrix))
  return(long_format)
}

#### Function to compute and adjust p-values using t-distribution (for Pearson) ####
f_compute_and_adjust_pvalues <- function(correlation_matrix, n_samples, adjustment_method = "BH") {
  mat_dim <- nrow(correlation_matrix)
  p_values <- matrix(NA, mat_dim, mat_dim)
  rownames(p_values) <- rownames(correlation_matrix)
  colnames(p_values) <- colnames(correlation_matrix)
  
  for(i in 1:mat_dim) {
    for(j in i:mat_dim) {
      if(!is.na(correlation_matrix[i, j])) {
        r <- correlation_matrix[i, j]
        t_value <- r * sqrt((n_samples - 2) / (1 - r^2))
        p_val <- 2 * (1 - pt(abs(t_value), df = n_samples - 2))
        p_values[i, j] <- p_val
        p_values[j, i] <- p_val  # enforce symmetry
      }
    }
  }
  p_values_long <- as.data.frame(as.table(p_values))
  colnames(p_values_long) <- c("Row", "Column", "P_Value")
  p_values_long <- p_values_long %>% filter(!is.na(P_Value))
  p_values_long$Adjusted_P_Value <- p.adjust(p_values_long$P_Value, method = adjustment_method)
  p_values_long$Significant <- p_values_long$Adjusted_P_Value < 0.05
  
  p_adjusted_matrix <- matrix(NA, mat_dim, mat_dim)
  rownames(p_adjusted_matrix) <- rownames(correlation_matrix)
  colnames(p_adjusted_matrix) <- colnames(correlation_matrix)
  for(k in 1:nrow(p_values_long)) {
    r_name <- as.character(p_values_long$Row[k])
    c_name <- as.character(p_values_long$Column[k])
    p_adjusted_matrix[r_name, c_name] <- p_values_long$Adjusted_P_Value[k]
    p_adjusted_matrix[c_name, r_name] <- p_values_long$Adjusted_P_Value[k]
  }
  return(list(Raw_P_Values = p_values, Adjusted_P_Values = p_adjusted_matrix, Results_Long = p_values_long))
}

#### Function for computing p-values via bootstrapping (non-parametric: Spearman/Kendall) ####
f_compute_and_adjust_pvalues_nonparametric <- function(subject_corr_list, nboot = 1000, conf.level = 0.95, adjustment_method = "BH") {
  n_subjects <- length(subject_corr_list)
  mat_dim <- nrow(subject_corr_list[[1]])
  
  p_values <- matrix(NA, mat_dim, mat_dim)
  rownames(p_values) <- rownames(subject_corr_list[[1]])
  colnames(p_values) <- colnames(subject_corr_list[[1]])
  
  ci_lower <- matrix(NA, mat_dim, mat_dim)
  ci_upper <- matrix(NA, mat_dim, mat_dim)
  rownames(ci_lower) <- rownames(subject_corr_list[[1]])
  colnames(ci_lower) <- colnames(subject_corr_list[[1]])
  rownames(ci_upper) <- rownames(subject_corr_list[[1]])
  colnames(ci_upper) <- colnames(subject_corr_list[[1]])
  
  for(i in 1:mat_dim) {
    for(j in i:mat_dim) {
      cell_values <- sapply(subject_corr_list, function(mat) mat[i, j])
      
      # Check for near-constant values (i.e., negligible variability)
      if(sd(cell_values, na.rm = TRUE) < 1e-8) {
        p_val <- NA
        ci_lower[i, j] <- NA
        ci_upper[i, j] <- NA
      } else {
        boot_mean <- function(data, indices) {
          return(mean(data[indices], na.rm = TRUE))
        }
        boot_result <- boot(data = cell_values, statistic = boot_mean, R = nboot)
        
        ci <- tryCatch({
          boot.ci(boot_result, conf = conf.level, type = "perc")
        }, error = function(e) {
          NULL
        })
        if(!is.null(ci)) {
          ci_lower[i, j] <- ci$percent[4]
          ci_upper[i, j] <- ci$percent[5]
          ci_lower[j, i] <- ci$percent[4]  # enforce symmetry
          ci_upper[j, i] <- ci$percent[5]
        } else {
          ci_lower[i, j] <- NA
          ci_upper[i, j] <- NA
          ci_lower[j, i] <- NA
          ci_upper[j, i] <- NA
        }
        p_val <- 2 * min(mean(boot_result$t >= 0), mean(boot_result$t < 0))
      }
      p_values[i, j] <- p_val
      p_values[j, i] <- p_val
    }
  }
  
  p_values_long <- as.data.frame(as.table(p_values))
  colnames(p_values_long) <- c("Row", "Column", "P_Value")
  p_values_long <- p_values_long %>% filter(!is.na(P_Value))
  p_values_long$Adjusted_P_Value <- p.adjust(p_values_long$P_Value, method = adjustment_method)
  p_values_long$Significant <- p_values_long$Adjusted_P_Value < 0.05
  
  p_adjusted_matrix <- matrix(NA, mat_dim, mat_dim)
  rownames(p_adjusted_matrix) <- rownames(p_values)
  colnames(p_adjusted_matrix) <- colnames(p_values)
  for(k in 1:nrow(p_values_long)) {
    r <- as.character(p_values_long$Row[k])
    c <- as.character(p_values_long$Column[k])
    p_adjusted_matrix[r, c] <- p_values_long$Adjusted_P_Value[k]
    p_adjusted_matrix[c, r] <- p_values_long$Adjusted_P_Value[k]
  }
  
  return(list(Raw_P_Values = p_values, Adjusted_P_Values = p_adjusted_matrix, Results_Long = p_values_long,
              CI_Lower = ci_lower, CI_Upper = ci_upper))
}

#### Function to compute the significant matrix ####
f_compute_significant_matrix <- function(corr_matrix, p_adjusted_matrix, alpha = 0.05) {
  significant_matrix <- matrix(NA, nrow = nrow(corr_matrix), ncol = ncol(corr_matrix))
  rownames(significant_matrix) <- rownames(corr_matrix)
  colnames(significant_matrix) <- colnames(corr_matrix)
  for(i in 1:nrow(corr_matrix)) {
    for(j in 1:ncol(corr_matrix)) {
      if(!is.na(p_adjusted_matrix[i, j]) && p_adjusted_matrix[i, j] < alpha) {
        significant_matrix[i, j] <- corr_matrix[i, j]
      }
    }
  }
  return(significant_matrix)
}

#### Function to create and return separate heatmaps ####
f_plot_combined_heatmaps <- function(corr_matrix, significant_matrix, title_avg, title_significant) {
  long_corr_matrix <- f_corr_mat_to_long_format2(corr_matrix)
  long_significant_matrix <- f_corr_mat_to_long_format2(significant_matrix)
  
  heatmap_avg <- ggplot(long_corr_matrix, aes(x = Column, y = Row, fill = Value)) +
    geom_tile() +
    scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, limits = c(-1, 1)) +
    labs(title = title_avg, x = "Matrices", y = "Matrices") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  heatmap_significant <- ggplot(long_significant_matrix, aes(x = Column, y = Row, fill = Value)) +
    geom_tile() +
    scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, limits = c(-1, 1), na.value = "white") +
    labs(title = title_significant, x = "Matrices", y = "Matrices") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(list(Heatmap_Original = heatmap_avg, Heatmap_Significant = heatmap_significant))
}

#### function to process correlation matrices  with both significant testing methods ####
f_process_correlation_matrices <- function(average_matrices_list, subject_matrices_list = NULL,
                                           corr_method = "pearson", n_samples = 80, 
                                           alpha = 0.05, adjustment_method = "BH") {
  results <- list()
  
  for(matrix_name in names(average_matrices_list)) {
    # Check if the matrix name includes the specified correlation method (e.g., "pearson", "spearman", or "kendall")
    if(!grepl(corr_method, matrix_name, ignore.case = TRUE)) {
      cat("Matrix", matrix_name, "is not", corr_method, ": omitting matrix\n")
      next
    }
    
    cat("Processing:", matrix_name, "\n")
    
    correlation_matrix <- average_matrices_list[[matrix_name]]
    
    if(corr_method == "pearson") {
      p_value_results <- f_compute_and_adjust_pvalues(correlation_matrix, n_samples, adjustment_method)
    } else if(corr_method %in% c("spearman", "kendall")) {
      if(is.null(subject_matrices_list)) {
        stop("Subject-level matrices are required for bootstrap significance testing with non-parametric methods.")
      }
      # Also check that the subject-level matrix name contains the method
      if(!grepl(corr_method, matrix_name, ignore.case = TRUE)) {
        cat("Subject-level matrix for", matrix_name, "is not", corr_method, ": omitting matrix\n")
        next
      }
      subject_corr_list <- subject_matrices_list[[matrix_name]]
      p_value_results <- f_compute_and_adjust_pvalues_nonparametric(subject_corr_list, nboot = 1000, adjustment_method = adjustment_method)
    } else {
      stop("Unsupported correlation method: ", corr_method)
    }
    ## Checking the matrix ##
    cat("Checking for matrix:", matrix_name, "\n")
    print(names(subject_matrices_list))
    
    p_adjusted_matrix <- p_value_results$Adjusted_P_Values
    significant_matrix <- f_compute_significant_matrix(correlation_matrix, p_adjusted_matrix, alpha)
    
    heatmaps <- f_plot_combined_heatmaps(
      corr_matrix = correlation_matrix,
      significant_matrix = significant_matrix,
      title_avg = paste("Average Correlation Matrix -", matrix_name),
      title_significant = paste("Significant -", matrix_name, "(Method:", corr_method, ", Adj. Method:", adjustment_method, ")")
    )
    
    combined_plot <- grid.arrange(heatmaps$Heatmap_Original, heatmaps$Heatmap_Significant, ncol = 2)
    
    results[[matrix_name]] <- list(
      Correlation_Matrix = correlation_matrix,
      Heatmap_Original = heatmaps$Heatmap_Original,
      Significant_Matrix = significant_matrix,
      P_Adjusted_Matrix = p_adjusted_matrix,
      Heatmap_Significant = heatmaps$Heatmap_Significant,
      Combined_Plot = combined_plot
    )
  }
  return(results)
}

#### Step 16.3: pre-requisites functions for non-parametric method - MaxT permutation test ####

#### Function to convert matrix to long format (ensuring numeric Value) ####
f_corr_mat_to_long_format2_alt <- function(corr_matrix) {
  long_format <- as.data.frame(as.table(corr_matrix))
  colnames(long_format) <- c("Row", "Column", "Value")
  long_format$Value <- as.numeric(as.character(long_format$Value))
  long_format$Row <- factor(long_format$Row, levels = rownames(corr_matrix))
  long_format$Column <- factor(long_format$Column, levels = colnames(corr_matrix))
  return(long_format)
}

#### Max-T permutation test for a single list of subject-level correlation matrices ####
f_permutation_maxT <- function(subject_corr_list, n_permutations = 1000) {
  n_subj <- length(subject_corr_list)
  mat_dim <- nrow(subject_corr_list[[1]])
  
  # Compute the observed group-average correlation matrix
  obs_matrix <- Reduce("+", subject_corr_list) / n_subj
  
  # Preallocate an array for bootstrapped group-average matrices and vector for max-T values
  perm_matrices <- array(NA, dim = c(mat_dim, mat_dim, n_permutations))
  maxT <- numeric(n_permutations)
  
  set.seed(123)  # for reproducibility
  for (i in 1:n_permutations) {
    perm_sum <- matrix(0, nrow = mat_dim, ncol = mat_dim)
    # For each subject, randomly flip the sign and add the matrix
    for (s in 1:n_subj) {
      flip <- sample(c(1, -1), size = 1)
      perm_sum <- perm_sum + subject_corr_list[[s]] * flip
    }
    perm_avg <- perm_sum / n_subj
    perm_matrices[,,i] <- perm_avg
    maxT[i] <- max(abs(perm_avg))
  }
  
  # Compute cell-wise p-values: For each cell, p = proportion of permutations where maxT >= |observed value|
  p_matrix <- matrix(NA, nrow = mat_dim, ncol = mat_dim)
  for (r in 1:mat_dim) {
    for (c in 1:mat_dim) {
      p_matrix[r, c] <- mean(maxT >= abs(obs_matrix[r, c]))
    }
  }
  rownames(p_matrix) <- rownames(obs_matrix)
  colnames(p_matrix) <- colnames(obs_matrix)
  
  return(list(observed = obs_matrix, p_values = p_matrix, perm_matrices = perm_matrices, maxT = maxT))
}

#### Function to create a heatmap from a matrix ####
f_create_heatmap <- function(matrix, title) {
  long_mat <- f_corr_mat_to_long_format2_alt(matrix)
  p <- ggplot(long_mat, aes(x = Column, y = Row, fill = Value)) +
    geom_tile() +
    scale_fill_gradient2(low = "red", mid = "white", high = "blue",
                         midpoint = 0, limits = c(-1,1), na.value = "white") +
    labs(title = title, x = "Matrices", y = "Matrices") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  return(p)
}

#### Function to process list of matrices using the max-T permutation approach ####
f_process_matrices_maxT <- function(average_matrices_list, subject_matrices_list, n_permutations = 1000, alpha = 0.05) {
  results <- list()
  start_time <- Sys.time()
  
  for (matrix_name in names(average_matrices_list)) {
    cat("Processing:", matrix_name, "...\n")
    # Retrieve the observed (group-average) correlation matrix and the corresponding subject-level list
    if (!matrix_name %in% names(subject_matrices_list)) {
      cat("  --> No matching subject-level data for", matrix_name, "; skipping.\n")
      next
    }
    subj_list <- subject_matrices_list[[matrix_name]]
    obs_matrix <- average_matrices_list[[matrix_name]]
    
    # Run the max-T permutation test
    perm_results <- f_permutation_maxT(subj_list, n_permutations)
    p_matrix <- perm_results$p_values
    
    # Determine significant indices (cells with p < alpha)
    sig_indices <- which(p_matrix < alpha, arr.ind = TRUE)
    sig_matrix <- matrix(NA, nrow = nrow(obs_matrix), ncol = ncol(obs_matrix))
    rownames(sig_matrix) <- rownames(obs_matrix)
    colnames(sig_matrix) <- colnames(obs_matrix)
    if(nrow(sig_indices) > 0) {
      for (k in seq_len(nrow(sig_indices))) {
        r <- sig_indices[k, 1]
        c <- sig_indices[k, 2]
        sig_matrix[r, c] <- obs_matrix[r, c]
      }
    }
    
    
    # Generate heatmaps for the observed matrix and for the significant matrix
    heatmap_obs <- f_create_heatmap(obs_matrix, paste("Observed Group-Average:", matrix_name))
    
    # Skip plotting sig_matrix if it is all NA
    if (all(is.na(sig_matrix))) {
      cat("  --> No significant cells found (sig_matrix all NA); skipping plot.\n")
      heatmap_sig <- ggplot() + ggtitle(paste("No significant cells for", matrix_name)) + theme_void()
    } else {
      heatmap_sig <- f_create_heatmap(sig_matrix, paste("Significant (max-T) for", matrix_name))
    }
    
    #heatmap_sig <- f_create_heatmap(sig_matrix, paste("Significant (max-T) for", matrix_name))
    combined_plot <- grid.arrange(heatmap_obs, heatmap_sig, ncol = 2)
    
    results[[matrix_name]] <- list(
      Observed_Matrix = obs_matrix,
      P_Values = p_matrix,
      Significant_Matrix = sig_matrix,
      Heatmap_Observed = heatmap_obs,
      Heatmap_Significant = heatmap_sig,
      Combined_Plot = combined_plot,
      Perm_Matrices = perm_results$perm_matrices,
      MaxT = perm_results$maxT
    )
    
    cat("  --> Number of significant cells (p < ", alpha, "):", sum(p_matrix < alpha, na.rm = TRUE), "\n")
  }
  
  # End time and calculate duration
  end_time <- Sys.time()
  duration <- difftime(end_time, start_time, units = "secs")
  print(paste("Total execution time:", round(duration, 2), "seconds"))
  return(results)
}

#### Step 16.4: Significance testing on parametric method correlation methods (pearson) #####
# For Pearson correlations:
FDR_significance_results_2nd_level_matrices_pearson <- f_process_correlation_matrices(
  average_matrices_list = additional_2nd_level_matrices,
  corr_method = "pearson",
  n_samples = 80,
  alpha = 0.05,
  adjustment_method = "BH"
)
#grid.arrange(FDR_significance_results_2nd_level_matrices_pearson$result_2nd_level_cosine_40x40_comb_pearson$Combined_Plot)


#### Step 16.5: Significance testing on non-parametric methods (spearman/kendall) - max-T permutation test ####
# additional_2nd_level_matrices: list of observed average correlation matrices
# additional_2nd_level_matrices_all_subjects: list of corresponding subject-level correlation matrices
MaxT_Results <- f_process_matrices_maxT(
  average_matrices_list = additional_2nd_level_matrices,
  subject_matrices_list = additional_2nd_level_matrices_all_subjects,
  n_permutations = 5000,
  alpha = 0.05
)
#grid.arrange(MaxT_Results$result_2nd_level_40x40_comb_spearman$Combined_Plot)

#### Step 16.6: Plotting the p-adjusted matrix with p-values displayed for supplemental ####

#### Plotting the p-adjusted matrix with p-values displayed ####
f_plot_p_adjusted_matrix_text <- function(p_adjusted_matrix, title = "Adjusted P-Values Matrix") {
  # Convert matrix to long format
  long_p_matrix <- f_corr_mat_to_long_format2(p_adjusted_matrix)  # Convert to long format
  
  # Round p-values for better readability
  long_p_matrix$Value <- round(long_p_matrix$Value, 3)
  
  # Create the heatmap with text labels
  heatmap_p_values <- ggplot(long_p_matrix, aes(x = Column, y = Row)) +
    geom_tile(fill = "white", color = "black") +  # White background with black grid
    geom_text(aes(label = Value), size = 1.5) +  # Overlay p-values as text ## 1.5 for multimodal; 3 for bimodal
    labs(title = title, x = "Matrices", y = "Matrices") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.major = element_blank(),  # Remove major grid lines
          panel.grid.minor = element_blank())  # Remove minor grid lines
  
  return(heatmap_p_values)
}

## Cosine - Pearson ##
heatmap_p_adj_cosine_pearson <- f_plot_p_adjusted_matrix_text(FDR_significance_results_2nd_level_matrices_pearson$result_2nd_level_cosine_40x40_comb_pearson$P_Adjusted_Matrix, 
                                           title = "FDR-Adjusted P-Values - Cosine Pearson")
#print(heatmap_p_adj_cosine_pearson)

## Cosine - Spearman ##
heatmap_p_adj_cosine_spearman <- f_plot_p_adjusted_matrix_text(MaxT_Results$result_2nd_level_cosine_40x40_comb_spearman$P_Values, 
                                                              title = "MaxT-results P-Values - Cosine Spearman")

## --------------------------------------------------------------------------------------
###------ SECTION 4: Noise ceilings fMRI (reliability estimate fMRI)  ----------------###

## Order: Carlin et al.2017: corr coeff -> z-transform -> noise ceiling - untransform ##

##### Step 17: Noise Ceiling Estimation in fMRI Matrices #####

#### Step 17.1: Load required libraries ####
library(purrr)
library(ggplot2)
library(dplyr)

#### Step 17.2: Prepare transformed matrices ####
## Combine 40x40 and 280x280 Z-transformed matrices ##
l_z_transformed_matrices_fmri <- c(l_z_transformed_matrices_40_fmri, l_z_transformed_matrices_280_fmri)

## Combine 40x40 and 280x280 rank-transformed matrices for spearman version on Noise ceilings (exploratory) ##
l_rank_transformed_matrices_fmri <- c(l_ranked_matrices_40_fmri, l_ranked_matrices_280_fmri)

#### Function to compute noise ceiling for single ROIs ####
f_compute_noise_ceiling <- function(roi_matrices, roi_name, correlation_method = "pearson") {
  n_participants <- length(roi_matrices)
  # Convert list of matrices to an array for easier manipulation
  matrix_dims <- dim(roi_matrices[[1]])
  data_array <- array(unlist(roi_matrices), dim = c(matrix_dims[1], matrix_dims[2], n_participants))
  # Compute the average RDM across participants
  avg_rdm <- apply(data_array, c(1, 2), mean)
  # Initialize vectors to store correlations
  upper_corrs <- numeric(n_participants)
  lower_corrs <- numeric(n_participants)
  
  # Calculate upper and lower bounds
  for (i in 1:n_participants) {
    # Participant's RDM
    participant_rdm <- data_array[, , i]
    
    # Upper bound: Correlation with average RDM
    upper_corrs[i] <- cor(as.vector(participant_rdm), as.vector(avg_rdm), method = correlation_method)
    
    # Lower bound: Leave-one-out average RDM
    loo_avg_rdm <- apply(data_array[, , -i], c(1, 2), mean)
    lower_corrs[i] <- cor(as.vector(participant_rdm), as.vector(loo_avg_rdm), method = correlation_method)
  }
  # Compute mean correlations
  upper_bound <- mean(upper_corrs)
  lower_bound <- mean(lower_corrs)
  # Compute the real average correlation across all pairs of participant matrices
  pairwise_corrs <- combn(n_participants, 2, function(idx) {
    cor(as.vector(data_array[, , idx[1]]), as.vector(data_array[, , idx[2]]), method = correlation_method)
  })
  real_average <- mean(pairwise_corrs)
  # Compute the difference between the lower bound and the real average
  lower_bound_diff_real_avg <- lower_bound - real_average
  # Compute the mean between upper and lower bounds
  mean_upper_lower <- (upper_bound + lower_bound) / 2
  
  # Print progress for each ROI
  cat("Processed ROI:", roi_name, "- Upper Bound:", round(upper_bound, 3), 
      "- Lower Bound:", round(lower_bound, 3), "- Real Average:", round(real_average, 3), 
      "- Diff (Lower - Real Avg):", round(lower_bound_diff_real_avg, 3), "\n")
  
  # Return results as a list
  return(list(
    upper_bound = upper_bound,
    lower_bound = lower_bound,
    real_average = real_average,
    avg_rdm = avg_rdm,
    lower_bound_diff_real_avg = lower_bound_diff_real_avg,  # Include the difference
    mean_bound = mean_upper_lower # Include the mean between bounds
  ))
}

#### Function to process all ROIs within a sublist and print progress for each list ####
process_sublist <- function(sublist, list_name, correlation_method) {
  roi_names <- names(sublist)
  results <- list()
  cat("\nProcessing list:", list_name, "\n")
  list_start_time <- Sys.time()  # Start time for the list
  
  for (roi in roi_names) {
    roi_matrices <- sublist[[roi]]
    results[[roi]] <- f_compute_noise_ceiling(roi_matrices, roi, correlation_method = correlation_method)
  }
  
  list_duration <- difftime(Sys.time(), list_start_time, units = "secs")
  cat("Completed list:", list_name, "- Duration:", round(list_duration, 2), "seconds\n")
  return(results)
}

#### Main function to process all lists ####
f_calculate_noise_ceiling_all <- function(l_roi_matrices_fmri, correlation_method = "pearson") {
  overall_start_time <- Sys.time()  # Start time for overall process
  
  # Process each of the 6 main lists
  noise_ceiling_results <- list()
  list_names <- names(l_roi_matrices_fmri)
  
  for (list_name in list_names) {
    sublist <- l_roi_matrices_fmri[[list_name]]
    noise_ceiling_results[[list_name]] <- process_sublist(sublist, list_name, correlation_method = correlation_method)
  }
  
  # Calculate and print the total duration
  overall_duration <- difftime(Sys.time(), overall_start_time, units = "secs")
  cat("\nOverall duration for noise ceiling calculation:", round(overall_duration, 2), "seconds\n")
  
  return(noise_ceiling_results)
}

##### Step 17.3: Estimate Noise Ceilings in fMRI matrices #####

#### 40x40 - Correlation Distance ####
### Raw Data ###
noise_ceiling_results_40_raw <- f_calculate_noise_ceiling_all(l_cor_dist_matrices_40_fmri, correlation_method = "pearson") # 30 - 55 seconds 
noise_ceiling_results_40_raw_spearman <- f_calculate_noise_ceiling_all(l_cor_dist_matrices_40_fmri, correlation_method = "spearman") # 45-80 seconds 
noise_ceiling_results_40_raw_kendall <- f_calculate_noise_ceiling_all(l_cor_dist_matrices_40_fmri, correlation_method = "kendall") # 2800 seconds 

#### 280x280 - Exploratory ####
noise_ceiling_results_280_raw <- f_calculate_noise_ceiling_all(l_cor_dist_matrices_280_fmri, correlation_method = "pearson") # 1350-1400 seconds  
noise_ceiling_results_280_raw_spearman <- f_calculate_noise_ceiling_all(l_cor_dist_matrices_280_fmri, correlation_method = "spearman") # 5000 seconds 

### Transformed data ###
## Z-transformed ##  
noise_ceiling_results_z_transformed_40 <- f_calculate_noise_ceiling_all(l_cor_dist_z_matrices_40_fmri, correlation_method = "pearson") # 40-45 seconds  
## Spearman - rank-transformed ##
noise_ceiling_results_rank_transformed_40_spearman <- f_calculate_noise_ceiling_all(l_cor_dist_ranked_matrices_40_fmri, correlation_method = "spearman") # 60-65 seconds 
## Kendall - rank-transformed data ##  
noise_ceiling_results_rank_transformed_40_kendall <- f_calculate_noise_ceiling_all(l_cor_dist_ranked_matrices_40_fmri, correlation_method = "kendall") # 2800 seconds

#### Correlation Distance - Fisher Transformed #### 
noise_ceiling_results_40_fisher <- f_calculate_noise_ceiling_all(l_fisher_matrices_40, correlation_method = "pearson") # 30-35 seconds 
noise_ceiling_results_40_fisher_spearman <- f_calculate_noise_ceiling_all(l_fisher_matrices_40, correlation_method = "spearman") # 60-65 seconds
noise_ceiling_results_40_fisher_kendall <- f_calculate_noise_ceiling_all(l_fisher_matrices_40, correlation_method = "kendall") # 2800 seconds

#### Cosine similarity distance ####
## Raw Data ##
noise_ceiling_results_40_cosine <- f_calculate_noise_ceiling_all(l_cosine_dissim_similarity_matrices_40_fmri, correlation_method = "pearson") # 30-45 seconds 
noise_ceiling_results_40_cosine_spearman <- f_calculate_noise_ceiling_all(l_cosine_dissim_similarity_matrices_40_fmri, correlation_method = "spearman") # 70-100 seconds 
noise_ceiling_results_40_cosine_kendall <- f_calculate_noise_ceiling_all(l_cosine_dissim_similarity_matrices_40_fmri, correlation_method = "kendall") # 3350 seconds

## Transformed data ##
noise_ceiling_results_cosine_z_transformed_40 <- f_calculate_noise_ceiling_all(l_cosine_dissim_z_matrices_40_fmri, correlation_method = "pearson") # 30-35 seconds 
noise_ceiling_results_cosine_rank_transformed_40_spearman <- f_calculate_noise_ceiling_all(l_cosine_dissim_ranked_matrices_40_fmri, correlation_method = "spearman") # 60-65 seconds
noise_ceiling_results_cosine_rank_transformed_40_kendall <- f_calculate_noise_ceiling_all(l_cosine_dissim_ranked_matrices_40_fmri, correlation_method = "kendall") # 2650-2700 seconds

#### Crossnobis distance ####
## Raw Data ##
noise_ceiling_results_40_crossnobis <- f_calculate_noise_ceiling_all(fmri_crossnobis_results, correlation_method = "pearson") # 150-200 seconds 
noise_ceiling_results_40_crossnobis_spearman <- f_calculate_noise_ceiling_all(fmri_crossnobis_results, correlation_method = "spearman") # 200-250 seconds 
noise_ceiling_results_40_crossnobis_kendall <- f_calculate_noise_ceiling_all(fmri_crossnobis_results, correlation_method = "kendall") # 8,000 seconds

#### Function to generate tables on noise ceilings ####
f_extract_noise_ceiling_fmri_table <- function(noise_ceiling_results, distance_type, transformation_type) {
  # Initialize a list to store the results for each ROI
  roi_data <- list()
  
  # Iterate over each list (dimension type) and each ROI within it
  for (list_name in names(noise_ceiling_results)) {
    for (roi_name in names(noise_ceiling_results[[list_name]])) {
      result <- noise_ceiling_results[[list_name]][[roi_name]]
      
      # Check if result is NULL or missing expected fields
      if (!is.null(result) && all(c("real_average", "lower_bound", "upper_bound", 
                                    "lower_bound_diff_real_avg", "mean_bound") %in% names(result))) {
        # Append each ROI's results as a row with additional identifiers
        roi_data <- append(roi_data, list(data.frame(
          Transformation = transformation_type,  # Transformation type
          Distance = distance_type,  # Distance type
          List = list_name,
          ROI = roi_name,
          Real_Average = result$real_average,
          Lower_Bound = result$lower_bound,
          Upper_Bound = result$upper_bound,
          Difference = result$lower_bound_diff_real_avg, 
          Mean_Bound = result$mean_bound
        )))
      } else {
        warning(paste("Missing or invalid data for", list_name, roi_name))
      }
    }
  }
  
  # Combine the list into a dataframe
  noise_ceiling_table <- do.call(rbind, roi_data)
  return(noise_ceiling_table)
}

#### Step 17.4: Tables with Noise Ceiling Results ####

### Correlation Distance - Raw - Pearson, Spearman and Kendall ###
noise_ceiling_table_raw_pearson_40 <- f_extract_noise_ceiling_fmri_table(noise_ceiling_results_40_raw,"Pearson","Raw")
noise_ceiling_table_raw_spearman_40 <- f_extract_noise_ceiling_fmri_table(noise_ceiling_results_40_raw_spearman,"Spearman", "Raw") 
noise_ceiling_table_raw_kendall_40 <- f_extract_noise_ceiling_fmri_table(noise_ceiling_results_40_raw_kendall, "Kendall", "Raw") 

## Transformed data: Z-transformed and rank-transformed ##
noise_ceiling_table_z_transformed_40 <- f_extract_noise_ceiling_fmri_table(noise_ceiling_results_z_transformed_40, "Pearson", "Z_Transformed")
noise_ceiling_table_rank_transformed_40_spearman <- f_extract_noise_ceiling_fmri_table(noise_ceiling_results_rank_transformed_40_spearman, "Spearman", "Rank_Transformed")
noise_ceiling_table_rank_transformed_40_kendall <- f_extract_noise_ceiling_fmri_table(noise_ceiling_results_rank_transformed_40_kendall, "Kendall", "Rank_Transformed")

### Correlation distance - Z-Fisher transformed ###
noise_ceiling_table_fisher_pearson_40 <- f_extract_noise_ceiling_fmri_table(noise_ceiling_results_40_fisher, "Pearson", "Fisher")
noise_ceiling_table_fisher_spearman_40 <- f_extract_noise_ceiling_fmri_table(noise_ceiling_results_40_fisher_spearman, "Spearman", "Fisher")
noise_ceiling_table_fisher_kendall_40 <- f_extract_noise_ceiling_fmri_table(noise_ceiling_results_40_fisher_kendall, "Kendall", "Fisher")

### Cosine similarity distance - Raw ###
noise_ceiling_table_cosine_pearson_40 <- f_extract_noise_ceiling_fmri_table(noise_ceiling_results_40_cosine, "Pearson", "Cosine")
noise_ceiling_table_cosine_spearman_40 <- f_extract_noise_ceiling_fmri_table(noise_ceiling_results_40_cosine_spearman, "Spearman", "Cosine")
noise_ceiling_table_cosine_kendall_40 <- f_extract_noise_ceiling_fmri_table(noise_ceiling_results_40_cosine_kendall, "Kendall", "Cosine")
#View(noise_ceiling_table_cosine_pearson_40)

## Transformed data ##
noise_ceiling_table_cosine_z_transformed_40 <- f_extract_noise_ceiling_fmri_table(noise_ceiling_results_cosine_z_transformed_40, "Pearson", "Z_Transformed")
noise_ceiling_table_cosine_rank_transformed_40_spearman <- f_extract_noise_ceiling_fmri_table(noise_ceiling_results_cosine_rank_transformed_40_spearman, "Spearman", "Rank_Transformed")
noise_ceiling_table_cosine_rank_transformed_40_kendall <- f_extract_noise_ceiling_fmri_table(noise_ceiling_results_cosine_rank_transformed_40_kendall, "kendall", "Rank_Transformed")

### Crossnobis distance - Raw ###
noise_ceiling_table_crossnobis_pearson_40 <- f_extract_noise_ceiling_fmri_table(noise_ceiling_results_40_crossnobis, "Pearson", "Crossnobis")
noise_ceiling_table_crossnobis_spearman_40 <- f_extract_noise_ceiling_fmri_table(noise_ceiling_results_40_crossnobis_spearman, "Spearman", "Crossnobis")
noise_ceiling_table_crossnobis_kendall_40 <- f_extract_noise_ceiling_fmri_table(noise_ceiling_results_40_crossnobis_kendall, "Kendall", "Crossnobis")

### Correlation distance - 280x280 - Raw (exploratory) ###
noise_ceiling_table_raw_pearson_280 <- f_extract_noise_ceiling_fmri_table(noise_ceiling_results_280_raw, "Pearson","Raw")
noise_ceiling_table_raw_spearman_280 <- f_extract_noise_ceiling_fmri_table(noise_ceiling_results_280_raw_spearman, "Spearman","Raw")

## RUn the code from here after confirming I have the tables from above !!! ##

#### Step 17.5: Noise Ceiling Plots with pairwise real-average correlation within ROI across subjects ####

#### Function for bar plots of noise ceiling ####
f_plot_noise_ceiling_fmri <- function(noise_ceiling_table, 
                                      plot_title = "Noise Ceiling Comparison FMRI - (Pearson)", 
                                      y_limits = c(0.5, 1), 
                                      y_breaks = seq(0.5, 1, by = 0.1)) {
  
  # Add Color and Label columns for conditional coloring and legend
  noise_ceiling_table <- noise_ceiling_table %>%
    mutate(
      Color = case_when(
        grepl("comb", List, ignore.case = TRUE) ~ "darkcyan",
        grepl("eng", List, ignore.case = TRUE) ~ "darkblue",
        grepl("spa", List, ignore.case = TRUE) ~ "lightcoral",
        TRUE ~ "grey"
      ),
      Label = case_when(
        grepl("comb", List, ignore.case = TRUE) ~ "Combined",
        grepl("eng", List, ignore.case = TRUE) ~ "English",
        grepl("spa", List, ignore.case = TRUE) ~ "Spanish",
        TRUE ~ "Other"
      )
    )
  
  # Convert ROI to factor with the desired order
  roi_names <- c('Occipital', 'fusiform', 'IT', 'MT', 'ParHipp', 'dlPFC', 'vmPFC', 'vlPFC', 'OFC', 'mPFC', 'Hippocampus')
  noise_ceiling_table$ROI <- factor(noise_ceiling_table$ROI, levels = roi_names)
  
  # Generate the final plot with coord_cartesian for y-axis range
  final_plot <- ggplot(noise_ceiling_table, aes(x = ROI, y = Real_Average, fill = Label)) +
    geom_bar(stat = "identity", position = position_dodge(), alpha = 0.6) +
    geom_errorbar(aes(ymin = Lower_Bound, ymax = Upper_Bound), width = 0.2, position = position_dodge(0.9)) +
    annotate("rect", xmin = -Inf, xmax = Inf, 
             ymin = min(noise_ceiling_table$Lower_Bound, na.rm = TRUE), 
             ymax = max(noise_ceiling_table$Upper_Bound, na.rm = TRUE), 
             fill = "grey", alpha = 0.2) +
    labs(title = plot_title,
         x = "ROI",
         y = "Correlation",
         fill = "Data Type") +
    scale_fill_manual(
      values = c("Combined" = "darkcyan", "English" = "darkblue", "Spanish" = "lightcoral")
    ) +
    coord_cartesian(ylim = y_limits) +  # Use coord_cartesian to set y-axis limits
    scale_y_continuous(breaks = y_breaks) +  # Customizable y-axis ticks
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major.y = element_line(color = "grey90"),  # Keep major grid lines on the y-axis
      panel.grid.minor.y = element_blank(),  # Remove minor grid lines on the y-axis
      panel.grid.major.x = element_blank(),  # Remove major grid lines on the  x-axis
      panel.grid.minor.x = element_blank()   # Optional: Remove minor grid lines on x-axis if present
    )
  
  # Print the plot 
  print(final_plot)
  # Return the final plot
  return(final_plot)
  }


### Correlation Distance ###
## Pearson, Spearman and Kendall ##
plot_noise_ceiling_fmri_40_pearson <- f_plot_noise_ceiling_fmri(noise_ceiling_table_raw_pearson_40, 
                                        plot_title = "Noise Ceiling FMRI (C.D. & Pearson)",
                                        y_limits = c(0.1, 1),
                                        y_breaks = seq(0.1, 1, by = 0.2))

plot_noise_ceiling_fmri_40_spearman <- f_plot_noise_ceiling_fmri(noise_ceiling_table_raw_spearman_40, 
                                                                plot_title = "Noise Ceiling FMRI (C.D. & Spearman)",
                                                                y_limits = c(0.1, 1),
                                                                y_breaks = seq(0.1, 1, by = 0.2))

plot_noise_ceiling_fmri_40_kendall <- f_plot_noise_ceiling_fmri(noise_ceiling_table_raw_kendall_40, 
                                                                 plot_title = "Noise Ceiling FMRI (C.D. & Kendall)",
                                                                 y_limits = c(0.1, 1),
                                                                 y_breaks = seq(0.1, 1, by = 0.2))

## Exploratory graphs: 280 noise ceilings ##
plot_noise_ceiling_fmri_280_pearson <- f_plot_noise_ceiling_fmri(noise_ceiling_table_raw_pearson_280, 
                                                                plot_title = "Noise Ceiling FMRI 280 (C.D. & Pearson)",
                                                                y_limits = c(0.2, 1),
                                                                y_breaks = seq(0.2, 1, by = 0.1))

### Z-transformed and rank-transformed ###
plot_noise_ceiling_fmri_40_z_pearson <- f_plot_noise_ceiling_fmri(noise_ceiling_table_z_transformed_40, 
                                                                plot_title = "Noise Ceiling FMRI - (C.D., Z-transf & Pearson)",
                                                                y_limits = c(0.6, 1),
                                                                y_breaks = seq(0.6, 1, by = 0.1))


plot_noise_ceiling_fmri_40_spearman <- f_plot_noise_ceiling_fmri(noise_ceiling_table_rank_transformed_40_spearman, 
                                                                 plot_title = "Noise Ceiling FMRI - (C.D., rank-transf & Spearman)",
                                                                 y_limits = c(0.1, 0.8),
                                                                 y_breaks = seq(0.1, 0.8, by = 0.2))

plot_noise_ceiling_fmri_40_spearman <- f_plot_noise_ceiling_fmri(noise_ceiling_table_rank_transformed_40_kendall, 
                                                                 plot_title = "Noise Ceiling FMRI - (C.D., rank-transf & Kendall)",
                                                                 y_limits = c(0.1, 0.8),
                                                                 y_breaks = seq(0.1, 0.8, by = 0.2))

### Bar plots - Fisher transformed ###
plot_noise_ceiling_fmri_fisher_40_pearson <- f_plot_noise_ceiling_fmri(noise_ceiling_table_fisher_pearson_40, 
                                                                plot_title = "Noise Ceiling FMRI (C.D.- Fisher & Pearson)",
                                                                y_limits = c(0.1, 1),
                                                                y_breaks = seq(0.1, 1, by = 0.2))

plot_noise_ceiling_fmri_fisher_40_spearman <- f_plot_noise_ceiling_fmri(noise_ceiling_table_fisher_spearman_40, 
                                                                 plot_title = "Noise Ceiling FMRI (C.D.- Fisher & Spearman)",
                                                                 y_limits = c(0.1, 0.8),
                                                                 y_breaks = seq(0.1, 0.8, by = 0.2))

plot_noise_ceiling_fmri_fisher_40_kendall <- f_plot_noise_ceiling_fmri(noise_ceiling_table_fisher_kendall_40, 
                                                                plot_title = "Noise Ceiling FMRI (C.D.- Fisher & Kendall)",
                                                                y_limits = c(0.1, 0.8),
                                                                y_breaks = seq(0.1, 0.8, by = 0.2))

### Cosine Similarity distance ###
## Pearson, Spearman and Kendall ##
plot_noise_ceiling_fmri_cosine_40_pearson <- f_plot_noise_ceiling_fmri(noise_ceiling_table_cosine_pearson_40, 
                                                                plot_title = "Noise Ceiling FMRI (Cosine & Pearson)",
                                                                y_limits = c(0.1, 1),
                                                                y_breaks = seq(0.1, 1, by = 0.2))

plot_noise_ceiling_fmri_cosine_40_spearman <- f_plot_noise_ceiling_fmri(noise_ceiling_table_cosine_spearman_40, 
                                                                 plot_title = "Noise Ceiling FMRI (Cosine & Spearman)",
                                                                 y_limits = c(0.1, 1),
                                                                 y_breaks = seq(0.1, 1, by = 0.2))

plot_noise_ceiling_fmri_cosine_40_kendall <- f_plot_noise_ceiling_fmri(noise_ceiling_table_cosine_kendall_40, 
                                                                plot_title = "Noise Ceiling FMRI (Cosine & Kendall)",
                                                                y_limits = c(0.1, 1),
                                                                y_breaks = seq(0.1, 1, by = 0.2))
print(plot_noise_ceiling_fmri_cosine_40_kendall)

## transformed data ##
plot_noise_ceiling_fmri_cosine_40_z_pearson <- f_plot_noise_ceiling_fmri(noise_ceiling_table_cosine_z_transformed_40, 
                                                                  plot_title = "Noise Ceiling FMRI - (Cosine, Z-transf & Pearson)",
                                                                  y_limits = c(0.1, 0.8),
                                                                  y_breaks = seq(0.1, 0.8, by = 0.2))


plot_noise_ceiling_fmri_cosine_40_rank_spearman <- f_plot_noise_ceiling_fmri(noise_ceiling_table_cosine_rank_transformed_40_spearman, 
                                                                 plot_title = "Noise Ceiling FMRI - (Cosine, rank-transf & Spearman)",
                                                                 y_limits = c(0.1, 0.8),
                                                                 y_breaks = seq(0.1, 0.8, by = 0.2))

plot_noise_ceiling_fmri_cosine_40_rank_kendall <- f_plot_noise_ceiling_fmri(noise_ceiling_table_rank_transformed_40_kendall, 
                                                                 plot_title = "Noise Ceiling FMRI - (Cosine, rank-transf & Kendall)",
                                                                 y_limits = c(0.1, 0.8),
                                                                 y_breaks = seq(0.1, 0.8, by = 0.2))


##### Step 18: barplot of RDMs with differences in explained variance per region #####

#### Function to Generate and Store Plots for Multiple Matrices ####
f_plot_multiple_behavioral_matrices <- function(matrix_names, matrix_data, noise_ceiling_table, distance_type, method) {
  
  # Define the desired ROI order explicitly
  roi_names <- c("Occipital", "fusiform", "IT", "MT", "ParHipp", 
                 "dlPFC", "vmPFC", "vlPFC", "OFC", "mPFC", "Hippocampus")
  
  # Initialize an empty list to store the plots
  plot_list <- list()
  
  # Abbreviate the distance and method for the title
  distance_abbr <- toupper(substr(distance_type, 1, 3))
  method_abbr <- toupper(substr(method, 1, 3))
  
  # Determine matrix label
  matrix_label <- ifelse(any(grepl("comb", names(matrix_data))), "comb",
                         ifelse(any(grepl("eng", names(matrix_data))), "eng",
                                ifelse(any(grepl("spa", names(matrix_data))), "spa", "comb")))
  
  # Loop through each matrix name
  for (row_name in matrix_names) {
    
    # Subset noise ceiling table
    noise_ceiling_table_filtered <- noise_ceiling_table %>%
      filter(grepl(matrix_label, List))
    
    # Function to extract ROI correlation values
    extract_roi_values <- function(matrix) {
      roi_columns <- grep("l_roi_.*", colnames(matrix), value = TRUE)
      row_values <- as.numeric(matrix[row_name, roi_columns, drop = FALSE])
      data.frame(roi = roi_names, correlation = row_values)
    }
    
    roi_values <- extract_roi_values(matrix_data)
    plot_data <- roi_values %>%
      left_join(noise_ceiling_table_filtered, by = c("roi" = "ROI"))
    
    # Determine primary Y-axis limit and dynamic scale factor
    y_axis_limit <- ifelse(any(grepl("Fisher", plot_data$Transformation, ignore.case = TRUE)), 1, 0.6)
    scale_factor <- y_axis_limit / 0.25  # Dynamic scaling for secondary axis
    
    # Scale Difference values explicitly
    plot_data <- plot_data %>%
      mutate(scaled_difference = Difference * scale_factor)
    
    # Correct for bar order
    plot_data$roi <- factor(plot_data$roi, levels = roi_names)
    
    # Generate the plot
    final_plot <- ggplot(plot_data, aes(x = roi)) +
      geom_bar(aes(y = correlation), stat = "identity", fill = "skyblue", width = 0.6) +
      geom_line(aes(y = scaled_difference, group = 1), color = "red", size = 1) +
      geom_point(aes(y = scaled_difference), color = "red", size = 2) +
      labs(
        title = paste("Corr:", distance_abbr, method_abbr, matrix_label, "-", row_name, sep = " "),
        x = "ROI",
        y = "Correlation Values"
      ) +
      scale_y_continuous(
        limits = c(0, y_axis_limit),
        sec.axis = sec_axis(
          ~ . / scale_factor,  # Rescale the secondary axis correctly
          name = "Corrected Dissimilarity Variance",
          breaks = c(0.1, 0.2), labels = scales::label_number(accuracy = 0.1)
        )
      ) +
      theme_minimal() +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.y.right = element_text(color = "red"),
        axis.text.y.right = element_text(color = "red")
      )
    
    # Store the plot
    plot_list[[row_name]] <- final_plot
  }
  return(plot_list)
}

## Define matrix names to plot ##
matrix_names <- c(
  "Behavioral_mean_matrix_eng_all_data_original",
  "Behavioral_mean_matrix_spa_all_data_original",
  "Behavioral_mean_matrix_comb_all_data_original",
  "Behavioral_imputed_mean_matrix_comb_all_data",
  "Behavioral_mean_matrix_comb_cat_specific_General_Similarity",
  "Behavioral_mean_matrix_comb_cat_specific_dissimilar",
  "Behavioral_mean_matrix_comb_cat_specific_category",
  "Behavioral_mean_matrix_comb_cat_specific_shape",
  "Behavioral_mean_matrix_comb_cat_specific_color",
  "Behavioral_mean_matrix_comb_cat_specific_animacy",
  "Behavioral_mean_matrix_comb_cat_specific_preference",
  "Behavioral_mean_matrix_comb_cat_specific_scare"
)

### barplots of 2nd-level correlations and CDV plots ###

## Correlation distance (C.D) ##
P_corrected_correlation_CDV_pearson <- f_plot_multiple_behavioral_matrices(
  matrix_names = matrix_names,
  matrix_data = additional_2nd_level_matrices$result_2nd_level_40x40_comb_pearson,
  noise_ceiling_table = noise_ceiling_table_raw_pearson_40,
  distance_type = "Correlation Distance",
  method = "Pearson"
)
P_corrected_correlation_CDV_Spearman <- f_plot_multiple_behavioral_matrices(
  matrix_names = matrix_names,
  matrix_data = additional_2nd_level_matrices$result_2nd_level_40x40_comb_spearman,
  noise_ceiling_table = noise_ceiling_table_raw_spearman_40,
  distance_type = "Correlation Distance",
  method = "Spearman"
)
P_corrected_correlation_CDV_Kendall <- f_plot_multiple_behavioral_matrices(
  matrix_names = matrix_names,
  matrix_data = additional_2nd_level_matrices$result_2nd_level_40x40_comb_kendall,
  noise_ceiling_table = noise_ceiling_table_raw_kendall_40,
  distance_type = "Correlation Distance",
  method = "Kendall"
)

## C.D. - post-fisher Transformation - for visualization purposes!! ##
P_corrected_correlation_CDV_fisher_pearson <- f_plot_multiple_behavioral_matrices(
  matrix_names = matrix_names,
  matrix_data = additional_2nd_level_matrices$result_2nd_level_fisher_40x40_comb_pearson,
  noise_ceiling_table = noise_ceiling_table_fisher_pearson_40,
  distance_type = "Fisher",
  method = "Pearson"
)
P_corrected_correlation_CDV_fisher_spearman <- f_plot_multiple_behavioral_matrices(
  matrix_names = matrix_names,
  matrix_data = additional_2nd_level_matrices$result_2nd_level_fisher_40x40_comb_spearman,
  noise_ceiling_table = noise_ceiling_table_fisher_spearman_40,
  distance_type = "Fisher",
  method = "Spearman"
)
P_corrected_correlation_CDV_fisher_kendall <- f_plot_multiple_behavioral_matrices(
  matrix_names = matrix_names,
  matrix_data = additional_2nd_level_matrices$result_2nd_level_fisher_40x40_comb_kendall,
  noise_ceiling_table = noise_ceiling_table_fisher_kendall_40,
  distance_type = "Fisher",
  method = "Kendall"
)

## Cosine Similarity ##
P_corrected_correlation_CDV_cosine_pearson <- f_plot_multiple_behavioral_matrices(
  matrix_names = matrix_names,
  matrix_data = additional_2nd_level_matrices$result_2nd_level_cosine_40x40_comb_pearson,
  noise_ceiling_table = noise_ceiling_table_cosine_pearson_40,
  distance_type = "Cosine Similarity Distance",
  method = "Pearson"
)
P_corrected_correlation_CDV_cosine_Spearman <- f_plot_multiple_behavioral_matrices(
  matrix_names = matrix_names,
  matrix_data = additional_2nd_level_matrices$result_2nd_level_cosine_40x40_comb_spearman,
  noise_ceiling_table = noise_ceiling_table_cosine_spearman_40,
  distance_type = "Cosine Similarity Distance",
  method = "Spearman"
)
P_corrected_correlation_CDV_cosine_Kendall <- f_plot_multiple_behavioral_matrices(
  matrix_names = matrix_names,
  matrix_data = additional_2nd_level_matrices$result_2nd_level_cosine_40x40_comb_kendall,
  noise_ceiling_table = noise_ceiling_table_cosine_spearman_40,
  "Cosine Similarity Distance",
  method = "Kendall"
)

### fMRI - English & Spanish - Pearson only ###
## Correlation distance ##
P_corrected_correlation_eng_CDV_pearson <- f_plot_multiple_behavioral_matrices(
  matrix_names = matrix_names,
  matrix_data = additional_2nd_level_matrices$result_2nd_level_40x40_eng_pearson,
  noise_ceiling_table = noise_ceiling_table_raw_pearson_40,
  distance_type = "Correlation Distance",
  method = "Pearson"
)
P_corrected_correlation_spa_CDV_pearson <- f_plot_multiple_behavioral_matrices(
  matrix_names = matrix_names,
  matrix_data = additional_2nd_level_matrices$result_2nd_level_40x40_spa_pearson,
  noise_ceiling_table = noise_ceiling_table_raw_pearson_40,
  distance_type = "Correlation Distance",
  method = "Pearson"
)

## Cosine Similarity distance ##
P_corrected_correlation_CDV_eng_cosine_pearson <- f_plot_multiple_behavioral_matrices(
  matrix_names = matrix_names,
  matrix_data = additional_2nd_level_matrices$result_2nd_level_cosine_40x40_eng_pearson,
  noise_ceiling_table = noise_ceiling_table_cosine_pearson_40,
  distance_type = "Cosine Similarity Distance",
  method = "Pearson"
)
P_corrected_correlation_CDV_spa_cosine_pearson <- f_plot_multiple_behavioral_matrices(
  matrix_names = matrix_names,
  matrix_data = additional_2nd_level_matrices$result_2nd_level_cosine_40x40_spa_pearson,
  noise_ceiling_table = noise_ceiling_table_cosine_pearson_40,
  distance_type = "Cosine Similarity Distance",
  method = "Pearson"
)

##### Step 19: Correction for attenuation in Behavior and fMRI #####
## Formula: 

# Behavioral reliability: relia_comb$spearman_brown. Also for eng and spa # 
# fMRI reliability: Noise ceiling tables #

#### Step 19.1: Utility functions for the main function ####

## Utility function to extract language ##
get_lang <- function(name) {
  if (grepl("_eng_", name)) return("eng")
  if (grepl("_spa_", name)) return("spa")
  return("comb")
}
## Utility functions to extract metric ##
get_metric <- function(name) {
  if (grepl("crossnobis", name)) return("crossnobis")
  if (grepl("cosine", name)) return("cosine")
  if (grepl("fisher", name)) return("fisher")
  if (grepl("z_", name)) return("z")
  if (grepl("rank", name)) return("rank")
  return("raw")
}
## Utility functions to extract correlation method ##
get_corr_method <- function(name) {
  if (grepl("kendall", name)) return("Kendall")
  if (grepl("spearman", name)) return("Spearman")
  return("Pearson")
}
## Utility functions to extract reliability estimate for each language data ##
get_relia <- function(lang, relia_comb, relia_eng, relia_spa) {
  switch(lang,
         "comb" = relia_comb$spearman_brown,
         "eng" = relia_eng$spearman_brown,
         "spa" = relia_spa$spearman_brown)
}

## Utility functions to extract ROI ##
get_roi_name <- function(label) {
  matched_roi <- roi_names[sapply(roi_names, function(roi) grepl(paste0("(?i)", roi, "$|_", roi, "$|_", roi, "_|", roi, "$"), label, perl = TRUE))]
  
  if (length(matched_roi) > 0) {
    return(matched_roi[1])  # Return first match
  } else {
    return(NA)
  }
}  

## Utility functions to extract the correct list of results to match ##
get_matching_list_identifier <- function(matrix_name, available_lists) {
  matrix_tokens <- unlist(strsplit(matrix_name, split = "_"))
  
  # Score all available lists based on token overlap
  match_scores <- sapply(available_lists, function(alist) {
    list_tokens <- unlist(strsplit(alist, split = "_"))
    length(intersect(matrix_tokens, list_tokens))
  })
  
  best_score <- max(match_scores)
  best_matches <- available_lists[match_scores == best_score]
  
  cat("✅ Best list match:", best_matches[1], "| Token match score:", best_score, "\n")
  
  if (length(best_matches) > 1) {
    warning(paste(
      "⚠️ Multiple best-matching lists for", matrix_name,
      "→", paste(best_matches, collapse = ", "),
      "(score:", best_score, ")"
    ))
  }
  
  return(list(best = best_matches[1], score = best_score, all_best = best_matches))
}

#### Step 19.2: Corresponding noise ceiling tables for all 2nd-level additional matrices ####
noise_ceiling_table_list <- list(
  ## correlation distance ##
  # No transformation #
  # pearson #
  result_2nd_level_40x40_comb_pearson = noise_ceiling_table_raw_pearson_40,
  result_2nd_level_40x40_eng_pearson = noise_ceiling_table_raw_pearson_40,
  result_2nd_level_40x40_spa_pearson = noise_ceiling_table_raw_pearson_40,
  # Spearman #
  result_2nd_level_40x40_comb_spearman = noise_ceiling_table_raw_spearman_40, 
  result_2nd_level_40x40_eng_spearman = noise_ceiling_table_raw_spearman_40,
  result_2nd_level_40x40_spa_spearman = noise_ceiling_table_raw_spearman_40,
  # Kendall #
  result_2nd_level_40x40_comb_kendall = noise_ceiling_table_raw_kendall_40,
  result_2nd_level_40x40_eng_kendall = noise_ceiling_table_raw_kendall_40,
  result_2nd_level_40x40_spa_kendall = noise_ceiling_table_raw_kendall_40,
  # Z-transformed #
  result_2nd_level_z_40x40_comb_pearson = noise_ceiling_table_z_transformed_40,
  result_2nd_level_z_40x40_eng_pearson = noise_ceiling_table_z_transformed_40,
  result_2nd_level_z_40x40_spa_pearson = noise_ceiling_table_z_transformed_40,
  # Rank-transformed #
  result_2nd_level_rank_40x40_comb_spearman = noise_ceiling_table_rank_transformed_40_spearman,
  result_2nd_level_rank_40x40_eng_spearman = noise_ceiling_table_rank_transformed_40_spearman,
  result_2nd_level_rank_40x40_spa_spearman = noise_ceiling_table_rank_transformed_40_spearman,
  result_2nd_level_rank_40x40_comb_kendall = noise_ceiling_table_rank_transformed_40_kendall,
  result_2nd_level_rank_40x40_eng_kendall = noise_ceiling_table_rank_transformed_40_kendall,
  result_2nd_level_rank_40x40_spa_kendall = noise_ceiling_table_rank_transformed_40_kendall,
  ## Correlation distance - Fisher transformed ## 
  result_2nd_level_fisher_40x40_comb_pearson = noise_ceiling_table_fisher_pearson_40,
  result_2nd_level_fisher_40x40_eng_pearson = noise_ceiling_table_fisher_pearson_40,
  result_2nd_level_fisher_40x40_spa_pearson = noise_ceiling_table_fisher_pearson_40,
  result_2nd_level_fisher_40x40_comb_spearman = noise_ceiling_table_fisher_spearman_40,
  result_2nd_level_fisher_40x40_eng_spearman = noise_ceiling_table_fisher_spearman_40,
  result_2nd_level_fisher_40x40_spa_spearman = noise_ceiling_table_fisher_spearman_40,
  result_2nd_level_fisher_40x40_comb_kendall = noise_ceiling_table_fisher_kendall_40,
  result_2nd_level_fisher_40x40_eng_kendall = noise_ceiling_table_fisher_kendall_40,
  result_2nd_level_fisher_40x40_spa_kendall = noise_ceiling_table_fisher_kendall_40,
  ## Cosine Similarity ##
  # No transformation #
  result_2nd_level_cosine_40x40_comb_pearson = noise_ceiling_table_cosine_pearson_40,
  result_2nd_level_cosine_40x40_eng_pearson = noise_ceiling_table_cosine_pearson_40,
  result_2nd_level_cosine_40x40_spa_pearson = noise_ceiling_table_cosine_pearson_40,
  result_2nd_level_cosine_40x40_comb_spearman = noise_ceiling_table_cosine_spearman_40,
  result_2nd_level_cosine_40x40_eng_spearman = noise_ceiling_table_cosine_spearman_40,
  result_2nd_level_cosine_40x40_spa_spearman = noise_ceiling_table_cosine_spearman_40,
  result_2nd_level_cosine_40x40_comb_kendall = noise_ceiling_table_cosine_kendall_40,
  result_2nd_level_cosine_40x40_eng_kendall = noise_ceiling_table_cosine_kendall_40,
  result_2nd_level_cosine_40x40_spa_kendall = noise_ceiling_table_cosine_kendall_40,
  # Transformed #
  result_2nd_level_z_cosine_40x40_comb_pearson = noise_ceiling_table_cosine_z_transformed_40,
  result_2nd_level_z_cosine_40x40_eng_pearson = noise_ceiling_table_cosine_z_transformed_40,
  result_2nd_level_z_cosine_40x40_spa_pearson = noise_ceiling_table_cosine_z_transformed_40,
  result_2nd_level_rank_cosine_40x40_comb_spearman = noise_ceiling_table_cosine_rank_transformed_40_spearman, 
  result_2nd_level_rank_cosine_40x40_eng_spearman = noise_ceiling_table_cosine_rank_transformed_40_spearman,
  result_2nd_level_rank_cosine_40x40_spa_spearman = noise_ceiling_table_cosine_rank_transformed_40_spearman,
  result_2nd_level_rank_cosine_40x40_comb_kendall = noise_ceiling_table_cosine_rank_transformed_40_kendall,
  result_2nd_level_rank_cosine_40x40_eng_kendall = noise_ceiling_table_cosine_rank_transformed_40_kendall,
  result_2nd_level_rank_cosine_40x40_spa_kendall = noise_ceiling_table_cosine_rank_transformed_40_kendall,
  ## Crossnobis ##
  result_2nd_level_crossnobis_40x40_comb_pearson = noise_ceiling_table_crossnobis_pearson_40,
  result_2nd_level_crossnobis_40x40_eng_pearson = noise_ceiling_table_crossnobis_pearson_40,
  result_2nd_level_crossnobis_40x40_spa_pearson = noise_ceiling_table_crossnobis_pearson_40,
  result_2nd_level_crossnobis_40x40_comb_spearman = noise_ceiling_table_crossnobis_spearman_40,
  result_2nd_level_crossnobis_40x40_eng_spearman = noise_ceiling_table_crossnobis_spearman_40,
  result_2nd_level_crossnobis_40x40_spa_spearman = noise_ceiling_table_crossnobis_spearman_40,
  result_2nd_level_crossnobis_40x40_comb_kendall = noise_ceiling_table_crossnobis_kendall_40,
  result_2nd_level_crossnobis_40x40_eng_kendall = noise_ceiling_table_crossnobis_kendall_40,
  result_2nd_level_crossnobis_40x40_spa_kendall = noise_ceiling_table_crossnobis_kendall_40
)

#### Function to apply the attenuation correction estimate to the 2nd-level raw correlations ####
f_correct_matrix_from_table_group <- function(matrix_name,
                                               raw_corrs,
                                               noise_ceiling_table,
                                               relia_comb,
                                               relia_eng,
                                               relia_spa) {
  cat("\n========== Processing:", matrix_name, "==========\n")
  
  # Detect language, metric, and method
  lang <- get_lang(matrix_name)
  metric <- get_metric(matrix_name)
  method <- get_corr_method(matrix_name)
  
  # Retrieve behavioral reliability
  r_behavioral <- tryCatch({
    if (lang == "eng") relia_eng$spearman_brown
    else if (lang == "spa") relia_spa$spearman_brown
    else if (lang == "comb") relia_comb$spearman_brown
    else NA
  }, error = function(e) NA)
  
  # Determine expected fMRI list name dynamically from the helper function
  available_lists <- unique(noise_ceiling_table$List)
  match_info <- get_matching_list_identifier(matrix_name, available_lists)
  list_identifier <- match_info$best
  
  if (is.na(list_identifier)) {
    stop(paste("❌ Could not find a suitable List match for matrix:", matrix_name))
  }
  
  # Initialize matrices and info storage
  corrected_corrs <- matrix(NA, nrow = nrow(raw_corrs), ncol = ncol(raw_corrs),
                            dimnames = dimnames(raw_corrs))
  r_corrected_matrix <- matrix(NA, nrow = nrow(raw_corrs), ncol = ncol(raw_corrs),
                               dimnames = dimnames(raw_corrs))
  r_neural_used <- list()
  roi_names_used <- c()
  
  # Summary print
  cat("Language detected:", lang, "\n")
  cat("Metric detected:", metric, "\n")
  cat("Correlation method detected:", method, "\n")
  cat("Behavioral reliability (r_behavioral):", r_behavioral, "\n")
  cat("Expected fMRI list name in table:", list_identifier, "\n\n")
  
  # 🧼 Clean noise_ceiling_table columns once here
  noise_ceiling_table <- noise_ceiling_table %>%
    dplyr::mutate(
      List = trimws(as.character(List)),
      ROI = trimws(as.character(ROI))
    )
  
  # Then match
  match_row <- noise_ceiling_table[
    noise_ceiling_table$List == list_identifier &
      noise_ceiling_table$ROI == roi_name,
  ]
  
  for (row_label in rownames(raw_corrs)) {
    for (col_label in colnames(raw_corrs)) {
      r_obs <- raw_corrs[row_label, col_label]
      
      row_is_roi <- !is.na(get_roi_name(row_label))
      col_is_roi <- !is.na(get_roi_name(col_label))
      
      if (row_is_roi && !col_is_roi) {
        roi_name <- get_roi_name(row_label)
      } else if (!row_is_roi && col_is_roi) {
        roi_name <- get_roi_name(col_label)
      } else {
        next
      }
      
      # --- Try matching ROI and list in noise ceiling table ---
      match_row <- noise_ceiling_table[
        noise_ceiling_table$List == list_identifier &
          noise_ceiling_table$ROI == roi_name,
      ]
      
      if (nrow(match_row) == 0) {
        cat("❌ No match found\n")
        cat("   - Expected ROI        :", roi_name, "\n")
        cat("   - Expected List       :", list_identifier, "\n")
        
        # Optional: show what ROI names are available for that List
        candidate_rows <- noise_ceiling_table[noise_ceiling_table$List == list_identifier, ]
        if (nrow(candidate_rows) > 0) {
          cat("   - Available ROIs for that List:\n")
          print(unique(candidate_rows$ROI))
        } else {
          cat("   - No entries in noise ceiling table for List:", list_identifier, "\n")
          cat("   - Available Lists in noise ceiling table:\n")
          print(unique(noise_ceiling_table$List))
        }
        
        next  # skip to next loop
      }
      
      if (nrow(match_row) == 0) {
        cat("❌ No match found for ROI:", roi_name, "with List:", list_identifier, "\n")
        next
      }
      
      r_neural <- match_row$Lower_Bound[1]
      r_neural_used[[roi_name]] <- r_neural
      roi_names_used <- unique(c(roi_names_used, roi_name))
      
      if (!is.na(r_neural) && !is.na(r_behavioral)) {
        r_corrected <- r_obs / sqrt(r_behavioral * r_neural)
        corrected_corrs[row_label, col_label] <- r_corrected
        r_corrected_matrix[row_label, col_label] <- r_corrected
        
        ## Uncomment this to manually evaluate the correct use of reliability (behavioral and neural) elements ##
        #cat(sprintf("✅ ROI: %-12s | r_obs: %0.3f | r_neural: %0.3f | r_behavioral: %0.3f | r_corrected: %0.3f\n",
        #            roi_name, r_obs, r_neural, r_behavioral, r_corrected)) 
      } else {
        corrected_corrs[row_label, col_label] <- NA
        r_corrected_matrix[row_label, col_label] <- NA
      }
    }
  }
  # ✅ Summary of correction
  num_corrected <- sum(!is.na(corrected_corrs))
  cat("\n✅ Total corrected values for", matrix_name, ":", num_corrected, "\n")
  
  if (num_corrected > 0) {
    cat("✅ Matrix correction complete for", matrix_name, "\n")
  } else {
    cat("⚠️ No values were corrected for", matrix_name, "\n")
  }

  return(list(
    corrected_corrs = corrected_corrs,
    raw_corrs = raw_corrs,
    r_corrected = r_corrected_matrix,
    r_behavioral = r_behavioral,
    r_neural_used = r_neural_used,
    roi_names_used = roi_names_used,
    language = lang,
    metric = metric,
    correlation_method = method,
    fMRI_list_id = list_identifier
  ))
}

#### Correction wrapper function ####
f_correct_all_from_table <- function(matrix_list, table_list, relia_comb, relia_eng, relia_spa) {
  output_list <- list()
  
  for (name in names(matrix_list)) {
    cat("\n\n========== Now correcting:", name, "==========\n")
    
    # Skip if not in noise ceiling table list
    if (!(name %in% names(table_list))) {
      cat("⚠️  No noise ceiling table available for matrix:", name, "\n➡️  Skipping to next.\n")
      next
    }
    
    raw_matrix <- matrix_list[[name]]
    noise_table <- table_list[[name]]
    
    # Extra check for NULLs
    if (is.null(raw_matrix)) {
      cat("❌ Matrix not found or is NULL:", name, "\n")
      next
    }
    if (is.null(noise_table)) {
      cat("❌ Noise ceiling table is NULL for matrix:", name, "\n")
      next
    }
    
    result <- f_correct_matrix_from_table_group(
      matrix_name = name,
      raw_corrs = raw_matrix,
      noise_ceiling_table = noise_table,
      relia_comb = relia_comb,
      relia_eng = relia_eng,
      relia_spa = relia_spa
    )
    
    output_list[[name]] <- result
  }
  
  return(output_list)
}

#### Step 19.3: Estimate the attenuation correction for all the 2nd-level matrices ####
all_corrected_results <- f_correct_all_from_table(
  matrix_list = additional_2nd_level_matrices,
  table_list = noise_ceiling_table_list,
  relia_comb = relia_comb,
  relia_eng = relia_eng,
  relia_spa = relia_spa
)

# View verification (uncomment if needed)
# View(all_corrected_results$result_2nd_level_rank_40x40_eng_spearman$corrected_corrs) # corrected
# View(all_corrected_results$result_2nd_level_rank_40x40_eng_spearman$raw_corrs) # uncorrected
# View(all_corrected_results$result_2nd_level_rank_40x40_eng_spearman$r_neural_used) # r neural
# print(relia_eng$spearman_brown) # r behavioral
# View(noise_ceiling_table_rank_transformed_40_spearman) # r neural


##### Step 20: Plot the corrected correlation coefficients #####  
library(ggplot2)
library(reshape2)

#### Function to generalize the plots on all the matrices ####
f_plot_corrected_behavior_brain_heatmap <- function(corrected_corrs,
                                                    title = "Corrected Behavior–Brain Correlations") {
  library(ggplot2)
  library(reshape2)
  
  row_labels <- rownames(corrected_corrs)
  col_labels <- colnames(corrected_corrs)
  
  # Detect behavioral and ROI columns (flexible)
  behavior_rows <- grepl("Behavioral", row_labels, ignore.case = TRUE)
  brain_cols <- grepl("^l_roi_", col_labels, ignore.case = TRUE)
  
  # If nothing found, throw warning
  if (!any(behavior_rows) || !any(brain_cols)) {
    warning("⚠️ No behavioral rows or ROI columns found in matrix.")
    return(NULL)
  }
  
  # Subset and melt
  filtered_matrix <- corrected_corrs[behavior_rows, brain_cols, drop = FALSE]
  behavior_order <- row_labels[behavior_rows]
  long_df <- melt(filtered_matrix, varnames = c("Behavior", "ROI"), value.name = "Corrected_Correlation")
  
  # Extract ROI name by last underscore token
  long_df$ROI <- gsub(".*_", "", long_df$ROI)
  long_df$Behavior <- factor(long_df$Behavior, levels = behavior_order)
  
  # ROI display order
  roi_display_order <- c("Occipital", "fusiform", "IT", "MT", "ParHipp",
                         "dlPFC", "vmPFC", "vlPFC", "OFC", "mPFC", "Hippocampus")
  long_df$ROI <- factor(long_df$ROI, levels = roi_display_order)
  
  # Plot
  heatmap_plot <- ggplot(long_df, aes(x = ROI, y = Behavior, fill = Corrected_Correlation)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%.2f", Corrected_Correlation)), size = 3) +
    scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0,
                         limits = c(0, 1), na.value = "gray90") +
    labs(title = title, x = "fMRI ROI", y = "Behavioral Model", fill = "Corrected\nr") +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text.y = element_text(size = 9))
  
  return(heatmap_plot)
}

#### Function adaption with label for raw and corrected #####
f_plot_behavior_brain_heatmap <- function(input_matrix,
                                          title = "Behavior–Brain Correlations",
                                          fill_label = "Corrected\nr") {
  library(ggplot2)
  library(reshape2)
  
  row_labels <- rownames(input_matrix)
  col_labels <- colnames(input_matrix)
  
  behavior_rows <- grepl("Behavioral", row_labels, ignore.case = TRUE)
  brain_cols <- grepl("^l_roi_", col_labels, ignore.case = TRUE)
  
  if (!any(behavior_rows) || !any(brain_cols)) {
    warning("⚠️ No behavioral rows or ROI columns found in matrix.")
    return(NULL)
  }
  
  filtered_matrix <- input_matrix[behavior_rows, brain_cols, drop = FALSE]
  behavior_order <- row_labels[behavior_rows]
  long_df <- melt(filtered_matrix, varnames = c("Behavior", "ROI"), value.name = "Correlation")
  
  long_df$ROI <- gsub(".*_", "", long_df$ROI)
  long_df$Behavior <- factor(long_df$Behavior, levels = behavior_order)
  
  roi_display_order <- c("Occipital", "fusiform", "IT", "MT", "ParHipp",
                         "dlPFC", "vmPFC", "vlPFC", "OFC", "mPFC", "Hippocampus")
  long_df$ROI <- factor(long_df$ROI, levels = roi_display_order)
  
  heatmap_plot <- ggplot(long_df, aes(x = ROI, y = Behavior, fill = Correlation)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%.2f", Correlation)), size = 3) +
    scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0,
                         limits = c(0, 1), na.value = "gray90") +
    labs(title = title, x = "fMRI ROI", y = "Behavioral Model", fill = fill_label) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text.y = element_text(size = 9))
  
  return(heatmap_plot)
}

### Corrected correlations ###
#### Step 20.1: Create a copy ####
copy_corrected_results <- all_corrected_results

#### Step 20.2: Store corrected plots ####
corrected_plots_list <- lapply(names(copy_corrected_results), function(name) {
  result <- copy_corrected_results[[name]]
  plot_title <- paste("Corrected Brain–Behavior Correlations:", name)
  
  if (!is.null(result$corrected_corrs)) {
    cat("📊 Plotting corrected:", name, "\n")
    return(
      f_plot_behavior_brain_heatmap(result$corrected_corrs,
                                    title = plot_title,
                                    fill_label = "Corrected\nr")
    )
  } else {
    cat("⚠️ Skipping (no corrected matrix):", name, "\n")
    return(NULL)
  }
})

#### Step 20.3: Name the list elements and display the plots ####
names(corrected_plots_list) <- names(copy_corrected_results)
#print(corrected_plots_list)

### raw correlations ###
raw_plots_list <- lapply(names(copy_corrected_results), function(name) {
  result <- copy_corrected_results[[name]]
  plot_title <- paste("Raw Brain–Behavior Correlations:", name)
  
  if (!is.null(result$raw_corrs)) {
    cat("🧪 Plotting raw:", name, "\n")
    return(
      f_plot_behavior_brain_heatmap(result$raw_corrs,
                                    title = plot_title,
                                    fill_label = "Raw\nr")
    )
  } else {
    cat("⚠️ Skipping (no raw matrix):", name, "\n")
    return(NULL)
  }
})
names(raw_plots_list) <- names(copy_corrected_results)

## --------------------------------------------------------------------------------------
###---------------- SECTION 5: CNNs and algorithm addition.  -------------------------###

## required libraries ##
library(reshape2)  
library(dplyr)
library(viridis)
library(lattice)
library(ggplot2)

## Matrices were converted to dissimilarity matrices in Python !!!! ##

##### Step 21: Import cosine dissimilarity matrices - category matrices for the integration #####

## Directory of CSV files ##
output_dir <- "C:/Users/j.gambasegovia/Documents/Papeles Alejo UF/1 - RSA - R.Project/RSA_CNN/final_objective_matrices"

## Get a list of all CSV files ##
csv_files <- list.files(output_dir, pattern = "_category_matrix\\.csv$", full.names = TRUE)

## Load each CSV file into a list ##
CNN_category_matrices <- lapply(csv_files, function(file) {
  read.csv(file, header = FALSE)
})

## Name each list element by its corresponding file name ##
names(CNN_category_matrices) <- gsub("_category_matrix\\.csv$", "", basename(csv_files))
#print(names(CNN_category_matrices))

##### Step 22: Define the desired order, organize the 14 matrices and flip-vertically them ##### 
##### to correspond the rest of the matrices #####

## define the expected order ##
desired_order <- c(
  "similarity_matrix_english", 
  "similarity_matrix_spanish",
  "english_color_similarity_matrix", 
  "spanish_color_similarity_matrix",
  "english_color_proportions_similarity_matrix",
  "spanish_color_proportions_similarity_matrix", 
  "english_edge_canny_similarity_matrix",
  "spanish_edge_canny_similarity_matrix",
  "english_hed_similarity_matrix",
  "spanish_hed_similarity_matrix",
  "english_category_finetuned_ResNet50_similarity_matrix", 
  "spanish_category_finetuned_ResNet50_similarity_matrix",
  "english_animacy_finetuned_ResNet50_similarity_matrix", 
  "spanish_animacy_finetuned_ResNet50_similarity_matrix"
)

## Organize the list ##
l_ordered_matrices_CNN_diss_ordered <- CNN_category_matrices[desired_order]
#typeof(l_ordered_matrices_CNN_diss$similarity_matrix_english)

## Flip vertically the matrices ##
l_ordered_matrices_CNN_diss <- lapply(l_ordered_matrices_CNN_diss_ordered, function(mat) {
  mat <- as.matrix(mat)
  mat[nrow(mat):1, ]
})

#### Function to generate 40x40 heatmaps ####
f_plot_category_matrices_CNN <- function(category_matrices, category_names) {
  
  # Define the reversed viridis palette
  #reversed_viridis <- rev(viridis(100)) ## Used for visualization purposes 
  
  # Define the viridis palette
  reversed_viridis <- viridis
  
  # Initialize the list to store plots
  l_cat_CNN_plots <- list()
  
  # Loop through each matrix in the category_matrices list
  for (matrix_name in names(category_matrices)) {
    # Extract the current matrix and convert it to a numeric matrix if it is a data.frame
    matrix <- as.matrix(category_matrices[[matrix_name]])
    
    # Define column labels to display only at positions 10, 20, 30, and 40
    col_labels <- 1:40
    col_labels[-c(10, 20, 30, 40)] <- ""  # Only keep labels at 10, 20, 30, 40
    row_labels <- category_names  # Use category names for rows
    
    # Create a heatmap without spindles, with specified labels
    plot_result <- levelplot(matrix,
                             col.regions = reversed_viridis,  # Use the reversed viridis palette
                             scales = list(
                               x = list(at = 1:40, labels = col_labels, draw = TRUE), 
                               y = list(at = 1:40, labels = row_labels, draw = TRUE, cex = 0.8),
                               tck = c(0, 0)  # Remove spindles
                             ),
                             xlab = NULL, ylab = NULL, main = matrix_name)  # Add title with matrix name
    
    # Store the plot with the matrix name as the key
    l_cat_CNN_plots[[matrix_name]] <- plot_result
  }
  
  return(l_cat_CNN_plots)
}

##### Step 23: plot the synthetic and algorithmic matrices in the 5-color category ######
l_cat_CNN_plots <- f_plot_category_matrices_CNN(l_ordered_matrices_CNN_diss, category_names)
## Display the ordered plots ##
#print(l_cat_CNN_plots$similarity_matrix_english)

#### Main function: 2nd-level correlation between multimodal datasets: Behavior, fMRI and CNN ####
f_compute_and_plot_2nd_level_correlation_multimodal <- function(behavioral_matrices, fmri_matrices_list, 
                                                                cnn_matrices_list,
                                                                correlation_method = "pearson", matrix_dim = 40) {
  
  # Start time
  start_time <- Sys.time()
  
  # Initialize the list to store all conditions
  all_conditions_2nd_level_matrices <- list()
  
  # Loop through each fMRI condition
  for (fmri_condition in names(fmri_matrices_list)) {
    print(paste("Processing condition:", fmri_condition))
    
    # Initialize list to store each participant's 2nd-level correlation matrix for the current condition
    condition_2nd_level_matrices <- list()
    
    # Extract the ROI list for the current condition
    fmri_rois <- fmri_matrices_list[[fmri_condition]]
    
    # Loop over all participants (assuming 80 participants)
    for (participant_num in 1:80) {
      participant_id <- paste0("part_", participant_num)
      
      # Step 1: Initialize list to store all matrices (behavioral + fMRI ROIs + CNN for the current participant and condition)
      combined_matrices <- list()
      
      # Step 2: Add the behavioral matrices to the combined list
      for (behav_name in names(behavioral_matrices)) {
        combined_matrices[[behav_name]] <- as.matrix(behavioral_matrices[[behav_name]])
      }
      
      # Step 3: Add fMRI matrices for the current participant and condition
      for (roi_name in names(fmri_rois)) {
        participant_matrix <- fmri_rois[[roi_name]][grep(participant_id, names(fmri_rois[[roi_name]]))][[1]]
        fmri_name <- paste(fmri_condition, roi_name, sep = "_")
        combined_matrices[[fmri_name]] <- as.matrix(participant_matrix)
      }
      
      # Step 4: Add CNN matrices to the combined list (ensuring they are numeric matrices)
      for (cnn_name in names(cnn_matrices_list)) {
        combined_matrices[[cnn_name]] <- as.matrix(cnn_matrices_list[[cnn_name]])
      }
      
      # Step 5: Initialize an empty matrix to store correlations for the current participant
      n <- length(combined_matrices)
      second_level_corr_matrix <- matrix(NA, n, n)
      rownames(second_level_corr_matrix) <- names(combined_matrices)
      colnames(second_level_corr_matrix) <- names(combined_matrices)
      
      # Step 6: Calculate pairwise correlations between all matrices in combined_matrices
      for (i in 1:n) {
        for (j in i:n) {
          corr_value <- cor(as.vector(combined_matrices[[i]]), as.vector(combined_matrices[[j]]), 
                            use = "pairwise.complete.obs", method = correlation_method)
          second_level_corr_matrix[i, j] <- corr_value
          second_level_corr_matrix[j, i] <- corr_value
        }
      }
      
      # Step 7: Store the 2nd-level correlation matrix in the main list with a participant-specific name
      participant_matrix_name <- paste0("result_2nd_level_corr_matrix_behav_fmri_cnn_", participant_id)
      condition_2nd_level_matrices[[participant_matrix_name]] <- second_level_corr_matrix
      
      # Print progress every 10 participants
      if (participant_num %% 10 == 0) {
        print(paste("Processed", participant_num, "participants for condition", fmri_condition))
      }
    }
    
    # Step 8: Average the 2nd-level correlation matrices across participants for the current condition
    avg_corr_matrix <- Reduce("+", condition_2nd_level_matrices) / length(condition_2nd_level_matrices)
    
    # Step 9: Capture the labels used in the average correlation matrix
    matrix_labels <- rownames(avg_corr_matrix)
    
    # Step 10: Convert the averaged correlation matrix to long format for plotting
    long_format_avg_corr_matrix <- f_convert_corr_matrix_to_long_format(avg_corr_matrix)
    
    # Step 11: Plot the heatmap of the average 2nd-level correlation matrix
    heatmap_plot <- f_create_heatmap(long_format_avg_corr_matrix, 
                                     title = paste("Average 2nd-Level Correlation Matrix for", fmri_condition, "-", correlation_method))
    
    # Display the heatmap
    print(heatmap_plot)
    
    # Store the results for the current condition in the main list
    all_conditions_2nd_level_matrices[[fmri_condition]] <- list(
      all_2nd_level_corr_matrices = condition_2nd_level_matrices,
      average_corr_matrix = avg_corr_matrix,
      heatmap_plot = heatmap_plot,
      matrix_labels = matrix_labels
    )
  }
  
  # End time and calculate duration
  end_time <- Sys.time()
  duration <- difftime(end_time, start_time, units = "secs")
  print(paste("Total execution time:", round(duration, 2), "seconds"))
  
  return(all_conditions_2nd_level_matrices)
}

##### Step 24: Generate the correlation plots for the combined, english and spanish fMRI lists #####

### matrices to use ###
#l_cosine_final_matrices_40x40
#View(l_cosine_dissim_similarity_matrices_40_fmri$l_roi_cosine_matrices_40_comb_fmri$Occipital$part_1_ROI_1_40x40_comb)
#l_ordered_matrices_CNN

#### Step 24.1: Create a copy of the original list ####:
l_ordered_matrices_CNN_diss_labelled <- l_ordered_matrices_CNN_diss

#### Step 24.2: Rename the elements by adding the "CNN_" prefix to each name ####:
names(l_ordered_matrices_CNN_diss_labelled) <- paste0("CNN_", names(l_ordered_matrices_CNN_diss_labelled))
names(l_ordered_matrices_CNN_diss_labelled)

## Remember: " if Error in as.table.default(corr_matrix) : cannot coerce to a table
## Called from: as.table.default(corr_matrix) - I need to re-run ALL the functions used to clean them from leaky components ##

#### Step 24.3: Plot the 2nd-level multimodal matrices #### 

### 40x40 ###
## Cosine distance - Pearson ## Duration time: 35 seconds ##
result_2nd_level_cosine_40x40_pearson_multimodal <- f_compute_and_plot_2nd_level_correlation_multimodal(
  behavioral_matrices = l_cosine_dissim_behavioral_40x40,
  fmri_matrices_list = l_cosine_dissim_similarity_matrices_40_fmri,
  cnn_matrices_list = l_ordered_matrices_CNN_diss_labelled,
  correlation_method = "pearson",
  matrix_dim = 40
)

## Cosine distance - Spearman ## Duration time: 250 seconds ##
result_2nd_level_cosine_40x40_spearman_multimodal <- f_compute_and_plot_2nd_level_correlation_multimodal(
  behavioral_matrices = l_cosine_dissim_behavioral_40x40,
  fmri_matrices_list = l_cosine_dissim_similarity_matrices_40_fmri,
  cnn_matrices_list = l_ordered_matrices_CNN_diss_labelled,
  correlation_method = "spearman",
  matrix_dim = 40
)

# ## Cosine distance - Kendall - Duration time: too much time!! ##
# result_2nd_level_cosine_40x40_pearson_multimodal <- f_compute_and_plot_2nd_level_correlation_multimodal(
#   behavioral_matrices = l_cosine_final_matrices_40x40,
#   fmri_matrices_list = l_cosine_similarity_matrices_40_fmri,
#   cnn_matrices_list = l_ordered_matrices_CNN,
#   correlation_method = "kendall",
#   matrix_dim = 40
# )

##### Step 25: Significance matrix for the multimodal 2nd-level matrix #####

#### Step 25.1: Add the additional matrices ####
CNN_2nd_level_matrices <- list(
  result_2nd_level_cosine_40x40_comb_pearson_multimodal = result_2nd_level_cosine_40x40_pearson_multimodal$l_roi_cosine_matrices_40_comb_fmri$average_corr_matrix,  ## cosine dissimilarity ##
  result_2nd_level_cosine_40x40_eng_pearson_multimodal = result_2nd_level_cosine_40x40_pearson_multimodal$l_roi_cosine_matrices_40_eng_fmri$average_corr_matrix,
  result_2nd_level_cosine_40x40_spa_pearson_multimodal = result_2nd_level_cosine_40x40_pearson_multimodal$l_roi_cosine_matrices_40_spa_fmri$average_corr_matrix,
  result_2nd_level_cosine_40x40_comb_spearman_multimodal = result_2nd_level_cosine_40x40_spearman_multimodal$l_roi_cosine_matrices_40_comb_fmri$average_corr_matrix,
  result_2nd_level_cosine_40x40_eng_spearman_multimodal = result_2nd_level_cosine_40x40_spearman_multimodal$l_roi_cosine_matrices_40_eng_fmri$average_corr_matrix,
  result_2nd_level_cosine_40x40_spa_spearman_multimodal = result_2nd_level_cosine_40x40_spearman_multimodal$l_roi_cosine_matrices_40_spa_fmri$average_corr_matrix
)


#### Step 25.2 Significance testing on parametric correlation methods ####
FDR_significance_results_2nd_level_matrices_CNN <- f_process_correlation_matrices(
  average_matrices_list = CNN_2nd_level_matrices,
  corr_method = "pearson",
  n_samples = 80, # Number of participants 
  alpha = 0.05, # Significance level
  adjustment_method = "BH"
)

#### Step 25.3: Aggregate all 2nd-level ALL subjects correlation matrices (for significance testing on non-parametric methods) #### 
CNN_2nd_level_matrices_all_subjects <- list(
  result_2nd_level_cosine_40x40_comb_pearson_multimodal = result_2nd_level_cosine_40x40_pearson_multimodal$l_roi_cosine_matrices_40_comb_fmri$all_2nd_level_corr_matrices,
  result_2nd_level_cosine_40x40_eng_pearson_multimodal = result_2nd_level_cosine_40x40_pearson_multimodal$l_roi_cosine_matrices_40_eng_fmri$all_2nd_level_corr_matrices,
  result_2nd_level_cosine_40x40_spa_pearson_multimodal = result_2nd_level_cosine_40x40_pearson_multimodal$l_roi_cosine_matrices_40_spa_fmri$all_2nd_level_corr_matrices,
  result_2nd_level_cosine_40x40_comb_spearman_multimodal = result_2nd_level_cosine_40x40_spearman_multimodal$l_roi_cosine_matrices_40_comb_fmri$all_2nd_level_corr_matrices,
  result_2nd_level_cosine_40x40_eng_spearman_multimodal = result_2nd_level_cosine_40x40_spearman_multimodal$l_roi_cosine_matrices_40_eng_fmri$all_2nd_level_corr_matrices,
  result_2nd_level_cosine_40x40_spa_spearman_multimodal = result_2nd_level_cosine_40x40_spearman_multimodal$l_roi_cosine_matrices_40_spa_fmri$all_2nd_level_corr_matrices
)

#### Step 25.4 Significance testing on non-parametric correlation methods for Spearman - max-T permutation test ####
MaxT_Results_CNN <- f_process_matrices_maxT(
  average_matrices_list = CNN_2nd_level_matrices,
  subject_matrices_list = CNN_2nd_level_matrices_all_subjects,
  n_permutations = 5000,
  alpha = 0.05
)
#print(MaxT_Results_CNN$result_2nd_level_cosine_40x40_comb_pearson_multimodal$Heatmap_Significant)

##### Step 26: Plot the P-adjusted matrix for the multi-modal 2nd-level matrices ####

## Cosine - Pearson ##
heatmap_p_adj_cosine_pearson_multimodal <- f_plot_p_adjusted_matrix_text(FDR_significance_results_2nd_level_matrices_CNN$result_2nd_level_cosine_40x40_comb_pearson_multimodal$P_Adjusted_Matrix, 
                                                              title = "FDR-Adjusted P-Values - Multimodal Cosine Pearson")

## Cosine - Pearson Permutation ##
heatmap_p_adj_cosine_pearson_Permut_multimodal <- f_plot_p_adjusted_matrix_text(MaxT_Results_CNN$result_2nd_level_cosine_40x40_comb_pearson_multimodal$P_Values, 
                                                                         title = "FDR-Adjusted P-Values - Multimodal Cosine Max T - Pearson ")
## Cosine - Spearman Permutation ##
heatmap_p_adj_cosine_spearman_Permut_multimodal <- f_plot_p_adjusted_matrix_text(MaxT_Results_CNN$result_2nd_level_cosine_40x40_comb_spearman_multimodal$P_Values, 
                                                                                title = "FDR-Adjusted P-Values - Multimodal Cosine Max T- Spearman")
#print(heatmap_p_adj_cosine_spearman_Permut_multimodal)
#### Step 27: Plot the correlation matrix for the multimodal 2nd-level matrices ####

#### Function to plot the p-adjusted matrix with p-values displayed ####
f_plot_cormat_matrix_text <- function(p_adjusted_matrix, title = "Adjusted P-Values Matrix") {
  # Convert matrix to long format
  long_p_matrix <- f_corr_mat_to_long_format2(p_adjusted_matrix)  # Convert to long format
  
  # Round p-values for better readability
  long_p_matrix$Value <- round(long_p_matrix$Value, 2)
  
  # Create the heatmap with text labels
  heatmap_p_values <- ggplot(long_p_matrix, aes(x = Column, y = Row)) +
    geom_tile(fill = "white", color = "black") +  # White background with black grid
    geom_text(aes(label = Value), size = 1.5) +  # Overlay p-values as text
    labs(title = title, x = "Matrices", y = "Matrices") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.major = element_blank(),  # Remove major grid lines
          panel.grid.minor = element_blank())  # Remove minor grid lines
  
  return(heatmap_p_values)
}

## Cosine - Pearson ##
heatmap_corrmat_cosine_pearson_multimodal <- f_plot_cormat_matrix_text(FDR_significance_results_2nd_level_matrices_CNN$result_2nd_level_cosine_40x40_comb_pearson_multimodal$Correlation_Matrix, 
                                                                         title = "Multimodal Cosine RDM - Pearson")
## Cosine - Pearson - max T ##
heatmap_corrmat_cosine_pearson_maxT_multimodal <- f_plot_cormat_matrix_text(MaxT_Results_CNN$result_2nd_level_cosine_40x40_comb_pearson_multimodal$Observed_Matrix, 
                                                                       title = "Multimodal Cosine RDM - Max T Pearson")

## Cosine - Spearman ##
heatmap_corrmat_cosine_spearman_multimodal <- f_plot_cormat_matrix_text(MaxT_Results_CNN$result_2nd_level_cosine_40x40_comb_spearman_multimodal$Observed_Matrix, 
                                                                           title = "Multimodal Cosine RDM - Max T Spearman")
#print(heatmap_corrmat_cosine_spearman_multimodal)
##### Step 28: Generate filtered matrices for the multimodal 2nd-level matrix #####

#### Function to filter and plot ####
f_filter_and_plot_per_condition_multimodal <- function(result_per_condition, title_prefix) {
  # Initialize a list to store filtered matrices and heatmaps for all conditions
  filtered_results <- list()
  
  # Loop through each condition in your result object
  for (condition_name in names(result_per_condition)) {
    message("Processing condition: ", condition_name)
    
    # Extract the average correlation matrix and labels for the condition
    corr_matrix <- result_per_condition[[condition_name]]$average_corr_matrix
    labels <- result_per_condition[[condition_name]]$matrix_labels
    
    # 1) ROWS: keep only fMRI by EXCLUDING Behavioral and CNN
    #    i.e., rows that do NOT match "Behavioral|CNN"
    filtered_rows <- grepl("Behavioral|CNN", labels)
    
    # 2) COLUMNS: keep only Behavioral + CNN, EXCLUDING fMRI
    #    i.e., columns that DO match "Behavioral|CNN"
    filtered_cols <- !grepl("Behavioral|CNN", labels)
    
    # Print which rows and columns survived the filter
    message("Filtered rows for condition: ", condition_name)
    message(paste(labels[filtered_rows], collapse = ", "))
    
    message("Filtered columns for condition: ", condition_name)
    message(paste(labels[filtered_cols], collapse = ", "))
    
    # Check if filtering resulted in valid rows and columns
    if (sum(filtered_rows) == 0 || sum(filtered_cols) == 0) {
      warning("No matching rows or columns found for condition: ", condition_name)
      next
    }
    
    # Subset the correlation matrix
    filtered_corr_matrix <- corr_matrix[filtered_rows, filtered_cols, drop = FALSE]
    
    # Convert the filtered matrix to long format for heatmap plotting
    long_format <- f_convert_corr_matrix_to_long_format(filtered_corr_matrix)
    
    # Create and plot the heatmap
    heatmap_title <- paste(title_prefix, "-", condition_name)
    heatmap_plot <- f_create_heatmap(long_format, heatmap_title)
    
    # Display the heatmap
    print(heatmap_plot)
    
    # Store the results for the condition
    filtered_results[[condition_name]] <- list(
      filtered_corr_matrix = filtered_corr_matrix,
      heatmap_plot = heatmap_plot,
      filtered_in_rows = labels[filtered_rows],
      filtered_in_cols = labels[filtered_cols]
    )
  }
  
  return(filtered_results)
}

### 40x40 ###
## Cosine distance - Pearson ##
filtered_corr_matrix_cosine_fmri_rows_40x40_pearson <- f_filter_and_plot_per_condition_multimodal(
  result_per_condition =  result_2nd_level_cosine_40x40_pearson_multimodal,
  title_prefix = "Filtered Pearson Correlation Cosine Distance 2nd-Level Matrix 40x40"
)

## Cosine distance - Spearman ##
filtered_corr_matrix_cosine_fmri_rows_40x40_Spearman <- f_filter_and_plot_per_condition_multimodal(
  result_per_condition = result_2nd_level_cosine_40x40_spearman_multimodal,
  title_prefix = "Filtered Spearman Correlation Cosine Distance 2nd-Level Matrix 40x40"
)

##### Step 29: Filter significant matrices #####

#### Function to filter and plot significant values ####
f_filter_and_plot_significant <- function(significance_results,
                                          row_pattern,       # e.g., "Behavioral|CNN" or whatever you need
                                          col_pattern,       # e.g., "comb|eng|spa" or "Behavioral|CNN"
                                          title_prefix = "Filtered Significant") {
  # Initialize a list to store the filtered matrices and plots
  filtered_results <- list()
  
  # Loop over each matrix in the significance results
  for (matrix_name in names(significance_results)) {
    cat("Processing matrix:", matrix_name, "\n")
    
    # Extract the significant matrix (cells that survived your p-value threshold)
    sig_matrix <- significance_results[[matrix_name]]$Significant_Matrix
    
    # Safety check in case some matrix doesn't exist or wasn't significant at all
    if (is.null(sig_matrix)) {
      warning("No significant matrix found for: ", matrix_name)
      next
    }
    
    # The row/column labels for filtering are simply the row/col names of sig_matrix
    labels <- rownames(sig_matrix)
    
    # 1) Filter rows
    filtered_rows <- grepl(row_pattern, labels)
    
    # 2) Filter columns
    filtered_cols <- grepl(col_pattern, labels)
    
    # If no rows/columns remain, skip
    if (sum(filtered_rows) == 0 || sum(filtered_cols) == 0) {
      warning("No matching rows or columns for matrix: ", matrix_name)
      next
    }
    
    # Subset the significant matrix
    filtered_sig_matrix <- sig_matrix[filtered_rows, filtered_cols, drop = FALSE]
    
    # Convert to long format for ggplot
    long_format <- f_corr_mat_to_long_format2(filtered_sig_matrix)  # or your existing function
    
    # Create a heatmap of just the significant portion
    heatmap_title <- paste(title_prefix, "-", matrix_name)
    heatmap_plot <- ggplot(long_format, aes(x = Column, y = Row, fill = Value)) +
      geom_tile() +
      # You can tweak the color scale as you wish
      scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0,
                           limits = c(-1, 1), na.value = "white") +
      labs(title = heatmap_title, x = "Matrices", y = "Matrices") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Print the heatmap to see it
    print(heatmap_plot)
    
    # Store the results in a list
    filtered_results[[matrix_name]] <- list(
      filtered_matrix = filtered_sig_matrix,
      heatmap_plot = heatmap_plot,
      filtered_rows = labels[filtered_rows],
      filtered_cols = labels[filtered_cols]
    )
  }
  
  return(filtered_results)
}

#### Step 29.1: Filter and plot the significant results - parametric ####
filtered_sig_results_pearson <- f_filter_and_plot_significant(
  significance_results = FDR_significance_results_2nd_level_matrices_CNN,
  row_pattern = "fmri|roi_cosine_matrices",   # Adjust to match your fMRI naming
  col_pattern = "Behavioral|CNN",
  title_prefix = "Significant Only"
)

#### Step 29.2: Filter and plot the significant results - non-parametric ####
filtered_sig_results_spearman <- f_filter_and_plot_significant(
  significance_results = MaxT_Results_CNN,
  row_pattern = "fmri|roi_cosine_matrices",   # Adjust to match your fMRI naming
  col_pattern = "Behavioral|CNN",
  title_prefix = "Significant Only"
)

##### Step 30: CNN - 2nd level correlation matrix MDS application #####

#### Step 30.1: Additional matrix labels (matching filtered_corr_matrix row names) #####
cnn_labels <- c(
  "CNN_similarity_matrix_english", "CNN_similarity_matrix_spanish",
  "CNN_english_color_similarity_matrix", "CNN_spanish_color_similarity_matrix",
  "CNN_english_color_proportions_similarity_matrix", "CNN_spanish_color_proportions_similarity_matrix",
  "CNN_english_edge_canny_similarity_matrix", "CNN_spanish_edge_canny_similarity_matrix",
  "CNN_english_hed_similarity_matrix", "CNN_spanish_hed_similarity_matrix",
  "CNN_english_category_finetuned_ResNet50_similarity_matrix", "CNN_spanish_category_finetuned_ResNet50_similarity_matrix",
  "CNN_english_animacy_finetuned_ResNet50_similarity_matrix", "CNN_spanish_animacy_finetuned_ResNet50_similarity_matrix"
)

##### Step 30.2: Additional corresponding short labels #####
cnn_short_labels <- c(
  "OB-SimENG", "OB-SimESP",
  "OB-ColENG", "OB-ColESP",
  "OB-ColPENG", "OB-ColPESP",
  "OB-EdgeCENG", "OB-EdgeCESP",
  "OB-HED-ENG", "OB-HED-ESP",
  "OB-CatENG", "OB-CatESP",
  "OB-AniENG", "OB-AniESP"
)

##### Step 30.3: Combine CNN short labels & existing ones #####
names(cnn_short_labels) <- cnn_labels 
##### Step 30.4: Append to `all_short_labels_combined` #####
all_short_labels_combined_cnn <- c(all_short_labels_combined, cnn_short_labels)

### Cosine Similarity Distance ###
## Pearson filtered 2nd-level multimodal matrix ##
mds_filtered_behavioral_CNN_2nd_level_cosine_pearson <- f_apply_mds_with_filtered_data(
  FDR_significance_results_2nd_level_matrices_CNN$result_2nd_level_cosine_40x40_comb_pearson_multimodal$Correlation_Matrix,  # Full correlation matrix including CNN
  filtered_corr_matrix_cosine_fmri_rows_40x40_pearson$l_roi_cosine_matrices_40_comb_fmri$filtered_corr_matrix,
  all_short_labels_combined_cnn,  # Updated short labels with CNN
  "MDS Plot for 2nd-Level Matrix Including CNN (Cosine Similarity & Pearson)"
)
#print(mds_filtered_behavioral_CNN_2nd_level_cosine_pearson$plot)

## Spearman filtered 2nd-level multimodal matrix ##
mds_filtered_behavioral_CNN_2nd_level_cosine_spearman <- f_apply_mds_with_filtered_data(
  MaxT_Results_CNN$result_2nd_level_cosine_40x40_comb_spearman_multimodal$Observed_Matrix,  # Full correlation matrix including CNN
  filtered_corr_matrix_cosine_fmri_rows_40x40_Spearman$l_roi_cosine_matrices_40_comb_fmri$filtered_corr_matrix,
  all_short_labels_combined_cnn,  # Updated short labels with CNN
  "MDS Plot for 2nd-Level Matrix Including CNN (Cosine Similarity & Spearman)"
)
#print(mds_filtered_behavioral_CNN_2nd_level_cosine_spearman$plot)

## Visualization of numerical p-values ##
#View(FDR_significance_results_CNN_2nd_level_matrices$result_2nd_level_cosine_40x40_pearson_multimodal$Correlation_Matrix)
#View(FDR_significance_results_CNN_2nd_level_matrices$result_2nd_level_cosine_40x40_spearman_multimodal$Correlation_Matrix)


### Additionals: Generate a barplot comparing behavioral-fmri and CNN matrices. ###












