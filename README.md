# Similarity Judgments & RSA Analysis Project

This repository contains all analysis scripts, data, and resources for a project investigating similarity judgments across multiple similarity properties and using representational similarity analysis (RSA) on behavioral, neural and CNN data to evaluate which properties correlate with different cortical brain regions and which conditions are required for CNNs and algorithmic structures to correlate with the other 2 modalities. It also assess the relationships and correlations across different correlation metrics and correlation methods. Besides, it explores the methodological constraints on noise ceilings and propose a different alternative to visualize and assess them in the context of multi-modal RSA, particularly for fMRI.  

---

## 📂 Project Structure

similarity_rsa_project/
│
├── R/ # R scripts and workspaces
│ ├── RSA project - phase 1_&_2_final          # analysis_script1.R
│ ├── RSA_project_Phase_3_final                # analysis_script2.R
│ ├── Workspace_RSA_analysis_Phase1_&_2_final  # workspace1.RData
│ └── phase_3_workspace_final                  # workspace2.RData
│
├── python/ # Python script and neural network adaptation
│ ├── RSA_CNN_project_analysis_final           # analysis_script.py
│ ├── Image-similarity/                        # Modified neural network component for local reading of images
│ └── RSA_english_images/                      # Images from the English population 
│ └── RSA_spanish_images/                      # Images from the Spanish population
│
├── data/ # Raw, de-identified datasets
│ ├── similarity_task_english.csv
│ └── similarity_task_spanish.csv
│
├── README.md # This file
└── .gitignore # Files excluded from version control

---

## 🚀 Getting Started

### Requirements

This project uses both **R** and **Python**. Below are the key tools used:

- R (v4.3+)
  - Packages analysis script1: `stringr`, `tidyr`,`Matrix`,`dplyr`,`tibble`,`softImpute`,`mice`,`lattice`,`rasterVis`, `raster`, `gridExtra`,`DT`,`psych`.
  - Packages analysis script2: `reticulate`,`ggplot2`,`MASS`,`lattice`,`reshape2`,`scales`,`cluster`,`boot`,`tidyr`,`dplyr`,`stats`,`gridExtra`,`purrr`,`viridis`.     
- Python (v3.10+)
  - Libraries: `cv2`,`h5py`, `hashlib`, `humans`, `local_similarity`,`main_multi`, `matplotlib`, `models`, `numpy`,`os`,`pandas`, `random`, `seaborn`,`skimage`, 
  `sklearn`, `sys`, `tensorflow`, `torch`, `torchvision`.

> 📌 To install R dependencies:
You can use `install.packages()` or load them from a script.

> 📌 To install Python dependencies:
You can use `pip install -r requirements.txt`
# 💡 Note: Some modules are custom modules located in the modified neural network folder. Please read the script analysis on the steps performed.  

