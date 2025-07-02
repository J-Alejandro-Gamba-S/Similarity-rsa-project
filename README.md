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


## 🙌 Acknowledgments ##

* This project builds upon and adapts several open-source tools and models. We are grateful to the developers and communities maintaining these resources:

- The folder `Python/image-similarity/` is adapted from the open-source repository [ryanfwy/image-similarity](https://github.com/ryanfwy/image-similarity), originally based on MobileNet (Howard et al., 2017). We modified it for local image processing and offline cosine similarity estimation.

- For pretrained deep learning models, we used `MobileNet`, `ResNet-50`, and the Holistically-Nested Edge Detection (HED) network. These models were accessed via `torchvision` and custom implementations, and were either used as-is or fine-tuned for category, animacy, and shape-based classification tasks.

- The Canny edge detector (Canny, 1986) and structural similarity index (SSIM) were employed to estimate shape-based similarity using classical computer vision techniques.

- Color-based dissimilarity was computed using histogram distributions and clustering-based color extraction, relying on `OpenCV` and `scikit-learn`.

- The ResNet-50 architecture was also fine-tuned for category and animacy classification. These models were trained using PyTorch, leveraging stratified splits, augmentation pipelines, and feature embedding extraction to generate similarity matrices.

All source citations and methodological details are available in the main manuscript and supplementary materials accompanying the publication.

We also thank the members of the PAC Lab at the University of Florida for their feedback and support throughout this project. 

## 📄 License ##

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.

If you use this code or dataset in your work, please cite the original paper as described in the main manuscript and supplementary materials.

Note: The datasets provided (`data/similarity_task_english.csv`, `data/similarity_task_spanish.csv`) are shared for non-commercial academic use only. Please contact the author for reuse beyond this scope.
