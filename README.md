# Similarity Judgments & RSA Analysis Project

This repository contains the full analysis pipeline and code for the project investigating similarity judgments across multiple dimensions using representational similarity analysis (RSA). The project integrates behavioral, neural (fMRI), and CNN-derived dissimilarity matrices to evaluate cross-modal relationships across similarity metrics, and proposes methodological advances in estimating and visualizing noise ceilings in multimodal RSA.

---

## 📂 Project Structure

```bash
similarity_rsa_project/
│
├── R/                          # R scripts
│   ├── RSA_project_Phase_1_2_final.R       # Analysis script for Phase 1 and 2 (behavioral)
│   └── RSA_project_Phase_3_final.R         # Analysis script for Phase 3 (Bimodal and multimodal)
│
├── Python/                     # Python analysis and neural network adaptation
│   ├── RSA_CNN_project_analysis_final.py   # Analysis script
│   ├── image-similarity/                   # Modified MobileNet-based repo for local image processing
│   ├── RSA_english_images/                 # English image set
│   └── RSA_spanish_images/                 # Spanish image set
│
├── .gitignore
└── README.md
```

---

## 🚀 Getting Started

### Requirements

This project uses both **R** and **Python**. Below are the key packages used:

- **R (v4.3+)**
  - Script 1: `stringr`, `tidyr`, `Matrix`, `dplyr`, `tibble`, `softImpute`, `mice`, `lattice`, `rasterVis`, `raster`, `gridExtra`, `DT`, `psych`
  - Script 2: `reticulate`, `ggplot2`, `MASS`, `lattice`, `reshape2`, `scales`, `cluster`, `boot`, `tidyr`, `dplyr`, `stats`, `gridExtra`, `purrr`, `viridis`

- **Python (v3.10+)**
  - Libraries: `cv2`, `h5py`, `hashlib`, `matplotlib`, `numpy`, `os`, `pandas`, `random`, `seaborn`, `skimage`, `sklearn`, `sys`, `tensorflow`, `torch`, `torchvision`
  - Plus custom modules in `image-similarity/` like `local_similarity`, `main_multi` and `model_util`.

> 💡 To install Python dependencies:
```bash
pip install -r requirements.txt
```

> 💡 For R dependencies:
```bash
Use `install.packages()` or load them directly in the R scripts.
```
---

## 📁 Data Availability

The raw datasets, the workspace for script1, behavioral and neural matrices and `.RData` workspaces are hosted on the [Open Science Framework (OSF)](https://osf.io/fkzuh/) due to GitHub’s file size limitations. The second workspace (of analysis script phase 3 - 30Gb) has been omitted given the size limitations on open data repositories. 

Available files:
- `similarity_task_english.csv`                     # raw dataset
- `similarity_task_spanish.csv`                     # raw dataset
- `Workspace_RSA_analysis_Phase1_&_2_final.RData`   # workspace for analysis script phase 1 and 2
- `RSA_All_40by40.pkl`                              # Neural dataset for the 40x40 voxel-based RSA matrices across regions.
- `RSA_All.pkl`                                     # Neural dataset for the 280x280 voxel-based RSA matrices across regions.
- `l_final_matrices_280x280_2.rds`                  # Behavioral matrices - Exemplar matrices (280x280) - Outcome of analysis script phase 1 and 2.  
- `l_final_matrices_40x40_2.rds`                    # Behavioral matrices - Category matrices (40x40) - Outcome of analysis script phase 3 and 4. 
- `Supplementary materials`                          

> 📌 Please download these manually to the appropriate folders before running the full analysis.

---

## 🙌 Acknowledgments

This project builds upon and adapts multiple open-source tools and pre-trained models:

- The `Python/image-similarity/` folder is adapted from [ryanfwy/image-similarity](https://github.com/ryanfwy/image-similarity), based on MobileNet (Howard et al., 2017), modified for offline image similarity estimation.
- Deep learning models used include MobileNet, ResNet-50 (fine-tuned for category and animacy classification), and HED (Holistically-Nested Edge Detection) for shape extraction.
- Traditional algorithms include Canny edge detection (Canny, 1986) and SSIM (Structural Similarity Index).
- Color similarity was estimated using histogram-based methods and clustering with OpenCV and `scikit-learn`.

All model citations and methodological descriptions are detailed in the **main manuscript** and **supplementary materials**.

Thanks to the PAC (Perception, Attention, and Consciousness) Lab at the University of Florida for their continued support and insightful feedback throughout the project.

---

## 📄 License

This project is licensed under the [MIT License](LICENSE).

If you use this repository or its associated datasets, please cite the original article (as outlined in the main manuscript and supplementary files). The dataset is shared for **non-commercial academic use** only. For broader usage, please contact the author.
