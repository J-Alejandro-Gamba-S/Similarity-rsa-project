# Similarity Judgments & RSA Analysis Project

This repository contains all analysis scripts and resources for a project investigating similarity judgments across multiple properties, using representational similarity analysis (RSA) on behavioral, neural, and CNN-derived data. The analysis evaluates which properties correlate with specific cortical brain regions and explores the conditions required for algorithmic and CNN-based similarity to align with behavior and fMRI. The project also examines different correlation metrics and methods, and proposes methodological improvements for estimating and visualizing noise ceilings in multi-modal RSA.

---

## 📂 Project Structure

```bash
similarity_rsa_project/
│
├── R/                          # R scripts
│   ├── RSA_project_Phase_1_2_final.R
│   └── RSA_project_Phase_3_final.R
│
├── Python/                     # Python analysis and neural network adaptation
│   ├── RSA_CNN_project_analysis_final.py
│   ├── image-similarity/      # Modified MobileNet-based repo for local image processing
│   ├── RSA_english_images/    # English image set
│   └── RSA_spanish_images/    # Spanish image set
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

The raw datasets and `.RData` workspaces are hosted on the [Open Science Framework (OSF)](https://osf.io/fkzuh/) due to GitHub’s file size limitations.

Available files:
- `similarity_task_english.csv`
- `similarity_task_spanish.csv`
- `Workspace_RSA_analysis_Phase1_&_2_final.RData`
- `phase_3_workspace_final.RData`

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
