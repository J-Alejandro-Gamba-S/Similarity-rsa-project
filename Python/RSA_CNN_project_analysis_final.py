# -*- coding: utf-8 -*-
"""
RSA Project Extension 1 - Computation of Objective Representational Spaces (English and Spanish).
# University of Florida - PAC LAB
# Written by Alejandro Gamba. 

Background: Previously in our lab, we collected similarity ratings of 280 images across 8 different properties
(General similarity, color, shape, category, animacy, dissimilarity, preference and scariness) to generate Behavioral 
RDMs (Representational Dissimilarity Matrix) and then assess their correlation with fMRI RDMs from the same images
across different metrics and correlation methods. 
  Here we want to use CNN and algorithms to compute the similarity representational space. 
   
RQ: How do AI-based representational spaces on the same image database resemble to similarity judgments spaces
from humans and brain activation patterns across our 11ROI ? 

Hyp:  
H1: The AI-based representational spaces correlate significantly with behavioral human similarity spaces and brain regions.
H0: The AI-based representational spaces does not correlate significantly with behavioral human similarity spaces and brain regions. 
H2: Certain AI-based representational spaces correlate with human similarity spaces and brain regions, but not others. 

-------------------------------------------------------------------------------

Code Summary: 
#### Section I: Opening the data, libraries and generate CSV files ####
- Step 1: Open data and import libraries 
- Step 2: Import Modified or downloaded resources (ImageSimilarity,MobileNet)
- Step 3: Generate sorted CSVs (English & Spanish)  

#### Section II: General-featured based extraction similarity matrix #### 
(Check the warning about running the code in this section!!)
- Step 4: Override the _sub_process () for offline images (& explanation)
- Step 5: Extract features from 280 images (EN & ES) 
- Step 6: Compute cosine similarity matrix between features vectors
- Step 7: Visualization of both RDMs 

#### Section III: 1st Feature: Color ####          
Method 1: Based on Histogram Distribution of Color
- Step 8: Load Required Dependencies and Directories
- Step 9: sorting and extracting color histogram 
- Step 10: Compute the cosine similarity matrix 
- Step 11: Visualize the similarity matrix 

Method 2: Based on color extraction and proportion from a global palette
- Step 12: Load Required Dependencies and Directories
- Step 13: Define required functions
    - Extract Pixels from All Images to Define Global Palette 
    - Define Global Color Palette 
    - Extract Color Proportions Based on Global Palette 
    - Process All Images and Generate Feature Vectors 
    - Function to Compute Similarity Matrix 
    - Visualize Similarity Matrix 
- Step 14: Define an integrated function to run for Full Dataset 
- Step 15: Define the image paths for each language
- Step 16: Process English Dataset 
- Step 17: Process Spanish Dataset 

#### Section IV: 2nd Feature: Shape ####
Method 1: Canny Edge Detection
- Step 18: Load Required Dependencies and Directories 
- Step 19: sort and extract edge features 
- Step 20: Compute the cosine similarity matrix 
- Step 21: Visualize the similarity matrix

Method 2: Holistically-Nested Edge Detection (HED)
- Step 22: Set seed for reproducibility
- Step 23: Load the HED model and configure training
- Step 24: Define training configuration
- Step 25: Prepare dataset with stratified split
- Step 26: Fine-tune the model
- Step 27: Compute SSIM for each image
- Step 28: Evaluate model performance
- Step 29: Save the fine-tuned model
- Step 30: Visualize edge detection with SSIM
- Step 31: Extract HED features for English images
- Step 32: Visualize English HED similarity matrix
- Step 33: Extract HED features for Spanish images
- Step 34: Visualize Spanish HED similarity matrix

#### Section V: 3rd Feature: Category ####
- Step 35: Fine-tune ResNet50 on 40 categories
- Step 36: Evaluate classification accuracy
- Step 37: Extract features from fine-tuned model
- Step 38: Generate category similarity matrices (English and Spanish)

#### Section VI: 4th Feature: Animacy ####
- Step 39: Fine-tune ResNet50 for binary animacy classification
- Step 40: Evaluate animacy classification accuracy
- Step 41: Extract features after custom FC
- Step 42: Generate animacy similarity matrices (English and Spanish)

#### Section VII: Transformations ####
- Step 43: Convert image-level similarity matrices to category-level dissimilarity matrices
- Step 44: Export final matrices as CSV

#### Notes ####
- Additional exploratory block on ResNet-50 direct classification is included at the end for benchmarking purposes.

Created on Fri Dec 20 10:55:45 2024
@author: j.gambasegovia
"""

# In[ ]:

# ----------------------------------------------------------------------------#
#### Section I: Opening the data, libraries and generate CSV files  ##############
# ----------------------------------------------------------------------------#

#### Previous steps done outside of the script: ####################### 
# - Configure a new Conda environment
# - Clone the Image Similarity Utility with MobileNet: (https://github.com/ryanfwy/image-similarity) 
# - Installed tensorflow and required libraries
# - Organize the images in a folder of the desired directory. 
# - update the main_multi.py and model_util.py for local images.  
#######################################################################

###### Step 1: Open data and import libraries ######
import torch
import os
import sys
import pandas as pd
from tensorflow.keras.applications.mobilenet import MobileNet, preprocess_input
import numpy as np
from sklearn.metrics.pairwise import cosine_similarity
import h5py
import hashlib

###### Step 2: Import Modified or downloaded resources ######
# Ensure the path to "image-similarity" is added to the Python path
similarity_path = r'C:\Users\j.gambasegovia\Documents\Papeles Alejo UF\1 - RSA - R.Project\RSA_CNN\image-similarity'
sys.path.append(similarity_path)
from main_multi import ImageSimilarity, DeepModel
# Import the LocalImageSimilarity class from external module
from local_similarity import LocalImageSimilarity
# Verify TensorFlow version and installation path
import tensorflow as tf

## Define directories for English and Spanish images ##
english_image_dir = r'C:\Users\j.gambasegovia\Documents\Papeles Alejo UF\1 - RSA - R.Project\RSA_CNN\RSA_english_images'
spanish_image_dir = r'C:\Users\j.gambasegovia\Documents\Papeles Alejo UF\1 - RSA - R.Project\RSA_CNN\RSA_spanish_images'

## Number of same images between both directories ##
#### 
def get_file_hash(file_path):
    """
    Generate the hash of a file to compare its content.
    """
    hasher = hashlib.md5()
    with open(file_path, 'rb') as f:
        buf = f.read()
        hasher.update(buf)
    return hasher.hexdigest()

#### Define a function to compare images ####
def compare_images(dir1, dir2):
    """
    Compare images in two directories and find how many are the same.
    """
    # Get all files from both directories
    files_dir1 = [os.path.join(dir1, f) for f in os.listdir(dir1) if os.path.isfile(os.path.join(dir1, f))]
    files_dir2 = [os.path.join(dir2, f) for f in os.listdir(dir2) if os.path.isfile(os.path.join(dir2, f))]

    # Generate hash maps for each directory
    hashes_dir1 = {get_file_hash(file): file for file in files_dir1}
    hashes_dir2 = {get_file_hash(file): file for file in files_dir2}

    # Find common hashes
    common_hashes = set(hashes_dir1.keys()) & set(hashes_dir2.keys())

    print(f"Number of identical images: {len(common_hashes)}")

    # Optional: Print common file names if needed
    for h in common_hashes:
        print(f"Common file: {hashes_dir1[h]} and {hashes_dir2[h]}")

## Compare images in the two directories ##
compare_images(english_image_dir, spanish_image_dir) ## 151/280 identical images

## category names ##
category_names = [
    "snake", "Frog", "Cockroach", "Ants", "spider", "Worms", "Bee", "Butterflies",
    "Birds", "Bat", "Chicken", "Mouse", "dog", "Cat", "Horses", "Shark", "Fish", "Gecko",
    "Turtle", "Beatle", "Grasshoper", "Catterpillar", "Fly", "Peacock", "Guinea pig",
    "Sheep", "Rabbit", "Giraffe", "Whale", "Dolphin", "Airplanes", "Car", "Bicycle",
    "Scissor", "Hammer", "Key", "Guitar", "Cellphone", "Umbrella", "Chair"
]

## Output paths for the CSV files ##
output_dir = r'C:\Users\j.gambasegovia\Documents\Papeles Alejo UF\1 - RSA - R.Project\RSA_CNN'
english_csv_path = os.path.join(output_dir, 'english_images.csv')
spanish_csv_path = os.path.join(output_dir, 'spanish_images.csv')

#### function for step 3: Utility Function for Sorting Images by Category ####
def sort_images_by_category(image_dir):
    """
    Sort images by their inferred category based on filenames.

    Args:
        image_dir (str): Directory containing the images.

    Returns:
        List[str]: Sorted list of image paths.
    """
    image_paths = [
        os.path.join(image_dir, f)
        for f in os.listdir(image_dir)
        if f.endswith(('.png', '.jpg', '.jpeg'))
    ]
    # Sort images based on the numeric value extracted from filenames
    sorted_image_paths = sorted(
        image_paths,
        key=lambda x: int(os.path.basename(x).split('_')[0])
    )
    return sorted_image_paths

#### function for step 3 Generate CSV Files for Sorted Images ####
def generate_sorted_csv(image_dir, output_csv_path):
    sorted_image_paths = sort_images_by_category(image_dir)
    num_images = len(sorted_image_paths)
    print(f"Total sorted images in {image_dir}: {num_images}")

    if num_images != 280:
        print(f"Warning: Expected 280 images but found {num_images}.")

    # Create DataFrame for CSV
    image_data = pd.DataFrame({
        'id': range(1, num_images + 1),
        'path': sorted_image_paths
    })

    image_data.to_csv(output_csv_path, index=False)
    print(f"Sorted CSV file created at: {output_csv_path}")

###### Step 3: Generate sorted CSV files for English and Spanish images ######
generate_sorted_csv(english_image_dir, english_csv_path)
generate_sorted_csv(spanish_image_dir, spanish_csv_path)


# In[ ]:

# ----------------------------------------------------------------------------#
#### Section II: General-featured based extraction similarity matrix  #########
# ----------------------------------------------------------------------------#

## required libraries ##
import matplotlib.pyplot as plt
import seaborn as sns
    
###### Step 4: Override the _sub_process () for offline images ######
## Rather than modifying it in this script, to run it properly, generate a new py file with ##
## definition of the new subclass that handles the offline images. ##   

## !! warning: I could only run this code properly by selecting it (from step 4 to step 8 included) 
## and run it separately. When run the complete file, it enters a infinite cycle to extract 
## features. I am unsure why that happens !! ## 

###### Step 5: Test the set-up for feature extraction of each of the 280 images for both languages ######
## Duration: 70-80s each - 160s in total  ##
if __name__ == "__main__":
    # Initialize the custom LocalImageSimilarity class
    similarity = LocalImageSimilarity()

    # Setup batch size and number of processes
    similarity.batch_size = 16
    similarity.num_processes = 2

    # Load data from English and Spanish CSV files
    print("Loading CSV data...")
    english_data = similarity.load_data_csv(english_csv_path, delimiter=',')
    spanish_data = similarity.load_data_csv(spanish_csv_path, delimiter=',')

    print("Extracting features...")
    similarity.save_data('english_features', english_data)
    similarity.save_data('spanish_features', spanish_data)
    
    # Extract features into memory
    english_features = similarity._model.extract_feature(similarity._predict_generator(
        [{'path': line[-1], 'fields': line} for line in english_data]
        ))
    spanish_features = similarity._model.extract_feature(similarity._predict_generator(
        [{'path': line[-1], 'fields': line} for line in spanish_data]
        ))
    print("Feature extraction completed. All data saved and loaded into memory.")
     
###### Step 6: Compute cosine similarity matrix between features vectors of the images ######
similarity_matrix_english = cosine_similarity(english_features)
similarity_matrix_spanish = cosine_similarity(spanish_features)

###### Step 7: Visualization of both RDMs ######

## English RDM ##
plt.figure(figsize=(10, 8))
sns.heatmap(similarity_matrix_english, cmap='viridis')
plt.title('English Image Similarity Matrix')
plt.xlabel('Image Index')
plt.ylabel('Image Index')
plt.show()

## Spanish RDM ##
plt.figure(figsize=(10, 8))
sns.heatmap(similarity_matrix_spanish, cmap='viridis')
plt.title('Spanish Image Similarity Matrix')
plt.xlabel('Image Index')
plt.ylabel('Image Index')
plt.show()
         
###### Step 6: Compute cosine similarity matrix between features vectors of the images ######
similarity_matrix_english = cosine_similarity(english_features)
similarity_matrix_spanish = cosine_similarity(spanish_features)

## Expand category names for all 280 images (7 images per category) ##
expanded_category_labels = []
for cat in category_names:
    expanded_category_labels.extend([cat] * 7)

###### Step 7: Visualization of both RDMs ######
## English RDM ##
plt.figure(figsize=(10, 8))
sns.heatmap(similarity_matrix_english, cmap='viridis')
plt.title('English Image Similarity Matrix')
plt.xlabel('Image Index')
plt.ylabel('Image Index')
plt.show()

## Spanish RDM ##
plt.figure(figsize=(10, 8))
sns.heatmap(similarity_matrix_spanish, cmap='viridis')
plt.title('Spanish Image Similarity Matrix')
plt.xlabel('Image Index')
plt.ylabel('Image Index')
plt.show()

## English RDM with category tick labels for reference ##
plt.figure(figsize=(12, 10))
sns.heatmap(similarity_matrix_english, cmap='viridis',
            xticklabels=expanded_category_labels,
            yticklabels=expanded_category_labels)
plt.title('English Image Similarity Matrix (Labelled by Category)')
plt.xticks(rotation=90)
plt.yticks(rotation=0)
plt.xlabel('Image (Category)')
plt.ylabel('Image (Category)')
plt.tight_layout()
plt.show()

# In[ ]:
####### Object-based Features: Color, Shape, Category, and Animacy #######
# ----------------------------------------------------------------------------#
#### -------------- Section III: 1st Feature: Color   -------------------- ####
# ----------------------------------------------------------------------------#

### ---- Method 1: Based on Histogram Distribution of Color ---- ###

###### Step 8: Load Required Dependencies and Directories ####
import cv2
import numpy as np
import os
from sklearn.metrics.pairwise import cosine_similarity
import matplotlib.pyplot as plt
import seaborn as sns

# Define directories for English and Spanish images and output_dir
# Previously defined line 91,92 and 95

#### Function to Sort Images correctly defined in line 99 for step 11 ####
#### Function to extract histogram distribution of color ####
def extract_color_histogram(image_path, bins=(8, 8, 8)):
    """
    Extract a color histogram from the given image path.

    Args:
        image_path (str): Path to the image file.
        bins (tuple): Number of bins for each channel in HSV.

    Returns:
        np.ndarray: Flattened histogram representing the color distribution.
    """
    # Load the image
    image = cv2.imread(image_path)
    # Convert the image from BGR to HSV color space
    hsv_image = cv2.cvtColor(image, cv2.COLOR_BGR2HSV)
    # Compute the color histogram
    hist = cv2.calcHist([hsv_image], [0, 1, 2], None, bins, [0, 180, 0, 256, 0, 256])
    # Normalize the histogram
    hist = cv2.normalize(hist, hist).flatten()
    return hist

### English Images ###
###### Step 9: sorting and extracting color histogram ######
print("Processing sorted English images for color features...")
sorted_english_image_paths = sort_images_by_category(english_image_dir)
english_color_features = [extract_color_histogram(image_path) for image_path in sorted_english_image_paths]

##### Step 10: Compute the cosine similarity matrix ######
english_color_features_array = np.array(english_color_features)
english_color_similarity_matrix = cosine_similarity(english_color_features_array)

###### Step 11: Visualize the similarity matrix ######
plt.figure(figsize=(10, 8))
sns.heatmap(english_color_similarity_matrix, cmap='viridis')
plt.title('English Images - Sorted Color-Based Similarity Matrix')
plt.xlabel('Image Index')
plt.ylabel('Image Index')
plt.show()

### Spanish Images ###
###### Step 09: sorting and extracting color histogram ######
print("Processing sorted Spanish images for color features...")
sorted_spanish_image_paths = sort_images_by_category(spanish_image_dir)
spanish_color_features = [extract_color_histogram(image_path) for image_path in sorted_spanish_image_paths]

###### Step 10: Compute the cosine similarity matrix ######
spanish_color_features_array = np.array(spanish_color_features)
spanish_color_similarity_matrix = cosine_similarity(spanish_color_features_array)

###### Step 11: Visualize the similarity matrix ######
plt.figure(figsize=(10, 8))
sns.heatmap(spanish_color_similarity_matrix, cmap='viridis')
plt.title('Spanish Images - Sorted Color-Based Similarity Matrix')
plt.xlabel('Image Index')
plt.ylabel('Image Index')
plt.show()

print("Color-based (histogram distribution) similarity matrices generated and visualized successfully.")


### ---- Method 2: Based on color extraction and proportion from a global palette ---- ###

###### Step 12: Load Required Dependencies and Directories ######
from sklearn.cluster import KMeans
from sklearn.metrics.pairwise import cosine_similarity
import cv2
import numpy as np
import os
import matplotlib.pyplot as plt
import seaborn as sns

###### Step 13: Define required functions #######
#### Extract Pixels from All Images to Define Global Palette ####
def extract_pixels_from_images(image_paths, sample_fraction=0.1):
    pixels = []
    for image_path in image_paths:
        image = cv2.imread(image_path)
        image = cv2.cvtColor(image, cv2.COLOR_BGR2RGB)
        sampled_pixels = image.reshape(-1, 3)
        sampled_pixels = sampled_pixels[np.random.choice(sampled_pixels.shape[0],
                                                          int(sampled_pixels.shape[0] * sample_fraction), replace=False)]
        pixels.append(sampled_pixels)
    return np.vstack(pixels)

#### Define Global Color Palette ####
def create_global_palette(image_paths, n_colors=9):
    pixels = extract_pixels_from_images(image_paths)
    kmeans = KMeans(n_clusters=n_colors, random_state=42).fit(pixels)
    return kmeans.cluster_centers_, kmeans

#### Extract Color Proportions Based on Global Palette ####
def extract_color_proportions(image_path, kmeans, n_colors):
    image = cv2.imread(image_path)
    image = cv2.cvtColor(image, cv2.COLOR_BGR2RGB)
    pixels = image.reshape(-1, 3)
    labels = kmeans.predict(pixels)
    proportions = np.bincount(labels, minlength=n_colors) / len(labels)
    return proportions

#### Process All Images and Generate Feature Vectors ####
def process_images(image_paths, kmeans, n_colors):
    feature_vectors = []
    for image_path in image_paths:
        proportions = extract_color_proportions(image_path, kmeans, n_colors)
        feature_vectors.append(proportions)
    return np.array(feature_vectors)

#### Function to Compute Similarity Matrix ####
def compute_similarity_matrix(feature_vectors):
    return cosine_similarity(feature_vectors)

#### Visualize Similarity Matrix ####
def visualize_similarity_matrix(similarity_matrix, title):
    plt.figure(figsize=(10, 8))
    sns.heatmap(similarity_matrix, cmap='viridis')
    plt.title(title)
    plt.xlabel('Image Index')
    plt.ylabel('Image Index')
    plt.show()

###### Step 14: Define an integrated function to run for Full Dataset ######
print("Processing full datasets...")
def process_full_dataset(image_paths, output_path, n_colors=9):
    # Create Global Palette
    print(f"Creating global color palette for {len(image_paths)} images...")
    global_palette, kmeans = create_global_palette(image_paths, n_colors=n_colors)

    # Process Images
    print("Extracting color proportions for all images...")
    feature_vectors = process_images(image_paths, kmeans, n_colors)

    # Compute Similarity Matrix
    print("Computing similarity matrix...")
    similarity_matrix = compute_similarity_matrix(feature_vectors)

    # Save Similarity Matrix
    np.save(output_path, similarity_matrix)
    print(f"Similarity matrix saved to {output_path}")

    # Visualize Similarity Matrix
    visualize_similarity_matrix(similarity_matrix, f"Color-Based Similarity Matrix for {len(image_paths)} Images")
    
    return similarity_matrix

###### Step 15: Define the image paths for each language #######
english_image_paths = sorted([os.path.join(english_image_dir, f) for f in os.listdir(english_image_dir) if f.endswith(('.png', '.jpg', '.jpeg'))])
spanish_image_paths = sorted([os.path.join(spanish_image_dir, f) for f in os.listdir(spanish_image_dir) if f.endswith(('.png', '.jpg', '.jpeg'))])

###### Step 16: Process English Dataset ######
english_color_proportions_similarity_matrix = process_full_dataset(english_image_paths, os.path.join(output_dir, 'english_color_proportions_similarity_matrix.npy'))

###### Step 17: Process Spanish Dataset ######
spanish_color_proportions_similarity_matrix = process_full_dataset(spanish_image_paths, os.path.join(output_dir, 'spanish_color_proportions_similarity_matrix.npy'))


print("Color-based (histogram distribution) similarity matrices generated and visualized successfully.")


# In[ ]: 
## Canny edge detection vs HED for shape RDM because CNN might involve semantic content as confound ##
###### 2st Feature: Shape (edge detection) Method 1: Canny Edge detection  ######
# ----------------------------------------------------------------------------#
#### -------------- Section IV: 2nd Feature: Shape  ---------------------- ####
# ----------------------------------------------------------------------------#

### ---- Method 1: Canny Edge detection  ---- ###

###### Step 18: Load Required Dependencies and Directories ######
import cv2
import numpy as np
from sklearn.metrics.pairwise import cosine_similarity
import matplotlib.pyplot as plt
import seaborn as sns
import os

# Define directories for English and Spanish images
# Previously defined line 91,92 and 95

#### Function to Sort Images defined in line 99 for step 17 #### 
#### Function Edge Extraction Using Canny Edge Detection for step 19 ####
def extract_edges(image_path):
    """
    Extract edges from the given image using the Canny edge detection method.

    Args:
        image_path (str): Path to the image file.

    Returns:
        np.ndarray: Flattened array of edge-detected features.
    """
    # Load the image in grayscale
    image = cv2.imread(image_path, cv2.IMREAD_GRAYSCALE)
    # Apply Canny edge detection
    edges = cv2.Canny(image, threshold1=100, threshold2=200)
    # Flatten the edge matrix
    return edges.flatten()

### Process English Images ###
###### Step 19: sort and extract edge features ######
print("Processing sorted English images for edge-based features...")
sorted_english_image_paths = sort_images_by_category(english_image_dir)
english_edge_features = [extract_edges(image_path) for image_path in sorted_english_image_paths]
###### Step 20: Compute the cosine similarity matrix ######
english_edge_features_array = np.array(english_edge_features)
english_edge_canny_similarity_matrix = cosine_similarity(english_edge_features_array)
###### Step 21: Visualize the similarity matrix ####
plt.figure(figsize=(10, 8))
sns.heatmap(english_edge_canny_similarity_matrix, cmap='viridis')
plt.title('English Images - Sorted Edge-Based Similarity Matrix (Canny)')
plt.xlabel('Image Index')
plt.ylabel('Image Index')
plt.show()

### Process Spanish Images ###
###### Step 19: sort and extract edge features ######
print("Processing sorted Spanish images for edge-based features...")
sorted_spanish_image_paths = sort_images_by_category(spanish_image_dir)
spanish_edge_features = [extract_edges(image_path) for image_path in sorted_spanish_image_paths]
###### Step 20: Compute the cosine similarity matrix ######
spanish_edge_features_array = np.array(spanish_edge_features)
spanish_edge_canny_similarity_matrix = cosine_similarity(spanish_edge_features_array)
###### Step 21: Visualize the similarity matrix ######
plt.figure(figsize=(10, 8))
sns.heatmap(spanish_edge_canny_similarity_matrix, cmap='viridis')
plt.title('Spanish Images - Sorted Edge-Based Similarity Matrix (Canny)')
plt.xlabel('Image Index')
plt.ylabel('Image Index')
plt.show()

print("Sorted edge-based similarity matrices (Canny) generated and visualized successfully.")



### ---- Method 2:Based on Holistically-Nested Edge Detection (HED) Deep Neural Network ---- ###

###### Step 22: Load Required Dependencies and Directories ######
import torchvision.transforms as transforms
from torch.utils.data import DataLoader, Dataset
from PIL import Image
from sklearn.model_selection import train_test_split
import os
import cv2
import numpy as np
import torch
import matplotlib.pyplot as plt
from skimage.metrics import structural_similarity as ssim
sys.path.append(r'C:\Users\j.gambasegovia\Documents\Papeles Alejo UF\1 - RSA - R.Project\RSA_CNN\hed')
from models import HED

###### Step 23: Set Seed for Reproducibility ######
def set_seed(seed=42):
    np.random.seed(seed)
    torch.manual_seed(seed)
    torch.cuda.manual_seed_all(seed)
    torch.backends.cudnn.deterministic = True
    torch.backends.cudnn.benchmark = False

set_seed(42)

###### Step 24: Load and Prepare HED Model ######
print("Loading the HED model...")
hed_model = HED()  # Initialize the model
hed_model.train()  # Set the model to training mode

###### Step 25: Define Training Configuration ######
learning_rate = 0.001
epochs = 15 # After 10-15 virtually no differences. 
batch_size = 8
criterion = torch.nn.MSELoss()
optimizer = torch.optim.Adam(hed_model.parameters(), lr=learning_rate)

def sort_images_by_category(image_dir):
    image_paths = [os.path.join(image_dir, f) for f in os.listdir(image_dir) if f.endswith(('.png', '.jpg', '.jpeg'))]
    return sorted(image_paths, key=lambda x: int(os.path.basename(x).split('_')[0]))

def stratified_split_sorted(image_paths, test_size=0.2):
    labels = [int(os.path.basename(path).split('_')[0]) - 1 for path in image_paths]
    train_paths, test_paths = train_test_split(image_paths, test_size=test_size, stratify=labels, random_state=42)
    return train_paths, test_paths

###### Step 26: Dataset Preparation with Stratified Split ######
class EdgeDataset(Dataset):
    def __init__(self, image_paths, transform=None):
        self.image_paths = image_paths
        self.transform = transform
    
    def __len__(self):
        return len(self.image_paths)
    
    def __getitem__(self, idx):
        image_path = self.image_paths[idx]
        image = Image.open(image_path).convert('RGB')
        if self.transform:
            image = self.transform(image)
        return image, image.mean(dim=0, keepdim=True)  # Adjust target to match model output

transform = transforms.Compose([
    transforms.Resize((224, 224)),
    transforms.ToTensor(),
    transforms.Normalize(mean=[0.485, 0.456, 0.406], std=[0.229, 0.224, 0.225])
])

sorted_english_image_paths = sort_images_by_category(english_image_dir)
train_paths, test_paths = stratified_split_sorted(sorted_english_image_paths)

train_dataset = EdgeDataset(train_paths, transform=transform)
test_dataset = EdgeDataset(test_paths, transform=transform)

train_loader = DataLoader(train_dataset, batch_size=batch_size, shuffle=True)
test_loader = DataLoader(test_dataset, batch_size=batch_size, shuffle=False)

###### Step 27: Fine-Tune the HED Model ######
print("Starting fine-tuning...")
for epoch in range(epochs):
    running_loss = 0.0
    for inputs, targets in train_loader:
        optimizer.zero_grad()
        outputs = hed_model(inputs)
        targets = targets.mean(dim=1, keepdim=True)  # Adjust target to match the model output dimensions
        loss = criterion(outputs[-1], targets)
        loss.backward()
        optimizer.step()
        running_loss += loss.item()
    print(f"Epoch {epoch+1}/{epochs}, Loss: {running_loss/len(train_loader):.4f}")

print("Fine-tuning completed!")

###### Step 28: Compute SSIM Scores for Test Images ######
def compute_ssim_for_images(image_paths, model):
    ssim_scores = []
    for image_path in image_paths:
        image = Image.open(image_path).convert('RGB')
        image_tensor = transform(image).unsqueeze(0)
        with torch.no_grad():
            edges = model(image_tensor)[-1].cpu().numpy().squeeze()
        edges_normalized = (edges * 255).astype(np.uint8)
        image_gray = cv2.cvtColor(np.array(image.resize((224, 224))), cv2.COLOR_RGB2GRAY)
        ssim_score = ssim(image_gray, edges_normalized, data_range=edges_normalized.max() - edges_normalized.min())
        ssim_scores.append(ssim_score)
    return ssim_scores

ssim_scores = compute_ssim_for_images(test_paths, hed_model)
mean_ssim = np.mean(ssim_scores)
print(f"Mean SSIM across the dataset: {mean_ssim:.4f}")

###### Step 29: Evaluate Model Performance ######
def evaluate_model(test_loader, model):
    print("Evaluating fine-tuned model...")
    total_loss = 0.0
    with torch.no_grad():
        for inputs, targets in test_loader:
            outputs = model(inputs)
            loss = criterion(outputs[-1], targets)
            total_loss += loss.item()
    print(f"Test Loss: {total_loss / len(test_loader):.4f}")

evaluate_model(test_loader, hed_model)

###### Step 30: Save Fine-Tuned Model ######
model_save_path = os.path.join(output_dir, 'hed_finetuned.pth')
torch.save({'state_dict': hed_model.state_dict()}, model_save_path)
print(f"Fine-tuned model saved at {model_save_path}")
## 0.4776 mean SSIM across dataset. 

###### Step 31: Visualize Edge Detection with SSIM ######
def visualize_hed_edges_with_ssim(image_paths, hed_model):
    for i, image_path in enumerate(image_paths[:2]):
        image = Image.open(image_path).convert('RGB')
        image_tensor = transform(image).unsqueeze(0)
        with torch.no_grad():
            edges = hed_model(image_tensor)[-1].cpu().numpy().squeeze()
        edges_normalized = (edges * 255).astype(np.uint8)
        image_gray = cv2.cvtColor(np.array(image.resize((224, 224))), cv2.COLOR_RGB2GRAY)
        ssim_score = ssim(image_gray, edges_normalized, data_range=edges_normalized.max() - edges_normalized.min())
        plt.figure(figsize=(10, 5))
        plt.subplot(1, 2, 1)
        plt.imshow(image)
        plt.title("Original Image")
        plt.axis('off')
        plt.subplot(1, 2, 2)
        plt.imshow(edges_normalized, cmap='gray')
        plt.title(f"HED Edge Detection\nSSIM: {ssim_score:.4f}")
        plt.axis('off')
        plt.show()
        print(f"SSIM for image {os.path.basename(image_path)}: {ssim_score:.4f}")

visualize_hed_edges_with_ssim(test_paths, hed_model)

print("HED fine-tuning, evaluation, and visualization completed successfully!")

"""Interpretation of SSIM Scores:
The Structural Similarity Index (SSIM) ranges between 0 and 1, where:

SSIM = 1.00 → Perfect structural similarity (ideal match).
0.85 ≤ SSIM < 1.00 → High similarity, indicating good edge preservation.
0.65 ≤ SSIM < 0.85 → Moderate similarity, potential misalignment or noise in edges.
0.50 ≤ SSIM < 0.65 → Low similarity, indicating poor edge detection accuracy.
SSIM < 0.50 → Significant differences, suggesting that edges are poorly captured or noisy."""

#### Function to sort Images defined in line 99 for step 31 ####

# Define output paths for similarity matrices
english_hed_similarity_matrix_path = os.path.join(output_dir, 'english_edge_hed_similarity_matrix.npy')
spanish_hed_similarity_matrix_path = os.path.join(output_dir, 'spanish_edge_hed_similarity_matrix.npy')

#### Function for Edge Extraction Using HED for step 31 ####
def extract_hed_edges(image_path, model):
    """
    Extract edges from the given image using the fine-tuned HED model.

    Args:
        image_path (str): Path to the image file.
        model (torch.nn.Module): Fine-tuned HED model.

    Returns:
        np.ndarray: Flattened array of edge-detected features.
    """
    image = Image.open(image_path).convert('RGB')
    image_tensor = transform(image).unsqueeze(0)
    with torch.no_grad():
        edges_list = model(image_tensor)  # The output is a list of tensors
        edges = edges_list[-1]  # Use the final edge detection tensor
    edges = edges.cpu().numpy().squeeze()  # Ensure it's converted from tensor to array
    return edges.flatten()

### Process English Images ###
###### step 32: sort, extract hed features, to array, and compute cosine similarity ######
print("Processing sorted English images for HED-based features...")
english_hed_features = [extract_hed_edges(image_path, hed_model) for image_path in sorted_english_image_paths]
english_hed_features_array = np.array(english_hed_features)
english_hed_similarity_matrix = cosine_similarity(english_hed_features_array)
###### step 33: Visualize Similarity Matrix ######
plt.figure(figsize=(10, 8))
sns.heatmap(english_hed_similarity_matrix, cmap='viridis')
plt.title('English Images - HED-Based Similarity Matrix')
plt.xlabel('Image Index')
plt.ylabel('Image Index')
plt.show()

## Process Spanish Images ##
###### step 32: sort, extract hed features, to array, and compute cosine similarity ######
print("Processing sorted Spanish images for HED-based features...")
sorted_spanish_image_paths = sort_images_by_category(spanish_image_dir)
spanish_hed_features = [extract_hed_edges(image_path, hed_model) for image_path in sorted_spanish_image_paths]
spanish_hed_features_array = np.array(spanish_hed_features)
spanish_hed_similarity_matrix = cosine_similarity(spanish_hed_features_array)
np.save(spanish_hed_similarity_matrix_path, spanish_hed_similarity_matrix)

###### step 33: Visualize Spanish Similarity Matrix ######
plt.figure(figsize=(10, 8))
sns.heatmap(spanish_hed_similarity_matrix, cmap='viridis')
plt.title('Spanish Images - HED-Based Similarity Matrix')
plt.xlabel('Image Index')
plt.ylabel('Image Index')
plt.show()

print("HED-based similarity matrices generated and visualized successfully.")


# In[]:
# ----------------------------------------------------------------------------#
## -- Section V: 3rd Feature: Category (ResNet50 Fine-Tuned Classification and Similarity) -- ##
# ----------------------------------------------------------------------------#

### ----------- Part 5.1: Accuracy Evaluation:  -------------- ###

###### Step 36: Load Required Dependencies and Define Directories ######
import os
import random
from PIL import Image
from torch.utils.data import Dataset, DataLoader
from torchvision import transforms, models
from torch import nn, optim
import torch
from sklearn.model_selection import train_test_split
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

##### Set Random Seed for Reproducibility #####
def set_seed(seed):
    """
    Set random seed for reproducibility across all libraries.

    Args:
        seed (int): Seed value to use.
    """
    random.seed(seed)
    np.random.seed(seed)
    torch.manual_seed(seed)
    torch.cuda.manual_seed_all(seed)
    torch.backends.cudnn.deterministic = True
    torch.backends.cudnn.benchmark = False

## Define the reproducible seed ##
seed_value = 42
set_seed(seed_value)

## Define relevant directories
english_image_dir = r'C:\Users\j.gambasegovia\Documents\Papeles Alejo UF\1 - RSA - R.Project\RSA_CNN\RSA_english_images'
spanish_image_dir = r'C:\Users\j.gambasegovia\Documents\Papeles Alejo UF\1 - RSA - R.Project\RSA_CNN\RSA_spanish_images'
output_dir = r'C:\Users\j.gambasegovia\Documents\Papeles Alejo UF\1 - RSA - R.Project\RSA_CNN'

###### Step 37: Define Dataset Augmentation Strategies ######
data_transforms = {
    'train': transforms.Compose([
        transforms.RandomResizedCrop(224),
        transforms.RandomHorizontalFlip(),
        transforms.ColorJitter(brightness=0.2, contrast=0.2, saturation=0.2, hue=0.1),
        transforms.ToTensor(),
        transforms.Normalize(mean=[0.485, 0.456, 0.406], std=[0.229, 0.224, 0.225]),
    ]),
    'test': transforms.Compose([
        transforms.Resize((224, 224)),
        transforms.ToTensor(),
        transforms.Normalize(mean=[0.485, 0.456, 0.406], std=[0.229, 0.224, 0.225]),
    ]),
}

## Utility Function to Sort Images ##
def sort_images_by_category(image_dir):
    """
    Sort images by their inferred category based on filenames.

    Args:
        image_dir (str): Directory containing the images.

    Returns:
        List[str]: Sorted list of image paths.
    """
    image_paths = [
        os.path.join(image_dir, f)
        for f in os.listdir(image_dir)
        if f.endswith(('.png', '.jpg', '.jpeg'))
    ]
    # Sort images based on the numeric value extracted from filenames
    sorted_image_paths = sorted(
        image_paths,
        key=lambda x: int(os.path.basename(x).split('_')[0])
    )
    return sorted_image_paths

###### Step 38: Define Custom Dataset and Sorting Functions ######
class CustomDataset(Dataset):
    def __init__(self, image_paths, transform=None):
        self.image_paths = image_paths
        self.transform = transform

    def __len__(self):
        return len(self.image_paths)

    def __getitem__(self, idx):
        image_path = self.image_paths[idx]
        label = int(os.path.basename(image_path).split('_')[0]) - 1  # Extract label from filename
        image = Image.open(image_path).convert('RGB')

        if self.transform:
            image = self.transform(image)

        return image, label


## Utility Function: Stratified Split on Sorted Images ##
def stratified_split_sorted(image_paths, test_size=0.2):
    """
    Perform a stratified split on sorted image paths, maintaining class distribution.

    Args:
        image_paths (list[str]): List of sorted image paths.
        test_size (float): Proportion of the dataset to include in the test split.

    Returns:
        train_paths (list[str]): Training image paths.
        test_paths (list[str]): Testing image paths.
    """
    labels = [int(os.path.basename(path).split('_')[0]) - 1 for path in image_paths]
    train_paths, test_paths = train_test_split(
        image_paths, test_size=test_size, stratify=labels, random_state=42
    )
    return train_paths, test_paths

###### Step 39: sort images and perform Stratified Split on Sorted Images ######
## sort ##
sorted_english_image_paths = sort_images_by_category(english_image_dir)
sorted_spanish_image_paths = sort_images_by_category(spanish_image_dir)
## stratified split ##
english_train_paths, english_test_paths = stratified_split_sorted(sorted_english_image_paths)
spanish_train_paths, spanish_test_paths = stratified_split_sorted(sorted_spanish_image_paths)

###### Step 40: Combine English and Spanish Paths and Create Data Loaders ######
train_paths = english_train_paths + spanish_train_paths
test_paths = english_test_paths + spanish_test_paths

## Function to create loaders ##
def create_data_loaders(train_paths, test_paths, transforms, batch_size=16):
    train_dataset = CustomDataset(train_paths, transform=transforms['train'])
    test_dataset = CustomDataset(test_paths, transform=transforms['test'])

    train_loader = DataLoader(train_dataset, batch_size=batch_size, shuffle=True)
    test_loader = DataLoader(test_dataset, batch_size=batch_size, shuffle=False)

    return train_loader, test_loader

train_loader, test_loader = create_data_loaders(train_paths, test_paths, data_transforms)

## Create Data Loaders ##
train_loader, test_loader = create_data_loaders(train_paths, test_paths, data_transforms)

###### Step 41: Load and Configure Pretrained ResNet50 Model for 40 Categories ######
print("Loading pre-trained ResNet-50 model...")
model = models.resnet50(weights=models.ResNet50_Weights.DEFAULT)
num_ftrs = model.fc.in_features
model.fc = nn.Linear(num_ftrs, 40)  # Adjust for 40 categories

criterion = nn.CrossEntropyLoss()
optimizer = optim.SGD(model.parameters(), lr=0.001, momentum=0.9)
scheduler = optim.lr_scheduler.StepLR(optimizer, step_size=7, gamma=0.1)

device = torch.device("cuda:0" if torch.cuda.is_available() else "cpu")
model = model.to(device)

###### Step 42: Fine-Tune the ResNet50 Model on Category Labels ######
num_epochs = 15
print("Starting fine-tuning...")
for epoch in range(num_epochs):
    print(f"Epoch {epoch + 1}/{num_epochs}")
    model.train()

    running_loss = 0.0
    for inputs, labels in train_loader:
        inputs, labels = inputs.to(device), labels.to(device)

        optimizer.zero_grad()
        outputs = model(inputs)
        loss = criterion(outputs, labels)
        loss.backward()
        optimizer.step()

        running_loss += loss.item() * inputs.size(0)

    epoch_loss = running_loss / len(train_loader.dataset)
    print(f"Train Loss: {epoch_loss:.4f}")

    scheduler.step()

###### Step 43: Evaluate ResNet50 Model Accuracy on Test Set ######
print("Evaluating on test set...")
model.eval()
correct = 0
total = 0

with torch.no_grad():
    for inputs, labels in test_loader:
        inputs, labels = inputs.to(device), labels.to(device)
        outputs = model(inputs)
        _, preds = torch.max(outputs, 1)
        correct += torch.sum(preds == labels).item()
        total += labels.size(0)

accuracy = correct / total * 100
print(f"Test Accuracy: {accuracy:.2f}%")
## results: Test accuracy: 96.43%


### --- Part 5.2: Similarity Matrix Generation from fined-tune model --- ###
print("Generating similarity matrices using sorted images...")


###### Step 44: Define Feature Extraction Pipeline for Fine-Tuned ResNet50 ######
preprocess = transforms.Compose([
    transforms.Resize((224, 224)),
    transforms.ToTensor(),
    transforms.Normalize(mean=[0.485, 0.456, 0.406], std=[0.229, 0.224, 0.225]),
])

## Feature Extraction Function ##
def extract_features(image_path, model):
    image = Image.open(image_path).convert('RGB')
    image_tensor = preprocess(image).unsqueeze(0).to(device)

    with torch.no_grad():
        features = model(image_tensor)

    return features.cpu().numpy().flatten()

## Generate Similarity Matrices ##
def process_images_and_generate_matrix(sorted_image_paths, model, title):
    features = [extract_features(image_path, model) for image_path in sorted_image_paths]
    features_array = np.array(features)

    similarity_matrix = cosine_similarity(features_array)

    plt.figure(figsize=(10, 8))
    sns.heatmap(similarity_matrix, cmap='viridis')
    plt.title(title)
    plt.xlabel('Image Index')
    plt.ylabel('Image Index')
    plt.show()
    
    return similarity_matrix

###### Step 45: Generate and Visualize Similarity Matrix - English Dataset ######
english_category_finetuned_ResNet50_similarity_matrix = process_images_and_generate_matrix(
    sorted_english_image_paths, 
    model,
    'English Images - Fine-Tuned Model Similarity Matrix'
)

###### Step 46: Generate and Visualize Similarity Matrix - Spanish Dataset ######
spanish_category_finetuned_ResNet50_similarity_matrix = process_images_and_generate_matrix(
    sorted_spanish_image_paths, 
    model, 
    'Spanish Images - Fine-Tuned Model Similarity Matrix'
)


print("Category similarity matrices for English and Spanish datasets generated and visualized successfully!")

# In[]:

# ----------------------------------------------------------------------------#
## -- Section VI: 4th Feature: Animacy (ResNet50 Fine-Tuned Classification and Similarity) -- ##
# ----------------------------------------------------------------------------#

### ----------- Part 6.1: Accuracy Evaluation:  -------------- ### 

###### Step 47: Load Required Dependencies ######
import os
from PIL import Image
from torch.utils.data import Dataset, DataLoader
from torchvision import transforms, models
from torch import nn, optim
from sklearn.model_selection import train_test_split
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.metrics.pairwise import cosine_similarity

###### Step 48: Define Helper Functions and Sorting Logic ######
def sort_image_paths(image_paths):
    def extract_category(image_path):
        return int(os.path.basename(image_path).split('_')[0])
    
    return sorted(image_paths, key=extract_category)

## Define Directories ##
# Defined directories above: lines 818-820.

## Dataset Augmentation ##
## defined (`data_transforms`): line 823

###### Step 49: Define Custom Dataset for Animacy Classification ######
class CustomDataset(Dataset):
    def __init__(self, image_paths, transform=None):
        # Sort image paths by category before assigning them
        self.image_paths = sort_image_paths(image_paths)
        self.transform = transform

        # Log sorted image paths and their labels
        print("Dataset Images and Labels (Sorted by Category):")
        for path in self.image_paths:
            label = int(os.path.basename(path).split('_')[0]) - 1
            label = 0 if label < 30 else 1  # Animate: 0, Inanimate: 1
            print(f"Image: {path}, Label: {label}")

    def __len__(self):
        return len(self.image_paths)

    def __getitem__(self, idx):
        image_path = self.image_paths[idx]
        label = int(os.path.basename(image_path).split('_')[0]) - 1
        label = 0 if label < 30 else 1  # Animate: 0, Inanimate: 1
        image = Image.open(image_path).convert('RGB')

        if self.transform:
            image = self.transform(image)

        return image, label

###### Step 50: Perform Stratified Split for Animacy Labels ######
def stratified_split_animacy(image_dir, test_size=0.2):
    image_paths = [os.path.join(image_dir, f) for f in os.listdir(image_dir) if f.endswith(('.png', '.jpg', '.jpeg'))]
    image_paths = sort_image_paths(image_paths)

    labels = [0 if int(os.path.basename(path).split('_')[0]) - 1 < 30 else 1 for path in image_paths]

    train_paths, test_paths = train_test_split(image_paths, test_size=test_size, stratify=labels, random_state=42)
    return train_paths, test_paths

english_train_paths, english_test_paths = stratified_split_animacy(english_image_dir)
spanish_train_paths, spanish_test_paths = stratified_split_animacy(spanish_image_dir)
train_paths = english_train_paths + spanish_train_paths
test_paths = english_test_paths + spanish_test_paths

###### Step 51: Create Data Loaders ######
def create_data_loaders(train_paths, test_paths, transforms, batch_size=16):
    train_dataset = CustomDataset(train_paths, transform=transforms['train'])
    test_dataset = CustomDataset(test_paths, transform=transforms['test'])

    train_loader = DataLoader(train_dataset, batch_size=batch_size, shuffle=True)
    test_loader = DataLoader(test_dataset, batch_size=batch_size, shuffle=False)

    return train_loader, test_loader

train_loader, test_loader = create_data_loaders(train_paths, test_paths, data_transforms)

###### Step 52: Load and Configure Pretrained ResNet50 Model for Animacy ######
print("Loading pre-trained ResNet-50 model...")
model_animacy = models.resnet50(weights=models.ResNet50_Weights.DEFAULT)
num_ftrs = model_animacy.fc.in_features
## Adding a fully connected layer followed by the binary output layer ##
model_animacy.fc = nn.Sequential(
    nn.Linear(num_ftrs, 128),  # First fully connected layer
    nn.ReLU(),  # Activation function
    nn.Dropout(0.5),  # Dropout for regularization
    nn.Linear(128, 2)  # Binary classification layer
)

criterion = nn.CrossEntropyLoss()
optimizer = optim.SGD(model_animacy.parameters(), lr=0.001, momentum=0.9)
scheduler = optim.lr_scheduler.StepLR(optimizer, step_size=7, gamma=0.1)
device = torch.device("cuda:0" if torch.cuda.is_available() else "cpu")
model_animacy = model_animacy.to(device)
print(model_animacy)

###### Step 53: Fine-Tune ResNet50 on Animacy Labels ######
num_epochs = 10
print("Starting fine-tuning...")
for epoch in range(num_epochs):
    print(f"Epoch {epoch + 1}/{num_epochs}")
    model_animacy.train()

    running_loss = 0.0
    for inputs, labels in train_loader:
        inputs, labels = inputs.to(device), labels.to(device)

        optimizer.zero_grad()
        outputs = model_animacy(inputs)
        loss = criterion(outputs, labels)
        loss.backward()
        optimizer.step()

        running_loss += loss.item() * inputs.size(0)

    epoch_loss = running_loss / len(train_loader.dataset)
    print(f"Train Loss: {epoch_loss:.4f}")

    scheduler.step()

###### Step 54: Evaluate Animacy Classifier on Test Set ######
print("Evaluating on test set...")
model_animacy.eval()
correct = 0
total = 0

with torch.no_grad():
    for inputs, labels in test_loader:
        inputs, labels = inputs.to(device), labels.to(device)
        outputs = model_animacy(inputs)
        _, preds = torch.max(outputs, 1)
        correct += torch.sum(preds == labels).item()
        total += labels.size(0)

accuracy_animacy = correct / total * 100
print(f"Test Accuracy: {accuracy_animacy:.2f}%")
## Test accuracy: 98.21%  ##


### ----------- Part 6.2: Similarity Matrix Generation:  -------------- ### 

###### Step 55: Define Feature Extractor Module ######
class FeatureExtractor(nn.Module):
    def __init__(self, model):
        super(FeatureExtractor, self).__init__()
        self.features = nn.Sequential(*list(model.children())[:-1])  # All layers except the final FC
        self.custom_fc = model.fc[0]  # First FC layer added above

    def forward(self, x):
        x = self.features(x)
        x = torch.flatten(x, 1)
        x = self.custom_fc(x)  # Extract features after the custom FC layer
        return x

feature_extractor = FeatureExtractor(model_animacy).to(device)

###### Step 56: Define Feature Extraction Function ######
def extract_features_with_custom_fc(image_path, model):
    image = Image.open(image_path).convert('RGB')
    image_tensor = preprocess(image).unsqueeze(0).to(device)

    with torch.no_grad():
        features = model(image_tensor)
    return features.cpu().numpy().flatten()

###### Step 57: Generate and Visualize Similarity Matrix ######
def process_images_and_generate_matrix(image_dir, model, title):
    print(f"Processing images in {image_dir}...")
    image_paths = [os.path.join(image_dir, f) for f in os.listdir(image_dir) if f.endswith(('.png', '.jpg', '.jpeg'))]
    image_paths = sort_image_paths(image_paths)
    features = []
    for image_path in image_paths:
        #print(f"Processing: {image_path}")
        feature_vector = extract_features_with_custom_fc(image_path, model)
        features.append(feature_vector)
    features_array = np.array(features)
    similarity_matrix = cosine_similarity(features_array)
    plt.figure(figsize=(10, 8))
    sns.heatmap(similarity_matrix, cmap='viridis')
    plt.title(title)
    plt.xlabel('Image Index')
    plt.ylabel('Image Index')
    plt.show()

    return similarity_matrix

#### Step 55: Process Images and Generate Similarity Matrix ####
english_animacy_finetuned_ResNet50_similarity_matrix = process_images_and_generate_matrix(
    english_image_dir, 
    feature_extractor, 
    'English Images - Animacy Model Similarity Matrix'
)

#### Step 56: Process Images and Generate Similarity Matrix ####
spanish_animacy_finetuned_ResNet50_similarity_matrix = process_images_and_generate_matrix(
    spanish_image_dir, 
    feature_extractor, 
    'Spanish Images - Animacy Model Similarity Matrix'
)

print("Similarity matrices generated and sorted successfully!")

# In[ ]:

# ----------------------------------------------------------------------------#
#### ------ Section VII: Transformations of Objective RDM -------- ####
# ----------------------------------------------------------------------------#
'''
## Matrices: ##
similarity_matrix_english
similarity_matrix_spanish
english_color_similarity_matrix
spanish_color_similarity_matrix 
english_edge_canny_similarity_matrix 
spanish_edge_canny_similarity_matrix
english_hed_similarity_matrix
spanish_hed_similarity_matrix
english_category_finetuned_ResNet50_similarity_matrix
spanish_category_finetuned_ResNet50_similarity_matrix
english_animacy_finetuned_ResNet50_similarity_matrix
spanish_animacy_finetuned_ResNet50_similarity_matrix
''' 

###### Step 60: Define Function to Convert Similarity to Category Dissimilarity ######
def process_similarity_matrices_dual(matrix_dict, images_per_category=7):
    """
    Convert similarity matrices into both flipped and unflipped category-level dissimilarity matrices.
    
    Returns:
        tuple: (unflipped_category_matrices, flipped_category_matrices)
    """
    unflipped_matrices = {}
    flipped_matrices = {}

    for matrix_name, similarity_matrix in matrix_dict.items():
        print(f"Processing {matrix_name}...")

        # Convert to dissimilarity
        dissimilarity_matrix = 1 - similarity_matrix

        # Compute category-level dissimilarity
        num_categories = dissimilarity_matrix.shape[0] // images_per_category
        category_matrix = np.zeros((num_categories, num_categories))

        for i in range(num_categories):
            for j in range(num_categories):
                start_i, end_i = i * images_per_category, (i + 1) * images_per_category
                start_j, end_j = j * images_per_category, (j + 1) * images_per_category
                category_matrix[i, j] = dissimilarity_matrix[start_i:end_i, start_j:end_j].mean()

        # Save unflipped matrix
        unflipped_matrices[matrix_name] = category_matrix

        # Flip matrix vertically for final use and visualization
        flipped_matrix = category_matrix[::-1, :]
        flipped_matrices[matrix_name] = flipped_matrix

        # Plot flipped matrix
        plt.figure(figsize=(12, 10))
        sns.heatmap(
            flipped_matrix,
            cmap='viridis',
            xticklabels=category_names,
            yticklabels=category_names[::-1]
        )
        plt.title(f"{matrix_name} - Category Dissimilarity Matrix (Flipped)")
        plt.xticks(rotation=90)
        plt.yticks(rotation=0)
        plt.xlabel('Category')
        plt.ylabel('Category (Flipped)')
        plt.tight_layout()
        plt.show()
        
        # Plot unflipped matrix
        plt.figure(figsize=(12, 10))
        sns.heatmap(
            category_matrix,
            cmap='viridis',
            xticklabels=category_names,
            yticklabels=category_names
        )
        plt.title(f"{matrix_name} - Category Dissimilarity Matrix (Flipped)")
        plt.xticks(rotation=90)
        plt.yticks(rotation=0)
        plt.xlabel('Category')
        plt.ylabel('Category (Flipped)')
        plt.tight_layout()
        plt.show()
    return unflipped_matrices, flipped_matrices

###### Step 61: Define Dictionary with All Similarity Matrices ######
similarity_matrices = {
    "similarity_matrix_english": similarity_matrix_english,
    "similarity_matrix_spanish": similarity_matrix_spanish,
    "english_color_similarity_matrix": english_color_similarity_matrix,
    "spanish_color_similarity_matrix": spanish_color_similarity_matrix,
    "english_color_proportions_similarity_matrix": english_color_proportions_similarity_matrix,
    "spanish_color_proportions_similarity_matrix": spanish_color_proportions_similarity_matrix,
    "english_edge_canny_similarity_matrix": english_edge_canny_similarity_matrix,
    "spanish_edge_canny_similarity_matrix": spanish_edge_canny_similarity_matrix,
    "english_hed_similarity_matrix": english_hed_similarity_matrix,
    "spanish_hed_similarity_matrix": spanish_hed_similarity_matrix,
    "english_category_finetuned_ResNet50_similarity_matrix": english_category_finetuned_ResNet50_similarity_matrix,
    "spanish_category_finetuned_ResNet50_similarity_matrix": spanish_category_finetuned_ResNet50_similarity_matrix,
    "english_animacy_finetuned_ResNet50_similarity_matrix": english_animacy_finetuned_ResNet50_similarity_matrix,
    "spanish_animacy_finetuned_ResNet50_similarity_matrix": spanish_animacy_finetuned_ResNet50_similarity_matrix    
}

###### Step 62: Run Category Matrix Processing (Both Flipped and Unflipped) ######
unflipped_matrices, flipped_matrices = process_similarity_matrices_dual(similarity_matrices, images_per_category=7)

###### Step 63: Export Flipped Matrices ######
final_output_dir = r'C:\Users\j.gambasegovia\Documents\Papeles Alejo UF\1 - RSA - R.Project\RSA_CNN\final_objective_matrices'
for matrix_name, category_matrix in flipped_matrices.items():
    output_path = os.path.join(final_output_dir, f"{matrix_name}_category_matrix.csv")
    df = pd.DataFrame(category_matrix)
    df.to_csv(output_path, index=False, header=False)
    print(f"Saved FLIPPED matrix: {output_path}")





