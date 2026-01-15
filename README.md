# AdvVisR - Advanced Data Visualisation in R

![Project Status](https://img.shields.io/badge/Status-In%20Progress-yellow)
![Course](https://img.shields.io/badge/Course-Advanced%20Visualization%20R-blue)
![R](https://img.shields.io/badge/Language-R-276DC3?logo=r&logoColor=white)
![Python](https://img.shields.io/badge/Python-3.8%2B-blue?logo=python&logoColor=white)
![ggplot2](https://img.shields.io/badge/Library-ggplot2-F00000)
![ggiraph](https://img.shields.io/badge/Library-ggiraph-orange)
![Pandas](https://img.shields.io/badge/Library-Pandas-150458?logo=pandas&logoColor=white)

## Project Overview

This repository hosts the **AdvVisR** project, which focuses on advanced data visualization techniques using **R**. The project analyzes a complex, simulated dataset regarding fitness activities, nutritional habits, and physiological metrics.

The core of the project involves a dual-stack approach:
1.  **Data Engineering (Python):** A sophisticated simulation engine generates realistic synthetic data (`Final_data_model.csv`), enforcing logical correlations (e.g., Age vs. Metabolism, Diet Type vs. Macro Ratios).
2.  **Visualization (R):** A suite of advanced, static, and interactive charts creates a narrative around the "fuel" (nutrition), the "machine" (workout mechanics), and the "results" (body composition).

## Authors
* **Cezary Kuźmowicz**
* **Wojciech Hrycenko**

---

## Repository Contents

### 1. Visualization & Analysis
**File:** `adv_vis_r_kuzmowicz_hrycenko.r`

**Objective**
To visualize the fitness journey through three thematic lenses using advanced **ggplot2** extensions:

* **Part 1: The Fuel (Nutrition):**
    * **Treemap (`treemapify`):** Analyzes Caloric Volume vs. Sugar Content across different diets (Keto, Paleo, Vegan).
    * **Small Multiples Radar Chart:** Compares nutritional balance (Proteins, Fats, Carbs, Sodium, Water) for each diet type.
* **Part 2: The Machine (Process):**
    * **Alluvial Plot (`ggalluvial`):** Traces the user flow from Experience Level → Workout Choice → Caloric Burn Efficiency.
    * **Interactive Scatter (`ggiraph`):** Explore the relationship between Heart Rate, Calories, and Duration with hover-over exercise details.
    * **Circular Barplot:** An "Anatomical Tachometer" visualizing targeted muscle groups.
* **Part 3: The Results (Physiology):**
    * **Dumbbell Plot:** Contrasts Resting Heart Rate vs. Max Heart Rate vs. Training Intensity.
    * **Ridgeline Plot (`ggridges`):** Visualizes "Mountains of Fat" – body fat distribution by gender and experience level.

### 2. Data Simulation Engine
**File:** `Data/data_modelling_macros.py`

**Description**
A Python script designed to generate high-quality synthetic data for visualization purposes. It goes beyond simple random generation by applying "physics enforcement" logic:
* **Population Balancing:** Ensures balanced representation of difficulty levels.
* **Macro Restructuring:** Adjusts nutrient profiles (Proteins/Fats/Carbs) dynamically based on Diet Type (e.g., Keto has high fats/low carbs).
* **Correlation Engineering:** Enforces realistic biological relationships (e.g., *BMI > 25* increases *Heart Rate* effort; *Age* reduces *Max Heart Rate*).

---

## Course Requirements (Original)

**Project**
Each student is obliged to submit group project (at most 2 students per group) regarding data visualisation. Topic and database can be selected freely as long as it utilizes techniques used for visualization in R (not necessarily discussed during classes). The basic requirements and conditions:

- Submitting project topic, dataset and group: End of the November,
- Performing presentation: last two classes in January,
- Submitting scripts and short description of the analysis.

The points (70%) can be gain from:

- Attractiveness of the chosen topic, quality of the insights infered from data,
- Advancement and innovativeness of R codes, effort invested in the project,
- Accuracy and visual aspect of the presentation,
- Attractiveness of the presentation.

**Charts Documentation:**
[Google Docs Link](https://docs.google.com/document/d/13mZX1WzhJiCK_4547lAoyrs_o8204YJ9_vEOpo-bK_c/edit?usp=sharing)

---

## Technologies and Libraries

The project utilizes a mix of **R** for visualization and **Python** for data preparation:

### R (Visualization)
* **tidyverse (ggplot2, dplyr):** Core data manipulation and plotting.
* **ggalluvial:** For flow diagrams (Alluvial plots).
* **treemapify:** For creating treemaps.
* **ggiraph:** For adding interactivity to ggplot2 geometries.
* **ggridges:** For ridgeline density plots.
* **ggcorrplot:** For visualization of correlation matrices.

### Python (Data Gen)
* **Pandas & NumPy:** For data frame manipulation and numerical simulation.

## Usage Instructions

1.  Clone this repository.
2.  **Data Generation (Optional):** Run `Data/data_modelling_macros.py` if you wish to regenerate the dataset with new random seeds.
3.  **Visualization:** Open `adv_vis_r_kuzmowicz_hrycenko.r` in RStudio.
4.  Ensure the working directory is set correctly (`setwd(...)`).
5.  Run the script to generate the plots in the RStudio Viewer.
