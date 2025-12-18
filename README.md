This repository contains the R implementation for modeling and predicting Soil Organic Carbon (SOC) content using Machine Learning techniques. The project analyzes the influence of soil physical-chemical properties, historical climate data, and multi-year cropland frequencies on SOC levels.

## Project Overview

The core objective of this research is to predict SOC ($g \cdot kg^{-1}$) using a Random Forest (RF) regressor. The study specifically investigates the sensitivity of the model to different groups of environmental predictors and identifies the most influential features.

Key analytical components include:
* **Machine Learning Model:** Random Forest (implemented via `randomForest` and `ranger`).
* **Feature Selection:** Recursive Feature Elimination (RFE) to identify essential predictors.
* **Sensitivity Analysis:** Comparison of four distinct modeling scenarios.
* **Model Interpretation:** Partial Dependence Plots (PDP) and Variable Importance analysis.

## Methodology

### Feature Groups
The features used for prediction are classified into three categories:
1.  **Soil Properties:** Physical-chemical properties (e.g., pH, texture, nutrients).
2.  **Climate:** Gridded historical climate data (1961–2000) from WorldClim (1 km resolution), including temperature, precipitation, and solar radiation.
3.  **Rotation (Cropland Frequencies):** Multi-year cropland frequencies derived from satellite observation.

### Modeling Scenarios (Sensitivity Analysis)
To assess the influence of specific feature groups, four modeling cases were evaluated:
* **Case 1 (Benchmark):** All 72 features included.
* **Case 2:** Excluding climatology features (24 features remaining).
* **Case 3:** Excluding crop rotation history (64 features remaining).
* **Case 4:** Excluding soil properties (57 features remaining).

### Evaluation Metrics
Model performance is assessed using 5-fold cross-validation. The primary metrics are the Coefficient of Determination ($R^2$) and Root Mean Square Error (RMSE):

$$
R^2 = 1 - \frac{\sum (y_i - \hat{y}_i)^2}{\sum (y_i - \bar{y})^2}
$$

$$
RMSE = \sqrt{\frac{\sum_{i=1}^{N} (\hat{y}_i - y_i)^2}{N}}
$$

## Repository Structure

```text
├── FEATURES_MATRIX_PREDICTION.gpkg    # Geospatial feature matrix for all croplands
├── MO_MODELING_DATA.gpkg              # Feature matrix for modeling at soil sampled sites
├── MO_PREDICTIONS_4000.gpkg           # SOC prediction outputs over all croplands
├── soc_features.csv                   # Processed CSV input for R script
├── mo_rf.R                            # Main analysis script
└── OUTPUT_CIP_ROT_01_09_2024/         # Output directory
    ├── MO_MODELING/                   # Saved RDS models and stats
    └── PLOTS_21_11/                   # Generated plots (Scatter, PDP, Importance)
