# Consulting Project ifo-Business-Survey

This project provides a systematic framework for relational analysis using R. It covers the entire research pipeline, from raw data processing to advanced econometric modeling, including factor analysis, network structures, and regime-switching forecasting.

---

## Directory Structure

### 1. Data & Outputs
* **`/Data`**: Contains all **raw datasets** (unprocessed).
* **`/output`**: A storage hub for **intermediary results**. Large computational objects, processed matrices, and model estimates are saved here to allow seamless integration between different analysis chapters without redundant re-computation.

### 2. Core Analysis Modules
The scripts are numbered to reflect the logical flow of the research:

* **Pre-processing & Exploratory Analysis**: 
    * `00_ifo_data_preprocessing.Rmd`: Data cleaning and transformation.
    * `descriptive_analysis.Rmd` & `missing_analysis.Rmd`: Data quality and statistical summaries.
* **Factor Modeling (DFM)**:
    * `01_Static_DFM_B_nd.Rmd` & `02_Dynamic_DFM.Rmd`: Extraction of static and dynamic factors.
* **Advanced Econometrics**:
    * `03_Time-Varying_Factor_Loadings.Rmd`: Analyzing evolving relationships over time.
    * `0401_Incremental_Predictive_Power.Rmd`: Evaluating **leading indicators**.
    * `05_MS_DFM_OOS.Rmd`: **Regime-switching analysis** (Markov-Switching DFM).
    * `06_FAVAR_Forecasting.Rmd`: **Factor-Augmented VAR** for macroeconomic forecasting.
* **Structural & Network Analysis**:
    * `Network.Rmd`: Investigating variable connectivity.
    * `DTW.Rmd`: Dynamic Time Warping for time-series similarity.

### 3. Stability & Robustness Checks
All files containing the keyword **`supplement`** (e.g., `01_supplement_static_DFM.Rmd`, `0301_supplement_TVFL.Rmd`) are dedicated to **stability checks**. These scripts validate the core results against different specifications to ensure robustness.

---

## Usage & Workflow

1.  **Data Preparation**: Always run the preprocessing scripts (`00_...`) first to populate the `/output` folder.
2.  **Modular Loading**: Since many models (like TVFL or MS-DFM) are computationally intensive, subsequent scripts are designed to load pre-saved objects from the `/output` directory.
3.  **Environment**: Ensure you have RStudio and a TeX distribution (e.g., TinyTeX) installed for knitting the `.Rmd` files.

---
