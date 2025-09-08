
# BIN381-Project

<!-- badges: start -->
<!-- badges: end -->

# =========================================================
#Data Understanding Phase
# =========================================================

Each team member is responsible for performing Exploratory 
Data Analysis (EDA) on their assigned dataset. 
The aim is to gain initial insights, assess data quality, 
and prepare the data for the modeling phase.

# -------------------------------
# Deliverables
# -------------------------------
- R Markdown script for EDA
  * Each member creates an .Rmd file with code, commentary, 
    and results.
  * Includes data cleaning notes, missing value checks, 
   duplicate checks, and outlier detection.

- Summary tables of key variables
  * Descriptive statistics (mean, median, SD, counts).
  * Frequency tables for categorical variables.

- Visuals
  * Bar charts, histograms, and boxplots to visualize 
    distributions and anomalies.
  * Scatterplots/correlation heatmaps where relevant.
  
- Preliminary dashboards
  * Combine key plots to highlight important findings.

- Data dictionary (Excel/PDF)
  * A shared document (one combined file) describing all datasets.
  * Each member documents variables from their dataset, including:
     - Variable name
     - Description/definition
     - Units/scale (if applicable)
     - Expected values/ranges
     - Notes on missing values or anomalies

# -------------------------------
# Workflow & Collaboration
# -------------------------------
- Each member works on their dataset in a dedicated script:
      /scripts/EDA/EDA_datasetX.Rmd
      
- All results and scripts are stored under:
      /scripts
      /outputs
      /docs/Data_Dictionary.xlsx
      
- At the end of this phase, the team will consolidate:
  * All R scripts into /scripts/EDA/
  * All plots and summary reports into /reports/EDA/
  * One combined Data Dictionary in /docs/

# =========================================================
