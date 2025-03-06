# Parental LeaveEmotion Dynamics

This repository contains the analysis pipeline for the following paper: "Following, Balancing, or Self-regulating? Parental Leave-Dependent Emotion Dynamics in New Mothers and Fathers".

## File Structure

- `00_masteranalysisscript.Rmd` - The main script that orchestrates the entire analysis pipeline
- `01_preprocessing.Rmd` - Data preprocessing and cleaning steps
- `02_coupledoscillator.Rmd` - Implementation of the coupled oscillator model
- `03_controlmodels.Rmd` - Control model implementations to check our assumptions
- `04_priorpredictivechecks.Rmd` - Prior predictive checks to validate model assumptions
- `05_simulation.Rmd` - Simulation scripts to generate synthetic data for figures
- `06_rawdata.Rmd` - Visualisation of the raw data

## Getting Started

### Installation

1. Clone this repository to your local machine
2. Open the project in RStudio
3. Install required packages if not already installed:

## Running the Analysis

**Important:** For convenience, all analyses can be executed through the master script, but individual scripts can also run independently.

1. Open `00_masteranalysisscript.Rmd` in RStudio
2. Run the entire document to execute the full analysis pipeline, or
3. Run specific chunks to execute individual components of the analysis

The master script uses the `here()` function to manage file paths and calls the other scripts in the correct sequence.

## Analysis Pipeline

1. **Preprocessing**: Data cleaning, normalization, and preparation for modeling (NB: the preprocessing code is included for completeness but will not run on the simulated dataset)
2. **Coupled Oscillator Model**: Implementation of the primary model examining oscillatory dynamics
3. **Control Models**: Control model implementations to check our assumptions
4. **Prior Predictive Checks**: Validation of prior assumptions
5. **Simulation**: Simulation scripts to generate synthetic data for figures
6. **Raw Data**: Visualisation of the raw data

## Output Files

Output files are generated in the `code_output_files` directory, including:
- `01_preprocessing_output.pdf`
- `02_coupledoscillator_output.pdf`
- `03_controlmodels_output.pdf`
- Additional files generated by the analysis

## Custom Dataset

The data include the following main variables:
- FatherValence_s
- MotherValence_s
- FatherArousal_s
- MotherArousal_s
- FatherValenceVel
- MotherValenceVel
- FatherValenceAcc
- MotherValenceAcc
- isProximate
- Leave
- CoupleID
- StudyDay
- time

## Notes

- The preprocessing code is included for completeness but will not run on simulated datasets
- All scripts use relative paths through the `here()` function, so the project structure should be maintained

## Data Anonymization
Note that due to the sensitive nature of the data (including GPS coordinates, birth dates, and other identifiable information), we cannot publicly share the raw data but instead provide a simulated dataset that preserves the key statistical properties. 


## Session Information
This code was developed and tested with the following environment:

R version 4.4.1 (2024-06-14)
Platform: x86_64-pc-linux-gnu
Running under: Ubuntu 22.04.4 LTS

Matrix products: default
BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.20.so;  LAPACK version 3.10.0

locale:
 [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C               LC_TIME=en_US.UTF-8       
 [4] LC_COLLATE=en_US.UTF-8     LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
 [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                  LC_ADDRESS=C              
[10] LC_TELEPHONE=C             LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       

time zone: Europe/Berlin
tzcode source: system (glibc)

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] rstan_2.32.6        StanHeaders_2.32.10 here_1.0.1          cmdstanr_0.8.1      deSolve_1.40       
 [6] imputeTS_3.3        rmarkdown_2.27      MASS_7.3-60.2       plotly_4.10.4       metR_0.18.0        
[11] geosphere_1.5-20    mvgam_1.1.4         crqa_2.0.6          gganimate_1.0.9     gghalves_0.1.4     
[16] xtable_1.8-4        modelr_0.1.11       see_0.10.0          bayestestR_0.15.2   bayesplot_1.11.1   
[21] ggbeeswarm_0.7.2    patchwork_1.3.0     viridis_0.6.5       viridisLite_0.4.2   ggridges_0.5.6     
[26] readxl_1.4.3        brms_2.22.0         ggrain_0.0.4        cowplot_1.1.3       job_0.3.1          
[31] ggquiver_0.3.3      ggcorrplot_0.1.4.1  lubridate_1.9.3     forcats_1.0.0       stringr_1.5.1      
[36] dplyr_1.1.4         purrr_1.0.2         readr_2.1.5         tidyr_1.3.1         tibble_3.2.1       
[41] tidyverse_2.0.0     pacman_0.5.1        tidybayes_3.0.7     ggplot2_3.5.1       Rcpp_1.0.14
