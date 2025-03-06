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

### Prerequisites

This code was run with:
- R (version 4.4.1 or higher recommended)
- RStudio
- The following main R packages:
  - brms
  - dplyr
  - tidyr
  - ggplot2
  - here (for path management)

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

## Anonymization of Data
Note that due to the sensitive nature of the data (including GPS coordinates, birth dates, and other identifiable information), we cannot publicly share the raw data but instead provide a simulated dataset that preserves the key statistical properties. 
