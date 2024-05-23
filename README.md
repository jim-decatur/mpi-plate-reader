# meghna_pr script

This repository contains the `meghna_pr` script, designed for processing data from the plate readers at MPI Tübingen. The script can handle output files from the aerobic and anaerobic plate readers, as well as tabular data formats, and performs various data cleaning, analysis, and visualization tasks.

## Script Overview

The `meghna_pr` function processes plate reader data and generates quality control plots and summary statistics. The function takes the following parameters:

- `input_data`: Type of input data (`"aerobic"`, `"anaerobic"`, or `"table"`).
- `plate_data`: Path to the plate reader data file.
- `metadata`: Path to the metadata file.
- `time_interval`: Time interval between measurements.
- `filtered_interval`: Time interval for plotting.

## Requirements

To run the script, you need R and several R packages. If the packages are not installed, the script will attempt to install them.

### Required Packages

- `tidyverse`
- `ggplot2`
- `purrr`
- `readxl`
- `tidyselect`
- `dplyr`
- `openxlsx`
- `gtools`
- `stringr`
- `reshape2`
- `RColorBrewer`

### Input File Formats

1. **Aerobic Data**: An Excel file from the S1 plate reader. The first row should contain column names, and the columns should contain date, text, and numeric data types.
2. **Anaerobic Data**: An Excel file from the anaerobic plate reader.
3. **Tabular Data**: A tab-separated (`.tsv`) or comma-separated (`.csv`) file with column names in the first row.

### Metadata File Format

The metadata file should be a tab-separated (`.tsv`) file containing the following columns:

- `well`: Well identifier
- `id`: Sample ID
- `condition`: Experimental condition
- `replicate`: Replicate identifier

The metadata can contain more than one `condition` column, as long as only one of these includes the "blank" condition. Subsequent `condition` columns should be named `condition2`, `condition3` etc.

## Running the Script

To execute the script in R, use the following commands:

```R
source("path/to/meghna_pr.R")

# Example usage
meghna_pr(
  input_data = "aerobic",
  plate_data = "path/to/plate_data.xlsx",
  metadata = "path/to/metadata.tsv",
  time_interval = 10,
  filtered_interval = 60
)
```

Replace `"path/to/meghna_pr.R"`, `"path/to/plate_data.xlsx"`, and `"path/to/metadata.tsv"` with the actual paths to your script and data files.

## Blanks

The script will remove any "blank" conditions that have an OD value greater than 0.25. If there are less than three "blank" conditions remaining, the script will stop.

## Outputs

The script generates several outputs and saves them in the `QC` directory:

1. **Raw Data**:
   - `01_raw_data.txt`: The raw data after initial cleaning.
   
2. **Data with Time Intervals**:
   - `02_data_time.txt`: Data with added time intervals.
   
3. **Processed Data**:
   - `04_data_blanks_removed.txt`: Data with blank wells removed.
   - `05_data_blanks_subtracted.txt`: Data with blank averages subtracted.
   - `06_data_melt.txt`: Melted data in long format.
   - `07_data_filtered.txt`: Data with metadata merged and filtered.
   - `08_data_meta_merged.txt`: Data with blank conditions removed.
   - `09_data_summary_stats.txt`: Summary statistics of the data.

4. **Plots**:
   - `absorbance_blanks_individual_replicates.png`: Individual replicate plots for blanks.
   - `absorbance_blanks_individual_replicates.eps`: EPS format of the above plot.
   - `absorbance_blanks_individual_replicates_outliers_removed.png`: Plots with outliers removed.
   - `absorbance_blanks_individual_replicates_outliers_removed.eps`: EPS format of the above plot.
   - Additional plots generated during the analysis, named according to the conditions and interactions analyzed.


