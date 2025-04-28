
## TSA_Finance Project

### Group members
- Daniel Huber
- Dongyuan Gao

### Research question
Is the Swiss National Bank (SNB) really driving inflation rates or is it more the other way round? 

### Data sources
- policy rates since 2004
- inflation rates (since 1983).

### Methods
- data preparation
- data visualization
- check stationarity
- check cross-correlations
- causality tests (i. e. Granger tests, lagged regression)
- maybe: ARIMA for more detailed insights.

### Options
Add further data, e. g.:
- Swiss economy (gross domestic product GDP)
- Exchange rates (CHF-USD or CHF-EUR).

<<<<<<< HEAD
### Project_Structure
time_series_project/
│
├── data/             # Excel files, raw
│
├── scripts/          # R script files
│    ├── 01_data_import_cleaning.R
│    ├── 02_exploratory_analysis.R
│    ├── 03_stationarity_and_transformation.R
│    ├── 04_modeling_VAR_analysis.R
│    ├── 05_visualization_and_reporting.R
│
├── output/           # Plots, tables, model outputs
│
├── report/           # Final presentation or report files
│
└── README.md         # Text explaining our project

01_data_import_cleaning.R
Import your Excel files, clean them (dates, columns), merge datasets.

02_exploratory_analysis.R
Plot inflation and policy rates separately and together. Look at summary stats.

03_stationarity_and_transformation.R
Test stationarity (ADF tests), possibly differencing or transformations.

04_modeling_VAR_analysis.R
Build VAR models, analyze lag structures, impulse response functions.

05_visualization_and_reporting.R
Create clean final plots, tables for presentation/report.


=======
>>>>>>> d360e084bb52df73fc626089542d20a9dc00c87a

