
# TsA in Finance: Group Project  
## Swiss National Bank Policy Rates and Inflation

###  Overview  
This project investigates the relationship between Swiss National Bank (SNB) policy interest rates and Swiss core inflation using time series analysis techniques.

We apply various statistical and econometric models to explore both short-term dynamics and long-term patterns between monetary policy actions and inflationary outcomes in Switzerland.

---

###  Project Structure

- **data/**  
  Contains all raw and processed datasets from SNB and other sources.

- **scripts/**  
  Contains modular R scripts for data loading, cleaning, modeling, and visualization.
  
- **sections/**  
  Contains Quarto (`.qmd`) files structured into chapters of the final report.

- **_book/**  
  Output folder for the final compiled HTML and PDF reports.

- **README.md**  
  You're reading it :)

---

###  How to Read the Report

####  PDF Version
- Best for printing and linear reading.
- Found at: `_book/TsA-in-Finance---Group-project.pdf`
- Use bookmarks or search to navigate quickly.

####  HTML Version
- Best for interactive viewing.
- Open `_book/index.html` in a web browser.
- Use sidebar navigation or search bar to jump to chapters.
- Plots ready to read.

---

###  Models and Methods Used

- Stationarity Tests (ADF)
- Correlation Analysis
- Linear Regression (with lag effects)
- Event-based models (policy rate shifts)
- Vector Autoregression (VAR)
- Granger Causality Tests
- ARIMA Modeling and Forecasting

---

###  Key Results 

- Policy rates Granger-cause inflation, but inflation does not Granger-cause policy changes.
- October 2022 SNB rate hike had a statistically significant effect on inflation.
- Overall linear relationships are weak; inflation is only moderately autocorrelated.
- ARIMA(5,1,0) model forecasts a slight inflation decline with moderate uncertainty.

---

###  Tools Used

- R (RStudio)
- Quarto for report generation
- GitHub for version control and collaboration
