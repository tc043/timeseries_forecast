# Chronos: Time Series Analysis & Forecasting Framework

Chronos is a professional, modular R-based framework designed for univariate time series exploration, modeling, validation, and interactive visualization. 

Originally built as a single flat script for forecasting daily births, this repository has been restructured into a production-grade framework featuring clean separation of concerns, automated unit tests, a command-line interface (CLI) pipeline, automated report compiling, and a premium interactive Shiny dashboard.

---

## 📊 Features

- **Modular Design**: Structured codebase split into specialized modules for data loading, model fitting, and ggplot2/plotly-based visualization.
- **Multiple Benchmark Models**:
  - **Seasonal Naive (SNAIVE)**: Baseline seasonal model.
  - **ETS (Error, Trend, Seasonal)**: State-space exponential smoothing.
  - **Holt-Winters**: Triple exponential smoothing (additive/multiplicative).
  - **Auto-ARIMA**: Automated Box-Jenkins model selection.
- **Automated Validation**: Holdout testing split (e.g., 70/30) with detailed evaluation metrics (RMSE, MAE, MAPE, MASE).
- **Statistical Diagnostics**: Automated Augmented Dickey-Fuller (ADF) stationarity testing and Ljung-Box residual autocorrelation testing.
- **Interactive Shiny Dashboard**: A premium, glassmorphic dark-theme UI featuring interactive Plotly graphs, parameter sliders, model selector, real-time forecasts, and data export.
- **CLI Command Runner**: Execute the forecasting pipeline, print metrics tables, and render parameterized RMarkdown HTML reports directly from the terminal.
- **Unit Testing**: Complete unit test coverage utilizing R's `testthat` package.

---

## 📂 Project Structure

```
timeseries_forecast/
├── data/
│   └── daily-total-female-births.csv  # Auto-downloaded raw time series data
├── R/
│   ├── data_loader.R                  # Data fetching, checking, and ts coercion
│   ├── models.R                       # Unified model wrappers and evaluation
│   └── plots.R                        # Sleek ggplot2 & plotly visualization helpers
├── tests/
│   ├── testthat.R                     # Main test suite runner
│   └── testthat/
│       ├── test-data_loader.R         # Tests for file reading & cleaning
│       └── test-models.R              # Tests for forecasting models & splitting
├── app.R                              # Interactive Shiny Dashboard (bslib + plotly)
├── main.R                             # CLI tool to run forecasts and compile report
├── report.Rmd                         # Parameterized RMarkdown report template
├── report.html                        # Compiled HTML analysis report (generated)
├── run_pipeline.sh                    # Automation runner (runs tests -> runs CLI)
└── README.md                          # Framework documentation
```

---

## 🚀 Getting Started

### Prerequisites
Make sure R is installed along with the following packages:
```R
install.packages(c("tidyverse", "forecast", "tseries", "shiny", "bslib", "plotly", "rmarkdown", "testthat"))
```

### Installation
Clone this repository and navigate to the root directory:
```bash
cd timeseries_forecast
```

---

## 💻 Usage

### 1. Automated Execution (Tests & Report)
You can run the entire pipeline—from testing to data ingestion and report generation—using the wrapper scripts:

**On Linux / macOS:**
```bash
./run_pipeline.sh
```

**On Windows (Command Prompt / PowerShell):**
```cmd
run_pipeline.bat
```

### 2. Command-Line Interface (`main.R`)
Use the CLI tool to customize training ratios, seasonal frequencies, and export reports:
```bash
# Display help menu
Rscript main.R --help

# Run forecasting pipeline with custom parameters
Rscript main.R --ratio=0.7 --freq=7 --horizon=30 --report=custom_report.html
```

### 3. Launching the Interactive Shiny Dashboard (`app.R`)
Launch the premium web application locally:
```bash
Rscript -e "shiny::runApp('.', port = 8080)"
```
Then navigate to `http://localhost:8080` in your web browser. The app allows you to:
- Select the default dataset or **upload your own custom CSV**.
- Dynamically tune the training split ratio, seasonal frequency, and forecast horizon.
- Inspect interactive, hoverable time-series charts, ACF/PACF graphs, and STL decomposition tables.
- View comparative accuracy tables and diagnose residual white noise.
- Export predicted values with 80% and 95% confidence intervals as a CSV file.

---

## 🧪 Testing
Unit tests are written using the `testthat` package. To manually run all tests:
```bash
Rscript tests/testthat.R
```
All testing targets are evaluated (data quality checks, train/test splitting boundaries, model fitting safety, and accuracy metrics).
