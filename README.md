# Dublin Bus Demand Forecast 2025

# Project Overview
This project analyzes monthly Dublin Bus passenger flow data from 2014 to 2024 and develops a forecast for passenger demand in 2025. The aim is to support operational planning, improve service stability, and provide useful insights for public transport management.

# Dataset
The dataset contains monthly Dublin Bus passenger numbers from 2014 to 2024. It is used to examine trend, seasonality, and abnormal fluctuations in passenger demand over time.

# Objectives
- Explore the main pattern of Dublin Bus passenger flow
- Identify long term trend and seasonal variation
- Detect unusual movements in the series
- Compare forecasting methods
- Select a reliable model for forecasting passenger demand in 2025

# Methods
The analysis includes:
- Time plot
- Seasonal plot
- STL decomposition
- ACF and PACF analysis
- Seasonal Naive method
- Exponential Smoothing
- ARIMA model
- Residual diagnostics
- Forecast comparison

# Project Structure

```text
├── code/
│   └── code.R
├── data/
│   └── bus.csv
├── figures/
│   ├── Figure1_TimePlot.png
│   ├── Figure2_Adjustment.png
│   ├── Figure3_SeasonalPlot.png
│   ├── Figure4_STL_Decomposition.png
│   ├── Figure5_ACF_PACF.png
│   ├── Figure6_Model_Comparison.png
│   ├── Figure7_Residual_Diagnostics.png
│   └── Figure8_FinalForecast.png
├── tables/
│   ├── Table0_Summary_Statistics.csv
│   ├── Table1_Model_Accuracy.csv
│   └── Table2_2025_Forecast_Results.csv
└── STP80140_project_bus_V2.Rproj
