# Shiny R Dashboard

A Shiny R web application for analyzing overtime hours, Units Per Labor Hour (UPLH), and equipment breakages across departments. It provides interactive visualizations, predictive maintenance forecasts, and actionable recommendations to optimize workforce productivity and equipment reliability, built as a demo for Q1 2025.

## Features

-Interactive dashboard with tabs for Overtime Overview, Overtime Trend, UPLH Review, and Equipment Analysis
-Plotly visualizations including bar charts, line plots, heatmaps, and Sankey diagrams
-Filterable data by department, date range, equipment, and severity
-Predictive maintenance forecasts for equipment breakages over the next 30 days
-Downloadable HTML reports with overtime and UPLH summaries
-Actionable insights and recommendations for reducing overtime and improving equipment reliability

## Prerequisites

-R (version 4.0 or higher)
RStudio (recommended for running Shiny apps)
-R packages: shiny, dplyr, ggplot2, plotly, DT, lubridate, shinythemes

## Setup

1. Clone the repository: git clone https://github.com/your-username/shiny-dashboard.git
2. Navigate to the project directory: cd shiny-dashboard
3. Install R packages:install.packages(c("shiny", "dplyr", "ggplot2", "plotly", "DT", "lubridate", "shinythemes"))


Ensure the data/ folder contains weekly_metrics.csv, employees.csv, and equipment_breakages.csv
Run the app in RStudio (open app.R and click "Run App") or in R:library(shiny)
runApp("app.R")



## Notes

Ensure CSV files in data/ match the expected format (see app.R for column requirements).
The dashboard is designed as a demo for Q1 2025, potentially for a specific organization (e.g., Curaleaf), used with permission.
Data files may contain sensitive information; do not share publicly without authorization.

