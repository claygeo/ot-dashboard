# Shiny R Dashboard

A Shiny R web application for analyzing overtime hours, Units Per Labor Hour (UPLH), and equipment breakages across departments. It provides interactive visualizations, predictive maintenance forecasts, and actionable recommendations to optimize workforce productivity and equipment reliability, built as a demo for Q1 2025.

## Table of Contents 
- [Features](#features)
- [Prerequisites](#prerequisites)
- [Setup](#setup)
- [Visuals](#visuals)
- [Notes](#notes)

## Features

- Interactive dashboard with tabs for Overtime Overview, Overtime Trend, UPLH Review, and Equipment Analysis
- Plotly visualizations including bar charts, line plots, heatmaps, and Sankey diagrams
- Filterable data by department, date range, equipment, and severity
- Predictive maintenance forecasts for equipment breakages over the next 30 days
- Downloadable HTML reports with overtime and UPLH summaries
- Actionable insights and recommendations for reducing overtime and improving equipment reliability

## Prerequisites

- R (version 4.0 or higher)
- RStudio (recommended for running Shiny apps)
- R packages: shiny, dplyr, ggplot2, plotly, DT, lubridate, shinythemes

## Setup

1. Clone the repository: git clone https://github.com/claygeo/ot-dashboard.gt
2. Navigate to the project directory: cd ot-dashboard
3. Install R packages:install.packages(c("shiny", "dplyr", "ggplot2", "plotly", "DT", "lubridate", "shinythemes"))


Ensure the data/ folder contains weekly_metrics.csv, employees.csv, and equipment_breakages.csv
Run the app in RStudio (open app.R and click "Run App") or in R:library(shiny)
runApp("app.R")

## Visuals

Main Overview: 
![image](https://github.com/user-attachments/assets/13df857e-cb03-48ac-9bc7-36cc0b9df8db)

Warehouse Layout:
![image](https://github.com/user-attachments/assets/f3853354-48c3-4bc4-a7da-33a4ddfca6e8)

Shelf Details:
![image](https://github.com/user-attachments/assets/42997d84-ca4d-42a7-a4a0-6ca127e8966a)

Component Search: 
![image](https://github.com/user-attachments/assets/7c1c6b21-7103-408e-9b81-31c1dc622590)

Analytics:
![image](https://github.com/user-attachments/assets/58c69b3c-f3ef-4fd0-a26f-67dd3163eb20)

## Notes

Ensure CSV files in data/ match the expected format (see app.R for column requirements).
The dashboard is designed as a demo for Q1 2025, used with permission.

