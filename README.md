---
title: "README"
output: html_document
date: "2024-05-12"
---

# Overview

This repository contains mutliple Shiny dashboard application designed to analyze and visualize factors influencing wine consumption globally. The application utilizes data from various sources and performs data manipulations, visualization, and regression analysis to generate insightful outputs.

### Files and Structure

#### Application Files
MapApp.R: Uses choropleth map to visualize wine consumption data 
TrendsApp.R: Analyze agregate and factored time trends
VariablesApp.R: A glance at variable data and reationship with wine consumption
RegResultsApp.R: A fixed effects model regression analysis and discussion of results
CapDashHelper.Rmd: Helper file, created to support the Shiny dashboard application by accomplishing data uploads, data manipulations, function creations for data visualization and regression analysis, and saving all data to RDA files.


#### Data Files
data/cap_data.rda: Contains the main dataset with wine consumption and related variables.
data/cap_pdata.rda: Panel data version of the main dataset.
data/title_data.rda: Dataset with descriptive and well-formatted variable names.
data/title_pdata.rda: Panel data version of the titled dataset.
data/coded_panel.rda: Dataset combined with country geometries for choropleth maps.
data/panel_subsets.rda: Various subsets of panel data for regression analysis.
data/model_formulas.rda: Formulas for different regression models.
data/robust_se_model_x.rda: Compluted and then manually uploaded robust standard errors of the model, x=[1:6]

#### Key Sections in CapDashHelper.Rmd
Data Uploads: Uploads data from Google Drive spreadsheet and other sources.
Data Manipulations: Edits and formats data for consistency and ease of use.
Titled Data: Renames variables for better readability in visualizations.
Choropleth Map: Prepares data for creating choropleth maps with country geometries.
Subset Panel Generation: Creates subsets of panel data for detailed analysis.
Regression Analysis: Defines functions for performing regression analysis and statistical computations.

#### Required Libraries
The helper file will load the following:
library(readr)
library(plm)
library(dplyr)
library(tidyr)
library(broom)
library(zoo)
library(sandwich)
library(lmtest)
library(stargazer)

### Usage
#### Running the Applications
For the first run of the Shiny dashboard applications, simply keep the provided files contained all in the same directory open and run the helper file in RStudio or another R environment to generate the data, then open and run any of the dashboard applications as you please. The first step of running the helper file need not be repeated after the first run unless the user would like to make edits to the data that the applications will use. The dashboards present various interactive visualizations and analyses.

#### Data Sources
Anderson, K. and V. Pinilla (with the assistance of A.J. Holmes), Annual Database of Global Wine Markets, 1835 to 2022, freely available in Excel at the University of Adelaide’s Wine Economics Research Centre, December 2023
