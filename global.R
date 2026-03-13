library(shiny)
library(lubridate)
library(leaflet)
library(terra)
library(ggplot2)
library(ncdf4)
library(dplyr)
library(shinycssloaders)

# year in the footer
year_footer = year(Sys.Date())

# call modules
source("R/modules/home_module.R")
source("R/modules/analysis_module.R")
source("R/modules/about_module.R")

# Upload Limit
options(shiny.maxRequestSize = 500 * 1024^2)

