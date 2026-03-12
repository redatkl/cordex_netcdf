library(shiny)
library(lubridate)
library(leaflet)

# year in the footer
year_footer = year(Sys.Date())

# call modules
source("R/modules/home_module.R")
source("R/modules/analysis_module.R")
source("R/modules/about_module.R")

