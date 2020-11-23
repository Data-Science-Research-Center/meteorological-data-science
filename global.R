################################################################################
# Entry point of the Shiny app
#
# Author: Cristian Pazmiño
# Created: 2020-04-06 10:30:24
################################################################################

# Import libraries -----
# Some needs review -----

library(shiny)
library(jsonlite)
library(mongolite)
library(shinymaterial)
library(DT)
library(shinyWidgets)
library(shinycssloaders)
library(highcharter)
library(rsconnect)
library(ggplot2)
library(readr)
library(dplyr)
library(colourpicker)
library(xts)
library(fs)
library(stats)
library(shinyjs)
library(shinyFeedback)

# Load modules -----
source("modules/mod_header_area.R")
source("modules/mod_explorer_area.R")
source("modules/mod_solution_area.R")
source("modules/mod_dbConnection.R")
source("modules/mod_dbCredentials.R")
source("modules/mod_dbQueries.R")



