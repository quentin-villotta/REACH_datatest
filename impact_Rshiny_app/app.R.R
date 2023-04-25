# Load packages
library(tidyr)
library(dplyr)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(purrr)
library(shinythemes)
library(readr)
library(readxl)
library(lubridate)
library(hrbrthemes)
library(httr)
library(glue)
library(stringr)
library(dashboardthemes)

# Source files
source('ui.R', local = TRUE)
source('server.R')


# Launch app
shinyApp(
  ui = ui,
  server = server
)