# Global HPHS Flexdashboard App Script
# Benedito Chou & Emelie Gustafsson
# June 19 2016

# ---- Load packages ----
library(dplyr)
library(flexdashboard)
library(shiny)
library(ggplot2)
library(plotly)
library(DT)
library(stringr)
library(ggvis)
library(scales)
library(shinyjs)
library(lubridate)
library(shinydashboard)

# ---- Load Data ----

load("www/core data.RData")