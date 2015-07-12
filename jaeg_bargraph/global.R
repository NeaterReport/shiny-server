# global for JAEG Bar Graph

# ---- Need More Emelie Solution! JAEG ----

# Issue:
# 1 - Ran out of colour palette for some themes for the multiple bar graph

# ---- Load Pakcages ----

library(DT)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(plyr)
library(dplyr) # load dplyr last
library(readr)
library(markdown)
library(rmarkdown)
# library(RLumShiny) # for the colour picker
library(scales)
library(shiny)
library(shinydashboard)
# library(sortableR)

# ---- Load Data ----

# Import the cleaned lost and found animals data from the City of Vancouver
lostAnimal <- read_csv("www/VancouverLostAnimals.csv")

# Create date variables
lostAnimal <- lostAnimal %>% mutate(Year = factor(year(Date)),
                                    Month = factor(month(Date)),
                                    Day = day(Date),
                                    Wday = wday(Date, label=TRUE))
