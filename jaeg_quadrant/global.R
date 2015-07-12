# global for JAEG Quadrant Plot

# ---- Need More Emelie Solution! JAEG ----

# To Do:
# 1 - Dynamically fit n x n grid plot
# 2 - Use the same colour for each quadrant for all plots
# 3 - Update the colour theme for the entire app
# 4 - Find better ways to generate random names
# 5 - More flexible way to explore the data
# 6 - Nicer display of the correlation and mean
# 7 - Add support for percent positive scores
# 8 - Add report
# 9 - Chubby or skiny graph detection (assume factor based on range?)
# 10 - Add more random data type
# 11 - Add more upload data type option
# 12 - Integrate col reorder with fixed col in DT

# Issue:
# 1 - App crashes when new data is loaded (if the var select field is not empty)
# 2 - Need way to auto reset the var select field
# 3 - Read old excel file
# 4 - Strange chubby sidebar Panel in datatable
# 5 - Can't plot var with double or triple first names

# ---- Load library ----

library(dplyr)
library(ggplot2)
library(ggthemes)
library(DT)
library(randomNames) # to generate random names!
require(markdown)
require(rmarkdown)
library(readxl)
library(shiny)
library(shinydashboard)
library(shinyjs) # to reset field
library(shinythemes)

# ---- Fn to generate random data ----

# FnRandom Rating
gen_rating <- function(v = 20, nr = 5, n = 100) {
  test_df <- data.frame(replicate(v, sample(1:nr, n, rep = TRUE)))
  # Convert into factor for chubby bar in ggplot2
  test_df[sapply(test_df, is.numeric)] <- lapply(test_df[sapply(test_df, is.numeric)], as.factor)
  test_df <- data.frame(ID = 1:n, test_df) # add sequential ID
  # Generate random first name to use as variable names
  names(test_df) <- randomNames(v+1, which.names="first")
  names(test_df)[1] <- "ID"
  names(test_df)[v+1] <- "DV"
  test_df
}

# Fn to Negatively Skew Rating
gen_pos_rating <- function(v = 20, nr = 5, n = 100) {
  test_df <- data.frame(replicate(v, round(rbeta(n,2,0.5,ncp=2)*nr)))
  # Convert into factor for chubby bar in ggplot2
  test_df[sapply(test_df, is.numeric)] <- lapply(test_df[sapply(test_df, is.numeric)], as.factor)
  test_df <- data.frame(ID = 1:n, test_df) # add sequential ID
  # Generate random first name to use as variable names
  names(test_df) <- randomNames(v+1, which.names="first")
  names(test_df)[1] <- "ID"
  names(test_df)[v+1] <- "DV"
  test_df
}

# Fn to Random Normal
gen_rnorm <- function(v = 20, n = 100) {
  test_df <- data.frame(ID = 1:n, replicate(v,sample(rnorm(n*10, 7, 2.5),n,rep=TRUE)))
  # Generate random first name to use as variable names
  names(test_df) <- randomNames(v+1, which.names="first")
  names(test_df)[1] <- "ID" # add sequential ID
  names(test_df)[v+1] <- "DV"
  test_df
}

# ---- Fn to calculate means and correlation ----

makeQuadrantData <- function(test_df = NULL, meancut, corcut) {
  
  # Convert any factors back to numerics to calculate the cor
  test_df[sapply(test_df, is.factor)] <- lapply(test_df[sapply(test_df, is.factor)], as.numeric)
  
  # Calculate cor except for the ID variable
  # ID variable is assumed to be the first column
  var_cor <- as.data.frame(cor(test_df[-1], 
             use = "pairwise.complete.obs", method = c("pearson")))
  
  # Calculate means except for the ID variable
  # ID variable is assumed to be the first column
  var_means <- colMeans((test_df[,-1]))
  
  # Extract the last col of the cor matrix (minus the last row, which is cor of DV onto itself)
  # The last col contains the cor coefficient between each IV and DV
  # Combine into one dataframe
  q_df <- data.frame(cor = var_cor[-nrow(var_cor),-1:-(ncol(var_cor)-1)], 
                     mean = var_means[-length(var_means)])

  # Use median as the default cutoffs when there is no user specify cut point
  if(!is.null(meancut) & !is.null(corcut)) {
    median_mean <- meancut
    median_cor <- corcut
  } else {
    median_mean <- median(q_df$mean)
    median_cor <- median(q_df$cor) 
  }
  
  # Assign group into quadrant for colouring
  #  1 | 2
  #  - - -
  #  4 | 3
  q_df$group <- NA
  q_df[q_df$mean <= median_mean & q_df$cor > median_cor, "group"] <- 1
  q_df[q_df$mean > median_mean & q_df$cor > median_cor, "group"] <- 2
  q_df[q_df$mean > median_mean & q_df$cor <= median_cor, "group"] <- 3
  q_df[q_df$mean <= median_mean & q_df$cor <= median_cor, "group"] <- 4
  q_df$group <- factor(q_df$group)
  
  assign("q_df", q_df, envir = globalenv())
  
  q_df
  
}