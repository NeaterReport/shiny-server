# global for JAEG BC Liquor

# ----- Need More Emelie Solution! JAEG -----
# To Do:
# 1 - Better ways to handled tie ranks (right now is random or subset first 10)
# 2 - Add additional info from the bcliquor store website (e.g., stock, rating, other?)
# 3 - Streamline the code
# 4 - Learn javascript to dynamically adjust iframe
# 5 - Add ability to highlight country in the explorer graph

# Issue:
# 1 - App may still crashes for some combination of class and country for liquor explorer
# 2 - Dynamic updates sliders for the liquor explorer
# 3 - legend placement of the liquor explorer
# 4 - Consistent order of legend for the top and bottom plots
# 5 - datatable single row selection is addictive, should be single for real

# ---- Load Packages ----

library(DT)
library(leaflet)
library(dplyr)
library(ggmap)
library(ggplot2)
library(ggvis)
library(readr)
library(rvest)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(sortableR)
library(stringr)
library(scales)
# library(treemapify)


# ---- Import geocoded country ----
load("www/geocoded_country.Rds")

# ---- Import data ----
bcliquor_df <- read_csv("www/BC Liquor Store Product Price List.csv")

# Make nicer name
names(bcliquor_df) <- c("type", "class", "subclass", "minorclass", "country", "SKU", "name", "baseUPC", "l/container", "container/Unit", "alcoholper", "price", "sweetness")

# Divide percent by 100 to turn into proportion for nice formatting
bcliquor_df$alcoholper <- bcliquor_df$alcoholper / 100

# Convert to Proper Case
# nifty fn
# see http://stackoverflow.com/questions/15776732/how-to-convert-a-vector-of-strings-to-title-case
topropper <- function(x) {
  # Makes Proper Capitalization out of a string or collection of strings. 
  sapply(x, function(strn)
  { s <- strsplit(strn, "\\s")[[1]]
  paste0(toupper(substring(s, 1,1)), 
         tolower(substring(s, 2)),
         collapse=" ")}, USE.NAMES=FALSE)
}

bcliquor_df[c(1:5, 7)] <- sapply(bcliquor_df[c(1:5,7)], topropper)

# Modified tree map fn
ggtify <- function (treeMap, label.groups = TRUE) {
  require(ggplot2)
  require(plyr)
  require(reshape2)
  if (missing(treeMap) || is.data.frame(treeMap) == FALSE) {
    stop("Must provide a data frame")
  }
  xlim <- c(min(treeMap["xmin"]), max(treeMap["xmax"]))
  ylim <- c(min(treeMap["ymin"]), max(treeMap["ymax"]))
  Plot <- ggplot(treeMap)
  Plot <- Plot + coord_cartesian(xlim = xlim, ylim = ylim)
  Plot <- Plot + geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, 
                               ymax = ymax, fill = fill))
  Plot <- Plot + geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, 
                               ymax = ymax), fill = NA, colour = "grey", size = 0.2)
  Plot <- Plot + theme(axis.ticks = element_blank(), axis.title = element_blank(), 
                       axis.text = element_blank())
  Plot <- Plot + guides(fill = guide_legend(title = attributes(treeMap)$fillName))
  if ("group" %in% colnames(treeMap)) {
    groupRects <- ddply(treeMap, .(group), plyr::summarise, xmin <- min(xmin), 
                        xmax <- max(xmax), ymin <- min(ymin), ymax <- max(ymax))
    names(groupRects) <- c("group", "xmin", "xmax", "ymin", 
                           "ymax")
    Plot <- Plot + geom_rect(data = groupRects, mapping = aes(xmin = xmin, 
                                                              xmax = xmax, ymin = ymin, ymax = ymax), colour = "grey", 
                             fill = NA, size = 1.2)
    Plot <- Plot + theme(panel.border = element_rect(size = 2, 
                                                     fill = NA, colour = "grey"))
  }
  if (label.groups == TRUE && "group" %in% colnames(treeMap)) {
    if ("label" %in% colnames(treeMap)) {
      groupLabels <- ddply(treeMap, c("group"), plyr::summarise, 
                           x <- max(xmax) - ((max(xmax) - min(xmin)) * 0.5), 
                           y <- min(ymin) + 2, size <- (max(xmax) - min(xmin))/nchar(as.character(group[1])))
    }
    else {
      groupLabels <- ddply(treeMap, c("group"), plyr::summarise, 
                           x <- max(xmax) - ((max(xmax) - min(xmin)) * 0.5), 
                           y <- max(ymax) - ((max(ymax) - min(ymin)) * 0.5), 
                           size <- (max(xmax) - min(xmin))/(nchar(as.character(group[1]))))
    }
    names(groupLabels) <- c("group", "x", "y", "size")
    Plot <- Plot + annotate("text", x = groupLabels$x, y = groupLabels$y, 
                            label = groupLabels$group, size = groupLabels$size, 
                            colour = "darkgrey", fontface = "bold", hjust = 0.5, 
                            vjust = 0)
  }
  if ("label" %in% colnames(treeMap)) {
    treeMap <- ddply(treeMap, "label", plyr::mutate, labelx = xmin + 
                       1, labely = ymax - 1, labelsize = 30
    )
    Plot <- Plot + geom_text(data = treeMap, aes(label = label, 
                                                 x = labelx, y = labely, size = labelsize), hjust = 0, 
                             vjust = 1, colour = "black")
    Plot <- Plot + scale_size(range = c(1, 8), guide = FALSE)
  }
  return(Plot)
}

# treempaify fn

treemapify <- function (data, area, fill, group = FALSE, label = FALSE, xlim = c(0, 
                                                                   100), ylim = c(0, 100))
{
  require(ggplot2)
  require(plyr)
  require(reshape2)
  if (missing(data) || is.data.frame(data) == FALSE) {
    stop("Must provide data")
  }
  if (missing(area) || area %in% colnames(data) == FALSE) {
    stop("Must specify an area aesthetic with area=\"colname\" (and it must exist in the data frame)")
  }
  if (missing(fill) || fill %in% colnames(data) == FALSE) {
    stop("Must specify a fill aesthetic with fill=\"colname\" (and it must exist in the data frame)")
  }
  if (missing(group) == FALSE && group %in% colnames(data) == 
      FALSE) {
    stop("If you want a group aesthetic (optional), it must be specified with group=\"colname\" (and it must exist in the data frame)")
  }
  if (missing(group) == FALSE && is.factor(data[[group]]) == 
      FALSE) {
    stop("Group aesthetic must be a factor")
  }
  if (missing(label) == FALSE && label %in% colnames(data) == 
      FALSE) {
    stop("If you want labels (optional), they must be specified with label=\"colname\" (and the column must exist in the data frame)")
  }
  if (missing(label) == FALSE && is.factor(data[[label]]) == 
      FALSE) {
    stop("Label column must be a factor")
  }
  if (is.numeric(xlim) == FALSE || length(xlim) != 2) {
    stop("Invalid xlim (try something like \"xlim=c(0,100)\")")
  }
  if (is.numeric(ylim) == FALSE || length(ylim) != 2) {
    stop("Invalid ylim (try something like \"ylim=c(0,100)\")")
  }
  if (missing(group) == FALSE) {
    if (missing(label)) {
      treeMapData <- data.frame(area = data[area], fill = data[fill], 
                                group = data[group])
      names(treeMapData) <- c("area", "fill", "group")
    }
    else {
      treeMapData <- data.frame(area = data[area], fill = data[fill], 
                                group = data[group], label = data[label])
      names(treeMapData) <- c("area", "fill", "group", 
                              "label")
    }
    plotArea <- prod(diff(xlim), diff(ylim))
    scaleFactor <- plotArea/sum(treeMapData$area)
    treeMapData$area <- scaleFactor * treeMapData$area
    groupData <- ddply(treeMapData, "group", summarise, area = sum(area), 
                       fill = group[1])
    groupTreeMap <- treemapify(groupData, area = "area", 
                               fill = "fill", xlim = xlim, ylim = ylim)
    if (missing(label)) {
      treeMap <- data.frame(area = numeric(), fill = factor(), 
                            group = factor(), xmin = numeric(), xmax = numeric(), 
                            ymin = numeric(), ymax = numeric())
    }
    else {
      treeMap <- data.frame(area = numeric(), fill = factor(), 
                            group = factor(), label = character(), xmin = numeric(), 
                            xmax = numeric(), ymin = numeric(), ymax = numeric())
    }
    for (thisGroup in groupTreeMap[["fill"]]) {
      xmin <- as.numeric(groupTreeMap[groupTreeMap[, "fill"] == 
                                        thisGroup, ]["xmin"])
      xmax <- as.numeric(groupTreeMap[groupTreeMap[, "fill"] == 
                                        thisGroup, ]["xmax"])
      ymin <- as.numeric(groupTreeMap[groupTreeMap[, "fill"] == 
                                        thisGroup, ]["ymin"])
      ymax <- as.numeric(groupTreeMap[groupTreeMap[, "fill"] == 
                                        thisGroup, ]["ymax"])
      thisGroupData <- treeMapData[treeMapData[, "group"] == 
                                     thisGroup, ]
      if (missing(label)) {
        thisGroupRects <- treemapify(thisGroupData, fill = "fill", 
                                     area = "area", xlim = c(xmin, xmax), ylim = c(ymin, 
                                                                                   ymax))
      }
      else {
        thisGroupRects <- treemapify(thisGroupData, fill = "fill", 
                                     area = "area", label = "label", xlim = c(xmin, 
                                                                              xmax), ylim = c(ymin, ymax))
      }
      thisGroupRects["group"] <- thisGroup
      treeMap <- rbind(treeMap, thisGroupRects)
    }
    attr(treeMap, "fillName") <- fill
    treeMap$area <- NULL
    return(treeMap)
  }
  if (missing(label)) {
    treeMapData <- data.frame(area = data[area], fill = data[fill])
    names(treeMapData) <- c("area", "fill")
  }
  else {
    treeMapData <- data.frame(area = data[area], fill = data[fill], 
                              label = data[label])
    names(treeMapData) <- c("area", "fill", "label")
  }
  treeMapData <- treeMapData[with(treeMapData, order(-area)), 
                             ]
  plotArea <- prod(diff(xlim), diff(ylim))
  scaleFactor <- plotArea/sum(treeMapData$area)
  treeMapData$area <- scaleFactor * treeMapData$area
  if (missing(label)) {
    treeMap <- data.frame(area = numeric(), fill = factor(), 
                          xmin = numeric(), xmax = numeric(), ymin = numeric(), 
                          ymax = numeric())
  }
  else {
    treeMap <- data.frame(area = numeric(), fill = factor(), 
                          label = character(), xmin = numeric(), xmax = numeric(), 
                          ymin = numeric(), ymax = numeric())
  }
  emptyxMin <- xlim[1]
  emptyxMax <- xlim[2]
  emptyyMin <- ylim[1]
  emptyyMax <- ylim[2]
  stackPointer <- 1
  continue <- TRUE
  while (continue) {
    nInRow <- 1
    emptyx <- emptyxMax - emptyxMin
    emptyy <- emptyyMax - emptyyMin
    if (emptyx > emptyy) {
      subdivideDirection <- "horizontal"
    }
    else if (emptyx < emptyy) {
      subdivideDirection <- "vertical"
    }
    else if (emptyx == emptyy) {
      subdivideDirection <- "horizontal"
    }
    if (subdivideDirection == "horizontal") {
      rowLongDimension = emptyyMax - emptyyMin
    }
    else {
      rowLongDimension = emptyxMax - emptyxMin
    }
    lastAspectRatio <- Inf
    stackPointerRow <- stackPointer
    while (continue) {
      if (missing(label)) {
        treeMapRow <- data.frame(area = numeric(), fill = factor(), 
                                 xmin = numeric(), xmax = numeric(), ymin = numeric(), 
                                 ymax = numeric())
      }
      else {
        treeMapRow <- data.frame(area = numeric(), fill = factor(), 
                                 label = character(), xmin = numeric(), xmax = numeric(), 
                                 ymin = numeric(), ymax = numeric())
      }
      stackPointer <- stackPointerRow
      totalRowArea <- sum(treeMapData$area[stackPointer:(stackPointer + 
                                                           nInRow - 1)])
      rowShortDimension <- totalRowArea/rowLongDimension
      if (subdivideDirection == "horizontal") {
        rowPlacePointer <- emptyyMin
      }
      else {
        rowPlacePointer <- emptyxMin
      }
      aspectRatio <- numeric()
      for (i in 1:nInRow) {
        thisRect <- treeMapData[stackPointer, ]
        stackPointer <- stackPointer + 1
        rectSubdivideLength <- thisRect$area/rowShortDimension
        if (subdivideDirection == "horizontal") {
          rectxMin <- emptyxMin
          rectxMax <- emptyxMin + rowShortDimension
          rectyMin <- rowPlacePointer
          rectyMax <- rowPlacePointer + rectSubdivideLength
          rowPlacePointer <- rectyMax
        }
        else {
          rectxMin <- rowPlacePointer
          rectxMax <- rowPlacePointer + rectSubdivideLength
          rowPlacePointer <- rectxMax
          rectyMin <- emptyyMin
          rectyMax <- emptyyMin + rowShortDimension
        }
        if (missing(label)) {
          newRect <- data.frame(area = thisRect$area, 
                                fill = thisRect$fill, xmin = rectxMin, xmax = rectxMax, 
                                ymin = rectyMin, ymax = rectyMax)
        }
        else {
          newRect <- data.frame(area = thisRect$area, 
                                fill = thisRect$fill, label = thisRect$label, 
                                xmin = rectxMin, xmax = rectxMax, ymin = rectyMin, 
                                ymax = rectyMax)
        }
        treeMapRow <- rbind(treeMapRow, newRect)
        aspectRatio <- max(c(aspectRatio, rowShortDimension/rectSubdivideLength, 
                             rectSubdivideLength/rowShortDimension))
      }
      if (aspectRatio > lastAspectRatio) {
        stackPointer <- stackPointer - 1
        treeMap <- rbind(treeMap, previousRow)
        if (subdivideDirection == "horizontal") {
          emptyxMin <- emptyxMin + previousShortDimension
        }
        else {
          emptyyMin <- emptyyMin + previousShortDimension
        }
        continue <- FALSE
      }
      else {
        if (stackPointer - 1 < nrow(treeMapData)) {
          nInRow <- nInRow + 1
          lastAspectRatio <- aspectRatio
          previousRow <- treeMapRow
          previousShortDimension <- rowShortDimension
        }
        else {
          treeMap <- rbind(treeMap, treeMapRow)
          continue <- FALSE
        }
      }
    }
    if (stackPointer - 1 == nrow(treeMapData)) {
      continue <- FALSE
    }
    else {
      continue <- TRUE
    }
  }
  attr(treeMap, "fillName") <- fill
  treeMap$area <- NULL
  return(treeMap)
}

# ---- Create my own theme JAEG ^^ ----

theme_jaeg <- 
  theme(panel.grid.major.x = element_line(colour = "#1F77B4", size = rel(.2)),
        panel.grid.major.y = element_line(colour = "#1F77B4", size = rel(.2)),
        panel.border = element_blank(),
        axis.text = element_text(colour = "#1F77B4"),
        axis.title = element_text(colour = "#2CA02C"),
        axis.ticks.x = element_blank(),
        axis.line = element_blank(),
        panel.background = element_rect(fill = "#FEFBFB"),
        plot.background = element_rect(fill = "#FEFBFB"),
        legend.position = "bottom",
        legend.background = element_rect(fill = "#FEFBFB"))

# ---- Geocode the country ----
# Only do once
# Did some manual fixing of country name to match
# country_geo <- bcliquor_df %>% 
#   group_by(country) %>% tally()
# 
# country_geo_code <- geocode(country_geo$country, output="more")
# 
# country_geo_coded <- cbind(country_geo, country_geo_code)
# 
# save(country_geo_coded, file="~/Desktop/PROTEST/Portfolio/BC liquor/www/geocoded_country.Rds")
# Load the geo-coded data from now on

