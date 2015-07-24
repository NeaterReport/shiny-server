# global for JAEG Wait Time

# ---- Need More Emelie Solution! JAEG ----
# To-do:
# 1 - Add the northern territory back on the map
# 2 - Add dynamic file naming

# Issue:
# 1 - Report Format need a lot of work!
# 2 - Center image using rmarkdown?
# 3 - Alignment of figure in Word

# Load packages
library(directlabels)
library(dplyr)
library(DT)
library(ggplot2)
library(ggthemes)
library(knitr)
library(readxl)
library(scales)
library(shiny)
library(shinydashboard)

# geo related packages
library(ggmap)
library(gpclib) # This has restrictive license, but maybe needed for foritfy if there is no rgeos
library(maps)
library(maptools) # read shapefiles
#library(rgdal) # read shapefiles
library(raster) 
#library(rgeos) # Suppose to be a free alternative for gpclib
#library(sp)

# How to add fancy header and footer on markdown for pdf
# see http://stackoverflow.com/questions/25329375/creating-a-footer-for-every-page-using-r-markdown

# import data
waitTime_region_df <- read_excel("www/CIHI treatment wait time.xlsx", "To R Region")
waitTime_prov_df <- read_excel("www/CIHI treatment wait time.xlsx", "To R Prov")

# Create Additional variable
# Add a dummy indicator
waitTime_prov_df$MetB <- ifelse(waitTime_prov_df$MetBenchmark > 90, "Met", "Not Met")

# reorder from west to east using factor
waitTime_prov_df$Prov <- factor(waitTime_prov_df$Province, 
                       levels = c("B.C.", "Alta.", "Sask.", "Man.", "Ont.", "Que.", "N.B.", "P.E.I.",
                                  "N.S.", "N.L.", "Canada"))

# Read in the shape file
map_canada <- readShapeSpatial("www/prov4map/prov4map.shp")
# use Prov_Code as the region ID
gpclibPermit() # Turn it on
mapo <- fortify(map_canada, region="Prov_Code")

# Source the bullegraph script
source("http://softeng.polito.it/software/R/BulletGraph.R")

# Custom bullegraph script
bulletgraph2 <- function(x,ref,limits, name=NULL, subname="", width=0.4, col=NULL, 
         palette=NULL,colored=T){
  if(length(limits)<3){
    stop("limits must be a vector with at least three elements")
  }
  if(length(x)!=1){
    stop(paste("x must be a scalar",name))
  }
  limits=sort(limits)
  if(x<limits[1] | x>limits[4]){
    stop("x must be within outer limits")
  }
  if(ref<limits[1] | ref>limits[4]){
    stop("x must be within outer limits")
  }
  if(width<.01 | width>1){
    stop("width must be in the range [0 .. 1]")
  }
  if(is.null(name)) name = sys.call()[[2]]
  if(is.null(palette)){
    palette = bulletgraph.palette2(length(limits)-1,colored)
  }
  if(is.null(col)){
    if(colored) col="steelblue3"
    else col = "black"
  }
  n = length(limits)
  ranges = matrix(tail(limits,-1)-c(0,head(tail(limits,-1),n-2)),n-1)
  barplot(ranges,col=palette,border=NA,horiz=T,
          xlim=c(min(limits),max(limits)),xpd=F)
  barplot(x[1],width= width,names.arg=name,cex.names=1,
          space=((1-width)/2+0.2)/width,
          add=T,horiz=T,border=NA,col=col,las=1,xpd=F)
  segments(ref,.3,ref,1.1,lwd=3) # move it here so the ref line shows on top
  mtext(subname,side=2,line=1,at=0.4,cex=0.6,adj=1,las=2)
  if(limits[1]!=0){
    warning("Bars should be drawn on a zero-based scale: using a jagged base to remark such non conformance.")
    jit = (limits[4] - limits[1])/100
    x = c(rep(c(limits[1],limits[1]+jit),6),0)
    y = c(2:13/10,1.2)
    polygon(x,y,col="white",border="white")
  }
}

bulletgraph.palette2 <- function(n,colored){
  if(colored){ # this colors are up to my personal taste ;-)
    # Adapted to looks like tableau
    if(n==2) cols = c(hsv(0,0,c(.65,.9)),"#eff3ff")
    if(n==3) cols = c(hsv(0,0,c(.6,.75,.9)),"#eff3ff")
    if(n==4) cols = c(hsv(0,0,c(.6,.75,.8,.9)),"#eff3ff")
    if(n==5) cols = c(hsv(0,0,c(.6,.75,.8,.9,.97)),"#eff3ff")
    if(n>5) cols = hsv((n:1 + n*0.3)/(n*1.3),.9,.8+(1:n/(n*5)))
  }else{ # these are the gray levels recommended by Stephen Few
    if(n==2) cols = hsv(0,0,c(.65,.9))
    if(n==3) cols = hsv(0,0,c(.6,.75,.9))
    if(n==4) cols = hsv(0,0,c(.5,.65,.8,.9))
    if(n==5) cols = hsv(0,0,c(.5,.65,.8,.9,.97))
    if(n>5) cols = hsv(1,0,seq(.4,.97,length.out=n))
  }
  return(cols)
}

# Create wrapper function for bulletgraph
bbgraph <- function(score = 50, bbname = "Region", col = T) {
  score <- as.numeric(score)
  bulletgraph2(x=score,ref=90,limits=c(0,60,80,100),
              name = bbname,subname="",
              colored=col)
}

# Create dummy data for plotting benchmark
# see http://stackoverflow.com/questions/11846295/how-to-add-different-lines-for-facets
benchmarkdata <- data.frame(Treatment = c(
  "Hip Replacement",
  "Knee Replacement",
  "Hip Fracture Repair (Acute/Day Surgery)",
  "Hip Fracture Repair (Emergency)",       
  "Cataract",
  "Bypass Surgery",                
  "Radiation Therapy",
  "CT Scan",                                
  "MRI Scan",
  "Bladder Cancer Surgery",
  "Breast Cancer Surgery",
  "Colorectal Cancer Surgery",
  "Lung Cancer Surgery",
  "Prostate Cancer Surgery"             
), Benchmark = c(182, 182, 48, 48, 112, 182, 28, rep(NA, 7)))

# Convert to datatable
benchmarkdata <- tbl_dt(benchmarkdata)