# ui for Statistics Corner

# ---- Need More Emelie Solution! JAEG ----
# To Do:
#1 Add more statistical module! Need more ingenious teaching tools from Emelie!

# ---- Load library ----

library(shiny)
library(shinydashboard)
library(ggplot2)
library(plyr)
library(dplyr)
library(ggthemes)
library(scales)
library(shinythemes)
library(MASS)

navbarPage(title = "Statistics Corner",
          theme = shinytheme("flatly"),
          
  tabPanel("Welcome",
    # Reference custom css style sheet here, otherwise the first page is blank, strange ...
    tags$head(
      tags$style(HTML("<link href='http://fonts.googleapis.com/css?family=Dancing+Script' rel='stylesheet' type='text/css'>"))
    ),
    includeCSS("www/styles.css"),
    fluidPage(
      wellPanel(
        h2("Welcome to Statistics Corner!"),
         p("Statistics Corner is a shiny app to learn about Statistics. Each concept illustrated is hand picked by @('_')@ and inspired by", 
           strong("Emelie!", 
                  style='font-family: Dancing Script;
                  color: pink;
                  font-size:x-large;'), 
           "We take an interactive approach to understand various key concepts in statistics, such as sampling and correlation."),
         p("If there is a concept you like to see, please let us know!"),
        h3("Inspiration"),
        p("This is one of many shiny apps dedicated to and inspired by",
          strong("Emelie!", 
                 style='font-family: Dancing Script;
                  color: pink;
                  font-size:x-large;'),
        ". The app is part of the JAEG signature collection of data visualization and reporting. If you like the app, please send your thanks to",
        strong("Emelie!", 
               style='font-family: Dancing Script;
                  color: pink;
                  font-size:x-large;')),
        # Gotta luv @('_')@!
        p(img(src="SnowMonkey.jpg"))
      ) # wellPanel
    ) # fluidPage
    ), # tabPanel "Welcome"
  
  # Apparantly, navbarMenu cannot be first item ...     

# ---- NavBar Menu ----
  navbarMenu(title = "Correlation",
    tabPanel(title = "Scatterplot",
      headerPanel(
        h2("Lets Correlate!")
      ),
      
    sidebarLayout(
      
# ---- Sidebar Panel ----
      sidebarPanel(
        actionButton("plot_button", " Plot Me!", icon = icon("circle-thin"), 
                     style='color: pink;
                     background-color: darkblue;'),
        br(),br(),
        
        numericInput("n", "Sample Size", value = 500, min = 1, max = 5000, step = 100),
        sliderInput("r", label="Correlation",
                    min=-1, max=1, value=0, step=.01,
                    animate = animationOptions(loop = FALSE, interval = 300)),
        numericInput("xmean", "Mean of x", value = 0, min = 0),
        numericInput("ymean", "Mean of y", value = 0, min = 0),
        numericInput("xsd", "SD of x", value = 1, min = 0),
        numericInput("ysd", "SD of y", value = 1, min = 0),
        br(),br(),
        
        # Add selector for graph style from ggthemes
        selectInput("graphstyle", label = "Roll your style!",
                    choices = c("Classic" = 1,
                                "Economist" = 2,
                                "Excel" = 3,
                                "Few" = 4,
                                "FiveThirtyEight" = 5,
                                "Stata" = 6,
                                "Tufte" = 7),
                    selected = 5)
      ), # sidebarPane

# ---- Main Panel ----
    mainPanel(
      h2("Scatter Plot"),
      plotOutput("plot_corr"),
      br(),br(),
      
      dataTableOutput("datatable")
      ) # mainPanel
    ) # sidebarLayout
  ), # tabPanel "Scatterplot"
  
  tabPanel(title = "Strength of Association",
           headerPanel(
             h2("Correlation of Various Magnitudes")
           ),
    fluidPage(
      wellPanel(
        numericInput("draw_n", "Sample", value = 100, min = 0, max = 5000, step=100),
        actionButton("button_draw", " Draw Me!", icon = icon("eye"), 
                   style='color: pink;
                     background-color: darkblue;')),
      
      fluidRow(
        box(solidHeader = TRUE, width=4,
            h5("Weak Positive Correlation", align="center"), 
            p(verbatimTextOutput("r1")),
            plotOutput("weakplot_pos")),
        box(solidHeader = TRUE, width=4,
            h5("Medium Positive Correlation", align="center"),
            p(verbatimTextOutput("r2")),
            plotOutput("medplot_pos")),
        box(solidHeader = TRUE, width=4,
            h5("Strong Positive Correlation", align="center"),
            p(verbatimTextOutput("r3")),
            plotOutput("strongplot_pos"))
      ),
      
      fluidRow(
        box(solidHeader = TRUE, width=4,
            h5("Weak Negative Correlation", align="center"),
            p(verbatimTextOutput("r4")),
            plotOutput("weakplot_neg")),
        box(SolidHeader = TRUE, width=4,
            h5("Medium Ngeative Correlation", align="center"),
            p(verbatimTextOutput("r5")),
            plotOutput("medplot_neg")),
        box(SolidHeader = TRUE, width=4,
            h5("Strong Negative Correlation", align="center"),
            p(verbatimTextOutput("r6")),
            plotOutput("strongplot_neg"))
      )
    ) # fluidPage
  ) # tabPanel "Scatterplot"
), # navbarMenu
  
  tabPanel(title = "Sampling",
    h2("Lets Sample!"),
    sidebarLayout(
      sidebarPanel(
        sliderInput("n_s", label="Sample Size",
                    min = 1, max = 5000, value = 10, step=100,
                    animate = animationOptions(loop = FALSE, interval = 200)),
        numericInput("mean_s", "Mean", value = 0, min = 0),
        numericInput("sd_s", "SD", value = 1, min = 0),
        br(),
        
        # Add selector for graph style from ggthemes
        selectInput("graphstyle2", label = "Roll your style!",
                    choices = c("Classic" = 1,
                                "Economist" = 2,
                                "Excel" = 3,
                                "Few" = 4,
                                "FiveThirtyEight" = 5,
                                "Stata" = 6,
                                "Tufte" = 7
                                ),
                    selected = 5)
      ), # sidebarPanel
      mainPanel(
        plotOutput("plot_sample")
      ) # mainPanel
    ) # sidebarLayout
  ) # tabPanel "Sampling"

) # navbarPage