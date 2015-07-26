# ui for JAEG MoE

# ---- Need More Emelie Solution! JAEG ----

# To Do:
# 1 - Verify formula
# 2 - Check confidence interval formula for small sample (t distribution?)
# 3 - Pick more sensible default values for graphs

# ---- Load library ----

library(dplyr)
library(ggplot2)
library(ggthemes)
require(markdown)
require(rmarkdown)
library(scales)
library(shiny)
library(shinydashboard)

dashboardPage(skin="purple",

# ---- Dashboard Header ----             
  dashboardHeader(title = "Margin of Error",
                              
    # Notification and Batches
    dropdownMenu(type = "tasks", badgeStatus = "primary",
                 taskItem(value = 100, color = "green", "To be Awesome! :)")
    )
  ),  # dashboardHeader
 
# ---- Dashboard Side Bar ----
  dashboardSidebar(
    includeCSS("www/styles.css"),
                
    # Customize title
    h4(textInput("caption", "Name Your App", "My Awesome Error!"), align="center"),
    
    # Show current date            
    h3(textOutput(("currentDate")), align="center"),
       
    # Create side menu         
    sidebarMenu(
      menuItem("Margin of Error", icon=icon("th"),
        menuSubItem("Proportion", tabName = "moe_prop", icon = icon("dot-circle-o")),
        menuSubItem("Mean", tabName = "moe_mean", icon = icon("square-o"))
      ),
      menuItem("Sample Size", icon = icon("calculator"),
               menuSubItem("Proportion", tabName = "sample_prop", icon = icon("dot-circle-o")),
               menuSubItem("Mean", tabName = "sample_mean", icon = icon("square-o"))
               ),
      menuItem("", tabName = "aboutus",
               icon = icon("info"), badgeLabel = "About the App", badgeColor = "orange"),
      menuItem("Emelie", href = "https://ca.linkedin.com/pub/emelie-gustafsson/58/930/647", icon = icon("linkedin")),
      menuItem("Ben", href = "https://ca.linkedin.com/in/beneditochou", icon = icon("linkedin"))
    ),
    
    br(),
    
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
#     br(),
#     
#     Gotta have @('_')@!
#     p(img(src="SnowMonkey.jpg", width="100%"))
    
  ), # dashboardSidebar

# ---- Dashboard Body ----
  dashboardBody(
    
    # Show the custom title
    h2(textOutput("caption", container = span)),
    
    tabItems(
      tabItem(tabName = "moe_prop",
        
        fluidRow(
          valueBoxOutput("moe_prop"),
          conditionalPanel(condition = "input.fpc == true",valueBoxOutput("moe_fpc_prop")),
          valueBoxOutput("n_prop")
        ),
        
        box(width=5, title = "Calculate your Margin of Errors",
          sliderInput("nn", label="Numbers of observations:",
                      min=1, max=1000, value=400, step=10, 
                      animate = animationOptions(loop = FALSE, interval = 300)),
          sliderInput("p", "Proportion:", .00, 1, .50, .05),
          
          checkboxInput("fpc", label = strong("Correct for Sampling Proportion (FPC)"), value = TRUE),
          helpText("Check if you work with a small population"),
          
          conditionalPanel(condition = "input.fpc == true",
                           uiOutput("ui_p_pop"))
        ),
                    
        box(width=7, title="Margin of Errors", solidHeader = TRUE, status = "primary",
                      plotOutput("moe_prop_plot", height = 400, click = "plot_click")
        )
      ), # tabItem "moe_prop"
                
      tabItem(tabName = "moe_mean",
        fluidRow(
          valueBoxOutput("moe_mean"),
          conditionalPanel(condition = "input.fpc2 == true",valueBoxOutput("moe_fpc_mean")),
          valueBoxOutput("moe_std")
        ),
          
        box(width=4, title = "Calculate your Margin of Errors",
            sliderInput("n_mean", label="Numbers of observations:",
                        min=1, max=10000, value=2000, step=100,
                        animate = animationOptions(loop = FALSE, interval = 300)),
            numericInput("mean", label="What is your mean?", value = 100),
            numericInput("std", label="What is your Standard Deviations?", value = 1, step = .1),
            checkboxInput("fpc2", label = strong("Correct for Sampling Proportion (FPC)"),value = TRUE),
            helpText("Check if you work with a small population"),
            conditionalPanel(condition = "input.fpc2 == true",
                             uiOutput("ui_mean_pop"))
        ),
          
        box(width=8, title = "Margin of Errors", solidHeader = TRUE, status = "primary",
          fluidRow(
            box(width=6,
                plotOutput("moe_mean_plot", height=400)),
            box(width=6,
                plotOutput("moe_mean_CI_plot", height=400))
            )
        )
      ), # tabItem "moe_mean"
      
      tabItem(tabName = "sample_prop",
        fluidRow(
          infoBoxOutput("desiredmoe"),
          infoBoxOutput("samplesize"),
          conditionalPanel(condition = "input.fpc3 == true", infoBoxOutput("samplesizefpc"))
        ),
                          
        box(height = 500,
          sliderInput("dmoe", label="What's your desired margin of error?", 
                      min=0.001, max=.20, value=.05, step=.005,
                      animate=animationOptions(interval=300, loop=FALSE)),
          numericInput("rr", label = "What's your anticipated response rate?",
                       value = .5, min=0.01, max=1.00, step=.01),
          
          checkboxInput("fpc3", label = strong("Correct for Sampling Proportion (FPC)"), value = FALSE),
          helpText("Check if you work with a small population"),
          
          conditionalPanel(condition = "input.fpc3 == true",
                           sliderInput("N_s", label = "Population Size:",
                                       min = 0, max = 10000, value = 1000, step = 500)),
          
          selectInput("z", label = "What's your desired level of confidence?",
                      choices = c("99%" = 2.576,
                                  "95%" = 1.96,
                                  "90%" = 1.645,
                                  "80%" = 1.282,
                                  "50%" = 0.674),
                      selected = 1.96)
        ),
        
        box(title="Sample Size", solidHeader = TRUE, status = "primary",
            sliderInput("sample_prop_range", "Select your desired moe range", min = .001, max = .2, 
                        value = c(.01, .05), sep = ","),
            plotOutput("samplesize_prop_plot", height = 400)
        )
      ), # tabItem "sample_prop"
      
      tabItem(tabName = "sample_mean",
              fluidRow(
                infoBoxOutput("desiredmoe_mean"),
                infoBoxOutput("samplesize_mean"),
                conditionalPanel(condition = "input.fpc4 == true", infoBoxOutput("samplesizefpc_mean"))
              ),
              
              box(height = 500,
                  sliderInput("dmoe_mean", label="What's your desired margin of error?", 
                              min=0, max = 10, value=.5, step=.1,
                              animate=animationOptions(interval=300, loop=FALSE)),
                  sliderInput("SD_s", label="What's your standard deviation?", 
                              min=0, max = 100, value=10, step=1,
                              animate=animationOptions(interval=300, loop=FALSE)),
                  numericInput("rr_mean", label = "What's your anticipated response rate?",
                               value = .5, min=0.01, max=1.00, step=.01),
                  
                  checkboxInput("fpc4", label = strong("Correct for Sampling Proportion (FPC)"), value = FALSE),
                  helpText("Check if you work with a small population"),
                  
                  conditionalPanel(condition = "input.fpc4 == true",
                                   sliderInput("N_s_mean", label = "Population Size:",
                                               min = 0, max = 10000, value = 1000, step = 500)),
                  
                  selectInput("z_mean", label = "What's your desired level of confidence?",
                              choices = c("99%" = 2.576,
                                          "95%" = 1.96,
                                          "90%" = 1.645,
                                          "80%" = 1.282,
                                          "50%" = 0.674),
                              selected = 1.96)
              ),
              
              box(title="Sample Size", solidHeader = TRUE, status = "primary",
                  sliderInput("sample_mean_range", "Select your SD range", min = .1, max = 100, 
                              value = c(.01, 10), sep = ","),
                  plotOutput("samplesize_mean_plot", height = 400)
              )
      ), # tabItem "sample_mean"
      
      tabItem(tabName = "aboutus",
              fluidPage(
                column(width = 5,
                       includeMarkdown("www/aboutus.md")
                )
              )
      ) # tabItem "aboutus"
    ) # tabItems
  ) # dashboardBody
) # dashboardPage