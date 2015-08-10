# ui for JAEG MoE

# ---- Need More Emelie Solution! JAEG ----

# To Do:
# 1 - Verify formula
# 2 - Check confidence interval formula for small sample (t distribution?)
# 3 - Check sample size for mean, seem okay
# see http://www.select-statistics.co.uk/sample-size-calculator-mean
# 4 - Pick more sensible default values for graphs
# 5 - Add design effect

# ---- Load library ----

library(dplyr)
library(ggplot2)
library(ggthemes)
require(markdown)
require(rmarkdown)
library(scales)
library(shiny)
library(shinydashboard)
library(shinyBS)

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
       
    h1(), # add some space

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
    selectInput("graphstyle", label = "Roll your graphic style!",
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
    
#     Show the custom title
#     h2(textOutput("caption", container = span)),
    
    tabItems(
      tabItem(tabName = "moe_prop",
        
        fluidRow(
          valueBoxOutput("moe_prop"),
          conditionalPanel(condition = "input.fpc == true",valueBoxOutput("moe_fpc_prop")),
          valueBoxOutput("n_prop")
        ),
        
        column(width = 4,
            
            h3("Calculate your Margin of Error"),
               
            p("Enter the number of", strong("observations"), "and ", strong("proportion"), ". If you work with a small population and know your population size, adjust the effect of your sample proportion on the margin of error by checking the ", strong("correct for sampling proportion")," box."),
            
          numericInput("nn", "Number of observations", value = 400, min = 1, max = 10000),
          numericInput("p", "Proportion", value = .50, min = .01, max = 1, step = .01),
          
          checkboxInput("fpc", label = strong("Correct for Sampling Proportion (FPC)"), value = FALSE),
          helpText("Check if you work with a small population."),
          
          conditionalPanel(condition = "input.fpc == true",
                           p("Enter the population size."),
                           uiOutput("ui_p_pop"))
        ),
                    
        box(width = 8, title="Margin of Error", solidHeader = TRUE, status = "primary",
                      plotOutput("moe_prop_plot", height = 400, click = "plot_click")
        )
      ), # tabItem "moe_prop"
                
      tabItem(tabName = "moe_mean",
        fluidRow(
          valueBoxOutput("moe_mean"),
          conditionalPanel(condition = "input.fpc2 == true",valueBoxOutput("moe_fpc_mean")),
          valueBoxOutput("moe_std")
        ),
          
       column(width=4,
            
            h3("Calculate your Margin of Error"),
            
            p("Enter the number of", strong("observations")," ,", strong("mean"), "and ", strong("standard deviation"), ". If you work with a small population and know your population size, adjust the effect of your sample proportion on the margin of error by checking the ", strong("correct for sampling proportion")," box."),
            
            numericInput("n_mean", "Number of observations", value = 400, min = 1, max = 10000),
            numericInput("mean", label="Mean", value = 100),
            numericInput("std", label="Standard deviation?", value = 1, step = .1),
            checkboxInput("fpc2", label = strong("Correct for Sampling Proportion (FPC)"),value = FALSE),
            helpText("Check if you work with a small population."),
            conditionalPanel(condition = "input.fpc2 == true",
                             p("Enter the population size."),
                             uiOutput("ui_mean_pop"))
        ),
          
        box(width=8, title = "Margin of Error", solidHeader = TRUE, status = "primary",
          fluidRow(
            column(width=6,
                plotOutput("moe_mean_plot", height=400)),
            column(width=6,
                plotOutput("moe_mean_CI_plot", height=400))
            )
        )
      ), # tabItem "moe_mean"
      
      tabItem(tabName = "sample_prop",
        fluidRow(
          infoBoxOutput("samplesize"),
          conditionalPanel(condition = "input.fpc3 == true", infoBoxOutput("samplesizefpc")),
          infoBoxOutput("desiredmoe")
        ),
        
        column(width=4,
               
          h3("Calculate your Sample Size"),
               
          p("Enter your", strong("desired margin of error"), " ,", strong("level of confidence"), " and ", strong("anticipated response rate"), ". If you work with a small population and know your population size, adjust the effect of your sample proportion on the margin of error by checking the ", strong("correct for sampling proportion")," box."),
               
          numericInput("dmoe", label = "What's your desired margin of error?",
                       value = .05, min=0.001, max=1.00, step=.01),
          selectInput("z", label = "What's your desired level of confidence?",
                      choices = c("99%" = 2.576,
                                  "95%" = 1.96,
                                  "90%" = 1.645,
                                  "80%" = 1.282,
                                  "50%" = 0.674),
                      selected = 1.96),
          numericInput("rr", label = "What's your anticipated response rate?",
                       value = .5, min=0.01, max=1.00, step=.01),
          
          checkboxInput("fpc3", label = strong("Correct for Sampling Proportion (FPC)"), value = FALSE),
          helpText("Check if you work with a small population."),
          
          conditionalPanel(condition = "input.fpc3 == true",
                           p("Enter the population size."),
                           numericInput("N_s", label = "Population Size",
                                        value = 1000, min = 0, max = 10000, step = 500))
        ),
        
        box(width = 8, title="How sample size relates to margin of error", solidHeader = TRUE, status = "primary",
            sliderInput("sample_prop_range", "Select your desired moe range", min = .001, max = .2, 
                        value = c(.01, .05), sep = ",", animate = TRUE, ticks = FALSE),
            plotOutput("samplesize_prop_plot", height = 400)
        )
      ), # tabItem "sample_prop"
      
      tabItem(tabName = "sample_mean",
              fluidRow(
                infoBoxOutput("samplesize_mean"),
                conditionalPanel(condition = "input.fpc4 == true", infoBoxOutput("samplesizefpc_mean")),
                infoBoxOutput("desiredmoe_mean")
              ),
              
              column(width=4,
                     
                     h3("Calculate your Sample Size"),
                     
                     p("Enter your", strong("desired margin of error"), " ,", strong("level of confidence"), " and ", strong("anticipated response rate"), ". If you work with a small population and know your population size, adjust the effect of your sample proportion on the margin of error by checking the ", strong("correct for sampling proportion")," box."),
              
                  numericInput("dmoe_mean", label = "What's your desired margin of error?",
                               value = 1, min=0.001, max=1.00, step=.01),
                  numericInput("SD_s", label = "What's your standard deviation?",
                               value = 10, min=0, max=1000),
                  selectInput("z_mean", label = "What's your desired level of confidence?",
                              choices = c("99%" = 2.576,
                                          "95%" = 1.96,
                                          "90%" = 1.645,
                                          "80%" = 1.282,
                                          "50%" = 0.674),
                              selected = 1.96),
                  numericInput("rr_mean", label = "What's your anticipated response rate?",
                               value = .5, min=0.01, max=1.00, step=.01),
                  
                  checkboxInput("fpc4", label = strong("Correct for Sampling Proportion (FPC)"), value = FALSE),
                  helpText("Check if you work with a small population."),
                  
                  conditionalPanel(condition = "input.fpc4 == true",
                                   p("Enter the population size."),
                                   numericInput("N_s_mean", label = "Population Size",
                                                value = 1000, min = 0, max = 10000, step = 500))
              ),
              
              box(width = 8,
                  title="How sample size relates to margin of error", solidHeader = TRUE, status = "primary",
                  sliderInput("sample_mean_range", "Select your SD range", min = .01, max = 100, 
                              value = c(.01, 5), sep = ",", animate = TRUE, ticks = FALSE),
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