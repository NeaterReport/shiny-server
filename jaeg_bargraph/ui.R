# ui for JAEG Bar Graph 2

# ---- Need More Emelie Solution! JAEG ----
# Issue:
# 1 - Ran out of colour palette for some themes for the multiple bar graphs

# ---- Load some packages here ---

library(shinydashboard)
# library(shinyjs) lets not use any js for now
library(rmarkdown)

dashboardPage(skin="purple",
  
# ---- Dashboard Header ----
  dashboardHeader(
    title = "JAEG Bar Graph",
    
    # Greetings :)
    dropdownMenu(type = "messages",
                 messageItem(
                   from = "Emelie & Ben",
                   message = "Welcome!",
                   icon = icon("smile-o"),
                   time = Sys.Date()
                 )
    )
    
  ), # dashboardHeader
 
# ---- Dashboard Sidebar ----
  dashboardSidebar(
    sidebarMenu(
      menuItem("Bar Graph", tabName = "bargraph", icon = icon("bar-chart")),
      menuItem("", tabName = "aboutus", icon = icon("info"), 
               badgeLabel = "Gotta luv @('_')@!", badgeColor = "orange"),
      menuItem("Emelie", 
               href = "https://ca.linkedin.com/pub/emelie-gustafsson/58/930/647", icon = icon("linkedin")),
      menuItem("Ben",
               href = "https://ca.linkedin.com/in/beneditochou", icon = icon("linkedin"))
    ),
    
    # Add selector for graph style from ggthemes
    selectInput("graphstyle", label = "Roll your style!",
                choices = c("Classic" = 1,
                            "Economist" = 2,
                            "Excel" = 3,
                            "Few" = 4,
                            "FiveThirtyEight" = 5,
                            "Stata" = 6,
                            "Tableau" = 7,
                            "Tufte" = 8),
                selected = 7),br(),
    
    # Gotta have @('_')@!
    p(img(src="SnowMonkey.jpg", width="100%"))
    
  ), # dashboardSidebar

# ---- Dashboard Body ----
  dashboardBody(
    # useShinyjs(), # call Shinyjs
    
    tabItems(
      tabItem(tabName = "bargraph",
        fluidPage(
          tabsetPanel(
            tabPanel("Single Group",
              wellPanel(
                fluidRow(
                  column(4,
                    selectInput("var", label = "Choose a variable",
                                choices = c("Breed",
                                            "Colour" = "Color",
                                            "Name",
                                            "Year",
                                            "Month",
                                            "Weekday" = "Wday"),
                                selected = "Year")
                  ),
                  column(4,
                         selectInput("state", label = "Choose a state",
                                     choices = c("Matched",
                                                 "Lost",
                                                 "Unknown"),
                                     selected = "Matched"
                         )
                  ),
                  column(4,
                         p(strong("Accessorize Me!")),
#                          div(class = "output", "Colour me!"),
#                          colourInput("colour", NULL, "grey60"), # colour selector from shinyjs!
                         checkboxInput("label", label = "Label me!", value=FALSE),
                         checkboxInput("order", label = "Order me!", value=FALSE),
                         checkboxInput("seedata", label = "Show me your data!", value=FALSE)
                  )
                ) # wellPanel
              ),
              fluidRow(
                       box(title = "Top 10 Lost and Found", width = 12,
                           solidHeader = TRUE, status = "warning", collapsible = TRUE,
                           plotOutput("plot_sg"), height="auto"),
                       box(title = "Data Table", width = 12, 
                           solidHeader = TRUE, status = "primary", collapsible = TRUE,
                           conditionalPanel(condition = "input.seedata == true",
                                            wellPanel(
                                              DT::dataTableOutput("selectdata")
                                            )
                           )
                       )
              )
            ) # tabPanel "Single Group"
            
#             tabPanel("Multiple Group",
#               wellPanel(
#                 fluidRow(
#                   column(width=6,
#                          selectInput("varx", label = "Choose a first group",
#                                      choices = c("Sex",
#                                                  "Year",
#                                                  "Month",
#                                                  "Weekday" = "Wday"),
#                                      selected = "Wday"),
#                          selectInput("varfill", label = "Choose a second group",
#                                      choices = c("Sex",
#                                                  "Year",
#                                                  "Month",
#                                                  "Weekday" = "Wday"),
#                                      selected = "Sex")
#                   ),
#                   column(width=6,
#                          selectizeInput("type", label = "Choose a type",
#                                         choices = c("Dodge" = "dodge",
#                                                     "Stack" = "stack",
#                                                     "Stack Fill" = "fill"),
#                                         selected = "dodge"),
#                          selectizeInput("varfacet", label = "Choose a facet",
#                                         choices = c("",
#                                                     "Sex",
#                                                     "Year",
#                                                     "Month",
#                                                     "Weekday" = "Wday"),
#                                         selected = NULL)
#                   )
#                 ) 
#               ), # wellPanel
#               fluidRow(
#                 plotOutput("plot_mg", height = "auto")
#               )
#             ) # tabPanel "Multiple Group"
          ) # tabsetPanel 
        ) # fluidpage
      ), # tabItem "bargraph"
      
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