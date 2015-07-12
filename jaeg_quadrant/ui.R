# ui for JAEG Quadrant Plot

fluidPage(theme = shinytheme("united"),
          includeCSS("www/styles.css"),
          useShinyjs(), # for field reset
  
  titlePanel(h2("Quandrant Analysis - Beta", align = "centre")),
  
  sidebarLayout(
    
# ---- SidebarPanel ---- 
    sidebarPanel(width = 2,
                 
      radioButtons("datatype", "Choose your data:",
                   choices = c("Pos Rating",
                               "Rating",
                               "Random",
                               "Excel 2007",
                               "CSV"),
                   selected = "Pos Rating"),
                 
      # Create custom input for each data type
      conditionalPanel(condition = "input.datatype == 'Rating' || 
                                  input.datatype == 'Pos Rating' || 
                                  input.datatype == 'Random'",
                       sliderInput("no_of_v", "Select the number of variable",
                                   min = 1, max = 100, value = 20, step = 5)),
      conditionalPanel(condition = "input.datatype == 'Rating' ||
                                  input.datatype == 'Pos Rating'",
                       sliderInput("no_of_rating", "Select the number of rating",
                                   min = 2, max = 10, value = 5, step = 1)),
      conditionalPanel(condition = "input.datatype == 'Excel 2007' ||
                                  input.datatype == 'CSV'",
                       fileInput("importfile", "Import your own data")),
      
      selectizeInput('fieldname', choices = NULL, label = "Pick your variables",
                     multiple = TRUE,
                     options = list(placeholder = "Pick your variables")),
      actionButton("getdata", "Get Data", icon = icon("spinner fa-spin"), width = "100%", 
                   style = "background-color: #2F5777;"),br(),br(),
      actionButton("reset", "Reset Variable", icon = icon("undo"), width = "100%", 
                   style = "background-color: #2F5777;"),
      helpText("Please reset your variable before getting new data!")
                 
    ), # sideBarPanel
    
# ---- MainPanel ---- 
    mainPanel(width=10,
    
      tabsetPanel(
        
        tabPanel("Quadrant Plot",
          fluidPage(
            fluidRow(
              br(),
              box(title = "What is a Quandrant Plot?", width = 4,
                p("Conduct quadrant analysis with your data or a randomly 
generated dataset. Quandrant analysis is commonly used in market research to identify priority areas using two criteria, one measuring importance and another capturing performance."),
                p("In this example, importance is measured by the strength of the correlation coefficients and performance by the mean scores. The cut-off is set as the median correlation and mean scores. In actual applications, different criteria and cut-off can be used to slice the data."),
                p( HTML('<i class="fa fa-info-circle"></i>'),"The plot is interactive. Click on a point and it will show an histogram of the variable and its correlation with the last variable in the data (assumed to be the outcome). Double clicking on the graph allow you to dynamically set the cut-off points.")
                ),
              box(width = 8,
                  br(),
                plotOutput("plot_quandrant",
                           click = "plot_click",
                           hover = "plot_hover",
                           dblclick = "plot_dblclick"),br(),
              box(verbatimTextOutput("info")), br()
              )
            ),
            fluidRow(
              box(plotOutput("info_histogram")),
              box(plotOutput("info_scatter"))
            ),
            fluidRow(
              dataTableOutput("info_data")
            )
          )
        ),
        
        tabPanel("Data Plot",
                 br(),
                 flowLayout(uiOutput("plots"))
        ),
        
        tabPanel("Data Table",
          fluidPage(
              br(),
              box(width = 9, dataTableOutput("datatable")) 
          )
        ),
        
        tabPanel("About @('_')@", icon = icon("info-circle"),
          fluidPage(
            column(width = 5,
                   includeMarkdown("www/aboutus.md")
            )
          )
        )
        
      ) # TabsetPanel
    ) # MainPanel
  ) # sideBarLayout
) # fluidPage