# ui for JAEG Wait Time

dashboardPage(skin = "purple",
              
# ---- DashBoard Header ----
  dashboardHeader(title = "JAEG Wait Time",
                  dropdownMenu(type = "messages",
                               messageItem(
                                 from = "Emelie & Ben",
                                 message = "Welcome!",
                                 icon = icon("smile-o"),
                                 time = Sys.Date()
                               ))
  ), # dashboardHeader

# ---- DashBoard Sidebar ----
  dashboardSidebar(
    
    sidebarMenu(
      menuItem("by Province", tabName = "province", 
               icon = icon("clock-o")),
      menuItem("National", tabName = "national",
               icon = icon("cube"), badgeLabel = "New", badgeColor = "teal"),
      menuItem("", tabName = "aboutus",
               icon = icon("info"), badgeLabel = "Gotta luv @('_')@!", badgeColor = "orange"),
      menuItem("Emelie", href = "https://ca.linkedin.com/pub/emelie-gustafsson/58/930/647", icon = icon("linkedin")),
      menuItem("Ben", href = "https://ca.linkedin.com/in/beneditochou", icon = icon("linkedin")),
      br(),
      radioButtons('format', 'Download your customized report', c('PDF', 'HTML', 'Word'),
                   inline = TRUE),
      downloadButton('downloadReport', class = "dlButton")
#       br(),br(),
#       Gotta have @('_')@!
#       p(img(src = "SnowMonkey.jpg", width="100%"))
    )    
  ), # dashboardSidebar

# ---- DashBoard Body ----
  dashboardBody(
    includeCSS("www/styles.css"),
    
    tabItems(
      tabItem(tabName = "province",
              
        headerPanel(h3("CIHI Wait Time for ", 
                   span(textOutput("trt_selected", inline=TRUE), 
                        style = "color: #2CA02C; font-weight: bold;"),
                   " in ", 
                   span(textOutput("prov_selected", inline=TRUE), 
                        style = "color: #FF7F0E; font-weight: bold;"),
                   imageOutput("image_flag", height=50)
                   ,align="center")),
        
        fluidPage(
          wellPanel(
            fluidRow(
              column(width = 6,
                infoBoxOutput("box_trt_rank", width = 4),
                infoBoxOutput("box_trt_per90", width = 4),
                infoBoxOutput("box_trt_per50", width = 4)
              ),
              column(width = 2,
                selectInput("ui_prov", "Select a Province",
                            choices = c("B.C.", "Alta.", "Sask.", "Man.", 
                                        "Ont.", "Que.", "N.B.", "P.E.I.", "N.S.", "N.L."),
                            selected = "B.C.")
              ),
              column(width = 2,
                selectInput("ui_trt", "Select a Treatment",
                            choices = unique(waitTime_prov_df$Treatment),
                            selected = "Hip Replacement")
              ),
              column(width = 2,
                 sliderInput("ui_year", "Select a Year", 
                             min =  2008, max = 2014, value = 2014, step = 1)
              )
            ) # fluidRow
          ), # wellPanel
          
          fluidRow(
            box(width = 12, plotOutput("thinLine_graph", height = 100)),
            
            tabBox(title = tagList(shiny::icon("th-large"), "Health Region (2014 only)"),
                   side = "left", height = "auto",
                   selected = "Graph",
                   tabPanel("Graph", plotOutput("bullet_graph", height = "auto")),
                   tabPanel("Table", 
                      fluidPage(
                        fluidRow(DT::dataTableOutput("region_data"))
                      )
                   )
            ),
            
            box(plotOutput("area_graph", height = "auto"))
          )
        ) # fluidPage
      ), # tabItem "province"
      
      tabItem(tabName = "national",
              
        fluidPage(
          fluidRow(
            column(width = 8,
                   h3(span(textOutput("trt_selected2", inline=TRUE), 
                           style = "color: #FF7F0E; font-weight: bold;"), " Coming Soon!")
            ),
            column(width = 4,
                   wellPanel(
                     selectInput("ui_trt2", "Select a Treatment",
                                 choices = unique(waitTime_prov_df$Treatment),
                                 selected = "Hip Replacement")
              )
            )
          ), # fluidRow

          fluidRow(
            tabBox(title = tagList(shiny::icon("flag"), "National Perspective"),
                   side = "left", height = "auto", width = 12,
                   selected = "Comparison Graph (Time)",
                
                tabPanel("Comparison Graph (Time)", 
                   fluidPage(
                     fluidRow(plotOutput("compare_graph_yr", height = 800))
                   )
                ),
                
                tabPanel("Comparison Graph (Trt)", 
                  fluidPage(
                    fluidRow(
                     column(offset = 8, width = 4,
                        sliderInput("ui_year2", "Select a Year", 
                                    min =  2008, max = 2014, value = 2014, step = 1, 
                                    animate = animationOptions(interval = 2000, 
                                                               loop = FALSE, playButton = NULL,
                                                               pauseButton = NULL))
                     )
                    ),
                    fluidRow(plotOutput("compare_graph_trt", height = 800))
                  )
                ),
                
                tabPanel("Trend Graph",
                         fluidPage(
                           fluidRow(
                             box(title = "Met Benchmark", 
                                 plotOutput("trend_mb_graph"), solidHeader = TRUE),
                             box(title = "90th Percentile (Day)", 
                                 plotOutput("trend_90_graph"), solidHeader = TRUE),
                             box(title = "50th Percentile (Day)", 
                                 plotOutput("trend_50_graph"), solidHeader = TRUE) 
                           )
                         )
                ),
                
                tabPanel("Map", 
                  fluidPage(
                     fluidRow(plotOutput("map", height = 800))
                  )
                )
            ) # tabBox
          ) # fluidRow
        ) # fluidPage
      ), # tabItem "national"
      
      tabItem(tabName = "aboutus",
        fluidPage(
          column(width = 5, offset = 1,
                 includeMarkdown("www/aboutus.md")
          )
        )
      ) # tabItem "aboutus"
    ) # tabItems
  ) # dashboardBody
) # dashboardPage