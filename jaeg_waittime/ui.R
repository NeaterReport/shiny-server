# ui for JAEG Wait Time

dashboardPage(skin = "purple",
              
# ---- DashBoard Header ----
  dashboardHeader(title = "CIHI Wait Time",
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
               icon = icon("info"), badgeLabel = "About the App", badgeColor = "orange"),
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
    
    # Call the Sortable javascript library on CDN
    # This next line add Sortable.min.js to make sortableR works on server :) Thanks to Andy Kipp
    # see https://groups.google.com/forum/#!topic/shiny-discuss/afbtbhRoofE
    tags$head(tags$script(src="//cdnjs.cloudflare.com/ajax/libs/Sortable/1.2.1/Sortable.min.js")),
    
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
              column(width = 6, id = "sortableInfo",
                infoBoxOutput("box_trt_rank", width = 4),
                infoBoxOutput("box_trt_per90", width = 4),
                infoBoxOutput("box_trt_per50", width = 4)
              ), sortableR("sortableInfo"),
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
              ),
              column(width = 12,
                p("The Canadian Institute of Health Information (CIHI) wait time data is the most comprehensive data on wait time for selected priority procedures and treatments across Canada. For selected provinces, information is avaliable at the health region level to examine regional differences.")
              )
            ) # fluidRow
          ), # wellPanel
          
          
          fluidRow(id = "sortable",
                   
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
          ), sortableR("sortable")
        ) # fluidPage
      ), # tabItem "province"
      
      tabItem(tabName = "national",
              
        fluidPage(
          fluidRow(
            column(width = 8,
                   h3(span(textOutput("trt_selected2", inline=TRUE), 
                           style = "color: #FF7F0E; font-weight: bold;"), " Coming Soon!"),
                   p("Each year, The Canadian Institute of Health Information (CIHI) releases wait time data on selected priority procedures and treatments. The Institute compiles this and other information to inform the general public on important health matters and shape public health policy.")
                   
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
                   selected = "Compare Wait Time Overtime",
                
                tabPanel("Compare Wait Time Overtime", 
                   fluidPage(
                     fluidRow(plotOutput("compare_graph_yr", height = 800))
                   )
                ),
                
                tabPanel("Compare Wait Time by Treatment", 
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
                
                tabPanel("Overall Trend",
                         fluidPage(
                           fluidRow(id = "sortable2",
                             box(title = "Met Benchmark", 
                                 plotOutput("trend_mb_graph"), solidHeader = TRUE),
                             box(title = "90th Percentile (Day)", 
                                 plotOutput("trend_90_graph"), solidHeader = TRUE),
                             box(title = "50th Percentile (Day)", 
                                 plotOutput("trend_50_graph"), solidHeader = TRUE) 
                           ), sortableR("sortable2")
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