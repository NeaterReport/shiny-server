# ui for JAEG BC Liquor

dashboardPage(skin = "purple",

# ---- Dashboard Header ----
  dashboardHeader(title = "Wine and Beer",
   dropdownMenu(type = "messages",
                messageItem(
                  from = "Emelie & Ben",
                  message = "Welcome!",
                  icon = icon("smile-o"),
                  time = Sys.Date()
                ))
  ), # dashboardHeader
  
# ---- Dashboard Sidebar ----
  dashboardSidebar(
    
    # Show current date            
    h3(textOutput("currentDate"), align="center"),
    
    # Create side menu         
    sidebarMenu(
      menuItem("Wine and Beer!", tabName = "drink", 
               icon = icon("glass")),
      menuItem("Liqour Explorer", tabName = "explorer",
               icon = icon("cube"), badgeLabel = "New", badgeColor = "teal"),
      menuItem("", tabName = "aboutus",
               icon = icon("info"), badgeLabel = "About the App", badgeColor = "orange"),
      menuItem("Emelie", href = "https://ca.linkedin.com/pub/emelie-gustafsson/58/930/647", icon = icon("linkedin")),
      menuItem("Ben", href = "https://ca.linkedin.com/in/beneditochou", icon = icon("linkedin"))
#       br(),
#       Gotta have @('_')@!
#       p(img(src="SnowMonkey.jpg", width="100%"))
    )
    
  ),  #dashboardSidebar
  
# ---- Dashboard Body ----
  dashboardBody(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
    includeCSS("www/styles.css"),
    
    # Call the Sortable javascript library on CDN
    # This next line add Sortable.min.js to make sortableR works on server :) Thanks to Andy Kipp
    # see https://groups.google.com/forum/#!topic/shiny-discuss/afbtbhRoofE
    tags$head(tags$script(src="//cdnjs.cloudflare.com/ajax/libs/Sortable/1.2.1/Sortable.min.js")),
    
    tabItems(
      
      tabItem(tabName = "drink",
        tabBox(width = 12,
          title = "Wine and Beer",
          # The id lets us use input$tabset1 on the server to find the current tab
          id = "tabset1",
               
          tabPanel("Sample by Country", icon = icon("map-marker"),
            fluidPage(
              fluidRow(
                selectInput("country", "Pick a Country",
                               choices = sort(unique(bcliquor_df$country)), 
                               selected = "Canada")
              ),
              fluidRow(id = "sortable",
                       box(title = "What do they sell?",
                           plotOutput("country_tmap")),
                       box(title = "Top 10 Price", 
                           plotOutput("country_price_top10")),
                       box(title = "Top 10 Alcohol Content",
                           plotOutput("country_alcohol_top10")),
                       box(title = "Top 10 Sweetness",
                           plotOutput("country_sweet_top10")),
                       box(title = "Price Distribution",
                           plotOutput("country_violin", click = "plot_click")),
                       box(title = "Price for:",
                           selectizeInput("whatclass", NULL,
                                          choices = NULL, selected = NULL),
                           plotOutput("class_violin"))
              ), sortableR("sortable")
            )
          ), # tabPanel "Sample by Country"
          
          tabPanel("Top and Bottom", icon = icon("sort-amount-desc"),
            fluidPage(
              fluidRow(id = "sorty",
                selectizeInput("whatdata", "Which top and bottom 10 data you wanna look?",
                               choices = c("Price", "Alcohol", "Sweetness"),
                               selected = "Price"),
                box(HTML("<img src='wine.png' height = 25> Top 10 Wine"),
                    status = "danger",
                    plotOutput("top10_wine_plot"),
                    collapsible = TRUE),
                box(HTML("<img src='wine.png' height = 25> Bottom 10 Wine"),
                    status = "danger",
                    plotOutput("bottom10_wine_plot"),
                    collapsible = TRUE),
                box(HTML("<img src='http://www.goeuro.com/images/images/BPI/beer-icon-yellow.png' height = 25> Top 10 Beer"),
                    status = "warning",
                    plotOutput("top10_beer_plot"),
                    collapsible = TRUE),
                box(HTML("<img src='http://www.goeuro.com/images/images/BPI/beer-icon-yellow.png' height = 25> Bottom 10 Beer"),
                    status = "warning",
                    plotOutput("bottom10_beer_plot"),
                    collapsible = TRUE),
                box("Top 10 Spirits", 
                    status = "primary",
                    plotOutput("top10_spirits_plot"),
                    collapsible = TRUE),
                box("Bottom 10 Spirits", 
                    status = "primary",
                    plotOutput("bottom10_spirits_plot"),
                    collapsible = TRUE),
                box(HTML("<img src='cocktail.png' height = 25> Top 10 Refreshment"),  
                    status = "success",
                    plotOutput("top10_refresh_plot"),
                    collapsible = TRUE),
                box(HTML("<img src='cocktail.png' height = 25> Bottom 10 Refreshment"), 
                    status = "success",
                    plotOutput("bottom10_refresh_plot"),
                    collapsible = TRUE)
              ), sortableR("sorty")
            )
          ), # tabPanel "Top and Bottom"
          
          tabPanel("Sample by Class", icon = icon("filter"),
            tabsetPanel(
          
              tabPanel("By Class",
                fluidPage(id = "sortme",
                  fluidRow(
                    column(width = 3,
                      selectizeInput("whatclass2", 
                                     "What kind of Liqour you fancy?",
                                     choices = c("Wine","Spirits","Beer",
                                                 "Refreshment Beverage",
                                                 "De-alcoholized Wine",
                                                 "De-alcoholized Beer",
                                                 "Culinary Products"),
                                     selected = "Wine")
                    ),
                    column(width = 3,
                      conditionalPanel(condition = "input.whatclass2 != 'Beer'",
                                       uiOutput("ui_whatclass2")
                    )
                    )
                  ),
                  box(leafletOutput("map_by_class")),
                  box(dataTableOutput("class_country")),
                  box(width = 12,
                      htmlOutput("url")
                  ), sortableR("sortme")
                )
              ), # tabPanel "By Class"
              
              tabPanel("By Country",
                fluidPage(
                  box(width = 5,
                    plotOutput("country_x_class_plot", height = "auto",
                               click = "plot_click")
                  ),
                  box(width = 7,
                    p(HTML('<i class="fa fa-info-circle"></i>'),
                      "The graph is interactive, click on the dimaond to query the BC Liquore Store website."),
                    p("Showing live query of", strong(textOutput("diamond_check", inline = TRUE), 
                                                      style = 'color: orange;'), "."), 
                    htmlOutput("url2")
                  )
                ) # fluidPage
              ) # tabPanel "By Country"
            ) # tabsetPanel
          ), # tabPanel "Sample by Class"
          
          tabPanel("Data", icon = icon("download"),
            fluidPage(
              fluidRow(
                downloadButton('downloadData', 'Download'),
                hr()
              )),
            fluidPage(
              box(width = 12,
                  dataTableOutput("rawdata") 
              )
            )
          ) # tabPanel "Data"
        ) # tabBox
      ), # tabPanel "Drink"
      
      tabItem(tabName = "explorer",
        fluidRow(
          column(9,
            wellPanel(
             fluidRow(
               box(
                 selectInput("xvar", "X-axis variable", 
                             choices = c("Price" = "price",
                                         "Alcohol %" = "alcoholper", 
                                         "Sweetness" = "sweetness"), 
                             selected = "price")
               ),
               box(
                 selectInput("yvar", "Y-axis variable", 
                             choices = c("Price" = "price",
                                         "Alcohol %" = "alcoholper", 
                                         "Sweetness" = "sweetness"), 
                             selected = "alcoholper") 
               )
             )
            ),
            ggvisOutput("plot_liquor_explorer")
          ),
          
          column(3,
            wellPanel(
              h4("Showing me",strong(textOutput("n_liquor", inline = TRUE), 
                                     style='color: orange;'), 
                 "different liquors", style = 'color: #1F77B4;'),
              # uiOutput("ui_price_slider"),
              sliderInput("price_e", "How Expensive?",
                          0, 30000, value = c(0, 30250)),
              sliderInput("alcohol_e", "How Strong?", 
                          0, 100, value = c(0, 100)),
              sliderInput("sweetness_e", "How Sweet?",
                          0, 10, value = c(0, 10)),
              helpText("If you want to only look at liquor with a sweetness rating, uncheck this box"),
              checkboxInput("includesweet", "Sweet or Not?",
                            value = TRUE),
              selectizeInput("country_e", "Pick Your Country of Choice",
                             choices = c(sort(unique(bcliquor_df$country))),
                             selected = "Canada",
                             options = list(placeholder = "Leave blank to see'em all")),
              helpText("Leave blank to see'm all"),
              selectizeInput("class_e", "Class of Liquor",
                             choices = unique(bcliquor_df$class),
                             selected = "Wine",
                             options = list(placeholder = "Leave blank to see'em all")),
              helpText("Leave blank to see'm all"),
              textInput("name_e", "Product Name (e.g., Johnnie Walker - Red Label)")
            )
          )
        ) # fluidRow
      ), # tabItem "Explorer"

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