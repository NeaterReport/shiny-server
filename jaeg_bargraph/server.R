# server for JAEG Bar Graph 2

# ---- Load packages ----
library(DT)
library(plyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(readr)
library(scales)
library(shiny)

# ---- Import data ----

# Import the cleaned lost and found animals data from the City of Vancouver
lostAnimal <- read_csv("www/VancouverLostAnimals.csv")

# Create date variables
lostAnimal <- lostAnimal %>% mutate(Year = factor(year(Date)),
                                    Month = factor(month(Date)),
                                    Day = day(Date),
                                    Wday = wday(Date, label=TRUE))

function(input, output) {
 
# ---- Aggreate and filter data ----
  
 # Summarize the data by the selected variable and state
  datasetInput <- reactive({
    # Add a progress message
    withProgress({
      setProgress(message = "Processing data")
      
      # Use plyr instead of dplyr because it is easier to work with string input
      subdata <- plyr::ddply(lostAnimal, c("State", input$var), plyr::summarise, count=length(State) ,.drop=FALSE) # call the plyr summarize fn using ::
      subdata <- subdata %>% filter(State == input$state) %>%
        mutate(rank = rank(-count)) %>%
        arrange(desc(count))
      if(input$var == "Name") {
        subdata <- filter(subdata, Name!="No Name") # exclude animals with no name
      } else {
        subdata
      }
      
      # Reorder by factor
      subdata[, input$var] <- factor(subdata[, input$var])
      if (input$order==TRUE) {
        subdata[,input$var] <- reorder(subdata[,input$var], subdata[,"count"]) # reorder by count
      }
      return(subdata)
    })
  })
  
# -----  Create data table -----
  
  # Only show the top 10
  output$selectdata <- renderDataTable(head(datasetInput(), 10), 
                                       options = list(searching = FALSE, paging = FALSE))
  
# -----  Make bar graph in ggplot2 -----
  
  output$plot_sg <- renderPlot({
    
    # Add a progress message
    withProgress({
      setProgress(message = "Making a Plot")
      
      mycolour <- ifelse(is.null(input$colour), "grey60", input$colour)# assign user picked co
      
      gg <- ggplot(head(datasetInput(), 10), 
                   aes_string(x = input$var, y = "count")) + 
        geom_bar(stat="identity", fill = mycolour) +
        ylab("Count") + xlab(input$var)
      
      # Customize the label and orientation depending on the type of var
      if(input$var %in% c("Year", "Month", "Day", "Wday") & input$label == TRUE) {
        gg <- gg +
          geom_text(aes(label = comma(count)), color = "darkgrey", vjust = -0.1)
      } else if(input$var %in% c("Name","Color","Breed") & input$label == TRUE) {
        gg <- gg + 
          geom_text(aes(label = comma(count)), color = "white", hjust = 1.2) +
          coord_flip()
      } else if (input$var %in% c("Name","Color","Breed") & input$label == FALSE) {
        gg <- gg + coord_flip()
      }
      
      # Branch to choose graph style
      if(input$graphstyle == 1) {gg + theme_classic()}
      else if (input$graphstyle == 2) {gg + theme_economist() + scale_fill_economist()}
      else if (input$graphstyle == 3) {gg + theme_excel() + scale_fill_excel()}
      else if (input$graphstyle == 4) {gg + theme_few() + scale_fill_few()}
      else if (input$graphstyle == 5) {gg + theme_fivethirtyeight() + 
          scale_fill_fivethirtyeight()}        
      else if (input$graphstyle == 6) {gg + theme_stata() + scale_fill_stata()}
      else if (input$graphstyle == 7) {gg + theme_fivethirtyeight() + 
          scale_fill_tableau("colorblind10")}
      else if (input$graphstyle == 8) {gg + theme_tufte()}
      
    })
  })
  
  #----  Make multi-group bar graph in ggplot2 ----
  
  # see https://groups.google.com/forum/#!topic/shiny-discuss/Fgpd0LoZ-y8
  # Adjust plot height using reactive fn
  plotheight<- reactive({
    if (input$varfacet!="") {plotheight = 1200} 
    else {plotheight = 400} # don't use auto, it makes quantum plot! not sure why >.<
  })
  
  output$plot_mg <- renderPlot({
    # Add a progress message
    withProgress({
      setProgress(message = "Making a Plot")
      # Aggregate the data differently if facet
      if (input$varfacet != "") {
        data_mg <- plyr::ddply(lostAnimal, c(input$varx, input$varfill, input$varfacet), plyr::summarise, count=length(State), .drop=FALSE) %>%
          mutate(rank = rank(-count)) %>%
          arrange(desc(count))
        if (input$varx == "Sex" | input$varfill == "Sex") {
          data_mg <- filter(data_mg, Sex != "X" & Sex != "")  # exclude animals with unknown sex
        } else {data_mg}
      } else {
        data_mg <- plyr::ddply(lostAnimal, c(input$varx, input$varfill), plyr::summarise, count=length(State), .drop=FALSE) %>%
          mutate(rank = rank(-count)) %>%
          arrange(desc(count))
        if (input$varx == "Sex" | input$varfill == "Sex") {
          data_mg <- filter(data_mg, Sex != "X" & Sex != "")  # exclude animals with unknown sex
        } else {data_mg}
      }
      
      gg <- ggplot(data_mg, aes_string(x = input$varx, y = "count", fill = input$varfill)) + 
        geom_bar(stat="identity", position=input$type) +
        ylab("Count") + xlab(input$varx)
      
      # Add facet
      # Use as.formula so that it will work with string var input
      if (input$varfacet!="") {gg <- gg + facet_grid(as.formula(paste(input$varfacet,"~.")))}
      
      # Branch to choose graph style
      if(input$graphstyle == 1) {gg + theme_classic()}
      else if (input$graphstyle == 2) {gg + theme_economist() + scale_fill_economist()}
      else if (input$graphstyle == 3) {gg + theme_excel() + scale_fill_excel()}
      else if (input$graphstyle == 4) {gg + theme_few() + scale_fill_few()}
      else if (input$graphstyle == 5) {gg + theme_fivethirtyeight() + 
          scale_fill_fivethirtyeight() + theme(strip.background = element_rect(fill="#2CA02C"),
                                               strip.text = element_text(color="white"))}
      else if (input$graphstyle == 6) {gg + theme_stata() + scale_fill_stata()}
      else if (input$graphstyle == 7) {gg + theme_fivethirtyeight() + 
          scale_fill_tableau("colorblind10") + theme(strip.background = element_rect(fill="#FFBC79"))}
      else if (input$graphstyle == 8) {gg + theme_tufte()}
    })
  }, height = plotheight) # adjust plot height using the reactive fn defined earlier
  
}