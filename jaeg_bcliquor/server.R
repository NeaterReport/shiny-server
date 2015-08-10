# server for JAEG BC Liquor

function(input, output, session) {
  
# ---- Sample by Country section ----
  
  # Create Country dataset
  by_country_df <- reactive({
    filter(bcliquor_df, country == input$country)
  })
  
   top_x_class_price_df <- reactive({
    by_country_df() %>% 
      group_by(class) %>%
      top_n(n = 10, wt = price) %>% arrange(class, -price)
  })
   
   top_x_class_alcohol_df <- reactive({
     by_country_df() %>% 
       group_by(class) %>%
       top_n(n = 10, wt = alcoholper) %>% arrange(class, -alcoholper)
   })
   
   top_x_class_sweet_df <- reactive({
     by_country_df() %>% 
       group_by(class) %>%
       top_n(n = 10, wt = sweetness) %>% arrange(class, -sweetness)
   })
   
   # Tree Map
   
   by_country_tmap_df <- reactive({
     by_country_tmap_df <- by_country_df() %>% group_by(class) %>%
       dplyr::summarize(count = n(),
                        price = mean(price, na.rm = TRUE)) %>% 
       mutate(pct = (100 * count) / sum(count))
     by_country_tmap_df$count2 <- paste0(by_country_tmap_df$count,"\n", 
                                         round(by_country_tmap_df$pct,1), "%")
     by_country_tmap_df$class <- factor(by_country_tmap_df$class)
     by_country_tmap_df$count2 <- factor(by_country_tmap_df$count2)
     return(by_country_tmap_df)
   })
   
   output$country_tmap <- renderPlot({
     treemapify(by_country_tmap_df(), area = "count", fill = "class", label = "count2") %>% 
       ggtify() + theme(legend.position = "top", legend.title = element_blank())
   })
   
   # Create Class Dataset
   by_class_df <- reactive({
     filter(bcliquor_df, class == input$whatclass2)
   })
   
  # Top 10 price graph
  output$country_price_top10 <- renderPlot({
    data <- arrange(top_x_class_price_df(), -price)[1:10,] # show first 10 only
    ggplot(data, aes(price, reorder(name, price), colour = class)) + 
      geom_point(size = 6) +
      geom_text(aes(label = paste0("$",price)), vjust = 2.5, size = rel(3)) +
      scale_x_continuous(labels = dollar) +
      scale_y_discrete(labels = function(x) str_pad(str_wrap(x, width = 30),side = "left",30)) + 
      ylab("Product Name") + xlab("Price ($)") + theme_jaeg +  theme(legend.position = "bottom",
                                                 panel.grid.major.y = element_blank(),
                                                 legend.title = element_blank())
  })
  
  # Top 10 alcohol percent graph
  output$country_alcohol_top10 <- renderPlot({
    data <- arrange(top_x_class_alcohol_df(), -price)[1:10,] # show first 10 only
    ggplot(data, aes(alcoholper, reorder(name, alcoholper), colour = class)) + 
      geom_point(size = 6) +
      geom_text(aes(label = paste0("$",price)), vjust = 2.5, size = rel(3)) +
      scale_x_continuous(labels = percent) +
      scale_y_discrete(labels = function(x) str_pad(str_wrap(x, width = 30),side = "left",30)) + 
      ylab("Product Name") + xlab("Alcohol %") + theme_jaeg +  theme(legend.position = "bottom",
                                                  panel.grid.major.y = element_blank(),
                                                  legend.title = element_blank())
  })
  
  # Top 10 sweetness
  output$country_sweet_top10 <- renderPlot({
    data <- arrange(top_x_class_sweet_df(), -price)[1:10,]  # show first 10 only
    ggplot(data, aes(sweetness, reorder(name, sweetness), colour = class)) + 
      geom_point(size = 6) +
      geom_text(aes(label = paste0("$",price)), vjust = 2.5, size = rel(3)) +
      scale_y_discrete(labels = function(x) str_pad(str_wrap(x, width = 30),side = "left",30)) + 
      ylab("Product Name") + xlab("Sweetness") + theme_jaeg +  theme(legend.position = "bottom",
                                                  panel.grid.major.y = element_blank(),
                                                  legend.title = element_blank())
  })
  
  # Violin Plot
  output$country_violin <- renderPlot({
    ggplot(by_country_df(), aes(class, price, fill = class)) + 
      geom_violin(scale = "width", colour = NA) + theme_jaeg + 
      theme(legend.position = "bottom",
            panel.grid.major.x = element_blank())
  })
  
  # Update input
  observe({
    updateSelectizeInput(session, inputId = "whatclass", 
                         choices = unique(by_country_df()$class), 
                         selected = unique(by_country_df()$class)[1], server = TRUE)
  })

  
  # Individual Violin Plot
  output$class_violin <- renderPlot({
    subdata <- filter(by_country_df(), class == input$whatclass)
    ggplot(subdata, aes(x = price)) + geom_bar(fill = "#00B0F6", colour = NA) +
      scale_x_continuous(labels = dollar) +
      theme_jaeg 
  })
  
# ---- Top and Bottom Section ----- 
  
# Top 10 table
  
  top_x_class_price_all_df <- reactive({
    bcliquor_df %>% 
      group_by(class) %>%
      top_n(n = 10, wt = price) %>% arrange(class, -price)
  })
  
  top_x_class_alcohol_all_df <- reactive({
    bcliquor_df %>% 
      group_by(class) %>%
      top_n(n = 10, wt = alcoholper) %>% arrange(class, -alcoholper)
  })
  
  top_x_class_sweet_all_df <- reactive({
    bcliquor_df %>% 
      group_by(class) %>%
      top_n(n = 10, wt = sweetness) %>% arrange(class, -sweetness)
  })
  
  datasetInput_top <- reactive({
    switch(input$whatdata,
           "Price" = top_x_class_price_all_df(),
           "Alcohol" = top_x_class_alcohol_all_df(),
           "Sweetness" = top_x_class_sweet_all_df())
  })
  
  # Not use atm
  output$top10_tbldata <- renderDataTable({
    datatable(datasetInput_top(), filter = "bottom", options = list(pageLength = 10))
  })
  
  # Top 10 wine
  output$top10_wine_plot <- renderPlot({
    subdata <- filter(datasetInput_top(), class == "Wine")
    if(input$whatdata == "Sweetness") {
      # Need to subset the first 10 here because of tie
      subdata <- subdata[1:10,]
    ggplot(subdata, aes(sweetness, reorder(name, sweetness), 
                        size = alcoholper*100, colour = country)) + geom_point() +
      scale_size(range = c(5, 7)) + 
      scale_y_discrete(labels = function(x) str_pad(str_wrap(x, width = 30),side = "left",30)) +
      xlab("Sweetness") + ylab("Product Name") +
      theme_jaeg + theme(panel.grid.major.y = element_blank()) + 
      labs(colour = "", size="Alcohol %")
    } else if (input$whatdata == "Price") {
      ggplot(subdata, aes(price, reorder(name, price), 
                          size = alcoholper*100, colour = country)) + geom_point() +
        scale_size(range = c(5, 7)) + 
        scale_y_discrete(labels = function(x) str_pad(str_wrap(x, width = 30),side = "left",30)) +
        xlab("Price") + ylab("Product Name") +
        scale_x_continuous(labels = dollar) +
        theme_jaeg + theme(panel.grid.major.y = element_blank()) +
        labs(colour = "", size="Alcohol %")
    } else if (input$whatdata == "Alcohol") {
      ggplot(subdata, aes(alcoholper, reorder(name, alcoholper), 
                          size = price, colour = country)) + 
        geom_point() + xlab("Alcohol %") + ylab("Product Name") +
        scale_size(range = c(5, 7)) + 
        scale_y_discrete(labels = function(x) str_pad(str_wrap(x, width = 30),side = "left",30)) +
        scale_x_continuous(labels = percent) +
        theme_jaeg + theme(panel.grid.major.y = element_blank()) + 
        labs(colour = "", size="price ($)")
    }
  })
  
  # Top 10 Beer
  output$top10_beer_plot <- renderPlot({
    subdata <- filter(datasetInput_top(), class == "Beer")
    if(input$whatdata == "Sweetness") {
      # Need to subset the first 10 here because of tie
      subdata <- subdata[1:10,]
      ggplot(subdata, aes(sweetness, reorder(name, sweetness), 
                          size = alcoholper*100, colour = country)) + 
        geom_point() +
        scale_size(range = c(5, 7)) + 
        scale_y_discrete(labels = function(x) str_pad(str_wrap(x, width = 30),side = "left",30)) + 
        xlab("Sweetness") + ylab("Product Name") +
        theme_jaeg + theme(panel.grid.major.y = element_blank()) + 
        labs(colour = "", size = "Alcohol %")
    } else if (input$whatdata == "Price") {
      ggplot(subdata, aes(price, reorder(name, price), 
                          size = alcoholper*100, colour = country)) + geom_point() +
        scale_size(range = c(5, 7)) + 
        scale_y_discrete(labels = function(x) str_pad(str_wrap(x, width = 30),side = "left",30)) +
        xlab("Price") + ylab("Product Name") +
        scale_x_continuous(labels = dollar) +
        theme_jaeg + theme(panel.grid.major.y = element_blank()) +
        labs(colour = "", size = "Alcohol %")
    } else if (input$whatdata == "Alcohol") {
      ggplot(subdata, aes(alcoholper, reorder(name, alcoholper), size = price, colour = country)) +
        geom_point() + xlab("Alcohol %") + ylab("Product Name") +
        scale_size(range = c(5, 7)) + 
        scale_y_discrete(labels = function(x) str_pad(str_wrap(x, width = 30),side="left",30)) +
        scale_x_continuous(labels = percent) +
        theme_jaeg + theme(panel.grid.major.y = element_blank()) + 
        labs(colour = "", size = "price ($)")
    }
  })
  
  # Top 10 Spirits
  output$top10_spirits_plot <- renderPlot({
    subdata <- filter(datasetInput_top(), class == "Spirits")
    if(input$whatdata == "Sweetness") {
      # Need to subset the first 10 here because of tie
      subdata <- subdata[1:10,]
      ggplot(subdata, aes(sweetness, reorder(name, sweetness),
                          size = alcoholper*100, colour = country)) + geom_point() +
        scale_size(range = c(5, 7)) + 
        scale_y_discrete(labels = function(x) str_pad(str_wrap(x, width = 30),side="left",30)) + 
        xlab("Sweetness") + ylab("Product Name") +
        theme_jaeg + theme(panel.grid.major.y = element_blank()) + 
        labs(colour = "", size = "Alcohol %")
    } else if (input$whatdata == "Price") {
      ggplot(subdata, aes(price, reorder(name, price), size = alcoholper*100, colour = country)) +
        geom_point() +
        scale_size(range = c(5, 7)) + 
        scale_y_discrete(labels = function(x) str_pad(str_wrap(x, width = 30),side = "left",30)) +
        xlab("Price") + ylab("Product Name") +
        scale_x_continuous(labels = dollar) +
        theme_jaeg + theme(panel.grid.major.y = element_blank()) +
        labs(colour = "", size = "Alcohol %")
    } else if (input$whatdata == "Alcohol") {
      ggplot(subdata, aes(alcoholper, reorder(name, alcoholper), size = price, colour = country)) +
        geom_point() + xlab("Alcohol %") + ylab("Product Name") +
        scale_size(range = c(5, 7)) + 
        scale_y_discrete(labels = function(x) str_pad(str_wrap(x, width = 30),side = "left",30)) + scale_x_continuous(labels = percent) +
        theme_jaeg + theme(panel.grid.major.y = element_blank()) + 
        labs(colour = "", size="price ($)")
    }
  })
  
  # Top 10 Refreshment
  output$top10_refresh_plot <- renderPlot({
    subdata <- filter(datasetInput_top(), class == "Refreshment Beverage")
    if(input$whatdata == "Sweetness") {
      # Need to subset the first 10 here because of tie
      subdata <- subdata[1:10,]
      ggplot(subdata, aes(sweetness, reorder(name, sweetness), 
                          size = alcoholper*100, colour = country)) + geom_point() +
        scale_size(range = c(5, 7)) + 
        scale_y_discrete(labels = function(x) str_pad(str_wrap(x, width = 30),side = "left",30)) + 
        xlab("Sweetness") + ylab("Product Name") +
        theme_jaeg + theme(panel.grid.major.y = element_blank()) + 
        labs(colour = "", size = "Alcohol %")
    } else if (input$whatdata == "Price") {
      ggplot(subdata, aes(price, reorder(name, price), size = alcoholper*100, colour = country)) +
        geom_point() +
        scale_size(range = c(5, 7)) + 
        scale_y_discrete(labels = function(x) str_pad(str_wrap(x, width = 30),side = "left",30)) +
        xlab("Price") + ylab("Product Name") +
        scale_x_continuous(labels = dollar) +
        theme_jaeg + theme(panel.grid.major.y = element_blank()) +
        labs(colour = "", size = "Alcohol %")
    } else if (input$whatdata == "Alcohol") {
      # Need to subset the first 10 here because of tie
      subdata <- subdata[1:10,]
      ggplot(subdata, aes(alcoholper, reorder(name, alcoholper), size = price, colour = country)) +
        geom_point() + xlab("Alcohol %") + ylab("Product Name") +
        scale_size(range = c(5, 7)) + 
        scale_y_discrete(labels = function(x) str_pad(str_wrap(x, width = 30),side = "left",30)) +
        scale_x_continuous(labels = percent) +
        theme_jaeg + theme(panel.grid.major.y = element_blank()) + 
        labs(colour = "", size = "price ($)")
    }
  })
  
  
  # Bottom 10 table
  
  bottom_x_class_price_all_df <- reactive({
    bcliquor_df %>% 
      group_by(class) %>%
      top_n(n = 10, wt = -price) %>% arrange(class, price)
  })
  
  bottom_x_class_alcohol_all_df <- reactive({
    bcliquor_df %>% 
      group_by(class) %>%
      top_n(n = 10, wt = -alcoholper) %>% arrange(class, alcoholper)
  })
  
  bottom_x_class_sweet_all_df <- reactive({
    bcliquor_df %>% 
      group_by(class) %>%
      top_n(n = 10, wt = -sweetness) %>% arrange(class, sweetness)
  })
  
  datasetInput_bottom <- reactive({
    switch(input$whatdata,
           "Price" = bottom_x_class_price_all_df(),
           "Alcohol" = bottom_x_class_alcohol_all_df(),
           "Sweetness" = bottom_x_class_sweet_all_df())
  })
  
  # Not use atm
  output$bottom10_tbldata <- renderDataTable({
    datatable(datasetInput_bottom(), filter = "bottom", options = list(pageLength = 10))
  })
  
  # Bottom 10 wine
  output$bottom10_wine_plot <- renderPlot({
    subdata <- filter(datasetInput_bottom(), class == "Wine")
    if(input$whatdata == "Sweetness") {
      # Need to subset the first 10 here because of tie
      subdata <- subdata[sample(nrow(subdata), 10),]
      ggplot(subdata, aes(sweetness, reorder(name, -sweetness), 
                          size = alcoholper*100, colour = country)) + geom_point() +
        scale_size(range = c(5, 7)) + 
        scale_y_discrete(labels = function(x) str_pad(str_wrap(x, width = 30),side = "left",30)) + 
        xlab("Sweetness") + ylab("Product Name") +
        theme_jaeg + theme(panel.grid.major.y = element_blank()) + 
        labs(colour = "", size = "Alcohol %")
    } else if (input$whatdata == "Price") {
      # Need to subset the first 10 here because of tie
      subdata <- subdata[sample(nrow(subdata), 10),]
      ggplot(subdata, aes(price, reorder(name, -price), size = alcoholper*100, colour = country)) +
        geom_point() +
        scale_size(range = c(5, 7)) + 
        scale_y_discrete(labels = function(x) str_pad(str_wrap(x, width = 30),side = "left",30)) +
        xlab("Price") + ylab("Product Name") +
        scale_x_continuous(labels = dollar) +
        theme_jaeg + theme(panel.grid.major.y = element_blank()) +
        labs(colour = "", size = "Alcohol %")
    } else if (input$whatdata == "Alcohol") {
      # Need to subset the first 10 here because of tie
      subdata <- subdata[sample(nrow(subdata), 10),]
      ggplot(subdata, aes(alcoholper, reorder(name, -alcoholper), size = price, colour = country)) +
        geom_point() + xlab("Alcohol %") + ylab("Product Name") +
        scale_size(range = c(5, 7)) + 
        scale_y_discrete(labels = function(x) str_pad(str_wrap(x, width = 30),side = "left",30)) +
        scale_x_continuous(labels = percent) +
        theme_jaeg + theme(panel.grid.major.y = element_blank()) + 
        labs(colour = "", size = "price ($)")
    }
  })
  
  # Bottom 10 beer
  output$bottom10_beer_plot <- renderPlot({
    subdata <- filter(datasetInput_bottom(), class == "Beer")
    if(input$whatdata == "Sweetness") {
      # Need to subset the first 10 here because of tie
      subdata <- subdata[sample(nrow(subdata), 10),]
      ggplot(subdata, aes(sweetness, reorder(name, -sweetness), 
                          size = alcoholper*100, colour = country)) + geom_point() +
        scale_size(range = c(5, 7)) + 
        scale_y_discrete(labels = function(x) str_pad(str_wrap(x, width = 30),side = "left",30)) + 
        xlab("Sweetness") + ylab("Product Name") +
        theme_jaeg + theme(panel.grid.major.y = element_blank()) + 
        labs(colour = "", size = "Alcohol %")
    } else if (input$whatdata == "Price") {
      # Need to subset the first 10 here because of tie
      subdata <- subdata[sample(nrow(subdata), 10),]
      ggplot(subdata, aes(price, reorder(name, -price), 
                          size = alcoholper*100, colour = country)) + geom_point() +
        scale_size(range = c(5, 7)) + 
        scale_y_discrete(labels = function(x) str_pad(str_wrap(x, width = 30),side = "left",30)) + 
        xlab("Price") + ylab("Product Name") +
        scale_x_continuous(labels = dollar) +
        theme_jaeg + theme(panel.grid.major.y = element_blank()) +
        labs(colour = "", size = "Alcohol %")
    } else if (input$whatdata == "Alcohol") {
      # Need to subset the first 10 here because of tie
      subdata <- subdata[sample(nrow(subdata), 10),]
      ggplot(subdata, aes(alcoholper, reorder(name, -alcoholper), size = price, colour = country)) +
        geom_point() + xlab("Alcohol %") + ylab("Product Name") +
        scale_size(range = c(5, 7)) + 
        scale_y_discrete(labels = function(x) str_pad(str_wrap(x, width = 30),side = "left",30)) +
        scale_x_continuous(labels = percent) +
        theme_jaeg + theme(panel.grid.major.y = element_blank()) + 
        labs(colour = "", size = "price ($)")
    }
  })
  
  # Bottom 10 Spirits
  output$bottom10_spirits_plot <- renderPlot({
    subdata <- filter(datasetInput_bottom(), class == "Spirits")
    if(input$whatdata == "Sweetness") {
      # Need to subset the first 10 here because of tie
      subdata <- subdata[sample(nrow(subdata), 10),]
      ggplot(subdata, aes(sweetness, reorder(name, -sweetness), 
                          size = alcoholper*100, colour = country)) + geom_point() +
        scale_size(range = c(5, 7)) + 
        scale_y_discrete(labels = function(x) str_pad(str_wrap(x, width = 30), side = "left",30)) +
        xlab("Sweetness") + ylab("Product Name") +
        theme_jaeg + theme(panel.grid.major.y = element_blank()) + 
        labs(colour = "", size = "Alcohol %")
    } else if (input$whatdata == "Price") {
      # Need to subset the first 10 here because of tie
      subdata <- subdata[sample(nrow(subdata), 10),]
      ggplot(subdata, aes(price, reorder(name, -price), 
                          size = alcoholper*100, colour = country)) + geom_point() +
        scale_size(range = c(5, 7)) + 
        scale_y_discrete(labels = function(x) str_pad(str_wrap(x, width = 30),side = "left",30)) +
        xlab("Price") + ylab("Product Name") +
        scale_x_continuous(labels = dollar) +
        theme_jaeg + theme(panel.grid.major.y = element_blank()) +
        labs(colour = "", size = "Alcohol %")
    } else if (input$whatdata == "Alcohol") {
      # Need to subset the first 10 here because of tie
      subdata <- subdata[sample(nrow(subdata), 10),]
      ggplot(subdata, aes(alcoholper, reorder(name, -alcoholper),
                          size = price, colour = country)) + geom_point() + 
        xlab("Alcohol %") + ylab("Product Name") +
        scale_size(range = c(5, 7)) + 
        scale_y_discrete(labels = function(x) str_pad(str_wrap(x, width = 30), side = "left",30)) + scale_x_continuous(labels = percent) +
        theme_jaeg + theme(panel.grid.major.y = element_blank()) + 
        labs(colour = "", size = "price ($)")
    }
  })
  
  # Bottom 10 Refreshment
  output$bottom10_refresh_plot <- renderPlot({
    subdata <- filter(datasetInput_bottom(), class == "Refreshment Beverage")
    if(input$whatdata == "Sweetness") {
      # Need to subset the first 10 here because of tie
      subdata <- subdata[sample(nrow(subdata), 10),]
      ggplot(subdata, aes(sweetness, reorder(name, -sweetness), 
                          size = alcoholper*100, colour = country)) + geom_point() +
        scale_size(range = c(5, 7)) + 
        scale_y_discrete(labels = function(x) str_pad(str_wrap(x, width = 30),side = "left",30)) + 
        xlab("Sweetness") + ylab("Product Name") +
        theme_jaeg + theme(panel.grid.major.y = element_blank()) + 
        labs(colour = "", size = "Alcohol %")
    } else if (input$whatdata == "Price") {
      # Need to subset the first 10 here because of tie
      subdata <- subdata[sample(nrow(subdata), 10),]
      ggplot(subdata, aes(price, reorder(name, -price), size = alcoholper*100, colour = country)) +
        geom_point() +
        scale_size(range = c(5, 7)) + 
        scale_y_discrete(labels = function(x) str_pad(str_wrap(x, width = 30),side = "left",30)) + 
        xlab("Price") + ylab("Product Name") +
        scale_x_continuous(labels = dollar) +
        theme_jaeg + theme(panel.grid.major.y = element_blank()) +
        labs(colour = "", size = "Alcohol %")
    } else if (input$whatdata == "Alcohol") {
      # Need to subset the first 10 here because of tie
      subdata <- subdata[sample(nrow(subdata), 10),]
      ggplot(subdata, aes(alcoholper, reorder(name, -alcoholper), size = price, colour = country)) +
        geom_point() + xlab("Alcohol %") + ylab("Product Name") +
        scale_size(range = c(5, 7)) + 
        scale_y_discrete(labels = function(x) str_pad(str_wrap(x, width = 30),side = "left",30)) + scale_x_continuous(labels = percent) +
        theme_jaeg + theme(panel.grid.major.y = element_blank()) + 
        labs(colour = "", size = "price ($)")
    }
  })
  
# ---- Sample by Class section ----
  
  # Update input
  minorclass_df <- reactive({bcliquor_df %>%
    filter(class == input$whatclass2)})
  
  # Update UI doesn't work for some reason
  # need to use renderUI to make new one
  output$ui_whatclass2 <- renderUI({
    selectizeInput("whatminorclass2", "I like ...",
    choices = unique(minorclass_df()$minorclass), 
    selected = NULL, multiple = FALSE)
  })
  
  class_country_df <- reactive({
      by_class_df() %>% group_by(country, class) %>%
        tally() %>% mutate(pct = n/sum(n)) %>%
        arrange(-pct)
  })
  
  # create % table
  output$class_country <- renderDataTable({
    if(is.null(input$whatminorclass2) || input$whatminorclass2 == "") {
      DT::datatable(class_country_df(), filter = "top", selection = "single") %>% formatPercentage('pct', 1)
    } else {
      minorclass_df <- by_class_df() %>% group_by(country, class, minorclass) %>%
        filter(minorclass == input$whatminorclass2) %>%
        tally() %>% mutate(pct = n / sum(n)) %>%
        arrange(-pct) 
      DT::datatable(minorclass_df, filter = "top", selection = "single") %>% formatPercentage('pct', 1)
    }
  })
  
  # Row selection
  output$row_selected <- renderPrint({
    r_index <- as.numeric(input$class_country_rows_selected)
    c_index <- class_country_df()[r_index,"country"]
    paste0("http://www.bcliquorstores.com/product-catalogue?country=",tolower(c_index))
  })
  
  # Use row selection to search query BC Liquor Store webpage and return a webpage to an iframe
  # Really cool stuff! ^^
  output$url <- renderUI({
    if(is.null(input$class_country_rows_selected)) return(NULL)
    
    # Get the row and column index to find the country to search
    r_index <- as.numeric(input$class_country_rows_selected)
    c_index <- class_country_df()[r_index,"country"]
    
    # Harmominze some country spelling
    if (c_index == "United States Of America") {c_index <- "United States"}
    if (c_index == "Korea - South") {c_index <-"Korea - Republic Of"}
    if (c_index == "Russia (ussr)" | c_index == "Russia") {c_index <- "Russian Federation"}
    
    if(input$whatclass2 %in% c("Wine", "Beer", "Spirits", "Refreshment Beverage")) {
      t_index <- class_country_df()[r_index,"class"]
      if (t_index == "Refreshment Beverage") {t_index <- "coolers/ciders"} # match the type
      url <- paste0("http://www.bcliquorstores.com/product-catalogue?country=",tolower(c_index),
                    "&type=",tolower(t_index))
    } else (
      url <- paste0("http://www.bcliquorstores.com/product-catalogue?country=",tolower(c_index))
    )
    tags$iframe(src=url, height=1000, width=1000) # adjust iframe here
  })
  
  # Diamond point graph of what each country offer in a table like format
  output$country_x_class_plot <- renderPlot({
    bcliquor_df$country_f <- factor(bcliquor_df$country)
    bcliquor_df$country_f <- factor(bcliquor_df$country_f, levels = rev(levels(bcliquor_df$country_f)))
    ggplot(bcliquor_df, aes(class, country_f, color = class)) + 
      geom_point(size = 8, shape = 18) + theme_bw() + 
      theme(axis.text.y = element_text(colour = "grey70"),
            axis.text.x = element_blank(), 
            axis.ticks = element_blank(), 
            axis.title = element_blank(), 
            legend.title = element_blank(),
            panel.border = element_blank(),
            legend.position = "top",
            legend.text = element_text(size = rel(.6))) + 
      guides(colour=guide_legend(nrow=3,byrow=TRUE))
  }, height = 1000, width = 400)
  
  # Add clickable diamond plot to pull information from BC Liquor Store
  
  output$diamond_check <- renderText({
    bcliquor_df$country_f <- factor(bcliquor_df$country)
    bcliquor_df$country_f <- factor(bcliquor_df$country_f, levels = rev(levels(bcliquor_df$country_f)))
    if (is.null(input$plot_click)) return(NULL)
    click_df <- nearPoints(bcliquor_df, input$plot_click, maxpoints = 1)
    if(nrow(click_df) == 0) return(NULL)
    paste0(click_df["class"], " from ", click_df["country"])
  })
  
  # Use row selection to search query BC Liquor Store webpage and return a webpage to an iframe
  # Really cool stuff! ^^
  output$url2 <- renderUI({
    bcliquor_df$country_f <- factor(bcliquor_df$country)
    bcliquor_df$country_f <- factor(bcliquor_df$country_f, levels = rev(levels(bcliquor_df$country_f)))
    if (is.null(input$plot_click)) return(NULL)
    click_df <- nearPoints(bcliquor_df, input$plot_click, maxpoints = 1)
    if(nrow(click_df) == 0) return(NULL)
    c_country <- click_df["country"]
    t_index <- click_df["class"]
    
    # Harmominze some country spelling
    if (c_country == "United States Of America") {c_country <- "United States"}
    if (c_country == "Korea - South") {c_country <-"Korea - Republic Of"}
    if (c_country == "Russia (ussr)" | c_country == "Russia") {c_country <- "Russian Federation"}
    
    if(input$whatclass2 %in% c("Wine", "Beer", "Spirits", "Refreshment Beverage")) {
      if (t_index == "Refreshment Beverage") {t_index <- "coolers/ciders"} # match the type
      url <- paste0("http://www.bcliquorstores.com/product-catalogue?country=",tolower(c_country),
                    "&type=",tolower(t_index))
    } else (
      url <- paste0("http://www.bcliquorstores.com/product-catalogue?country=",tolower(c_country))
    )
    tags$iframe(src=url, height=1000, width=730) # adjust iframe here
  })
  
  

# ---- Leaflet Central ----
  
  # Map out the country of origin of all the liquor by class
  
  # Single Icon
  # not use atm
  beerIcon <- makeIcon(
    iconUrl = "http://www.goeuro.com/images/images/BPI/beer-icon-yellow.png",
    iconWidth = 30, iconHeight = 30,
    iconAnchorX = 0, iconAnchorY = 0,
    shadowUrl = "",
    shadowWidth = 30, shadowHeight = 30,
    shadowAnchorX = 0, shadowAnchorY = 0
  )
  
  # Multiple Icons
  beericon <- "http://www.goeuro.com/images/images/BPI/beer-icon-yellow.png"
  spiritsicon <- "http://images.vectorhq.com/images/premium/thumbs/214/pirate-icon-bottle-of-rum-flat-design-style-modern-vector-illustration-isolated-on-stylish-color-background-long-shadow-icon_214869631.jpg"
  wineicon <- "wine.png"
  refreshicon <- "cocktail.png"
  
  # Refer to variable with space using the back tick ` trick
  # see http://stackoverflow.com/questions/4551424/how-to-refer-to-a-variable-name-with-spaces
  
  # how to set multiple icons, see https://rstudio.github.io/leaflet/markers.html
  liquorIcons <- iconList(
    Beer = makeIcon(beericon, beericon, 24, 24),
    Wine = makeIcon(wineicon, wineicon, 24, 24),
    `De-alcoholized Beer` = makeIcon(beericon, beericon, 24, 24),
    `De-alcoholized Wine` = makeIcon(wineicon, wineicon, 24, 24),
    Spirits = makeIcon(spiritsicon, spiritsicon, 16, 24),
    `Refreshment Beverage` = makeIcon (refreshicon, refreshicon, 24, 24),
    `Culinary Products` = makeIcon(refreshicon, refreshicon, 24, 24)
  )

  # Lets map!
  output$map_by_class <- renderLeaflet({
    
    # Switch data here as well, otherwise the icon doesn't change
    if(is.null(input$whatminorclass2) || input$whatminorclass2 == "") {
    data <- inner_join(class_country_df(), country_geo_coded, by = "country")
    } else {
      data <- by_class_df() %>% group_by(country, class, minorclass) %>%
        filter(minorclass == input$whatminorclass2)
      data <- inner_join(data, country_geo_coded, by = "country")
    }
    
    leaflet(data) %>% 
      # Overlay Tiles
      addTiles(group = "OSM (default)") %>%
      addProviderTiles("Stamen.Toner", group = "Toner") %>%
      addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
      # Overlay groups
      addMarkers(~lon, ~lat, popup = ~as.character(country), icon = ~liquorIcons[class]) %>%
      # Layers control
      addLayersControl(
        baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
        options = layersControlOptions(collapsed = FALSE))
  })
  
# ---- Raw Data for download Section ----
  
  # Make datatable
  # Did some weird arranging with the dom but it seems to work
  output$rawdata <- renderDataTable({
   DT::datatable(bcliquor_df[c(1:7,11:13,8:10)], extensions = c('ColVis', 'Responsive', 'ColReorder'),
                 options = list(dom = 'C<"clear">lRfrtip',
                 searchHighlight = TRUE)) %>%
      formatPercentage('alcoholper', 1) %>% formatCurrency('price')
  })
  
  # Make download button
  output$downloadData <- downloadHandler(
    filename = "BC Liquor Store Price Data.csv", # name the file
    content = function(file) {
      write.csv(bcliquor_df, file, row.names = FALSE)
    }
  )
  
# ---- Liquor Explorer ----
  
  # Inspired by Yihui movie explorer app
  # see https://github.com/rstudio/shiny-examples/tree/master/051-movie-explorer
  
  # This dynamic price update seem to work now!
  observe({
    withProgress({
      setProgress(message = "Processing data")
    # filter by class and country
    if ((input$country_e == "") & (input$class_e == "")) {
      minprice <- min(bcliquor_df$price, na.rm = TRUE)
      maxprice <- max(bcliquor_df$price, na.rm = TRUE)
      updateSliderInput(session, "price_e",
                        value = c(minprice, maxprice),
                        min = minprice,
                        max = maxprice)
    } else if ((input$country_e == "") & (input$class_e != "")) {
      minprice <- min(filter(bcliquor_df,
                             class == input$class_e)$price, na.rm = TRUE)
      maxprice <- max(filter(bcliquor_df,
                             class == input$class_e)$price, na.rm = TRUE)
      if((minprice != Inf || minprice != -Inf) && (maxprice != -Inf || maxprice != Inf)) {
        updateSliderInput(session, "price_e",
                          value = c(minprice, maxprice),
                          min = minprice,
                          max = maxprice)
      }
    } else if ((!is.null(input$class_e) | input$class_e != "") & 
               (!is.null(input$country_e) | input$country_e != "")) {
      minprice <- min(filter(bcliquor_df,
                             country == input$country_e)$price, na.rm = TRUE)
      maxprice <- max(filter(bcliquor_df,
                             country == input$country_e)$price, na.rm = TRUE)
      if((minprice != Inf || minprice != -Inf) && (maxprice != -Inf || maxprice != Inf)) {
        updateSliderInput(session, "price_e",
                          value = c(minprice, maxprice),
                          min = minprice,
                          max = maxprice)
      }
    } else {
      minprice <- min(bcliquor_df$price, na.rm = TRUE)
      maxprice <- max(bcliquor_df$price, na.rm = TRUE)
      updateSliderInput(session, "price_e",
                        value = c(minprice, maxprice),
                        min = minprice,
                        max = maxprice)
    }
    })
  })
  
  # Filter the movies, returning a data frame
  explorer_df <- reactive({
    
    withProgress({
      setProgress(message = "Drawing Plot")
      
    # Due to dplyr issue #318, we need temp variables for input values
    minprice <- input$price_e[1]
    maxprice <- input$price_e[2]
    minalcohol <- input$alcohol_e[1]/100 # divide by 100 to get nice format
    maxalcohol <- input$alcohol_e[2]/100 # divide by 100 to get nice format
    minsweetness <- input$sweetness_e[1]
    maxsweetness <- input$sweetness_e[2]
    
    # Apply filters
    m <- bcliquor_df %>%
      filter(
        price >= minprice,
        price <= maxprice,
        alcoholper >= minalcohol,
        alcoholper <= maxalcohol,
        sweetness >= minsweetness,
        sweetness <= maxsweetness
      ) %>%
      arrange(-price)
    
    # Optional: Include na sweetness?
    if (input$includesweet == TRUE) {
    m <- rbind(m, filter(bcliquor_df, is.na(sweetness)))
    }
    
    # Optional: filter by country
    if (!is.null(input$country_e) && input$country_e != "") {
      m <- m %>% filter(country %in% input$country_e) 
    }
    
    # Optional: filter by class
    if (!is.null(input$class_e) && input$class_e != "") {
      m <- m %>% filter(class %in% input$class_e) 
    }
    
    # Optional: filter by product name
    if (!is.null(input$name_e) && input$name_e != "") {
      # productname <- paste0("%", input$name_e, "%")
      # m <- m %>% filter(name %like% productname)
      # Use grepl as an alternative %like% is specific to SQLite
      m <- m %>% filter(grepl(input$name_e,name, ignore.case = TRUE))
    }
    
    m <- as.data.frame(m)
    
    })

  })
  
  # Function for generating tooltip text
  # SKU as an id works; the generated ID for some reason doesn't work
  liquor_tooltip <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.null(x$SKU)) return(NULL)
    
    # Pick out the liquor with this SKU
    all_liquor <- isolate(explorer_df())
    liquor <- all_liquor[all_liquor$SKU == x$SKU, ]
    
    # Image switcher, to deal with some jpg and some png issue
    # There are more variation, need Emelie solution here! ^^
    # web scrap the page to find the right link and intel!
  
    # set the user agent to help
    # see https://github.com/hadley/rvest/issues/74
    ua <- "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:33.0) Gecko/20100101 Firefox/33.0"
    
    # Scarp the page
    # Use try to catche retrieval errors
    # see http://stackoverflow.com/questions/30586480/submit-urls-from-a-data-frame-column-using-rvest
    liquor_pg <- try(rvest::html(paste0("http://www.bcliquorstores.com/product/",liquor$SKU),
                      httr::user_agent(ua)), silent = TRUE)
    
    # if any retrieval error, return a special tooltip with no info from BC Liquore Store site
    if (inherits(liquor_pg, "try-error")) {return(
      paste0("<span style = 'color: red;'><b> Not on BC Liquor Store Site </span></b><br>",
             "<span style = 'color: #1F77B4;'><b>", liquor$name, "</b></span><br>",
             "from <b>", liquor$country, "</b><br>",
             "<span style = 'color: #2CA02C;'>$",
             format(liquor$price, big.mark = ",", scientific = FALSE), "</span><br>",
             "<b>", liquor$minorclass, "</b><br>",
             "Alcohol %: <span style = 'color: #2CA02C;'>",
             paste0(liquor$alcoholper*100, "%"), "</span><br>",
             "Sweetness: <span style = 'color: #2CA02C;'>", liquor$sweetness, "</span><br>",
             "<span style = 'color: #FF7F0E;'>SKU", liquor$SKU, "</span><br>")
      ) }
    
    # Smoke out the png or jpg!
    img_bag <- liquor_pg %>% html_nodes("a img") %>%
      html_attr("src")
    
    # Catch no matching element
    # Don't use ifelse because it return a list
    if(length(grep(liquor$SKU, img_bag)) == 0) {
      img_show <- img(src = "http://www.bcliquorstores.com/sites/all/themes/bcliquor3/graphics/placeholders/ph-wine-thumb.gif", class = "tooltip_img")
    } else {
      img_show <- img(src = img_bag[grep(liquor$SKU, img_bag)], class = "tooltip_img")
    }
    
    # Get average rating
    avg_rating <- liquor_pg %>% html_nodes(".average-rating") %>%
      html_text()
    
    # Customize the tooltips
    paste0("<span style = 'color: #1F77B4;'><b>", liquor$name, "</b></span><br><br>",
           img_show,
           "<div class = 'tooltip_text'>",
           "from <b>", liquor$country, "</b><br>",
           "<span style = 'color: #2CA02C;'>$",
           format(liquor$price, big.mark = ",", scientific = FALSE), "</span><br>",
            "<b>", liquor$minorclass, "</b><br>",
            "Alcohol %: <span style = 'color: #2CA02C;'>",
           paste0(liquor$alcoholper*100, "%"), "</span><br>",
            "Sweetness: <span style = 'color: #2CA02C;'>", liquor$sweetness, "</span><br>",
            "<span style = 'color: #FF7F0E;'>SKU", liquor$SKU, "</span><br>",
           "Average rating: <b>", avg_rating, "</b><br></div>"
    )
  }
  
  # A reactive expression with the ggvis plot
  vis <- reactive({
    
    # Make full labels
    xvar_name <- switch(input$xvar,
            "price" = "Price",
            "alcoholper" = "Alcohol %",
            "sweetness" = "Sweetness")
    
    yvar_name <- switch(input$yvar,
                        "price" = "Price",
                        "alcoholper" = "Alcohol %",
                        "sweetness" = "Sweetness")
    
    # Normally we could do something like props(x = ~price, y = ~sweetness),
    # but since the inputs are strings, we need to do a little more work.
    xvar <- prop("x", as.symbol(input$xvar))
    yvar <- prop("y", as.symbol(input$yvar))

    # Make baseplot before axis label
    
    data <- explorer_df()
    data$alcoholper <- data$alcoholper*100
    
    p <- data %>%
      ggvis(x = xvar, y = yvar, key := ~SKU) %>%
      layer_points(size := 50, size.hover := 200,
                   fillOpacity := 0.2, fillOpacity.hover := 0.5,
                   fill = ~subclass) %>%
      # add_tooltip(all_values, "hover") %>%
      # add_tooltip(function(df) df$price) %>%
      add_tooltip(liquor_tooltip, "hover") %>%
      add_legend("fill") %>%
      #       scale_nominal("stroke", domain = c("Yes", "No"),
      #                     range = c("orange", "#aaa")) %>%
      set_options(width = 800, height = 700)
      
    # format the axis with $ sign
    if (xvar_name == "Price" & yvar_name != "Price") {
     p %>% add_axis("x", title = xvar_name, format = "$") %>%
        add_axis("y", title = yvar_name)
    } else if (yvar_name == "Price" & xvar_name != "Price") {
      p %>% add_axis("x", title = xvar_name) %>%
        add_axis("y", title = yvar_name, format = "$")
    } else if (xvar_name == "Price" & yvar_name == "Price") {
      p %>% add_axis("x", title = xvar_name, format = "$") %>%
        add_axis("y", title = yvar_name, format = "$")
    } else {
      p %>% add_axis("x", title = xvar_name) %>%
        add_axis("y", title = yvar_name)
    }

  })
  
  vis %>% bind_shiny("plot_liquor_explorer")
  
  
  # Count the number of liquor selected
  output$n_liquor <- renderText({nrow((explorer_df()))})
  
}
