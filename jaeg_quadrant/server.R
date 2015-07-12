# server for JAEG Quadrant Plot

function(input, output, session) {
  
# ---- Generate random or import data ----
  
  dataset <- reactive({
    
    # Get upload file
    inFile <- input$importfile
    
    # Nested input$getdata to regenerate a random data when the button is clicked
    if (input$datatype == "Pos Rating") {
      input$getdata
      gen_pos_rating(v = input$no_of_v, nr = input$no_of_rating)
    } else if (input$datatype == "Rating") {
      input$getdata
      gen_rating(v = input$no_of_v, nr = input$no_of_rating)
    } else if (input$datatype == "Random") {
      input$getdata
      gen_rnorm(v = input$no_of_v)
    } else if (input$datatype == "Excel 2007") {
      if (is.null(inFile))
        return(NULL)
      # Provide explicit file extension to read in excel file in shiny
      # See http://stackoverflow.com/questions/30624201/read-excel-in-a-shiny-app
      file.rename(inFile$datapath, paste(inFile$datapath, ".xlsx", sep=""))
      read_excel(paste(inFile$datapath, ".xlsx", sep=""), sheet = 1)
    } else if (input$datatype == "CSV") {
      if (is.null(inFile))
        return(NULL)
      read.csv(inFile$datapath)
    }
    
  })

# ---- Create a universal dataset ----
  
  # This code creates two dataset
  # One with all the variables and a second with the user selected variables
  # This way, all the variables will be plotted if none is chosen
  dataset2 <- reactive({
    if(is.null(input$fieldname)) {
      data <- dataset()
    } else {
      data <- dataset()[input$fieldname]
    }
  })


# ----- Update variable selection -----
  
  observe({
    if (identical(dataset(), '') || identical(dataset(), data.frame())) {
      return(NULL)
    }
    updateSelectizeInput(session, 'fieldname', choices = names(dataset()),
                         selected = NULL, server = TRUE)
  })
  
# ----- Create Quadrant Data -----
  
# Solutions from Amber James, Thanks!
# Use reactiveValues and ObserveEvent
# see https://groups.google.com/forum/#!topic/shiny-discuss/7ZO92Oy30Dw

  # Create a null set of reactiveValues
  values<- reactiveValues(
    xcut = NULL,
    ycut = NULL
  )
  
  # Update the reactiveValues when doublecliked
  observeEvent(input$plot_dblclick, {
    values$xcut <- input$plot_dblclick$x
    values$ycut <- input$plot_dblclick$y
  })
  
  # Reset the reactiveValues back to null when new data is loaded
  observeEvent(input$getdata, {
    values$xcut <- NULL
    values$ycut <- NULL
  })

  # Make Quadrant Plot
  output$plot_quadrant <- renderPlot({
      
    # Check that user have selected a data first
    validate(
      need(!is.null(dataset2()), "Please supply a dataset")
    )
      
    data <- dataset2()
      
    # If you make the data here, it won't crash and will let you subset not sure why!
    # If you make the data outside of the same RenderPlot, it will crash ...
    makeQuadrantData(data, meancut = values$xcut, corcut = values$ycut)
      
    # Need this so that the median will be use as cutoff when there is no user supplied vlaues
    if(is.null(values$xcut) & is.null(values$ycut)) {
      xcut <- median(q_df$mean)
      ycut <- median(q_df$cor)
    } else {
      xcut <- values$xcut
      ycut <- values$ycut
    }
      
    # Make the scatterplot with reference lines
    ggplot(q_df, aes(mean, cor, colour = group)) + geom_point(size = 5) +
      geom_vline(xintercept = xcut, colour = "orange") +
      geom_hline(yintercept = ycut, colour = "orange") +
      ggplot2::annotate("rect", xmin = xcut, xmax = +Inf, 
                        ymin = ycut, ymax = +Inf, fill = "orange", alpha = 1/3) +
      theme(panel.background = element_rect(fill = "#FEFBFB"),
            plot.background = element_rect(fill = "#FEFBFB"),
            axis.text = element_text(colour = "#1F77B4"),
            axis.title = element_text(colour = "#2CA02C"))
      
    })
  
# ----- Select Point Graph -----
  
  # Show information about the point selected
  # May want ot change this to the actual correlation, not the selected
  output$info <- renderText({
    if (is.null(input$plot_click)) return(NULL)
    click_df <- nearPoints(q_df, input$plot_click)
    if (nrow(click_df) == 0) return(NULL)
    paste0("mean = ", round(click_df$mean, 2), "\ncorrelation = ", round(click_df$cor, 3)) # show cor and mean
  })
  
  # Create the colourset use for the individual graphs based on the quadrant group
  colourset <- reactive({
    input$plot_dblclick
    click_df <- nearPoints(q_df, input$plot_click)
    colour_lookup <- c("1" = "#F8766D", "2" = "#7CAE00", "3" = "#00BFC4", "4" = "#C77CFF")
    unname(colour_lookup[click_df[1, ncol(click_df)]])
  })

  # Create histogram of the selected point (IV)
  output$info_histogram <- renderPlot({
    
    input$plot_dblclick
    if (is.null(input$plot_click)) return(NULL)
    click_df<- nearPoints(q_df, input$plot_click)
    if(nrow(click_df) == 0) return(NULL)
    varname <- rownames(click_df)
    vardata <- dataset2()[varname]
    
    ggplot(vardata, aes_string(x = names(vardata)[1])) + 
      geom_histogram(fill = colourset()) + theme_bw() + 
      theme(panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(colour = "#1F77B4"),
            panel.border = element_blank(),
            axis.text = element_text(colour = "#1F77B4"),
            axis.title = element_text(colour = "#2CA02C"),
            axis.ticks.x = element_blank(),
            axis.line = element_blank(),
            panel.background = element_rect(fill = "#FEFBFB"),
            plot.background = element_rect(fill = "#FEFBFB"))
  })
  
  # Create the scatterplot of selected point (IV) and the DV
  output$info_scatter <- renderPlot({
    
    if (is.null(input$plot_click)) return(NULL)
    click_df <- nearPoints(q_df, input$plot_click)
    if(nrow(click_df) == 0) return(NULL)
    varname <- rownames(click_df)
    xdata <- dataset2()[varname]
    ydata <- dataset2()[ncol(dataset2())]
    vardata <- data.frame(iv = xdata, dv = ydata)
    
    # check if all the variables are factor, if so, do a geom_tile instead
    if (all(unname(sapply(vardata, class)) == "factor")) {
      ggplot(vardata, aes_string(x = names(vardata)[1], y = names(vardata)[2])) +
        stat_bin2d() + theme_bw() + 
        scale_fill_continuous(low = "#EEEEEE", high = colourset()) +
        theme(panel.grid.major = element_blank(),
              panel.border = element_blank(),
              axis.text = element_text(colour = "#1F77B4"),
              axis.title = element_text(colour = "#2CA02C"),
              axis.ticks.x = element_blank(),
              axis.line = element_blank(),
              legend.title = element_text(colour="#1F77B4", face="bold"),
              panel.background = element_rect(fill = "#FEFBFB"),
              plot.background = element_rect(fill = "#FEFBFB"))
    } else {
      ggplot(vardata, aes_string(x = names(vardata)[1], y = names(vardata)[2])) + 
        geom_point(size=3, shape=21, colour=colourset()) + geom_smooth() + 
        geom_rug(colour = "grey60") + theme_bw() +
        theme(panel.grid.major.x = element_line(colour = "#1F77B4"),
              panel.grid.major.y = element_line(colour = "#1F77B4"),
              panel.border = element_blank(),
              axis.text = element_text(colour = "#1F77B4"),
              axis.title = element_text(colour = "#2CA02C"),
              axis.ticks.x = element_blank(),
              axis.line = element_blank(),
              panel.background = element_rect(fill = "#FEFBFB"),
              plot.background = element_rect(fill = "#FEFBFB"))
    }
  })

  # ----- Select Table -----
  
  # Show the raw data for the selected point (IV) and DV
  output$info_data <- renderDataTable({
    
    if (is.null(input$plot_click)) return(NULL)
    click_df <- nearPoints(q_df, input$plot_click)
    if(nrow(click_df) == 0) return(NULL)
    varname <- rownames(click_df)
    xdata <- dataset2()[varname]
    ydata <- dataset2()[ncol(dataset2())]
    info_data <- data.frame(xdata, ydata)
    
    # Add filtering and apply formatting options using the DT package
    DT::datatable(info_data, filter = 'top')
  })
  
# ----- Dynamic Individual Histogram -----
  
  # Dynamic multiple plots
  # See https://gist.github.com/wch/5436415/
  # Insert the right number of plot output objects into the web page
  output$plots <- renderUI({
    
    # Check that user have selected a data first
    validate(
      need(!is.null(dataset2()), "Please supply a dataset")
    )
    
    data <- dataset2()
    
    # Create a list to store the plots
    plot_output_list <- lapply(1:ncol(data), function(i) {
      plotname <- paste("plot", i, sep="")
      plotOutput(plotname, width = 500, height = 160) # Adjust the dimension here
    })
    
    # Convert the list to a tagList - this is necessary for the list of items to display properly.
    do.call(tagList, plot_output_list)
  })
  
  
  # Call renderPlot for each one. Plots are only actually generated 
  # when they are visible on the web page.
  observe({
    
    # Switch between full and user selected dataset
    if(is.null(input$fieldname)) {
      data <- dataset()
    } else {
      data <- dataset2()
    }
    
    # Check that user have selected a data first
    validate(
      need(!is.null(data), 'Please select a data file first')
    )
    
    # Now populate the graph
    for (i in 1:ncol(data)) {
      
      # Need local so that each item gets its own number. Without it, the value
      # of i in the renderPlot() will be the same across all instances, because
      # of when the expression is evaluated.
      local({
        my_i <- i
        plotname <- paste("plot", my_i, sep="")
        
        output[[plotname]] <- renderPlot({
          ggplot(data, aes_string(x = names(data)[my_i])) + geom_histogram(fill= "grey70") +
            theme_bw() + 
            theme(panel.grid.minor.x = element_blank(),
                  panel.grid.major.x = element_blank(),
                  panel.grid.major.y = element_line(colour = "#1F77B4", size = .1),
                  panel.border = element_blank(),
                  axis.text = element_text(colour = "#1F77B4"),
                  axis.title = element_text(colour = "#2CA02C"),
                  axis.ticks.x = element_blank(),
                  axis.line = element_blank(),
                  panel.background = element_rect(fill = "#FEFBFB"),
                  plot.background = element_rect(fill = "#FEFBFB"))
        })
      })
    }
  })

# ---- Data table ----
  
  output$datatable <- DT::renderDataTable({
    
    # Check that user have selected a data first
    validate(
      need(!is.null(dataset2()), "Please supply a dataset")
    )
    
    data <- dataset2()
    
    # Add filtering and apply formatting options using the DT package
    # Use extensions to fix the ID (first col) and outcome (last col)
    DT::datatable(data,
                  extensions = list(FixedColumns = list(leftColumns = 2, rightColumns = 1)),
                  options = list(
                    pageLength = 10,
                    dom = 't',
                    scrollX = TRUE,
                    scrollCollapse = TRUE)) %>% 
      formatStyle(1, backgroundColor = "#FFF2F3") %>%
      formatStyle(ncol(data), backgroundColor = '#FFF2F3', fontWeight = 'bold')
  })
  
# ----- Reset button -----

  observeEvent(input$reset, {
    shinyjs::reset("fieldname")
  })
  
}