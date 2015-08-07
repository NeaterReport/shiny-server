# server for JAEG Wait Time


function(input, output, session) {
  
# ---- Get Name of Treatment and Province ----
  
  # Obtain the names of selected variables to display back on the dashboard
  output$trt_selected <- renderText({input$ui_trt})
  output$prov_selected <- renderText({input$ui_prov})
  
  output$trt_selected2 <- renderText({input$ui_trt2})
  output$year_selected2 <- renderText({input$ui_year2})

# ---- Provincial Flag ----
  
  # Grab the correct provincial flag
  output$image_flag <- renderImage({
    # When input$n is 3, filename is ./images/image3.jpeg
    filename <- normalizePath(file.path('.',
                                        paste('www/', 
                                        gsub("\\.","",input$ui_prov), '_flag.png', sep='')))
    
    # Return a list containing the filename and alt text
    list(src = filename,
         alt = paste("Image number ", input$ui_prov),
         height = 50)
    
  }, deleteFile = FALSE)
  

# ---- Create Subdata ----
  
  # Individual Province Section
  # Province by Year
  subdata_prov_x_yr_df <- reactive({
    filter(waitTime_prov_df, Province == input$ui_prov, Year == input$ui_year)
  })
  
  # Treatment by Year
  subdata_trt_x_time_df <- reactive({
    filter(waitTime_prov_df, Treatment == input$ui_trt, Year == input$ui_year, Province != "Canada")
  })
  
  # Province
  subdata_prov_df <- reactive({
    filter(waitTime_prov_df, Province == input$ui_prov)
  })
  
  # Region by Treatment
  subdata_region_x_trt_df <- reactive({
    filter(waitTime_region_df[complete.cases(waitTime_region_df),], Province == input$ui_prov, Treatment == input$ui_trt) %>%
      arrange(-MetBenchmark)
  })
  
  # National Section
  # Treatment by Province without Canada
  subdata_trt_x_prov_df <- reactive({
    filter(waitTime_prov_df, Treatment == input$ui_trt2, Province != "Canada")
  })
  
  # Treatment by Province with Canada
  subdata_trt_x_provCanada_df <- reactive({
    filter(waitTime_prov_df, Treatment == input$ui_trt2)
  })
  
  # Time by Province with Canada
  subdata_time_x_provCanada_df <- reactive({
    # need subset by percentile or else won't plot
    filter(waitTime_prov_df, !is.na(Percentile50), Year == input$ui_year2)
  })
  
# ---- Individual Province Section ----
  
  # ---- Make Info Box
  
  output$box_trt_rank <- renderInfoBox({
    # Get data
    data <-  subdata_trt_x_time_df() %>% mutate(rank = rank(MetBenchmark))
    
    trt_rank <- as.numeric(data[data$Province == input$ui_prov,"rank"])
  
    infoBox(title = "Rank", value=trt_rank, icon = icon("medkit"),
      color = "blue", fill = TRUE
    )
  })
  
  output$box_trt_per90 <- renderInfoBox({
    
    # Get data
    data <-  subdata_trt_x_time_df() %>% mutate(rank = rank(MetBenchmark))
    
    trt_per <- as.numeric(data[data$Province == input$ui_prov,"Percentile90"])
    
    infoBox(title = "90th Percentile", value=paste0(trt_per," Days"), icon = icon("clock-o"),
            color = "green", fill = TRUE
    )
  })
  
  output$box_trt_per50 <- renderInfoBox({
    
    # Get data
    data <-  subdata_trt_x_time_df() %>% mutate(rank = rank(MetBenchmark))
    
    trt_per <- as.numeric(data[data$Province == input$ui_prov,"Percentile50"])
    
    infoBox(title = "50th Percentile", value=paste0(trt_per," Days"), icon = icon("clock-o"),
            color = "green", fill = TRUE
    )
  })
  
  # Health Region Data
  output$region_data <- DT::renderDataTable({
    # Only show the relevant variables
    subdata_region_x_trt_df()[,c("Region","Volume", "MetBenchmark","Percentile50","Percentile90")] %>% mutate(MetBenchmark = MetBenchmark/100) %>%
      DT::datatable(extensions = 'Responsive') %>%
      formatPercentage('MetBenchmark', 1)
  })
  
  # Bullet Graph
  
  # see https://groups.google.com/forum/#!topic/shiny-discuss/Fgpd0LoZ-y8
  # Adjust plot height using reactive fn
  plotheight <- reactive({
    nrow(subdata_region_x_trt_df())*50 
  })
  
  output$bullet_graph <- renderPlot({
    
    # subdata by treatment
    plot_data <-  subdata_region_x_trt_df()
    
    # Validate that there is data before plotting
    validate(
      # check if everything is na, if so, abort and show msg
      need(nrow(plot_data != 0), "No data avaliable, please select another.")
    )
  
      n_graph <- nrow(plot_data)
      
      par(mfrow=c(n_graph,1))	
      par(mar=c(2,14,.1,1))
      
      # plot them all using lapply
      lapply(seq(1,n_graph), 
             function(i) bbgraph(plot_data[i,"MetBenchmark"], plot_data[i,"HealthRegion"])
      )

  }, height = plotheight)
  
  # thin line graph, a JAEG alternative to dot graph
  output$thinLine_graph <- renderPlot({
    
    plot_data <-  subdata_trt_x_time_df()
    
    validate(
      # check if everything is na, if so, abort and show msg
      need(all(is.na(plot_data[,"MetBenchmark"])) == FALSE, "No data avaliable, please select another.")
    )
  
  # reorder and create factored province for ordering
    plot_data$prov <- reorder(factor(plot_data$Province), plot_data$MetBenchmark)
    
    # Assign grey colour to all
    colourband <- rep("grey70", 10)
    
    # find out which level the selected province is in and the index via which
    num_c <- which(levels(plot_data$prov) == input$ui_prov)
    
    # Use the index to update the colour palette to blue to highlight it!
    colourband[num_c] <- "#1F77B4"
    
    ggplot(plot_data, aes(MetBenchmark/100, factor(Year), colour = prov)) + geom_point(size = 25, alpha = 1/2, shape = 124) + theme_minimal() +
      theme(axis.title = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            strip.background =   element_rect(fill = "grey30", colour = "grey10"),
            strip.text = element_text(colour = "yellow"),
            panel.grid.major.x = element_line(colour = "pink"),
            panel.grid.major.y = element_blank(),
            legend.position = "bottom",
            legend.margin = unit(-0.6,"cm"), # remove the legend margin
            plot.margin = unit(c(0,0,0,0),"cm"), # remove the plot margin
            panel.margin = unit(c(0,0,0,0), "cm")) + # remove the panel margin
      scale_x_continuous(label = percent) + 
      guides(color = guide_legend(title = paste0("% Met Benchmark in ",input$ui_year), 
                                  nrow = 1, byrow =TRUE, 
                                  override.aes = list(alpha = 1, size = 8))) + 
      scale_colour_manual(values = colourband)
  })
  
  # Dot graphy Not use atm
  output$dot_graph <- renderPlot({
    
    plot_data <-  subdata_trt_x_time_df()
  
  ggplot(plot_data, aes(MetBenchmark/100, factor(Year), colour = Region, shape = Region)) + geom_text(aes(label = Province), vjust=-.8) + geom_point(size = 5, alpha = 2/3) +
    xlab(paste0("% of ", input$ui_trt, " Met Benchmark")) +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          strip.background =   element_rect(fill = "grey30", colour = "grey10"),
          strip.text = element_text(colour = "yellow"),
          legend.position = "none") + scale_x_continuous(label = percent) + facet_grid(Region~.)
  })
  
  # Area graph
  output$area_graph <- renderPlot({
    
   plot_data <- subdata_prov_df()
   
   # need to use complete case or else facet has problem with missing
   # The 90th percentile seems to work fine as a proxy
   # Might lose some data but is okay
   plot_data <- plot_data[complete.cases(plot_data$Percentile90),]
   
   # to add legend for separate geom line or area without melting or gathering
   # melt won't work because ribbon doesn't accept gradient in ggplot2 or so they say
   # see http://stackoverflow.com/questions/10349206/add-legend-to-ggplot2-line-plot
   
   ggplot(plot_data, aes(factor(Year), Percentile90, group=factor(Treatment))) + 
     geom_area(aes(factor(Year), Percentile90, group=factor(Treatment), 
                   fill = "90th Percentile"), size = 3) + 
     geom_line(aes(factor(Year), Percentile90, group=factor(Treatment),
                   colour = MetB), size = 3) +
     geom_area(aes(factor(Year), Percentile50, group=factor(Treatment), 
                   fill = "50th Percentile"), size = 3) + 
     facet_wrap(~Treatment, ncol = 2) + 
     theme_fivethirtyeight() + 
     scale_fill_manual("", 
                       breaks = c("90th Percentile", "50th Percentile"),
                       values = c("#fee8c8","#fdbb84")) +
     theme(legend.position = "top") +   
     guides(colour=guide_legend(title = "Met Benchmark?"),
          fill=guide_legend(title = "Percentile (Days)", nrow=1,byrow=TRUE))
   
  }, height = 800)
   
# ---- National Section ----
  
  # Line graph of trend over time by province
  
  # Met Benchmark
  output$trend_mb_graph <- renderPlot({
    
    plot_data <- subdata_trt_x_prov_df()
    
    # Validate that there is data before plotting
    validate(
      # check if everything is na, if so, abort and show msg
      need(all(is.na(plot_data[,"MetBenchmark"])) == FALSE, "No data avaliable, please select another.")
    )
    
      p <- ggplot(plot_data, aes(factor(Year), MetBenchmark, group=Province, size = Volume)) +
        geom_line(aes(colour = "Province")) + 
        geom_line(aes(colour = Region)) + # double plotting trick to get the direct label right
        theme_bw() + 
        scale_colour_manual(values=c("grey80","#a6cee3","grey80","#1f78b4", rep("grey80",9),"#b2df8a","black")) + scale_size_continuous(range = c(.3, 5)) + # need to manually set the colour for direct label
        xlab("") + ylab("Met Benchmark (%)") + 
        theme(axis.text = element_text(colour = "#1F77B4"),
              axis.title = element_text(colour = "#2CA02C"),
              panel.grid.major = element_line(colour = "grey70", size = .2, linetype = "dotted"),
              panel.border = element_blank(),
              legend.key = element_blank(),
              legend.text = element_text(colour = "grey60"),
              legend.title = element_text(colour = "#2CA02C"))
      
      direct.label(p, list('last.qp', cex=.7))
    
  })
  
  # 90th Percentile
  output$trend_90_graph <- renderPlot({
    
    plot_data <- subdata_trt_x_prov_df()
    
    # Validate that there is data before plotting
    validate(
      # check if everything is na, if so, abort and show msg
      need(all(is.na(plot_data[,"Percentile90"])) == FALSE, "No data avaliable, please select another.")
    )
    
    p <- ggplot(plot_data, aes(factor(Year), Percentile90, group=Province, size = Volume)) +
      geom_line(aes(colour = "Province")) + 
      geom_line(aes(colour = Region)) + # double plotting trick to get the direct label right
      theme_bw() + 
      scale_colour_manual(values=c("grey80","#a6cee3","grey80","#1f78b4", rep("grey80",9),"#b2df8a","black")) + scale_size_continuous(range = c(.3, 5)) + # need to manually set the colour for direct label
      xlab("") + ylab("90th Percentile (Day)") + 
      theme(axis.text = element_text(colour = "#1F77B4"),
            axis.title = element_text(colour = "#2CA02C"),
            panel.grid.major = element_line(colour = "grey70", size = .2, linetype = "dotted"),
            panel.border = element_blank(),
            legend.key = element_blank(),
            legend.text = element_text(colour = "grey60"),
            legend.title = element_text(colour = "#2CA02C"))
    
    direct.label(p, list('last.qp', cex=.7))
    
  })
  
  # 50th Percentile
  output$trend_50_graph <- renderPlot({
    
    plot_data <- subdata_trt_x_prov_df()
    
    # Validate that there is data before plotting
    validate(
      # check if everything is na, if so, abort and show msg
      need(all(is.na(plot_data[,"Percentile50"])) == FALSE, "No data avaliable, please select another.")
    )
    
    p <- ggplot(plot_data, aes(factor(Year), Percentile50, group=Province, size = Volume)) +
      geom_line(aes(colour = "Province")) + 
      geom_line(aes(colour = Region)) + # double plotting trick to get the direct label right
      theme_bw() + 
      scale_colour_manual(values=c("grey80","#a6cee3","grey80","#1f78b4", rep("grey80",9),"#b2df8a","black")) + scale_size_continuous(range = c(.3, 5)) + # need to manually set the colour for direct label
      xlab("") + ylab("50th Percentile (Day)") + 
      theme(axis.text = element_text(colour = "#1F77B4"),
            axis.title = element_text(colour = "#2CA02C"),
            panel.grid.major = element_line(colour = "grey70", size = .2, linetype = "dotted"),
            panel.border = element_blank(),
            legend.key = element_blank(),
            legend.text = element_text(colour = "grey60"),
            legend.title = element_text(colour = "#2CA02C"))
    
    direct.label(p, list('last.qp', cex=.7))
    
  })
  
# ---- Choropleth Map ----
  
  output$map <- renderPlot({
    
    # Add this so it will update when use selects a new treatment
    input$ui_trt2
    
    # Prepare the data
    plot_data <- subdata_trt_x_prov_df()
    
    # Validate that there is data before plotting
    validate(
      # check if everything is na, if so, abort and show msg
      need(all(is.na(plot_data[,"MetBenchmark"])) == FALSE, "No data avaliable, please select another.")
    )
    
    # assign id for merging with shapefile
    plot_data$id <- as.character(plot_data$Prov_Code)
    
    # Join the data
    mapo2 <- left_join(mapo, plot_data)
    
    # Map it!
    ggplot() + geom_polygon(data = filter(mapo2, !is.na(Year)), aes(long, lat, group = group, fill = MetBenchmark/100), color = NA) + facet_wrap(~Year) + theme_nothing(legend = TRUE) + 
      scale_fill_distiller(palette = "PuRd", labels = percent,
                           breaks = pretty_breaks(n = 5), values = c(1,0)) +
      labs(title = "% Met Brenchmark",
           fill = "") +  guides(fill = guide_legend(reverse = TRUE)) +
      theme(strip.background = element_rect(fill = "white"),
            strip.text = element_text(size = rel(1.5), colour = "grey70"),
            plot.margin = unit(c(1,1,1,1),"cm"),
            plot.title=element_text(size = 18, vjust=3, colour = "#2CA02C"))
    
  })
  
# ---- Comparisoin Graph ----
  
  output$compare_graph_yr <- renderPlot({
    
    plot_data <- subdata_trt_x_provCanada_df()
    
    ggplot(plot_data) + 
      geom_segment(aes(x = Percentile90, y = Prov, yend = Prov, xend = Percentile50), 
                   colour = "grey70") + 
      geom_point(aes(Percentile50, Prov), colour = "red", size = 3) + 
      geom_point(aes(Percentile90, Prov), colour = "orange", size = 3) + 
      facet_wrap(~Year) + 
      theme_minimal() +
      theme(panel.grid.major.x = element_line(colour = "#2CA02C", size = .1),
            panel.grid.major.y = element_blank(), axis.ticks = element_blank(),
            axis.text = element_text(colour = "#1F77B4"),
            axis.title = element_text(colour = "#2CA02C"),
            plot.title = element_text(size = 18, vjust=3, colour = "#2CA02C", face="bold"),
            strip.text = element_text(size = 12)) + 
      labs(x = "", y = "", title = input$ui_trt2)
    
  })
  
  output$compare_graph_trt <- renderPlot({
    
    plot_data <- subdata_time_x_provCanada_df()
  
    ggplot(plot_data) + 
      geom_segment(aes(x = Percentile90, y = Prov, yend = Prov, xend = Percentile50), 
                   colour = "grey70") + 
      geom_point(aes(Percentile50, Prov), colour = "red", size = 3) + 
      geom_point(aes(Percentile90, Prov), colour = "orange", size = 3) +
      facet_wrap(~Treatment, scales = "free") + 
      theme_minimal() + 
      theme(panel.grid.major.x = element_line(colour = "#2CA02C", size = .1),
            panel.grid.major.y = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_text(colour = "#1F77B4"),
            axis.title = element_text(colour = "#2CA02C"),
            plot.title = element_text(size = 18, vjust=3, colour = "#2CA02C", face="bold"),
            strip.text = element_text(size = 12)) + 
      labs(x = "", y = "", title = input$ui_year2)
    
  })
  
  
# ---- Report Handler ----
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste(paste0('my-report-',gsub("\\.","",input$ui_prov)), sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },
    
    content = function(file) {
      src <- normalizePath(switch(input$format,
        PDF = 'sampleReport.Rmd', HTML = 'sampleReport.Rmd', Word = 'sampleReportW.Rmd'
      ))
      
      # not sure what this add, as it adds complexity to reading in the data
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      # owd <- setwd(tempdir())
      owd <- setwd("www")
      on.exit(setwd(owd))
       file.copy(src, 'sampleReport.Rmd')
       # file.copy(src, './www/CIHI treatment wait time.xlsx')
      
      library(rmarkdown)
      out <- render(src, switch(
        input$format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
      ))
      file.rename(out, file)
    }
  )
  

}