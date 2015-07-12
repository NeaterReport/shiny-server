# server for Statistics Corner

function(input, output) {
  
# ---- Create Title  ----
  output$caption <- renderText({
    input$caption
  })
  
# ---- Correlation ----

  # Code to generate simple correlated data
  data <- reactive({
    # This trigger a new plot using the action button
    if(input$plot_button > 0) {x <- rnorm(input$n)
                               y <- rnorm(input$n)}
      x <- rnorm(input$n)
      y <- rnorm(input$n)
      z <- input$r * scale(x)[,1] + sqrt(1 - input$r^2) * 
        scale(resid(lm(y ~ x)))[,1]
      xresult <- input$xmean + input$xsd * scale(x)[,1]
      yresult <- input$ymean + input$ysd * z
      data.frame(x = round(xresult,3),y = round(yresult,3))
  })
  
  # Create datatable
  output$datatable <- renderDataTable(data(), options = list(pageLength = 10))
 
  # Create scatterplot
    output$plot_corr <- renderPlot({  
      
    # Tame overplotting by varying and point sizes
     if(input$n <= 500) {
       # Scale the alpha
       alpha = input$n^1 / input$n^1.05
       size = 3
     } else if (input$n > 500) {
       # Scale the alpha and size
       alpha <- input$n^1 / input$n^1.2
       size = 3 * input$n^1 / input$n^1.02
     }
    
    # Calculate correlation coefficient
    cor <- cor(data()$x, data()$y)
    # Format label
    cor_lab <- sprintf("italic(r) == %.3f", cor)
    
    # Make the scatterplot
    gg <- ggplot(data(), aes(x = x, y = y)) + 
      geom_point(color = "orange", size = size, alpha = alpha) +
      geom_smooth(method = lm) +
      annotate("text", x = Inf, y = -Inf, label = cor_lab,
                hjust = 1.5, vjust = -1.5, parse = TRUE, size = 10, color="blue")
    # Branch to enable choose graph style
    if(input$graphstyle == 1) {gg + theme_classic()}
    else if (input$graphstyle == 2) {gg + theme_economist()}
    else if (input$graphstyle == 3) {gg + theme_excel()}
    else if (input$graphstyle == 4) {gg + theme_few()}
    else if (input$graphstyle == 5) {gg + theme_fivethirtyeight()}        
    else if (input$graphstyle == 6) {gg + theme_stata()}
    else if (input$graphstyle == 7) {gg + theme_tufte()}
    })
    
# ---- Make example correlation scatterplots in R ----   

    # Generate multiple correlated data
    cordata <- eventReactive(input$button_draw,{
      # This recompute the matrix when it is non-positive definitive matrix (error)
      pass <- FALSE
      # Add a progress message
      withProgress({
        setProgress(message = "Generating Random Data")
      while (pass==FALSE) {
        # randomly sample a range of desired r for generating the correlated matrix
        r1 <- round(runif(1, min = .1, max = .29),3)
        r2 <- round(runif(1, min = .3, max = .49),3)
        r3 <- round(runif(1, min = .51, max = .99),3)
        
        # use try as a way to catch the error
       out <- try(mvrnorm(input$draw_n, mu = c(0,0,0,0), 
                Sigma = matrix(c(1,r1,r2,-r1,
                                 r1,1,r3,-r2,
                                 r2,r3,1,-r3,
                                 -r1,-r2,-r3,1),
                               ncol = 4),
                empirical = TRUE), TRUE)
       if (is.matrix(out)) {pass=TRUE}
      }
      as.data.frame(out)
      })
    })
    
    # Generate text output for correlation coefficient
    output$r1 <- renderText({paste0("r = ", round(with(cordata(), cor(V1, V2)),3))})
    output$r2 <- renderText({paste0("r = ", round(with(cordata(), cor(V1, V3)),3))})
    output$r3 <- renderText({paste0("r = ", round(with(cordata(), cor(V2, V3)),3))})
    output$r4 <- renderText({paste0("r = ", round(with(cordata(), cor(V1, V4)),3))})
    output$r5 <- renderText({paste0("r = ", round(with(cordata(), cor(V2, V4)),3))})
    output$r6 <- renderText({paste0("r = ", round(with(cordata(), cor(V3, V4)),3))})
    
    # Create our own theme - the jaeg theme :)
    theme_jaeg <- theme_bw() + theme(axis.title = element_blank(),
                       axis.text = element_text(color = "grey"),
                       panel.border = element_blank(),
                       axis.line = element_line("grey"),
                       panel.background = element_rect(fill = "mintcream"),
                       plot.background = element_rect(fill = "mintcream"))
    
    # Set the theme to jaeg ^^
    theme_set(theme_jaeg)
    
    # Create the individual scatterplot, one for each example
    output$weakplot_pos <- renderPlot({
      ggplot(cordata(), aes(x = V1, y = V2)) + 
        geom_point(color = "pink", alpha = input$draw_n^1 / input$draw_n^1.05) + # scale the alpha
        geom_smooth(method = lm, fill = "cadetblue", color = "dimgrey") +
        stat_ellipse(type = "norm", geom = "polygon", 
                     color = NA, fill = "orange", alpha = .1) # add ellipse
      })
    
    output$medplot_pos <- renderPlot({
      ggplot(cordata(), aes(x = V1, y = V3)) + 
        geom_point(color = "plum", alpha = input$draw_n^1 / input$draw_n^1.05) + # scale the alpha
        geom_smooth(method = lm, fill="cadetblue", color="dimgrey") +
        stat_ellipse(type = "norm", geom = "polygon", 
                     color = NA, fill = "orange", alpha = .1) # add ellipse
      })
    
    output$strongplot_pos <- renderPlot({
      ggplot(cordata(), aes(x = V2, y = V3)) + 
        geom_point(color = "orchid", alpha = input$draw_n^1 / input$draw_n^1.05) + # scale the alpha
        geom_smooth(method = lm, fill = "cadetblue", color = "dimgrey") +
        stat_ellipse(type = "norm", geom = "polygon", 
                     color = NA, fill = "orange", alpha=.1) # add ellipse
      })
    
    output$weakplot_neg <- renderPlot({
      ggplot(cordata(), aes(x = V1, y = V4)) + 
        geom_point(color = "pink", alpha = input$draw_n^1 / input$draw_n^1.05) + # scale the alpha
        geom_smooth(method = lm, fill = "cadetblue", color = "dimgrey") +
        stat_ellipse(type = "norm", geom = "polygon", color = NA, fill = "orange", alpha=.1) # add ellipse
      })
    
    output$medplot_neg <- renderPlot({
      ggplot(cordata(), aes(x = V2, y = V4)) + 
        geom_point(color = "plum", alpha = input$draw_n^1 / input$draw_n^1.05) + # scale the alpha
        geom_smooth(method = lm, fill = "cadetblue", color = "dimgrey") +
        stat_ellipse(type = "norm", geom = "polygon", 
                     color = NA, fill = "orange", alpha = .1) # add ellipse
      })
    
    output$strongplot_neg <- renderPlot({
      ggplot(cordata(), aes(x = V3, y = V4)) + 
        geom_point(color = "orchid", alpha = input$draw_n^1 / input$draw_n^1.05) + # scale the alpha
        geom_smooth(method = lm, fill = "cadetblue", color = "dimgrey") +
        stat_ellipse(type = "norm", geom = "polygon", 
                     color = NA, fill = "orange", alpha = .1) # add ellipse
      })
    
# ---- Sampling ----
    
    # Create histogram with density curve
    output$plot_sample <- renderPlot({
      data <- data.frame(x = rnorm(input$n_s, input$mean_s, input$sd_s))
      gg <- ggplot(data, aes(x=x)) + 
        # This is so the histogram will be the same height as the density plot
        geom_histogram(aes(y = ..density..), fill="orange", alpha=.4) +
        geom_density() +
        labs(title = "Histogram")
      # Branch to enable choose graph style
      if(input$graphstyle == 1) {gg + theme_classic()}
      else if (input$graphstyle == 2) {gg + theme_economist()}
      else if (input$graphstyle == 3) {gg + theme_excel()}
      else if (input$graphstyle == 4) {gg + theme_few()}
      else if (input$graphstyle == 5) {gg + theme_fivethirtyeight()}        
      else if (input$graphstyle == 6) {gg + theme_stata()}
      else if (input$graphstyle == 7) {gg + theme_tufte()}
    })

}

