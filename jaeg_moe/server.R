# server for JAEG MoE

function(input, output, clientData, session) {
  
# Make title
  output$caption <- renderText({
    input$caption
  })
  
# What's Today
  output$currentDate <- renderText({as.character(Sys.Date())})
  
# ---- Update UI ----
  
  # Dynamic pop UI depending on the no. of observations
  output$ui_p_pop <- renderUI({
    minN <- input$nn
    sliderInput("N",label = "Population Size:",
                min = minN, max = 10000, value = 10000, step = 500)
  })
  
  output$ui_mean_pop <- renderUI({
    minN <- input$n_mean
    sliderInput("N_mean",label = "Population Size:",
                min = minN, max = 10000, value = 10000, step = 500)
  })

# ---- Calculate MOE ---- 
  
# ---- MOE for Proportion ----
  
  # Calculate values for info box
  moe_p_df <- reactive({
     data.frame(x = 1.96 * sqrt((input$p * (1-input$p)) / input$nn),
     y = 1.96 * sqrt((input$p * (1-input$p)) / input$nn) * sqrt((input$N-input$nn) / (input$N-1))
     )
    })
  
  # Make MOE Graph, reactive fn method
  n <- reactive({input$nn})
  N <- reactive({input$N})
  p <- reactive({input$p})
  
  # Define the plotting fn
  moe_varyn_fn <- function(n) {1.96 * sqrt((p()[1] * (1-p()[1])) / n)}
  moe_varyn_fpc_fn <- function(n) {
    1.96 * sqrt((p()[1] * (1-p()[1])) / n) * (sqrt((N()[1]-n) / (N()[1]-1)))
    }
  
  # Create df to add point
  prop_data_df <- reactive({data.frame(n = c(n(), n()), 
                                    y = c(moe_varyn_fn(n()),
                                    moe_varyn_fpc_fn(n())))})
  
  # Plot MoE by sample size
  output$moe_prop_plot <- renderPlot({
      min <- input$nn
      gg <- ggplot(data.frame(n = c(min * 2/3, min * 3/2)), aes(n)) +
      scale_y_continuous(labels = percent) +
      stat_function(fun = moe_varyn_fn, geom = "line", color="blue") + 
      stat_function(fun = moe_varyn_fpc_fn, geom = "line", color="orange") +
      geom_point(data = prop_data_df(), aes(n, y), 
                 size = 4, shape = 21, color="black", fill=c("blue","orange"), alpha = .5) +
      xlab("Sample Size") + 
      ylab("Margin of Error")
      
    # Branch to choose graph style
     if(input$graphstyle == 1) {gg + theme_classic()}
      else if (input$graphstyle == 2) {gg + theme_economist()}
      else if (input$graphstyle == 3) {gg + theme_excel()}
      else if (input$graphstyle == 4) {gg + theme_few()}
      else if (input$graphstyle == 5) {gg + theme_fivethirtyeight()}
      else if (input$graphstyle == 6) {gg + theme_stata()}
      else if (input$graphstyle == 7) {gg + theme_tufte()}
    })
  
  # Create values for valueBox
  output$moe_prop <- renderValueBox({
    # Adjust the colour to show good or bad MoE
    status_colour <- ifelse(moe_p_df()[1] < .051, "green", "yellow")
    valueBox(
      value = paste("+/-", sprintf("%2.2f%%", moe_p_df()[1] * 100), " "),
      subtitle = "MOE",
      icon = icon("eye"),
      color = status_colour
    )
  })
  
  output$moe_fpc_prop <- renderValueBox({
    # Adjust the colour to show good or bad MoE
    status_colour <- ifelse(moe_p_df()[2] < .051, "green", "yellow")
    valueBox(
      value = paste("+/-", sprintf("%2.2f%%", moe_p_df()[2] * 100), " "),
      subtitle = "MOE with FPC",
      icon = icon("eye"),
      color = status_colour
    )
  })
  
  output$n_prop <- renderValueBox({
    valueBox(
      value = input$nn,
      subtitle = "Sample Size",
      icon = icon("eye")
    )
  })
  

# ---- MOE for Mean ----
  
  # Calculate values for info box
  moe_m_df <- reactive({
    data.frame(x = 1.96 * input$std / sqrt(input$n_mean),
               y = 1.96 * input$std / sqrt(input$n_mean) * sqrt((input$N_mean-input$n_mean) / (input$N_mean-1))
    )
  })
  
  # Make MOE Graph, reactive fn method
  mean <- reactive({input$mean})
  std <- reactive({input$std})
  n_mean <- reactive({input$n_mean})
  N_mean <- reactive({input$N_mean}) 
  
  # Define plotting fn
  moe_mean_varyn_fn <- function(n) {1.96 * std() / sqrt(n)}
  moe_mean_varyn_fpc_fn <- function(n) {1.96 * std()/sqrt(n) * sqrt((N_mean()-n) / (N_mean()-1))}
  
  # Create df to add point
  mean_data_df <- reactive({data.frame(n = c(n_mean(), n_mean()), 
                                    y = c(moe_mean_varyn_fn(n_mean()),
                                          moe_mean_varyn_fpc_fn(n_mean())))})
  
  # Plot MoE by sample size
  output$moe_mean_plot <- renderPlot({
    min <- input$n_mean
    gg <- ggplot(data.frame(n = c(min - .5 * min, min + .5 * min)), aes(n)) +
      scale_y_continuous() +
      stat_function(fun = moe_mean_varyn_fn, geom = "line", color = "blue") + 
      stat_function(fun = moe_mean_varyn_fpc_fn, geom = "line", color = "orange") +
      geom_point(data = mean_data_df(), aes(n, y), 
                 size = 4, shape = 21, color = "black", fill = c("blue","orange"), alpha = .5) +
      xlab("Sample Size") + 
      ylab("Margin of Error")
    
    # Branch to choose graph style
    if(input$graphstyle == 1) {gg + theme_classic()}
    else if (input$graphstyle == 2) {gg + theme_economist()}
    else if (input$graphstyle == 3) {gg + theme_excel()}
    else if (input$graphstyle == 4) {gg + theme_few()}
    else if (input$graphstyle == 5) {gg + theme_fivethirtyeight()}
    else if (input$graphstyle == 6) {gg + theme_stata()}
    else if (input$graphstyle == 7) {gg + theme_tufte()}
  })
    
  # Plot mean +/- confidence interval
  output$moe_mean_CI_plot <- renderPlot({
    
    # I think we need to switch to the t-distribution when the sample size is small
    df <- data.frame(mean = input$mean, se = input$std / sqrt(input$n_mean), 
                     z = c(2.576,1.96,1.645,1.282,0.674), CI=c("99%", "95%", "90%", "80%", "50%"))
    
    df$moe <- df$z * df$se
    
    limits <- aes(ymax = mean + moe, ymin=mean - moe)
    
    gg <- ggplot(df, aes(colour = CI, y = mean, x = CI)) +
      geom_point(size = 5) + geom_errorbar(limits, width = 0.2) + theme(legend.position = "bottom")
    
    # Branch to choose graph style
    if(input$graphstyle == 1) {gg + theme_classic()}
    else if (input$graphstyle == 2) {gg + theme_economist()}
    else if (input$graphstyle == 3) {gg + theme_excel()}
    else if (input$graphstyle == 4) {gg + theme_few()}
    else if (input$graphstyle == 5) {gg + theme_fivethirtyeight()}
    else if (input$graphstyle == 6) {gg + theme_stata()}
    else if (input$graphstyle == 7) {gg + theme_tufte()}
  })  
  
  # Create values for valueBox
  output$moe_mean <- renderValueBox({
    # Adjust the colour to show good or bad MoE
    status_colour <- ifelse(moe_m_df()[1] < .051, "green", "yellow")
    valueBox(
      value = round(moe_m_df()[1],3),
      subtitle = "MOE",
      icon = icon("eye"),
      color = status_colour
    )
  })
  
  output$moe_fpc_mean <- renderValueBox({
    # Adjust the colour to show good or bad MoE
    status_colour <- ifelse(moe_m_df()[2] < .051, "green", "yellow")
    valueBox(
      value = round(moe_m_df()[2],3),
      subtitle = "MOE with FPC",
      icon = icon("eye"),
      color = status_colour
    )
  })
  
  output$moe_std <- renderValueBox({
    valueBox(
      value = input$std,
      subtitle = "Standard Deviation",
      icon = icon("eye")
    )
  })
  
# ---- Calculate Sample Size ----   
  
# ---- Sample size for proportion ----
  
  # Make MOE Graph, reactive fn method
  dmoe <- reactive({input$dmoe})
  NN <- reactive({input$N_s})
  
  sample_need <- reactive({(as.numeric(input$z)^2*.25) / (dmoe()[1]^2)})
  sample_need_withfpc <-reactive({(sample_need() * NN()) / (sample_need() + (NN()-1))})
  
  # Create values for info box
  output$desiredmoe <- renderInfoBox({
    infoBox("Desired Margin of Error",
            paste("+/-", sprintf("%2.1f%%", dmoe() * 100), " "), icon = icon("bullseye"))
  })
  
  output$samplesize <- renderInfoBox({
    infoBox("Sample Size", round(sample_need() / input$rr), icon = icon("circle"), color = "yellow")
  })
  
  output$samplesizefpc <- renderInfoBox({
    infoBox("Sample Size with FPC", round(sample_need_withfpc() / input$rr), icon = icon("square"))
  })
  
  # Make Sample Size Graph
  # Define fn, w/o fpc
  sample_varymoe_99_fn <- function(n) {(2.576^2 * .25) / (n^2)}
  sample_varymoe_95_fn <- function(n) {(1.96^2 * .25) / (n^2)}
  sample_varymoe_90_fn <- function(n) {(1.645^2 * .25) / (n^2)}
  sample_varymoe_80_fn <- function(n) {(1.282^2 * .25) / (n^2)}
  sample_varymoe_50_fn <- function(n) {(0.674^2 * .25) / (n^2)}
  
  # Plot Sample size by confidence level
  
  output$samplesize_prop_plot <- renderPlot({
    min <- input$sample_prop_range[1]
    max <- input$sample_prop_range[2]
    gg <- ggplot(data.frame(n = c(min, max)), aes(n)) +
      stat_function(fun = sample_varymoe_99_fn, geom = "line", aes(colour = "99")) +
      stat_function(fun = sample_varymoe_95_fn, geom = "line", aes(colour = "95")) +
      stat_function(fun = sample_varymoe_90_fn, geom = "line", aes(colour = "90")) +
      stat_function(fun = sample_varymoe_80_fn, geom = "line", aes(colour = "80")) +
      stat_function(fun = sample_varymoe_50_fn, geom = "line", aes(colour = "50")) +
      xlab("Sample Size") + 
      ylab("Margin of Error") +
      theme_fivethirtyeight() +  scale_colour_manual("Confidence Level", values = c("#a1d99b", "#74c476", "#41ab5d", "#238b45", "#005a32"), label = c("50%", "80%", "90%", "95%", "99%"))
    # see http://stackoverflow.com/questions/19950219/using-legend-with-stat-function-in-ggplot2
    # to add legend to function line
    
    # Branch to choose graph style
    if(input$graphstyle == 1) {gg + theme_classic()}
    else if (input$graphstyle == 2) {gg + theme_economist()}
    else if (input$graphstyle == 3) {gg + theme_excel()}
    else if (input$graphstyle == 4) {gg + theme_few()}
    else if (input$graphstyle == 5) {gg + theme_fivethirtyeight()}
    else if (input$graphstyle == 6) {gg + theme_stata()}
    else if (input$graphstyle == 7) {gg + theme_tufte()}
    
  })
  
# ---- Sample size for mean ----
  
  # Make MOE Graph, reactive fn method
  dmean_moe <- reactive({input$dmoe_mean})
  SD <- reactive({input$SD_s})
  z2 <- reactive({as.numeric(input$z_mean)})
  N2 <- reactive({input$N_s_mean}) 
  
  # See formula in http://stattrek.com/sample-size/simple-random-sample.aspx
  sample_mean_need <- reactive({(z2()^2 * SD()^2) / dmean_moe()^2})
  sample_mean_need_withfpc <- reactive({
    (z2()^2 * SD()^2) * (N2() / (N2() - 1)) / (dmean_moe()^2 + z2()^2 * (SD()^2 / (N2() - 1)))})
  
  # Create values for info box
  output$desiredmoe_mean <- renderInfoBox({
    infoBox("Desired Margin of Error", dmean_moe(), icon = icon("bullseye"))
  })
  
  output$samplesize_mean <- renderInfoBox({
    infoBox("Sample Size", round(sample_mean_need() / input$rr_mean), icon = icon("circle"), color = "yellow")
  })
  
  # Don't know how!
  output$samplesizefpc_mean <- renderInfoBox({
    infoBox("Sample Size", round(sample_mean_need_withfpc() / input$rr_mean), icon = icon("circle"))
  })
  
  # Make Sample Size Graph
  # Define fn, w/o fpc
  sample_mean_varymoe_99_fn <- function(sd) {(2.576^2 * sd^2) / dmean_moe()^2}
  sample_mean_varymoe_95_fn <- function(sd) {(1.96^2 * sd^2) / dmean_moe()^2}
  sample_mean_varymoe_90_fn <- function(sd) {(1.645^2 * sd^2) / dmean_moe()^2}
  sample_mean_varymoe_80_fn <- function(sd) {(1.282^2 * sd^2) / dmean_moe()^2}
  sample_mean_varymoe_50_fn <- function(sd) {(0.674^2 * sd^2) / dmean_moe()^2}
  
  # Plot Sample size by confidence level
  
  output$samplesize_mean_plot <- renderPlot({
    min <- input$sample_mean_range[1]
    max <- input$sample_mean_range[2]
    gg <- ggplot(data.frame(sd = c(min, max)), aes(sd)) +
      stat_function(fun = sample_mean_varymoe_99_fn, geom = "line", aes(colour = "99")) +
      stat_function(fun = sample_mean_varymoe_95_fn, geom = "line", aes(colour = "95")) +
      stat_function(fun = sample_mean_varymoe_90_fn, geom = "line", aes(colour = "90")) +
      stat_function(fun = sample_mean_varymoe_80_fn, geom = "line", aes(colour = "80")) +
      stat_function(fun = sample_mean_varymoe_50_fn, geom = "line", aes(colour = "50")) +
      xlab("Sample Size") + 
      ylab("Margin of Error") +
      theme_fivethirtyeight() +  scale_colour_manual("Confidence Level", values = c("#a1d99b", "#74c476", "#41ab5d", "#238b45", "#005a32"), label = c("50%", "80%", "90%", "95%", "99%"))
    # see http://stackoverflow.com/questions/19950219/using-legend-with-stat-function-in-ggplot2
    # to add legend to function line
    
    # Branch to choose graph style
    if(input$graphstyle == 1) {gg + theme_classic()}
    else if (input$graphstyle == 2) {gg + theme_economist()}
    else if (input$graphstyle == 3) {gg + theme_excel()}
    else if (input$graphstyle == 4) {gg + theme_few()}
    else if (input$graphstyle == 5) {gg + theme_fivethirtyeight()}
    else if (input$graphstyle == 6) {gg + theme_stata()}
    else if (input$graphstyle == 7) {gg + theme_tufte()}
    
  })
  
}
