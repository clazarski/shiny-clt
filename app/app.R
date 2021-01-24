## ###########################################################
##
## CLT/app.R
##
## Educational shiny app to teach students the ideas
## around the central limit theorem
##
## @author: Craig Lazarski & Jeffery Painter
## @modified: 2021-Jan-24
##
## ###########################################################

library(shiny)
library(ggplot2)

# These are the sample distributions the student will explore
# They are labelled 'A', 'B', etc because we don't want to reveal
# the distribution to the student, but rather, they will use
# this app to explore the properties of each, and try to see
# how their behavior differs under various input parameters
myDistributions = c(
  "A" = 1,
  "B" = 2,
  "C" = 3,
  "D" = 4
)

ui <- navbarPage(
  title = "CLT Explorer",
  
  ## ###########################################################
  ## Panel 1
  ## ###########################################################
  tabPanel(
    "Guess the distribution ",
    
    sidebarLayout(
      sidebarPanel(
        em("Select a distribution and sample size"),
        selectInput(
          inputId = "panel1_input_distribution",
          label = "Distribution",
          myDistributions,
          selected = 1
        ),
        
        sliderInput(
          inputId = "panel1_input_sample_size",
          "Sample size:",
          min = 0,
          max = 50,
          value = 10
        ),
        
        actionButton(inputId = "panel1_btn_graph", label = "Graph Samples"),
      ),
      
      mainPanel(
        # output
        plotOutput(outputId = "panel1_plot"),
        includeHTML("www/task_01.html"),
      )
    ),
    
  ),
  ## ###########################################################
  ## Panel 2
  ## ###########################################################
  tabPanel(
    "Confirm the distribution ",
    
    sidebarLayout(
      sidebarPanel(
        em("Select a distribution and sample size"),
        selectInput(
          inputId = "panel2_input_distribution",
          label = "Distribution",
          myDistributions,
          selected = 1
        ),
        
        sliderInput(
          inputId = "panel2_input_sample_size",
          "Sample size:",
          min = 0,
          max = 1000,
          value = 10
        ),
        
        actionButton(inputId = "panel2_btn_graph", label = "Graph Samples")
      ),
      
      mainPanel(
        # output
        plotOutput(outputId = "panel2_plot"),
        includeHTML("www/task_02.html"),
      )
    ),
    
  ),
  ## ###########################################################
  ## Panel 3
  ## ###########################################################
  tabPanel("CLT ",
           
           sidebarLayout(
             sidebarPanel(
               em("Select a distribution and sample size"),
               selectInput(
                 inputId = "panel3_input_distribution",
                 label = "Distribution",
                 myDistributions,
                 selected = 1
               ),
               
               sliderInput(
                 inputId = "panel3_input_sample_size",
                 "Sample size:",
                 min = 1,
                 max = 50,
                 value = 1
               ),
               
               sliderInput(
                 inputId = "panel3_input_simulations",
                 "Number of samples",
                 min = 1,
                 max = 100,
                 value = 1
               ),
               
               actionButton(inputId = "panel3_btn_graph", label = "Update Graph"),
               
               # text output
               tableOutput(outputId  = "panel3_parameters"),
               tableOutput(outputId  = "panel3_statistics"),
               
               # Allow student to choose what is displayed on the graph
               checkboxInput(inputId = "panel3_checkbox_show_dist", "Sampling Distribution", value = FALSE),
               checkboxInput(inputId = "panel3_checkbox_show_samples", "Samples", value = TRUE)
             ),
             
             mainPanel(
               # output
               plotOutput(outputId = "panel3_plot"),
               includeHTML("www/task_03.html")
             )
           )
  ),
  ## ###########################################################
  ## Panel 4
  ## ###########################################################
  tabPanel("CLT SD ",
           
           sidebarLayout(
             sidebarPanel(
               
               em("Select a distribution and sample size"),
               selectInput(
                 inputId = "panel4_input_distribution",
                 label = "Distribution",
                 myDistributions,
                 selected = 1
               ),
               
               sliderInput(
                 inputId = "panel4_input_sample_size",
                 "Sample size:",
                 min = 1,
                 max = 50,
                 value = 1
               ),
               
               sliderInput(
                 inputId = "panel4_input_standard_deviation",
                 "Sampling distribution Standard Deviation",
                 min = 0,
                 max = 5,
                 value = 0,
                 step = .05
               ),
               
               # text output
               tableOutput(outputId = "panel4_parameters"),
               tableOutput(outputId = "panel4_statistics")
             ),
             
             mainPanel(
               # output
               plotOutput(outputId = "panel4_plot"),
               includeHTML("www/task_04.html")
             )
           )
  ),
  tabPanel("About",
           includeHTML("www/about.html"))
)


## ###########################################################
## Global functions
## ###########################################################

## ###########################################################
##  getSampleData() will create a data.frame
##    containing a set of samples based on the
##    parent distribution selected
## ###########################################################
getSampleData <- function(distId, total_samples) {
  df = NULL
  my_df_column = c("x")
  
  if ( distId == 1 )
  {
    # normal distribution
    df = as.data.frame(rnorm(n = total_samples, mean = 5, sd = 5))
    colnames(df) = my_df_column
    return(df)
  }
  
  if ( distId == 2 )
  {
    # uniform distribution
    df = as.data.frame(runif(n = total_samples, min = -5, max = 15))
    colnames(df) = my_df_column
    return(df)
  }
  
  if ( distId == 3 )
  {
    # chi-square distribution
    df = as.data.frame(rchisq(n = total_samples, df = 12.5))
    colnames(df) = my_df_column
    return(df)
  }
  
  if ( distId == 4 )
  {
    # bimodal distribution
    d1 = rnorm(n = total_samples / 2, mean = 0, sd = 5 )
    d2 = rnorm(n = total_samples / 2, mean = 10, sd = 5 )
    df = as.data.frame(c( d1, d2 ) )    
    colnames(df) = my_df_column
    return(df)
  }
  
}

## ###########################################################
getPopulationParameters <- function(distId) {
  # set the population mean for each distribution
  population_means = c(5, 5, 12.5, 5)
  popmean = population_means[[distId]]
  popsd = 5
  
  popval = as.data.frame(cbind(popmean, popsd))
  colnames(popval) = c("Population mean", "Population SD")
  return(popval)
}


## ###########################################################
##  Sample data points used through out the app
## ###########################################################
points = as.data.frame(seq(from = -15,
                           to = 30,
                           by = .25))
colnames(points) = c("x")

getBinWidth <- function(total_samples) 
{
  bw = 1
  if (total_samples <= 100) {
    bw = .8
  }
  if (total_samples > 100 && total_samples <= 400) {
    bw = .5
  }
  if (total_samples > 400 && total_samples <= 600) {
    bw = .45
  }
  if (total_samples > 600 && total_samples <= 800) {
    bw = .4
  }
  if (total_samples > 800 && total_samples <= 1000) {
    bw = .35
  } else {
    bw = 0.3
  }
  
  return(bw)
}

## ###########################################################
## End shared functions
## ###########################################################


server <- function(input, output, session) {
  ## #################################
  ## Panel 1 input/outputs
  ## #################################
  # INPUT
  #    panel1_input_distribution
  #    panel1_input_sample_size
  #    panel1_btn_graph
  ## #################################
  # OUTPUT
  #   panel1_plot
  ## #################################
  
  # Updates ONLY when the button is clicked in panel_1
  panel1_rv <- eventReactive(input$panel1_btn_graph, {
    distId = as.numeric(input$panel1_input_distribution)
    total_samples = input$panel1_input_sample_size
    df = getSampleData(distId, total_samples)
    return(df)
  })
  
  
  # ggplot our data frame
  output$panel1_plot <- renderPlot({
    total_samples = isolate(input$panel1_input_sample_size)
    
    ggplot(panel1_rv(), aes(x)) +
      ggtitle("Distribution") +
      theme_classic() +
      theme(
        plot.title = element_text(
          lineheight = 0.8,
          size = 20,
          face = "bold"
        ),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      ) +
      coord_cartesian(xlim = c(-10, 25)) +
      geom_dotplot(binwidth = 1, fill = "195190") +
      labs(title = "Distribution of samples") +
      scale_y_continuous(breaks = NULL)
  })
  
  ######################################################
  # Panel 2
  ######################################################
  
  ## #################################
  ## Panel 2 input/outputs
  ## #################################
  # INPUT
  #    panel2_input_distribution
  #    panel2_input_sample_size
  #    panel2_btn_graph
  ## #################################
  # OUTPUT
  #   panel2_plot
  ## #################################
  
  panel2_rv <- eventReactive(input$panel2_btn_graph, {
    distId = as.numeric(input$panel2_input_distribution)
    total_samples = (input$panel2_input_sample_size)
    df = getSampleData(distId, total_samples)
    return(df)
  })
  
  # ggplot our data frame
  output$panel2_plot <- renderPlot({
    total_samples = isolate(input$panel2_input_sample_size)
    my_binwidth = getBinWidth(total_samples)
    
    ggplot(panel2_rv(), aes(x)) +
      ggtitle("Distribution") +
      theme_classic() +
      theme(
        plot.title = element_text(
          lineheight = 0.8,
          size = 20,
          face = "bold"
        ),
        axis.title.y = element_blank(),
        axis.text.y = element_blank()
      ) +
      coord_cartesian(xlim = c(-10, 25)) +
      geom_dotplot(binwidth = my_binwidth)
  })
  
  
  ######################################################
  # Panel 3
  ######################################################
  
  ## #################################
  ## Panel 3 input/outputs
  ## #################################
  # INPUT
  #    panel3_input_distribution
  #    panel3_input_sample_size
  #    panel3_input_simulations
  #    panel3_checkbox_show_dist
  #    panel3_checkbox_show_samples
  ## #################################
  # OUTPUT
  #    panel3_parameters
  #    panel3_statistics
  #    panel3_plot
  ## #################################
  
  # Shared data
  mydata <- reactiveValues(
    df = NULL,
    add_dist = FALSE,
    add_sample_data = TRUE,
    current_dist = 1
  )
  
  observeEvent(input$panel3_btn_graph, {
    updateGraph3Data()
  })
  
  # Update the graph data here
  updateGraph3Data <- function()
  {
    #
    # Read in the current distribution and update
    # the shared reactive variable used in ggplot
    #
    # We need this because when we change sample
    # size, we want to show the sampling distribution
    # but not change it if the dist drop down is changed
    distId = as.numeric(input$panel3_input_distribution)
    mydata$current_dist = distId
    
    total_samples = input$panel3_input_sample_size
    sims  = input$panel3_input_simulations
    
    if (sims > 0) {
      if (distId == 1) {
        df = data.frame()
        for (index in 1:sims) {
          df_temp = as.data.frame(mean(rnorm(
            n = total_samples,
            mean = 5,
            sd = 5
          )))
          colnames(df_temp) = c("x")
          df = rbind(df, df_temp)
        }
        mydata$df = df
        
      } else if (distId == 2) {
        df = data.frame()
        for (index in 1:sims) {
          df_temp = as.data.frame(mean(runif(
            n = total_samples,
            min = -7.3,
            max = 10
          )))
          colnames(df_temp) = c("x")
          df = rbind(df, df_temp)
        }
        mydata$df = df
        return()
      } else if (distId == 3) {
        df = data.frame()
        for (index in 1:sims) {
          df_temp = as.data.frame(mean(rchisq(n = total_samples, df = 12.5)))
          colnames(df_temp) = c("x")
          df = rbind(df, df_temp)
        }
        mydata$df = df
        
      } else if (distId == 4) {
        df = data.frame()
        for (index in 1:sims) {
          # bimodal distribution
          a = mean(rnorm(n = total_samples / 2, mean = 0, sd = 5 ))
          b = mean(rnorm(n = total_samples / 2, mean = 10, sd = 5 ))
          c = (a + b) / 2
          df1_temp = as.data.frame(c)
          colnames(df1_temp) = c("x")
          df = rbind(df, df1_temp)
        }
        mydata$df = df
      }
    }
    else{
      mydata$df = NULL
    }
    return()
  }
  
  
  # Should we add the parent distribution?
  observeEvent(input$panel3_checkbox_show_dist, {
    status = input$panel3_checkbox_show_dist
    if (status == TRUE) {
      mydata$add_dist = TRUE
    } else {
      mydata$add_dist = FALSE
    }
    
  })
  
  # should we show the sample data?
  observeEvent(input$panel3_checkbox_show_samples, {
    status = input$panel3_checkbox_show_samples
    if (status == TRUE) {
      mydata$add_sample_data = TRUE
    } else {
      mydata$add_sample_data = FALSE
    }
  })
  
  addPopulationDist <- function(plot, distId) {
    sf = NULL
    if (distId == 1) {
      sf = stat_function(fun = dnorm, data = points, n = 101, args = list(mean = 5, sd = 5), color = 'red')
      plot = plot + sf
    }
    
    if (distId == 2) {
      sf =  stat_function(fun = dunif, data = points, n = 101, args = list(min = -7.3, max = 10), color = 'red')
      plot = plot + sf
    }
    
    if (distId == 3) {
      sf =  stat_function(fun = dchisq, data = points, n = 101, args = list(df = 12.5), color = 'red')
      plot = plot + sf
    }
    
    if (distId == 4) {
      sf =   stat_function(
        fun = dnorm,
        data = points,
        n = 101,
        args = list(mean = 0, sd = 5),
        color = 'red'
      )
      plot = plot + sf
      
      sf =  stat_function(
        fun = dnorm,
        data = points,
        n = 101,
        args = list(mean = 10, sd = 5),
        color = 'red'
      )
      plot = plot + sf
      
      
    }
    return(plot)
  }
  
  # ggplot our data frame
  output$panel3_plot <- renderPlot({
    
    sims = isolate(input$panel3_input_simulations)
    
    # read from last known distribution in shared reactive variable
    distId = mydata$current_dist
    
    # this is NOT isolated, because you want to redraw the sample dist
    total_samples = input$panel3_input_sample_size
    my_mean = 5
    population_means = c(5, 1.35, 12.5, 5)
    my_mean = population_means[[distId]]
    
    if (is.null(mydata$df)) {
      df = data.frame(c(-30))
      colnames(df) = c("x")
      mydata$df = df
    }
    
    # to modify graph
    my_binwidth = .55
    
    p1 = ggplot(mydata$df, aes(x)) +
      ggtitle("Distribution") +
      theme_classic() +
      theme(
        plot.title = element_text(
          lineheight = 0.8,
          size = 20,
          face = "bold"
        ),
        axis.title.y = element_blank(),
        axis.text.y = element_blank()
      ) +
      coord_cartesian(xlim = c(-10, 35), ylim = c(0, .6)) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
      labs(title = "Parent Distribution vs Distribution of sample mean", x = "Distribution of sample means")
    
    # Add the populationn distribution    
    p1 = addPopulationDist(p1, distId)
    
    # Do we want to show the sample data?
    if (mydata$add_sample_data == TRUE) {
      p1 = p1 + geom_dotplot(binwidth = my_binwidth)
    }
    
    # Do we want to show the sampling distribution?
    if (mydata$add_dist == TRUE) {
      p1 = p1 + stat_function(
        fun = dnorm,
        data = points,
        n = 101,
        args = list(mean = my_mean, sd = 5 / sqrt(total_samples)),
        size = 1.5,
        linetype = "dashed",
        color = 'blue'
      )
    }
    return(p1)
  })
  
  output$panel3_parameters <- renderTable({
    # what is our current distribution?
    distId = as.numeric(input$panel3_input_distribution)
    popval = getPopulationParameters(distId)
    return(popval)
  })
  
  # This panel relies on shared data from the underlying distribution
  output$panel3_statistics <- renderTable({
    df = mydata$df
    distId = mydata$current_dist
    total_samples = isolate(input$panel3_input_sample_size)
    
    if (distId == 4 && total_samples == 1) {
      df = as.data.frame(cbind(NA, NA))
      colnames(df) = c("Mean of samples", "SD of samples")
      
    } else {
      if (!is.null(df)) {
        avg = round(mean(df$x), 4)
        mysd = (sd(df$x))
        if (avg > -30) {
          df = as.data.frame(cbind(avg, mysd))
        }
        
        else {
          df = as.data.frame(cbind(NA, NA))
        }
        colnames(df) = c("Mean of samples", "SD of samples")
      }
    }
    return(df)
  })
  
  ######################################################
  # Panel 4
  ######################################################
  
  ## #################################
  ## Panel 4 input/outputs
  ## #################################
  # INPUT
  #    panel4_input_distribution
  #    panel4_input_sample_size
  #    panel4_input_standard_deviation
  ## #################################
  # OUTPUT
  #    panel4_parameters
  #    panel4_statistics
  #    panel4_plot
  ## #################################
  
  # ggplot our data frame
  output$panel4_plot <- renderPlot({
    distId = as.numeric(input$panel4_input_distribution)
    my_mean = 5
    population_means = c(5, 1.35, 12.5, 5)
    my_mean = population_means[[distId]]
    my_sd = input$panel4_input_standard_deviation
    
    # this is NOT isolated so that the sample dist updates any time the
    # slider is moved
    total_samples = input$panel4_input_sample_size
    
    # empty data frame to start
    df = data.frame(c(-30))
    colnames(df) = c("x")
    
    p1 = ggplot(df, aes(x)) +
      ggtitle("Distribution") +
      theme_classic() +
      theme(
        plot.title = element_text(
          lineheight = 0.8,
          size = 20,
          face = "bold"
        ),
        axis.title.y = element_blank(),
        axis.text.y = element_blank()
      ) +
      coord_cartesian(xlim = c(-10, 35), ylim = c(0, .6)) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
      labs(title = "Parent Distribution vs Distribution of sample mean", x = "Distribution of sample means")
    
    p1 = addPopulationDist(p1, distId)
    p1 = p1 + stat_function(
      fun = dnorm,
      data = points,
      n = 101,
      args = list(mean = my_mean, sd = 5 / sqrt(total_samples)),
      size = 1.5,
      linetype = "dashed",
      color = 'blue'
    )
    
    p1 = p1 + stat_function(
      fun = dnorm,
      data = points,
      n = 101,
      args = list(mean = my_mean, sd = my_sd),
      size = 1.5,
      linetype = "solid",
      color = 'dark green'
    ) +
      annotate(
        "text",
        x = 20,
        y = .5,
        color = "red",
        label = "Population Distribution",
        size = 6
      ) +
      annotate(
        "text",
        x = 20,
        y = .45,
        color = "blue",
        label = "Sampling Distribution",
        size = 6
      ) +
      annotate(
        "text",
        x = 20,
        y = .4,
        color = "dark green",
        label = "Guessing Distribution",
        size = 6
      )
    
    return(p1)
  })
  
  output$panel4_parameters <- renderTable({
    # what is our current distribution?
    distId = as.numeric(input$panel4_input_distribution)
    popval = getPopulationParameters(distId)
    return(popval)
  })
}

shinyApp(ui = ui, server = server)