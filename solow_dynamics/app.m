library(shiny)
library(ggplot2)
library(latex2exp)
library(markdown)
library(gridExtra)



ui <- fluidPage(withMathJax(),
                titlePanel(h1('Showing the dynamic path of the economy in the Solow model')),
                sidebarLayout(
                  sidebarPanel(
                    p(
                      'How does the economy evolve from one steady state to another? Assume the production function is $$ y_t = f(A, k_t, h) = A k^{\\alpha} h^{1-\\alpha} $$'
                    ),
                    p(
                      'And the capital accumulation equation is $$ k_{t+1} =k_t + \\gamma y_t - \\delta k_{t} $$'
                    ),
                    p('Remember the steady state condition is that $$ k_t = k_{t-1}$$ which occurs when  $$ \\gamma y_t = \\delta k_t $$'),
                    p('Below you can pick different values; hit the "Plot and calculate" button to draw the new steady state (you can use this to quiz yourself!)'),
                    actionButton("plot", "Plot and calculate"),
                    h3("Parameters"),
                    sliderInput(
                      "A1",
                      "Initial Productivity  (\\(A\\) )",
                      min = 1,
                      max = 1700,
                      value = 355
                    ),
                    sliderInput(
                      "A2",
                      "New Productivity  (\\(A\\) )",
                      min = 1,
                      max = 1700,
                      value = 355
                    ),
                    sliderInput(
                      "h1",
                      "Initial human capital per worker  (\\(h\\) )",
                      min = 0.001,
                      max = 5,
                      value = 2.5
                    ),
                    sliderInput(
                      "h2",
                      "New human capital per worker  (\\(h\\) )",
                      min = 0.001,
                      max = 5,
                      value = 2.5
                    ),
                    sliderInput(
                      "alpha1",
                      "Initial \\(\\alpha\\)",
                      min = 0.001,
                      max = .999,
                      value = .33
                    ),
                    sliderInput(
                      "alpha2",
                      "New \\(\\alpha\\)",
                      min = 0.001,
                      max = .999,
                      value = .33
                    ),
                    sliderInput(
                      "delta1",
                      "Initial depreciation fraction (\\(\\delta\\) )",
                      min = 0.001,
                      max = .999,
                      value = .2
                    ),
                    sliderInput(
                      "delta2",
                      "New depreciation fraction (\\(\\delta\\) )",
                      min = 0.001,
                      max = .999,
                      value = .2
                    ),
                    sliderInput(
                      "saverate1",
                      "Initial savings rate (\\( \\gamma \\) )",
                      min = 0.001,
                      max = .999,
                      value = .25
                    ),
                    sliderInput(
                      "saverate2",
                      "New savings rate (\\( \\gamma \\) )",
                      min = 0.001,
                      max = .999,
                      value = .35
                    )
                  ),
                  mainPanel(
                    h1("Steady state values:"),
                    uiOutput('solow_soln'),
                    h1("Solution plots:"),
                    plotOutput('solow_plot')
                    #      h1("Transition over time"),
                    #      plotOutput('timepathplot')
                  )
                ))
server <- function(input, output) {
  # Now write functions that
  # 0. See if the button's been pushed.  If it's been pushed, do all the calculations
  # 1. Based on parameters, calculate the steady state level of output, capital, and consumption per worker
  # 2. Plot the steady state and label it
  
  
  # steadystates$kstar1() <-  ((input$saverate1 * input$A1 * input$h1 ^ (1 - input$alpha1)) / input$delta1) ^ (1 / (1 - input$alpha1))
  # steadystates$ystar1 <- input$A1 * steadystates$kstar1() ^ input$alpha1 * input$h1 ^ (1 - input$alpha1)
  # steadystates$cstar1 <- (1 - input$saverate1) *   steadystates$ystar1()
  # steadystates$istar1 <- (input$saverate1) * steadystates$ystar1()
  # steadystates$kstar2 <-  ((input$saverate2 * input$A2 * input$h2 ^ (2 - input$alpha2)) / input$delta2) ^ (2 / (2 - input$alpha2))
  # steadystates$ystar2 <- input$A2 * steadystates$kstar2() ^ input$alpha2 * input$h2 ^ (2 - input$alpha2)
  # steadystates$cstar2 <- (2 - input$saverate2) *   steadystates$ystar2()
  # steadystates$istar2 <- (input$saverate2) * steadystates$ystar2()
  
  
  
  
  
  kstar1 <- reactive({
    input$plot
    
    isolate({
      ((input$saverate1 * input$A1 * input$h1 ^ (1 - input$alpha1)) / input$delta1) ^
        (1 / (1 - input$alpha1))
    })
  })
  
  ystar1 <- reactive({
    input$plot
    
    isolate({
      input$A1 * kstar1() ^ input$alpha1 * input$h1 ^ (1 - input$alpha1)
    })
  })
  cstar1 <- reactive({
    input$plot
    isolate({
      (1 - input$saverate1) * ystar1()
    })
  })
  istar1 <- reactive({
    input$plot
    
    isolate({
      (input$saverate1) * ystar1()
    })
  })
  
  kstar2 <- reactive({
    input$plot
    isolate({
      ((input$saverate2 * input$A2 * input$h2 ^ (1 - input$alpha2)) / input$delta2) ^
        (1 / (1 - input$alpha2))
    })
  })
  ystar2 <- reactive({
    input$plot
    
    isolate({
      input$A2 * kstar2() ^ input$alpha2 * input$h2 ^ (1 - input$alpha2)
    })
  })
  
  
  cstar2 <- reactive({
    input$plot
    isolate({
      (1 - input$saverate2) * ystar2()
    })
  })
  
  istar2 <- reactive({
    input$plot
    isolate({
      (input$saverate2) * ystar2()
    })
  })
  
  
  ## PATHS
  
  
  
  kpath <- reactive({
    input$plot
    isolate({
 #     browser()
      diff = 100
      kp = kstar1()
      ii =  1
      while (diff > 0.1) {
        ii = ii + 1
        kp[ii] = (1 - input$delta2) * kp[ii - 1] + input$saverate2 * input$A2 *
          kp[ii - 1] ^ (input$alpha2) * input$h2 ^ (1 - input$alpha2)
        diff = abs(kp[ii] - kstar2())
      }
      out <- kp
    })
  })
  


    ypath <- reactive({
      #   browser()
      input$plot
      isolate({
      input$A2 * kpath() ^ (input$alpha2) * input$h2 ^ (1 - input$alpha2)
      })
    })
  #   
  #   # cpath <- reactive({
  #   #   #     browser()
  #   #   (1 - input$saverate2) * ypath()
  #   # })
  #   # 
    ipath <- reactive({
      #     browser()
      input$plot
      isolate({
      input$saverate2 * ypath()
      })
    })

    deppath <- reactive({
      #     browser()
      input$plot
      isolate({
      input$delta2 * kpath()
    })

  })

    solow_df <- reactive({
      input$plot
      isolate({
      onpath <- rep(1, length(kpath()))
      df <- data.frame(kpath(), ypath(), ipath(), deppath(), onpath)
    #  browser()
      rbind(data.frame(kpath.. = kstar1(), ypath.. = ystar1(), ipath.. = istar1(), deppath.. = istar1(), onpath = 0), df)
      })
    })

  
  
  
  
  
  output$solow_soln <- renderUI({
    input$plot
    
    isolate({
      withMathJax(
        sprintf(
          "The steady state values are: $$ \\begin{array}{l|c|c}
          \\text{Steady state value} & \\text{Initial steady state value} & \\text{New Steady State value} \\\\ \\hline
          k^* = \\left( \\frac{ \\gamma A h^{1-\\alpha}}{\\delta} \\right)^{\\frac{1}{1- \\alpha}} & %.2f & %.2f \\\\
          y^* = A (k^*)^{\\alpha} h^{1-\\alpha} & %.2f & %.2f \\\\
          c^* = (1 - \\gamma) y^* & %.2f & %.2f \\\\
          i^* = \\gamma y^* &  %.2f &  %.2f
          \\end{array} $$",
          kstar1(),
          kstar2(),
          ystar1(),
          ystar2(),
          cstar1(),
          cstar2(),
          istar1(),
          istar2()
        )
      )
    })
  })
  
  
  
  
  
  
  
  
  
  
  
  output$solow_plot <- renderPlot({
    input$plot
    
    isolate({
      
      ggplot(solow_df()) +
        stat_function(
          fun = function(kpath..)
            isolate(input$delta1) * kpath..,
          #  geom = "line",
          aes(colour = "Initial Depreciation", linetype = "Initial Depreciation"),
          #  linetype = "dashed",
          size = 1.25
        ) +
        stat_function(
          fun = function(kpath..)
            isolate(input$A1) * kpath.. ^ (isolate(input$alpha1)) * isolate(input$h1) ^ (1 - isolate(input$alpha1)),
          #    geom = "point",
          aes(colour = "Initial Production", linetype = "Initial Production"),
          #   linetype = "dashed",
          size = 1.25
        ) +
        stat_function(
          fun = function(kpath..)
            isolate(input$saverate1) * isolate(input$A1) * kpath.. ^ (isolate(input$alpha1)) * isolate(input$h1) ^ (1 - isolate(input$alpha1)),
          aes(colour = "Initial Investment", linetype = "Initial Investment"),
          #  geom = "point",
          #  linetype = "dashed",
          size = 1.25
        ) +
        stat_function(
          fun = function(kpath..)
            isolate(input$delta2) * kpath..,
          #    geom = "line",
          aes(colour = "New Depreciation", linetype = "New Depreciation"),
          #  linetype = "solid",
          size = 1.25
        ) +
        stat_function(
          fun = function(kpath..)
            isolate(input$A2) * kpath.. ^ (input$alpha2) * isolate(input$h2) ^ (1 - input$alpha2),
          aes(colour = "New Production", linetype = "New Production"),
          #  linetype = "solid",
          size = 1.25
        ) +
        stat_function(
          fun = function(kpath..)
            isolate(input$saverate2) * isolate(input$A2) * kpath.. ^ (input$alpha2) * isolate(input$h2) ^ (1 - input$alpha2),
          aes(colour = "New Investment", linetype = "New Investment"),
          #    linetype = "solid",
          size = 1.25
        ) +
        geom_path(data = solow_df(), aes(kpath.., y = ypath..), size = 2, arrow = arrow() 
        )+
        geom_path(data = solow_df(), aes(kpath.., y = ipath..), size = 2, arrow = arrow() 
        )+
        geom_path(data = solow_df(), aes(kpath.., y = deppath..), size = 2, arrow = arrow() 
        )+
        #  stat_function(
        #      fun = kpath(),
        #      aes(colour = "New Investment", linetype = "New Investment"),
        #   )+
        geom_segment(
          x = kstar1(),
          y = 0,
          xend = kstar1(),
          yend = ystar1(),
          linetype = "dotted"
        ) +
        geom_segment(
          x = 0,
          y = ystar1(),
          xend = kstar1(),
          yend = ystar1(),
          linetype = "dotted"
        ) +
        geom_segment(
          x = 0,
          y = istar1(),
          xend = kstar1(),
          yend = istar1(),
          linetype = "dotted"
        ) +
        geom_segment(
          x = kstar2(),
          y = 0,
          xend = kstar2(),
          yend = ystar2(),
          linetype = "dotted"
        ) +
        geom_segment(
          x = 0,
          y = ystar2(),
          xend = kstar2(),
          yend = ystar2(),
          linetype = "dotted"
        ) +
        geom_segment(
          x = 0,
          y = istar2(),
          xend = kstar2(),
          yend = istar2(),
          linetype = "dotted"
        )  +
        theme_classic() +
        theme(legend.position = "left") +
        scale_linetype_manual(name = "Guide", values = c("dotdash", "dotdash", "dotdash", "solid", "solid", "solid")
        ) +
        #scale_size_manual("Function", breaks=c("Initial Depreciation", "Initial Production", "Initial Investment", "New Depreciation", "New Production", "New Investment"), values=c(1.5,1.5,1.5, 1.5,1.5,1.5)) +
        scale_colour_manual(name = "Guide",
                            #    "Function",
                            values = c("red", "blue", "#336600", "red", "blue", "#336600"))+
        
        scale_x_continuous(expand = c(0, 0), limits = c(0, 1.5 * max(kstar1(), kstar2()))) +
        scale_y_continuous(expand = c(0, 0), limits = c(0, 1.25 * max(ystar1(), ystar2()))) +
        xlab(TeX('Capital per worker, $k$')) +
        ylab(
          TeX(
            'Output per worker($y$),Depreciation ($\\delta k$), Investment per worker($i$)'
          )
        ) +
        labs(caption = "Dot-Dashed lines indicate equations with initial values, solid lines are with new values")
    })
  })
  
}



shinyApp(ui = ui, server = server)
