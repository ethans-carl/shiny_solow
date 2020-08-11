library(shiny)
library(ggplot2)
library(latex2exp)
library(markdown)
library(tibble)
library(tidyr)
library(readr)
library(purrr)
library(stringr)
library(forcats)
library(tidyverse)

#library(gridExtra)

ui <- fluidPage(withMathJax(),
                title = "Exploring Growth Data",
                plotOutput('scatterplot'),
                #titlePanel(h1('Exploring growth data')),
                #sidebarLayout(
                fluidRow(
                  #sidebarPanel(
                    p(
                      'This widget explores some data from the Penn World Tables version 9.1 that are related to variables in the Solow model'
                    ),
                    p(
                      'You can select which variables and years to compare using the dropdown menus'
                    ),
                    column(3,
                    selectInput("yr", "Year:", 1950:2017, selected ="1960")
                    ),
                    column(4,
                    selectInput(
                      "xvar",
                      "Horizonal (x) variable:",
                      c(
                        "Real GDP/Worker" = "rgdp_worker",
                        "Human capital/worker" = "hc",
                        "Capital per worker" = "k_worker",
                        "Consumption per worker" = "c_worker",
                        "Capital's share (alpha)" = "alpha",
                        "Depreciation share (delta)" = "delta",
                        "Saving rate (gamma)" = "s",
                        "Total factor productivity" = "tfp"
                      )
                    ),
                   
                    selectInput(
                      "yvar",
                      "Vertical (y) variable:",
                      c(
                        "Real GDP/Worker" = "rgdp_worker",
                        "Human capital/worker" = "hc",
                        "Capital per worker" = "k_worker",
                        "Consumption per worker" = "c_worker",
                        "Capital's share (alpha)" = "alpha",
                        "Depreciation share (delta)" = "delta",
                        "Saving rate (gamma)" = "s",
                        "Total factor productivity" = "tfp"
                      ),
                      selected = "k_worker"
                    )),
                    column(4,
                   # numericInput("xmin", "X-axis minimum (number)", 0),
                  #  numericInput("xmax", "X-axis maximum (number)", 5e4),
                  #  numericInput("ymin", "Y-axis minimum (number)", 0),
                  #  numericInput("ymax", "Y-axis maximum (number)",5e4),
                    selectInput(
                      "overlap",
                      "Turn off overlapping labels?",
                      c("No" = FALSE, "Yes" = TRUE)
                    )
                  ))
                )



server <- function(input, output)
{
  pwt <- read.csv("pwt_subset_for_principles.csv")
  plotdat <- reactive(plotdat <- filter(pwt, year == input$yr))
                       

  
  # xminno <- reactive({
  #   as.numeric(input$xmin)
  # })
  # 
  # xmaxno <- reactive({
  #   as.numeric(input$xmax)
  # })
  # 
  # yminno <- reactive({
  #   as.numeric(input$ymin)
  # })
  # 
  # ymaxno <- reactive({
  #   as.numeric(input$ymax)
  # })
  
  
  
  
  xlab <- reactive(input$xvar)

  xlabeltxt <-
    reactive(if (xlab() == "hc") {
      "Human capital/worker"
    } else if (xlab() == "rgdp_worker") {
      "Real GDP per worker"
    } else if (xlab() == "k_worker") {
      "Capital per worker"
    } else if (xlab() == "c_worker") {
      "Consumption per worker"
    } else if (xlab() == "delta") {
      "Rate of depreciation"
    } else if (xlab() == "s") {
      "Saving rate"
    } else if (xlab() == "tfp") {
      "Total factor productivity (A)"
    }
    else {
      xlab()
    })

  #

  ylab <- reactive(input$yvar)

  ylabeltxt <-
    reactive(if (ylab() == "hc") {
      "Human capital/worker"
    } else if (ylab() == "rgdp_worker") {
      "Real GDP per worker"
    } else if (ylab() == "k_worker") {
      "Capital per worker"
    } else if (ylab() == "c_worker") {
      "Consumption per worker"
    } else if (ylab() == "delta") {
      "Rate of depreciation"
    } else if (ylab() == "s") {
      "Saving rate"
    } else if (ylab() == "tfp") {
      "Total factor productivity (A)"
    }
    else {
      ylab()
    })
  
  
  #output$scatterplot = renderPlot({ggplot(plot.default)
  
  
   pwt_scatter_plot <- function(xvar,
                                yvar,
                                xmax = 5e5,
                                ymax = 1e5,
                                ylabeltxt = "foo",
                                xlabeltxt = "bar",
                                overlap = FALSE)
   {
     #plotdat <- filter(pwt, year == yr)
     ggplot(plotdat(),
            aes_string(x = xvar, y = yvar, label = "countrycode")) +
       geom_text(check_overlap = overlap) +
       geom_point(alpha = .2) +
        labs( x= xlabeltxt(), y = ylabeltxt()) +
     #  xlab(xlabeltxt()) 
     #+ ylab(ylabeltxt()) +
     # coord_cartesian(xlim = c(xminno(), xmaxno()) ,
     #                ylim =  c(yminno(), ymaxno())) +
       theme_minimal()
     }
   
  
  output$scatterplot =   renderPlot({
    pwt_scatter_plot(
      xvar = input$xvar,
      yvar = input$yvar,
      xmax = xmax,
      ymax = ymax,
      xlabeltxt = xlabeltxt(),
      #    ylabeltxt = ylabeltxt(),
      overlap = input$overlap
    )
  })
  
  
  
}

shinyApp(ui = ui, server = server)
