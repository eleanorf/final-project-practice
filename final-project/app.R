#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
library(ggforce)
library(readr)

d <- read_csv("tract_outcomes_simple.csv")

# Define UI for application that draws a histogram
ui <- navbarPage(
    "Final Project",
    tabPanel("Home", 
             titlePanel("Home"),
             h3("Welcome to My Page"),
             h3("About Me"),
             h4("This is Smaller Text"),
             a("Google", href = "https://google.com")),
    tabPanel("Model",
             fluidPage(
                 selectInput("x", "X variable", choices = names(d)),
                 selectInput("y", "Y variable", choices = names(d)),
                 selectInput("geom", "geom", c("point", "column", "jitter", "line")),
                 plotOutput("plot")
             )),
    tabPanel("Discussion",
             titlePanel("Discussion Title"),
             p("Tour of the modeling choices you made and 
              an explanation of why you made them")),
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("Hello! I am planning on doing my final project in the realm of 
               Criminal Justice. I have not yet found the data that I will use 
               for my final project, but I have data that I am using for this 
               Milestone. Kayla has been helping me reach out to organizations 
               that I can potentially work with for the final project. The data
               that I am using today is from", 
               a("Opportunity Insights", 
                 href = "https://opportunityinsights.org/data/"),
               "called Household Income and Incarceration for Children from 
               Low-Income Households by Census Tract, Race, and Gender."),
             h3("About Me"),
             h4("Hello!"),
             p("My name is Eleanor Fitzgibbons and I study Government. You can 
             reach me at efitzgibbons@college.harvard.edu. This is a link to 
               my", a("repo.", 
                      href = "https://github.com/eleanorf/final-project")))
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    plot_geom <- reactive({
        switch(input$geom,
               point = geom_point(),
               column = geom_col(),
               #smooth = geom_smooth(se = TRUE, na.rm = TRUE),
               jitter = geom_jitter(),
               line = geom_line()
        )
    })
    
    output$plot <- renderPlot({
        ggplot(d, aes(.data[[input$x]], .data[[input$y]])) +
            plot_geom()
    }, res = 96)
}

# Run the application 
shinyApp(ui = ui, server = server)
