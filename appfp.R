library(shiny)
library(tidyverse)
library(dslabs)
library(hrbrthemes)
data(gapminder)

ui = fluidPage(
    titlePanel("A Tale of Living Longer and Living Too Long"),
    tabsetPanel(
        tabPanel("Infant Mortality against Per Capita Income",
                 sidebarLayout(
                     sidebarPanel(
                         sliderInput("yearim",
                                     label = "Select a year",
                                     min = min(gapminder$year),
                                     max = 2011,
                                     value = 1960, 
                                     step=3, 
                                     sep = "", 
                                     ticks = FALSE, 
                                     animate = TRUE)),
                     mainPanel(
                         plotOutput("scatterPlot1")))),
        
        tabPanel("Life expectancy against Per Capita Income",
                 sidebarLayout(
                     sidebarPanel(
                         sliderInput("yearle",
                                     label = "Select a year",
                                     min = min(gapminder$year),
                                     max = 2011,
                                     value = 1960, 
                                     step=3, 
                                     sep = "", 
                                     ticks = FALSE, 
                                     animate = TRUE)),
                     mainPanel(
                         plotOutput("scatterPlot2"))))))

server = function(input, output) {
    
    output$scatterPlot1 = renderPlot({
        filter(gapminder, year == input$yearim, !is.na(gdp)) %>%
            ggplot(aes(x = (gdp/ population), y = infant_mortality, color = continent)) +
            geom_point() +
            xlim(c(0, 65000))+
            ylim(c(0,250))+
            xlab("Per Capita Income") +
            ylab("Infant Mortality")+
            theme_ipsum() +
            theme(legend.position="bottom")})
    
    
    output$scatterPlot2 = renderPlot({
        filter(gapminder, year == input$yearle,  !is.na(gdp)) %>%
            ggplot(aes(x = (gdp/ population), y = life_expectancy, color = continent)) +
            geom_point() +
            xlim(c(0, 65000))+
            ylim(c(25,90))+
            xlab("Per Capita Income") +
            ylab("Life expectancy")+
            theme_ipsum() +
            theme(legend.position="bottom")})}

shinyApp(ui = ui, server = server)
