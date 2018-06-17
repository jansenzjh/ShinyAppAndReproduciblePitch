library(shiny)
library(ggplot2)
library(dplyr)
library(rsconnect)
# Select columns to be used in the analysis
diam <- diamonds[,c(1:4,7)]
# Define server logic required to draw a plot
shinyServer(function(input, output) {
  output$distPlot <- renderPlot({
    # Based on the user input, find the related diamond data
    diam <- filter(diamonds, grepl(input$cut, cut), grepl(input$col, color), grepl(input$clar, clarity))
    # Regression
    fit <- lm( price~carat, diam)
    # check the price
    pred <- predict(fit, newdata = data.frame(carat = input$car,
                                              cut = input$cut,
                                              color = input$col,
                                              clarity = input$clar))
    # Drawing plot
    plot <- ggplot(data=diam, aes(x=carat, y = price))+
      geom_point(aes(color = cut), alpha = 0.3)+
      geom_smooth(method = "lm")+
      geom_vline(xintercept = input$car, color = "purple")+
      geom_hline(yintercept = pred, color = "orange")
    plot
  })
  
  output$result <- renderText({
    # Rendering text
    diam <- filter(diamonds, grepl(input$cut, cut), grepl(input$col, color), grepl(input$clar, clarity))
    fit <- lm( price~carat, diam)
    pred <- predict(fit, newdata = data.frame(carat = input$car,
                                              cut = input$cut,
                                              color = input$col,
                                              clarity = input$clar))
    res <- paste("$ ",round(pred, digits = 1))
    res
  })
  
})