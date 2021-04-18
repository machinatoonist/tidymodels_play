shinyServer(function(input, output) {

  output$plot <- renderPlot({
    
    # 550.5 Dealing With Toggles: Adding a check to see if the user wants to group
    if (input$wants_group) {
      group_col <- input$group_col
    } else {
      group_col <- NULL
    }
    
    # Show the plot with the dynamically chosen input columns
    heart %>%
      ggplot(aes(x = !!input$x_axis, y = cp, color = !!group_col)) +
      geom_point()
    
  })
  
  output$user_data <- renderDataTable({ heart })
  
})
