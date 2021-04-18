# This data dashboard should have the following interface:
# - Choose columns to place on the x- and y-axis of a plot
# - Filter the data down to specific values
# - Choose a column to group on
# - Show what the underlying data looks like to the user
# - Visualize the data as an informative plot to the user

shinyUI(fluidPage(
    
    # 459.4: Coding The Main Layout
    # In the lines below, give the user interface a sidebarLayout
    sidebarLayout(
        sidebarPanel(
            varSelectInput(inputId = "x_axis",
                           label = "Choose a column from the data:",
                           data = heart),
            checkboxInput(inputId = "wants_group", 
                          label = "Enable grouping?:", 
                          value = FALSE,),
            varSelectInput(inputId = "group_col",
                           label = "Choose a column to group on:",
                           data = heart),
            checkboxInput(inputId = "wants_table", 
                          label = "Show underlying data?:", 
                          value = FALSE,)
        ),
        mainPanel( 
            plotOutput("plot"),
            conditionalPanel(
                condition = "input.wants_table",
                dataTableOutput("user_data")
                )
            )
    )
    
    
))
