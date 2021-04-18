shinyUI(
    fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30),
            
            hr(),
            
            # fileInput("upload", NULL, buttonLabel = "Upload...", multiple = TRUE)

            fileInput("file", NULL, buttonLabel = "Upload...", multiple = FALSE,
                      accept = c(".csv", ".tsv")),
            numericInput("n", "Rows", value = 5, min = 1, step = 1)
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot"),
            # tableOutput("files")
            tableOutput("names"),
            tableOutput("head"),

        )
    )
)
    )
