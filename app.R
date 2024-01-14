# CLEAR ENVIRONMENT AND REMOVE PREVIOUS PACKAGES
quickcode::clean(clearPkgs = TRUE)

# LOAD NEW PACKAGES AND PRINT VERSIONS
libraryAll(shiny,TidyDensity,ggplot2,dplyr,DT, magrittr,tidyr,plyr)

# APP BODY
# Define UI
ui <- fluidPage(
  titlePanel("TidyDensity App"),
  sidebarLayout(
    sidebarPanel(
      radioButtons(inputId = "data_input_type",
                   label = "Data Input Type:",
                   choices = c("Select Function", "Enter Data"),
                   selected = "Select Function"),
      conditionalPanel(
        condition = "input.data_input_type == 'Enter Data'",
        textInput(inputId = "data",
                  label = "Enter data as a comma-separated list of numeric values")
      ),
      conditionalPanel(
        condition = "input.data_input_type == 'Select Function'",
        selectInput(inputId = "functions",
                    label = "Select Function",
                    choices = c(
                      "tidy_normal", 
                      "tidy_bernoulli", 
                      "tidy_beta", 
                      "tidy_gamma"
                    )
        )
      ),
      numericInput(inputId = "num_sims",
                   label = "Number of simulations:",
                   value = 1,
                   min = 1,
                   max = 15),
      numericInput(inputId = "n",
                   label = "Sample size:",
                   value = 50,
                   min = 30,
                   max = 200),
      selectInput(inputId = "plot_type",
                  label = "Select plot type",
                  choices = c(
                    "density",
                    "quantile",
                    "probability",
                    "qq",
                    "mcmc"
                  )
      ),
      downloadButton(outputId = "download_data", label = "Download Data")
    ),
    mainPanel(
      plotOutput("density_plot"),
      DT::dataTableOutput("data_table")
    )
  )
)
# Define server
server <- function(input, output) {
  
  # Create reactive data
  data <- reactive({
    # Call selected function with user input or tidy_empirical if user entered data
    if (input$data_input_type == "Enter Data") {
      data <- input$data
      if (is.null(data) || data == "") {
        return(NULL)
      }
      data <- as.numeric(strsplit(data, ",")[[1]])
      tidy_empirical(data)
    } else {
      match.fun(input$functions)(.num_sims = input$num_sims, .n = input$n)
    }
  })
  
  # Create density plot
  output$density_plot <- renderPlot({
    # Call autoplot on reactive data
    if (!is.null(data())) {
      p <- data() |>
        tidy_autoplot(.plot_type = input$plot_type)
      
      print(p)
    }
  })
  
  # Create data table
  output$data_table <- DT::renderDataTable({
    # Return reactive data as a data table
    if (!is.null(data())) {
      DT::datatable(data())
    }
  })
  
  # Download data handler
  output$download_data <- downloadHandler(
    filename = function() {
      if (input$data_input_type == "Enter Data") {
        paste0("tidy_empirical.csv")
      } else {
        paste0(input$functions, ".csv")
      }
    },
    content = function(file) {
      write.csv(data(), file, row.names = FALSE)
    }
  )
  
}
# Run the app
shinyApp(ui = ui, server = server)
