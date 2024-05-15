# ---- 1. Libraries ----
#install.packages(c("tidyverse", "ggplot2", "shiny", "cluster", "arules" , "arulesViz"))
library(tidyverse)
library(ggplot2)
library(shiny)
library(cluster)
library(arules)
library(arulesViz)

# ---- 2. Load Data Function (with validation) ----
loadData <- function(dataPath) {
  if (!file.exists(dataPath)) {
    stop("File not found. Please check the provided path.")
  }
  
  tryCatch({
    data <- read.csv(dataPath, stringsAsFactors = TRUE) # Important for arules
    data <- data %>% na.omit() # Remove rows with NA values
    return(data)
  }, error = function(err) {
    stop("Error loading data. Please ensure it's a valid CSV with correct structure.")
  })
}

# ---- 3. UI Definition ----
ui <- fluidPage(
  titlePanel("Data Analysis App"),
  sidebarLayout(
    sidebarPanel(
      # Dataset Path - with a check button to validate the path
      textInput("datasetPath", "Dataset Path", value = "", placeholder = "Enter full path to CSV file"),
      actionButton("checkPath", "Check Path"), # Button to check the file path
      
      numericInput("nClustersInput", "Number of Clusters", value = 3, min = 2, max = 4, step = 1),
      
      # Apriori parameters with range validation
      numericInput("minSupportInput", "Min. Support", value = 0.01, min = 0.001, max = 1, step = 0.01),
      numericInput("minConfidenceInput", "Min. Confidence", value = 0.5, min = 0.001, max = 1, step = 0.01),
      
      selectInput("clusteringColumns", "Columns for Clustering", choices = NULL, multiple = TRUE),
      selectInput("assocColumns", "Columns for Association Rules", choices = NULL, multiple = TRUE),
      
      selectInput("selectedPaymentTypes", "Select Payment Types", 
                  choices = c("Cash", "Credit"), 
                  selected = c("Cash", "Credit"), 
                  multiple = TRUE),
      
      actionButton("runClustering", "Run Clustering"),
      actionButton("runAssociationRules", "Run Association Rules"),
      
      br(),
      actionButton("saveData", "Save Data"),
      actionButton("addData", "Add Data"),
      
      # Fields for adding new data
      textInput("nameInput", "Name"),
      numericInput("ageInput", "Age", value = 25),
      numericInput("totalInput", "Total Spending", value = 100),
      textInput("cityInput", "City"),
      selectInput("paymentTypeInput", "Payment Type", choices = c("Cash", "Credit"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Dashboard", plotOutput("dashboard")),
        tabPanel("Payment Comparisons", 
                 plotOutput("ageComparison"), 
                 plotOutput("cityComparison")
        ),
        tabPanel("Clustering", plotOutput("clusteringPlot"), tableOutput("clusterTable")),
        tabPanel("Association Rules", verbatimTextOutput("rulesOutput"), plotOutput("rulesPlot")),
        tabPanel("Data Table", tableOutput("dataTable"))
      )
    )
  )
)

# ---- 4. Server Logic ----
server <- function(input, output, session) {
  userData <- reactiveValues(data = NULL, dataPath = NULL)
  
  # Validate the dataset path when the button is pressed
  observeEvent(input$checkPath, {
    req(input$datasetPath)
    
    if (file.exists(input$datasetPath)) {
      userData$dataPath <- input$datasetPath
      userData$data <- loadData(input$datasetPath)
      showNotification("Path is valid and data loaded successfully.", type = "message")
      
      updateSelectInput(session, "clusteringColumns", choices = names(userData$data))
      updateSelectInput(session, "assocColumns", choices = names(userData$data))
    } else {
      showNotification("Invalid file path. Please enter a valid path to the CSV file.", type = "error")
    }
  })
  
  # Add new data entered by user
  observeEvent(input$addData, {
    req(userData$data) # Ensure there's data to add to
    
    newDataRow <- data.frame(
      name = input$nameInput,
      age = input$ageInput,
      total = input$totalInput,
      city = input$cityInput,
      paymentType = input$paymentTypeInput
    )
    
    userData$data <- rbind(userData$data, newDataRow)
  })
  
  # Save data to the original file
  observeEvent(input$saveData, {
    req(userData$data, userData$dataPath)
    
    write.csv(userData$data, userData$dataPath, row.names = FALSE) # Save data to CSV
  })
  
  # Clustering 
  observeEvent(input$runClustering, {
    req(userData$data, input$nClustersInput, input$clusteringColumns)
    
    clustering_data <- userData$data[, input$clusteringColumns, drop = FALSE]
    cluster_result <- kmeans(clustering_data, centers = input$nClustersInput, nstart = 10)
    
    # Add cluster information to the data
    userData$data$cluster <- cluster_result$cluster
    
    # Plot clustering results
    output$clusteringPlot <- renderPlot({
      ggplot(clustering_data, aes(x = clustering_data[, 1], y = clustering_data[, 2], color = as.factor(cluster_result$cluster))) +
        geom_point() +
        labs(title = "Clustering with K-means", color = "Cluster")
    })
    
    # Show clustering results
    output$clusterTable <- renderTable({
      userData$data
    })
  })
  
  # Association Rules 
  observeEvent(input$runAssociationRules, {
    req(userData$data, input$minSupportInput, input$minConfidenceInput, input$assocColumns)
    
    # Convert data to "transactions"
    trans_data <- as(userData$data[, input$assocColumns], "transactions")
    
    # Apply Apriori algorithm
    rules <- apriori(trans_data, parameter = list(support = input$minSupportInput, confidence = input$minConfidenceInput))
    
    # Display top rules
    output$rulesOutput <- renderPrint({
      if (length(rules) == 0) {
        "No association rules found. Try adjusting the support or confidence values."
      } else {
        inspect(rules[1:5]) # Display the first 5 rules
      }
    })
    
    # Visualize the rules
    output$rulesPlot <- renderPlot({
      if (length(rules) > 0) {
        plot(rules, method = "graph", engine = "igraph")
      }
    })
  })
  
  # Generate plots 
  output$dashboard <- renderPlot({
    req(userData$data, input$selectedPaymentTypes)
    
    # Check if the 'paymentType' column exists in the dataset
    if ("paymentType" %in% names(userData$data)) {
      # Filter data by selected payment types
      data_filtered <- userData$data %>% filter(paymentType %in% input$selectedPaymentTypes)
      
      # Basic scatter plot with payment type coloring
      ggplot(data_filtered, aes(x = age, y = total, color = paymentType)) +
        geom_point() +
        labs(title = "Age vs Total Spending by Payment Type") +
        theme_minimal()
    } else {
      # If 'paymentType' column doesn't exist, display a message or handle it accordingly
      ggplot() + labs(title = "PaymentType column not found") + theme_minimal()
    }
  })
  
  # Comparison plots 
  output$ageComparison <- renderPlot({
    req(userData$data, input$selectedPaymentTypes)
    
    data_filtered <- userData$data %>% filter(paymentType %in% input$selectedPaymentTypes)
    
    ggplot(data_filtered, aes(x = paymentType, y = age)) +
      geom_boxplot() +
      labs(title = "Age Distribution by Payment Type") +
      theme_minimal()
  })
  
  output$cityComparison <- renderPlot({
    req(userData$data, input$selectedPaymentTypes)
    
    data_filtered <- userData$data %>% filter(paymentType %in% input$selectedPaymentTypes)
    
    ggplot(data_filtered, aes(x = paymentType, fill = city)) +
      geom_bar(position = "dodge") +
      labs(title = "Cities by Payment Type") +
      theme_minimal()
  })
  
  # Display the entered data in the 'Data Table' tab
  output$dataTable <- renderTable({
    userData$data
  })
}

# ---- 5. Run the Shiny App ----
shinyApp(ui = ui, server = server)
# G:\project data.csv