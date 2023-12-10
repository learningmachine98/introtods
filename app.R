library(shiny)
library(randomForest)
library(tidyverse)

# Load Mushroom dataset
mushroom <- read.csv("/introtods/agaricus-lepiota.data", header = FALSE)

# Add column names
colnames(mushroom) <- c("edibility", "cap_shape", "cap_surface", "cap_color", "bruises", "odor",
                        "gill_attachement", "gill_spacing", "gill_size", "gill_color",
                        "stalk_shape", "stalk_root", "stalk_surface_above_ring",
                        "stalk_surface_below_ring", "stalk_color_above_ring",
                        "stalk_color_below_ring", "veil_type", "veil_color",
                        "ring_number", "ring_type", "spore_print_color",
                        "population", "habitat")

# Convert categorical variables to factors
mushroom <- mushroom %>% map_df(function(.x) as.factor(.x))

# Define UI
ui <- fluidPage(
  titlePanel("Random Forest Model Parameter Tuning"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("ntree", "Number of Trees:", value = 50),
      numericInput("mtry", "Number of Variables to Split on at Each Node (mtry):", value = 3),
      actionButton("update", "Update Model"),
      br(),
      textOutput("model_info")
    ),
    mainPanel(
      plotOutput("classification_plot")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive values to store the current model
  model <- reactiveValues(model = NULL)
  
  # Function to update the model
  observeEvent(input$update, {
    # Train the random forest model
    model$model <- randomForest(edibility ~ ., ntree = input$ntree, mtry = input$mtry, data = mushroom)
    
    # Display model information
    output$model_info <- renderText({
      paste("Number of Trees:", input$ntree, "\n",
            "Number of Variables to Split on at Each Node (mtry):", input$mtry)
    })
  })
  
  # Plot the classification results
  output$classification_plot <- renderPlot({
    if (!is.null(model$model)) {
      # Make predictions using the Random Forest model
      predictions <- predict(model$model, newdata = mushroom, type = "response")
      
      # Create a bar plot of predicted probabilities for each class
      ggplot(data.frame(prob = predictions, type = model$model$forest$nclass, class = levels(mushroom$edibility)),
             aes(x = class, y = prob, fill = class)) +
        geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
        labs(title = "Predicted Probabilities by Mushroom Type",
             x = "Mushroom Type",
             y = "Predicted Probability",
             fill = "Mushroom Type") +
        theme_minimal()
    }
  })
}

# Run the application
shinyApp(ui, server)

