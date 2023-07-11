library(shiny)

# Soil texture classification function
soil_texture <- function(sand, silt, clay) {
  if (86 <= sand && sand <= 100 && 0 <= silt && silt <= 14 && 0 <= clay && clay <= 10) {
    return("Sand")
  } else if (70 <= sand && sand <= 86 && 0 <= silt && silt <= 30 && 0 <= clay && clay <= 15) {
    return("Loamy sand")
  } else if (44 <= sand && sand <= 85 && 0 <= silt && silt <= 50 && 0 <= clay && clay <= 20) {
    return("Sandy loam")
  } else if (23 <= sand && sand <= 52 && 28 <= silt && silt <= 50 && 7 <= clay && clay <= 27) {
    return("Loam")
  } else if (20 <= sand && sand <= 50 && 50 <= silt && silt <= 88 && 0 <= clay && clay <= 27) {
    return("Silty loam")
  } else if (0 <= sand && sand <= 20 && 80 <= silt && silt <= 100 && 0 <= clay && clay <= 15) {
    return("Silt")
  } else if (20 <= sand && sand <= 45 && 15 <= silt && silt <= 40 && 27 <= clay && clay <= 40) {
    return("Clay loam")
  } else if (45 <= sand && sand <= 80 && 0 <= silt && silt <= 28 && 20 <= clay && clay <= 35) {
    return("Sandy clay loam")
  } else if (0 <= sand && sand <= 20 && 40 <= silt && silt <= 72 && 28 <= clay && clay <= 40) {
    return("Silty clay loam")
  } else if (45 <= sand && sand <= 65 && 0 <= silt && silt <= 20 && 35 <= clay && clay <= 55) {
    return("Sandy clay")
  } else if (0 <= sand && sand <= 20 && 40 <= silt && silt <= 60 && 40 <= clay && clay <= 60) {
    return("Silty clay")
  } else if (0 <= sand && sand <= 45 && 0 <= silt && silt <= 40 && 40 <= clay && clay <= 100) {
    return("Clay")
  } else {
    return("Texture class could not be determined")
  }
}

# Define UI
ui <- fluidPage(
  titlePanel("Soil Texture Classifier"),
  sidebarLayout(
    sidebarPanel(
      numericInput("sand", "Percentage of Sand:", min = 0, max = 100, value = 0),
      numericInput("silt", "Percentage of Silt:", min = 0, max = 100, value = 0),
      numericInput("clay", "Percentage of Clay:", min = 0, max = 100, value = 0),
      actionButton("classify_button", "Classify"),
      textOutput("texture_output")
    ),
    mainPanel(
      p("Enter the percentages of sand, silt, and clay to determine the soil texture class."),
      p("The soil texture class will be displayed below after classification.")
    )
  )
)

# Define server
server <- function(input, output) {
  observeEvent(input$classify_button, {
    # Get input percentages
    sand <- input$sand
    silt <- input$silt
    clay <- input$clay
    
    # Determine soil texture class based on input percentages
    texture_class <- soil_texture(sand, silt, clay)
    
    # Display the determined soil texture class
    output$texture_output <- renderText(paste("The soil texture class is:", texture_class))
  })
}

# Run the app
shinyApp(ui = ui, server = server)
