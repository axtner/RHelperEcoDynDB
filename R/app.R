library(shiny)
library(shinyjs)
library(shinyFiles)

# Define UI
ui <- fluidPage(
  titlePanel("PCR App"),
  sidebarLayout(
    sidebarPanel(
      shinyDirButton("dir", "Select Output Directory", "Upload"),
      actionButton("run_pcr", "Run PCR")
    ),
    mainPanel(
      textOutput("output_text")
    )
  )
)



# Define server logic
server <-  function(input, output, session) {
  shinyDirChoose(input, "dir", roots = c(wd = getwd()))
  
  output$output_text <- renderText({
    req(input$dir)
    isolate({
      # Set the selected directory as the output directory
      output_dir <- input$dir$datapath
      
      # Print the selected directory (you can remove this line in the final version)
      cat("Selected directory:", output_dir, "\n")
      
      # Run your existing function when the "Run PCR" button is clicked
      if (input$run_pcr > 0) {
        # Pass the selected directory as an argument to createPcr function
        createPcr(out_dir = output_dir)
        return("PCR run completed!")
      }
    })
  })
}
  
  
  function(input, output) {
  # Include your existing script here
  source("D:/BioDivCloud/1_12_scripts_tutorials/RHelperEcoDynDB/R/createPcr.R", local = TRUE)
  
  # Create a reactive expression for the output text
  output$output_text <- renderText({
    isolate({
      # Run your existing function when the "Run PCR" button is clicked
      if (input$run_pcr > 0) {
        createPcr()
        return("PCR run completed!")
      }
    })
  })
}

# Run the Shiny app
shinyApp(ui, server)
