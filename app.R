library(shiny)
library(shinythemes)

# Define UI ----
ui <- fluidPage(theme = shinytheme("cerulean"),
  titlePanel("Dose Finding Activity"),
  
  # create widgets for dose selection list and Submit button
  sidebarLayout(
    sidebarPanel(selectInput("dose", h5("Choose a dose for the next patient"), 
                             choices = list("Dose 1", 
                                            "Dose 2",
                                            "Dose 3", 
                                            "Dose 4",
                                            "Dose 5"), 
                             selected = "Dose 1"),
                 actionButton(inputId = "submit",
                              label = "Submit"),
                 br(),
                 br(),
                 div(textOutput("maximum"), style = "color:red"),
                 br(),
                 tags$button("Restart", id="restart", type="button", onclick="history.go(0)"),
                 br(),
                 br(),
                 br(),
                 br(),
                 h5("Dose Information"),
                 p("Dose 1: 10 milligrams (mg)"),
                 p("Dose 2: 20 mg"),
                 p("Dose 3: 30 mg"),
                 p("Dose 4: 40 mg"),
                 p("Dose 5: 50 mg")),
    mainPanel(tableOutput("patients_table"))
  )
)


# Define server logic ----
server <- function(input, output) {
  
  # warning text under dose selection button
  output$maximum <-
    renderText("Maximum number of patients: 30")
  
  # create a table with nice column names to display before Submit button is pressed
  first_table <- data.frame(
    patient = "NA",
    dose = "NA",
    side_effects = "NA")
  
  colnames(first_table) <- c("Patient", "Dose", "Side Effects?")
  
  output$patients_table  <-
    renderTable(first_table[-1, ], width = "100%")
  
  # create and name reactive values
  values <- reactiveValues()
  
  values$patient <- 0
  values$dose <- "NA"
  values$side_effects <- "NA"
  
  # create our main data frame cdf
  values$cdf <- data.frame(
    patient = as.integer(0),
    dose = "NA",
    side_effects = "NA")
  
  # define what should happen when Submit button is pressed
  observeEvent(input$submit, {
    if (values$patient < 30) {    # stops at max number of patients
      if (input$dose == "Dose 1") {
        dose_outcome <- rbinom(1, 1, 0.05)
      }
      else if (input$dose == "Dose 2") {
        dose_outcome <- rbinom(1, 1, 0.10)
      }
      else if (input$dose == "Dose 3") {
        dose_outcome <- rbinom(1, 1, 0.33)
      }
      else if (input$dose == "Dose 4") {
        dose_outcome <- rbinom(1, 1, 0.5)
      }
      else if (input$dose == "Dose 5") {
        dose_outcome <- rbinom(1, 1, 0.8)
      }
      else {
        dose_outcome <- "else"
      }
    
      if (dose_outcome == 0) {
        side_effects <- "No side effects"
      }
      else if (dose_outcome == 1) {
        random_side_effect = sample(1:3, 1)
        if (random_side_effect == 1){
          side_effects <- "Severe diarrhea"
        }
        else if (random_side_effect == 2){
          side_effects <- "Severe nausea"
        }
        else {
          side_effects <- "Severe nausea and diarrhea"
        }
      }
      else {
        side_effects <- "Error"
      }
    
      selected_dose = input$dose
    
      values$patient = values$patient + 1
  
      # create new line with updated values
      newLine <- c(
        values$patient,
        selected_dose,
        side_effects)
  
      # append new line to data frame cdf
      values$cdf[nrow(values$cdf) + 1, ] <- newLine
      colnames(values$cdf) <- c("Patient", "Dose", "Side Effects?")
      
      # recreate table output
      # table does not display our first "Patient 0" row
      output$patients_table  <-
        renderTable(values$cdf[-1, ], striped = TRUE, spacing = "xs", width = "100%")

    }
    # output message when max number of patients is reached
    else {
      output$maximum <-
        renderText("You have treated the maximum number of patients.")
    }
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
