#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)
 
# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)



 
  
  
library(shiny)
library(bslib)
library(htmltools)
 

 


ui <- fluidPage(
  theme = bs_theme(bootswatch = "lux"),
  tags$style(HTML("
    h1, h2, h3, h4, h5, h6, .card-title {
      text-transform: none !important;
    }
  ")),
  titlePanel(tags$h2("Benefit Calculator of Intensive Blood Pressure Lowering Treatment", style="font-weight:600;")),
  
  sidebarLayout(
    sidebarPanel(
      tags$h4("Patient Characteristics", style="font-weight:600;"),
      numericInput("age", "Age (years):", value = 65, min = 18, max = 100),
      numericInput("time", "Time from onset to treatment (hours):", value = 2, min = 0, max = 24),
      numericInput("sbp", "Systolic BP (mmHg):", value = 170, min = 80, max = 250),
      numericInput("nihss", "NIHSS Score:", value = 12, min = 0, max = 42),
      numericInput("vol", "Hematoma Volume (mL):", value = 15, min = 0, max = 150),
      
      selectInput("region", "Region:",
                  choices = c("China" = "China",
                              "India" = "India",
                              "South America" = "SA",
                              "Other" = "Other")),
      
      selectInput("stroke", "History of Stroke:", 
                  choices = c("No" = 0, "Yes" = 1)),
      
      selectInput("dm", "Diabetes Mellitus:", 
                  choices = c("No" = 0, "Yes" = 1)),
      
      selectInput("loc", "ICH Location:",
                  choices = c("Lobar" = "lobar",
                              "Deep" = "deep",
                              "Infratentorial" = "infra")),
      
      selectInput("ivh", "Intraventricular Hemorrhage:",
                  choices = c("No" = 0, "Yes" = 1)),
      
      actionButton("calc", "Calculate", class = "btn btn-primary btn-lg")
    ),
    
    mainPanel(
      tags$br(),
      tags$h3("Results", style="font-weight:600;"),
      
      div(class="card bg-light mb-3",
          div(class="card-body",
              verbatimTextOutput("risk0")
          )
      ),
      
      div(class="card bg-light mb-3",
          div(class="card-body",
              verbatimTextOutput("risk1")
          )
      ),
      
      div(class="card border-primary mb-3",
          div(class="card-body",
              tags$h4("Absolute Risk Reduction (ARR,%)", style="font-weight:600;"),
              tags$h3(textOutput("arr"), style="color:#0d6efd; font-weight:700;")
          )
      ),
      
      div(class="card border-success mb-3",
          div(class="card-body",
              tags$h4("Exploratory Stratification", style="font-weight:600;"),
              tags$h3(textOutput("benefit"), style="color:#198754; font-weight:700;")
          )
      ),
      
   #   div(class="card border-dark mb-3",
   #         div(class="card-body",
   #           tags$h4("Treatment Suggestion", style="font-weight:600;"),
   #           tags$h2(textOutput("suggestion"), style="color:black; font-weight:800;")
   #       ),
          
   div(class = "alert alert-warning mt-3",
       tags$b("Disclaimer: "),
       "This tool is provided for research purposes only and does not constitute clinical advice. The predictions are exploratory and should not be used to guide patient care or health policy decisions. Predictions for countries outside the regions included in this study should be interpreted with caution."
   ),
   div(
     class = "text-muted small",
     "Calculator version 1.0.0 (released October 2026)"
   )
      )
    )
  )


server <- function(input, output) {
  
  observeEvent(input$calc, {
    
    region_coef <- switch(input$region,
                          "China" = 0,
                          "India" = 0.6410,
                          "SA" = 0.7540,
                          "Other" = -0.1073)
    
    loc_coef <- switch(input$loc,
                       "lobar" = 0,
                       "deep" = 0.3046,
                       "infra" = 0.3701)
    
    compute_risk <- function(treat){
      B <- -4.7940 +
        (-0.1759 * treat) +
        0.0376 * input$age +
        -0.0806 * input$time +
        region_coef +
        0.0056 * input$sbp +
        0.0692 * input$nihss +
        0.3119 * as.numeric(input$stroke) +
        0.2789 * as.numeric(input$dm) +
        loc_coef +
        0.0342 * input$vol +
        0.4159 * as.numeric(input$ivh)
      
      1 / (1 + exp(-B))
    }
    
    risk0 <- compute_risk(0)
    risk1 <- compute_risk(1)
    
    output$risk0 <- renderText({
      paste("Risk WITHOUT intensive BP lowering:", round(100*risk0, 2), "%")
    })
    
    output$risk1 <- renderText({
      paste("Risk WITH intensive BP lowering:", round(100*risk1, 2), "%")
    })
    
    arr <- (risk0 - risk1) * 100
    
    output$arr <- renderText({
      paste(round(arr, 2), "%")
    })
    
    benefit <- ifelse(arr > 2.1, "Benefit", "No Clear Benefit")
    output$benefit <- renderText({ benefit })
    
    suggestion <- ifelse(arr > 2.1, "Yes", "No")
    output$suggestion <- renderText({ suggestion })
  })
}

shinyApp(ui, server)


 

