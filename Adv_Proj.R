library(shiny)
library(ggplot2)
library(dplyr)

#Data
birthweight_data <- readRDS("C:/Users/phili/OneDrive/Desktop/BirthWeight.rds")
birthweight_data$smoke <- as.factor(birthweight_data$smoke)

#UI
ui <- fluidPage(
  titlePanel("Birthweight vs Smoking Habits of Pregnant Women"),
  sidebarLayout(
    sidebarPanel(
      helpText("Visualizing the correlation between birthweight of infants and smoking habits of Pregnant Women."),
      sliderInput("weightRange", "Birthweight Range (oz):", 
                  min = min(birthweight_data$bwt), max = max(birthweight_data$bwt), 
                  value = c(min(birthweight_data$bwt), max(birthweight_data$bwt))),
      selectInput("smokeStatus", "Smoking Status:", 
                  choices = levels(birthweight_data$smoke), 
                  selected = levels(birthweight_data$smoke), 
                  multiple = TRUE)
    ),
    mainPanel(
      plotOutput("scatterPlot"),
      tableOutput("summaryTable"),
      h3("Statistical Metrics"),
      verbatimTextOutput("statMetrics")
    )
  )
)

#Server
server <- function(input, output) {
  
  filteredData <- reactive({
    birthweight_data %>%
      filter(bwt >= input$weightRange[1], bwt <= input$weightRange[2],
             smoke %in% input$smokeStatus)
  })
  
  output$scatterPlot <- renderPlot({
    ggplot(filteredData(), aes(x = smoke, y = bwt, color = smoke)) +
      geom_boxplot(aes(fill = smoke), alpha = 0.5) +
      geom_jitter(width = 0.2, alpha = 0.7, size = 2) +
      scale_color_manual(values = c("blue", "red")) +
      scale_fill_manual(values = c("lightblue", "lightcoral")) +
      labs(x = "Smoking Status", y = "Birthweight (oz)", 
           title = "Birthweight of Infants vs Smoking Habits of Pregnant Women") +
      theme_minimal() +
      theme(legend.position = "top")
  })
  
  output$summaryTable <- renderTable({
    filteredData() %>%
      group_by(smoke) %>%
      summarise(
        count = n(),
        mean_birthweight = mean(bwt, na.rm = TRUE),
        sd_birthweight = sd(bwt, na.rm = TRUE),
        median_birthweight = median(bwt, na.rm = TRUE)
      )
  })
  
  output$statMetrics <- renderPrint({
    data <- filteredData()
    if (nrow(data) > 1) {
      complete_cases <- data[complete.cases(data$bwt, data$smoke), ]
      if (nrow(complete_cases) > 1) {
        correlation <- cor(as.numeric(as.factor(complete_cases$smoke)), complete_cases$bwt, use = "complete.obs")
      } else {
        correlation <- NA
      }
      metrics <- data.frame(
        Metric = c("Mean Birthweight", "Median Birthweight", "Standard Deviation", 
                   "Correlation"),
        Value = c(mean(data$bwt, na.rm = TRUE),
                  median(data$bwt, na.rm = TRUE),
                  sd(data$bwt, na.rm = TRUE),
                  correlation)
      )
    } else {
      metrics <- data.frame(
        Metric = c("Mean Birthweight", "Median Birthweight", "Standard Deviation", 
                   "Correlation"),
        Value = c(NA, NA, NA, NA)
      )
    }
    print(metrics)
  })
}

#Application
shinyApp(ui = ui, server = server)

