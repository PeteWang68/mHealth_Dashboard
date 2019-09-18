library(shiny)
library(tidyverse)
phonecall = read.csv("./data/phonecall.csv")
text = read.csv("./data/text.csv")
patient_info = read.csv("./data/patient_info.csv")

ui <- fluidPage(
  
  titlePanel("prototype"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      selectizeInput('SUBJID', 'Subject ID', choices = c(1,2,3,4,5)),

      selectizeInput('type', 'Data Type', choices = c("phonecall", "text")),

      radioButtons("time", "Time",
                   c("Hour",
                     "Day",
                     "Week",
                     "Month")),

      selectizeInput('day', 'DAY', choices = c(1:365)),
      sliderInput("dayrange", "Day Range:",
                  min = 1, max = 365,
                  value = 7:365),
      sliderInput("weekrange", "Week Range:",
                  min = 1, max = 52,
                  value = 2:52),
      sliderInput("monthrange", "Month Range:",
                  min = 1, max = 12,
                  value = 1:12)
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Patient Info", tableOutput("info")),
                  tabPanel("Plot", plotOutput("plot")),
                  tabPanel("Social Interaction", plotOutput("network"))
      )
      
    )
  )
)

# Define server logic for random distribution app ----
server <- function(input, output) {
  
  output$info = renderTable(
    patient_info[input$SUBJID,]
  )

  
  output$plot <- renderPlot({
    if (input$type == "phonecall" & input$time == "Hour") {
    num = 365 * (as.numeric(input$SUBJID) - 1) + as.numeric(input$day)

    phonecall[num, -1] %>% t() %>% cbind(., hour = c(1:24)) %>% as.data.frame() %>% mutate(value = .[,1]) %>% 
      select(hour, value) %>% 
      ggplot(aes(x = hour, y = value)) +
      geom_line() +
      labs(
        title = "Distribution by Hour",
        x = "Hours",
        y = "Number of variable"
      )
    } else if (input$type == "text" & input$time == "Hour") {
      num = 365 * (as.numeric(input$SUBJID) - 1) + as.numeric(input$day)
      
      text[num, -1] %>% t() %>% cbind(., hour = c(1:24)) %>% as.data.frame() %>% mutate(value = .[,1]) %>% 
        select(hour, value) %>% 
        ggplot(aes(x = hour, y = value)) +
        geom_line() +
        labs(
          title = "Distribution by Hour",
          x = "Hours",
          y = "Number of variable"
        )
    } else if (input$type == "phonecall" & input$time == "Day") {
      min = 365 * (as.numeric(input$SUBJID) - 1) + as.numeric(min(input$dayrange))
      max = 365 * (as.numeric(input$SUBJID) - 1) + as.numeric(max(input$dayrange))
      
      phonecall[min:max, -1] %>% cbind(., value = rowSums(.), days = c(min:max)) %>% 
        ggplot(aes(x = days, y = value)) +
        geom_line() +
        labs(
          title = "Distribution by Day",
          x = "Days",
          y = "Number of variable"
        )
    } else if (input$type == "text" & input$time == "Day") {
      min = 365 * (as.numeric(input$SUBJID) - 1) + as.numeric(min(input$dayrange))
      max = 365 * (as.numeric(input$SUBJID) - 1) + as.numeric(max(input$dayrange))
      
      text[min:max, -1] %>% cbind(., value = rowSums(.), days = c(min:max)) %>% 
        ggplot(aes(x = days, y = value)) +
        geom_line() +
        labs(
          title = "Distribution by Day",
          x = "Days",
          y = "Number of variable"
        )
    } else if (input$type == "phonecall" & input$time == "week") {
      min = 365 * (as.numeric(input$SUBJID) - 1) + 7 * (as.numeric(min(input$weekrange)) - 1) + 1
      max = 365 * (as.numeric(input$SUBJID) - 1) + 7 * (as.numeric(max(input$weekrange)) - 1) + 1
      num = as.numeric(max(input$weekrange)) - as.numeric(min(input$weekrange)) + 1
      
      data = phonecall[min:max, -1] %>% cbind(., value = rowSums(.))
      newdata = c(1:num) %>% as.data.frame()
      
      for (i in 1:num) {
        newdata[i, 2] = colSums(data[(7*(i-1)+1):(7*(i-1)+7),24:25]) %>% .[[2]]  
      }
      
      newdata %>% cbind(., weeks = c(as.numeric(min(input$weekrange)):as.numeric(max(input$weekrange)))) %>% 
        ggplot(aes(x = weeks, y = V2)) +
        geom_line() +
        labs(
          title = "Distribution by Week",
          x = "Weeks",
          y = "Number of variable"
        )
    }
  })
}

# Create Shiny app ----
shinyApp(ui, server)