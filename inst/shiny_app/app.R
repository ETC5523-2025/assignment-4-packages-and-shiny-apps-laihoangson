# inst/shiny_app/app.R

library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(plotly)

# Define the UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),

  # App title
  titlePanel("AUSTRALIAN COVID-19 QUARANTINE EXPLORER"),

  # Sidebar layout with input and output definitions
  sidebarLayout(
    # Sidebar panel for user inputs
    sidebarPanel(
      selectInput(
        inputId = "state_select",
        label = "Select a State/Territory:",
        choices = c(
          "All States",
          unique(QuarantineAnalysis::breaches_data$state)
        ),
        selected = "All States"
      ),
      hr(),

      sliderInput(
        "date_slider",
        "Select Date Range for Line Chart:",
        min = min(QuarantineAnalysis::data_time$report_date),
        max = max(QuarantineAnalysis::data_time$report_date),
        value = c(min(QuarantineAnalysis::data_time$report_date),
                  max(QuarantineAnalysis::data_time$report_date)),
        step = 1,
        timeFormat = "%Y-%m-%d"
      ),
      hr(),

      h4("Field Descriptions"),
      p(strong("State:"), "The Australian state or territory represented in the dataset."),
      p(strong("Month/Date:"), "The month or the date the event or risk estimate was recorded."),
      p(strong("Total Risk:"), "The modelled likelihood that the quarantine system could lead to spread into the community."),
      p(strong("Breaches:"), "Individual quarantine incidents officially reported by state health authorities."),
      p(strong("Variant:"), "The COVID-19 variant associated with the breach."),
      p(strong("Onward:"), "Whether the breach led to known community transmission (TRUE/FALSE).")
    ),

    # Main panel for displaying outputs
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Timeline Plots",

          # Line Plot: Total Risk Time Series
          plotlyOutput(outputId = "total_risk_plot"),
          hr(),
          h4("How to Interpret This Line Chart"),
          div(class = "explanation-text",
              p("This line chart shows the daily estimated quarantine risk across Australia. You can use the dropdown on the left to filter the data for a specific state or view all states combined. You can also use the date slider on the left to filter the time period of interest")
          ),
          hr(),

          # Bar plot for breaches
          plotlyOutput(outputId = "breach_plot"),
          hr(),
          h4("How to Interpret This Bar Chart"),
          div(class = "explanation-text",
              p("This bar chart shows the number of quarantine breaches recorded each month. You can use the dropdown on the left to filter the data for a specific state or view all states combined. This visualization helps to identify periods with a higher frequency of breaches.")
          )
        ),
        tabPanel(
          "Data Table",
          # The data table also changes based on the input
          DT::dataTableOutput(outputId = "breaches_table"),
          hr(),
          h4("How to Interpret This Table"),
          div(class = "explanation-text",
              p("This table displays the raw data for the selected state(s). You can also use the search box to filter results further or sort columns by clicking on the headers.")
          )
        )
      )
    )
  )
)

# Define the server logic
server <- function(input, output) {

  # Reactive data
  filtered_data <- reactive({
    breaches <- QuarantineAnalysis::breaches_data
    risk <- QuarantineAnalysis::data_time

    # Convert to Date
    start_date <- as.Date(input$date_slider[1])
    end_date <- as.Date(input$date_slider[2])

    # Filter by state
    if (input$state_select != "All States") {
      breaches <- breaches |> filter(state == input$state_select)
      risk <- risk |> filter(state == input$state_select)
    }

    # Filter risk plot by the selected date range
    risk <- risk |> filter(report_date >= start_date, report_date <= end_date)

    list(breaches = breaches, risk = risk)
  })

  # Risk line plot
  # This plot shows the daily estimated quarantine risk
  output$total_risk_plot <- renderPlotly({
    req(filtered_data()$risk)

    # If "All States" selected, only use AUS (national total)
    if (input$state_select == "All States") {
      plot_data <- filtered_data()$risk |> filter(state == "AUS")
      title_text <- "Daily Quarantine Risk in Australia"
    } else {
      plot_data <- filtered_data()$risk
      title_text <- paste("Daily Quarantine Risk in", input$state_select)
    }

    # Plot by date
    risk_data <- plot_data %>%
      arrange(report_date) %>%
      select(report_date, total)

    plot_ly(
      data = risk_data,
      x = ~report_date,
      y = ~total,
      type = "scatter",
      mode = "lines+markers",
      line = list(color = "#1f77b4", width = 2),
      marker = list(size = 4, color = "#1f77b4"),
      hoverinfo = "text",
      text = ~paste(
        "Date:", format(report_date, "%Y-%m-%d"),
        "<br>Total risk:", round(total, 2)
      )
    ) %>%
      layout(
        title = title_text,
        xaxis = list(title = "Date", tickangle = -45),
        yaxis = list(title = "Estimated Total Risk"),
        hovermode = "x unified"
      )
  })

  # Breach bar plot
  # This bar chart shows the number of recorded quarantine breaches each month.
  output$breach_plot <- renderPlotly({
    req(filtered_data()$breaches)

    plot_data <- filtered_data()$breaches %>%
      mutate(year_month = format(date, "%Y-%m")) %>%
      count(year_month)

    plot_ly(
      data = plot_data,
      x = ~year_month,
      y = ~n,
      type = "bar",
      marker = list(color = "#007bff", opacity = 0.7),
      text = ~paste("Number of breaches:", n),
      hoverinfo = "text",
      textposition = "none"
    ) %>%
      layout(
        title = paste("Monthly Quarantine Breaches in", input$state_select),
        xaxis = list(title = "Month", tickangle = -45),
        yaxis = list(title = "Number of Breaches",
                     tickformat = ".0f", tickmode = "linear", dtick = 1)
      )
  })

  # Data Table
  # This table displays the raw data for the filtered breaches dataset.
  output$breaches_table <- DT::renderDataTable({
    DT::datatable(
      filtered_data()$breaches,
      options = list(pageLength = 10),
      caption = htmltools::tags$caption(
        style = 'caption-side: top; text-align: left; font-weight: bold; font-size: 16px; color: black;',
        "QUARANTINE BREACHES DATA"
      )
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
