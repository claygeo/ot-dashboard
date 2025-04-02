# Load required libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(lubridate)
library(shinythemes)

# Load the datasets with error handling
tryCatch({
  weekly_metrics <<- read.csv("data/weekly_metrics.csv", stringsAsFactors = FALSE)
  employees <<- read.csv("data/employees.csv", stringsAsFactors = FALSE)
  equipment_breakages <<- read.csv("data/equipment_breakages.csv", stringsAsFactors = FALSE)
  
  # Data preprocessing
  weekly_metrics$week_end <<- as.Date(weekly_metrics$week_end)
  equipment_breakages$date <<- as.Date(equipment_breakages$date)
}, error = function(e) {
  stop("Error loading data files: ", e$message)
})

# Define departments for the filter (including "All")
departments <- c("All", unique(weekly_metrics$department))

# Define UI with custom styling
ui <- navbarPage(
  title = div(
    style = "color: white; font-size: 24px; font-weight: bold;",
    "Overtime and Productivity Dashboard"
  ),
  theme = shinytheme("flatly"),
  header = tags$head(
    tags$style(HTML("
      body { background-color: #F5F5F5; }
      .navbar { background-color: #003087; }
      .navbar-default .navbar-brand { color: white; }
      .navbar-default .navbar-nav > li > a { color: white; }
      .sidebar { background-color: white; box-shadow: 2px 2px 5px rgba(0,0,0,0.1); padding: 15px; }
      .main-panel { background-color: white; box-shadow: 2px 2px 5px rgba(0,0,0,0.1); padding: 15px; }
      .footer { text-align: center; color: gray; font-size: 12px; padding: 10px; }
      .summary-box { font-weight: bold; color: #005EB8; font-size: 16px; margin-bottom: 20px; }
      .insight-box { font-style: italic; color: #333; font-size: 14px; margin-top: 10px; }
    "))
  ),
  tabPanel(
    "OT Overview",
    sidebarLayout(
      sidebarPanel(
        selectInput("department_filter", "Select Department", choices = departments, selected = "All"),
        sliderInput(
          "date_range", "Select Date Range",
          min = min(weekly_metrics$week_end), max = max(weekly_metrics$week_end),
          value = c(min(weekly_metrics$week_end), max(weekly_metrics$week_end)),
          timeFormat = "%Y-%m-%d"
        ),
        actionButton("reset_filters", "Reset Filters", style = "background-color: #005EB8; color: white;"),
        downloadButton("download_report", "Download Report", style = "background-color: #005EB8; color: white; margin-top: 10px;")
      ),
      mainPanel(
        div(class = "summary-box", textOutput("total_ot_summary")),
        h3("Projected Overtime Hours by Department"),
        plotlyOutput("ot_by_dept_plot"),
        div(class = "insight-box", textOutput("ot_recommendation")),
        h3("Employee Overtime Details"),
        DTOutput("employee_table")
      )
    )
  ),
  tabPanel(
    "OT Trend",
    sidebarLayout(
      sidebarPanel(
        selectInput("department_filter_trend", "Select Department", choices = departments, selected = "All"),
        sliderInput(
          "date_range_trend", "Select Date Range",
          min = min(weekly_metrics$week_end), max = max(weekly_metrics$week_end),
          value = c(min(weekly_metrics$week_end), max(weekly_metrics$week_end)),
          timeFormat = "%Y-%m-%d"
        )
      ),
      mainPanel(
        h3("Weekly Overtime Trend"),
        plotlyOutput("ot_trend_plot"),
        div(class = "insight-box", textOutput("ot_trend_insight"))
      )
    )
  ),
  tabPanel(
    "UPLH Review",
    sidebarLayout(
      sidebarPanel(
        selectInput("department_filter_uplh", "Select Department", choices = departments, selected = "All"),
        sliderInput(
          "date_range_uplh", "Select Date Range",
          min = min(weekly_metrics$week_end), max = max(weekly_metrics$week_end),
          value = c(min(weekly_metrics$week_end), max(weekly_metrics$week_end)),
          timeFormat = "%Y-%m-%d"
        )
      ),
      mainPanel(
        h3("UPLH and Overtime by Week"),
        plotlyOutput("uplh_plot"),
        div(class = "insight-box", textOutput("uplh_projection")),
        h3("Monthly UPLH Summary"),
        DTOutput("uplh_table"),
        div(class = "insight-box", textOutput("uplh_recommendation"))
      )
    )
  ),
  tabPanel(
    "Equipment Analysis",
    sidebarLayout(
      sidebarPanel(
        selectInput("department_filter_eq", "Select Department", choices = departments, selected = "All"),
        dateRangeInput(
          "date_range_eq", "Select Date Range",
          start = min(equipment_breakages$date), 
          end = max(equipment_breakages$date),
          min = min(equipment_breakages$date),
          max = max(equipment_breakages$date)
        ),
        selectInput("equipment_filter", "Filter by Equipment", 
                    choices = c("All", unique(equipment_breakages$equipment)), 
                    selected = "All"),
        selectInput("severity_filter", "Filter by Severity", 
                    choices = c("All", unique(equipment_breakages$severity)), 
                    selected = "All"),
        checkboxInput("show_cost_metrics", "Show Cost Metrics", value = TRUE),
        checkboxInput("show_time_metrics", "Show Downtime Metrics", value = TRUE),
        actionButton("predict_breakages", "Predict Future Breakages", 
                     style = "background-color: #005EB8; color: white; margin-top: 10px;")
      ),
      mainPanel(
        fluidRow(
          column(width = 4,
                 div(class = "summary-box", style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px;",
                     h4("Total Breakages"),
                     textOutput("total_breakages"),
                     hr(),
                     textOutput("avg_repair_cost"),
                     textOutput("total_downtime")
                 )
          ),
          column(width = 4,
                 div(class = "summary-box", style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px;",
                     h4("Critical Issues"),
                     textOutput("critical_breakages"),
                     hr(),
                     textOutput("most_expensive_repair"),
                     textOutput("most_downtime")
                 )
          ),
          column(width = 4,
                 div(class = "summary-box", style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px;",
                     h4("Risk Assessment"),
                     textOutput("highest_risk_equipment"),
                     hr(),
                     textOutput("most_common_cause"),
                     textOutput("recurrent_issues")
                 )
          )
        ),
        
        br(),
        
        h3("Breakage Frequency and Impact"),
        plotlyOutput("breakage_heatmap"),
        div(class = "insight-box", textOutput("heatmap_insight")),
        
        br(),
        
        conditionalPanel(
          condition = "input.show_cost_metrics == true",
          h3("Cost Analysis"),
          fluidRow(
            column(width = 6, plotlyOutput("cost_by_equipment")),
            column(width = 6, plotlyOutput("cost_by_department"))
          ),
          div(class = "insight-box", textOutput("cost_insight"))
        ),
        
        conditionalPanel(
          condition = "input.show_time_metrics == true",
          h3("Downtime Analysis"),
          fluidRow(
            column(width = 6, plotlyOutput("downtime_trend")),
            column(width = 6, plotlyOutput("downtime_by_cause"))
          ),
          div(class = "insight-box", textOutput("downtime_insight"))
        ),
        
        h3("Breakage Root Cause Analysis"),
        plotlyOutput("cause_sankey"),
        div(class = "insight-box", textOutput("cause_insight")),
        
        br(),
        
        h3("Recommendations Based on Analysis"),
        div(class = "insight-box", style = "background-color: #e8f4fc; padding: 15px; border-radius: 5px;",
            htmlOutput("equipment_recommendations")
        ),
        
        br(),
        
        conditionalPanel(
          condition = "input.predict_breakages",
          h3("Predictive Maintenance Forecast"),
          plotlyOutput("predictive_maintenance"),
          div(class = "insight-box", textOutput("prediction_insight"))
        ),
        
        br(),
        
        h3("Detailed Breakage Records"),
        DTOutput("breakage_table")
      )
    )
  ),
  footer = div(
    class = "footer",
    "Prepared by Clay | Demo for Q1 2025"
  )
)

# Define server
server <- function(input, output, session) {
  # Reset filters
  observeEvent(input$reset_filters, {
    updateSelectInput(session, "department_filter", selected = "All")
    updateSliderInput(session, "date_range", value = c(min(weekly_metrics$week_end), max(weekly_metrics$week_end)))
  })
  
  # Reactive data for OT Overview
  filtered_employees <- reactive({
    data <- employees
    if (input$department_filter != "All") {
      data <- data %>% filter(department == input$department_filter)
    }
    data %>% arrange(desc(projected_ot))
  })
  
  filtered_weekly <- reactive({
    data <- weekly_metrics
    if (input$department_filter != "All") {
      data <- data %>% filter(department == input$department_filter)
    }
    data %>% filter(week_end >= input$date_range[1] & week_end <= input$date_range[2])
  })
  
  # Total OT Summary
  output$total_ot_summary <- renderText({
    total_ot <- sum(filtered_weekly()$ot_hours)
    paste("Total Projected OT Hours:", round(total_ot, 2))
  })
  
  # OT by Department Plot
  output$ot_by_dept_plot <- renderPlotly({
    data <- filtered_weekly() %>%
      group_by(department) %>%
      summarise(projected_ot = sum(ot_hours)) %>%
      arrange(desc(projected_ot))
    
    color_scale <- scales::col_numeric(
      palette = c("#2ECC71", "#E74C3C"),
      domain = c(min(data$projected_ot), max(data$projected_ot))
    )(data$projected_ot)
    
    p <- ggplot(data, aes(x = projected_ot, y = reorder(department, projected_ot), fill = projected_ot,
                          text = paste("Department:", department, "<br>OT Hours:", round(projected_ot, 2),
                                       "<br>% of Total:", round(projected_ot / sum(projected_ot) * 100, 2), "%"))) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = round(projected_ot, 2)), hjust = -0.2, size = 3) +
      scale_fill_gradient(low = "#2ECC71", high = "#E74C3C") +
      labs(x = "Projected OT Hours", y = "Department") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text")
  })
  
  # OT Recommendation
  output$ot_recommendation <- renderText({
    data <- filtered_weekly() %>%
      group_by(department) %>%
      summarise(projected_ot = sum(ot_hours)) %>%
      arrange(desc(projected_ot))
    
    highest_dept <- data$department[1]
    highest_ot <- round(data$projected_ot[1], 2)
    paste("Focus on reducing OT in", highest_dept, "(highest OT:", highest_ot, "hours) by redistributing workload or hiring additional staff.")
  })
  
  # Employee Table
  output$employee_table <- renderDT({
    datatable(filtered_employees(), options = list(order = list(list(5, "desc")))) %>%
      formatStyle(
        "projected_ot",
        backgroundColor = styleInterval(15, c("white", "lightcoral"))
      )
  })
  
  # Reactive data for OT Trend
  filtered_weekly_trend <- reactive({
    data <- weekly_metrics
    if (input$department_filter_trend != "All") {
      data <- data %>% filter(department == input$department_filter_trend)
    }
    data <- data %>%
      filter(week_end >= input$date_range_trend[1] & week_end <= input$date_range_trend[2]) %>%
      group_by(week_end) %>%
      summarise(ot_hours = sum(ot_hours))
  })
  
  # OT Trend Plot
  output$ot_trend_plot <- renderPlotly({
    data <- filtered_weekly_trend()
    
    peak_week <- data$week_end[which.max(data$ot_hours)]
    peak_value <- max(data$ot_hours)
    
    p <- ggplot(data, aes(x = week_end, y = ot_hours, text = paste("Date:", week_end, "<br>OT Hours:", round(ot_hours, 2)))) +
      geom_line(color = "#005EB8") +
      geom_point(color = "#005EB8") +
      geom_smooth(method = "loess", color = "#4DA8DA", se = FALSE) +
      annotate("text", x = peak_week, y = peak_value, label = paste("Peak:", round(peak_value, 2), "hours"), vjust = -1, color = "red") +
      labs(x = "Week Ending", y = "Overtime Hours") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  # OT Trend Insight
  output$ot_trend_insight <- renderText({
    data <- filtered_weekly_trend()
    if (nrow(data) > 1) {
      ot_changes <- diff(data$ot_hours) / data$ot_hours[-length(data$ot_hours)] * 100
      avg_change <- mean(ot_changes, na.rm = TRUE)
      paste("Average Weekly OT Change:", round(avg_change, 2), "%")
    } else {
      "Not enough data to calculate trend."
    }
  })
  
  # Reactive data for UPLH Review
  filtered_weekly_uplh <- reactive({
    data <- weekly_metrics
    if (input$department_filter_uplh != "All") {
      data <- data %>% filter(department == input$department_filter_uplh)
    }
    data <- data %>%
      filter(week_end >= input$date_range_uplh[1] & week_end <= input$date_range_uplh[2]) %>%
      group_by(week_end) %>%
      summarise(
        uplh = mean(uplh, na.rm = TRUE),
        ot_hours = sum(ot_hours)
      )
  })
  
  # UPLH and OT Plot
  output$uplh_plot <- renderPlotly({
    data <- filtered_weekly_uplh()
    
    max_uplh <- max(data$uplh, na.rm = TRUE)
    max_ot <- max(data$ot_hours, na.rm = TRUE)
    
    p <- ggplot(data) +
      geom_bar(aes(x = week_end, y = uplh, text = paste("Date:", week_end, "<br>UPLH:", round(uplh, 2))), stat = "identity", fill = "#7F8C8D") +
      geom_line(aes(x = week_end, y = ot_hours / max_ot * max_uplh, text = paste("Date:", week_end, "<br>OT Hours:", round(ot_hours, 2))), color = "#005EB8") +
      geom_hline(yintercept = 12.71, linetype = "solid", color = "green", size = 1) +
      geom_hline(yintercept = 14.62, linetype = "dashed", color = "red", size = 1) +
      annotate("text", x = min(data$week_end), y = 12.71, label = "Goal: 12.71", hjust = 0, vjust = -1, color = "green") +
      annotate("text", x = min(data$week_end), y = 14.62, label = "Target: 14.62", hjust = 0, vjust = -1, color = "red") +
      scale_y_continuous(
        name = "UPLH",
        sec.axis = sec_axis(~ . * max_ot / max_uplh, name = "Overtime Hours")
      ) +
      labs(x = "Week Ending") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  # UPLH Projection
  output$uplh_projection <- renderText({
    data <- filtered_weekly_uplh()
    if (nrow(data) > 1) {
      model <- lm(uplh ~ as.numeric(week_end), data = data)
      next_week <- max(data$week_end) + 7
      projected_uplh <- predict(model, newdata = data.frame(week_end = as.numeric(next_week)))
      paste("Projected UPLH for April 2025:", round(projected_uplh, 2))
    } else {
      "Not enough data to project UPLH."
    }
  })
  
  # Monthly UPLH Table
  output$uplh_table <- renderDT({
    data <- weekly_metrics %>%
      mutate(
        Year = year(week_end),
        Month = month(week_end, label = TRUE)
      ) %>%
      group_by(Year, Month) %>%
      summarise(
        Worked_Hours = sum(worked_hours),
        Overtime = sum(ot_hours),
        Packaged_Units = sum(packaged_units),
        UPLH = mean(uplh, na.rm = TRUE)
      ) %>%
      ungroup() %>%
      mutate(
        Goal_Met = ifelse(UPLH >= 12.71, "Yes", "No")
      )
    
    datatable(data, options = list(pageLength = 5)) %>%
      formatStyle("Goal_Met", target = "cell", backgroundColor = styleEqual(c("Yes", "No"), c("lightgreen", "lightcoral")))
  })
  
  # UPLH Recommendation
  output$uplh_recommendation <- renderText({
    data <- weekly_metrics %>%
      group_by(department) %>%
      summarise(uplh = mean(uplh, na.rm = TRUE)) %>%
      arrange(uplh)
    
    lowest_dept <- data$department[1]
    paste("To meet the 15% UPLH improvement target (14.62), consider reducing OT in departments with low UPLH, such as", lowest_dept, ".")
  })
  
  # Reactive data for Equipment Analysis
  filtered_equipment <- reactive({
    data <- equipment_breakages
    
    if (input$department_filter_eq != "All") {
      data <- data %>% filter(department == input$department_filter_eq)
    }
    
    data <- data %>% filter(date >= input$date_range_eq[1] & date <= input$date_range_eq[2])
    
    if (input$equipment_filter != "All") {
      data <- data %>% filter(equipment == input$equipment_filter)
    }
    
    if (input$severity_filter != "All") {
      data <- data %>% filter(severity == input$severity_filter)
    }
    
    return(data)
  })
  
  # Summary statistics
  output$total_breakages <- renderText({
    data <- filtered_equipment()
    sprintf("%d incidents reported", nrow(data))
  })
  
  output$avg_repair_cost <- renderText({
    data <- filtered_equipment()
    sprintf("Avg. repair cost: $%.2f", mean(data$repair_cost, na.rm = TRUE))
  })
  
  output$total_downtime <- renderText({
    data <- filtered_equipment()
    sprintf("Total downtime: %.1f hours", sum(data$downtime_hours, na.rm = TRUE))
  })
  
  output$critical_breakages <- renderText({
    data <- filtered_equipment() %>% filter(severity == "Critical")
    sprintf("%d critical incidents (%.1f%%)", 
            nrow(data), 
            nrow(data) / nrow(filtered_equipment()) * 100)
  })
  
  output$most_expensive_repair <- renderText({
    data <- filtered_equipment()
    most_exp <- data[which.max(data$repair_cost), ]
    sprintf("Highest cost: $%.2f (%s)", 
            most_exp$repair_cost, 
            most_exp$equipment)
  })
  
  output$most_downtime <- renderText({
    data <- filtered_equipment()
    most_down <- data[which.max(data$downtime_hours), ]
    sprintf("Longest downtime: %.1f hours (%s)", 
            most_down$downtime_hours, 
            most_down$equipment)
  })
  
  output$highest_risk_equipment <- renderText({
    data <- filtered_equipment() %>%
      group_by(equipment) %>%
      summarise(
        incidents = n(),
        avg_severity = mean(ifelse(severity == "Low", 1, 
                                   ifelse(severity == "Medium", 2, 
                                          ifelse(severity == "High", 3, 4))), na.rm = TRUE),
        risk_score = incidents * avg_severity
      ) %>%
      arrange(desc(risk_score))
    
    if(nrow(data) > 0) {
      sprintf("Highest risk: %s (Score: %.1f)", 
              data$equipment[1], 
              data$risk_score[1])
    } else {
      "No data available"
    }
  })
  
  output$most_common_cause <- renderText({
    data <- filtered_equipment() %>%
      count(cause) %>%
      arrange(desc(n))
    
    if(nrow(data) > 0) {
      sprintf("Most common cause: %s (%d incidents)", 
              data$cause[1], 
              data$n[1])
    } else {
      "No data available"
    }
  })
  
  output$recurrent_issues <- renderText({
    data <- filtered_equipment() %>%
      group_by(equipment, cause) %>%
      summarise(count = n()) %>%
      arrange(desc(count))
    
    if(nrow(data) > 0) {
      sprintf("Recurrent issue: %s - %s (%d times)", 
              data$equipment[1], 
              data$cause[1], 
              data$count[1])
    } else {
      "No data available"
    }
  })
  
  # Breakage Heatmap
  output$breakage_heatmap <- renderPlotly({
    data <- filtered_equipment() %>%
      group_by(equipment, severity) %>%
      summarise(count = n()) %>%
      ungroup()
    
    data$severity <- factor(data$severity, levels = c("Low", "Medium", "High", "Critical"))
    
    all_combinations <- expand.grid(
      equipment = unique(filtered_equipment()$equipment),
      severity = c("Low", "Medium", "High", "Critical")
    )
    
    data <- all_combinations %>%
      left_join(data, by = c("equipment", "severity")) %>%
      mutate(count = ifelse(is.na(count), 0, count))
    
    p <- ggplot(data, aes(x = severity, y = equipment, fill = count, 
                          text = paste("Equipment:", equipment, 
                                       "<br>Severity:", severity,
                                       "<br>Count:", count))) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "#b3d9ff", high = "#003087", na.value = "white") +
      labs(title = "Breakage Frequency by Equipment and Severity",
           x = "Severity", y = "Equipment", fill = "Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
            plot.title = element_text(hjust = 0.5))
    
    ggplotly(p, tooltip = "text")
  })
  
  output$heatmap_insight <- renderText({
    data <- filtered_equipment() %>%
      group_by(equipment, severity) %>%
      summarise(count = n()) %>%
      ungroup()
    
    if(nrow(data) > 0) {
      highest <- data %>% arrange(desc(count)) %>% slice(1)
      sprintf("Focus on %s with %s severity breakages: %d incidents recorded.", 
              highest$equipment, 
              tolower(highest$severity), 
              highest$count)
    } else {
      "No patterns detected in the current filtered data."
    }
  })
  
  # Cost Analysis Plots
  output$cost_by_equipment <- renderPlotly({
    data <- filtered_equipment() %>%
      group_by(equipment) %>%
      summarise(
        total_cost = sum(repair_cost, na.rm = TRUE),
        avg_cost = mean(repair_cost, na.rm = TRUE),
        incidents = n()
      ) %>%
      arrange(desc(total_cost))
    
    p <- ggplot(data, aes(x = reorder(equipment, total_cost), y = total_cost, 
                          text = paste("Equipment:", equipment,
                                       "<br>Total Cost: $", round(total_cost, 2),
                                       "<br>Avg Cost: $", round(avg_cost, 2),
                                       "<br>Incidents:", incidents))) +
      geom_bar(stat = "identity", fill = "#4DA8DA") +
      geom_text(aes(label = sprintf("$%.0f", total_cost)), hjust = -0.1, size = 3) +
      coord_flip() +
      labs(title = "Total Repair Cost by Equipment",
           x = "Equipment", y = "Total Cost ($)") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  output$cost_by_department <- renderPlotly({
    data <- filtered_equipment() %>%
      group_by(department) %>%
      summarise(
        total_cost = sum(repair_cost, na.rm = TRUE),
        incidents = n()
      ) %>%
      mutate(cost_per_incident = total_cost / incidents) %>%
      arrange(desc(total_cost))
    
    p <- ggplot(data, aes(x = "", y = total_cost, fill = department,
                          text = paste("Department:", department,
                                       "<br>Total Cost: $", round(total_cost, 2),
                                       "<br>Cost per Incident: $", round(cost_per_incident, 2),
                                       "<br>Incidents:", incidents))) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      scale_fill_brewer(palette = "Blues") +
      labs(title = "Repair Cost Distribution by Department",
           x = NULL, y = NULL, fill = "Department") +
      theme_minimal() +
      theme(axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank())
    
    ggplotly(p, tooltip = "text")
  })
  
  output$cost_insight <- renderText({
    data <- filtered_equipment() %>%
      group_by(department) %>%
      summarise(
        total_cost = sum(repair_cost, na.rm = TRUE),
        incidents = n()
      ) %>%
      mutate(cost_per_incident = total_cost / incidents) %>%
      arrange(desc(cost_per_incident))
    
    if(nrow(data) > 0) {
      sprintf("%s department has the highest cost per incident ($%.2f). Review maintenance protocols and equipment handling procedures.", 
              data$department[1], 
              data$cost_per_incident[1])
    } else {
      "No cost patterns detected in the current filtered data."
    }
  })
  
  # Downtime Analysis
  output$downtime_trend <- renderPlotly({
    data <- filtered_equipment() %>%
      mutate(month = floor_date(date, "month")) %>%
      group_by(month) %>%
      summarise(
        total_downtime = sum(downtime_hours, na.rm = TRUE),
        incidents = n()
      )
    
    p <- ggplot(data, aes(x = month, y = total_downtime, 
                          text = paste("Month:", format(month, "%b %Y"),
                                       "<br>Total Downtime:", round(total_downtime, 1), "hours",
                                       "<br>Incidents:", incidents))) +
      geom_line(color = "#005EB8", size = 1) +
      geom_point(color = "#005EB8", size = 3) +
      geom_smooth(method = "loess", se = FALSE, color = "#4DA8DA", linetype = "dashed") +
      labs(title = "Monthly Equipment Downtime Trend",
           x = "Month", y = "Total Downtime (hours)") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  output$downtime_by_cause <- renderPlotly({
    data <- filtered_equipment() %>%
      group_by(cause) %>%
      summarise(
        total_downtime = sum(downtime_hours, na.rm = TRUE),
        incidents = n(),
        avg_downtime = total_downtime / incidents
      ) %>%
      arrange(desc(total_downtime))
    
    p <- ggplot(data, aes(x = reorder(cause, -total_downtime), y = total_downtime, fill = avg_downtime,
                          text = paste("Cause:", cause,
                                       "<br>Total Downtime:", round(total_downtime, 1), "hours",
                                       "<br>Avg Downtime:", round(avg_downtime, 1), "hours/incident",
                                       "<br>Incidents:", incidents))) +
      geom_bar(stat = "identity") +
      scale_fill_gradient(low = "#b3d9ff", high = "#003087") +
      labs(title = "Downtime by Failure Cause",
           x = "Cause", y = "Total Downtime (hours)", fill = "Avg. Downtime") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = "text")
  })
  
  output$downtime_insight <- renderText({
    data <- filtered_equipment() %>%
      group_by(cause) %>%
      summarise(
        total_downtime = sum(downtime_hours, na.rm = TRUE),
        incidents = n()
      ) %>%
      arrange(desc(total_downtime))
    
    if(nrow(data) > 0) {
      sprintf("'%s' is the leading cause of downtime (%.1f hours). Implement targeted training and preventive measures.", 
              data$cause[1], 
              data$total_downtime[1])
    } else {
      "No downtime patterns detected in the current filtered data."
    }
  })
  
  # Cause Analysis Sankey Diagram
  output$cause_sankey <- renderPlotly({
    data <- filtered_equipment()
    
    source_nodes <- c(unique(data$department), unique(data$equipment))
    target_nodes <- c(unique(data$equipment), unique(data$cause))
    all_nodes <- unique(c(source_nodes, target_nodes))
    
    nodes <- data.frame(
      node = 0:(length(all_nodes) - 1),
      label = all_nodes
    )
    
    dept_to_equip <- data %>%
      group_by(department, equipment) %>%
      summarise(value = n()) %>%
      ungroup()
    
    dept_to_equip$source <- match(dept_to_equip$department, all_nodes) - 1
    dept_to_equip$target <- match(dept_to_equip$equipment, all_nodes) - 1
    
    equip_to_cause <- data %>%
      group_by(equipment, cause) %>%
      summarise(value = n()) %>%
      ungroup()
    
    equip_to_cause$source <- match(equip_to_cause$equipment, all_nodes) - 1
    equip_to_cause$target <- match(equip_to_cause$cause, all_nodes) - 1
    
    links <- rbind(
      dept_to_equip %>% select(source, target, value),
      equip_to_cause %>% select(source, target, value)
    )
    
    p <- plot_ly(
      type = "sankey",
      orientation = "h",
      node = list(
        label = nodes$label,
        color = colorRampPalette(c("#b3d9ff", "#003087"))(length(nodes$label)),
        pad = 15,
        thickness = 20,
        line = list(
          color = "black",
          width = 0.5
        )
      ),
      link = list(
        source = links$source,
        target = links$target,
        value = links$value,
        color = colorRampPalette(c("#E8F4FC", "#4DA8DA"))(nrow(links))
      )
    )
    
    p %>% layout(
      title = "Breakage Flow: Department → Equipment → Cause",
      font = list(size = 10),
      xaxis = list(showgrid = F, zeroline = F),
      yaxis = list(showgrid = F, zeroline = F)
    )
  })
  
  output$cause_insight <- renderText({
    data <- filtered_equipment() %>%
      group_by(department, equipment, cause) %>%
      summarise(count = n()) %>%
      ungroup() %>%
      arrange(desc(count))
    
    if(nrow(data) > 0) {
      insight <- data[1, ]
      sprintf("Most common failure pattern: %s department → %s → %s (occurred %d times). This indicates a systemic issue that requires process improvement.", 
              insight$department, 
              insight$equipment,
              insight$cause,
              insight$count)
    } else {
      "No clear failure patterns detected in the current filtered data."
    }
  })
  
  # Recommendations
  output$equipment_recommendations <- renderUI({
    data <- filtered_equipment()
    
    eq_issues <- data %>%
      group_by(equipment) %>%
      summarise(
        incidents = n(),
        total_cost = sum(repair_cost, na.rm = TRUE),
        total_downtime = sum(downtime_hours, na.rm = TRUE)
      ) %>%
      arrange(desc(incidents))
    
    causes <- data %>%
      count(cause) %>%
      arrange(desc(n))
    
    dept_issues <- data %>%
      group_by(department) %>%
      summarise(incidents = n()) %>%
      arrange(desc(incidents))
    
    if(nrow(data) > 0) {
      HTML(paste0(
        "<p><strong>1. Equipment Focus:</strong> Prioritize maintenance for <span style='color:#E74C3C'>", eq_issues$equipment[1], 
        "</span> with ", eq_issues$incidents[1], " incidents causing $", round(eq_issues$total_cost[1], 2), 
        " in repair costs and ", round(eq_issues$total_downtime[1], 1), " hours of downtime.</p>",
        
        "<p><strong>2. Root Cause Action:</strong> Address '<span style='color:#E74C3C'>", causes$cause[1], 
        "</span>' as the most common failure cause (", causes$n[1], " incidents). Implement targeted training and process improvements.</p>",
        
        "<p><strong>3. Department Support:</strong> Provide additional training and resources to <span style='color:#E74C3C'>", 
        dept_issues$department[1], "</span> department where ", dept_issues$incidents[1], " equipment breakages have occurred.</p>",
        
        "<p><strong>4. Preventive Maintenance Schedule:</strong> Based on failure patterns, implement a preventive maintenance schedule focused on high-risk equipment.</p>",
        
        "<p><strong>5. Cost Reduction Strategy:</strong> Projected annual savings of $", 
        round(sum(data$repair_cost, na.rm = TRUE) * 12 / (as.numeric(diff(range(data$date))) / 30) * 0.3, 2),
        " by implementing these recommendations (30% reduction in failures).</p>"
      ))
    } else {
      HTML("<p>No data available to generate recommendations. Please adjust filters.</p>")
    }
  })
  
  # Predictive Maintenance Plot
  observeEvent(input$predict_breakages, {
    output$predictive_maintenance <- renderPlotly({
      data <- equipment_breakages
      
      daily_incidents <- data %>%
        mutate(date = as.Date(date)) %>%
        group_by(date) %>%
        summarise(incidents = n())
      
      date_range <- seq(min(daily_incidents$date), max(daily_incidents$date), by = "day")
      daily_incidents <- daily_incidents %>%
        right_join(data.frame(date = date_range), by = "date") %>%
        mutate(incidents = ifelse(is.na(incidents), 0, incidents))
      
      daily_incidents <- daily_incidents %>%
        mutate(
          day_of_week = wday(date),
          day_of_month = day(date),
          month = month(date),
          time_index = as.numeric(date - min(date))
        )
      
      model <- lm(incidents ~ time_index + factor(day_of_week) + factor(month), data = daily_incidents)
      
      future_dates <- seq(max(daily_incidents$date) + 1, max(daily_incidents$date) + 30, by = "day")
      future_data <- data.frame(
        date = future_dates,
        day_of_week = wday(future_dates),
        day_of_month = day(future_dates),
        month = month(future_dates),
        time_index = as.numeric(future_dates - min(daily_incidents$date))
      )
      
      future_data$predicted_incidents <- predict(model, newdata = future_data)
      future_data$predicted_incidents <- pmax(0, future_data$predicted_incidents)
      
      future_data$risk_level <- cut(
        future_data$predicted_incidents,
        breaks = c(-Inf, 0.5, 1.5, 2.5, Inf),
        labels = c("Low", "Medium", "High", "Critical")
      )
      
      p <- ggplot() +
        geom_line(data = daily_incidents, aes(x = date, y = incidents, color = "Historical"), size = 1) +
        geom_line(data = future_data, aes(x = date, y = predicted_incidents, color = "Predicted"), 
                  size = 1, linetype = "dashed") +
        geom_point(data = subset(future_data, risk_level %in% c("High", "Critical")), 
                   aes(x = date, y = predicted_incidents, color = risk_level), size = 4) +
        scale_color_manual(values = c("Historical" = "#005EB8", "Predicted" = "#4DA8DA", 
                                      "High" = "#FFA500", "Critical" = "#E74C3C")) +
        labs(title = "Breakage Prediction for Next 30 Days",
             x = "Date", y = "Expected Incidents", color = "Type") +
        theme_minimal() +
        theme(legend.position = "bottom")
      
      ggplotly(p)
    })
    
    output$prediction_insight <- renderText({
      data <- equipment_breakages
      
      daily_incidents <- data %>%
        mutate(date = as.Date(date)) %>%
        group_by(date) %>%
        summarise(incidents = n())
      
      date_range <- seq(min(daily_incidents$date), max(daily_incidents$date), by = "day")
      daily_incidents <- daily_incidents %>%
        right_join(data.frame(date = date_range), by = "date") %>%
        mutate(incidents = ifelse(is.na(incidents), 0, incidents))
      
      daily_incidents <- daily_incidents %>%
        mutate(
          day_of_week = wday(date),
          day_of_month = day(date),
          month = month(date),
          time_index = as.numeric(date - min(date))
        )
      
      model <- lm(incidents ~ time_index + factor(day_of_week) + factor(month), data = daily_incidents)
      
      future_dates <- seq(max(daily_incidents$date) + 1, max(daily_incidents$date) + 30, by = "day")
      future_data <- data.frame(
        date = future_dates,
        day_of_week = wday(future_dates),
        day_of_month = day(future_dates),
        month = month(future_dates),
        time_index = as.numeric(future_dates - min(daily_incidents$date))
      )
      
      future_data$predicted_incidents <- predict(model, newdata = future_data)
      future_data$predicted_incidents <- pmax(0, future_data$predicted_incidents)
      
      high_risk_days <- future_data %>%
        filter(predicted_incidents > 1.5) %>%
        arrange(desc(predicted_incidents)) %>%
        head(3)
      
      if(nrow(high_risk_days) > 0) {
        dates_text <- paste(format(high_risk_days$date[1:min(3, nrow(high_risk_days))], "%b %d"), collapse = ", ")
        sprintf("Schedule preventive maintenance before high-risk dates: %s. Expected incident rate is %.1f times the average.", 
                dates_text, 
                mean(high_risk_days$predicted_incidents) / mean(daily_incidents$incidents, na.rm = TRUE))
      } else {
        "No high-risk days detected in the next 30 days. Maintain regular maintenance schedule."
      }
    })
  })
  
  # Detailed table
  output$breakage_table <- renderDT({
    data <- filtered_equipment() %>%
      select(date, department, equipment, cause, severity, downtime_hours, repair_cost)
    
    datatable(data, options = list(pageLength = 5, scrollX = TRUE)) %>%
      formatCurrency(columns = "repair_cost", currency = "$", digits = 2) %>%
      formatStyle(
        "severity",
        backgroundColor = styleEqual(
          c("Low", "Medium", "High", "Critical"),
          c("#b3d9ff", "#4DA8DA", "#FFA500", "#E74C3C")
        ),
        color = styleEqual(
          c("Low", "Medium", "High", "Critical"),
          c("black", "black", "black", "white")
        )
      )
  })
  
  # Download Report
  output$download_report <- downloadHandler(
    filename = function() {
      paste("ot_dashboard_report_", Sys.Date(), ".html", sep = "")
    },
    content = function(file) {
      temp_report <- tempfile(fileext = ".Rmd")
      writeLines(
        c(
          "---
title: 'Overtime and Productivity Report'
date: '`r Sys.Date()`'
output: html_document
---
          
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)
library(dplyr)
library(ggplot2)
library(plotly)
weekly_metrics <- read.csv('data/weekly_metrics.csv')
employees <- read.csv('data/employees.csv')
weekly_metrics$week_end <- as.Date(weekly_metrics$week_end)

data <- weekly_metrics %>%
  group_by(department) %>%
  summarise(projected_ot = sum(ot_hours)) %>%
  arrange(desc(projected_ot))
ggplot(data, aes(x = projected_ot, y = reorder(department, projected_ot), fill = projected_ot)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = round(projected_ot, 2)), hjust = -0.2, size = 3) +
  scale_fill_gradient(low = '#2ECC71', high = '#E74C3C') +
  labs(x = 'Projected OT Hours', y = 'Department', title = 'Projected Overtime Hours by Department') +
  theme_minimal() +
  theme(legend.position = 'none')
  
data <- weekly_metrics %>%
  group_by(week_end) %>%
  summarise(uplh = mean(uplh, na.rm = TRUE), ot_hours = sum(ot_hours))
max_uplh <- max(data$uplh, na.rm = TRUE)
max_ot <- max(data$ot_hours, na.rm = TRUE)
ggplot(data) +
  geom_bar(aes(x = week_end, y = uplh), stat = 'identity', fill = '#7F8C8D') +
  geom_line(aes(x = week_end, y = ot_hours / max_ot * max_uplh), color = '#005EB8') +
  geom_hline(yintercept = 12.71, linetype = 'solid', color = 'green', size = 1) +
  geom_hline(yintercept = 14.62, linetype = 'dashed', color = 'red', size = 1) +
  annotate('text', x = min(data$week_end), y = 12.71, label = 'Goal: 12.71', hjust = 0, vjust = -1, color = 'green') +
  annotate('text', x = min(data$week_end), y = 14.62, label = 'Target: 14.62', hjust = 0, vjust = -1, color = 'red') +
  scale_y_continuous(name = 'UPLH', sec.axis = sec_axis(~ . * max_ot / max_uplh, name = 'Overtime Hours')) +
  labs(x = 'Week Ending', title = 'UPLH and Overtime by Week') +
  theme_minimal()
```"
        ),
        temp_report
      )
      rmarkdown::render(temp_report, output_file = file)
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)