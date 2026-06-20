# Superstore Sales Dashboard - Final Version with Clear Labels

options(repos = c(CRAN = "https://cloud.r-project.org"))
options(quarto.available = FALSE)

if (!requireNamespace("BiocManager", quietly = TRUE)) {
  assign("BiocManager", list(install = function(...) {}), envir = .GlobalEnv)
}

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(DT)
library(scales)

# Load and clean data
df <- read.csv("SampleSuperstore.csv", stringsAsFactors = FALSE)

clean_numeric <- function(x) {
  x_clean <- gsub("[^0-9.-]", "", as.character(x))
  as.numeric(x_clean)
}

df$Sales <- clean_numeric(df$Sales)
df$Profit <- clean_numeric(df$Profit)
df$Discount <- clean_numeric(df$Discount)
df$Quantity <- clean_numeric(df$Quantity)

df <- df[!is.na(df$Sales) & !is.na(df$Profit), ]

if("Sub.Category" %in% names(df)) {
  names(df)[names(df) == "Sub.Category"] <- "Sub.Category"
} else if("Sub-Category" %in% names(df)) {
  names(df)[names(df) == "Sub-Category"] <- "Sub.Category"
}

# Color palettes
region_colors <- c("Central" = "#1E88E5", "East" = "#43A047",
                   "South" = "#FB8C00", "West" = "#8E24AA")
category_colors <- c("Furniture" = "#FFB74D", "Office Supplies" = "#4DB6AC",
                     "Technology" = "#EC407A")
border_color <- "#2C3E50"

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Superstore Sales Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Data Table", tabName = "datatable", icon = icon("table")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    ),
    selectInput("region_filter", "Select Region:",
                choices = c("All", unique(df$Region)), selected = "All"),
    selectInput("category_filter", "Select Category:",
                choices = c("All", unique(df$Category)), selected = "All"),
    selectInput("segment_filter", "Select Segment:",
                choices = c("All", unique(df$Segment)), selected = "All")
  ),
  dashboardBody(
    tags$head(tags$style(HTML('
      .small-box { border-radius: 12px; box-shadow: 0 4px 12px rgba(0,0,0,0.15); transition: transform 0.2s; }
      .small-box:hover { transform: translateY(-5px); }
      .bg-green { background: linear-gradient(135deg, #00b4db, #0083b0); }
      .bg-blue { background: linear-gradient(135deg, #2193b0, #6dd5ed); }
      .bg-purple { background: linear-gradient(135deg, #8E2DE2, #4A00E0); }
      .bg-yellow { background: linear-gradient(135deg, #f7971e, #ffd200); }
      .about-section { padding: 20px; background: #F9F6F0; border-radius: 12px; margin-top: 20px; }
    '))),
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                valueBoxOutput("total_sales_box"),
                valueBoxOutput("total_profit_box"),
                valueBoxOutput("profit_margin_box"),
                valueBoxOutput("avg_order_value_box")
              ),
              fluidRow(
                box(title = "Sales by Region (sorted highest to lowest)", plotOutput("region_plot")),
                box(title = "Sales by Category", plotOutput("category_plot"))
              ),
              fluidRow(
                box(title = "Top 10 Sub-Categories by Profit", plotOutput("top_subcategory_plot")),
                box(title = "Discount vs Profit Margin", plotOutput("discount_profit_plot"))
              )
      ),
      tabItem(tabName = "datatable",
              box(title = "Raw Sales Data", width = 12, DTOutput("data_table"))
      ),
      tabItem(tabName = "about",
              box(width = 12, title = "About This Project",
                  div(class = "about-section",
                      h3("Project Overview"),
                      p("This dashboard analyzes Superstore sales data to provide business insights."),
                      h3("Tools Used"), p("Excel, SPSS, R, Shiny"),
                      h3("Key Insights"),
                      p("Technology category generates the highest profit (p < 0.001)."),
                      p("Discounts significantly reduce profit margin (r = -0.22, p < 0.001)."),
                      p("No significant difference in sales across regions (p = 0.503)."),
                      h3("How I Built This"),
                      p("1. Cleaned the dataset in Excel."),
                      p("2. Performed ANOVA and correlation in SPSS."),
                      p("3. Designed an interactive Shiny dashboard."),
                      p("4. Deployed on ShinyApps.io."),
                      h3("Connect with Me"),
                      a(href = "https://www.linkedin.com/in/zainab-altaf-56bba6366/",
                        target = "_blank", "linkedin.com/in/zainab-altaf-56bba6366")
                  )
              )
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  filtered_data <- reactive({
    data <- df
    if(input$region_filter != "All") data <- data[data$Region == input$region_filter, ]
    if(input$category_filter != "All") data <- data[data$Category == input$category_filter, ]
    if(input$segment_filter != "All") data <- data[data$Segment == input$segment_filter, ]
    data
  })
  
  safe_sum <- function(x) {
    if(length(x) == 0) return(0)
    sum(x, na.rm = TRUE)
  }
  
  output$total_sales_box <- renderValueBox({
    valueBox(dollar(safe_sum(filtered_data()$Sales)), "Total Sales", icon = icon("dollar-sign"), color = "green")
  })
  output$total_profit_box <- renderValueBox({
    valueBox(dollar(safe_sum(filtered_data()$Profit)), "Total Profit", icon = icon("chart-line"), color = "blue")
  })
  output$profit_margin_box <- renderValueBox({
    margin <- safe_sum(filtered_data()$Profit) / safe_sum(filtered_data()$Sales)
    valueBox(percent(margin), "Profit Margin", icon = icon("percentage"), color = "purple")
  })
  output$avg_order_value_box <- renderValueBox({
    avg <- safe_sum(filtered_data()$Sales) / nrow(filtered_data())
    valueBox(dollar(avg), "Avg Order Value", icon = icon("shopping-cart"), color = "yellow")
  })
  
  # Sales by Region - with clear labels
  output$region_plot <- renderPlot({
    region_data <- filtered_data() %>%
      group_by(Region) %>%
      summarise(Sales = sum(Sales)) %>%
      arrange(desc(Sales))
    
    ggplot(region_data, aes(x = reorder(Region, -Sales), y = Sales, fill = Region)) +
      geom_bar(stat = "identity", width = 0.7, color = border_color, linewidth = 0.8) +
      geom_text(aes(label = dollar(Sales, accuracy = 1)), 
                vjust = -0.6, size = 5, color = border_color, fontface = "bold") +
      scale_fill_manual(values = region_colors) +
      labs(title = "Sales by Region (sorted highest to lowest)", x = "Region", y = "Total Sales") +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            axis.title = element_text(face = "bold"),
            panel.grid.major.x = element_blank()) +
      scale_y_continuous(labels = dollar, expand = expansion(mult = c(0, 0.25)))
  })
  
  # Sales by Category - with clear labels
  output$category_plot <- renderPlot({
    category_data <- filtered_data() %>%
      group_by(Category) %>%
      summarise(Sales = sum(Sales))
    
    ggplot(category_data, aes(x = Category, y = Sales, fill = Category)) +
      geom_bar(stat = "identity", width = 0.7, color = border_color, linewidth = 0.8) +
      geom_text(aes(label = dollar(Sales, accuracy = 1)), 
                vjust = -0.6, size = 5, color = border_color, fontface = "bold") +
      scale_fill_manual(values = category_colors) +
      labs(title = "Sales by Category", x = "Category", y = "Total Sales") +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            axis.title = element_text(face = "bold"),
            panel.grid.major.x = element_blank()) +
      scale_y_continuous(labels = dollar, expand = expansion(mult = c(0, 0.25)))
  })
  
  # Top 10 Sub-Categories by Profit
  output$top_subcategory_plot <- renderPlot({
    plot_data <- filtered_data() %>%
      group_by(Sub.Category) %>%
      summarise(Profit = sum(Profit)) %>%
      arrange(desc(Profit)) %>%
      head(10)
    
    bar_colors <- colorRampPalette(c("#FF6B6B", "#FFD93D"))(10)
    ggplot(plot_data, aes(x = reorder(Sub.Category, Profit), y = Profit, fill = Sub.Category)) +
      geom_col(alpha = 0.9, width = 0.7, color = border_color, linewidth = 0.5) +
      geom_text(aes(label = dollar(Profit, accuracy = 1)), 
                hjust = -0.1, size = 4, color = border_color, fontface = "bold") +
      scale_fill_manual(values = bar_colors) +
      coord_flip() +
      labs(x = "Sub-Category", y = "Total Profit", title = "Top 10 Sub-Categories by Profit") +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
      scale_y_continuous(labels = dollar, expand = expansion(mult = c(0, 0.1)))
  })
  
  # Discount vs Profit Margin
  output$discount_profit_plot <- renderPlot({
    data <- filtered_data()
    data <- data[!is.na(data$Discount) & !is.na(data$Profit) & data$Sales > 0, ]
    if(nrow(data) == 0) return(NULL)
    data$ProfitMargin <- data$Profit / data$Sales
    ggplot(data, aes(x = Discount, y = ProfitMargin)) +
      geom_point(alpha = 0.6, color = "#00B4D8", size = 2.5) +
      geom_smooth(method = "lm", se = FALSE, color = "#FF007F", linewidth = 1.2) +
      labs(title = "Discount vs Profit Margin", x = "Discount", y = "Profit Margin") +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
      scale_y_continuous(labels = percent) +
      scale_x_continuous(labels = percent)
  })
  
  output$data_table <- renderDT({
    datatable(filtered_data(), options = list(scrollX = TRUE, pageLength = 10))
  })
}

shinyApp(ui, server)