# Superstore Sales Dashboard

## 📊 An interactive R Shiny dashboard built to analyze Superstore sales data. This dashboard helps business users track key performance indicators, visualize revenue trends, and monitor product performance across different regions, categories, and customer segments.



---

## 🚀 Live Demo

🔗 **View the live app here: (https://zainab1201.shinyapps.io/superstore-dashboard/)**

---

### 📊 Key Visualizations

1. **Sales by Region (Sorted Highest to Lowest)**
   - Bar chart showing sales performance across Central, East, South, and West regions
   - Regions sorted from highest to lowest sales for easy comparison
   - Interactive hover for exact values

2. **Sales by Category**
   - Visual comparison of sales across product categories
   - Breakdown by region with color coding
   - Shows total sales for Furniture, Office Supplies, and Technology

3. **Top 10 Sub-Categories by Profit**
   - Identifies the most profitable product sub-categories
   - Helps focus on high-margin products
   - Interactive bars with profit values

4. **Discount vs Profit Margin Analysis**
   - Scatter plot showing relationship between discount rates and profit margins
   - Color-coded by category (Furniture, Office Supplies, Technology)
   - Identifies optimal discount strategies

### 📈 Key Metrics Displayed
- Total Sales Revenue
- Total Profit
- Total Quantity Sold
- Number of Orders
- Regional Performance Rankings
- Category-wise Performance

---

## 🛠️ Technologies Used

- **R Shiny** - Interactive web framework
- **shinydashboard** - Dashboard layout
- **tidyverse / dplyr** - Data manipulation
- **ggplot2** - Static visualizations
- **plotly** - Interactive charts

---

## 🛠️ Technologies Used

- R Shiny
- ggplot2 / plotly
- dplyr
- [other packages]

---

## 💻 How to Run Locally

```r
# Install required packages
install.packages(c("shiny", "ggplot2", "dplyr"))

# Run the app
shiny::runGitHub("sales-dashboard", "gmzainab16-eng")
