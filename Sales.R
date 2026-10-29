library(tidyverse)
library(maps)
library(scales)  # For formatting numbers


data <- read_csv("Sales-Datacamp.csv") %>% 
        dplyr::select("Order Date","Segment","Country","City","State","Postal Code","Region",
                      "Category","Sub-Category","Sales","Quantity","Discount","Profit") %>%
        mutate(`Order Date` = as.Date(`Order Date`,format="%m/%d/%y"))
data
str(data)

# Summarize Sales and Profit by Region
regional_contributions <- data %>% group_by(Region) %>% 
                          summarise(Sales = sum(Sales, na.rm = TRUE),Profit = sum(Profit, na.rm = TRUE))
regional_contributions
str(regional_contributions)

# Summarize Sales by Sub-Category and Segment
sales_per_segment <- data %>% group_by(`Sub-Category`, Segment) %>%
                     summarise(Sales = sum(Sales, na.rm = TRUE)) %>%
                     ungroup() %>% spread(Segment,Sales)  
sales_per_segment
str(sales_per_segment)

# Summarize Quarterly Sales by Region
quarterly_sales <- data %>%
  mutate(Quarter = paste0("Q", quarter(`Order Date`)), 
         Year = year(`Order Date`)) %>%
  group_by(Region, Year, Quarter) %>%
  summarise(Sales = sum(Sales, na.rm = TRUE)) %>%
  arrange(Region, Year, Quarter) %>% unite(Year,Quarter,col="Quarters",sep="-") %>% spread(Region,Sales)
quarterly_sales
str(quarterly_sales)

# Summarize Monthly Sales
monthly_sales_summary <- data %>%
  mutate(Month = month(`Order Date`, label = TRUE, abbr = TRUE)) %>%  # Extract the month as a label (Jan, Feb, etc.)
  group_by(Month) %>%                                                 # Group by month (ignoring year)
  summarise(Monthly_Sales = sum(Sales, na.rm = TRUE)) %>%             # Sum sales for each month
  arrange(match(Month, month.abb)) 

# Summarize Total Sales by State
state_sales <- data %>%
  group_by(State) %>%
  summarise(Total_Sales = sum(Sales, na.rm = TRUE))


########################################################################################################
# Plot...

# Create a donut chart
ggplot(regional_contributions, aes(x = 2, y = Sales, fill = Region)) +
  geom_col(color = "white", width = 1) +  # Creates the donut sections
  coord_polar(theta = "y") +              # Converts to polar coordinates
  xlim(0.5, 2.5) +                        # Adds the hole in the center
  theme_void() +                          # Removes background and axes
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, size = 16)  # Centers title and sets font size
  ) +
  labs(title = "Regional Sales Contributions") +
  scale_fill_brewer(palette = "Set3")     # Optional color palette for differentiation
# Show all available palettes: RColorBrewer::display.brewer.all()

# Create a column chart for total sales by month (aggregated across all years)
ggplot(monthly_sales_summary, aes(x = Month, y = Monthly_Sales)) +
  geom_col(fill = "skyblue", color = "black") +  # Creates columns with specified colors
  geom_text(aes(label = paste0(scales::comma(round(Monthly_Sales / 1000)), "K")),  # Add labels with commas for readability
            vjust = -0.3, size = 3.5) +  # Adjust position and size of the labels
  labs(title = "Total Sales by Month (Aggregated Across All Years)",
       x = "Month",
       y = "Total Sales") +
  #scale_y_continuous(labels = scales::comma) +  # Format Y-axis with commas for numeric readability
  scale_y_continuous(labels = function(x) paste0(scales::comma(x / 1000), "K")) +  # Format Y-axis in thousands with "K"
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Adjust x-axis labels for readability

# Plotting the bar chart for total sales per state
ggplot(state_sales, aes(x = reorder(State, Total_Sales), y = Total_Sales)) +
  geom_col(fill = "skyblue") +
  coord_flip() +  # Flips the axes for a horizontal bar chart
  labs(title = "Total Sales per State", x = "State", y = "Total Sales") +
  theme_minimal()


################
#Maps
################
# Convert state names to lowercase for matching with map data
state_sales$State <- tolower(state_sales$State)

# Load U.S. state map data
us_states <- map_data("state")

# Join sales data with map data
map_data <- us_states %>%
  left_join(state_sales, by = c("region" = "State"))

# Plot the map
ggplot(map_data, aes(x = long, y = lat, group = group, fill = Total_Sales)) +
  geom_polygon(color = "white") +
  scale_fill_continuous(low = "lightblue", high = "darkblue", na.value = "grey80", 
                        name = "Total Sales", labels = scales::comma) +
  labs(title = "Total Sales by State") +
  #theme_void()
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank()
  )

### With Bubbles
# Calculate state centroids
state_centroids <- us_states %>%
  group_by(region) %>%
  summarise(long = mean(range(long)), lat = mean(range(lat)))

# Join sales data with centroids
centroid_sales <- state_centroids %>%
  left_join(state_sales, by = c("region" = "State"))

# Plot the map with bubbles
ggplot() +
  # Draw the map with state borders
  geom_polygon(data = us_states, aes(x = long, y = lat, group = group),
               fill = "lightgrey", color = "white") +
  # Add bubbles based on Total Sales
  geom_point(data = centroid_sales, aes(x = long, y = lat, size = Total_Sales),
             color = "blue", alpha = 0.7) +
  scale_size_continuous(range = c(3, 15), name = "Total Sales", labels = scales::comma) +
  labs(title = "Total Sales by State with Bubble Size") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank()
  )
