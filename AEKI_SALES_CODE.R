# Loading Libraries
library(readxl)       # For reading Excel files
library(openxlsx)     # Enhanced Excel file reading and writing
library(dplyr)        # Data manipulation
library(tidyverse)    # Data manipulation and visualization
library(ggplot2)      # For data visualization
library(DataExplorer) # Automating exploratory data analysis

# Loading Datasets
base_path <- "/Users/hugogonzalez/Desktop/BIDA /DATA ANALISYS /"
Orders <- read_excel(paste0(base_path, "AEKI_Data.xlsx"), sheet = "Orders")
Products <- read_excel(paste0(base_path, "AEKI_Data.xlsx"), sheet = "Products")
Returns <- read_excel(paste0(base_path, "AEKI_Data.xlsx"), sheet = "Returns")
Demographics <- read_excel(paste0(base_path, "AEKI_Data.xlsx"), sheet = "Demographics")
World_Cities <- read.csv(paste0(base_path, "worldcities.csv"))
Orders_2016 <- read_excel(paste0(base_path, "AEKI_2016.xlsx"))


# Standardizing The Datasets

# Renaming Columns in Products Dataset
colnames(Products)[colnames(Products) == "ID Product"] <- "Product ID"

# Function to Replace Spaces and Hyphens in Column Names
rename_columns <- function(data) {
    colnames(data) <- gsub(" |-", "_", colnames(data))
    return(data)
}

# Applying the Function to Rename Columns
Orders <- rename_columns(Orders)
Products <- rename_columns(Products)
Returns <- rename_columns(Returns)
Demographics <- rename_columns(Demographics)
Orders_2016 <- rename_columns(Orders_2016)

# Dropping Columns in Orders Dataset
Orders <- subset(Orders, select = -Row_ID)
Orders_2016 <- subset(Orders_2016, select = -Row_ID)

# Renaming and Filtering Columns in World Cities Dataset
World_Cities <- World_Cities %>%
    select(city, country, admin_name, lat, lng, population, id) %>%
    rename(City_ID = id, City_Population = population, City = city, State = admin_name, Country = country) %>%
    filter(Country == "United States")

# Renaming Specific City Names in World Cities Dataset
World_Cities$City[World_Cities$City == "New York"] <- "New York City"
World_Cities$City[World_Cities$City == "Port St. Lucie"] <- "Port Saint Lucie"
World_Cities$City[World_Cities$City == "McAllen"] <- "Mcallen"
World_Cities$City[World_Cities$City == "St. Cloud" & World_Cities$State == "Minnesota"] <- "Saint Cloud"
World_Cities$City[World_Cities$City == "St. Petersburg"] <- "Saint Petersburg"
World_Cities$City[World_Cities$City == "St. Charles"] <- "Saint Charles"
World_Cities$City[World_Cities$City == "St. Louis"] <- "Saint Louis"
World_Cities$City[World_Cities$City == "St. Peters"] <- "Saint Peters"
World_Cities$City[World_Cities$City == "St. Paul"] <- "Saint Paul"
World_Cities$City[World_Cities$City == "Milford city"] <- "Milford"
World_Cities$City[World_Cities$City == "Novi" & World_Cities$State == "Michigan"] <- "Canton"



# Create a new dataframes with selected columns from the original data frame
Orders_Processing <- Orders
Products_Processing <- Products
Orders_2016_Processing <- Orders_2016
Returns_Processing <- Returns
Citites_Processing <- World_Cities
Demographics_Processing <- Demographics

# Products Dataframe Preparation 

# Dropping the duplicate rows from the Product ID column
# Dropping the duplicate rows from the Product ID column
Products_Processing <- Products_Processing[!duplicated(Products_Processing$Product_ID), ]


# Rename the values in columnn Category:  "Office Suplies"  to "Office Supplies"
Products_Processing$Category[Products_Processing$Category == "Office Suplies"] <- "Office Supplies"

# Merge Data Frames: Orders and Products 

# Combine the two data sets based on the Product ID column
Orders_Processing <- merge(Orders_Processing, Products_Processing, by = "Product_ID")

#Merge Data Frames: Orders_Products and Orders_2016 

# Combine the two data sets based on the Order ID column
Orders_Processing <- rbind(Orders_Processing, Orders_2016_Processing)

# Merge Data Frames: Orders_Processing and Returns 
Orders_Processing <- Orders_Processing %>%
    left_join(Returns_Processing[, c("Order_ID", "Returned")], by = "Order_ID")

# Replace NA in the Returned column with "no"
Orders_Processing$Returned[is.na(Orders_Processing$Returned)] <- "No"

# Fix Values in Country Column 

# Add observations with Italy values into another dataset
Orders_Wrong_Entries <- Orders_Processing[Orders_Processing$Country == "Italy", ]

# Drop the observations with Italy values from the original dataset
Orders_Processing <- Orders_Processing[Orders_Processing$Country != "Italy", ]

# Discount 

Orders_Processing <- Orders_Processing %>%
    mutate(Discount = case_when(
        Discount == 1.4 ~ 0.4,
        Discount == 1.6 ~ 0.6,
        TRUE ~ Discount
    ))

# Quantity Column 

# Put it in the Orders_Wrong_Entries df
Orders_Wrong_Entries <- rbind(Orders_Wrong_Entries, Orders_Processing[Orders_Processing$Quantity == 19, ])

# Drop it from the original dataset
Orders_Processing <- Orders_Processing[Orders_Processing$Quantity != 19, ]

# We drop it beacause sales are 0,0002

#Profit Column 
# Add the observations with profit more than 22638.48 to the Orders_Wrong_Entries df
Orders_Wrong_Entries <- rbind(Orders_Wrong_Entries, Orders_Processing[Orders_Processing$Profit > 22638.48, ])

# Drop the observations with profit more than 22638.48 from the original dataset
Orders_Processing <- Orders_Processing[Orders_Processing$Profit <= 22638.48, ]

# Products 

# Add Test value from sub-category column to Wrong_Entries df
Orders_Wrong_Entries <- rbind(Orders_Wrong_Entries, Orders_Processing[Orders_Processing$Sub_Category == "Test", ])

# Drop Test value from sub-category column
Orders_Processing <- Orders_Processing[Orders_Processing$Sub_Category != "Test", ]

# Merging Data Frames: Orders_Processing with Cities 
# Rename Orange to East Orange ###
Orders_Processing$City[Orders_Processing$City == "Orange"] <- "East Orange"

# Merge the datasets again
Orders_Processing <- Orders_Processing %>%
    left_join(Citites_Processing, by = c("City" = "City", "State" = "State", "Country" = "Country"))


# Merging Data Frames: Orders_Processing with Demographics 
# Perform the left join
Orders_Processing <- Orders_Processing %>%
    left_join(Demographics_Processing, by = "State")

#Extracting Time Data

# ExtraActing the year

# Extract the year and add it as a new column in the new dataset
Orders_Processing$Year <- as.numeric(format(Orders_Processing$Order_Date, "%Y"))

#Extracting the month
# Extract the month and add it as a new column in the new dataset
Orders_Processing$Month <- as.numeric(format(Orders_Processing$Order_Date, "%m"))

# Extracting the day

# Extract the day and add it as a new column in the new dataset
Orders_Processing$Day <- as.numeric(format(Orders_Processing$Order_Date, "%d"))

# Extracting the week-day 

Orders_Processing$Day_of_Week <- weekdays(Orders_Processing$Order_Date)

#Calculating Processing Days

# Calucalate processing days based on Order_Date and Ship_Date
# Calculate the distance between the Order_Date and Ship_Date
Orders_Processing$Processing_Days <- as.numeric(difftime(Orders_Processing$Ship_Date, Orders_Processing$Order_Date, units = "days"))


# Calculating Product Prices 
# Calculate Net Sales and Unit Price
Orders_Processing <- Orders_Processing %>%
    mutate(
        Net_Sales = Sales - (Sales * Discount), # Calculate Net Sales
        Unit_Price = Net_Sales / Quantity # Calculate Unit Price
    )


# Calculate Avg Min Max 

Orders_Processing <- Orders_Processing %>%
    group_by(Product_ID, Year) %>%
    mutate(
        Average_Unit_Price = mean(Unit_Price, na.rm = TRUE),
        Max_Unit_Price = max(Unit_Price, na.rm = TRUE),
        Min_Unit_Price = min(Unit_Price, na.rm = TRUE)
    ) %>%
    ungroup()


# Calculate Total Order, Profit  

Orders_Processing <- Orders_Processing %>%
    group_by(Order_ID) %>%
    mutate(
        Total_Order_Price = sum(Net_Sales, na.rm = TRUE), # Total Net Sales per order
        Total_Order_Profit = sum(Profit, na.rm = TRUE) # Total Profit per order
    ) %>%
    ungroup()


ggplot(Orders_Processing, aes(x = Category, y = Average_Unit_Price, color = Category)) +
    geom_boxplot(outlier.color = "#ff0000") +
    ggtitle("Average Product Price per Category") +
    xlab("Category") +
    ylab("Average Unit Price") +
    scale_color_brewer(palette = "Set1") +
    theme_minimal()

Orders_Processing %>%
    filter(Category == "Technology") %>%
    ggplot(aes(x = Sub_Category, y = Average_Unit_Price, color = Sub_Category)) +
    geom_boxplot(outlier.color = "black") +
    ggtitle("Technology") +
    xlab("Sub_Category") +
    ylab("Average Unit Price") +
    scale_color_brewer(palette = "Set1") +
    theme_minimal()
Orders_Processing %>%
    filter(Category == "Office Supplies") %>%
    ggplot(aes(x = Sub_Category, y = Average_Unit_Price, color = Sub_Category)) +
    geom_boxplot(outlier.color = "black") +
    ggtitle("Office Supplies") +
    xlab("Sub_Category") +
    ylab("Average Unit Price") +
    scale_color_brewer(palette = "Set1") +
    theme_minimal()
Orders_Processing %>%
    filter(Category == "Furniture") %>%
    ggplot(aes(x = Sub_Category, y = Average_Unit_Price, color = Sub_Category)) +
    geom_boxplot(outlier.color = "black") +
    ggtitle("Furniture") +
    xlab("Sub_Category") +
    ylab("Average Unit Price") +
    scale_color_brewer(palette = "Set1") +
    theme_minimal()
  ggplot(Orders_Processing, aes(x=Category, y= Average_Unit_Price, color = Category)) +
  geom_boxplot(outlier.color = "black") +
  facet_wrap(~ Year, scales = "fixed", nrow = 3,) +
  ggtitle("Avarage Unit Price by Category") +
  xlab("Category") +
  ylab("Average Unit Price") +
  scale_color_brewer(palette = "Set1") +
  theme_minimal()
# Creating the Classification
data_classification <- Orders_Processing %>%
    mutate(Price_Range_Category = case_when(
        Average_Unit_Price > 1000 ~ "Above 1000",
        Average_Unit_Price > 500 & Average_Unit_Price <= 1000 ~ "500-1000",
        Average_Unit_Price > 400 & Average_Unit_Price <= 500 ~ "400-500",
        Average_Unit_Price > 300 & Average_Unit_Price <= 400 ~ "300-400",
        Average_Unit_Price > 250 & Average_Unit_Price <= 300 ~ "250-300",
        Average_Unit_Price > 200 & Average_Unit_Price <= 250 ~ "200-250",
        Average_Unit_Price > 150 & Average_Unit_Price <= 200 ~ "150-200",
        Average_Unit_Price > 100 & Average_Unit_Price <= 150 ~ "100-150",
        Average_Unit_Price > 50 & Average_Unit_Price <= 100 ~ "50-100",
        Average_Unit_Price > 25 & Average_Unit_Price <= 50 ~ "25-50",
        Average_Unit_Price > 10 & Average_Unit_Price <= 25 ~ "10-25",
        Average_Unit_Price > 5 & Average_Unit_Price <= 10 ~ "5-10",
        Average_Unit_Price > 0 & Average_Unit_Price <= 5 ~ "0-5",
        TRUE ~ "Other" # Catch-all for any unexpected cases
    ))
aggregated_data <- data_classification %>%
    group_by(Year, Price_Range_Category) %>%
    summarise(
        Total_Orders = n(), # Count total number of orders
        Total_Gross_Revenue = sum(Sales, na.rm = TRUE), # Sum total sales
        Total_Net_Revenue = sum(Net_Sales, na.rm = TRUE), # Sum total net sales
        Total_Profit = sum(Profit, na.rm = TRUE), # Sum total profit
        Total_Products = n_distinct(Product_ID), # Count unique products
        Total_Number_of_Returns = sum(Returned == "Yes", na.rm = TRUE), # Count total number of returns
        Order_After_Returns = n() - sum(Returned == "Yes", na.rm = TRUE), # Count total number of orders after returns
        Gross_Revenue_After_Returns = sum(Sales, na.rm = TRUE) - sum(Sales[Returned == "Yes"], na.rm = TRUE), # Sum total sales after returns
        Net_Revenue_After_Returns = sum(Net_Sales, na.rm = TRUE) - sum(Net_Sales[Returned == "Yes"], na.rm = TRUE), # Sum total net sales after returns
        Profit_After_Returns = sum(Profit, na.rm = TRUE) - sum(Profit[Returned == "Yes"], na.rm = TRUE), # Sum total profit after returns
        Avg_Unit_Price_Category = mean(Average_Unit_Price, na.rm = TRUE), # Average unit price
        Avg_Discount = mean(Discount, na.rm = TRUE), # Average discount
        Avg_Quantity = mean(Quantity, na.rm = TRUE), # Average quantity
        .groups = "drop"
    )

head(aggregated_data)
aggregated_data %>%
    filter(Price_Range_Category == "Above 1000" | Price_Range_Category == "500-1000" | Price_Range_Category == "400-500" | Price_Range_Category == "300-400" | Price_Range_Category == "250-300") %>%
    ggplot(aes(x = Year, y = Total_Orders, group = Price_Range_Category, color = Price_Range_Category)) +
    geom_line() +
    theme_minimal() +
    labs(title = "Total Orders by Year and Price Range", x = "Year", y = "Total Orders")

aggregated_data %>%
    filter(Price_Range_Category == "200-250" | Price_Range_Category == "150-200" | Price_Range_Category == "100-150") %>%
    ggplot(aes(x = Year, y = Total_Orders, group = Price_Range_Category, color = Price_Range_Category)) +
    geom_line() +
    theme_minimal() +
    labs(title = "Total Orders by Year and Price Range", x = "Year", y = "Total Orders")

aggregated_data %>%
    filter(Price_Range_Category == "50-100" | Price_Range_Category == "25-50" | Price_Range_Category == "10-25" | Price_Range_Category == "5-10" | Price_Range_Category == "0-5") %>%
    ggplot(aes(x = Year, y = Total_Orders, group = Price_Range_Category, color = Price_Range_Category)) +
    geom_line() +
    theme_minimal() +
    labs(title = "Total Orders by Year and Price Range", x = "Year", y = "Total Orders")

long_data_orders_returns <- aggregated_data %>%
    pivot_longer(
        cols = c(Total_Orders, Total_Number_of_Returns),
        names_to = "Metric",
        values_to = "Value"
    ) %>%
    mutate(Metric = factor(Metric, levels = c("Total_Orders", "Total_Number_of_Returns")))

ggplot(long_data_orders_returns %>% filter(Year == 2014), aes(x = Price_Range_Category, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
  geom_text(aes(label = Value), vjust = -0.3, position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c("Total_Orders" = "#13b4d1", "Total_Number_of_Returns" = "#f14b4e")) +
  theme_minimal() +
  labs(
    title = "Total Number of Orders and Returns for Each Price Range in 2014",
    x = "Price Range Category",
    y = "Total"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(long_data_orders_returns %>% filter(Year == 2015), aes(x = Price_Range_Category, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
  geom_text(aes(label = Value), vjust = -0.3, position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c("Total_Orders" = "#13b4d1", "Total_Number_of_Returns" = "#f14b4e")) +
  theme_minimal() +
  labs(
    title = "Total Number of Orders and Returns for Each Price Range in 2015",
    x = "Price Range Category",
    y = "Total"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(long_data_orders_returns %>% filter(Year == 2016), aes(x = Price_Range_Category, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
  geom_text(aes(label = Value), vjust = -0.3, position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c("Total_Orders" = "#13b4d1", "Total_Number_of_Returns" = "#f14b4e")) +
  theme_minimal() +
  labs(
    title = "Total Number of Orders and Returns for Each Price Range in 2016",
    x = "Price Range Category",
    y = "Total"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(long_data_orders_returns %>% filter(Year == 2017), aes(x = Price_Range_Category, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
  geom_text(aes(label = Value), vjust = -0.3, position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c("Total_Orders" = "#13b4d1", "Total_Number_of_Returns" = "#f14b4e")) +
  theme_minimal() +
  labs(
    title = "Total Number of Orders and Returns for Each Price Range in 2017",
    x = "Price Range Category",
    y = "Total"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
order_summary <- Orders_Processing %>%
    group_by(Returned) %>%
    summarise(Count = n(), .groups = "drop") %>%
    mutate(Percentage = Count / sum(Count) * 100)

ggplot(order_summary, aes(x = "", y = Count, fill = factor(Returned))) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar(theta = "y") +
    geom_text(aes(label = paste0(round(Percentage, 1), "%")),
        position = position_stack(vjust = 0.5)
    ) +
    scale_fill_manual(
        values = c("#13b4d1", "#f14b4e"),
        labels = c("Not Returned", "Returned"),
        name = "Order Status"
    ) +
    labs(
        title = "Proportion of Returned vs Non-Returned Orders",
        x = NULL,
        y = NULL
    ) +
    theme_minimal() +
    theme(
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.title = element_text(size = 10)
    )

ggplot(aggregated_data, aes(x = Year, y = Price_Range_Category, fill = Total_Orders)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Total_Orders), color = "black", size = 3, vjust = 1) +
    scale_fill_gradient2(low = "#00ff9d", high = "#0011ff", mid = "#117dd0", midpoint = median(aggregated_data$Total_Orders), space = "Lab") +
    theme_minimal() +
    theme(
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(hjust = 0.5)
    ) +
    labs(title = "Heatmap of Total Number of Orders by Year and Price Range", x = "Year", y = "Price Range Category")

ggplot(aggregated_data, aes(x = Year, y = Price_Range_Category, fill = Total_Number_of_Returns)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Total_Number_of_Returns), color = "black", size = 3, vjust = 1) +
    scale_fill_gradient2(low = "#fffb00", high = "red", mid = "red", midpoint = median(aggregated_data$Total_Orders), space = "Lab") +
    theme_minimal() +
    theme(
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(hjust = 0.5)
    ) +
    labs(title = "Heatmap of Total Number of Orders by Year and Price Range", x = "Year", y = "Price Range Category")
ggplot(aggregated_data, aes(x = Year, y = Gross_Revenue_After_Returns, fill = Price_Range_Category)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~Price_Range_Category, ncol = 3) +
    theme_minimal() +
    labs(title = "Gross Revenue After Returns by Year and Price Range", x = "Year", y = "Gross Revenue After Returns")

long_data_netrev_profit <- aggregated_data %>%
    pivot_longer(
        cols = c(Net_Revenue_After_Returns, Profit_After_Returns),
        names_to = "Metric",
        values_to = "Value"
    ) %>%
    mutate(Metric = factor(Metric, levels = c("Net_Revenue_After_Returns", "Profit_After_Returns")))

ggplot(long_data_netrev_profit %>% filter(Year == 2014), aes(x = Price_Range_Category, y = Value, fill = Metric)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
    scale_fill_manual(values = c("Net_Revenue_After_Returns" = "#4a4ced", "Profit_After_Returns" = "#3a902b")) +
    theme_minimal() +
    labs(
        title = "Net Sales and Profit in 2014",
        x = "Price Range Category",
        y = "Total"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(long_data_netrev_profit %>% filter(Year == 2015), aes(x = Price_Range_Category, y = Value, fill = Metric)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
    scale_fill_manual(values = c("Net_Revenue_After_Returns" = "#4a4ced", "Profit_After_Returns" = "#3a902b")) +
    theme_minimal() +
    labs(
        title = "Net Sales and Profit in 2015",
        x = "Price Range Category",
        y = "Total"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(long_data_netrev_profit %>% filter(Year == 2016), aes(x = Price_Range_Category, y = Value, fill = Metric)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
    scale_fill_manual(values = c("Net_Revenue_After_Returns" = "#4a4ced", "Profit_After_Returns" = "#3a902b")) +
    theme_minimal() +
    labs(
        title = "Net Sales and Profit in 2016",
        x = "Price Range Category",
        y = "Total"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(long_data_netrev_profit %>% filter(Year == 2017), aes(x = Price_Range_Category, y = Value, fill = Metric)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
    scale_fill_manual(values = c("Net_Revenue_After_Returns" = "#4a4ced", "Profit_After_Returns" = "#3a902b")) +
    theme_minimal() +
    labs(
        title = "Net Sales and Profit in 2017",
        x = "Price Range Category",
        y = "Total"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(aggregated_data, aes(x = Avg_Unit_Price_Category, y = Order_After_Returns, color = Price_Range_Category)) +
    geom_point() +
    theme_minimal() +
    labs(title = "Total Orders vs Average Unit Price", x = "Average Unit Price", y = "Total Orders")
ggplot(aggregated_data, aes(x = Avg_Unit_Price_Category, y = Net_Revenue_After_Returns, color = Price_Range_Category)) +
    geom_point() +
    theme_minimal() +
    labs(title = "Revenue vs Average Unit Price", x = "Average Unit Price", y = "Revenue")
ggplot(aggregated_data, aes(x = Avg_Unit_Price_Category, y = Profit_After_Returns, color = Price_Range_Category)) +
    geom_point() +
    theme_minimal() +
    labs(title = "Profit vs Average Unit Price", x = "Average Unit Price", y = "Profit")
ggplot(aggregated_data, aes(x = Avg_Unit_Price_Category, y = Avg_Discount, color = Price_Range_Category)) +
    geom_point() +
    theme_minimal() +
    labs(title = "Average Disount vs Average Unit Price", x = "Average Unit Price", y = "Average Disount")
ggplot(aggregated_data, aes(x = Avg_Unit_Price_Category, y = Avg_Quantity, color = Price_Range_Category)) +
    geom_point() +
    theme_minimal() +
    labs(title = "Avg Quantity vs Average Unit Price", x = "Average Unit Price", y = "Average Discount")
aggregated_data_subcategory <- data_classification %>%
    mutate(Price_Range_Category = factor(Price_Range_Category,
        levels = c("0-5", "5-10", "10-25", "25-50", "50-100", "100-150", "150-200", "200-250", "250-300", "300-400", "400-500", "500-1000", "Above 1000"), # Add your price ranges here in order
        ordered = TRUE
    )) %>%
    group_by(Year, Price_Range_Category, Category, Sub_Category) %>%
    summarise(Total_Orders = n(), .groups = "drop")
aggregated_data_subcategory %>%
    filter(Price_Range_Category == "50-100" | Price_Range_Category == "25-50" | Price_Range_Category == "10-25" | Price_Range_Category == "5-10" | Price_Range_Category == "0-5") %>%
    ggplot(aes(x = Category, y = Total_Orders, fill = Category)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    facet_wrap(~Price_Range_Category, scales = "free_x", ncol = 5) +
    theme_minimal() +
    labs(
        title = "Total Orders by Category, and Price Range across Years",
        x = "Sub-Category",
        y = "Total Orders"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

aggregated_data_subcategory %>%
    filter(Price_Range_Category == "200-250" | Price_Range_Category == "150-200" | Price_Range_Category == "100-150") %>%
    ggplot(aes(x = Category, y = Total_Orders, fill = Category)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    facet_wrap(~Price_Range_Category, scales = "free_x", ncol = 5) +
    theme_minimal() +
    labs(
        title = "Total Orders by Category, and Price Range across Years",
        x = "Sub-Category",
        y = "Total Orders"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

aggregated_data_subcategory %>%
    filter(Price_Range_Category == "Above 1000" |
             Price_Range_Category == "500-1000" |
             Price_Range_Category == "400-500" |
             Price_Range_Category == "300-400" |
             Price_Range_Category == "250-300") %>%
    ggplot(aes(x = Category, y = Total_Orders, fill = Category)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    facet_wrap(~Price_Range_Category, scales = "free_x", ncol = 5) +
    theme_minimal() +
    labs(
        title = "Total Orders by Category, and Price Range across Years",
        x = "Sub-Category",
        y = "Total Orders"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(
    aggregated_data_subcategory %>%
        filter(Category == "Technology" &
            (Price_Range_Category == "50-100" |
                Price_Range_Category == "25-50" |
                Price_Range_Category == "10-25" |
                Price_Range_Category == "5-10" |
                Price_Range_Category == "0-5")),
    aes(x = Sub_Category, y = Total_Orders, fill = Sub_Category)
) +
    geom_bar(stat = "identity") +
    facet_wrap(~Price_Range_Category, scales = "free_x", ncol = 3) +
    theme_minimal() +
    labs(
        title = "Total Orders in Technology Category by Sub-Category and Price Range",
        x = "Sub-Category",
        y = "Total Orders"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(
    aggregated_data_subcategory %>%
        filter(Category == "Technology" &
            (Price_Range_Category == "200-250" |
               Price_Range_Category == "150-200" |
               Price_Range_Category == "100-150")),
    aes(x = Sub_Category, y = Total_Orders, fill = Sub_Category)
) +
    geom_bar(stat = "identity") +
    facet_wrap(~Price_Range_Category, scales = "free_x", ncol = 3) +
    theme_minimal() +
    labs(
        title = "Total Orders in Technology Category by Sub-Category and Price Range",
        x = "Sub-Category",
        y = "Total Orders"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(
    aggregated_data_subcategory %>%
        filter(Category == "Technology" &
            (Price_Range_Category == "Above 1000" |
               Price_Range_Category == "500-1000" |
               Price_Range_Category == "400-500" |
               Price_Range_Category == "300-400" |
               Price_Range_Category == "250-300")),
    aes(x = Sub_Category, y = Total_Orders, fill = Sub_Category)
) +
    geom_bar(stat = "identity") +
    facet_wrap(~Price_Range_Category, scales = "free_x", ncol = 3) +
    theme_minimal() +
    labs(
        title = "Total Orders in Technology Category by Sub-Category and Price Range",
        x = "Sub-Category",
        y = "Total Orders"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(
    aggregated_data_subcategory %>%
        filter(Category == "Office Supplies" &
            (Price_Range_Category == "50-100" |
                Price_Range_Category == "25-50" |
                Price_Range_Category == "10-25" |
                Price_Range_Category == "5-10" |
                Price_Range_Category == "0-5")),
    aes(x = Sub_Category, y = Total_Orders, fill = Sub_Category)
) +
    geom_bar(stat = "identity") +
    facet_wrap(~Price_Range_Category, scales = "free_x", ncol = 3) +
    theme_minimal() +
    labs(
        title = "Total Orders in Technology Category by Sub-Category and Price Range",
        x = "Sub-Category",
        y = "Total Orders"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(
    aggregated_data_subcategory %>%
        filter(Category == "Office Supplies" &
            (Price_Range_Category == "200-250" |
               Price_Range_Category == "150-200" |
               Price_Range_Category == "100-150")),
    aes(x = Sub_Category, y = Total_Orders, fill = Sub_Category)
) +
    geom_bar(stat = "identity") +
    facet_wrap(~Price_Range_Category, scales = "free_x", ncol = 3) +
    theme_minimal() +
    labs(
        title = "Total Orders in Technology Category by Sub-Category and Price Range",
        x = "Sub-Category",
        y = "Total Orders"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(
    aggregated_data_subcategory %>%
        filter(Category == "Office Supplies" &
            (Price_Range_Category == "Above 1000" |
               Price_Range_Category == "500-1000" |
               Price_Range_Category == "400-500" |
               Price_Range_Category == "300-400" |
               Price_Range_Category == "250-300")),
    aes(x = Sub_Category, y = Total_Orders, fill = Sub_Category)
) +
    geom_bar(stat = "identity") +
    facet_wrap(~Price_Range_Category, scales = "free_x", ncol = 3) +
    theme_minimal() +
    labs(
        title = "Total Orders in Technology Category by Sub-Category and Price Range",
        x = "Sub-Category",
        y = "Total Orders"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(
    aggregated_data_subcategory %>%
        filter(Category == "Furniture" &
            (Price_Range_Category == "50-100" |
                Price_Range_Category == "25-50" |
                Price_Range_Category == "10-25" |
                Price_Range_Category == "5-10" |
                Price_Range_Category == "0-5")),
    aes(x = Sub_Category, y = Total_Orders, fill = Sub_Category)
) +
    geom_bar(stat = "identity") +
    facet_wrap(~Price_Range_Category, scales = "free_x", ncol = 3) +
    theme_minimal() +
    labs(
        title = "Total Orders in Technology Category by Sub-Category and Price Range",
        x = "Sub-Category",
        y = "Total Orders"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(
    aggregated_data_subcategory %>%
        filter(Category == "Furniture" &
            (Price_Range_Category == "200-250" |
               Price_Range_Category == "150-200" |
               Price_Range_Category == "100-150")),
    aes(x = Sub_Category, y = Total_Orders, fill = Sub_Category)
) +
    geom_bar(stat = "identity") +
    facet_wrap(~Price_Range_Category, scales = "free_x", ncol = 3) +
    theme_minimal() +
    labs(
        title = "Total Orders in Technology Category by Sub-Category and Price Range",
        x = "Sub-Category",
        y = "Total Orders"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(
    aggregated_data_subcategory %>%
        filter(Category == "Furniture" &
            (Price_Range_Category == "Above 1000" |
               Price_Range_Category == "500-1000" |
               Price_Range_Category == "400-500" |
               Price_Range_Category == "300-400" |
               Price_Range_Category == "250-300")),
    aes(x = Sub_Category, y = Total_Orders, fill = Sub_Category)
) +
    geom_bar(stat = "identity") +
    facet_wrap(~Price_Range_Category, scales = "free_x", ncol = 3) +
    theme_minimal() +
    labs(
        title = "Total Orders in Technology Category by Sub-Category and Price Range",
        x = "Sub-Category",
        y = "Total Orders"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

discount_aggregated <- Orders_Processing %>%
    mutate(
        Discount_Amount = Sales - Net_Sales, # Calculate discount amount for each order
        Discount_Percentage = Discount * 100 # Convert discount to percentage format
    ) %>%
    group_by(Year, Category, Sub_Category) %>% # Group by year, category and sub-category
    summarise(
        Total_Discount = sum(Discount_Amount), # Total discount amount
        Average_Discount_Percentage = mean(Discount_Percentage), # Average discount percentage
        .groups = "drop"
    )


ggplot(discount_aggregated, aes(x = Category, y = Average_Discount_Percentage, fill = Category)) +
    geom_boxplot() +
    theme_minimal() +
    labs(
        title = "Distribution of Discount Percentages by Category",
        x = "Category",
        y = "Discount Percentage"
    )

ggplot(discount_aggregated, aes(x = Sub_Category, y = Total_Discount, fill = Category)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    labs(
        title = "Total Discounts by Category and Subcategory",
        x = "Category",
        y = "Total Discount Amount"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(discount_aggregated, aes(x = Sub_Category, y = Average_Discount_Percentage, fill = Category)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    labs(
        title = "Average Percentage of Discounts by Category and Subcategory",
        x = "Category",
        y = "Total Discount Amount"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
discount_aggregated %>%
filter(Category == "Technology") %>%
ggplot(aes(x = Year, y = Total_Discount, color = Sub_Category)) +
    geom_line() +
    theme_minimal() +
    labs(
        title = "Trend of Total Discount Over Time by Category Technology",
        x = "Year",
        y = "Total Discount Amount"
    )
discount_aggregated %>%
filter(Category == "Office Supplies") %>%
ggplot(aes(x = Year, y = Total_Discount, color = Sub_Category)) +
    geom_line() +
    theme_minimal() +
    labs(
        title = "Trend of Total Discount Over Time by Category Office Supplies ",
        x = "Year",
        y = "Total Discount Amount"
    )

discount_aggregated %>%
filter(Category == "Furniture") %>%
ggplot(aes(x = Year, y = Total_Discount, color = Sub_Category)) +
    geom_line() +
    theme_minimal() +
    labs(
        title = "Trend of Total Discount Over Time by Category Furniture",
        x = "Year",
        y = "Total Discount Amount"
    )

discount_aggregated %>%
filter(Category == "Technology") %>%
ggplot(aes(x = Year, y = Average_Discount_Percentage, color = Sub_Category)) +
    geom_line() +
    theme_minimal() +
    labs(
        title = "Trend of Average_Discount_Percentage Over Time by Category Technology",
        x = "Year",
        y = "Average Discount %"
    )
discount_aggregated %>%
filter(Category == "Office Supplies") %>%
ggplot(aes(x = Year, y = Average_Discount_Percentage, color = Sub_Category)) +
    geom_line() +
    theme_minimal() +
    labs(
        title = "Trend of Average_Discount_Percentage Over Time by Category Office Supplies ",
        x = "Year",
        y = "Average Discount %"
    )

discount_aggregated %>%
filter(Category == "Furniture") %>%
ggplot(aes(x = Year, y = Average_Discount_Percentage, color = Sub_Category)) +
    geom_line() +
    theme_minimal() +
    labs(
        title = "Trend of Average_Discount_Percentage Over Time by Category Furniture",
        x = "Year",
        y = "Average Discount %"
    )

ggplot(discount_aggregated, aes(x = Average_Discount_Percentage, y = Total_Discount, color = Category)) +
    geom_point() +
    theme_minimal() +
    labs(
        title = "Relationship",
        x = "Average Discount Percentage",
        y = "Total Discount"
    )

discount_frequency <- Orders_2016_Processing %>%
    mutate(Discount_Percentage = Discount * 100) %>% # Convert to percentage format
    count(Discount_Percentage) %>%
    arrange(desc(n))

ggplot(discount_frequency, aes(x = factor(Discount_Percentage), y = n, fill = Discount_Percentage)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = n), vjust = -0.3, position = position_dodge(width = 0.9)) + # Display count on bars
    scale_fill_viridis_c() + # A more colorful palette
    theme_minimal()



library(sf)
library(ggplot2)
library(maps)

# Base world map
world <- sf::st_as_sf(maps::map("world", plot = FALSE, fill = TRUE))

# US states map
us_states <- sf::st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))

# ggplot code with Orders_Processing dataset
ggplot(data = us_states) +
    geom_sf() +
    geom_point(data = Orders_Processing, aes(x = lng, y = lat, color = Category), size = 2, alpha = 0.6) +
    coord_sf(xlim = c(-125, -65), ylim = c(25, 50), expand = FALSE) +
    labs(
        title = "Geographical Distribution of Orders in the USA",
        x = "Longitude",
        y = "Latitude"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom", legend.title.align = 0.5) +
    scale_color_brewer(type = "qual", palette = "Set1")

state_aggregated_data <- Orders_Processing %>%
    group_by(State) %>%
    summarise(
        Total_Profit = sum(Profit, na.rm = TRUE), # Replace Net_Sales with your measure
        Avg_Lat = mean(lat, na.rm = TRUE),
        Avg_Lng = mean(lng, na.rm = TRUE)
    ) %>%
    arrange(desc(Total_Profit))

top_states <- head(state_aggregated_data, 5)

ggplot() +
    geom_sf(data = us_states) +
    geom_point(data = top_states, aes(x = Avg_Lng, y = Avg_Lat, size = Total_Profit, color = State), alpha = 0.7) +
    coord_sf(xlim = c(-125, -65), ylim = c(25, 50), expand = FALSE) +
    labs(
        title = "Top States by Profit",
        x = "Longitude",
        y = "Latitude"
    ) +
    theme_minimal()

print(top_states)
city_aggregated_data <- Orders_Processing %>%
    group_by(City) %>%
    summarise(
        Total_Profit = sum(Profit, na.rm = TRUE), # Replace Net_Sales with your measure
        Avg_Lat = mean(lat, na.rm = TRUE),
        Avg_Lng = mean(lng, na.rm = TRUE)
    ) %>%
    arrange(desc(Total_Profit))

# Top 10 cities
top_cities <- head(city_aggregated_data, 10)

ggplot() +
    geom_sf(data = us_states) +
    geom_point(data = top_cities, aes(x = Avg_Lng, y = Avg_Lat, size = Total_Profit, color = City), alpha = 0.7) +
    coord_sf(xlim = c(-125, -65), ylim = c(25, 50), expand = FALSE) +
    labs(
        title = "Top Cities by Profit",
        x = "Longitude",
        y = "Latitude"
    ) +
    theme_minimal()

print(top_cities)

# Aggregate sales and profits by state, and calculate average population for each state
statewise_data <- Orders_Processing %>%
  group_by(State) %>%
  summarise(Total_Sales = sum(Sales),
            Total_Profit = sum(Profit),
            Average_Population = mean(Population2021)) %>%
  ungroup()

# Calculate correlations
sales_population_corr <- cor(statewise_data$Total_Sales, statewise_data$Average_Population)
profit_population_corr <- cor(statewise_data$Total_Profit, statewise_data$Average_Population)

# Print correlation coefficients
print(paste("Correlation between Sales and Population:", sales_population_corr))
print(paste("Correlation between Profit and Population:", profit_population_corr))

# Scatter plot for Sales vs Population
ggplot(statewise_data, aes(x = Average_Population, y = Total_Sales)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Sales vs Population", x = "Average Population", y = "Total Sales")

# Scatter plot for Profit vs Population
ggplot(statewise_data, aes(x = Average_Population, y = Total_Profit)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Profit vs Population", x = "Average Population", y = "Total Profit")

library(broom)


# Example analysis: Correlation between Discount and Sales
discount_sales_corr <- cor(Orders_Processing$Discount, Orders_Processing$Sales, use = "complete.obs")

# Fit a linear model
model <- lm(Sales ~ Discount, data = Orders_Processing)
summary(model) # To get R2 and other stats
model_fit <- glance(model)

# Print correlation coefficient and R2
print(paste("Correlation coefficient:", discount_sales_corr))
print(paste("R-squared:", model_fit$r.squared))

# Scatter plot with regression line
ggplot(Orders_Processing, aes(x = Discount, y = Sales)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Sales vs Discount", x = "Discount", y = "Sales")

# Calculating the correlation between Discount and Profit
discount_profit_corr <- cor(Orders_Processing$Discount, Orders_Processing$Profit, use = "complete.obs")

# Fit a linear regression model
model <- lm(Profit ~ Discount, data = Orders_Processing)
model_summary <- summary(model)
model_fit <- glance(model)

# Print correlation coefficient and R2
print(paste("Correlation coefficient between Discount and Profit:", discount_profit_corr))
print(paste("R-squared of the model:", model_fit$r.squared))

# Scatter plot with regression line for Discount vs Profit
ggplot(Orders_Processing, aes(x = Discount, y = Profit)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Profit vs Discount", x = "Discount", y = "Profit")



# Assuming you have already calculated the yearly_state_profit_per_capita

state_profit_per_capita <- Orders_Processing %>%
  group_by(State, Year) %>%
  summarise(Total_Profit = sum(Total_Order_Profit, na.rm = TRUE),
            Population = mean(Population2021, na.rm = TRUE)) %>%
  mutate(Profit_Per_Capita = Total_Profit / Population)

# Calculate the growth rate of profit per capita for each state
growth_rate_profit_per_capita <- state_profit_per_capita %>%
  arrange(State, Year) %>%
  group_by(State) %>%
  mutate(Profit_Per_Capita_Growth = (Profit_Per_Capita / lag(Profit_Per_Capita) - 1) * 100) %>%
  na.omit() %>%
  ungroup()

# Calculate average growth rate for each state
average_growth_rate <- growth_rate_profit_per_capita %>%
  group_by(State) %>%
  summarise(Average_Growth = mean(Profit_Per_Capita_Growth, na.rm = TRUE)) %>%
  ungroup()

# Select top 3 and bottom 3 states based on average growth rate
top_bottom_growth_states <- average_growth_rate %>%
  arrange(desc(Average_Growth)) %>%
  slice(c(1:5, (n()-4):n()))

# Plotting
ggplot(top_bottom_growth_states, aes(x = reorder(State, Average_Growth), y = Average_Growth, fill = Average_Growth)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 3 and Bottom 3 States by Average Growth Rate of Profit Per Capita",
       x = "State",
       y = "Average Growth Rate (%)") +
  theme_minimal() +
  scale_fill_gradient(low = "red", high = "green")  # Color gradient for visual appeal

# Save the plot
ggsave("growth_rate_profit_per_capita_plot.png", width = 10, height = 8)


# Average profti per person dataset
result <- Orders_Processing %>%
  group_by(State) %>%
  summarise(Total_Profit = sum(Total_Order_Profit),
            Avg_Population = mean(Population2021)) %>%
  mutate(Average_Profit_Per_Person = Total_Profit / Avg_Population)

# Plot the Average Profit Per Person for each State
ggplot(result, aes(x = reorder(State, Average_Profit_Per_Person), y = Average_Profit_Per_Person)) +
  geom_bar(stat = "identity", fill = "blue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Average Profit Per Person by State",
       x = "State",
       y = "Average Profit Per Person")

# Orders_Processing <- read.csv("path_to_your_Orders_Processing_file.csv")

# Calculate total profit for each state and then divide by the population of 2021
state_profit_per_capita <- Orders_Processing %>%
  group_by(State) %>%
  summarise(Total_Profit = sum(Total_Order_Profit, na.rm = TRUE),
            Population = mean(Population2021, na.rm = TRUE)) %>%
  mutate(Profit_Per_Capita = Total_Profit / Population)

# Arrange the data by Profit_Per_Capita
arranged_data <- state_profit_per_capita %>%
  arrange(desc(Profit_Per_Capita))

# Select the top 5 and bottom 5 states
top_bottom_states <- arranged_data %>%
  slice(c(1:5, (n()-4):n()))

# Plotting
ggplot(top_bottom_states, aes(x = reorder(State, Profit_Per_Capita), y = Profit_Per_Capita, fill = Profit_Per_Capita)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flips the axes for better readability
  labs(title = "Top 5 and Bottom 5 States by Profit Per Capita (2021)",
       x = "State",
       y = "Profit Per Capita") +
  theme_minimal() +
  scale_fill_gradient(low = "blue", high = "red")  # Color gradient for visual appeal





# Load the GDP dataset
gdp_data <- read_excel("/Users/hugogonzalez/Desktop/BIDA /DATA ANALISYS /GDP_PER.xlsx")

# Calculate Profit Per Person for each state
profit_per_person <- Orders_Processing %>%
  group_by(State) %>%
  summarise(Total_Profit = sum(Profit, na.rm = TRUE),
            Population = mean(Population2021, na.rm = TRUE),
            Profit_Per_Person = Total_Profit / Population) %>%
  ungroup()

# Identify common columns
common_columns <- intersect(names(result), names(gdp_data))

gdp_data <- gdp_data %>%
  rename(State = `State or Federal District`)

# Identifying the common columns
common_columns <- intersect(names(result), names(gdp_data))

# Adding the specific column 'Nominal GDP per Capita 2022' from gdp_data
additional_columns <- c(common_columns, "Nominal GDP per Capita 2022")

# Subsetting the results dataset to include only the common columns
results_common <- result[, common_columns]

# Subsetting the gdp_data dataset to include common columns and the additional column
gdp_data_common <- gdp_data[, additional_columns]

# Merging the datasets on the 'State' column
merged_data <- merge(results_common, gdp_data_common, by = "State")

# merge datasets of profit per person and gdp per capita 
gdp_person_state <- merge(merged_data, profit_per_person, by = "State")

# Remove the dollar sign ('$') and any commas, then convert to numeric
gdp_person_state$`Nominal GDP per Capita 2022` <- as.numeric(gsub("[\\$,]", "", gdp_person_state$`Nominal GDP per Capita 2022`))

# Create a new column with the ratio
gdp_person_state$Profit_GDP_Ratio <- gdp_person_state$Profit_Per_Person / gdp_person_state$`Nominal GDP per Capita 2022`

# Sort the data frame based on Nominal GDP per Capita 2022 in descending order
sorted_data <- gdp_person_state[order(-gdp_person_state$`Nominal GDP per Capita 2022`), ]

# Select the highest 5 and lowest 5 rows
highest_5 <- head(sorted_data, 5)
lowest_5 <- tail(sorted_data, 5)

# Combine the highest and lowest 5 rows into a single data frame
combined_data <- rbind(highest_5, lowest_5)

# Create a bar plot to visualize the selected values
ggplot(combined_data, aes(x = reorder(State, -Profit_GDP_Ratio), y = Profit_GDP_Ratio, fill = State)) +
  geom_bar(stat = "identity") +
  labs(title = "Highest and LoweSt 5 States by Profit to GDP Ratio",
       x = "State",
       y = "Profit to GDP Ratio") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_discrete() +
  coord_flip()


