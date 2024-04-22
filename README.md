# <p align="center"> R-project-on-Global-Superstore </p>

# <p align="center"> ![image](https://github.com/manojgaikwad13/R-project-on-Global-Superstore/assets/96239993/b859c143-2f02-4c9a-8524-6fe872627f28) </p>

## Overview 
This R project aims to analyze the Global Superstore dataset to gain insights into sales trends, customer behavior, and geographical patterns. The dataset contains information about orders, customers, products, and sales across multiple regions and categories.
Purpose: 
This section briefly explains why the project exists and what problem it aims to solve. In the case of the Global Superstore Data Analysis Project, the purpose could be to extract meaningful insights from the Global Superstore dataset to inform business decisions, improve sales strategies, or optimize operations.

### For example:

Analyze sales trends over time to identify seasonal patterns.
Segment customers based on purchasing behavior to target marketing efforts effectively.
Determine the most profitable product categories and regions.
Forecast future sales to aid in inventory management and resource planning.
Scope: This defines the boundaries of the project, indicating what will and will not be covered. It helps manage expectations and provides clarity on what stakeholders can expect from the analysis. 

### For instance:

The analysis will focus on historical sales data from the Global Superstore dataset.
Customer demographic data may be included for segmentation analysis.
The project will not delve into external factors influencing sales, such as economic indicators or competitor analysis.

**Tools:-** Excel,RStudio

[Dataset Used](Global-Superstore.csv)

[R Programming (Code)](Project_R_Final.R)

## Analysis

```r
#General Overview:
#What are the dimensions of the dataset?
dim(a)

#How many records (rows) are there in the dataset?
paste("The Number of Rows are : ",nrow(a))

#What are the variables (columns) present in the dataset?
paste("Column Names Present in a Dataset are : ")
colnames(a)
```
Result : 
# <p align="center">![image](https://github.com/manojgaikwad13/R-project-on-Global-Superstore/assets/96239993/03a29863-f0a6-4834-b2cc-ee05d5ac9713)
</p>


```r
#Data Distribution:
#What is the distribution of sales, profits, and quantities sold?
summary(a$Sales)
summary(a$Profit)
summary(a$Quantity)

#Are there any outliers in sales or profits?
boxplot(a$Sales)
boxplot(a$Profit)

#How do sales and profits vary across different product categories, regions, or customer segments?
# Sales and profits by product categories
sales_by_category <- aggregate(a$Sales, by = list(Category = a$Category), FUN = sum)
sales_by_category
profits_by_category <- aggregate(a$Profit, by = list(Category = a$Category), FUN = sum)
profits_by_category

# Sales and profits by regions
sales_by_region <- aggregate(a$Sales, by = list(Region = a$Region), FUN = sum)
sales_by_region
profits_by_region <- aggregate(a$Profit, by = list(Region = a$Region), FUN = sum)
profits_by_region

# Sales and profits by customer segments
sales_by_segment <- aggregate(a$Sales, by = list(Segment = a$Segment), FUN = sum)
sales_by_segment
profits_by_segment <- aggregate(a$Profit, by = list(Segment = a$Segment), FUN = sum)
profits_by_segment

```
Result : 
# <p align="center">![image](https://github.com/manojgaikwad13/R-project-on-Global-Superstore/assets/96239993/03a29863-f0a6-4834-b2cc-ee05d5ac9713)
</p>


```r
#Geographical Analysis:
#Which regions or countries have the highest sales or profits?
a$Region[which.max(a$Sales)]
a$Country[which.max(a$Profit)]

#Are there any regional patterns in product preferences or sales behavior?
# Calculate total sales for each product category by region
sales_by_category_region <- aggregate(a$Sales, by = list(Region = a$Region, Category = a$Category), FUN = sum)
sales_by_category_region
# Visualize regional patterns in product preferences
library(ggplot2)
ggplot(sales_by_category_region, aes(x = Region, y = Sales, fill = Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Sales by Product Category and Region")


#How do sales vary across different regions or countries?
# Calculate total sales for each region or country
total_sales_by_region <- aggregate(a$Sales, by = list(Region = a$Region), FUN = sum)
total_sales_by_region
total_sales_by_country <- aggregate(a$Sales, by = list(Country = a$Country), FUN = sum)
total_sales_by_country
# Visualize sales variation across different regions or countries
ggplot(total_sales_by_region, aes(x = reorder(Region, -x), y = x)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Total Sales by Region")

ggplot(total_sales_by_country, aes(x = reorder(Country, -x), y = x)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(title = "Total Sales by Country")
```
Result : 
# <p align="center">![image](https://github.com/manojgaikwad13/R-project-on-Global-Superstore/assets/96239993/03a29863-f0a6-4834-b2cc-ee05d5ac9713)
</p>


```r
#Product Analysis:
#What are the top-selling products?
library(dplyr)
top_selling = a%>%
  group_by(a$Sub.Category)%>%
  summarize(Total_Sales = sum(Sales))%>%
  arrange(desc(Total_Sales))
top_selling

#Which product categories contribute the most to the overall sales or profits?
top_selling = a%>%
  group_by(a$Category)%>%
  summarize(Total_Profit = sum(Profit))%>%
  arrange(desc(Total_Profit))%>%
  head(1)
top_selling
```
Result : 
# <p align="center">![image](https://github.com/manojgaikwad13/R-project-on-Global-Superstore/assets/96239993/03a29863-f0a6-4834-b2cc-ee05d5ac9713)
</p>


```r
#Customer Analysis:
#Who are the most valuable customers (in terms of sales or profits)?
top_customers <- a %>%
  group_by(Customer.Name) %>%
  summarize(Total_Sales = sum(Sales), Total_Profit = sum(Profit)) %>%
  arrange(desc(Total_Sales))
top_customers

#Are there any patterns in purchasing behavior among different customer segments?
purchasing_behavior <- a %>%
  group_by(Segment) %>%
  summarize(Average_Sales = mean(Sales),
            Total_Sales = sum(Sales),
            Average_Quantity = mean(Quantity),
            Total_Quantity = sum(Quantity))

print(purchasing_behavior)


ggplot(a, aes(x = "Order.Date", y = "Sales", color = Segment)) +
  geom_boxplot() +
  labs(title = "Purchasing Behavior by Customer Segment Over Time",
       x = "Order Date",
       y = "Sales",
       color = "Customer Segment") +
  theme_minimal()

#How do sales vary across different customer segments?
sales_by_segment <- aggregate(Sales ~ Segment, data = a, sum)
print(sales_by_segment)

sales_by_segment <- sales_by_segment[order(-sales_by_segment$Sales), ]
print(sales_by_segment)


ggplot(sales_by_segment, aes(x = Segment, y = Sales, fill = Segment)) +
  geom_bar(stat = "identity") +
  labs(title = "Sales by Customer Segment",
       x = "Customer Segment",
       y = "Sales",
       fill = "Customer Segment") +
  theme_minimal()
```
Result : 
# <p align="center">![image](https://github.com/manojgaikwad13/R-project-on-Global-Superstore/assets/96239993/03a29863-f0a6-4834-b2cc-ee05d5ac9713)
</p>

```r
#Correlation Analysis:
#Is there any correlation between sales, profits, and other variables (e.g., quantity sold, discount)?
correlation_matrix <- cor(a[, c("Sales", "Profit", "Quantity", "Discount")])


print(correlation_matrix)
install.packages("corrplot")
library(corrplot)
corrplot(correlation_matrix, method = "color")

#Are there any strong correlations between different variables in the dataset?
correlation_matrix <- cor(a[, c("Sales", "Profit", "Quantity", "Discount")])

strong_correlations <- subset(as.data.frame(as.table(correlation_matrix)),
                              abs(Freq) > 0.7 & Var1 != Var2)


print("Strong Correlations:")
print(strong_correlations)
```
Result : 
# <p align="center">![image](https://github.com/manojgaikwad13/R-project-on-Global-Superstore/assets/96239993/03a29863-f0a6-4834-b2cc-ee05d5ac9713)
</p>v


```r
#Customer Segmentation:
#How do sales and profits vary across different customer segments?
# Aggregating sales and profits by customer segment
sales_by_segment <- aggregate(Sales ~ Segment, data = a, sum)
profits_by_segment <- aggregate(Profit ~ Segment, data = a, sum)
print(sales_by_segment)
print(profits_by_segment)
# Sorting the results in descending order
sales_by_segment <- sales_by_segment[order(-sales_by_segment$Sales), ]
profits_by_segment <- profits_by_segment[order(-profits_by_segment$Profit), ]

# Visualizing sales by customer segment
ggplot(sales_by_segment, aes(x = Segment, y = Sales, fill = Segment)) +
  geom_bar(stat = "identity") +
  labs(title = "Sales by Customer Segment",
       x = "Customer Segment",
       y = "Sales",
       fill = "Customer Segment") +
  theme_minimal()

# Visualizing profits by customer segment
ggplot(profits_by_segment, aes(x = Segment, y = Profit, fill = Segment)) +
  geom_bar(stat = "identity") +
  labs(title = "Profits by Customer Segment",
       x = "Customer Segment",
       y = "Profits",
       fill = "Customer Segment") +
  theme_minimal()
```
Result : 
# <p align="center">![image](https://github.com/manojgaikwad13/R-project-on-Global-Superstore/assets/96239993/03a29863-f0a6-4834-b2cc-ee05d5ac9713)
</p>


```r
#Profitability Analysis:
#Which product categories or subcategories are the most profitable?
most_profitable = a%>%
  group_by(a$Category)%>%
  summarize(Total_Profit = sum(Profit))%>%
  arrange(desc(Total_Profit))%>%
  head(1)
print("Most Profitable Product Categories:")
print(most_profitable)

most_profitable_subcat = a%>%
  group_by(a$Sub.Category)%>%
  summarize(Total_Profit = sum(Profit))%>%
  arrange(desc(Total_Profit))%>%
  head(1)
print("Most Profitable Product Subcategories:")
print(most_profitable_subcat)

# Calculate total profits for each product category and subcategory
profits_by_category <- aggregate(a$Profit, by = list(Category = a$Category), FUN = sum)
profits_by_category
profits_by_subcategory <- aggregate(a$Profit, by = list(Subcategory = a$Sub.Category), FUN = sum)
profits_by_subcategory

# Sort the results in descending order to find the most profitable categories and subcategorimean()# Sort the results in descending order to find the most profitable categories and subcategorimax()# Sort the results in descending order to find the most profitable categories and subcategories
profits_by_category <- profits_by_category[order(-profits_by_category$x), ]
profits_by_category
profits_by_subcategory <- profits_by_subcategory[order(-profits_by_subcategory$x), ]
profits_by_subcategory


# Are there any products or regions where profit margins are particularly high or low?
# Calculate profit margins for each product or region
a$Profit_Margin <- a$Profit / a$Sales
a$Profit_Margin
# Identify products or regions with particularly high or low profit margins
high_margin_products <- subset(a, Profit_Margin > 0.2)  # Adjust the threshold as needed
high_margin_products
low_margin_products <- subset(a, Profit_Margin < 0.05)  # Adjust the threshold as needed
low_margin_products
# Print products or regions with high or low profit margins
print("Products with High Profit Margins:")
print(head(high_margin_products))
print("Products with Low Profit Margins:")
print(head(low_margin_products))
```
Result : 
# <p align="center">![image](https://github.com/manojgaikwad13/R-project-on-Global-Superstore/assets/96239993/781b6a92-a883-4cf6-96b0-0bc76f7902fd)
</p>

# <p align="center">![image](https://github.com/manojgaikwad13/R-project-on-Global-Superstore/assets/96239993/bcdc96ba-c717-4270-82b5-3b64342474da)
</p>

# <p align="center">![image](https://github.com/manojgaikwad13/R-project-on-Global-Superstore/assets/96239993/3bcbfc0e-619f-4b42-8942-5e65fbc377a7)
</p>

# <p align="center">![image](https://github.com/manojgaikwad13/R-project-on-Global-Superstore/assets/96239993/d68bcaf3-be95-4406-a42a-41e0c393b7ce)
</p>

# <p align="center">![image](https://github.com/manojgaikwad13/R-project-on-Global-Superstore/assets/96239993/b605093e-4ef5-4255-8a92-f3e7b20769c4)
</p>

# <p align="center">![image](https://github.com/manojgaikwad13/R-project-on-Global-Superstore/assets/96239993/af94b487-c091-4e3a-a36d-b00df2228787)
</p>

```r
#Order Analysis:
#What is the average order size?
mean_order_size <- mean(a$Quantity)
mean_order_size

#How do order sizes vary across different product categories or regions?
# Calculate average order size by product category
order_size_by_category <- aggregate(a$Quantity, by = list(Category = a$Category), FUN = mean)
order_size_by_category

# Calculate average order size by region
order_size_by_region <- aggregate(a$Quantity, by = list(Region = a$Region), FUN = mean)
order_size_by_region

#Is there any correlation between order size and other variables like sales or profits?
correlation_matrix <- cor(a[, c("Quantity", "Sales", "Profit")])
correlation_matrix
```
Result : 
# <p align="center">![image](https://github.com/manojgaikwad13/R-project-on-Global-Superstore/assets/96239993/b8bc8725-960c-4b7c-990b-7be1f80e4073)
</p>


