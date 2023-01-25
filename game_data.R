#import datasets and load libraries

library("readxl")
library("tidyverse")
library("ggthemes")

sales_data <- read_csv('vgsales.csv')

# Questions to answer
#
# Guiding research question: What trends are observable when considering the decisions consumers make regarding video game purchases 
#
# 1) What publisher is the all time favorite? Compare total global sales of all publishers on the list. Bar chart? 
# 2) What are the most popular platforms overall? What are the most popular platforms by decade? Also bar chart?
# 3) What are the most popular games by total sales/year
# 4) Do the top selling games differ by region? Top 10 NA games vs Top 10 EU games
# 5) What are the most popular genres by year? (its always action)


### NUMBER ONE ###
favorite_publisher <- sales_data %>%
  group_by(Publisher) %>%
  summarise(Total_Sales = sum(Global_Sales)) %>%
  top_n(20) %>%
  arrange(desc(Total_Sales))

ggplot(data = favorite_publisher, aes(x = reorder(Publisher, -Total_Sales), y = Total_Sales)) +
  geom_col() +
  theme_economist() +
  labs(title = "Top 20 Publishers Based on Most Copies Sold Across All Games", x = "Platform", y = "Total Copies Sold (millions)") +
  theme(axis.title.y = element_text(margin = margin(0,20,0,0))) +
  theme(axis.title.x = element_text(margin = margin(20,0,0,0))) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

### NUMBER TWO ###
popular_platforms <- sales_data %>%
  group_by(Platform) %>%
  summarise(Total_Sales = sum(Global_Sales)) %>%
  top_n(20) %>%
  arrange(desc(Total_Sales))

ggplot(data = popular_platforms, aes(x = reorder(Platform, -Total_Sales), y = Total_Sales)) +
  geom_col() +
  theme_economist() +
  labs(title = " 20 Most Popular Platforms Based on Total Sales for All Games on that Platform", x = "Platform", y = "Total Copies Sold (millions)") +
  theme(axis.title.y = element_text(margin = margin(0,20,0,0))) +
  theme(axis.title.x = element_text(margin = margin(20,0,0,0))) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

### NUMBER THREE ###
sales_per_year <- sales_data %>%
  group_by(Name) %>%
  summarise(Sales_Per_Year = sum(Global_Sales/(2021-as.numeric(Year)))) %>%
  top_n(10) %>%
  arrange(desc(Sales_Per_Year))

ggplot(data = sales_per_year, aes(x = reorder(Name, -Sales_Per_Year), y = Sales_Per_Year)) +
  geom_col() +
  theme_economist() +
  labs(title = "Top 10 Best Selling Games by Average Sales per Year Since Release", x = "Game Title", y = "Average Copies Sold per Year (millions)") +
  theme(axis.title.y = element_text(margin = margin(0,20,0,0))) +
  theme(axis.title.x = element_text(margin = margin(20,0,0,0))) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

### NUMBER FOUR ### 
top_na_games <- sales_data %>%
  group_by(Name) %>%
  summarise(NA_Total = sum(NA_Sales)) %>%
  top_n(10) %>%
  arrange(desc(NA_Total))
          
top_eu_games <- sales_data %>%
  group_by(Name) %>%
  summarise(EU_Total = sum(EU_Sales)) %>%
  top_n(10) %>%
  arrange(desc(EU_Total))

ggplot(data = top_na_games, aes(x = reorder(Name, -NA_Total), y = NA_Total,)) +
  geom_col() +
  theme_economist() +
  labs(title = "Top 10 Best Selling Games of All Time in North America", x = "Game Title", y = "Total Copies Sold in North America (millions)") +
  theme(axis.title.y = element_text(margin = margin(0,20,0,0))) +
  theme(axis.title.x = element_text(margin = margin(20,0,0,0))) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggplot(data = top_eu_games, aes(x = reorder(Name, -EU_Total), y = EU_Total,)) +
  geom_col() +
  theme_economist() +
  labs(title = "Top 10 Best Selling Games of All Time in the EU", x = "Game Title", y = "Total Copies Sold in EU (millions)") +
  theme(axis.title.y = element_text(margin = margin(0,20,0,0))) +
  theme(axis.title.x = element_text(margin = margin(20,0,0,0))) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
### NUMBER FIVE ### 

general_sale_trend <- sales_data %>%
  group_by(Year, Genre) %>%
  filter(Year <= 2016) %>%
  summarise(Unique_Games_Released = n_distinct(Name))

ggplot(data = general_sale_trend, aes(x = Year, y = Unique_Games_Released, fill = Genre)) + 
  geom_col() +
  theme_economist() +
  labs(title = "Unique Games Published Each Year by Genre", x = 'Year', y = 'Number of Unique Games Released') +
  theme(axis.title.y = element_text(margin = margin(0,20,0,0))) +
  theme(axis.title.x = element_text(margin = margin(20,0,0,0))) +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))

### Experimental Line Graph ###

line_graph_data <- sales_data %>%
  group_by(Publisher, Year) %>%
  summarise(Yearly_Sales = sum(Global_Sales)) %>%
  filter(Publisher == "Nintendo"|Publisher == "Electronic Arts"|Publisher == "Activision"|Publisher == "Sony Computer Entertainment"|Publisher == "Ubisoft"|Publisher == "Take-Two Interactive"|Publisher == "THQ"|Publisher == "Konami Digital Entertainment"|Publisher == "Sega"|Publisher == "Namco Bandai Games"|Publisher == "Microsoft Game Studios"|Publisher == "Capcom") %>%
  filter(Year <= 2016)

ggplot(data = line_graph_data, aes(x = Year, y = Yearly_Sales, color = Publisher)) +
  geom_line(aes(group=Publisher)) +
  geom_point() +
  theme_economist() +
  labs(title = "Yearly Copies Sold by Top 12 Publishers of All Time", x = "Year", y = "Total Copies Sold (millions)") +
  theme(axis.title.y = element_text(margin = margin(0,20,0,0))) +
  theme(axis.title.x = element_text(margin = margin(20,0,0,0))) +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))

### Pie Chart ###

pie_chart <- sales_data %>%
  group_by(Genre) %>%
  summarise(Total = sum(Global_Sales))

ggplot(data = pie_chart, aes(x = "", y = Total, fill = Genre)) +
  geom_bar(stat="identity", width=1, color = "white") +
  coord_polar("y", start=0) +
  theme_void() +
  labs(title = "Most Popular Genres by Total Copies Sold")
  