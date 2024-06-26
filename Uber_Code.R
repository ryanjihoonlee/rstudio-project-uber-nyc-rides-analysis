
library(ggplot2)
library(ggthemes)
library(lubridate)
library(dplyr)
library(tidyr)
library(DT)
library(scales)

colors <- c("#09091A", "#4A4A4A", "#A8A8A8", "#0090FF", "#2AB27B", "#FFC90E", "#8E44AD")

## Data Import

apr_data <- read.csv("/Users/ryanlee/Desktop/R/R Projects/Uber/dataset/uber-raw-data-apr14.csv")
may_data <- read.csv("/Users/ryanlee/Desktop/R/R Projects/Uber/dataset/uber-raw-data-may14.csv")
jun_data <- read.csv("/Users/ryanlee/Desktop/R/R Projects/Uber/dataset/uber-raw-data-jun14.csv")
jul_data <- read.csv("/Users/ryanlee/Desktop/R/R Projects/Uber/dataset/uber-raw-data-jul14.csv")
aug_data <- read.csv("/Users/ryanlee/Desktop/R/R Projects/Uber/dataset/uber-raw-data-aug14.csv")
sep_data <- read.csv("/Users/ryanlee/Desktop/R/R Projects/Uber/dataset/uber-raw-data-sep14.csv")

data <- rbind(apr_data, may_data, jun_data, jul_data, aug_data, sep_data)

data$Date.Time <- as.POSIXct(data$Date.Time, format = "%m/%d/%Y %H:%M:%S")

data$Time <- format(as.POSIXct(data$Date.Time, format = "%m/%d/%Y %H:%M:%S"), format = "%H:%M:%S")

data$Date.Time <- ymd_hms(data$Date.Time)

data$day <- factor(day(data$Date.Time))
data$month <- factor(month(data$Date.Time, label = TRUE))
data$year <- factor(year(data$Date.Time))
data$dayofweek <- factor(wday(data$Date.Time, label = TRUE))
data$hour <- factor(hour(hms(data$Time)))
data$minute <- factor(minute(hms(data$Time)))
data$second <- factor(second(hms(data$Time)))

## Hourly Distribution and Month Trends of Uber Trips

hour_data <- data |> 
  filter(!is.na(hour)) |> 
  group_by(hour) |> 
  summarize(Total = n())

datatable(hour_data)

ggplot(hour_data, aes(hour, Total)) +
  geom_bar(stat = "identity", fill = "black", color = "blue") +
  labs(title = "Uber Trips Every Hour", x = "Hour", y = "Total Trips") +
  theme(element_text(face = "bold"), legend.position = "none") +
  scale_y_continuous(labels = comma)

month_hour <- data |> 
  filter(!is.na(month) & !is.na(hour)) |> 
  group_by(month, hour) |> 
  summarize(Total = n())

ggplot(month_hour, aes(hour, Total, fill = month)) +
  geom_bar(stat = "identity") +
  labs(title = "Uber Trips by Hour and Month", x = "Hour", y = "Total Trips") +
  theme(element_text(face = "bold")) +
  scale_y_continuous(labels = comma)

## Daily Distribution and Monthly Trends of Uber Trips

day_group <- data |> 
  filter(!is.na(day)) |> 
  group_by(day) |> 
  summarize(Total = n()) 

datatable(day_group)

ggplot(day_group, aes(day, Total)) +
  geom_bar(stat = "identity", fill = "black") + 
  labs(title = "Uber Trips Every Day", x = "Day", y = "Total Trips") +
  theme(element_text(face = "bold"), legend.position = "none") +
  scale_y_continuous(labels = comma)

day_month_group <- data |> 
  filter(!is.na(month) & !is.na(day)) |> 
  group_by(month, day) |> 
  summarize(Total = n())

ggplot(day_month_group, aes(day, Total, fill = month)) + 
  geom_bar(stat = "identity") +
  labs(title = "Uber Trips by Day and Month", x = "Day", y = "Total Trips") +
  theme(element_text(face = "bold")) +
  scale_y_continuous(labels = comma) + 
  scale_fill_manual(values = colors)

## Monthly Distribution and Annual Trends of Uber Trips

month_group <- data |>
  filter(!is.na(month)) |>
  group_by(month) |> 
  summarize(Total = n())

datatable(month_group)

ggplot(month_group, aes(month, Total, fill = month)) +
  geom_bar(stat = "identity") +
  labs(title = "Uber Trips by Month", x = "Month", y = "Total Trips") + 
  theme(element_text(face = "bold"), legend.position = "none") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colors)

month_weekday <- data |>
  filter(!is.na(month) & !is.na(dayofweek)) |>
  group_by(month, dayofweek) |> 
  summarize(Total = n())

ggplot(month_weekday, aes(month, Total, fill = dayofweek)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Uber Trips by Day and Month", x = "Month", y = "Total Trips") +
  theme(element_text(face = "bold")) +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colors)

## Trip Counts by Bases

ggplot(data, aes(Base)) +
  geom_bar(fill = "black") +
  labs(title = "Uber Trips by Bases", x = "Base") +
  theme(element_text(face = "bold")) +
  scale_y_continuous(labels = comma)

ggplot(data, aes(Base, fill = month)) +
  geom_bar(position = "dodge") +
  labs(title = "Uber Trips by Bases and Month", x = "Base") +
  theme(element_text(face = "bold")) +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colors)

ggplot(data, aes(Base, fill = dayofweek)) +
  geom_bar(position = "dodge") +
  labs(title = "Uber Trips by Bases and Day of Week", x = "Base") +
  theme(element_text(face = "bold")) +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colors)

## Heatmap of Day, Hour, and Month Trends

day_and_hour <- data |>
  filter(!is.na(day) & !is.na(hour)) |>
  group_by(day, hour) |> 
  summarize(Total = n())

datatable(day_and_hour)

ggplot(day_and_hour, aes(day, hour, fill = Total)) +
  geom_tile(color = "white") +
  labs(title = "Heat Map by Hour and Day", x = "Day", y = "Hour") +
  theme(element_text(face = "bold"))

ggplot(day_month_group, aes(day, month, fill = Total)) +
  geom_tile(color = "white") +
  labs(title = "Heat Map by Month and Day", x = "Day", y = "Month") +
  theme(element_text(face = "bold"))

ggplot(month_weekday, aes(dayofweek, month, fill = Total)) +
  geom_tile(color = "white") +
  labs(title = "Heat Map by Month and Day of Week", x = "Day of Week", y = "Month") +
  theme(element_text(face = "bold"))

month_base <- data |>
  filter(!is.na(Base) & !is.na(month)) |>
  group_by(Base, month) |> 
  summarize(Total = n())

dayofweek_bases <- data |> 
  filter(!is.na(Base) & !is.na(dayofweek)) |> 
  group_by(Base, dayofweek) |> 
  summarize(Total = n())

ggplot(month_base, aes(Base, month, fill = Total)) +
  geom_tile(color = "white") +
  labs(title = "Heat Map by Month and Bases", y = "Month") +
  theme(element_text(face = "bold"))

ggplot(dayofweek_bases, aes(Base, dayofweek, fill = Total)) +
  geom_tile(color = "white") +
  labs(title = "Heat Map by Bases and Day of Week", y = "Day of Week") +
  theme(element_text(face = "bold"))

## Map Visualizations of Rides in New York City

min_lat <- 40.5774
max_lat <- 40.9176
min_long <- -74.15
max_long <- -73.7004

ggplot(data, aes(x = Lon, y = Lat)) +
  geom_point(size = 1, color = "blue") +
  labs(title = "NYC Map Based on Uber Rides During 2014 (Apr-Sep)") +
  theme(element_text(face = "bold")) +
  scale_x_continuous(limits = c(min_long, max_long)) +
  scale_y_continuous(limits = c(min_lat, max_lat)) +
  theme_map()

ggplot(data_2014, aes(x = Lon, y = Lat, color = Base)) + 
  geom_point(size = 1) +
  labs(title = "NYC Map Based on Uber Rides During 2014 (Apr-Sep) by Base") +
  theme(element_text(face = "bold")) +
  scale_x_continuous(limits = c(min_long, max_long)) +
  scale_y_continuous(limits = c(min_lat, max_lat)) +
  theme_map()
