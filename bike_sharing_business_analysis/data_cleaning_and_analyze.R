# DATA CLEANING

# Importing libraries and csv files
library(tidyverse)
library(lubridate)

trips_data <- list.files(path='C:/Users/sams/Documents/R/Bycicles Project/Data') %>% 
  lapply(read_csv, show_col_types = FALSE) %>% 
  bind_rows 

#Inspect dataframe
colnames(trips_data)
str(trips_data)
head(trips_data)
summary(trips_data)

#Count unique and missing values
sapply(trips_data, function(x) n_distinct(x))
sapply(trips_data, function(x) sum(is.na(x)))

#Remove unnecessary columns
trips_data_v2 <- trips_data %>% 
  select(rideable_type, started_at, ended_at, start_station_name, 
         end_station_name, member_casual)

#Rename "rideable_type" and "member_casual" columns
trips_data_v2 <- trips_data_v2 %>% 
  rename(bike_type = rideable_type, customer_type = member_casual)

#Change "docked_bike" to "classic_bike"
trips_data_v2$bike_type[trips_data_v2$bike_type == "docked_bike"] <- "classic_bike"
unique(trips_data_v2$bike_type)

#Fill "start_station_name" and "end_station_name" missing values with "unknown"
trips_data_v2[is.na(trips_data_v2)] <- "unknown"


#Create "trip_length", "date", "day", "week_day", "month", and "year" columns
trips_data_v2 <- trips_data_v2 %>% 
  mutate(trip_length = ended_at - started_at, 
         date = as.Date(started_at),
         day = format(as.Date(started_at), "%d"), 
         week_day = wday(started_at),
         month = format(as.Date(started_at), "%m"),
         year = format(as.Date(started_at),"%Y"))

#Convert trip_length to numeric and change unit of measurement to minutes
trips_data_v2$trip_length <- round(as.numeric(trips_data_v2$trip_length)/60, 1)

#Drop rows with trip_length less than 1
trips_data_v2 <- trips_data_v2[trips_data_v2$trip_length >= 1,]



#DATA ANALYZE

#Trip length descriptive analysis
summary(trips_data$trip_length)

#Remove outliers
upper_bound = median(trips_data$trip_length) + 1.5 * IQR(trips_data$trip_length)
outliers =  trips_data %>% filter(trip_length > upper_bound)
trips_data = trips_data %>% filter(trip_length <= upper_bound)

#Plot trip length histogram by customer type
trips_data_v2 %>% ggplot(aes(x=trip_length, fill=customer_type)) + 
  geom_histogram(binwidth = 6, show.legend=FALSE) +
  labs(title="Trip Duration Distribution", x="trip duration (in minutes)",
       y="number of trips", fill="Customer Type") +
  facet_wrap(~customer_type) +
  scale_x_continuous(breaks = seq(0,30, by=5))


#Compare casual users and members trip length summary statistics
stats_data <- trips_data_v2 %>% group_by(customer_type) %>%
  summarise(min = min(trip_length), median = median(trip_length),
            mean = mean(trip_length), max = max(trip_length))
stats_data

#Get the average trip time and number of trips by week day 
week_data <- trips_data_v2 %>% 
  group_by(week_day, customer_type) %>% 
  summarise(number_of_trips = n(), average_duration = mean(trip_length))
week_data

#Get the average trip time and number of trips by date
daily_data <- trips_data_v2 %>% 
  group_by(date, customer_type) %>% 
  summarise(number_of_trips = n(), average_duration = mean(trip_length))
daily_data

#Get top 5 start and end stations for casual users 
top_start_stations <- trips_data_v2 %>% filter(customer_type == "casual") %>% 
  filter(start_station_name != "unknown") %>% 
  group_by(start_station_name) %>% 
  summarise(number_of_visits = n()) %>% 
  arrange(-number_of_visits) %>% 
  head(5)
top_start_stations

top_end_stations <- trips_data_v2 %>% filter(customer_type == "casual") %>% 
  filter(end_station_name != "unknown") %>% 
  group_by(end_station_name) %>% 
  summarise(number_of_visits = n()) %>% 
  arrange(-number_of_visits) %>% 
  head(5)
top_end_stations

#Plot average trip time by customer type
stats_data %>% ggplot(aes(x=customer_type, y=mean, fill=customer_type)) +
  geom_col(position="dodge", show.legend=FALSE) +
  labs(title="Average Trip Duration by Customer Type",
       x=NULL,y="average trip duration (minutes)")


#Plot average trip time by customer type and date
daily_data %>% ggplot(aes(x = date, y = average_duration, color = customer_type)) +
  geom_point() +
  labs(title = "Average Trip Duration by Date and Customer Type", 
       x = NULL, y = "average trip duration (minutes)", fill = "Customer type") +
  ylim(0, 15)

#Plot number of trips by customer type and date
daily_data %>% ggplot(aes(x=date, y=number_of_trips, color=customer_type)) +
  geom_point() +
  labs(title="Number of Trips by Date and Customer Type", 
       x=NULL, y="number of trips", fill="Customer type") 

#Plot average trip time by customer type and week day
week_data %>% ggplot(aes(x=week_day, y=average_duration, fill=customer_type)) +
  geom_col(position="dodge") +
  labs(title="Average Trip Duration by Week Day and Customer Type", 
       x=NULL, y="average trip duration (minutes)", fill="Customer type") +
  scale_x_discrete(limits = c("Sunday", "Monday", "Tuesday", "Wednesday",
                              "Thursday", "Friday", "Saturday"))

#Plot number of trips by customer type and week day
plot_data %>% ggplot(aes(x = week_day, y = number_of_trips/1000, fill = customer_type)) +
  geom_col(position = "dodge") +
  labs(title = "Number of Trips by Week Day Customer Type", 
       x = NULL, y = "number of trips", fill = "Customer type") +
  scale_x_discrete(limits = c("Sunday", "Monday", "Tuesday", "Wednesday",
                              "Thursday", "Friday", "Saturday")) +
  scale_y_continuous(label = function(x) paste0(x,'K'))

#Plot top 5 start stations for casual users
ggplot(top_start_stations, aes(x=number_of_visits, 
                               y=forcats::fct_reorder(start_station_name, number_of_visits))) + 
  geom_col(aes(fill=start_station_name), show.legend=FALSE) +
  labs(title ="Start Stations Most Visted by Casual Users",
       x="number of visits", y="station name")

#Plot top 5 start stations for casual users
ggplot(top_end_stations, aes(x=number_of_visits, 
                             y=forcats::fct_reorder(end_station_name, number_of_visits))) + 
  geom_col(aes(fill=end_station_name), show.legend=FALSE) +
  labs(title ="End Stations Most Visited by Casual Users",
       x="number of visits", y="station name")

