q1_2020 <- read_csv("Divvy_Trips_2020_Q1.csv")
may_2020 <- read_csv("202005-divvy-tripdata.csv")
june_2020 <- read_csv("202006-divvy-tripdata.csv")
july_2020<- read_csv("202007-divvy-tripdata.csv")
aug_2020<- read_csv("202008-divvy-tripdata.csv")
sep_2020<- read_csv("202009-divvy-tripdata.csv")
oct_2020<- read_csv("202010-divvy-tripdata.csv")
nov_2020 <- read_csv("202011-divvy-tripdata.csv")
dec_2020<- read_csv("202012-divvy-tripdata.csv")

## Checking consistency in each data frame
colnames(may_2020)
colnames(june_2020)
colnames(july_2020)
colnames(aug_2020)
colnames(sep_2020)
colnames(oct_2020)
colnames(nov_2020)
colnames(dec_2020)

## Inspecting Data Frames
str(may_2020)
str(june_2020)
str(july_2020)
str(aug_2020)
str(sep_2020)
str(oct_2020)
str(nov_2020)
str(dec_2020)

## Format July col_character() into col_datetime()
july_2020 <- mutate(july_2020, started_at =as.POSIXct(format(strptime(started_at, "%d/%m/%Y %H:%M"), "%Y-%m-%d %H:%M")))
july_2020 <- mutate(july_2020, ended_at =as.POSIXct(format(strptime(ended_at, "%d/%m/%Y %H:%M"), "%Y-%m-%d %H:%M")))
str(july_2020)

## stack individual month data frame into one big data frame
all_trips <- bind_rows(q1_2020,may_2020,june_2020,july_2020,aug_2020,sep_2020,oct_2020,nov_2020)

## Remove the lat, lng, lat, lng
all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng))
## check for consistent name conventions
table(all_trips$member_casual)

#add columns that list the date,month, day and year of each ride
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

# add 'ride_length' calculation to all trips 
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

#inspect all trips
str(all_trips)

# Convert "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)
mean(all_trips$ride_length) #straight average (total ride length / rides)
median(all_trips$ride_length) #midpoint number in the ascending array of ride lengths
max(all_trips$ride_length) #longest ride
min(all_trips$ride_length) #shortest ride

#because the Data contains negative calculations for ride_length (maintenance time)
# A new data frame consisting HQ QR or ride length is less than 0
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]
# Remove non existent ride lengths
all_trips_v2 <- all_trips_v2[!is.na(all_trips_v2$ride_length),]

is.factor(all_trips_v2$ride_length)
is.numeric(all_trips_v2$ride_length)

# Descriptive analysis on ride_length (all figures in seconds)
mean(all_trips_v2$ride_length) #straight average (total ride length / rides)
median(all_trips_v2$ride_length) #midpoint number in the ascending array of ride lengths
max(all_trips_v2$ride_length) #longest ride
min(all_trips_v2$ride_length) #shortest ride
# OR you can write like this
summary(all_trips_v2$ride_length)

# Compare members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

# See the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# Notice that the days of the week are out of order. Let's fix that.
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Now, let's run the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# analyze ridership data by type and weekday
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)								# sorts

# Let's visualize the number of rides by rider type
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

write.csv(all_trips_v2, "D:\\R\\Data\\R Script\\Trips_Jan_Nov.csv", row.names = FALSE)

write.csv(all_trips_v2, "D:\\R\\Data\\R Script\\Trips_Jan_Nov_v1.csv", row.names = TRUE)
