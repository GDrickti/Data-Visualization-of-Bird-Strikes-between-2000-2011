install.packages("cli")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("readr")
install.packages("tidyr")
install.packages("tibble")
install.packages("purrr")


library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(tibble)
library(purrr)
library(lubridate) 

bird_strikes= read_csv("~/R/Bird Strikes data.xlsx - Bird Strikes.csv", 
                       col_types = cols(`Wildlife: Number Struck Actual` = col_number(), 
                                        `Aircraft: Number of engines?` = col_number(), 
                                        `Cost: Total $` = col_number(), `Feet above ground` = col_number(), 
                                        `Number of people injured` = col_number()))
View(bird_strikes)

# Convert the date column to a date object
bird_strikes$FlightDate <- as.Date(bird_strikes$FlightDate, format = "%m/%d/%Y")

#1.Visuals Depicting the Number of Bird Strikes:
# Visualize the number of bird strikes over the years
ggplot(bird_strikes, aes_string(x = "year(FlightDate)")) +
  geom_bar(color="black", fill="orange") +
  labs(title = "Bird Strikes Over the Years",
       x = "Year", y = "Count")
  
#2.Yearly Analysis & Bird Strikes in the US:
# Group data by year and calculate the total bird strikes
yearly_strikes <- bird_strikes %>%
  group_by(year = year(FlightDate)) %>%
  summarize(total_strikes = sum(`Wildlife: Number Struck Actual`))

# Create a line plot for yearly bird strikes
ggplot(yearly_strikes, aes(x = year, y = total_strikes)) +
  geom_line() +
  labs(title = "Yearly Bird Strikes in the US",
       x = "Year", y = "Total Strikes")

#3.Top 10 US Airlines in Terms of Bird Strikes:
# Group data by airline and calculate the total bird strikes
top_airlines <- bird_strikes %>%
  group_by(`Aircraft: Airline/Operator`) %>%
  summarize(total_strikes = sum(`Wildlife: Number Struck Actual`)) %>%
  arrange(desc(total_strikes)) %>%
  top_n(10)

# Create a bar chart for top airlines
ggplot(top_airlines, aes(x = reorder(`Aircraft: Airline/Operator`, total_strikes), y = total_strikes)) +
  geom_bar(stat = "identity", color="black", fill="darkorchid1") +
  labs(title = "Top 10 US Airlines with Bird Strikes",
       x = "Airline", y = "Total Strikes") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#4,Airports with Most Incidents of Bird Strikes (Top 50):
# Group data by airport and calculate the total bird strikes
top_airports <- bird_strikes %>%
  group_by(`Airport: Name`) %>%
  summarize(total_strikes = sum(`Wildlife: Number Struck Actual`)) %>%
  arrange(desc(total_strikes)) %>%
  top_n(50)

# Create a bar chart for top airports
ggplot(top_airports, aes(x = reorder(`Airport: Name`, total_strikes), y = total_strikes)) +
  geom_bar(stat = "identity", fill="firebrick2") +
  labs(title = "Top 50 Airports with Bird Strikes",
       x = "Airport", y = "Total Strikes") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#5.Yearly Cost Incurred Due to Bird Strikes:
# Group data by year and calculate the total cost
yearly_cost <- bird_strikes %>%
  group_by(year = year(FlightDate)) %>%
  summarize(total_cost = sum(`Cost: Total $`))

# Create a line plot for yearly cost
ggplot(yearly_cost, aes(x = year, y = total_cost)) +
  geom_line(color="hotpink3") +
  labs(title = "Yearly Cost Incurred due to Bird Strikes",
       x = "Year", y = "Total Cost")

#6.When Do Most Bird Strikes Occur?:
# Group data by phase of flight and calculate the total bird strikes
phase_strikes <- bird_strikes %>%
  group_by(`When: Phase of flight`) %>%
  summarize(total_strikes = sum(`Wildlife: Number Struck Actual`))

# Create a bar chart for flight phases
ggplot(phase_strikes, aes(x = reorder(`When: Phase of flight`, total_strikes), y = total_strikes)) +
  geom_bar(stat = "identity",fill="lime green",color="black") +
  labs(title = "Bird Strikes by Flight Phase",
       x = "Flight Phase", y = "Total Strikes") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#7.Altitude of Airplanes at the Time of Strike:
# Create a histogram for altitude
ggplot(bird_strikes, aes(x = `Feet above ground`)) +
  geom_histogram(binwidth = 1000, fill = "skyblue", color = "black") +
  labs(title = "Altitude Distribution During Bird Strikes",
       x = "Altitude (Feet)", y = "Count")

#8.Phase of Flight at the Time of Strike:
# Group data by flight phase and calculate the total bird strikes
phase_strikes <- bird_strikes %>%
  group_by(`When: Phase of flight`) %>%
  summarize(total_strikes = sum(`Wildlife: Number Struck Actual`))

# Create a bar chart for flight phases
ggplot(phase_strikes, aes(x = reorder(`When: Phase of flight`, total_strikes), y = total_strikes)) +
  geom_bar(stat = "identity", color="black", fill="plum1") +
  labs(title = "Bird Strikes by Flight Phase",
       x = "Flight Phase", y = "Total Strikes") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#9.Average Altitude in Different Flight Phases at the Time of Strike:
# Calculate average altitude by flight phase
avg_altitude <- bird_strikes %>%
  group_by(`When: Phase of flight`) %>%
  summarize(avg_altitude = mean(`Feet above ground`))

# Create a bar chart for average altitudes
ggplot(avg_altitude, aes(x = reorder(`When: Phase of flight`, avg_altitude), y = avg_altitude)) +
  geom_bar(stat = "identity",fill="light blue", color="black") +
  labs(title = "Average Altitude by Flight Phase",
       x = "Flight Phase", y = "Average Altitude (Feet)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#10.Effect of Bird Strikes & Impact on Flight:
# Group data by impact and calculate the total bird strikes
impact_strikes <- bird_strikes %>%
  group_by(`Effect: Impact to flight`) %>%
  summarize(total_strikes = sum(`Wildlife: Number Struck Actual`))

# Create a pie chart for impact types
ggplot(impact_strikes, aes(x = "", y = total_strikes, fill = `Effect: Impact to flight`)) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y") +
  labs(title = "Impact of Bird Strikes on Flight",
       fill = "Impact Type") +
  theme_void()

#11.Effect of Strike at Different Altitude:
# Create a scatter plot for altitude vs. impact severity
ggplot(bird_strikes, aes(x = `Feet above ground`, y = `Effect: Impact to flight`)) +
  geom_point(alpha = 0.5) +
  labs(title = "Effect of Strike at Different Altitude",
       x = "Altitude (Feet)", y = "Impact Severity")

#12.Were Pilots Informed? & Prior Warning and Effect of Strike:
# Group data by pilot warning and calculate the total bird strikes
pilot_warnings <- bird_strikes %>%
  group_by(`Pilot warned of birds or wildlife?`) %>%
  summarize(total_strikes = sum(`Wildlife: Number Struck Actual`))

# Create a bar chart for pilot warnings
ggplot(pilot_warnings, aes(x = `Pilot warned of birds or wildlife?`, y = total_strikes)) +
  geom_bar(stat = "identity", fill = "light green", color = "black") +
  labs(title = "Bird Strikes by Pilot Warnings",
       x = "Pilot Warning", y = "Total Strikes")
