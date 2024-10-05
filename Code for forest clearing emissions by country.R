###### FOREST LAND CLEARING EMISSIONS BY COUNTRY ######


#### Setting directory and importing the .csv data ####
setwd("C:/Users/ether/OneDrive - National University of Singapore/PP5531 Introduction to Coding for Public Policy using R/Group Project/Climate TRACE data/forestry_and_land_use/DATA") forest_clearing_emissions \<- read.csv('forest-land-clearing_country_emissions.csv')
forest_clearing_CO2emissions_asia <- read.csv("forest-land-clearing_country_emissions.csv")
View(forest_clearing_emissions)


#### Library the following packages ####
install.packages(dplyr)
install.packages(ggplot2)
install.packages(lubridate)
install.packages("ggthemes")

# can skip to this part if you already have the packages #
library(dplyr)
library(ggplot2)
library(lubridate)
library(ggthemes)


#### Filtering for CO2 emissions in ASEAN and select East Asian, South Asian countries ####

ASEAN <- c('THA', 'MYS', 'IDN', 'SGP', 'PHL', 'BRN', 'VNM', 'KHM', 'MMR', 'LAO')
East_Asia <- c('CHN', 'JPN', 'KOR')
South_Asia <- c('IND')

asian_countries <- c(ASEAN, East_Asia, South_Asia)

forest_clearing_CO2emissions_asia <- forest_clearing_emissions %>%
  filter(iso3_country %in% asian_countries) %>%
  filter(gas == "co2") %>%


## double checking if all the selected Asian countries are included ##
unique(forest_clearing_emissions_asia$iso3_country)
unique_count <- unique(forest_clearing_emissions_asia$iso3_country)
length(unique_count)

# we have 14 countries i.e. all countries have data here

## creating dummy variable for year for easy graph plotting ##
forest_clearing_CO2emissions_asia <- forest_clearing_CO2emissions_asia %>%
  mutate(start_time = as.Date(start_time),
         year = year(start_time)
         )

View(forest_clearing_CO2emissions_asia)

##### Visualising the data #####


# as a bar chart #
ggplot(forest_clearing_CO2emissions_asia, aes(iso3_country, emissions_quantity, fill = factor(year))) + 
  geom_col() +
  labs(x = "Asian country", y = "Total emissions", title = "Emissions from Land Clearing by Select Asian countries") +
  theme_minimal()

# as a line graph #
ggplot(forest_clearing_CO2emissions_asia, aes(year, emissions_quantity, color = iso3_country)) + 
  geom_line() +
  geom_point() +  
  labs(x = "Year", y = "Emissions Quantity", title = "Emissions from Land Clearing by Country Over Time") +
  theme_minimal()


#### EXTRA OPTION: If we want to combine ASEAN countries together (since their emissions are so little) ####

forest_clearing_CO2emissions_asia_2 <- forest_clearing_emissions %>%
  filter(iso3_country %in% asian_countries) %>%
  filter(gas == "co2") %>%
  mutate(start_time = as.Date(start_time),
         year = year(start_time)) %>%
  mutate(iso3_country = ifelse(iso3_country %in% ASEAN, "ASEAN", iso3_country)) %>%
  group_by(iso3_country, year) %>%
  summarize(emissions_quantity = sum(emissions_quantity))

View(forest_clearing_CO2emissions_asia_2)


ggplot(forest_clearing_CO2emissions_asia_2, aes(iso3_country, emissions_quantity, fill = factor(year))) + 
  geom_col() +
  labs(x = "Asian country", y = "Total emissions", title = "Emissions from Land Clearing by Select Asian countries") +
  theme_minimal()

ggplot(forest_clearing_CO2emissions_asia_2, aes(year, emissions_quantity, color = iso3_country)) + 
  geom_line() +
  geom_point() +  
  labs(x = "Year", y = "Emissions Quantity", title = "Emissions from Land Clearing by Country Over Time") +
  theme_minimal()
