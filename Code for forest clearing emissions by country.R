###### FOREST LAND CLEARING EMISSIONS BY COUNTRY ######


#### Setting directory and importing the .csv data ####
setwd("C:/Users/ether/OneDrive - National University of Singapore/PP5531 Introduction to Coding for Public Policy using R/Group Project/Climate TRACE data/forestry_and_land_use/DATA") 
forest_clearing_emissions <- read.csv('forest-land-clearing_country_emissions.csv')
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
  filter(gas == "co2")


## double checking if all the selected Asian countries are included ##
unique_count <- unique(forest_clearing_CO2emissions_asia$iso3_country)
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


#### If we want to combine ASEAN countries together (since their emissions are so little) ####

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





###### SOURCES OF FOREST FIRE EMISSIONS BY COUNTRY ######


#### Setting directory and importing the .csv data ####

#setwd("C:/Users/ether/OneDrive - National University of Singapore/PP5531 Introduction to Coding for Public Policy using R/Group Project/Climate TRACE data/forestry_and_land_use/DATA") 
forest_fire_emissions <- read.csv('forest-land-fires_country_emissions.csv')
View(forest_fire_emissions)

#### Library the following packages ####
***use ctrl + +shift + c to comment/uncomment codes***
# install.packages(dplyr)
# install.packages(ggplot2)
# install.packages(lubridate)
# install.packages("ggthemes")

# can skip to this part if you already have the packages #
# library(dplyr)
# library(ggplot2)
# library(lubridate)
# library(ggthemes)


#### Filtering for CO2 emissions in ASEAN and select East Asian, South Asian countries ####

# ASEAN <- c('THA', 'MYS', 'IDN', 'SGP', 'PHL', 'BRN', 'VNM', 'KHM', 'MMR', 'LAO')
# East_Asia <- c('CHN', 'JPN', 'KOR')
# South_Asia <- c('IND')

# asian_countries <- c(ASEAN, East_Asia, South_Asia)

forest_fire_emissions_asia <- forest_fire_emissions %>%
  filter(iso3_country %in% asian_countries) %>%
  filter(gas == "co2")


## double checking if all the selected Asian countries are included ##
unique_count <- unique(forest_fire_emissions_asia$iso3_country)
length(unique_count)

# Again, we have 14 countries i.e. all countries have data here

## creating dummy variable for year for easy graph plotting ##
forest_fire_emissions_asia <- forest_fire_emissions_asia %>%
  mutate(start_time = as.Date(start_time),
         year = year(start_time)
  )

View(forest_degradation_emissions_asia)


##### Visualising the data #####

# as a bar chart #
ggplot(forest_fire_emissions_asia, aes(iso3_country, emissions_quantity, fill = factor(year))) + 
  geom_col() +
  labs(x = "Asian country", y = "Total emissions", title = "Emissions from Land Degradation by Asian Countries") +
  theme_minimal()

# as a line graph #
ggplot(forest_fire_emissions_asia, aes(year, emissions_quantity, color = iso3_country)) + 
  geom_line() +
  geom_point() +  
  labs(x = "Year", y = "Emissions Quantity", title = "Emissions from Land Degradation by Asian Countries Over Time") +
  theme_minimal()





###### NET FOREST EMISSIONS BY COUNTRY ######


#### Setting directory and importing the .csv data ####

#setwd("C:/Users/ether/OneDrive - National University of Singapore/PP5531 Introduction to Coding for Public Policy using R/Group Project/Climate TRACE data/forestry_and_land_use/DATA") 
net_forest_emissions <- read.csv('net-forest-land_country_emissions.csv')
View(net_forest_emissions)

#### Library the following packages ####
***use ctrl + +shift + c to comment/uncomment codes***
# install.packages(dplyr)
# install.packages(ggplot2)
# install.packages(lubridate)
# install.packages("ggthemes")

# can skip to this part if you already have the packages #
# library(dplyr)
# library(ggplot2)
# library(lubridate)
# library(ggthemes)


#### Filtering for CO2 emissions in ASEAN and select East Asian, South Asian countries ####

# ASEAN <- c('THA', 'MYS', 'IDN', 'SGP', 'PHL', 'BRN', 'VNM', 'KHM', 'MMR', 'LAO')
# East_Asia <- c('CHN', 'JPN', 'KOR')
# South_Asia <- c('IND')

# asian_countries <- c(ASEAN, East_Asia, South_Asia)

net_forest_emissions_asia <- net_forest_emissions %>%
  filter(iso3_country %in% asian_countries) %>%
  filter(gas == "co2")


## double checking if all the selected Asian countries are included ##
unique_count <- unique(forest_fire_emissions_asia$iso3_country)
length(unique_count)

# Again, we have 14 countries i.e. all countries have data here

## creating dummy variable for year for easy graph plotting ##
net_forest_emissions_asia <- net_forest_emissions_asia %>%
  mutate(start_time = as.Date(start_time),
         year = year(start_time)
  )

View(net_forest_emissions_asia)


##### Visualising the data #####
## extra graphs included due to drastic differences in emissions quantity for this subsector ##

# as a bar chart #
ggplot(net_forest_emissions_asia, aes(iso3_country, emissions_quantity, fill = factor(year))) + 
  geom_col() +
  labs(x = "Asian country", y = "Total emissions", title = "Net Forest Emissions by Asian Countries") +
  theme_gray()

# as a line graph #
ggplot(net_forest_emissions_asia, aes(year, emissions_quantity, color = iso3_country)) + 
  geom_line() +
  geom_point() +  
  labs(x = "Year", y = "Emissions Quantity", title = "Net Forest Emissions by Asian Countries Over Time") +
  theme_minimal()

### only for China, Thailand and Myanmar (biggest emissions change observed) ###
net_forest_emissions %>%
  filter(iso3_country %in% c('CHN', 'MMR', 'THA')) %>%
  filter(gas == "co2") %>%
  mutate(start_time = as.Date(start_time),
         year = year(start_time)) %>%
  ggplot(., aes(year, emissions_quantity, color = iso3_country)) + 
  geom_line() +
  geom_point() +  
  labs(x = "Year", y = "Emissions Quantity", title = "Net Forest Emissions by Asian Countries Over Time") +
  theme_minimal()

### for all other countries except BRN and SGP (small emissions change observed) ###
net_forest_emissions %>%
  filter(iso3_country %in% c('MYS', 'PHL','VNM','JPN', 'KOR')) %>%
  filter(gas == "co2") %>%
  mutate(start_time = as.Date(start_time),
         year = year(start_time)) %>%
  ggplot(., aes(year, emissions_quantity, color = iso3_country)) + 
  geom_line() +
  geom_point() +  
  labs(x = "Year", y = "Emissions Quantity", title = "Net Forest Emissions by Asian Countries Over Time") +
  theme_minimal()

### only for BRN and SGP (small emissions change observed) ###
net_forest_emissions %>%
  filter(iso3_country %in% c('SGP', 'BRN')) %>%
  filter(gas == "co2") %>%
  mutate(start_time = as.Date(start_time),
         year = year(start_time)) %>%
  ggplot(., aes(year, emissions_quantity, color = iso3_country)) + 
  geom_line() +
  geom_point() +  
  labs(x = "Year", y = "Emissions Quantity", title = "Net Forest Emissions by Asian Countries Over Time") +
  theme_minimal()

### China, Thailand and Myanmar have had copious fluctuations in net forest emissions in recent years.
### Singapore has had almost negligible net forest emissions change in recent years (unsurprising given that it is almost fully urbanised)






###### SHRUBGRASS FIRE EMISSIONS BY COUNTRY ######


#### Setting directory and importing the .csv data ####

#setwd("C:/Users/ether/OneDrive - National University of Singapore/PP5531 Introduction to Coding for Public Policy using R/Group Project/Climate TRACE data/forestry_and_land_use/DATA") 
shrub_fire_emissions <- read.csv('shrubgrass-fires_country_emissions.csv')
View(shrub_fire_emissions)

#### Library the following packages ####
***use ctrl + +shift + c to comment/uncomment codes***
# install.packages(dplyr)
# install.packages(ggplot2)
# install.packages(lubridate)
# install.packages("ggthemes")

# can skip to this part if you already have the packages #
# library(dplyr)
# library(ggplot2)
# library(lubridate)
# library(ggthemes)


#### Filtering for CO2 emissions in ASEAN and select East Asian, South Asian countries ####

# ASEAN <- c('THA', 'MYS', 'IDN', 'SGP', 'PHL', 'BRN', 'VNM', 'KHM', 'MMR', 'LAO')
# East_Asia <- c('CHN', 'JPN', 'KOR')
# South_Asia <- c('IND')

# asian_countries <- c(ASEAN, East_Asia, South_Asia)

shrub_fire_emissions_asia <- shrub_fire_emissions %>%
  filter(iso3_country %in% asian_countries) %>%
  filter(gas == "co2")


## double checking if all the selected Asian countries are included ##
unique_count <- unique(shrub_fire_emissions_asia$iso3_country)
length(unique_count)

# Again, we have 14 countries i.e. all countries have data here

## creating dummy variable for year for easy graph plotting ##
shrub_fire_emissions_asia <- shrub_fire_emissions_asia %>%
  mutate(start_time = as.Date(start_time),
         year = year(start_time)
  )

View(shrub_degradation_emissions_asia)


##### Visualising the data #####

# as a bar chart #
ggplot(shrub_fire_emissions_asia, aes(iso3_country, emissions_quantity, fill = factor(year))) + 
  geom_col() +
  labs(x = "Asian country", y = "Total emissions", title = "Emissions from Shrubgrass Fire by Asian Countries") +
  theme_minimal()

# as a line graph #
ggplot(shrub_fire_emissions_asia, aes(year, emissions_quantity, color = iso3_country)) + 
  geom_line() +
  geom_point() +  
  labs(x = "Year", y = "Emissions Quantity", title = "Emissions from Shrubgrass Fire by Asian Countries Over Time") +
  theme_minimal()

