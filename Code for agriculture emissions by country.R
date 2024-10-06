###### RICE CULTIVATION EMISSIONS (CH4) BY COUNTRY ######
## this dataset contains mostly CH4, CO2 equivalent over 20 years, or CO2 equivalent over 100 years
## So I have opted to analyse CH4 emissions first, due to the potency of methane in trapping heat in the atmosphere. ##

# if you need to clear your environment #
rm(list = ls())

#### Setting directory and importing the .csv data ####
setwd("C:/Users/ether/OneDrive - National University of Singapore/PP5531 Introduction to Coding for Public Policy using R/Group Project/Climate TRACE data/agriculture/DATA") 
rice_farming_emissions <- read.csv('rice-cultivation_country_emissions.csv')
View(rice_farming_emissions)


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

rice_farming_CH4emissions_asia <- rice_farming_emissions %>%
  filter(iso3_country %in% asian_countries) %>%
  filter(gas == "ch4")


## double checking if all the selected Asian countries are included ##
unique_count <- unique(rice_farming_CH4emissions_asia$iso3_country)
length(unique_count)

# we have 14 countries i.e. all countries have data here

## creating dummy variable for year for easy graph plotting ##
rice_farming_CH4emissions_asia <- rice_farming_CH4emissions_asia %>%
  mutate(start_time = as.Date(start_time),
         year = year(start_time)
  )

View(rice_farming_CH4emissions_asia)


##### Visualising the data #####

# as a bar chart #
ggplot(rice_farming_CH4emissions_asia, aes(iso3_country, emissions_quantity, fill = factor(year))) + 
  geom_col() +
  labs(x = "Asian country", y = "Total emissions", title = "Emissions from Rice Cultivation in Select Asian countries") +
  theme_minimal()

# as a line graph #
ggplot(rice_farming_CH4emissions_asia, aes(year, emissions_quantity, color = iso3_country)) + 
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("darkred", "steelblue3", "antiquewhite4", "lightsteelblue3", "aquamarine3", "darkslategray4", "pink", "lightgoldenrod1","lightsalmon4", "gold3", "mistyrose4", "maroon", "darkorange", "lightpink3")) +
  labs(x = "Year", y = "Emissions Quantity", title = "Emissions from Rice Cultivation in Select Asian Countries Over Time") +
  theme_minimal()



###### CH4 EMISSIONS FROM ENTERIC FERMENTATION IN FEEDLOT COW DIGESTION BY COUNTRY ######
## this dataset also contains mostly CH4, CO2 equivalent over 20 years, or CO2 equivalent over 100 years
## So I have opted to analyse CH4 emissions first, due to the potency of methane in trapping heat in the atmosphere.


# if you need to clear your environment #
# rm(list = ls())

#### Setting directory and importing the .csv data ####
setwd("C:/Users/ether/OneDrive - National University of Singapore/PP5531 Introduction to Coding for Public Policy using R/Group Project/Climate TRACE data/agriculture/DATA") 
feedlot_cow_emissions <- read.csv('enteric-fermentation-cattle-feedlot_country_emissions.csv')
View(feedlot_cow_emissions)


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


#### Filtering for CH4 emissions in ASEAN and select East Asian, South Asian countries ####

ASEAN <- c('THA', 'MYS', 'IDN', 'SGP', 'PHL', 'BRN', 'VNM', 'KHM', 'MMR', 'LAO')
East_Asia <- c('CHN', 'JPN', 'KOR')
South_Asia <- c('IND')

asian_countries <- c(ASEAN, East_Asia, South_Asia)

feedlot_cow_methane_emissions_asia <- feedlot_cow_emissions %>%
  filter(iso3_country %in% asian_countries) %>%
  filter(gas == "ch4")


## double checking if all the selected Asian countries are included ##
unique_count <- unique(feedlot_cow_methane_emissions_asia$iso3_country)
length(unique_count)

# we have 14 countries i.e. all countries have data here

## creating dummy variable for year for easy graph plotting ##
feedlot_cow_methane_emissions_asia <- feedlot_cow_methane_emissions_asia %>%
  mutate(start_time = as.Date(start_time),
         year = year(start_time)
  )

View(feedlot_cow_methane_emissions_asia)


##### Visualising the data #####

# as a bar chart #
ggplot(feedlot_cow_methane_emissions_asia, aes(iso3_country, emissions_quantity, fill = factor(year))) + 
  geom_col() +
  labs(x = "Asian country", y = "Total emissions", title = "Total Methane Released by Cows in Select Asian countries") +
  theme_minimal()

# as a line graph #
ggplot(feedlot_cow_methane_emissions_asia, aes(year, emissions_quantity, color = iso3_country)) + 
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("darkred", "steelblue3", "antiquewhite4", "lightsteelblue3", "aquamarine3", "darkslategray4", "pink", "lightgoldenrod1","lightsalmon4", "gold3", "mistyrose4", "maroon", "darkorange", "lightpink3")) +
  labs(x = "Year", y = "Emissions Quantity", title = "Total Methane Released by Feedlot Cows in Select Asian Countries Over Time") +
  theme_minimal()

  # alternative colour scheme #

ggplot(cow_methane_emissions_asia, aes(year, emissions_quantity, color = iso3_country)) + 
  geom_line() +
  geom_point() +
  scale_color_viridis_d(option = "plasma") +
  labs(x = "Year", y = "Emissions Quantity", title = "Total Methane Released by Feedlot Cows in Select Country Over Time") +
  theme_minimal()



###### CH4 EMISSIONS FROM ENTERIC FERMENTATION IN PASTURE COW DIGESTION BY COUNTRY ######
## this dataset also contains mostly CH4, CO2 equivalent over 20 years, or CO2 equivalent over 100 years
## So I have opted to analyse CH4 emissions first, due to the potency of methane in trapping heat in the atmosphere.


# if you need to clear your environment #
# rm(list = ls())

#### Setting directory and importing the .csv data ####
#setwd("C:/Users/ether/OneDrive - National University of Singapore/PP5531 Introduction to Coding for Public Policy using R/Group Project/Climate TRACE data/agriculture/DATA") 
pasture_cow_emissions <- read.csv('enteric-fermentation-cattle-pasture_country_emissions.csv')
View(pasture_cow_emissions)


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


#### Filtering for CH4 emissions in ASEAN and select East Asian, South Asian countries ####

ASEAN <- c('THA', 'MYS', 'IDN', 'SGP', 'PHL', 'BRN', 'VNM', 'KHM', 'MMR', 'LAO')
East_Asia <- c('CHN', 'JPN', 'KOR')
South_Asia <- c('IND')

asian_countries <- c(ASEAN, East_Asia, South_Asia)

pasture_cow_methane_emissions_asia <- pasture_cow_emissions %>%
  filter(iso3_country %in% asian_countries) %>%
  filter(gas == "ch4")


## double checking if all the selected Asian countries are included ##
unique_count <- unique(pasture_cow_methane_emissions_asia$iso3_country)
length(unique_count)

# we have 14 countries i.e. all countries have data here

## creating dummy variable for year for easy graph plotting ##
pasture_cow_methane_emissions_asia <- pasture_cow_methane_emissions_asia %>%
  mutate(start_time = as.Date(start_time),
         year = year(start_time)
  )

View(pasture_cow_methane_emissions_asia)


##### Visualising the data #####

# as a bar chart #
ggplot(pasture_cow_methane_emissions_asia, aes(iso3_country, emissions_quantity, fill = factor(year))) + 
  geom_col() +
  labs(x = "Asian country", y = "Total emissions", title = "Total Methane Released by Cows in Select Asian countries") +
  theme_minimal()

# as a line graph #
ggplot(pasture_cow_methane_emissions_asia, aes(year, emissions_quantity, color = iso3_country)) + 
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("darkred", "steelblue3", "antiquewhite4", "lightsteelblue3", "aquamarine3", "darkslategray4", "pink", "lightgoldenrod1","lightsalmon4", "gold3", "mistyrose4", "maroon", "darkorange", "lightpink3")) +
  labs(x = "Year", y = "Emissions Quantity", title = "Total Methane Released by Feedlot Cows in Select Asian Countries Over Time") +
  theme_minimal()



###### CH4 EMISSIONS FROM PASTURE COW MANURE BY COUNTRY ######
## this dataset also contains mostly CH4, CO2 equivalent over 20 years, or CO2 equivalent over 100 years
## So I have opted to analyse CH4 emissions first, due to the potency of methane in trapping heat in the atmosphere.


# if you need to clear your environment #
# rm(list = ls())

#### Setting directory and importing the .csv data ####
#setwd("C:/Users/ether/OneDrive - National University of Singapore/PP5531 Introduction to Coding for Public Policy using R/Group Project/Climate TRACE data/agriculture/DATA") 
pasture_cow_manure_emissions <- read.csv('manure-left-on-pasture-cattle_country_emissions.csv')
View(pasture_cow_manure_emissions)


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


#### Filtering for CH4 emissions in ASEAN and select East Asian, South Asian countries ####

ASEAN <- c('THA', 'MYS', 'IDN', 'SGP', 'PHL', 'BRN', 'VNM', 'KHM', 'MMR', 'LAO')
East_Asia <- c('CHN', 'JPN', 'KOR')
South_Asia <- c('IND')

asian_countries <- c(ASEAN, East_Asia, South_Asia)

pasture_cow_manure_emissions_asia <- pasture_cow_manure_emissions %>%
  filter(iso3_country %in% asian_countries) %>%
  filter(gas == "ch4")


## double checking if all the selected Asian countries are included ##
unique_count <- unique(pasture_cow_manure_emissions_asia$iso3_country)
length(unique_count)

# we have 14 countries i.e. all countries have data here

## creating dummy variable for year for easy graph plotting ##
pasture_cow_manure_emissions_asia <- pasture_cow_manure_emissions_asia %>%
  mutate(start_time = as.Date(start_time),
         year = year(start_time)
  )

View(pasture_cow_manure_emissions_asia)


##### Visualising the data #####

# as a bar chart #
ggplot(pasture_cow_manure_emissions_asia, aes(iso3_country, emissions_quantity, fill = factor(year))) + 
  geom_col() +
  labs(x = "Asian country", y = "Total emissions", title = "Total Methane Released by Pasture Cows in Select Asian countries") +
  theme_minimal()

# as a line graph #
ggplot(pasture_cow_manure_emissions_asia, aes(year, emissions_quantity, color = iso3_country)) + 
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("darkred", "steelblue3", "antiquewhite4", "lightsteelblue3", "aquamarine3", "darkslategray4", "pink", "lightgoldenrod1","lightsalmon4", "gold3", "mistyrose4", "maroon", "darkorange", "lightpink3")) +
  labs(x = "Year", y = "Emissions Quantity", title = "Total Methane Released by Pasture Cows in Select Asian Countries Over Time") +
  theme_minimal()



###### CH4 EMISSIONS FROM FEEDLOT COW MANURE BY COUNTRY ######
## this dataset also contains mostly CH4, CO2 equivalent over 20 years, or CO2 equivalent over 100 years
## So I have opted to analyse CH4 emissions first, due to the potency of methane in trapping heat in the atmosphere.


# if you need to clear your environment #
# rm(list = ls())

#### Setting directory and importing the .csv data ####
#setwd("C:/Users/ether/OneDrive - National University of Singapore/PP5531 Introduction to Coding for Public Policy using R/Group Project/Climate TRACE data/agriculture/DATA") 
feedlot_cow_manure_emissions <- read.csv('manure-management-cattle-feedlot_country_emissions.csv')
View(feedlot_cow_manure_emissions)


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


#### Filtering for CH4 emissions in ASEAN and select East Asian, South Asian countries ####

ASEAN <- c('THA', 'MYS', 'IDN', 'SGP', 'PHL', 'BRN', 'VNM', 'KHM', 'MMR', 'LAO')
East_Asia <- c('CHN', 'JPN', 'KOR')
South_Asia <- c('IND')

asian_countries <- c(ASEAN, East_Asia, South_Asia)

feedlot_cow_manure_emissions_asia <- feedlot_cow_manure_emissions %>%
  filter(iso3_country %in% asian_countries) %>%
  filter(gas == "ch4")


## double checking if all the selected Asian countries are included ##
unique_count <- unique(feedlot_cow_manure_emissions_asia$iso3_country)
length(unique_count)

# we have 14 countries i.e. all countries have data here

## creating dummy variable for year for easy graph plotting ##
feedlot_cow_manure_emissions_asia <- feedlot_cow_manure_emissions_asia %>%
  mutate(start_time = as.Date(start_time),
         year = year(start_time)
  )

View(feedlot_cow_manure_emissions_asia)


##### Visualising the data #####

# as a bar chart #
ggplot(feedlot_cow_manure_emissions_asia, aes(iso3_country, emissions_quantity, fill = factor(year))) + 
  geom_col() +
  labs(x = "Asian country", y = "Total emissions", title = "Total Methane Released by Cow Manure in Select Asian countries") +
  theme_minimal()

# as a line graph #
ggplot(feedlot_cow_manure_emissions_asia, aes(year, emissions_quantity, color = iso3_country)) + 
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("darkred", "steelblue3", "antiquewhite4", "lightsteelblue3", "aquamarine3", "darkslategray4", "pink", "lightgoldenrod1","lightsalmon4", "gold3", "mistyrose4", "maroon", "darkorange", "lightpink3")) +
  labs(x = "Year", y = "Emissions Quantity", title = "Total Methane Released by Feedlot Cow Manure in Select Asian Countries Over Time") +
  theme_minimal()





###### SYNTHETIC FERTILISER EMISSIONS BY COUNTRY ######
## this dataset only contains CO2 equivalent over 20 years and CO2 equivalent over 100 years
## So I have opted to analyse CO2 equivalent over 20 years emissions first.


# if you need to clear your environment #
# rm(list = ls())

#### Setting directory and importing the .csv data ####
#setwd("C:/Users/ether/OneDrive - National University of Singapore/PP5531 Introduction to Coding for Public Policy using R/Group Project/Climate TRACE data/agriculture/DATA") 
syn_fertiliser_emissions <- read.csv('synthetic-fertilizer-application_country_emissions.csv')
View(syn_fertiliser_emissions)


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


#### Filtering for CO2 equivalent over 20 years emissions in ASEAN and select East Asian, South Asian countries ####

ASEAN <- c('THA', 'MYS', 'IDN', 'SGP', 'PHL', 'BRN', 'VNM', 'KHM', 'MMR', 'LAO')
East_Asia <- c('CHN', 'JPN', 'KOR')
South_Asia <- c('IND')

asian_countries <- c(ASEAN, East_Asia, South_Asia)

syn_fertiliser_emissions_asia <- syn_fertiliser_emissions %>%
  filter(iso3_country %in% asian_countries) %>%
  filter(gas == "co2e_20yr")


## double checking if all the selected Asian countries are included ##
unique_count <- unique(syn_fertiliser_emissions_asia$iso3_country)
length(unique_count)

# we have 14 countries i.e. all countries have data here

## creating dummy variable for year for easy graph plotting ##
syn_fertiliser_emissions_asia <- syn_fertiliser_emissions_asia %>%
  mutate(start_time = as.Date(start_time),
         year = year(start_time)
  )

View(syn_fertiliser_emissions_asia)


##### Visualising the data #####

# as a bar chart #
ggplot(syn_fertiliser_emissions_asia, aes(iso3_country, emissions_quantity, fill = factor(year))) + 
  geom_col() +
  labs(x = "Asian country", y = "Total emissions", title = "Total CO2 (equivalent over 20 years) Released by Synthetic Fertilisers in Select Asian countries") +
  theme_minimal()

# as a line graph #
ggplot(syn_fertiliser_emissions_asia, aes(year, emissions_quantity, color = iso3_country)) + 
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("darkred", "steelblue3", "antiquewhite4", "lightsteelblue3", "aquamarine3", "darkslategray4", "pink", "lightgoldenrod1","lightsalmon4", "gold3", "mistyrose4", "maroon", "darkorange", "lightpink3")) +
  labs(x = "Year", y = "Emissions Quantity", title = "Total CO2 (equivalent over 20 years) Released by Synthetic Fertilisers in Select Asian Countries Over Time") +
  theme_minimal()





###### OTHER AGRICULTURAL SOIL EMISSIONS BY COUNTRY ######
## this dataset only contains CO2 equivalent over 20 years and CO2 equivalent over 100 years
## So I have opted to analyse CO2 equivalent over 20 years emissions first.


# if you need to clear your environment #
# rm(list = ls())

#### Setting directory and importing the .csv data ####
#setwd("C:/Users/ether/OneDrive - National University of Singapore/PP5531 Introduction to Coding for Public Policy using R/Group Project/Climate TRACE data/agriculture/DATA") 
other_agrisoil_emissions <- read.csv('other-agricultural-soil-emissions_country_emissions.csv')
View(syn_fertiliser_emissions)


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


#### Filtering for CO2 equivalent over 20 years emissions in ASEAN and select East Asian, South Asian countries ####

ASEAN <- c('THA', 'MYS', 'IDN', 'SGP', 'PHL', 'BRN', 'VNM', 'KHM', 'MMR', 'LAO')
East_Asia <- c('CHN', 'JPN', 'KOR')
South_Asia <- c('IND')

asian_countries <- c(ASEAN, East_Asia, South_Asia)

other_agrisoil_emissions_asia <- other_agrisoil_emissions %>%
  filter(iso3_country %in% asian_countries) %>%
  filter(gas == "co2e_20yr")


## double checking if all the selected Asian countries are included ##
unique_count <- unique(other_agrisoil_emissions_asia$iso3_country)
length(unique_count)

# we have 14 countries i.e. all countries have data here

## creating dummy variable for year for easy graph plotting ##
syn_fertiliser_emissions_asia <- syn_fertiliser_emissions_asia %>%
  mutate(start_time = as.Date(start_time),
         year = year(start_time)
  )

View(syn_fertiliser_emissions_asia)


##### Visualising the data #####

# as a bar chart #
ggplot(syn_fertiliser_emissions_asia, aes(iso3_country, emissions_quantity, fill = factor(year))) + 
  geom_col() +
  labs(x = "Asian country", y = "Total emissions", title = "Total CO2 (equivalent over 20 years)  Released by Other Agricultural Soils in Select Asian countries") +
  theme_minimal()

# as a line graph #
ggplot(syn_fertiliser_emissions_asia, aes(year, emissions_quantity, color = iso3_country)) + 
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("darkred", "steelblue3", "antiquewhite4", "lightsteelblue3", "aquamarine3", "darkslategray4", "pink", "lightgoldenrod1","lightsalmon4", "gold3", "mistyrose4", "maroon", "darkorange", "lightpink3")) +
  labs(x = "Year", y = "Emissions Quantity", title = "Total CO2 (equivalent over 20 years)  Released by Other Agricultural Soils in Select Asian Countries Over Time") +
  theme_minimal()
