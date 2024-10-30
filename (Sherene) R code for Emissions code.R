#### Master Code for Emissions ####

This code can be applied on raw EMISSIONS data (sans sources, confidence and ownership data files), and then filtered slowly by relevant countries.

The emissions data of different sectors will be joined together to create a master dataset below.

SUGGESTION: You can view this in R markdown and run the codes in chunks.



#### Install and library the following packages ####
(Can skip to next part if you already have them installed)
```{r}

install.packages(dplyr)
install.packages(ggplot2)
install.packages(lubridate)
install.packages("ggthemes")
install.packages("RColorBrewer")

```


#### Library all the packages below ####
```{r}

library(dplyr)
library(ggplot2)
library(lubridate)
library(ggthemes)
library(RColorBrewer)

```


#### Unzipping all the data files and importing them into R ####
```{r}

# Setting working directory (optional) and folder path to the folder containing all Excel files
setwd("C:/Users/ether/OneDrive - National University of Singapore/PP5531 Introduction to Coding for Public Policy using R/Group Project/Climate TRACE data/combined_emissions_data") 
folder_path <- "C:/Users/ether/OneDrive - National University of Singapore/PP5531 Introduction to Coding for Public Policy using R/Group Project/Climate TRACE data/combined_emissions_data"  # Replace with your actual folder path

# List all CSV files in the folder
file_list <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

# Loop through each file and import it into a separate data frame
all_emissions = data.frame()

for (file in file_list) {
  print(file)
  # Extract the file name without extension
  file_name <- tools::file_path_sans_ext(basename(file))
  
  # Read the CSV file
  data <- read.csv(file)
  
  # Assign the data frame to a variable in the global environment
  assign(file_name, data, envir = .GlobalEnv)
  
  all_emissions <- all_emissions %>% rbind(data)
}

# Converting emissions quantity units to per billion tonnes
all_emissions <- all_emissions %>% 
  mutate(emissions_quantity_in_billions = (emissions_quantity / 1e9))

```


#### Setting up filters (for CO2 per 100yr) in select Asian countries ####
```{r}

ASEAN <- c('THA', 'MYS', 'IDN', 'SGP', 'PHL', 'BRN', 'VNM', 'KHM', 'MMR', 'LAO')
East_Asia <- c('CHN', 'JPN', 'KOR')
South_Asia <- c('IND')

asian_countries <- c(ASEAN, East_Asia, South_Asia)

asian_US_countries <- c(ASEAN, East_Asia, South_Asia, 'USA')


```


#### Preliminary analysis of data ####
```{r}

# Checking emissions across the world
# (will need to zoom out the graph to see the chart properly)

all_emissions_graph <- all_emissions %>%
  mutate(start_time = as.Date(start_time),
         year = year(start_time)) %>%
  filter(gas == "co2e_100yr") %>%
# removing all net forest changes
  filter(!(subsector %in% c("net-forest-land", "net-shrubgrass", "net-wetland", "removals"))) %>%
  filter(year < 2023) %>%
# Drop unwanted columns in each dataset before joining
  select(-start_time, -end_time, -emissions_quantity_units, -temporal_granularity, -created_date, -modified_date) %>%      
  group_by(iso3_country, year) %>%
  summarize(total_emissions = sum(emissions_quantity_in_billions, na.rm = T))

ggplot(all_emissions_graph, aes(x = year, y = total_emissions, fill = iso3_country)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Emissions for Each Country per Year",
    x = "Year",
    y = "Emissions Quantity (tonnes)"
  ) +
  coord_flip() + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



### Plotting graph of total emissions across Asian countries + US over the years ###

all_emissions_Asia_US <- all_emissions %>%   # piping all_emissions through some filtering by country and gas type
  mutate(start_time = as.Date(start_time),
         year = year(start_time)) %>%
  filter(iso3_country %in% asian_US_countries) %>%
  filter(gas == "co2e_100yr") %>%
  # removing all net forest changes as they are positive values adding to emissions, which is incorrect
  filter(!(subsector %in% c("net-forest-land", "net-shrubgrass", "net-wetland", "removals"))) %>%
  filter(year < 2023) %>%
  select(-start_time, -end_time, -emissions_quantity_units, -temporal_granularity, -created_date, -modified_date) %>%      # Drop unwanted columns in each dataset before joining
  group_by(iso3_country, year) %>%
  summarize(total_emissions = sum(emissions_quantity_in_billions, na.rm = T))

ggplot(all_emissions_Asia_US, aes(x = factor(year), y = total_emissions, fill = reorder(iso3_country, -total_emissions))) +
  geom_bar(stat = "identity") +
  labs(title = "Total Emissions by Country and Year (with U.S)\n(in billion tonnes)",
       x = "Year",
       y = "Total Emissions (CO2 per 100 years)",
       fill = "Country") +
  scale_fill_manual(values = c("darkred", "snow2", "antiquewhite3", "lightgoldenrod1", "aquamarine3", "darkslategray4", "pink", "lightsteelblue3","lightsalmon3", "gold3", "mistyrose2", "maroon", "darkorange", "steelblue3", "lightpink3")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.title = element_text(size = 12)
  )



### Obtaining cumulative emissions for a preliminary look ###

# creating a vector to use as filter for case study countries + US for comparison
casestudy_countries <- c("CHN", "IND", "IDN", "USA")

emissions_cumulative <- all_emissions %>%
  mutate(start_time = as.Date(start_time),
         year = year(start_time)) %>%
  filter(iso3_country %in% casestudy_countries) %>%
  filter(gas == "co2e_100yr") %>%
  filter(year < 2023) %>%
  select(-start_time, -end_time, -temporal_granularity, -created_date, -modified_date) %>%
  group_by(iso3_country, year) %>%
  summarise(total_emissions = sum(emissions_quantity, na.rm = TRUE), .groups = 'drop') %>%
  mutate(cumulative_emissions = cumsum(total_emissions))

ggplot(emissions_cumulative, aes(x = year, y = cumulative_emissions, color = iso3_country, group = iso3_country)) +
  geom_line() +
  geom_point() +
  labs(title = "Cumulative Emissions Over the Years",
       x = "Year",
       y = "Cumulative Emissions (CO2 per 100 years)",
       color = "Country"
       ) +
  scale_colour_manual(values = c("darkred", "antiquewhite3", "lightgoldenrod2", "salmon1")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1))

# we did not use this cumulative emissions graph in the end, as the short time scale may give a misleading look as though China has been emitting the least over the years (we would need to include more of the preceding years for a more meaningful and accurate depiction of the cumulative emissions since pre-industrial period)

```


#### Plotting graph of total emissions across ONLY select Asian countries over the years ####
```{r}

all_emissions_Asia <- all_emissions %>%  #summing emissions across countries and grouping by year
  mutate(start_time = as.Date(start_time),
         year = year(start_time)) %>%
  filter(iso3_country %in% asian_countries) %>%
  filter(gas == "co2e_100yr") %>%
  # removing all net forest changes as they are positive values adding to emissions, which is incorrect
  filter(!(subsector %in% c("net-forest-land", "net-shrubgrass", "net-wetland", "removals"))) %>%
  filter(year < 2023) %>%
  select(-start_time, -end_time, -emissions_quantity_units, -temporal_granularity, -created_date, -modified_date)
# Drop unwanted columns in each dataset before joining
  
all_emissions_Asia_forgraph <- all_emissions_Asia %>%
  group_by(iso3_country, year) %>%
  summarize(total_emissions = sum(emissions_quantity_in_billions, na.rm = T))

ggplot(all_emissions_Asia_forgraph, aes(x = factor(year), y = total_emissions, fill = reorder(iso3_country, -total_emissions))) +
  geom_bar(stat = "identity") +
  labs(title = "Total Emissions by Country and Year \nin select Asian countries (in billion tonnes)",
       x = "Year",
       y = "Total Emissions (CO2 per 100 years)",
       fill = "Country") +
  scale_fill_manual(values = c("darkred", "antiquewhite3", "lightgoldenrod1", "aquamarine3", "darkslategray4", "pink", "lightsteelblue3","lightsalmon3", "gold3", "mistyrose2", "maroon", "darkorange", "steelblue3", "lightpink3")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.title = element_text(size = 12)
  )


```


#### Calculating Year-on-Year (YoY) change in emissions for select Asian countries ####
```{r}

emissions_Asia_yoy_change <- all_emissions_Asia %>%
  arrange(iso3_country, year) %>%
  group_by(iso3_country, year) %>%
  summarize(total_emissions = sum(emissions_quantity_in_billions, na.rm = T)) %>%
  mutate(yearly_change = total_emissions - lag(total_emissions)) %>%
  ungroup()
  
ggplot(emissions_Asia_yoy_change, aes(x = year, y = yearly_change, color = reorder(iso3_country, -yearly_change, na.rm = T))) +
  geom_line(size = 0.5) +
  geom_point(size = 1.5) +
  labs(
    title = "Year-on-Year Change in Emissions for select Asian Countries",
    x = "Year",
    y = "Year-on-Year Change in Emissions (tonnes)",
    color = "Country"
  ) +
  scale_colour_manual(values = c("darkred", "antiquewhite3", "aquamarine3", "darkslategray4", "pink", "lightsteelblue3","lightsalmon3", "gold3", "mistyrose2", "maroon", "darkorange", "steelblue3", "lightpink3", "lightgoldenrod2")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


```


#### Calculating Year-on-Year (YoY) change in emissions for ONLY case study countries (China, India and Indonesia) ####
```{r}

emissions_top3asia_yoy_change <- all_emissions_Asia %>%
  filter(iso3_country %in% c('CHN', 'IND', 'IDN')) %>%
  arrange(iso3_country, year) %>%
  group_by(iso3_country, year) %>%
  summarize(total_emissions = sum(emissions_quantity_in_billions, na.rm = T)) %>%
  mutate(
    yearly_change = total_emissions - lag(total_emissions),
    yearly_change_percent = (yearly_change / lag(total_emissions)) * 100
  )
  
ggplot(emissions_top3asia_yoy_change, aes(x = year, y = yearly_change_percent, color = iso3_country)) +
  geom_line(size = 0.5) +
  geom_point(size = 1.5) +
  labs(
    title = "Year-on-Year Change in Emissions\nfor Asian Countries",
    x = "Year",
    y = "Year-on-Year Change in Emissions (%)",
    color = "Country"
  ) +
  scale_colour_manual(values = c("darkred", "antiquewhite3", "goldenrod1")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.title = element_text(size = 10)
  )
```


#### Visualing INDONESIA's (IDN) emissions by sector, over the years ####
```{r}

all_emissions_indonesia <- all_emissions %>%
  mutate(start_time = as.Date(start_time),
         year = year(start_time)) %>%
  filter(iso3_country %in% 'IDN') %>%
  filter(gas == "co2e_100yr") %>%
  # removing all net forest changes as they are positive values adding to emissions, which is incorrect
  filter(!(subsector %in% c("net-forest-land", "net-shrubgrass", "net-wetland", "removals"))) %>%
  filter(year < 2023) %>%
  select(-start_time, -end_time, -temporal_granularity, -created_date, -modified_date)

emissions_by_sector_Indonesia <- all_emissions_indonesia %>%  
  group_by(sector, subsector, year) %>%
  summarize(total_emissions = sum(emissions_quantity_in_billions))

Indonesia_agriculture <- emissions_by_sector_Indonesia %>%
  filter(sector == "agriculture") %>%
  group_by(year) %>%
  summarize(total_emissions = sum(total_emissions, na.rm = T), .groups = 'drop')

ggplot(emissions_by_sector_Indonesia, aes(x = factor(year), y = total_emissions, fill = sector)) +
  geom_bar(stat = "identity") +
  labs(title = "Indonesia's Emissions\n(in billion tonnes)",
       x = "Year",
       y = "Total Emissions (CO2 per 100 years)",
       fill = "Sector") +
  scale_fill_manual(values = c("darkolivegreen4", "steelblue2", "gold", "seashell2", "palevioletred4", "darkslategray4", "maroon", "mistyrose3","lightsalmon2", "gold3", "mistyrose4", "maroon", "darkorange", "lightpink3")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 17),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.title = element_text(size = 14)
    )

ggplot(emissions_by_sector_Indonesia, aes(x = factor(year), y = total_emissions)) +
  geom_bar(stat = "identity", fill = "mistyrose3") +
  geom_hline(yintercept = 0, color = "sienna4", linetype = "dashed") +  # Adds y = 0 line
    labs(title = "Indonesia's Total Emissions Across the Years\n(in billion tonnes)",
       x = "Year",
       y = "Total Emissions (CO2 per 100 years)") +
#  ylim(0, 2.5e+10) +  # Set the y-axis limits as needed
  theme_minimal() +
  theme(
    plot.title = element_text(size = 15),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.title = element_text(size = 12)
  )



## Breakdown of emissions in agriculture in INDONESIA (top-emitting #1):

subsector_agri_emissions_Indonesia <- emissions_by_sector_Indonesia %>%
  filter(sector == "agriculture") 

ggplot(subsector_agri_emissions_Indonesia, aes(x = factor(year), y = total_emissions, fill = subsector)) +
  geom_bar(stat = "identity") +
  labs(title = "Breakdown of Indonesia's Agricultural Emissions\nOver the Years (in billion tonnes)",
       x = "Year",
       y = "Total Emissions (CO2 per 100 years)",
       fill = "Subsector") +
  scale_fill_manual(values = c("darkolivegreen", "khaki4", "khaki3", "khaki2", "honeydew4", "honeydew3", "honeydew2", "lemonchiffon3","snow2", "lightpink3")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 17),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.title = element_text(size = 12)
  )



## Breakdown of emissions in LULC in INDONESIA (top-emitting #2):

all_emissions_indonesia_full <- all_emissions %>%
  mutate(start_time = as.Date(start_time),
         year = year(start_time)) %>%
  filter(iso3_country %in% 'IDN') %>%
  filter(gas == "co2e_100yr") %>%
  filter(year < 2023) %>%
  select(-start_time, -end_time, -temporal_granularity, -created_date, -modified_date)


subsector_LULC_emissions_Indonesia <- all_emissions_indonesia_full %>%
  group_by(sector, subsector, year) %>%
  summarize(total_emissions = sum(emissions_quantity_in_billions)) %>%
  filter(sector == "forestry-and-land-use")

ggplot(subsector_LULC_emissions_Indonesia, aes(x = factor(year), y = total_emissions, fill = subsector)) +
  geom_bar(stat = "identity") +
  geom_line(data = subset(subsector_LULC_emissions_Indonesia, subsector == "forest-land-clearing"), 
            aes(x = factor(year), y = total_emissions, group = 1), 
            color = "#C1A2B2", size = 0.5) + 
  geom_point(data = subset(subsector_LULC_emissions_Indonesia, subsector == "forest-land-clearing"), 
             aes(x = factor(year), y = total_emissions), 
             color = "#C1A2B2", size = 1.5) +  
  labs(title = "Breakdown of Indonesia's LULC Emissions\nOver the Years (in billion tonnes)",
       x = "Year",
       y = "Total Emissions (CO2 per 100 years)",
       fill = "Subsector") +
  scale_fill_manual(values = c("darkolivegreen", "darkseagreen4", "darkseagreen3", "darkseagreen2", "burlywood4", "honeydew3", "honeydew2", "burlywood","darkcyan", "honeydew4")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 17),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.title = element_text(size = 12)
  )



## Breakdown of emissions in fossil fuel operations in INDONESIA (top-emitting #3):

subsector_fossilfuel_emissions_Indonesia <- all_emissions_indonesia %>%
  group_by(sector, subsector, year) %>%
  summarize(total_emissions = sum(emissions_quantity_in_billions)) %>%
  filter(sector == "fossil-fuel-operations")

ggplot(subsector_fossilfuel_emissions_Indonesia, aes(x = factor(year), y = total_emissions, fill = subsector)) +
  geom_bar(stat = "identity") +
  labs(title = "Breakdown of Indonesia's Emissions from Fossil Fuel Operations Over the Years\n(in billion tonnes)",
       x = "Year",
       y = "Total Emissions (CO2 per 100 years)") +
  scale_fill_manual(values = c("darkolivegreen", "darkseagreen4", "darkseagreen3", "darkseagreen2", "burlywood4", "honeydew3", "honeydew2", "burlywood","darkcyan", "honeydew4")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 17),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.title = element_text(size = 12)
  )

```


#### Visualing INDIA's (IND) emissions by sector, over the years ####
```{r}

all_emissions_india <- all_emissions %>%   # piping all_emissions through some filtering by country and gas type
  mutate(start_time = as.Date(start_time),
         year = year(start_time)) %>%
  filter(iso3_country %in% 'IND') %>%
  filter(gas == "co2e_100yr") %>%
# filter(sector == "manufacturing") %>%
# removing all net forest changes as they are positive values adding to emissions, which is incorrect
  filter(!(subsector %in% c("net-forest-land", "net-shrubgrass", "net-wetland", "removals"))) %>%
  filter(year < 2023) %>%
  select(-start_time, -end_time, -temporal_granularity, -created_date, -modified_date)      # Drop unwanted columns in each dataset before joining

emissions_by_sector_india <- all_emissions_india %>%  #summing emissions across countries and grouping by year
  group_by(sector, year) %>%
  summarize(total_emissions = sum(emissions_quantity_in_billions))

ggplot(emissions_by_sector_india, aes(x = factor(year), y = total_emissions, fill = sector)) +
  geom_bar(stat = "identity") +
  labs(title = "India's Emissions\n(in billion tonnes)",
       x = "Year",
       y = "Total Emissions (CO2 per 100 years)",
       fill = "Sector") +
  scale_fill_manual(values = c("darkolivegreen4", "steelblue2", "gold", "seashell2", "palevioletred4", "darkslategray4", "maroon", "mistyrose3","lightsalmon2", "gold3", "mistyrose4", "maroon", "darkorange", "lightpink3")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 17),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.title = element_text(size = 12)
  )

ggplot(emissions_by_sector_india, aes(x = factor(year), y = total_emissions)) +
  geom_bar(stat = "identity", fill = "salmon") +
  geom_hline(yintercept = 0, color = "sienna4", linetype = "dashed") +  # Adds y = 0 line
  labs(title = "India's Total Emissions Across the Years\n(in billion tonnes)",
       x = "Year",
       y = "Total Emissions (CO2 per 100 years)") +
# ylim(0, 2.5e+10) +  # Set the y-axis limits
  theme_minimal() +
  theme(
    plot.title = element_text(size = 15),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.title = element_text(size = 12)
  )


## Breakdown of emissions in power in INDONESIA (top-emitting #1):

subsector_energy_emissions_India <- emissions_by_sector_Indonesia %>%
  filter(sector == "power") 

ggplot(subsector_energy_emissions_India, aes(x = factor(year), y = total_emissions, fill = subsector)) +
  geom_bar(stat = "identity") +
  labs(title = "Breakdown of India's Power Emissions\nOver the Years (in billion tonnes)",
       x = "Year",
       y = "Total Emissions (CO2 per 100 years)",
       fill = "Subsector") +
  scale_fill_manual(values = c("slategray1", "slategray")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 17),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.title = element_text(size = 12)
  )


```


#### Visualing China's (CHN) emissions by sector, over the years ####
```{r}

all_emissions_China <- all_emissions %>%   # piping all_emissions through some filtering by country and gas type
  mutate(start_time = as.Date(start_time),
         year = year(start_time)) %>%
  filter(iso3_country %in% 'CHN') %>%
  filter(gas == "co2e_100yr") %>%
  # removing all net forest changes as they are positive values adding to emissions, which is incorrect
  filter(!(subsector %in% c("net-forest-land", "net-shrubgrass", "net-wetland", "removals"))) %>%
  filter(year < 2023) %>%
  select(-start_time, -end_time, -temporal_granularity, -created_date, -modified_date)      # Drop unwanted columns in each dataset before joining

emissions_by_sector_China <- all_emissions_China %>%  #summing emissions across countries and grouping by year
  group_by(sector, year) %>%
  summarize(total_emissions = sum(emissions_quantity_in_billions))

ggplot(emissions_by_sector_China, aes(x = factor(year), y = total_emissions, fill = sector)) +
  geom_bar(stat = "identity") +
  labs(title = "China's Emissions\n(in billion tonnes)",
       x = "Year",
       y = "Total Emissions (CO2 per 100 years)") +
  scale_fill_manual(values = c("darkolivegreen4", "steelblue2", "gold", "seashell2", "palevioletred4", "darkslategray4", "maroon", "mistyrose3","lightsalmon2", "gold3", "mistyrose4", "maroon", "darkorange", "lightpink3")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 17),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.title = element_text(size = 12)
  )

ggplot(emissions_by_sector_China, aes(x = factor(year), y = total_emissions)) +
  geom_bar(stat = "identity", fill = "peachpuff4") +
  geom_hline(yintercept = 0, color = "rosybrown1", linetype = "dashed") +  # Adds y = 0 line
  labs(title = "China's Total Emissions Across the Years\n(in billion tonnes)",
       x = "Year",
       y = "Total Emissions (CO2 per 100 years)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 15),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.title = element_text(size = 12)
  )


```


#### Visualing USA's (USA) emissions by sector, over the years ####
```{r}

all_emissions_US <- all_emissions %>%   # piping all_emissions through some filtering by country and gas type
  mutate(start_time = as.Date(start_time),
         year = year(start_time)) %>%
  filter(iso3_country %in% 'USA') %>%
  filter(gas == "co2e_100yr") %>%
  filter(year < 2023) %>%
  select(-start_time, -end_time, -temporal_granularity, -created_date, -modified_date)      # Drop unwanted columns in each dataset before joining

emissions_by_sector_US <- all_emissions_US %>%  #summing emissions across countries and grouping by year
  group_by(sector, year) %>%
  summarize(total_emissions = sum(emissions_quantity))

ggplot(emissions_by_sector_US, aes(x = factor(year), y = total_emissions, fill = sector)) +
  geom_bar(stat = "identity") +
  labs(title = "USA's Emissions by Sector Over the Years",
       x = "Year",
       y = "Total Emissions (CO2e)") +
  scale_fill_manual(values = c("darkolivegreen4", "steelblue2", "gold", "seashell2", "firebrick2", "darkslategray4", "maroon", "mistyrose3","lightsalmon2", "gold3", "mistyrose4", "maroon", "darkorange", "lightpink3")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


```


#### GHG removals by forests ####


###### Indonesia's forest removals ######
```{r}

removals_emissions_indonesia <- all_emissions %>%   # piping all_emissions through some filtering by country and gas type
  mutate(start_time = as.Date(start_time),
         year = year(start_time)) %>%
  filter(iso3_country %in% 'IDN') %>%
  filter(gas == "co2e_100yr") %>%
  filter(year < 2023) %>%
  filter (subsector == "removals") %>%
select(-start_time, -end_time, -temporal_granularity, -created_date, -modified_date)      # Drop unwanted columns in each dataset before joining

ggplot(removals_emissions_indonesia, aes(factor(year), -emissions_quantity_in_billions)) +
  geom_bar(stat = "identity", fill = "honeydew3") +
  labs(title = "Greenhouse Gas Sequestration by Forests \nin Indonesia (in billion tonnes)",
       x = "Year",
       y = "Total Emissions (CO2 per 100 year)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.title = element_text(size = 12)
  )


```

###### India's forest removals ######
```{r}

removals_emissions_india <- all_emissions %>%   # piping all_emissions through some filtering by country and gas type
  mutate(start_time = as.Date(start_time),
         year = year(start_time)) %>%
  filter(iso3_country %in% 'IND') %>%
  filter(gas == "co2e_100yr") %>%
  filter(year < 2023) %>%
  filter (subsector == "removals") %>%
  select(-start_time, -end_time, -temporal_granularity, -created_date, -modified_date)      # Drop unwanted columns in each dataset before joining

ggplot(removals_emissions_india, aes(factor(year), -emissions_quantity_in_billions)) +
  geom_bar(stat = "identity", fill = "honeydew3") +
  labs(title = "Greenhouse Gas Sequestration by Forests\n in India (in billion tonnes)",
       x = "Year",
       y = "Total Emissions (CO2 per 100 year)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.title = element_text(size = 12)
  )
```


###### China's forest removals ######
```{r}

removals_emissions_china <- all_emissions %>%   # piping all_emissions through some filtering by country and gas type
  mutate(start_time = as.Date(start_time),
         year = year(start_time)) %>%
  filter(iso3_country %in% 'CHN') %>%
  filter(gas == "co2e_100yr") %>%
  filter(year < 2023) %>%
  filter (subsector == "removals") %>%
  select(-start_time, -end_time, -temporal_granularity, -created_date, -modified_date)      # Drop unwanted columns in each dataset before joining

ggplot(removals_emissions_china, aes(factor(year), -emissions_quantity_in_billions)) +
  geom_bar(stat = "identity", fill = "honeydew3") +
  labs(title = "Greenhouse Gas Sequestration by Forests\n in China (in billion tonnes)",
       x = "Year",
       y = "Total Emissions (CO2 per 100 year)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.title = element_text(size = 12)
  )
```
