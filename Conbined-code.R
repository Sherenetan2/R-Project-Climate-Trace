library(readxl)
library(writexl)
library(dplyr)
library(ggplot2)
library(RColorBrewer)


# define the path of the folder
data_folder <- "C:/Users/hasie/Desktop/LKYSPP/SEM3/R programming/group project/data"  

# Get all the Excel files in the folder and filter out the files ending with "emissions"
file_list <- list.files(path = data_folder, pattern = "_emissions\\.csv$", recursive = TRUE, full.names = TRUE)
print(file_list)
# Create an empty data box to store all extracted table data
all_data <- data.frame()

# Iterate through each matching file, read the data and merge it into all_data
for (file in file_list) {
  # Read the first table of an csv file
  file_data <- read.csv(file)
  
  # Consolidation of data
  all_data <- bind_rows(all_data, file_data)
}

# Save the merged data to a new Excel file
write_xlsx(all_data, "C:/Users/hasie/Desktop/LKYSPP/SEM3/R programming/group project/CHN_IDN_IND_emissions_data.xlsx")  


# read the file 
file_path <- "C:/Users/hasie/Desktop/LKYSPP/SEM3/R programming/group project/CHN_IDN_IND_emissions_data.xlsx"
data <- read_excel(file_path)

# Create a new column 'year', extract the first 4 characters of the 'start_time' column
data <- data %>%
  mutate(year = substr(start_time, 1, 4))

# Delete the specified column
columns_to_drop <- c("start_time", "end_time", "created_date", "modified_date", "temporal_granularity")
data <- data %>%
  select(-one_of(columns_to_drop))

# Create a new column 'emissions_quantity_billions' whose value is 'emissions_quantity' divided by 1 billion
data <- data %>%
  mutate(emissions_quantity_billions = emissions_quantity / 1e9)

View(data)

# First filter out the data from 2015 to 2022
filteredyear_data <- data %>%
  filter(year >= 2015 & year <= 2022)

co2e_filteredyear_data <- filteredyear_data %>%
  filter(gas == "co2e_100yr")

# Calculation of total annual emissions per country (in billions)
totalemissionscountry_data <- co2e_filteredyear_data %>%
  group_by(year, iso3_country) %>%
  summarise(total_emissions_billion = sum(emissions_quantity_billions, na.rm = TRUE))

# Read population data
population_data <- data.frame(
  year = c(2015:2022),
  CHN = c(1.375, 1.383, 1.390, 1.395, 1.400, 1.410, 1.413, 1.412),
  IND = c(1.309, 1.324, 1.339, 1.354, 1.369, 1.384, 1.399, 1.415),
  IDN = c(0.258, 0.261, 0.264, 0.266, 0.270, 0.274, 0.276, 0.279)
)

# Conversion of population data from wide format to long format
library(reshape2)
population_long <- melt(population_data, id.vars = "year", variable.name = "iso3_country", value.name = "population")

# Combining emissions and population data
totalemissionscountry_data <- merge(totalemissionscountry_data, population_long, by = c("iso3_country", "year"))

# Create a new column to calculate emissions per person which equals to（total_emissions_billion / population）
totalemissionscountry_data$emissions_per_capita <- totalemissionscountry_data$total_emissions_billion / totalemissionscountry_data$population

View(totalemissionscountry_data)
# calculate the changes in per emissions
ggplot(totalemissionscountry_data, aes(x = as.numeric(year), y = emissions_per_capita, color = iso3_country, group = iso3_country)) +
  geom_line(size = 1.5) +  
  geom_point(size = 2.5) +  
  labs(title = "Change in Per Capita Emissions, 2015 - 2022", 
       x = "Year", 
       y = "Per Capita Emissions (CO2 per 100 years)",
       color = "Country") +  
  scale_x_continuous(breaks = seq(2015, 2022, 1)) +  
  scale_color_manual(values = c("CHN" = "#8B0000",  
                                "IND" = "#CDC0B0",  
                                "IDN" = "lightgoldenrod2")) +  
  theme_minimal() +  
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),  
        text = element_text(size = 8), 
        axis.text.x = element_text(angle = 45, hjust = 1)  
  )
# calculate the changes in total emissions
ggplot(totalemissionscountry_data, aes(x = as.numeric(year), y = total_emissions_billion, color = iso3_country, group = iso3_country)) +
  geom_line(size = 1.5) +  
  geom_point(size = 2.5) +  
  labs(title = "Change in Total Emissions, 2015 - 2022 (in billion tonnes)", 
       x = "Year", 
       y = "Total Emissions (CO2 per 100 years)",
       color = "Country") +  
  scale_x_continuous(breaks = seq(2015, 2022, 1)) +  
  scale_color_manual(values = c("CHN" = "#8B0000",  
                                "IND" = "#CDC0B0",  
                                "IDN" = "lightgoldenrod2")) +  
  theme_minimal() +  
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),  
    text = element_text(size = 8),  
    axis.text.x = element_text(angle = 45, hjust = 1)  
  )




# Filter data with sector as power
power_data <- subset(co2e_filteredyear_data, sector == "power"& iso3_country == "CHN")

ggplot(power_data, aes(x = as.factor(year), y = emissions_quantity_billions, fill = subsector)) +
  geom_bar(stat = "identity") +  
  labs(title = "China's Power Sector Emissions by Subsector",
       x = "Year",
       y = "Total Emissions (Billion)",
       fill = "Subsector") +
  scale_fill_manual(values = c("electricity-generation" = "#CDB7B5",  
                               "other-energy-use" = "#E6BCBE")) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5, size = 12),  
        text = element_text(size = 10),  
        axis.text.x = element_text(angle = 45, hjust = 1))
  

# Prepare the colors for the manufacturing part
# Define the base colour
base_color <- "#528B8B"

# Generate gradient colours with colorRampPalette
color_palette <- colorRampPalette(c(base_color, "#DDEBEB"))  

# Generate 7 gradient colours
colors <- color_palette(7)

# View generated colours
print(colors)
# Filter data with sector as manufacturing
manufacturing_data <- subset(co2e_filteredyear_data, sector == "manufacturing"& iso3_country == "CHN")

ggplot(manufacturing_data, aes(x = as.factor(year), y = emissions_quantity_billions, fill = subsector)) +
  geom_bar(stat = "identity") + 
  labs(title = "China's Manufacturing Sector Emissions by Subsector",
       x = "Year",
       y = "Total Emissions (Billion)",
       fill = "Subsector") +
  scale_fill_manual(values = colors) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5, size = 12),  
        text = element_text(size = 10),  
        axis.text.x = element_text(angle = 45, hjust = 1))
