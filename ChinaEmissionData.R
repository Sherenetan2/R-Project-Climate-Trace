library(dplyr)   
library(readr)
# Setting the path to the folder containing all Excel files
folder_path <- "C:/Users/hasie/Desktop/LKYSPP/SEM3/R programming/group project/data/CHN (1)/DATA/emission"

# List all CSV files in a folder
file_list <- list.files(path = folder_path, pattern = "*.csv", full.names = TRUE)

# Use lapply to read all the CSV files and use bind_rows to merge them into a single dataframe
combined_data <- file_list %>%
  lapply(read_csv) %>%   # Reading files one by one with read_csv
  bind_rows()            # Merge dataframes by rows using bind_rows

# View merged data
print(combined_data)
View(combined_data)

# Save the merged data as a new CSV file
write_csv(combined_data, "combined_data.csv")


datayear <- combined_data %>%
  mutate(year = substr(start_time, 1, 4))  # Extract the year of the start_time column and create a new column

library(dplyr)

# Create new columns to mark the number of population based on the year
datayear <- datayear %>%
  mutate(population = case_when(
    year == 2015 ~ 1379860000,
    year == 2016 ~ 1387790000,
    year == 2017 ~ 1396215000,
    year == 2018 ~ 1402760000,
    year == 2019 ~ 1407745000,
    year == 2020 ~ 1411100000,
    year == 2021 ~ 1412360000,
    year == 2022 ~ 1412175000,
    year == 2023 ~ 1410710000
  ))


# Check the data frame to confirm that the new column has been generated

View(datayear)
# delete the unrelated columns
datayear <- datayear %>%
  select(-start_time, -end_time, -created_date, -modified_date)


# Extract all rows with a value of "co2e_100ye".
data_co2e_100yr <- datayear %>%
  filter( gas == "co2e_100yr")

View(data_co2e_100yr)
# Checks for the presence of 0 or NA in a column
has_zeros_or_na <- any(data_co2e_100yr$gas == 0 | is.na(data_co2e_100yr$gas))

# check the result
if (has_zeros_or_na) {
  print("The column contains 0 or missing values (NA).")
} else {
  print("The column does not contain 0 or missing values (NA).")
}

library(ggplot2)
str(data_co2e_100yr$gas)

# Grouping by year and superimposing values for each year
yearly_data <- data_co2e_100yr %>%
  group_by( year ) %>%
  summarise(total_emission = sum(emissions_quantity, na.rm = TRUE))

yearly_data

# calculate the yearly growth
yearly_data <- yearly_data %>%
  arrange(year) %>%  # Sort by year to ensure correct calculations
  mutate(growth_rate = (total_emission / lag(total_emission) - 1) * 100)
# draw the yearly growth, excluding year 2015 and 2023
yearly_data
ggplot(yearly_data %>% filter(year != 2023, year!=2015), aes(x = year, y = growth_rate, group=1)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title = "Yearly Growth Rate of Total Emission (Excluding 2015 and 2023)",
       x = "Year",
       y = "Growth Rate (%)") +
  theme_minimal()


ggplot(yearly_data, aes(x = year, y = total_emission)) +
  geom_col(fill = "blue") +
  labs(title = "Yearly Trend of Total Emission", x = "Year", y = "Total Emission") +
  theme_minimal()  


# Stacked bar charts showing emissions from different classifications for each year
ggplot(data_co2e_100yr, aes(x = year, y = emissions_quantity, fill = sector)) +
  geom_bar(stat = "identity") +  # Using stacked histograms, identity indicates the use of y-values
  labs(title = "Yearly Emission by Category", x = "Year", y = "Total Emission") +
  theme_minimal() +  
  scale_fill_brewer(palette = "Set3")  

# Creation of a per capita emissions column
data_co2e_100yr <- data_co2e_100yr %>%
  mutate(per_capita_emission = emissions_quantity / population)
# Plotting per capita emissions on a bar chart
ggplot(data_co2e_100yr, aes(x = year, y = per_capita_emission, fill = sector)) + 
  geom_bar(stat = "identity") +
  labs(title = "Per Capita Emission by Year", x = "Year", y = "Per Capita Emission") +
  theme_minimal() +
scale_fill_brewer(palette = "Set3") 

yearly_per_data <- data_co2e_100yr %>%
  group_by( year ) %>%
  summarise(total_emission = sum(emissions_quantity, na.rm = TRUE))
# paste the population to different years
yearly_per_data <- yearly_per_data %>%
  mutate(population = case_when(
    year == 2015 ~ 1379860000,
    year == 2016 ~ 1387790000,
    year == 2017 ~ 1396215000,
    year == 2018 ~ 1402760000,
    year == 2019 ~ 1407745000,
    year == 2020 ~ 1411100000,
    year == 2021 ~ 1412360000,
    year == 2022 ~ 1412175000,
    year == 2023 ~ 1410710000
  ))
# add a new column of emission per population
yearly_per_data <- yearly_per_data %>%
  mutate( per_population = total_emission / population)

# add a new column of emission per population growth
yearly_per_data  <- yearly_per_data %>%
  arrange(year) %>%  # Sort by year to ensure correct calculations
  mutate(per_growth_rate = (per_population / lag(per_population) - 1) * 100)
# draw the yearly per growth, excluding year 2015 and 2023
yearly_data
ggplot(yearly_per_data %>% filter(year != 2023, year!=2015), aes(x = year, y = per_growth_rate, group=1)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title = "Yearly Growth Rate of Per Capita Emissions (Excluding 2015 and 2023)",
       x = "Year",
       y = "Growth Rate (%)") +
  theme_minimal()
