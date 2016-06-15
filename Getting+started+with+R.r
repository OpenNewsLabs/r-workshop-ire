# Link to notebook with annotations: 
# https://github.com/OpenNewsLabs/r-workshop-ire/blob/master/Getting%20started%20with%20R.ipynb

# Before starting, ensure this .r file and the .csv data files are in the same folder. 
# We then need to tell R that our files are saved in the same location.
# In order to do this, you should click through the following:
# "Session --> Set Working Directory --> To Source File Location"

census2000 <- read.csv('2000_census_demographic_profile.csv')

View(head(census2000))

census2000 <- read.csv('2000_census_demographic_profile.csv', skip = 1)

View(head(census2000))

## if dplyr was not installed we would have to run this
# install.packages('dplyr')

## to import the package and all of its functions 
library('dplyr')

census2000.trimmed <- select(
    census2000, # name of the data frame
    # list of all the six column names we want to keep
    Id2, 
    Geography, 
    Number..Total.population, 
    Number..HOUSING.OCCUPANCY...Total.housing.units, 
    Number..HOUSING.OCCUPANCY...Total.housing.units...Occupied.housing.units, 
    Number..HOUSING.OCCUPANCY...Total.housing.units...Vacant.housing.units
)

View(head(census2000.trimmed))

colnames(census2000.trimmed) <- c(
    'fips.code', 'geography', 'population',
    'total.housing.units', 'occupied.housing.units', 'vacant.housing.units'
)

View(head(census2000.trimmed))

str(census2000.trimmed)

# install.packages('stringr')

library('stringr')

census2000.trimmed$population <- str_replace(
    census2000.trimmed$population, 
    pattern = ',', 
    replacement = ''
)

View(head(census2000.trimmed))

census2000.trimmed$population <- as.numeric(census2000.trimmed$population)

str(census2000.trimmed)

census2000.trimmed$total.housing.units <- as.numeric(str_replace(census2000.trimmed$total.housing.units, pattern = ',', replacement = ''))
census2000.trimmed$occupied.housing.units <- as.numeric(str_replace(census2000.trimmed$occupied.housing.units, pattern = ',', replacement = ''))
census2000.trimmed$vacant.housing.units <- as.numeric(str_replace(census2000.trimmed$vacant.housing.units, pattern = ',', replacement = ''))

str(census2000.trimmed)

View(head(census2000.trimmed, n = 10))

# install.packages('tidyr')

library('tidyr')

?separate()

census2000.trimmed <- separate(
    census2000.trimmed, # name of the data frame
    geography, # column to split
    c('tract', 'parish', 'state'), # new column names
    ', ' # delimiter to split on (note the space after the comma)
)

View(head(census2000.trimmed))

table(census2000.trimmed$parish)

census2010 <- read.csv('2010_census_demographic_profile.csv', skip = 1)

census2010.trimmed <- select(
  census2010, # name of the data frame
  # list of all the column names we want to keep
  Id2, Geography, Number..SEX.AND.AGE...Total.population, 
  Number..HOUSING.OCCUPANCY...Total.housing.units, 
  Number..HOUSING.OCCUPANCY...Total.housing.units...Occupied.housing.units, 
  Number..HOUSING.OCCUPANCY...Total.housing.units...Vacant.housing.units
)

colnames(census2010.trimmed) <- c('fips.code', 'census.tract', 'population', 
                               'total.housing.units', 'occupied.housing.units', 'vacant.housing.units')

census2010.trimmed$population <- as.numeric(str_replace(census2010.trimmed$population, pattern = ',', replacement = ''))
census2010.trimmed$total.housing.units <- as.numeric(str_replace(census2010.trimmed$total.housing.units, pattern = ',', replacement = ''))
census2010.trimmed$occupied.housing.units <- as.numeric(str_replace(census2010.trimmed$occupied.housing.units, pattern = ',', replacement = ''))
census2010.trimmed$vacant.housing.units <- as.numeric(str_replace(census2010.trimmed$vacant.housing.units, pattern = ',', replacement = ''))

census2010.trimmed <- separate(census2010.trimmed, census.tract, c('tract', 'parish', 'state'), ', ')

orleans2010 <- filter(census2010.trimmed, parish == 'Orleans Parish')

census.comparison <- merge(
    census2000.trimmed, # first data frame
    census2010.trimmed, # second data frame
    by = c('fips.code', 'tract', 'parish', 'state'), # keys to use for join
    suffixes = c('.00', '.10'), # suffixes to append to new columns
    all = TRUE # specifying to keep all data from both data frames
)

View(census.comparison[65:69, ])

write.csv(census.comparison, 'census_comparison_result.csv', row.names = FALSE)

# note the use of "==" since we are expressing a criterion
orleans <- filter(census.comparison, parish == 'Orleans Parish') 
View(head(orleans))

sum(orleans$population.00)

summary(orleans$population.00)

sum(orleans$population.00, na.rm = TRUE)

sum(orleans$population.10, na.rm = TRUE)

nola2000pop <- sum(orleans$population.00, na.rm = TRUE)
nola2010pop <- sum(orleans$population.10, na.rm = TRUE)

perc.change.nola <- (nola2010pop - nola2000pop)/nola2000pop * 100

print(paste('The percent change in New Orleans population since 2000 is ', round(perc.change.nola), '%', sep =''))
