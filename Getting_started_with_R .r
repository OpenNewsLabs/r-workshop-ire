
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
    # list of all the column names we want to keep
    Id2, Geography, Number..Total.population, 
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

census2000.trimmed$population <- str_replace(census2000.trimmed$population, pattern = ',', replacement = '')

View(head(census2000.trimmed))

census2000.trimmed$population <- as.numeric(census2000.trimmed$population)

str(census2000.trimmed)

census2000.trimmed$total.housing.units <- as.numeric(str_replace(census2000.trimmed$total.housing.units, pattern = ',', replacement = ''))
census2000.trimmed$occupied.housing.units <- as.numeric(str_replace(census2000.trimmed$occupied.housing.units, pattern = ',', replacement = ''))
census2000.trimmed$vacant.housing.units <- as.numeric(str_replace(census2000.trimmed$vacant.housing.units, pattern = ',', replacement = ''))

str(census2000.trimmed)

View(head(census2000.trimmed, 10))

# install.packages('tidyr')

library('tidyr')

# ?separate()

census2000.trimmed <- separate(
    census2000.trimmed,
    geography,
    c('tract', 'parish', 'state'),
    ', '
)

View(head(census2000.trimmed))

table(census2000.trimmed$parish)

orleans2000 <- filter(census2000.trimmed, parish == 'Orleans Parish')
head(orleans2000)

sum(orleans2000$population)

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

sum(orleans2010$population)

nola2000pop <- sum(orleans2000$population)
nola2010pop <- sum(orleans2010$population)

perc.change.nola <- (nola2010pop - nola2000pop)/nola2000pop * 100

print(paste('The percent change in New Orleans population since 2000 is ', round(perc.change.nola), '%', sep =''))

census.comparison <- merge(
    census2000.trimmed,
    census2010.trimmed, 
    by = c('fips.code', 'tract', 'parish', 'state'), 
    suffixes = c('.00', '.10'), 
    all = TRUE
)

View(head(census.comparison))

write.csv(census.comparison, 'census_comparison.csv', row.names = FALSE)
