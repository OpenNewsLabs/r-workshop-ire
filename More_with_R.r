
census <- read.csv('census_comparison.csv')
head(census)

## if dplyr was not installed we would have to run this
# install.packages('dplyr')

## to import the package and all of its functions 
library('dplyr')

orleans <- census %>% filter(parish == 'Orleans Parish') # note the use of the pipe, %>%
str(orleans)

sum(orleans$population.00)

summary(orleans$population.00)

sum(orleans$population.00, na.rm = TRUE)

plot(orleans$population.00, orleans$population.10)

plot(
    orleans$population.00,
    orleans$population.10, 
    xlim = c(0, 8000), 
    ylim = c(0, 8000),
    xlab = '2000 population',
    ylab = '2010 population',
    main = 'Census Tracts in Orleans Parish'
)

abline(0, 1)

orleans$pop.diff <- orleans$population.10 - orleans$population.00

pop.drops.orleans <- sum(orleans$pop.diff < 0, na.rm = TRUE)

print(paste('In New Orleans,', pop.drops.orleans, 'tracts had fewer people in 2010 than in 2000.'))

parishes <- census %>% 
group_by(parish) %>%  # this tells R to group our data by parishes
summarise_each( 
    # sum all the columns 
    funs(sum(., na.rm = TRUE)),
    # except the non-numerical ones
    -fips.code, -tract, -state
) 

parishes$perc.pop.diff <- (parishes$population.10 - parishes$population.00) / parishes$population.00 * 100

head(parishes)

parishes %>% 
select(parish, population.00, population.10, perc.pop.diff) %>% # select the columns of interest
arrange(perc.pop.diff) %>%
head()

parishes$perc.occupied.00 <- parishes$occupied.housing.units.00 / parishes$total.housing.units.00 * 100
parishes$perc.occupied.10 <- parishes$occupied.housing.units.10 / parishes$total.housing.units.10 * 100
parishes$perc.occupied.diff <- parishes$perc.occupied.10 - parishes$perc.occupied.00

parish.occupancy.rates <- parishes %>% 
select(parish, perc.occupied.00, perc.occupied.10, perc.occupied.diff) %>% 
arrange(desc(perc.occupied.diff)) # arrange(desc()) arranges in descending order

head(parish.occupancy.rates, n = 3)

tail(parish.occupancy.rates, n = 3)

hist(parish.occupancy.rates$perc.occupied.diff)

hist(parish.occupancy.rates$perc.occupied.diff, breaks = 20, main = 'Difference in occupancy rates, by parish',
    xlab = 'Percent point change in occupancy between 2000 and 2010', 
    ylab = 'Number of parishes'
    )
abline(v = 0, lwd = 5)

names(parishes)

plot(parishes$population.10, parishes$perc.occupied.10)
abline(lm(parishes$perc.occupied.10 ~ parishes$population.10))

curved.model <- lm(parishes$perc.occupied.10 ~ sqrt(parishes$population.10))

predicted.values <- predict(curved.model)

plot(parishes$population.10, parishes$perc.occupied.10)
lines(sort(parishes$population.10), sort(predicted.values))
