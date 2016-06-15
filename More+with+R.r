# Link to notebook with annotations: 
# https://github.com/OpenNewsLabs/r-workshop-ire/blob/master/More%20with%20R.ipynb

# Before starting, ensure this .r file and the .csv data files are in the same folder. 
# We then need to tell R that our files are saved in the same location.
# In order to do this, you should click through the following:
# "Session --> Set Working Directory --> To Source File Location"

census <- read.csv('census_comparison.csv')
View(head(census))

## if dplyr was not installed we would have to run this
# install.packages('dplyr')

## to import the package and all of its functions 
library('dplyr')

parishes <- census %>% # notice the use of "%>%"
group_by(parish) %>%  # this tells R to group our data by parishes
summarise_each( 
    # sum all the columns 
    funs(sum(., na.rm = TRUE)),
    # except the non-numerical ones
    -fips.code, -tract, -state
) 

parishes$perc.pop.diff <- (parishes$population.10 - parishes$population.00) / parishes$population.00 * 100

View(head(parishes))

parishes %>% 
select(parish, population.00, population.10, perc.pop.diff) %>% # select the columns of interest
arrange(perc.pop.diff) %>%
head()

orleans <- census %>% filter(parish == 'Orleans Parish')
orleans %>% str() # this is equivalent to str(orleans)

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

abline(0, 1) # Draw a line with y-intercept of 0 and slope of 1

orleans$pop.diff <- orleans$population.10 - orleans$population.00

pop.drops.orleans <- sum(orleans$pop.diff < 0, na.rm = TRUE)

print(paste('In New Orleans,', pop.drops.orleans, 'tracts had fewer people in 2010 than in 2000.'))

parishes$perc.occupied.00 <- parishes$occupied.housing.units.00 / parishes$total.housing.units.00 * 100
parishes$perc.occupied.10 <- parishes$occupied.housing.units.10 / parishes$total.housing.units.10 * 100
parishes$perc.occupied.diff <- parishes$perc.occupied.10 - parishes$perc.occupied.00

parish.occupancy.rates <- parishes %>% 
select(parish, perc.occupied.00, perc.occupied.10, perc.occupied.diff) %>% 
arrange(desc(perc.occupied.diff)) # arrange(desc()) arranges in descending order

head(parish.occupancy.rates, n = 3)

tail(parish.occupancy.rates, n = 3)

hist(parish.occupancy.rates$perc.occupied.diff)

hist(
    parish.occupancy.rates$perc.occupied.diff, 
    breaks = 20, 
    main = 'Difference in occupancy rates, by parish',
    xlab = 'Percent point change in occupancy between 2000 and 2010', 
    ylab = 'Number of parishes'
)
abline(v = 0, lwd = 5) # draw a vertical line at 0 with width of 5 pixels

names(parishes)

plot(parishes$population.10, parishes$perc.occupied.10)
abline(lm(parishes$perc.occupied.10 ~ parishes$population.10))

curved.model <- lm(parishes$perc.occupied.10 ~ log(parishes$population.10))

predicted.values <- predict(curved.model)

plot(parishes$population.10, parishes$perc.occupied.10)
lines(sort(parishes$population.10), sort(predicted.values))

options(scipen=5) # to prevent axes from appearing in scientific notation
plot(
    parishes$population.10, parishes$perc.occupied.10, type = "n", # type = "n" tells R not to plot the points
    xlab = "Population",
    ylab = "Occupancy rate (%)",
    main = "Louisiana parishes in 2010"
)
text(parishes$population.10, parishes$perc.occupied.10, labels = parishes$parish, cex = 0.6) # adding text where the points should be
lines(sort(parishes$population.10), sort(predicted.values), col = "red", lwd = 2)
