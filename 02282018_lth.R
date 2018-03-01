## check out this: https://www.tidyverse.org/

library(dplyr)
gapminder <- read.csv("data/gapminder-FiveYearData.csv", stringsAsFactors = TRUE)
View(gapminder)
head(gapminder)
tail(gapminder)
head(gapminder,n=10)
mean(gapminder[gapminder$continent == "Africa", "gdpPercap"])
# the convention in tidyverse is that the first input variable is the data
year_country_gdp <- select(gapminder, year, country, gdpPercap)
# it seems that there's no a straightforward way to autocomplete the column names
# but it seems that there're some third party functions to do this
head(year_country_gdp)
# do same as above, but with base R
same_objects <- gapminder[,c("year","country","gdpPercap")]
# do same as above, but with dplyr piping
year_country_gdp <- gapminder %>% select(year,country,gdpPercap)
# shift+cmd+M will pull up %>%
# another shortcut: alt+shift+K
year_country_gdp_africa <- gapminder %>%
  filter(continent == "Africa") %>% 
  select(year,country,gdpPercap)
  

group_membership <- gapminder %>% filter(country %in% c("Cambodia","China"))
head(group_membership)  
View(group_membership)

# Create a new dataframe object called africa_asia_lifeExp 
# that is filtered by “Africa” and “Asia” and has a lifeExp less than 60*. 
# Select all variables except for gdpPercap. 
# Use the pipe operator to chain the functions together.
africa_asia_lifeExp <- gapminder %>% filter(continent %in% c("Asia","Africa")) %>% 
  filter(lifeExp < 60) %>% 
  select(-gdpPercap)

# gourp_by
# useful for split-apply-combine
gapminder %>% group_by(continent)


gdp_bycontinents <- gapminder %>%
  group_by(continent,year) %>%
  # summarize(mean(gdpPercap)) %>% 
  # if don't supply a new name (like the line below), 
  # then "mean(gdpPercap)" will be the new column name
  summarize(mean_gdpPercap = mean(gdpPercap)) 
View(gdp_bycontinents)

# Create a data frame containing the median lifeExp for each country
median_country_lifeExp <- gapminder %>% group_by(country) %>% 
  summarize(median_lifeExp = median(lifeExp))

# What if we wanted to add these values to our original data frame 
# instead of creating a new object?
gapminder_with_extra_vars <- gapminder %>%
  group_by(continent, year) %>%
  mutate(mean_gdpPercap = mean(gdpPercap),
         sd_gdpPercap = sd(gdpPercap),
         mean_pop = mean(pop),
         sd_pop = sd(pop))
head(gapminder_with_extra_vars)
head(gapminder)

# We can use also use mutate() to create new variables prior to 
# (or even after) summarizing information.
gdp_pop_bycontinents_byyear <- gapminder %>%
  mutate(gdp_billion = gdpPercap*pop/10^9) %>%
  group_by(continent, year) %>%
  summarize(mean_gdpPercap = mean(gdpPercap),
            sd_gdpPercap = sd(gdpPercap),
            mean_pop = mean(pop),
            sd_pop = sd(pop),
            mean_gdp_billion = mean(gdp_billion),
            sd_gdp_billion = sd(gdp_billion))
head(gdp_pop_bycontinents_byyear)

# arraange(). default is ascending
gapminder_with_extra_vars <- gapminder %>%
  group_by(continent, year) %>%
  mutate(mean_gdpPercap = mean(gdpPercap),
         sd_gdpPercap = sd(gdpPercap),
         mean_pop = mean(pop),
         sd_pop = sd(pop)) %>%
  arrange(desc(year),continent)
View(gapminder_with_extra_vars)

# Use dplyr to add a column to the gapminder dataset that 
# contains the total population of the continent of each observation 
# in a given year. For example, if the first observation is Afghanistan 
# in 1952, the new column would contain the population of Asia in 1952.

gapminder_challenge1 <- gapminder %>% group_by(continent,year) %>% mutate(sum(pop))
View(gapminder_challenge)

# Use dplyr to: (a) add a column called gdpPercap_diff that contains 
# the difference between the observation’s gdpPercap and the mean gdpPercap
# of the continent in that year, (b) arrange the dataframe by the column 
# you just created, in descending order (so that the relatively richest 
# country/years are listed first)
gapminder <- gapminder %>%  group_by(continent, year) %>% 
  mutate(mean_continent_gdp = mean(gdpPercap),
         gdpPercap_diff = gdpPercap - mean_continent_gdp) %>% 
  arrange(desc(gdpPercap_diff))
