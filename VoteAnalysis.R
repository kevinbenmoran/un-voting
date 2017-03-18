setwd("C:/Users/Kevin/Desktop/GitHub/UNGenVotingData")

library(dplyr)
library(countrycode)
library(purrr)
library(ggplot2)



load("UNVotesPublished.RData")
raw_data <- x
rm(x)

glimpse(raw_data)

# Select columns that are of initial interest.
# Create column with country name
voting_data <- mutate(raw_data, country = countrycode(ccode, "cown", "country.name")) %>%
  select(rcid, vote, country, year, me:ec)

voting_data_present <- filter(voting_data, vote %in% c(1,2,3))

by_country <- group_by(voting_data_present, country)
summary_by_country <- summarise(by_country, 
          total.votes     = n(), 
          yes_percent     = mean(vote == 1),
          no_percent      = mean(vote == 3),
          abstain_percent = mean(vote == 2))

arrange(summary_by_country, desc(no_percent))
arrange(summary_by_country, desc(yes_percent))
arrange(summary_by_country, desc(abstain_percent))

scouncil <- c("United States of America", 
         "United Kingdom of Great Britain and Northern Ireland", 
         "Russian Federation", 
         "China", 
         "France")

scouncil_by_country <- filter(by_country, country %in% scouncil) 
summarise(scouncil_by_country, total_votes = n(), percent_no = mean(vote == 3))

us_by_year <- voting_data_present %>%
  filter(country == "United States of America") %>%
  group_by(year) %>%
  summarise(percent_yes = mean(vote == 1))

scouncil_by_year <- voting_data_present %>%
  filter(country %in% scouncil) %>%
  group_by(country, year) %>%
  summarise(percent_yes = mean(vote == 1))
  
us_by_year
scouncil_by_year

ggplot(us_by_year, aes(x = year, y = percent_yes)) +
  geom_line() +
  geom_smooth(method = 'lm', formula = y ~ x)

# Plot on one graph seperated by color
ggplot(scouncil_by_year, aes(x = year, y = percent_yes, color = country)) +
  geom_line()

# Plot on multiple facets
ggplot(scouncil_by_year, aes(x = year, y = percent_yes)) +
  geom_line() +
  facet_wrap(~country)

