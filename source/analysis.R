library(tidyverse)
library(dplyr)
library(ggplot2)

# The functions might be useful for A4
source("../source/a4-helpers.R")

## Section 2  ---- 
#----------------------------------------------------------------------------#
incarceration_df <- read.csv("~/documents/info201/data/incarceration_trends.csv")

highest_county <- incarceration_df %>% 
  select(year, total_jail_pop, county_name) %>% 
  drop_na() %>% 
  filter(year == max(year)) %>% 
  filter(total_jail_pop == max(total_jail_pop)) %>% 
  pull(county_name) 

number_incarcerated <- incarceration_df %>% 
  select(year, total_jail_pop, county_name) %>% 
  drop_na() %>% 
  filter(year == max(year)) %>% 
  filter(total_jail_pop == max(total_jail_pop)) %>% 
  pull(total_jail_pop) 

white_incarcerated <- incarceration_df %>% 
  select(year, county_name, total_jail_pop, white_jail_pop) %>% 
  drop_na() %>% 
  filter(year == max(year)) %>% 
  filter(total_jail_pop == max(total_jail_pop)) %>% 
  pull(white_jail_pop)

black_incarcerated <- incarceration_df %>% 
  select(year, total_jail_pop, county_name, black_jail_pop) %>% 
  drop_na() %>% 
  filter(year == max(year)) %>% 
  filter(total_jail_pop == max(total_jail_pop)) %>% 
  pull(black_jail_pop) 

white_black_difference <- black_incarcerated - white_incarcerated

#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population

#This function returns a data frame of jail populations per year
get_year_jail_pop <- function() {
  jail_pop_yr <- incarceration_df %>% 
    select(year, total_jail_pop) %>% 
    drop_na() %>% 
    group_by(year) %>% 
    summarise(total_jail_pop = sum(total_jail_pop)) %>% 
  return(jail_pop_yr)   
}

yr_jail <- get_year_jail_pop()

#This function returns a bar chart plot of the change in jail population
plot_jail_pop_for_us <- function()  {
  yr_pop_jail <- ggplot(yr_jail, aes(x = year, y = total_jail_pop)) +
    geom_bar(stat = "identity") +
    labs(title = "Increase of Jail Population in U.S. (1970-2018)",
           x = "Year",
           y = "Total Jail Population")
  return(yr_pop_jail)
} 

#----------------------------------------------------------------------------#

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 

#This function returns a data frame of jail populations by state

get_jail_pop_by_states <- function(states){
  state_jail <- incarceration_df %>% 
    drop_na() %>% 
    select(state, year, total_jail_pop) 
  states_jail_pop <- state_jail %>% 
    filter(state %in% states) 
  return(states_jail_pop) 
}

state_data <- get_jail_pop_by_states(c("CA", "WA")) 

#This function returns a line graph plot of the change in jail populations by state 
plot_jail_pop_by_states <- function(states) {
  line_plot <- ggplot(get_jail_pop_by_states(states), aes(x=year, y=total_jail_pop, color = state))+
      stat_summary(fun = "mean", geom = "line") + xlab("Year") +
      ylab("Total Jail Population") +
        ggtitle("Increase in Jail Population in U.S. States (1970-2018)")
      return(line_plot)
}

plot_jail_pop_by_states(c("CA", "WA"))

#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# Growth of White and Black Prison Populations

#This function returns a data frame of black and white jail populations 
black_white_jail_pop <- function() {
  black_white_jail_pop_yr <- incarceration_df %>% 
    select(year, black_jail_pop, white_jail_pop) %>% 
    drop_na() %>% 
    group_by(year) %>%
    mutate(white_total_jail_pop = sum(white_jail_pop)) %>% 
    mutate(black_total_jail_pop = sum(black_jail_pop)) %>% 
    return(black_white_jail_pop_yr)   
}

bw_jail_pop <- black_white_jail_pop()

#This function returns a scatter plot of the change in black and white jail populations
plot_black_white_jail_pop <- function(){
  bw_jail_plot <- ggplot(bw_jail_pop) +
  geom_point(aes(x = year, y = white_total_jail_pop, color="black")) +
  geom_point(aes(x = year, y = black_total_jail_pop, color="white")) +
    labs(title = "Growth of White and Black Prison Populations",
         x = "Year",
         y = "Jail Population",
         color = "Race")
  return(bw_jail_plot)
}

plot_black_white_jail_pop()

#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# Population Incarcerated by State

#This function returns a data frame of total jail populations by state in 2018
# state_jail_pop <- function() {
  state_jail_pop_2018 <- incarceration_df %>% 
    select(year, total_jail_pop, state) %>% 
    drop_na() %>% 
    filter(year == "2018") %>% 
    group_by(state) %>% 
    summarize(state, total_jail_pop)
  state_jail_pop_2018 <- aggregate(total_jail_pop~.,state_jail_pop_2018,FUN=sum)
  # return(state_jail_pop_2018)   


#Creates a data set that works with map
state_df <- state_jail_pop()

state_name <- c("alaska", "alabama", "arkansas", "arizona", "california", "colorado",
                "district of columbia", "florida", "georgia", "iowa", "idaho", "illinois",
                "indiana", "kansas", "kentucky", "louisiana", "massachusetts", "maryland", 
                "maine", "michigan", "minnesota", "missouri", "mississippi", "montana", 
                "north carolina", "north dakota", "nebraska", "new hampshire", "new jersey", 
                "new mexico", "nevada", "new york", "ohio", "oklahoma", "oregon", "pennsylvania",
                "south carolina", "south dakota", "tennessee", "texas", "utah", "virginia",
                "washington", "wisconsin", "west virginia", "wyoming")

state_df <- subset(state_df, select = -c(state))
state_df$state = state_name

#This function returns a chloropleth map of total jail populations by state in 2018
state_shape <- map_data("state")
ggplot(state_shape) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group),
    color = "white", size = 0.3) + coord_map() 

state_shape <- map_data("state") %>% 
  rename(state = region) %>% 
  left_join(state_data, by = "state") 

map_graph <- ggplot(state_shape) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = total_jail_pop),
    color = "white", size = 0.3) +
  coord_map() +
  scale_fill_continuous(low = "Blue", high = "Red") + 
  labs(fill = "Jail Population", title = "Jail Population by State", 
       subtitle = "2018")
  )

#----------------------------------------------------------------------------#

## Load data frame ---- 

incarceration_df <- read.csv("~/documents/info201/data/incarceration_trends.csv")
