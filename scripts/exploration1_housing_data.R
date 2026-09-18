
# Tell me about the house-I'll explore its selling price 

library(tidyverse)
library(janitor)

FM_housing <- read.csv("data/FM_Housing_2018_2022_clean.csv") |> 
  clean_names()

glimpse(FM_housing)

#dataset dimenssions 

dim(FM_housing)

#EDA 

FM_housing |>
  summarise(across(everything(), ~ sum(is.na(.)))) |>
  pivot_longer(
    everything(),
    names_to = "variable",
    values_to = "missing"
  ) |>
  mutate( 
    percent_missing = missing/nrow(FM_housing) *100) |> 
  arrange(desc(missing)) |> 
  print(n=20)

summary(FM_housing$sold_price)

# visualizing EDA. Here we want to logically explore things a person would want to utilize for predicting 
# the price of a house. ex:city,total_sq_ft,year_built, style, total_bedrooms,Total_full_baths,garage_type,
# gen_tax,lot_size_sq_ft ,garage_stalls,high_school,features?

ggplot(FM_housing,aes(x=sold_price))+
  geom_histogram(bins =50)+
  coord_cartesian(xlim = c(0, 1000000)) +
  labs(
    title = "sold_price distribution",
    x="Sold price ($)",
    y="Number of properties"
  )+
  theme_minimal()

# total_sq_ft
ggplot(FM_housing, aes(x=total_sq_ft, y=sold_price))+
         geom_point(color= "dark blue",alpha=.2)+
         labs(
           title = "sold_price vs total square footage",
           x="total square footage",
           y ="sold price ($)"
         )+
         theme_minimal()

#bedrooms 
ggplot(FM_housing, aes(
  x = factor(total_bedrooms),
  y = sold_price
)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 1000000)) +
  labs(
    title = "Sold Price by Number of Bedrooms",
    x = "Number of Bedrooms",
    y = "Sold Price ($)"
  ) +
  theme_minimal()

#most houses fall within 2-5 bedrooms 
FM_housing |>
  count(total_bedrooms) |>
  arrange(total_bedrooms)
  
# Number of garages vs sold price 

ggplot(FM_housing, aes(
  x = factor(garage_stalls),
  y = sold_price
)) +
  geom_boxplot() +
  labs(
    title = "Sold Price by Garage Stalls",
    x = "Garage Stalls",
    y = "Sold Price ($)"
  ) +
  theme_minimal()
       
FM_housing |>
  count(garage_stalls) |>
  arrange(garage_stalls)

FM_housing |>
  filter(garage_stalls == 21) |>
  select(
    sold_price,
    property_type,
    book_section,
    city,
    total_sq_ft,
    total_bedrooms,
    total_bathrooms,
    garage_stalls,
    lot_size_sq_ft
  )

# Potential data entry error:
# One 1,560 sq-ft residential property is recorded as having
# 21 garage stalls. Investigate/handle before modeling. As such I will mutate 

#Bathroom vs sold price 

ggplot(FM_housing, aes(
  x = factor(total_bathrooms),
  y = sold_price
)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 2000000)) +
  labs(
    title = "Sold Price by total bathrooms",
    x = "Bathrooms",
    y = "Sold Price ($)"
  ) +
  theme_minimal()

FM_housing |>
  count(total_bathrooms) |>
  arrange(total_bathrooms)

#year built vs sold price 

ggplot( FM_housing, aes(
  x=year_built,
  y=sold_price))+
    geom_point( color="dark blue", alpha = 0.2)+
  labs(
  title = "Sold Price vs. Year Built",
x = "Year Built",
y = "Sold Price ($)"
) +
  theme_minimal()

FM_housing |>
count(year_built) |>
  arrange(year_built) |>
  head(30)
    
summary(FM_housing$year_built)


ggplot( FM_housing,aes(
  x=(year_built),
  y=sold_price))+
  geom_point( color="darkblue", alpha = 0.2)+
  coord_cartesian(xlim = c(1800, 2022))+
  labs(
    title = "Sold Price vs. Year Built",
    x = "Year Built",
    y = "Sold Price ($)"
  ) +
  theme_minimal()

FM_housing |>
  filter(year_built < 1800 | year_built > 2022) |>
  count(year_built) |>
  arrange(year_built)

# Highschool vs sold price 

FM_housing |>
  count(high_school, sort = TRUE)

ggplot(FM_housing, aes(
  x = high_school,
  y = sold_price
)) +
  geom_boxplot() +
  coord_flip(ylim = c(0, 1000000))
  labs(
    title = "Sold Price by highschool",
    x = "highschool",
    y = "Sold Price ($)"
  ) +
  theme_minimal()
  
  # City
  FM_housing |>
    count(city, sort = TRUE)
  ggplot(FM_housing, aes(
    x = city,
    y = sold_price
  )) +
    geom_boxplot() +
    coord_cartesian(ylim = c(0, 1000000)) +
  labs(
    title = "Sold Price by city",
    x = "city",
    y = "Sold Price ($)"
  ) +
    theme_minimal()
  
  #Sold prices vary somewhat by city, with West Fargo showing a higher median sold price and Moorhead a lower median.
  #However, substantial overlap exists among the three city distributions, suggesting that city alone does not strongly distinguish property prices.
  
  
  #sold_price is right-skewed, several house characteristics appear predictive, location matters to some degree, 
  #and we've found concrete data-quality issues such as garage_stalls = 21 and impossible year_built values. 
  #We also identified sold_price_per_sq_ft as target leakage. 