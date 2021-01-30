reflection-milestone3
================
Deepak Sidhu, Nicholas Wu, William Xu, Zeliha Ural Merpez

# Reflection

## What We Have Implemented

We have developed the dashboard according to our proposal set in
Milestone 1. The “World Happiness Ranking” plot shows the
happiness index on a world map. A user can hover over to see the actual 
happiness index of each country. The dashboard has two drop down menus 
at the top that the user can make selections for regions and preferences that 
he/she wishes to explore. Once the user makes choices from the drop down menus, 
three plots will get updated. The bar chart on the right ranks countries within
the region by user preference. 

The error bar chart at the bottom left section ranks countries within the region
by their happiness scores and shows the lower and upper bound of the scores. 
The bar chart next to it shows the population density ranking within the region. 
A user can hover over to see the exact scores and densities, or click on the 
plot to highlight any country to compare its ranking in both happiness score and 
population density at the same time. 

The last section in our dashboard is a widget that contains another drop down 
menu and a data table. It allows the user to select multiple countries and 
returns the exact scores on the preference that user is interested in. 

## What Is Not Yet Implemented

Due to time constraint, we left out two pie charts that help the user
visualize the proportion between population and migrants of each
country. We also hope to improve the data table widget to lists out more country 
specific details such as cost of living index, purchasing power, groceries index
and rent index.

We also wish to improve the interactivity between the happiness score and 
population density plots so a user can select multiple countries to make 
comparison among countries. 

## Thoughts on Implementing Dashboard in R

We enjoyed implementing dashboard in R, especially using ggplot for plotting. 
However, with the lack of decorator function in R, it requires more time for 
de-bugging while implementing dashboard in R. 

## Changes Made Based on Feedback Received 

Based on Joel's feedback, we updated the color scheme used in the dashboard to 
make it look more consistent. 
