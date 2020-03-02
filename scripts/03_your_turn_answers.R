library(tidyverse)
library(leaflet)
library(sf)

# If you wanted to use the tigris package to download county files yourself
# library(tigris)
# options(tigris_class = "sf")
# lamsal <- counties(state=c("LA", "MS", "AL", cb=T))

# Otherwise, we'll load it off our local computer

lamsal <- st_read("shapefiles/LAMSAL.shp")

# This is what it looks like by itself
leaflet(lamsal) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data=lamsal, fillOpacity = 1, 
              weight = 0.9, 
              smoothFactor = 0.2, 
              fill=FALSE,
              stroke=TRUE,
              color="black")

# We're going to use the Waffle House data from before
# This time, we're going to create a new dataframe by
# counting the number of stores per county GEOID
# and then join that data frame to lamsal

# Here's the code to start out
county_count <- sheet %>% 
  group_by(GEOID) %>% 
  count(GEOID, name="WH", sort=T) %>% 
  filter(!is.na(GEOID))


#### Your turn: Join and visualize the data -------------

# Tip: line 52 or 112 in 03_leaflet_choropleth.R will can be used to join

counties_count <- left_join(lamsal, county_count)


# And here's the color palette
pal <- colorNumeric("Oranges", domain=counties_count$WH)

# Set up the popup text so it says "County, State: # of WH"
# Refer to line 63 in 03_leaflet_choropleth.R

popup_wh <- paste0(counties_count$NAMELSAD, ": ", 
                   counties_count$WH, " Waffle Houses")

  
#### Generating a leaflet map with variables for style -----
## No zoom or setView needed this time, I think
## Use addProviderTiles("CartoDB.Positron)

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = counties_count , 
              fillColor = ~pal(counties_count$WH), 
              fillOpacity = 1, 
              weight = 0.9, 
              smoothFactor = 0.2, 
              stroke=TRUE,
              color="white",
              popup = ~popup_wh) %>%
  addLegend(pal = pal, 
            values = counties_count$WH, 
            position = "bottomright", 
            title = "Waffle Houses")


counties_count <- counties_count %>% 
  filter(!is.na(WH))


leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = counties_count , 
              fillColor = ~pal(counties_count$WH), 
              fillOpacity = 1, 
              weight = 0.9, 
              smoothFactor = 0.2, 
              stroke=TRUE,
              color="white",
              popup = ~popup_wh) %>%
  addPolygons(data=lamsal, fillOpacity = .4, 
              weight = 0.3, 
              smoothFactor = 0.2, 
              fill=FALSE,
              stroke=TRUE,
              color="black") %>% 
  addLegend(pal = pal, 
            values = counties_count$WH, 
            position = "bottomright", 
            title = "Waffle Houses")

## Can you figure out how to calculate the percent 
## of Waffle Houses not open per county and map it?
