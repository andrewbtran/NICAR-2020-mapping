#### Setup --------------

# This function checks if you don't have the correct packages installed yet
# If not, it will install it for you
packages <- c("sf", "tidyverse", "tigris",
              "arcos", "mapdeck")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())), repos = "http://cran.us.r-project.org")  
}
library(sf)
library(tidyverse)
library(tigris)
library(arcos)

#### Importing in a shapefile -----

counties <- st_read("shapefiles/us_counties.shp")


#### Loading mapdeck -------

library(mapdeck)

mb_key <- "PutYourKeyHere"

#### Quick plotting -------

mapdeck(token = mb_key, 
        style = mapdeck_style("light"),
        zoom=10) %>%
  add_polygon(
    data =counties,
    fill_opacity=0.1,
    stroke_colour="black",
    stroke_width=1
  )

#### Import data we want to map ----------------
annual_summary <- read_csv("data/county_pill_summary.csv")

#### Join and clean the data --------------------

counties_merged_annual <- left_join(counties, annual_summary, by=c("GEOID"="countyfips"))

counties_merged_annual <- counties_merged_annual %>% 
  filter(!is.na(avg_pills_per_person))

# Setting up the popup text
counties_merged_annual$popup <- paste0("<strong>",counties_merged_annual$BUYER_COUNTY, "</strong><br />", counties_merged_annual$avg_pills_per_person, " pills per person per year")

#### Choropleth map -------------------

mapdeck(token = mb_key, 
        style = mapdeck_style("light"),
        zoom=2,
        location=c(-98.294108,39.628777))%>% 
  add_polygon(
    data = counties_merged_annual,
    fill_colour = "avg_pills_per_person",
    fill_opacity = .9,
    auto_highlight = TRUE,
    palette = "inferno",
    tooltip = "popup",
    update_view = FALSE
  )

#### Elevation map -------------------

#We'll need to boost the numbers a bit so the elevation can be seen zoomed out
counties_merged_annual$elevation <- counties_merged_annual$avg_pills_per_person^2.4

mapdeck(token = mb_key, 
    style = mapdeck_style("light"),
    zoom=2,
    location=c(-98.294108,39.628777),
    pitch = 45 
  ) %>% 
  add_polygon(
    data = counties_merged_annual,
    fill_colour = "avg_pills_per_person",
    fill_opacity = .9,
    auto_highlight = TRUE,
    palette = "inferno",
    tooltip = "popup",
    update_view = FALSE,
    elevation = "elevation"
)

#### Bring in pharmacy data ---------

la_pharmacies <- read_csv("data/la_pharmacies.csv")

glimpse(la_pharmacies)

#### Dot map --------------

mapdeck(token = mb_key, style = mapdeck_style("light"),
        zoom=6,
        location=c(-92.485530,31.335469)) %>% 
  add_scatterplot(
    data=la_pharmacies,
    radius=10,
    fill_opacity=.3,
    update_view= FALSE
  )


#### Hex grid map --------------
mapdeck(token = mb_key, style = mapdeck_style("light"),
        zoom=7,
        location=c(-92.485530,31.335469)) %>% 
  add_hexagon(
    data=la_pharmacies,
    radius=4000,
    update_view= FALSE,
    legend = TRUE,
    legend_options=list(title="Pharmacies"),
    #elevation_scale = 100,
    colour_range = colourvalues::colour_values(6:1, palette = colourvalues::get_palette("viridis")[70:256,])
  )

#### Square grid map -------------
mapdeck(token = mb_key, style = mapdeck_style("light"),
        zoom=7,
        location=c(-92.485530,31.335469)) %>% 
  add_screengrid(
    data=la_pharmacies,
    
    update_view= FALSE,
    cell_size = 10,
    opacity=.5,
    colour_range = colourvalues::colour_values(6:1, palette = colourvalues::get_palette("viridis")[70:256,])
  )

#### Add labels ----------------
location <- data.frame(
  txt="NICAR 2020",
  lon=-90.067511,
  lat=29.952622
)

mapdeck(token = mb_key, style = mapdeck_style("light"),
        zoom=12,
        location=c(-90.067511, 29.952622)) %>% 
  add_screengrid(
    data=la_pharmacies,
    cell_size = 10,
    opacity=.5,
    colour_range = colourvalues::colour_values(6:1, 
                                               palette = colourvalues::get_palette("viridis")[70:256,])
  ) %>% 
  add_text(
    data=location,
    lon = 'lon',
    lat = 'lat',
    fill_colour = 'red',
    text = 'txt',
    size = 16,
    update_view= FALSE
    
  )