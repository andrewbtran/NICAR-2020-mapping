
#### Setup --------------

# This function checks if you don't have the correct packages installed yet
# If not, it will install it for you
packages <- c("dplyr", "httpuv", "leaflet", "sf", "here")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())), repos = "http://cran.us.r-project.org")  
}

library(leaflet)
library(dplyr)

#### Putting a marker on a map --------------

# Grab a latitude and longitude from a Google Maps location
# Insert your latitude and longitude in the code below
# NOTE: Don't get them reversed otherwise you'll end up in the South Pole.

# Initialize and assign m as the leaflet object
m <- leaflet() %>%
  # Now add tiles to it
  addTiles() %>%  
  # Setting the middle of where the map should be and the zoom level
  setView(lng=-77.030137, lat=38.902986, zoom = 16) %>%
  # Now, add a marker with a popup, 
  addMarkers(lng=-77.030137, lat=38.902986, popup="<b>Hello</b><br><a href='https://www.washingtonpost.com'>-Me</a>")

m 



#### Multiple locations from a csv --------------

library(readr)

pharmacies <- read_csv("data/all_pharmacies_summarized.csv")

glimpse(pharmacies)

# Pick a state, any state.
# I'll use Louisiana here because that's where NICAR is this year

pharm_state <- pharmacies %>% 
  filter(BUYER_STATE=="LA")


m <- leaflet(pharm_state) %>% 
  addProviderTiles(providers$CartoDB.DarkMatter) %>% 
  setView(-92.469698, 31.012156, zoom = 7) %>% 
  addCircleMarkers(~lon, ~lat, 
                   popup=paste0(pharm_state$BUYER_NAME, "<br />",
                                pharm_state$per_person, " pills per person per year"),
                   weight = 3,
                   radius= 3, 
                   color="#ffa500", 
                   stroke = FALSE, 
                   fillOpacity = 0.3) 
m


#### Adjust size of circles based on data -------


# we also have to rearrange the order of the dataframe
# so the smaller values will render over the larger ones
# this means big circles won't block the small circles

pharm_state <- pharm_state %>% 
  arrange(desc(per_person))

m <- leaflet(pharm_state) %>% 
  addProviderTiles(providers$CartoDB.DarkMatter) %>% 
  setView(-92.469698, 31.012156, zoom = 7) %>% 
  addCircleMarkers(~lon, ~lat, 
                   popup=paste0(pharm_state$BUYER_NAME, "<br />",
                                pharm_state$per_person, " pills per person per year"),
                   weight = 3,
                   radius=sqrt(pharm_state$per_person)*3, 
                   color="#ffa500", 
                   stroke = FALSE, 
                   fillOpacity = 0.3) 
m


#### New dots on a map -----

# creating a color palette based on categories

cof <- colorFactor(c("#ffa500", "#13ED3F"), domain=c("CHAIN PHARMACY", "RETAIL PHARMACY"))

# mapping based on type
m <- leaflet(pharm_state) %>% 
  addProviderTiles(providers$CartoDB.DarkMatter) %>% 
  setView(-92.469698, 31.012156, zoom = 7) %>% 
  addCircleMarkers(~lon, ~lat,
                   popup=paste0(pharm_state$BUYER_NAME, "<br />",
                                pharm_state$per_person, " pills per person per year"),
                   weight = 3,
                   radius=sqrt(pharm_state$per_person)*3, 
                   stroke = FALSE, 
                   fillOpacity = 0.3,
                   # this line below is really the only thing that's different
                   color=~cof(BUYER_BUS_ACT)) 
m

#### Add a legend -----

m <- leaflet(pharm_state)  %>% 
  addProviderTiles(providers$CartoDB.DarkMatter) %>% 
  setView(-92.469698, 31.012156, zoom = 7) %>% 
  addCircleMarkers(~lon, ~lat,
                   popup=paste0(pharm_state$BUYER_NAME, "<br />",
                                pharm_state$per_person, " pills per person per year"),
                   weight = 3,
                   radius=sqrt(pharm_state$per_person)*3, 
                   stroke = FALSE, 
                   fillOpacity = 0.3,
                   color=~cof(BUYER_BUS_ACT))   %>%
  # this line below is really the only thing that's different
  addLegend("bottomright", 
            colors= c("#ffa500", "#13ED3F"), 
            labels=c("Chain", "Retail"), 
            title="Pharmacy type") 

m


#### Add circles to the legend -----

# set legend features
colors <- c("gray", "gray", "gray", "#ffa500", "#13ED3F")
labels <- c("5 pills", "15 pills", "30 pills", "Chain", "Retail")
sizes <- c(sqrt(5)*3, sqrt(15)*3, sqrt(30)*3, 15, 15)
shapes <- c("circle", "circle", "circle", "square", "square")
borders <- c("gray", "gray", "gray", "#ffa500", "#13ED3F")

addLegendCustom <- function(map, colors, labels, sizes, shapes, borders, opacity = 0.5){
  
  make_shapes <- function(colors, sizes, borders, shapes) {
    shapes <- gsub("circle", "50%", shapes)
    shapes <- gsub("square", "0%", shapes)
    paste0(colors, "; width:", sizes, "px; height:", sizes, "px; border:3px solid ", borders, "; border-radius:", shapes)
  }
  make_labels <- function(sizes, labels) {
    paste0("<div style='float: right; display: inline-block;height: ", 
           sizes, "px;margin-top: 4px;line-height: ", 
           sizes, "px;'>", labels, "</div>")
  }
  legend_colors <- make_shapes(colors, sizes, borders, shapes)
  legend_labels <- make_labels(sizes, labels)
  
  return(addLegend(map, colors = legend_colors, labels = legend_labels, opacity = opacity))
}

## okay, back to our map code but replacing the addLegend() function
## with our new addCustomLegend() function

m <- leaflet(pharm_state)  %>% 
  addProviderTiles(providers$CartoDB.DarkMatter) %>% 
  setView(-92.469698, 31.012156, zoom = 7) %>% 
  addCircleMarkers(~lon, ~lat,
                   popup=paste0(pharm_state$BUYER_NAME, "<br />",
                                pharm_state$per_person, " pills per person per year"),
                   weight = 3,
                   radius=sqrt(pharm_state$per_person)*3, 
                   stroke = FALSE, 
                   fillOpacity = 0.3,
                   color=~cof(BUYER_BUS_ACT))   %>%
  # this line below is really the only thing that's different
  addLegendCustom(colors, labels, sizes, shapes, borders)

m


#### But more dots... ---------------

# leafgl is not yet on CRAN
# so uncomment and run the instlal line below

#devtools::install_github("r-spatial/leafgl")

library(leafgl)
library(sf)

# Convert the latitude and longitude into a geometry for easy mapping
pharms_spatial <- pharmacies %>% 
  st_as_sf(coords=c("lon", "lat"), crs = "+proj=longlat") 

m <- leaflet() %>%
  addProviderTiles(provider = providers$CartoDB.DarkMatter) %>%
  addGlPoints(data = pharms_spatial,
              opacity=.5,
              popup="BUYER_NAME",
              group = "BUYER_BUS_ACT") %>%
  setView(-92.469698, 31.012156, zoom = 3) %>% 
  addLayersControl(overlayGroups = "BUYER_BUS_ACT")

m
