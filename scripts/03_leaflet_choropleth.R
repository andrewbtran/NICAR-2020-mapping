#### Setup --------------

# This function checks if you don't have the correct packages installed yet
# If not, it will install it for you
packages <- c("tidyverse", "readr", "httpuv",
              "leaflet", "sf", "tigris", "arcos",
              "sp", "rmapshaper")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())), repos = "http://cran.us.r-project.org")  
}

library(sf)
library(leaflet)
library(tidyverse)
library(arcos)
library(tigris)

#### Importing shapefiles from your computer --------------
counties <- st_read("shapefiles/us_counties.shp")

#### Quickly visualizing the shapefile -------------
counties %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolygons(popup=~NAME)

#### Import data we want to map ----------------
annual_summary <- read_csv("data/county_pill_summary.csv")

# If you'd like to build the data above from scratch
# using the arcos api, uncomment and run the lines below

#annual_counties <- summarized_county_annual(key="WaPo")
#annual_population <- county_population(key = "WaPo")
#annual <- left_join(annual_counties, annual_population)
#annual <- annual %>% 
#  mutate(pills_per_person=round(DOSAGE_UNIT/population,2))


#annual_summary <- annual %>% 
#  group_by(BUYER_COUNTY, BUYER_STATE, countyfips) %>% 
#  summarize(avg_pills_per_person=round(mean(pills_per_person, na.rm=T),2))

glimpse(annual_summary)


#### Join the data to the shapefile ----------------
# Now we use the Tigris function geo_join to bring together 
# the states shapefile and the sb_states dataframe -- GEOID and countyfips
# are the two columns they'll be joined by

counties_merged_annual <- geo_join(counties, annual_summary, "GEOID", "countyfips")


# Getting rid of rows with NA values
# Using the Base R method of filtering subset() because we're dealing with a SpatialPolygonsDataFrame and not a normal data frame, thus filter() wouldn't work

counties_merged_annual <- subset(counties_merged_annual, !is.na(avg_pills_per_person))

pal <- colorNumeric("Greens", domain=counties_merged_annual$avg_pills_per_person)

# Setting up the popup text
popup_sb <- paste0(counties_merged_annual$BUYER_COUNTY, ", ", 
                   counties_merged_annual$BUYER_STATE, 
                   "</br/> Average pills per person: \n", 
                   as.character(counties_merged_annual$avg_pills_per_person))

#### Generating a leaflet map with variables for style -----

# Mapping it with the new tiles CartoDB.Positron
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(-98.483330, 38.712046, zoom = 4) %>% 
  addPolygons(data = counties_merged_annual , 
              fillColor = ~pal(counties_merged_annual$avg_pills_per_person), 
              fillOpacity = 1, 
              weight = 0.9, 
              smoothFactor = 0.2, 
              stroke=TRUE,
              color="white",
              popup = ~popup_sb) %>%
  addLegend(pal = pal, 
            values = counties_merged_annual$avg_pills_per_person, 
            position = "bottomright", 
            title = "Average pills per person")


#### Bring in shapefiles with AK and HI moved ---

#library(sp)

# albersusa is not on CRAN so you need to uncomment 
# the code below and run it to install
#remotes::install_github("hrbrmstr/albersusa")

#library(albersusa)

#counties_reprojected <- counties_sf()
#states_reprojected <- usa_sf()

counties_reprojected <- st_read("shapefiles/counties_reprojected.shp")
states_reprojected <- st_read("shapefiles/states_reprojected.shp")

library(rmapshaper)

# simplifying the state borders so they're smaller
state_borders <- rmapshaper::ms_simplify(states_reprojected, keep = 0.1)


#### Re merging data to new shapefiles -------

counties_merged_annual_re <- left_join(counties_reprojected, annual_summary, by=c("fips"= "countyfips"))

# Getting rid of rows with NA values
# These are dataframes so we can use filter instead of subset

counties_merged_annual_re <- filter(counties_merged_annual_re, !is.na(avg_pills_per_person)) %>% 
  arrange(desc(fips))

#### new adjustments to projections and style ----

# setting up a color palette depending 
# on the range of numbers in avg_pills_per_person
# pal <- colorNumeric("Reds", domain = counties_merged_annual$avg_pills_per_person)

# but i'm cheating by setting the ceiling at 150
# and anything above that will be a darker color 
# that I set up in the NA field
pal <- colorNumeric("Reds", domain =0:150, na.color = "#640E27")

# this sets up some custom projection in leaflet
# complicated but necessary
epsg2163 <- leafletCRS(
  crsClass = "L.Proj.CRS",
  code = "EPSG:2163",
  proj4def = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs",
  resolutions = 2^(16:7))

#### Visualizing the reprojected data ----


m <- leaflet(counties_merged_annual_re, options = leafletOptions(crs = epsg2163)) %>%
  addPolygons(data=counties_reprojected, fillOpacity = 1, 
              weight = 0.9, 
              smoothFactor = 0.2, 
              stroke=TRUE,
              color="white") %>% 
  addPolygons(
    fillColor = ~pal(avg_pills_per_person), fillOpacity = 1, 
    weight = 0.9, 
    smoothFactor = 0.2, 
    stroke=TRUE,
    color="white",
    label = ~paste(BUYER_COUNTY, avg_pills_per_person),
    labelOptions = labelOptions(direction = "auto"))

m

#### Code to generate minicharts ----
#library(arcos)
#annual_counties <- summarized_county_annual(key="WaPo")
#annual_population <- county_population(key = "WaPo")
#annual <- left_join(annual_counties, annual_population)
#annual <- annual %>% 
#  mutate(pills_per_person=round(DOSAGE_UNIT/population,2))

#annual <- filter(annual, !is.na(countyfips))
#county_list <- unique(annual$countyfips)


#for (i in 1:length(county_list)) {

#    county_to_slice_out <- county_list[i]

#    dataFiltered<-filter(annual,countyfips==county_to_slice_out)

#    p<- ggplot(dataFiltered,aes(year, pills_per_person)) +
#      geom_col(fill="#640E27") +
#      theme_minimal() +
#      scale_x_discrete(limits=c(2006, 2012)) +
#      theme(axis.title.y=element_blank(),
#            axis.text.y=element_blank(),
#        axis.ticks.y=element_blank(),
#        panel.grid.major = element_blank(),
#        panel.grid.minor = element_blank()) +
#      labs(y="", x="")

#    if (!dir.exists("minicharts")) {
#      dir.create("minicharts")
#    }

#    file_dir <- "minicharts"
#    file_name <- paste0(county_list[i], ".png")
#    file_save <- paste0(file_dir, "/", file_name)

#   ggsave(filename=file_name, path=file_dir, plot=p, width=70, height=30, units="mm") 
#    print(paste0(file_save, "-", i, " of ", length(county_list)))
#}

#### Create new popups based on location of png charts ---


popup_sb <- paste0(
  "<strong>",
  str_to_title(counties_merged_annual_re$BUYER_COUNTY),
  " County</strong></br>",
  round(counties_merged_annual_re$avg_pills_per_person, 1),
  " pills per person, per year </br>
  <img src='",
  getwd(),
  "/minicharts/", counties_merged_annual_re$fips, ".png' width=150px, height=60px</>")

#### New map with png chart popups ----

for_exporting <- leaflet(counties_merged_annual_re, options = leafletOptions(crs = epsg2163)) %>%
  addPolygons(data=counties_reprojected, fillOpacity = 1, 
              weight = 0.9, 
              smoothFactor = 0.2, 
              stroke=TRUE,
              color="white") %>% 
  addPolygons(
    fillColor = ~pal(avg_pills_per_person), fillOpacity = 1, 
    weight = 0.9, 
    smoothFactor = 0.2, 
    stroke=TRUE,
    color="white",
    group="countyfips",
    popup=~popup_sb)

# Loading a library to save the html we're creating to the local computer
library(htmltools)

save_html(for_exporting, file="map_with_images.html",  background = "white")

browseURL("map_with_images.html")

#### popups with html and css ---------

# import the data
annual <- read_csv("data/annual.csv")

annual_wide <- annual %>% 
  select(BUYER_COUNTY, BUYER_STATE, countyfips, year, pills_per_person) %>% 
  group_by(BUYER_COUNTY, BUYER_STATE, countyfips) %>% 
  mutate(avg_per_person_per_year=round(mean(pills_per_person, na.rm=T),1),
         max=max(pills_per_person),
         min=min(pills_per_person)) %>% 
  spread(year, pills_per_person) %>% 
  mutate(val2006=(`2006`-min)/(max-min)*10,
         val2007=(`2007`-min)/(max-min)*10,
         val2008=(`2008`-min)/(max-min)*10,
         val2009=(`2009`-min)/(max-min)*10,
         val2010=(`2010`-min)/(max-min)*10,
         val2011=(`2011`-min)/(max-min)*10,
         val2012=(`2012`-min)/(max-min)*10
  ) %>% 
  arrange(desc(countyfips))

## and now the intense css and html part

popup_sb <- paste0('<div class="tooltip" style="max-width: 160px;">
                   <h3>', str_to_title(annual_wide$BUYER_COUNTY), ' County</h3>
                   <p>', annual_wide$avg_per_person_per_year, ' pills per person, per year</p>
                   <div class="tooltip-vis-wrap" style="width: 110px; margin: 0 auto;">
                   <div class="tooltip-vis" style="display: flex; align-items: flex-end; margin-top: 2em;">
                   <div class="year" data-year="2006" style="width: 15px; background: #870235; margin: 0 1px; height:', annual_wide$val2006, 'px;"></div>
                   <div class="year" data-year="2007" style="width: 15px; background: #870235; margin: 0 1px; height:', annual_wide$val2007, 'px;"></div>
                   <div class="year" data-year="2008" style="width: 15px; background: #870235; margin: 0 1px; height:', annual_wide$val2008, 'px;"></div>
                   <div class="year" data-year="2009" style="width: 15px; background: #870235; margin: 0 1px; height:', annual_wide$val2009, 'px;"></div>
                   <div class="year" data-year="2010" style="width: 15px; background: #870235; margin: 0 1px; height:', annual_wide$val2010, 'px;"></div>
                   <div class="year" data-year="2011" style="width: 15px; background: #870235; margin: 0 1px; height:', annual_wide$val2011, 'px;"></div>
                   <div class="year" data-year="2012" style="width: 15px; background: #870235; margin: 0 1px; height:', annual_wide$val2012, 'px;"></div>
                   <div class="year" data-year="2013" style="width: 15px; background: #870235; margin: 0 1px; height:10.475px;"></div>
                   <div class="year" data-year="2014" style="width: 15px; background: #870235; margin: 0 1px; height:9.75px;"></div>
                   </div>
                   <div class="tooltip-vis-labels" style="display: flex; justify-content: space-between; border-top: 1px solid #000;">
                   <p class="year-label" style="font-size: 13px; margin-top: 2px;">2006</p>
                   <p class="year-label" style="font-size: 13px; margin-top: 2px;">2014</p>
                   </div>
                   </div>
                   </div>')

#### New map with html/css popups -------

for_exporting <- leaflet(counties_merged_annual_re, options = leafletOptions(crs = epsg2163)) %>%
# Check out the layers I'm adding here
# first is all counties with white fill color
# in case there are counties with no values, it won't just be transparent 
  addPolygons(data=counties_reprojected, fillOpacity = 1, 
              weight = 0.9, 
              smoothFactor = 0.2, 
              stroke=TRUE,
              color="white") %>% 
# the next layer is for counties with values only
# the reds are layered here
  addPolygons(
    fillColor = ~pal(avg_pills_per_person), fillOpacity = 1, 
    weight = 0.9, 
    smoothFactor = 0.2, 
    stroke=FALSE) %>% 
# Adding state borders
  addPolygons(data=state_borders, fillOpacity = 0, 
              weight = 0.9, 
              smoothFactor = 0.2, 
              stroke=TRUE,
              color="black") %>% 
# adding the counties back in but transparent 
# and clickable for popups
  addPolygons(fillOpacity = 0, 
              weight = 0.9, 
              smoothFactor = 0.2, 
              opacity=1,
              color="transparent",
              #stroke=FALSE,
              popup=~popup_sb,
              highlightOptions = highlightOptions(color = "black", weight = 2,
                                                  bringToFront = TRUE)) %>% 
  addLegend("bottomright", pal = pal, values = 0:150,
            title = "Pills per person",
            opacity = 1
  )


# exporting to html 
save_html(for_exporting, 
          file="map_with_css.html", 
          background = "white")

browseURL("map_with_css.html")


