library(tidyverse)
library(leaflet)

# Download the sheet of Waffle House locations
sheet <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRrObtIYokP-AywK_A33uF2phYvbBhi-lZxNndqngrB9U2srj_1iWBlLdUWBKisjlUIAqZSSeEr1VXy/pub?output=csv")

#### Challenge No. 1 ----------------

# use glimpse() to look at what you have in the sheet dataframe
# Can you make a leaflet map of all the Waffle Houses in the country?
# Set the center of the map to Laurel, MS and zoom to 7
# Let the popup text bring up the address1 column
# As reference look at the #### Putting a marker on a map
# chunk of code under line 14 of 02_leaflet_locator_map.R


#### Challenge No. 2 ----------------

# Visit this sheet http://bit.ly/wflsheet
# and fill out the contacted and status cells based on 
# what your notes say

# redownload the sheet again
sheet <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRrObtIYokP-AywK_A33uF2phYvbBhi-lZxNndqngrB9U2srj_1iWBlLdUWBKisjlUIAqZSSeEr1VXy/pub?output=csv")

# Can you make a leaflet map of all the Waffle Houses in the country?
# But this time color-coded based on FEMA's Waffle House Index?
# Let the popup text bring up the address1 column AND city AND state
# As reference look at the #### Multiple locations from a csv
# chunk of code under line 48 of 02_leaflet_locator_map.R

## Here's some code to help you get started
## for the color palette

pall <- colorFactor(c("#e41a1c", "#4daf4a" , "#ffff00","#ffa500"), domain=c("Closed", "Full menu", "Limited menu", "Unknown"))
