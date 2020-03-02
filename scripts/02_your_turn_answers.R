library(tidyverse)
library(leaflet)

#### Challenge No. 1 answer ----

sheet <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRrObtIYokP-AywK_A33uF2phYvbBhi-lZxNndqngrB9U2srj_1iWBlLdUWBKisjlUIAqZSSeEr1VXy/pub?output=csv")

m <- leaflet(sheet) %>% 
  addProviderTiles(providers$CartoDB.DarkMatter) %>% 
  setView( -89.149229, 31.795991, zoom = 6) %>% 
  addCircleMarkers(~lon, ~lat, 
                   popup=paste0(sheet$addressLine1, "<br />",
                                sheet$city, ", ", 
                                str_to_title(sheet$province)),
                   weight = 3,
                   radius=3, 
                   stroke = FALSE)
m

#### Challenge No. 2 answer ----

sheet <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRrObtIYokP-AywK_A33uF2phYvbBhi-lZxNndqngrB9U2srj_1iWBlLdUWBKisjlUIAqZSSeEr1VXy/pub?output=csv")

pall <- colorFactor(c("#e41a1c", "#4daf4a" , "#ffff00","#ffa500"), domain=c("Closed", "Full menu", "Limited menu", "Unknown"))

m <- leaflet(sheet) %>% 
  addProviderTiles(providers$CartoDB.DarkMatter) %>% 
  setView(-92.469698, 31.012156, zoom = 7) %>% 
  addCircleMarkers(~lon, ~lat, 
                   popup=paste0(sheet$addressLine1, "<br />",
                                sheet$city, ", ", 
                                str_to_title(sheet$province)),
                   weight = 3,
                   radius=3, 
                   stroke = FALSE, 
                   fillOpacity = 0.8,
                   color=~pall(status)) %>% 
  addLegend("bottomright", 
            colors= c("#e41a1c", "#ffff00", "#ffa500", "#4daf4a"), 
            labels= c("Closed", "Limited menu", "Unknown", "Full menu"), 
            title="Waffle House Index") 

m


