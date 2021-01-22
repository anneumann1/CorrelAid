library(downloader)
library(tmap)
library(sf)
###data wind turbines###
Wind_Turbine_Database_FGP <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-27/wind-turbine.csv')

names(Wind_Turbine_Database_FGP)[2] <- "territory"
names(Wind_Turbine_Database_FGP)[12] <- "year"

Wind_Turbine_Database_FGP$year <- as.numeric(Wind_Turbine_Database_FGP$year)
df <- Wind_Turbine_Database_FGP %>%
  dplyr::group_by(territory, year) %>%
  dplyr::summarise(Freq = n())

###add territories and provinces####
url <- "https://download2.exploratory.io/maps/canada_provinces.zip"
unzip("canada_provinces.zip", exdir = "canada_provinces")
canada <- sf::st_read("canada_provinces/canada_provinces/canada_provinces.geojson")

names(canada)[3] <- "territory"
###Merge the data and create maps###
canadaturbines <- merge(df, canada, by = "territory")
carto <- st_as_sf(x = canadaturbines,
                  crs = "+datum=WGS84")
carto_norm = carto %>%
  split(.$year) %>%
  do.call(rbind, .)

anim_can = tm_shape(canada) + tm_polygons(col = "lightblue") + tm_shape(carto_norm) +
  tm_polygons("Freq", title = "windmillcount: ") +
  tm_facets(along = "year",
            free.coords = FALSE,
            drop.units = TRUE) +
  tm_layout(legend.outside.position = "right",
            legend.outside = TRUE) + tm_text("Freq")

tmap_animation(anim_can, delay = 150,width = 1326,height = 942) 