library(tidyverse)
library(cartography)
library(remotes)
library(popcircle)
library(sf)

##Download the data##
url<-"https://download2.exploratory.io/maps/world.zip"
download.file(url, dest="world.zip", mode="wb") 
unzip("world.zip", exdir = "world")
worldgeo <- sf::st_read("world/world.geojson")
names(worldgeo)[1]<-"country"
women <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-08/women.csv')

##Data Wrangling and merge data sets##
women$country <- gsub("Exiled Uighur from Ghulja (in Chinese, Yining)", "China", women$country,fixed = T)
women$country <- gsub("Iraq/UK", "United Kingdom", women$country, fixed=T)
women$country <- gsub("DR Congo", "Dem. Rep. Congo", women$country)
women$country <- gsub("Northern Ireland", "United Kingdom", women$country)
women$country <- gsub("Republic of Ireland", "Ireland", women$country)
women$country <- gsub("UAE", "United Arab Emirates",women$country)
women$country <- gsub("UK", "United Kingdom",women$country)
women$country <- gsub("US", "United States", women$country)
women$country <- gsub("Wales, United Kingdom", "United Kingdom",women$country, fixed=T)
womenworld<-merge(worldgeo, women,by="country")
count<- womenworld%>% 
  dplyr::group_by(country,role) %>% 
  dplyr::summarise(Freq=n())
##Create Popcircle##
pop <- popcircle(x = count, var = "Freq")                                                             
pop_circle <- pop$circle
pop_shape <-pop$shapes
pop_shape <- st_transform(pop_shape, 4326)
pop_circle <- st_transform(pop_circle, 4326)
plot(st_geometry(pop_circle), bg = "#333333",col = "#FFFFFF", border = "white")                       
plot(st_geometry(pop_shape), col = "#FFFFFF", border = "#333333",add = TRUE, lwd = 1.5)
labelLayer(x = pop_circle, txt = "role", halo = TRUE, overlap = FALSE, col = "#666666", r=.15)

# works on windows only
windowsFonts(A=windowsFont("Bookman Old Style"))
tt <- st_bbox(pop_circle)
text(tt[1], tt[4], labels = "100 WOMEN...",family="A",font=3,adj=c(0,1),
     col = "grey", cex = 2)
text(tt[2], tt[2], labels = "...leading change in 2020",family="A", font=3,adj=c(0,1),
     col = "grey", cex = 1.5)