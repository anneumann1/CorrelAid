library(ggplot2)
library(tidyverse)
library(plyr)
library(ggrepel) #for arrows and labels

hike_data <- readr::read_rds(url('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-24/hike_data.rds'))

##Data cleaning as proposed##
clean_hike_data <- hike_data %>% 
  mutate(
    trip = case_when(
      grepl("roundtrip",length) ~ "roundtrip",
      grepl("one-way",length) ~ "one-way",
      grepl("of trails",length) ~ "trails"),
    
    length_total = as.numeric(gsub("(\\d+[.]\\d+).*","\\1", length)) * ((trip == "one-way") + 1),
    
    gain = as.numeric(gain),
    rating= as.numeric(rating),
    highpoint = as.numeric(highpoint),
    
    location_general = gsub("(.*)\\s[-][-].*","\\1",location))

##Extra table to calculate mean rating for each region##
check2<-ddply(clean_hike_data, .(location_general), summarize,  rating=mean(rating))

ggplot()+
  geom_point(data=clean_hike_data,aes(location_general,rating,size=length_total,col=trip),alpha=0.25)+
  labs(y="Ratings", x="location",title = "How popular are Washington´s hiking trails?",
       color = "Trail Category:", size="Length in:\nmiles:")+
  geom_hline(yintercept = 2.50,color="white") +
  geom_point(data=check2, aes(location_general, rating,size=50),shape=17,alpha=1500,color="darkgrey")+ 
  ggrepel::geom_text_repel(data=check2, aes(location_general, rating),segment.color = "#CCCCCC", colour = "grey",label = "average rating")+
  guides(size = guide_legend(override.aes = list(shape = 1)))+
  geom_vline(xintercept = 0)+
  theme(axis.title.y = element_text(color = "white"),
        axis.title.x = element_text(color = "white", margin = margin(10, 0, 0, 0)),
        axis.text.y = element_text(color = "#CCCCCC", size = 12),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(hjust = 0, color = "white"),
        panel.grid.major = element_line(linetype = "blank"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#333333", color = NA),
        plot.background = element_rect(fill = "#333333", color = NA),
        plot.title = element_text(hjust = 0.5),
        legend.background = element_rect(fill = "#333333", color = NA),
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white"),
        legend.key = element_rect(fill = "#333333"),
        title = element_text(colour = "#FFFFFF"))+coord_flip()


