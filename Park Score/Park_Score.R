library(ggbump)
library(glue)

parks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-22/parks.csv')
topten<-parks %>%
  dplyr::group_by(year)%>%
  dplyr::arrange((rank), .by_group=TRUE)%>%
  slice(1:10)

b<-topten %>%
  subset(city %in% c("Chicago","Boston","San Francisco","Irvine","Portland","Cincinnati","Arlington, Virginia","St. Paul","Washington, D.C.","Minneapolis"))

c<-b %>%
  subset(year==2020)

topten%>%
  ggplot(aes(year, rank, color = city)) +
  geom_point(size = 5,alpha=0.25)+
  geom_bump(smooth = 15, size = 2, alpha = 0.25)+
  geom_text(data = . %>% subset(year == max(year)),
            aes(x = year+0.09, label = city), size = 3, hjust = 0) +
  geom_text(data = . %>% subset(year == min(year)),
            aes(x = year, label = city), size = 3, hjust = 1.2)+
  scale_x_continuous(breaks = seq(2012, 2020, by=1))+
  geom_bump(b,mapping=aes(year, rank, color = city),smooth = 15, size = 2, alpha = 10)+
  geom_point(c,mapping=aes(year,rank),size = 5,alpha=0.7)+
  scale_y_reverse(breaks = seq(1, 10, by=1))+
  geom_segment(c,mapping=aes(x = 2021,y =rank , xend = year+total_pct/50,yend = rank), size = 6)+
  geom_text(c,mapping= aes(x = 2021.5, y = rank, label = total_pct, color = city), hjust = 0, size = 2, nudge_x = .4)+
  labs(title ="Recreational Paradise",subtitle = "The top 10 cities according to the ParkScore®\nThe score consists of 10 indicators such as the:\n● Median Park Size\n● Percent of Residents within a 10-Minute Walk to Park\n● Parkland as Percent of City Area  or\n● Playgrounds per 10,000 Residents",caption = glue("Data Source: The Trust for Public Land\nGraphics:Andreas Neumann"))+
  theme(text = element_text(family ="serif"),
        legend.position = "none",
        panel.grid = element_blank(),
        plot.title = element_text(hjust = .5, color = "white", size = 15),
        plot.subtitle = element_text(hjust = .5, color = "white", size = 10),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y =  element_text(face = 2, color = "gray48", size = 12),
        axis.title.y =   element_text(face = 2, color = "grey48", size = 11),
        axis.text.x = element_text(face = 2, color = "gray48", size = 10),
        panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        title = element_text(colour = "grey48"))+
  annotate(geom = "curve", x = 2012, y = 0.05, xend = 2012, yend = 1,curvature = .3, arrow = arrow(length = unit(2, "mm")), colour='white') +
  annotate(geom = "text", x = 2012.95, y = 0.015, label = "Since 2012 San Francisco dropped 7 ranks\nfrom her top spot to #8", hjust = "right",color="white",size=3.5)+
  annotate(geom = "text", x = 2021.55, y = 0.5, label = "Total Points(%)", hjust = "right",color="grey48",size=3.5)+
  annotate(geom = "text", x = 2011.7, y = 0.5, label = "City", hjust = "right",color="grey48",size=3.5) 
