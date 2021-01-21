library(ggtextures)
library(scales)
library(glue)

mac <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-22/big-mac.csv')

###Data Wrangling and number of burgers that can be purchased for $10###
mac$pr<-10/mac$dollar_price
mac$pr_1<-round(mac$pr, digits = 0)
mac$date2 <- format(mac$date, format = "%Y/%b/%d")

###Select data for Jan 2020###
mac2<-mac %>%
  group_by(name, date2)%>%
  filter(str_detect(date2, "2020/Jan"))

###Isolating and fixing wrongly rounded numbers###
mac2$test<-ifelse(mac2$pr_1*mac2$dollar_price >10,"Yes","No")

r <- subset(mac2, test=="Yes")
r$pr_2<-floor(r$pr)
s<- subset(mac2, test=="No")
s$pr_2<-round(s$pr,0)
mac2<-rbind(r,s)

###Select burger emoji###
image<- c("https://emojipedia-us.s3.dualstack.us-west-1.amazonaws.com/thumbs/160/microsoft/106/hamburger_1f354.png")

###transform column "dollar_price" into dollar numbers for labeling###
mac2$dollar<-dollar(mac2$dollar_price)

###Plot###
mac2 %>% 
  ggplot(aes(x = reorder(name, pr_2), y = pr_2,
             image = image)) +
  geom_isotype_col(img_width = grid::unit(1, "native"),
                   ncol =NA , nrow = NA, hjust = 1, vjust = 0) +
  scale_y_continuous(breaks = c(0, 1, 2, 3, 4, 5),
                     limits = c(0, 30),
                     expand = expansion(mult = c(0, 0))) +   
  coord_flip() + 
  geom_text(aes(label = dollar, hjust = -0.1), size = 5)+
  labs(title = "How many burgers can you buy for $10?",
       caption = glue("Source: Data for Jan. 2020\nThe Economist"))+
  theme(
    axis.line=element_blank(),
    axis.ticks=element_blank(),
    axis.title.x=element_blank(), 
    axis.text.x=element_blank(),
    axis.title.y=element_blank(),
    legend.position="none",
    panel.border=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    plot.background=element_rect(fill = "#FFFFCC", color = NA),
    panel.background = element_rect(fill = "#FFFFCC", color = NA),
    plot.title = element_text(hjust = 0.5))