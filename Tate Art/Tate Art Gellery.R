library(glue)
library(patchwork)

###Load data###

artwork <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-12/artwork.csv')
artists <- readr::read_csv("https://github.com/tategallery/collection/raw/master/artist_data.csv")
names(artists)[2]<-"artist"

###Add/count and plot categories###

artwork$category<- case_when(
  str_detect(artwork$medium, "oil|Oil|canvas|watercolour|Watercolour|Spray|Tempera|acrylic|Acrylic|painting|Gouache|Crayon|Bodycolour") ~ "painting", 
  str_detect(artwork$medium, "graphite|Graphite|chalk|Chalk|Ink|ink|pastel|Pastel|Charcoal") ~ "drawing",  
  str_detect(artwork$medium, "lithographs|Drypoint|drypoint|lithograph|Lithograph|Intaglio print|Relief print|Mezzotint|Screenprint|engraving|Engraving|Tracing|books|Woodcut|etching|Etching|Print on paper") ~ "printwork",
  str_detect(artwork$medium, "stoneware|Stoneware|Porcelain|Earthenware|teapot") ~ "pottery",
  str_detect(artwork$medium, "Photograph|photograph|photo") ~ "photograph",
  str_detect(artwork$medium, "Bronze|Metal|metal|Steel|wood|mirror|glass|Glass|stone|Stone|marble|Silver|Slate|Sofa|Terracotta|Tin|Wax|Wooden") ~ "plastic arts",
  str_detect(artwork$medium, "Video|Film|Vinyl") ~ "audio/video", 
  str_detect(artwork$medium, "Textile|Textiles|wool|Wool") ~ "textiles")

sum<-artwork%>% 
  dplyr::group_by(category,acquisitionYear) %>% 
  dplyr::summarise(Freq=n())%>%
  drop_na(category)%>%
  drop_na(acquisitionYear)


ab<-ggplot(data=sum,aes(acquisitionYear,Freq))+geom_line(aes(linetype = category),color="#33CCFF")+
  labs(y="No. of Art Acquisitions", x="Year",caption = glue("*art objects without art medium descriptions are not included\nData source: Tate Art Museum"))+
  ggtitle("Total number of Art Acquisitions by\n Tate-Art-Collection...*\n    ???")+
  theme(axis.title.y = element_text(color = "#33CCFF"),
        axis.title.x = element_text(color = "#33CCFF", margin = margin(10, 0, 0, 0)),
        axis.text.y = element_text(color = "#33CCFF", size = 12),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(hjust = 0, color = "#33CCFF"),
        panel.grid.major = element_line(linetype = "blank"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#333333", color = NA),
        plot.background = element_rect(fill = "#333333", color = NA),
        plot.title = element_text(hjust = 0.5),
        legend.background = element_rect(fill = "#333333", color = NA),
        legend.text = element_text(color = "#33CCFF"),
        legend.key = element_rect(fill = "#333333"),
        title = element_text(colour = "#33CCFF"))


###Maps###
##Data Wrangling(renaming, merging with spatial data) and plotting##


url<-"https://download2.exploratory.io/maps/world.zip"
download.file(url, dest="world.zip", mode="wb") 
unzip("world.zip", exdir = "world")
worldgeo <- sf::st_read("world/world.geojson")
worldgeo <- st_as_sf(x = worldgeo,crs = "+datum=WGS84")

artists$birth<-gsub(", +", ",", artists$placeOfBirth)

artists<-artists %>% 
  separate(birth,c("place","country"),sep=",")

artists[which(artists$place=="Polska"),11] <- "Poland"
artists[which(artists$place=="United Kingdom"),11] <- "United Kingdom"
artists[which(artists$place=="Braintree"),11] <- "United Kingdom"
artists[which(artists$place=="South Africa"),11] <- "South Africa"
artists[which(artists$place=="France"),11] <- "France"
artists[which(artists$place=="Slovenija"),11] <- "Slovenia"
artists[which(artists$place=="Barbados"),11] <- "Barbados"
artists[which(artists$place=="Egremont"),11] <- "United Kingdom"
artists[which(artists$place=="Deutschland"),11] <- "Germany"
artists[which(artists$place=="Zhonghua"),11] <- "China"
artists[which(artists$place=="Guyana"),11] <- "Guyana"
artists[which(artists$place=="Australia"),11] <- "Australia"
artists[which(artists$place=="Nederland"),11] <- "Netherlands"
artists[which(artists$place=="Ceská Republika"),11] <- "Czechia"
artists[which(artists$place=="Éire"),11] <- "Ireland"
artists[which(artists$place=="Nigeria"),11] <- "Nigeria"
artists[which(artists$place=="United States"),11] <- "United States"
artists[which(artists$place=="Colombia"),11] <- "Colombia"
artists[which(artists$place=="Rossiya"),11] <- "Russia"
artists[which(artists$place=="Italia"),11] <- "Italy"
artists[which(artists$place=="Brasil"),11] <- "Brazil"
artists[which(artists$place=="Yisra'el"),11] <- "Israel"
artists[which(artists$place=="Danmark"),11] <- "Denmark"
artists[which(artists$place=="London"),11] <- "United Kingdom"
artists[which(artists$place=="België"),11] <- "Belgium"
artists[which(artists$place=="Sverige"),11] <- "Sweden"
artists[which(artists$place=="Perth"),11] <- "United Kingdom"
artists[which(artists$place=="Jamaica"),11] <- "Jamaica"
artists[which(artists$place=="Myanmar"),11] <- "Myanmar"
artists[which(artists$place=="Magyarország"),11] <- "Hungary"
artists[which(artists$place=="Nihon"),11] <- "Japan"
artists[which(artists$place=="Taehan Min'guk"),11] <- "South Korea"
artists[which(artists$place=="Ukrayina"),11] <- "Ukraine"
artists[which(artists$place=="México"),11] <- "Mexico"
artists[which(artists$place=="Isle of Man"),11] <- "Isle of Man"
artists[which(artists$place=="Îran"),11] <- "Iran"
artists[which(artists$place=="Canada"),11] <- "Canada"
artists[which(artists$place=="Stockholm"),11] <- "Sweden"
artists[which(artists$place=="Perú"),11] <- "Peru"
artists[which(artists$place=="Lietuva"),11] <- "Lithuania"
artists[which(artists$place=="Costa Rica"),11] <- "Costa Rica"
artists[which(artists$place=="Saint Hélier"),11] <- "United Kingdom"
artists[which(artists$place=="New Zealand"),11] <- "New Zealand"
artists[which(artists$place=="Sri Lanka"),11] <- "Sri Lanka"
artists[which(artists$place=="Österreich"),11] <- "Austria"
artists[which(artists$place=="Viet Nam"),11] <- "Vietnam"
artists[which(artists$place=="Samoa"),11] <- "Samoa"
artists[which(artists$place=="España"),11] <- "Spain"
artists[which(artists$place=="Zambia"),11] <- "Zambia"
artists[which(artists$place=="Bangladesh"),11] <- "Bangladesh"
artists[which(artists$place=="Türkiye"),11] <- "Turkey"
artists[which(artists$place=="Bermondsey"),11] <- "United Kingdom"
artists[which(artists$place=="Zimbabwe"),11] <- "Zimbabwe"
artists[which(artists$place=="Singapore"),11] <- "Singapore"
artists[which(artists$place=="Cuba"),11] <- "Cuba"
artists[which(artists$place=="Stoke on Trent"),11] <- "United Kingdom"
artists[which(artists$place=="Suriyah"),11] <- "Syria"
artists[which(artists$place=="Hrvatska"),11] <- "Croatia"
artists[which(artists$place=="Bristol"),11] <- "United Kingdom"
artists[which(artists$place=="Al-Lubnan"),11] <- "Lebanon"
artists[which(artists$place=="Bermondsey"),11] <- "United Kingdom"
artists[which(artists$place=="Schweiz"),11] <- "Switzerland"
artists[which(artists$place=="Bharat"),11] <- "India"
artists[which(artists$place=="Montserrat"),11] <- "United Kingdom"
artists[which(artists$place=="Staten Island"),11] <- "United States"
artists[which(artists$place=="Misr"),11] <- "Egypt"
artists[which(artists$place=="Hertfordshire"),11] <- "United Kingdom"
artists[which(artists$place=="Schlesien"),11] <- "Poland"
artists[which(artists$place=="Canterbury"),11] <- "United Kingdom"
artists[which(artists$place=="Rochdale"),11] <- "United Kingdom"
artists[which(artists$place=="Beckington"),11] <- "United Kingdom"
artists[which(artists$place=="Melmerby"),11] <- "France"
artists[which(artists$place=="Charlotte Amalie"),11] <- "United States"
artists[which(artists$place=="Armenia"),11] <- "Armenia"
artists[which(artists$place=="As-Sudan"),11] <- "Sudan"
artists[which(artists$place=="România"),11] <- "Romania"
artists[which(artists$place=="Chung-hua Min-kuo"),11] <- "Taiwan"



artists$country <- gsub("Yisra'el", "Israel", artists$country,fixed = T)
artists$country <- gsub("Al-'Iraq", "Iraq", artists$country, fixed=T)
artists$country <- gsub("Al-Jaza'ir", "Algeria", artists$country, fixed=T)
artists$country <- gsub("Al-Lubnan", "Lebanon", artists$country, fixed=T)
artists$country <- gsub("België", "Belgium", artists$country, fixed=T)
artists$country <- gsub("Bharat", "India", artists$country, fixed=T)
artists$country <- gsub("Bosna i Hercegovina", "Bosnia and Herz.", artists$country, fixed=T)
artists$country <- gsub("Cameroun", "Cameroon", artists$country, fixed=T)
artists$country <- gsub("Ceská Republika", "Czechia", artists$country, fixed=T)
artists$country <- gsub("Choson Minjujuui In'min Konghwaguk", "North Korea", artists$country, fixed=T)
artists$country <- gsub("Brasil", "Brazil", artists$country, fixed=T)
artists$country <- gsub("D.C.", "Colombia", artists$country, fixed=T)
artists$country <- gsub("Danmark", "Denmark", artists$country, fixed=T)
artists$country <- gsub("Deutschland", "Germany", artists$country, fixed=T)
artists$country <- gsub("Eesti", "Estonia", artists$country, fixed=T)
artists$country <- gsub("Éire", "Ireland", artists$country,fixed = T)
artists$country <- gsub("Mehoz", "Israel", artists$country,fixed = T)
artists$country <- gsub("Ellás", "Greece", artists$country, fixed=T)
artists$country <- gsub("España", "Spain", artists$country, fixed=T)
artists$country <- gsub("Hrvatska", "Croatia", artists$country, fixed=T)
artists$country <- gsub("Îran", "Iran", artists$country, fixed=T)
artists$country <- gsub("Ísland", "Iceland", artists$country, fixed=T)
artists$country <- gsub("Italia", "Italy", artists$country, fixed=T)
artists$country <- gsub("Jugoslavija", "Serbia", artists$country, fixed=T)
artists$country <- gsub("Lao", "Lao PDR", artists$country, fixed=T)
artists$country <- gsub("Latvija", "Latvia", artists$country, fixed=T)
artists$country <- gsub("Magyarország", "Hungary", artists$country, fixed=T)
artists$country <- gsub("Makedonija", "Macedonia", artists$country, fixed=T)
artists$country <- gsub("México", "Mexico", artists$country, fixed=T)
artists$country <- gsub("Misr", "Egypt", artists$country, fixed=T)
artists$country <- gsub("Nihon", "Japan", artists$country, fixed=T)
artists$country <- gsub("Norge", "Norway", artists$country, fixed=T)
artists$country <- gsub("Österreich", "Austria", artists$country, fixed=T)
artists$country <- gsub("Panamá", "Panama", artists$country, fixed=T)
artists$country <- gsub("Perú", "Peru", artists$country, fixed=T)
artists$country <- gsub("Pilipinas", "Philippines", artists$country, fixed=T)
artists$country <- gsub("Polska", "Poland", artists$country, fixed=T)
artists$country <- gsub("Prathet Thai", "Thailand", artists$country, fixed=T)
artists$country <- gsub("Suomi", "Finland", artists$country, fixed=T)
artists$country <- gsub("Zhonghua", "China", artists$country, fixed=T)
artists$country <- gsub("România", "Romania", artists$country, fixed=T)
artists$country <- gsub("Türkiye", "Turkey", artists$country, fixed=T)
artists$country <- gsub("Nederland", "Netherlands", artists$country, fixed=T)
artists$country <- gsub("Rossiya", "Russia", artists$country, fixed=T)
artists$country <- gsub("Schweiz", "Switzerland", artists$country, fixed=T)
artists$country <- gsub("Slovenija", "Slovenia", artists$country, fixed=T)
artists$country <- gsub("Sverige", "Sweden", artists$country, fixed=T)
artists$country <- gsub("România", "Romania", artists$country, fixed=T)
artists$country <- gsub("Ukrayina", "Ukraine", artists$country, fixed=T)
artists$country <- gsub("Slovenská Republika", "Slovakia", artists$country, fixed=T)
artists$country <- gsub("Suriyah", "Syria", artists$country, fixed=T)
artists$country <- gsub("Viet Nam", "Vietnam", artists$country, fixed=T)
artists$country <- gsub("Nigeria", "Nigeria", artists$country, fixed=T)


names(artists)[1]<-"artistId"
a<-merge(artists,artwork,by="artistId")

names(worldgeo)[1]<-"country"
a<-merge(a,worldgeo,by="country")

a <- st_as_sf(x = a,crs = "+datum=WGS84")

freq<-a %>%
  dplyr::group_by(category,country) %>% 
  dplyr::summarise(Freq=n())

b<-ggplot(data=freq)+geom_sf(color="#33CCFF",fill = "#333333")+
  ggtitle("...and the artists' countries of origin\n   ???")+
  facet_grid(vars(category))+scale_x_discrete(expand = c(0, 0.5))+
  scale_x_discrete(expand = c(0, 0.5))+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major = element_line(linetype = "blank"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#333333", color = NA),
        plot.background = element_rect(fill = "#333333", color = NA),
        plot.title = element_text(hjust = 0.5),
        title = element_text(colour = "#33CCFF"),
        strip.background =element_rect(fill="#333333"),
        strip.text = element_text(color ="#33CCFF"))

###Put everything together###
ab+b


