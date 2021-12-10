require(geojsonio)
require(tidyverse)
require(data.table)
require(sf)
require(sp)

setwd("C:/Users/ZELST007/Desktop/covid-19/meetings/")

geoUrl <- "https://geodata.nationaalgeoregister.nl/cbsgebiedsindelingen/wfs?request=GetFeature&service=WFS&version=2.0.0&typeName=cbs_provincie_2021_gegeneraliseerd&outputFormat=json"
fileName <- "C:/Users/ZELST007/Documents/Code Zwart/provinciegrenzen2021.geojson" # File for GGD map
download.file(geoUrl, fileName)
provincegrenzen <- geojson_read(fileName, what = "sp") # Load GGD map



excess_mort_province <- read.csv("C:/Users/ZELST007/Documents/Code Zwart/excess_mortality_provinces_clean.csv")
excess_mort_province_filtered <- excess_mort_province %>%
  filter(Week == 48) %>%
  filter(Jaar == 2021)

excess_province_long <- gather(excess_mort_province_filtered, "statnaam","excess_mortality",3:14)
excess_province_long$excess_mortality <- round(excess_province_long$excess_mortality,0)


excess_province_long$statnaam <- recode(excess_province_long$statnaam, "Noord.Holland" = "Noord-Holland",
                                        "Zuid.Holland" = "Zuid-Holland",
                                        "Noord.Brabant" = "Noord-Brabant")

provincegrenzen@data <- provincegrenzen@data %>%
  left_join(excess_province_long,by=c("statnaam"))

g.province <- fortify(provincegrenzen, region = "id")
provinceDF <- merge(g.province, provincegrenzen@data, by = "id")



centroids.df <- as.data.frame(coordinates(provincegrenzen))
names(centroids.df) <- c("long", "lat") 
popList <- provincegrenzen@data$excess_mortality

idList <- provincegrenzen@data$statcode

pop.df <- data.frame(id = idList, population = popList, centroids.df)





province_mort_plot <- ggplot(data = provinceDF) +
  geom_polygon(aes(x=long, y=lat, group = group, fill = excess_mortality), color = "black", lwd=0.2) +
  coord_equal()+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.pos = "right",
        legend.title = element_blank()) +
  ggtitle("Procentuele oversterfte per provincie") +
  theme_void() +
  scale_fill_gradientn(colours=c("yellow","orange", "red","purple"), name = "Percentage")

province_mort_plot + geom_text(data=pop.df, aes(label=paste0(population,"%"), x=long, y=lat), size = 4.5, colour="black",fontface="bold") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        plot.title.position = "plot",
        plot.subtitle = element_text(hjust = 0.5, size = 14),
        plot.caption = element_text(hjust = 1, vjust = 8.0)) +
  labs(subtitle = "Week 48 - 2021 \n Gecorrigeerd voor bevolkingsgroei, omvang, en vergrijzing",
       caption = "Bron: CBS | Datum: 10-12-2021 | Plot: @mzelst")

ggsave("../data-misc/excess_mortality/plots_weekly_update/oversterfte_provincie.png")


## Oversterfte per week per provincie


excess_mort_province <- read.csv("C:/Users/ZELST007/Documents/Code Zwart/excess_mortality_provinces_clean.csv")
excess_mort_province_filtered <- excess_mort_province %>%
  filter(Week == 47) %>%
  filter(Jaar == 2021)

excess_province_long <- gather(excess_mort_province, "statnaam","excess_mortality",3:14)
excess_province_long$excess_mortality <- round(excess_province_long$excess_mortality,0)


excess_province_long$weekyear <- ifelse(excess_province_long$Week<10,
                          paste0(excess_province_long$Jaar,"-",0,excess_province_long$Week),
                          paste0(excess_province_long$Jaar,"-",excess_province_long$Week))

target <- c("Limburg", "Zeeland")

excess_province_long %>%
  filter(statnaam %in% target) %>%
  ggplot() +
  geom_line(aes(x = factor(weekyear), y =excess_mortality, group = statnaam, color = statnaam), lwd = 1.0) + 
  coord_equal()+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.pos = "right",
        legend.title = element_blank()) +
  ggtitle("Procentuele oversterfte per provincie") +
  theme_void()

province_mort_plot <- ggplot(data = provinceDF) +
  geom_polygon(aes(x=long, y=lat, group = group, fill = excess_mortality), color = "black", lwd=0.2) +
  coord_equal()+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.pos = "right",
        legend.title = element_blank()) +
  ggtitle("Procentuele oversterfte per provincie") +
  theme_void() +
  scale_fill_gradientn(colours=c("yellow","orange", "red","purple"), name = "Percentage")

province_mort_plot + geom_text(data=pop.df, aes(label=paste0(population,"%"), x=long, y=lat), size = 4.5, colour="black",fontface="bold") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        plot.title.position = "plot",
        plot.subtitle = element_text(hjust = 0.5, size = 16),
        plot.caption = element_text(hjust = 1, vjust = 8.0)) +
  labs(subtitle = "Week 47 - 2021",
       caption = "Bron: CBS | Datum: 03-12-2021 | Plot: @mzelst")

