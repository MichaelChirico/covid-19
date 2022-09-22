require(geojsonio)
require(tidyverse)
require(data.table)
require(sf)
require(sp)

geoUrl <- "https://geodata.nationaalgeoregister.nl/cbsgebiedsindelingen/wfs?request=GetFeature&service=WFS&version=2.0.0&typeName=cbs_provincie_2021_gegeneraliseerd&outputFormat=json"
fileName <- "data-misc/excess_mortality/provinciegrenzen2021.geojson" # File for GGD map
download.file(geoUrl, fileName)
provincegrenzen <- geojson_read(fileName, what = "sp") # Load GGD map

week.now <- isoweek(Sys.Date())-1

excess_mort_province <- fread("data-misc/excess_mortality/excess_mortality_provinces_clean.csv")
excess_mort_province_filtered <- excess_mort_province %>%
  dplyr::filter(Week == week.now) %>%
  dplyr::filter(Jaar == 2022)

excess_province_long <- gather(excess_mort_province_filtered, "statnaam","excess_mortality",3:14)
excess_province_long$excess_mortality <- round(excess_province_long$excess_mortality,0)


excess_province_long$statnaam <- recode(excess_province_long$statnaam, "Noord.Holland" = "Noord-Holland",
                                        "Zuid.Holland" = "Zuid-Holland",
                                        "Noord.Brabant" = "Noord-Brabant",
                                        "Fryslan" = "Fryslân")

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
  theme_bw() +
  coord_equal()+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.pos = "right",
        legend.title = element_blank()) +
  ggtitle("Procentuele oversterfte per provincie") +
  scale_fill_gradientn(colours=c("green","yellow","orange", "red","purple"), name = "Percentage")

province_mort_plot + geom_text(data=pop.df, aes(label=paste0(population,"%"), x=long, y=lat), size = 4.5, colour="black",fontface="bold") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        plot.title.position = "plot",
        plot.subtitle = element_text(hjust = 0.5, size = 14),
        plot.caption = element_text(hjust = 1, vjust = 8.0),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  labs(subtitle = paste0("Week ",week.now," - 2022 \n Gecorrigeerd voor bevolkingsgroei, omvang, en vergrijzing"),
       caption = paste0("Bron: CBS | Datum: ",Sys.Date()," | Plot: @mzelst"))

ggsave("data-misc/excess_mortality/plots_weekly_update/oversterfte_provincie.png", width = 16, height = 12)
