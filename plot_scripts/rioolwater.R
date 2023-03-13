library(rgeos)
library(maptools)
library(raster)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(ggalt)
library(scales)
require(viridis)
library(geojsonio)

fileName <- "misc/maps/vrgrenzen2020.geojson" # File for GGD map
vrgrenzen <- geojson_read(fileName, what = "sp") # Load veiligheidsregio map

## Parse daily percentage positive tests - safety region ##

sewer.data.vr <- data.frame()

for (i in 1:25) {
  if(i<10){
    db <- fromJSON(txt=paste0("https://coronadashboard.rijksoverheid.nl/json/VR0",i,".json"))
  }else{
    db <- fromJSON(txt=paste0("https://coronadashboard.rijksoverheid.nl/json/VR",i,".json"))
  }
  db <- as.data.frame(db$sewer[1])
  VR_name <- ifelse(i < 10, paste0("VR0",i),paste0("VR",i))
  db$VR <- VR_name
  sewer.data.vr <- rbind(sewer.data.vr,db)
}



sewer.data.vr.test <- sewer.data.vr %>%
  mutate(date = as.Date(as.POSIXct(values.date_unix, origin = "1970-01-01"))) %>%
  group_by(VR) %>%
  mutate(sewer_7d = frollmean(values.average, 7))  %>%
  mutate(groei_riool = sewer_7d/(dplyr::lag(sewer_7d,7))) %>%
  mutate(groei_riool_7d = round(frollmean(groei_riool,7),2)) %>%
  mutate(groei_percentage = groei_riool_7d*100-100) %>%
  ungroup() %>%
  dplyr::filter(date == last(date)) %>%
  dplyr::select(date, VR, sewer_7d, groei_riool, groei_riool_7d,groei_percentage) %>%
  rename(statcode = VR)

vrgrenzen@data <- vrgrenzen@data %>%
  left_join(sewer.data.vr.test,by=c("statcode"))

g.vr <- fortify(vrgrenzen, region = "id")
vrDF <- merge(g.vr, vrgrenzen@data, by = "id")

vrDF$brks <- cut(vrDF$groei_percentage, 
                 breaks=c(-10000,-200,-100,-50,-25,0, 25, 50, 100, 200,10000), 
                   labels=c("-200% of lager","-100% tot 200%","-50% tot -100%", "-25% tot -50%","0% tot -25%","0% tot 25%", "25% tot 50%", 
                            "50% tot 100%", "100% tot 200%","200%+"))

centroids.df <- as.data.frame(coordinates(vrgrenzen))
names(centroids.df) <- c("long", "lat") 
popList <- vrgrenzen@data$groei_percentage

valueList <- round(vrgrenzen@data$sewer_7d,0)

idList <- vrgrenzen@data$statcode

pop.df <- data.frame(id = idList, rioolwaarde = valueList, groei_percentage = popList, centroids.df)


#colors <- c("-100% tot 0%" = "lightgreen", "0% tot 25%" = "yellow", "25% tot 50%" = "orange","50% tot 100%" = "magenta","100% tot 200%" = "red")

#colors <- c("-200% of lager" = "#86BADC","-100% tot 200%" = "#709DC8","-50% tot -100%" = "#517AC1", "-25% tot -50%" = "#4253A3", "0% tot -25%" = "#3F3FA7", "0% tot 25%" = "#4B3AAA", "25% tot 50%" = "#54298F","50% tot 100%" = "#742185","100% tot 200%" = "#891187", "200%+" = "#7B1251")

# RdBu colorbrewer
# colors <- c(
#   "-200% of lager" = "#053061",
#   "-100% tot 200%" = "#2166ac",
#   "-50% tot -100%" = "#4393c3", 
#   "-25% tot -50%" = "#92c5de", 
#   "0% tot -25%" = "#d1e5f0", 
#   "0% tot 25%" = "#fddbc7", 
#   "25% tot 50%" = "#f4a582",
#   "50% tot 100%" = "#d6604d",
#   "100% tot 200%" = "#b2182b", 
#   "200%+" = "#67001f")

# RdYlBu
# colors <- c(
#   "-200% of lager" = "#313695",
#   "-100% tot 200%" = "#4575b4",
#   "-50% tot -100%" = "#74add1", 
#   "-25% tot -50%" = "#abd9e9", 
#   "0% tot -25%" = "#e0f3f8", 
#   "0% tot 25%" = "#fee090", 
#   "25% tot 50%" = "#fdae61",
#   "50% tot 100%" = "#f46d43",
#   "100% tot 200%" = "#d73027", 
#   "200%+" = "#a50026")




#86BADC 23%, #709DC8 30%, #517AC1 36%, #4253A3 45%, #3F3FA7 51%, #4B3AAA 57%, #54298F 64%, #742185 70%, #891187 76%, #7B1251 83%, 


# Minimal colors: #F8FFFF 0%, #C9E5ED 8%, #A1CBDF 16%,
# Most extreme: #67062B 89%, #5A0712 95%, #2B0100 100%); 


sewer.vr.map <- ggplot(data = vrDF) +
  geom_polygon(aes(x=long, y=lat, group = group, fill = brks), color="#2b2b2b", size = 0.15) +
  coord_equal()+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.pos = "right",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_blank()) +
  labs(title = "Rioolwater per veiligheidsregio",
       subtitle = "Cijferwaardes: Gemiddelde aantal virusdeeltjes per 100.000 inwoners \n Kleuren: Relatieve groei/daling ten opzichte van vorige week",
       color = "Legend",
       caption = paste("Bron data: RIVM  | Plot: @Ipie33 | Datum laatst beschikbare data: ",format(last(sewer.data.vr.test$date),"%d-%b-%y"))) +
  scale_fill_brewer(type="div", 
                    direction =-1, 
                    aesthetics = c("colour", "fill"), 
                    palette = "RdBu", 
                    drop = FALSE, 
                    breaks = c("-200% of lager","-100% tot 200%","-50% tot -100%", "-25% tot -50%","0% tot -25%","0% tot 25%", "25% tot 50%", "50% tot 100%", "100% tot 200%","200%+"))

sewer.vr.map + geom_text(data=pop.df, aes(label=paste0(rioolwaarde), x=long, y=lat), size = 3.5, colour="black",fontface="bold") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        plot.title.position = "plot",
        plot.subtitle = element_text(hjust = 0.5, size = 14))

ggsave("plots/rioolwater_veiligheidsregio.png", width = 16, height = 8)


# automatic interpolated color brewer scales

sewer.vr.map <- ggplot(data = vrDF) +
  geom_polygon(aes(x=long, y=lat, group = group, fill = groei_percentage), color="#2b2b2b", size = 0.15) +
  coord_equal()+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.pos = "right",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_blank()) +
  labs(title = "Rioolwater per veiligheidsregio",
       subtitle = "Cijferwaardes: Gemiddelde aantal virusdeeltjes per 100.000 inwoners \n Kleuren: Relatieve groei/daling ten opzichte van vorige week",
       color = "Legend",
       caption = paste("Bron data: RIVM  | Plot: @Ipie33 | Datum laatst beschikbare data: ",format(last(sewer.data.vr.test$date),"%d-%b-%y"))) +
  scale_fill_distiller(type="div", aesthetics = c("colour", "fill"), palette = "RdBu", limits=c(-200,200))

sewer.vr.map + geom_text(data=pop.df, aes(label=paste0(rioolwaarde), x=long, y=lat), size = 3.5, colour="black",fontface="bold") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        plot.title.position = "plot",
        plot.subtitle = element_text(hjust = 0.5, size = 14))

ggsave("plots/rioolwater_veiligheidsregio2.png", width = 16, height = 8)


## Gemeente sewer

#gemeente.dat <- fromJSON("https://coronadashboard.rijksoverheid.nl/json/GM0867.json")


#fileName_gemeente <- "misc/maps/Gemeentegrenzen2022_RD.geojson"
#gemeentegrenzen <- geojson_read(fileName_gemeente, what = "sp")

#municipalities <- fread("misc/municipalities-population.csv")
#gemeente.codes <- unique(municipalities$Municipality_code)

#sewer.data.mun <- data.frame()

#for (gemeente in gemeente.codes) {
#  db.gemeente <- fromJSON(txt=paste0("https://coronadashboard.rijksoverheid.nl/json/",gemeente,".json"))
#
#  db.gemeente <- as.data.frame(db$sewer[1])
#  gemeente_code <- gemeente
#  db$statcode <- gemeente_code
#  sewer.data.mun <- rbind(sewer.data.mun,db)
#}


#sewer.data.mun.test <- sewer.data.mun %>%
#  mutate(date = as.Date(as.POSIXct(values.date_unix, origin = "1970-01-01"))) %>%
#  group_by(statcode) %>%
#  mutate(sewer_7d = frollmean(values.average, 7))  %>%
#  mutate(groei_riool = sewer_7d/(dplyr::lag(sewer_7d,7))) %>%
#  mutate(groei_riool_7d = round(frollmean(groei_riool,7),2)) %>%
#  mutate(groei_percentage = groei_riool_7d*100-100) %>%
#  ungroup() %>%
#  dplyr::filter(date == last(date)) %>%
#  dplyr::select(date, statcode, sewer_7d, groei_riool, groei_riool_7d,groei_percentage) %>%
#  rename(statcode = statcode)
                                   