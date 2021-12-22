library(rvest)
u <- "https://www.rivm.nl/coronavirus-covid-19/virus/varianten/omikronvariant?s=09"

tables <- u %>% 
  read_html() %>%
  html_table(fill = TRUE)

dates <- seq(as.Date("2021-12-01"), as.Date(Sys.Date()), by="days")

synlab <- data.frame(tables[3])
synlab <- synlab[2:nrow(synlab),2:3]
synlab$date <- dates
synlab$lab <- "Synlab"
colnames(synlab) <- c("tests","positive_tests","date","lab")

saltro <- data.frame(tables[4])
saltro <- saltro[2:nrow(saltro),2:3]
saltro$date <- dates
saltro$lab <- "Saltro"
colnames(saltro) <- c("tests","positive_tests","date","lab")

ggd.amsterdam <- data.frame(tables[5])
ggd.amsterdam <- ggd.amsterdam[2:nrow(ggd.amsterdam),2:3]
ggd.amsterdam$date <- dates
ggd.amsterdam$lab <- "GGD_Amsterdam"
colnames(ggd.amsterdam) <- c("tests","positive_tests","date","lab")

total.df <- merge(synlab, saltro, by = c("date"))
total.df <- merge(total.df,  ggd.amsterdam, by = c("date"))


total.df <- rbind(ggd.amsterdam, saltro,synlab)

write.csv(total.df, file = "data-misc/variants-rivm/omikron/prevalence_omikron.csv",row.names = F)