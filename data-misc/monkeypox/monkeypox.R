setwd("C:/Users/ZELST007/Desktop/covid-19")
monkeypox.nl <- fread("data-misc/monkeypox/monkeypox.csv")

monkeypox.nl <- monkeypox.nl %>%
  mutate(
    date = as.Date(Datum, tryFormats = c('%m/%d/%Y')),
    .before = Datum
  ) %>%
  mutate(
    Datum = NULL
  ) 

title.monkeypox <- paste0("Cumulatief gemelde besmettingen met monkeypox in Nederland: ", max(monkeypox.nl$Cumulatief))

monkeypox.nl %>%
  dplyr::filter(date() >= "2022-05-17") %>%
  ggplot() + 
  geom_col(aes(x = date, y = Cumulatief)) +
  ggtitle(title.monkeypox) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x.bottom = element_text(size=14),
        axis.text.y = element_text(size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.title.position = "plot",
        plot.caption = element_text(size = 6)) +
  labs(subtitle = "De datum van testafname is onbekend \n Ik rapporteer het aantal zoals het door het RIVM wordt gemeld per dag")

