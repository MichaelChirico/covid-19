require(rvest)

setwd("C:/Users/ZELST007/Desktop/covid-19")
monkeypox.nl <- fread("data-misc/monkeypox/monkeypox.csv")

monkeypox.nl <- monkeypox.nl %>%
  mutate(
    date = as.Date(Datum, tryFormats = c('%m/%d/%Y')),
    .before = Datum
  ) %>%
  mutate(Cumulatief = cumsum(Besmettingen)) %>%
  mutate(Datum = NULL)

rivm.webpage <- "https://www.rivm.nl/"

websites <- rivm.webpage %>%
  read_html() %>%
  html_nodes("a") %>%
  html_attr("href") 

websites.numbered <- grep("monkeypox",websites)

u <- paste0("https://www.rivm.nl",websites[websites.numbered[2]])

new.cumulative.cases <- read_html(u) %>%
  html_nodes("h1") %>%
  html_text() %>%
  parse_number()

new.daily.cases <- last(monkeypox.nl$Cumulatief-new.cumulative.cases)

new.obs <- data.frame(date = as.Date(Sys.Date()), Besmettingen = new.daily.cases, Cumulatief = new.cumulative.cases)

monkeypox.nl <- rbind(monkeypox.nl,new.obs)


title.monkeypox <- paste0("Cumulatief gemelde besmettingen met monkeypox in Nederland: ", max(monkeypox.nl$Cumulatief,na.rm=T))

Sys.setlocale("LC_TIME", "dutch")

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
        plot.caption = element_text(size = 12)) +
  labs(subtitle = "De datum van testafname is onbekend",
       caption = paste0("Bron: RIVM | Plot: @mzelst | Datum: ",as.Date(Sys.Date())))


