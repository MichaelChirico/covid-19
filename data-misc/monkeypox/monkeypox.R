require(rvest)
require(padr)
require(discordr)
require(git2r)
setwd("C:/Users/ZELST007/Desktop/covid-19")
monkeypox.nl <- fread("data-misc/monkeypox/monkeypox_globaldothealth.csv")
source("workflow/twitter/token_mzelst.R")


monkeypox.nl <- monkeypox.nl %>%
  mutate(
    Date_confirmation = as.Date(Date_confirmation, tryFormats = c('%m/%d/%Y')),
    .before = Date_confirmation
  ) %>%
  mutate(Besmettingen = 1)

write.csv(monkeypox.nl, file = "data-misc/monkeypox/monkeypox_globaldothealth.csv",row.names=F)

monkeypox.nl <- aggregate(Besmettingen ~ Date_confirmation, data = monkeypox.nl, FUN = sum)
monkeypox.nl <- pad(monkeypox.nl)
monkeypox.nl <- monkeypox.nl %>%
  mutate(date = Date_confirmation) %>%
  mutate(Date_confirmation = NULL) %>%
  mutate(Besmettingen = ifelse(is.na(Besmettingen),0,Besmettingen)) %>%
  mutate(Cumulatief = cumsum(Besmettingen))


title.monkeypox <- paste0("Cumulatief gemelde besmettingen met monkeypox in Nederland: ", max(monkeypox.nl$Cumulatief,na.rm=T))

Sys.setlocale("LC_TIME", "dutch")

monkeypox.plot <- monkeypox.nl %>%
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
       caption = paste0("Bron: RIVM | Plot: @mzelst | Datum: ",as.Date(Sys.Date()))) +
  ylim(0, max(monkeypox.nl$Cumulatief)+5) +
  xlim(min(monkeypox.nl$date)-2, Sys.Date() + 1)
monkeypox.plot
write.csv(monkeypox.nl, file = "data-misc/monkeypox/monkeypox.csv", row.names = F)

ggsave(monkeypox.plot, file = "plots/monkeypox.png",width = 16, height = 8)


rivm.text <- "https://www.rivm.nl/monkeypox-apenpokken" %>%
  read_html() %>%
  html_nodes('.blockPadding') %>%
  html_text()

tweet.monkeypox <- paste0("#Monkeypox

",rivm.text,"

Voor meer informatie, zie de website van het RIVM: https://rivm.nl/monkeypox-apenpokken")

post_tweet (
  tweet.monkeypox,
  token = token.mzelst,
  media = "plots/monkeypox.png")


git.credentials <- read_lines("git_auth.txt")
git.auth <- cred_user_pass(git.credentials[1],git.credentials[2])

## Push to git
repo <- init()
add(repo, path = "*")
commit(repo, all = T, paste0("[", Sys.Date(), "] - Monkeypox update"))
push(repo, credentials = git.auth)