require(rvest)
require(padr)
require(discordr)
require(git2r)
setwd("C:/Users/ZELST007/Desktop/covid-19")
#monkeypox.nl <- fread("data-misc/monkeypox/monkeypox_globaldothealth.csv")
source("workflow/twitter/token_mzelst.R")


monkeypox.nl <- fread("data-misc/monkeypox/monkeypox.csv")
monkeypox.nl <- monkeypox.nl %>%
  mutate(
    date = as.Date(date, tryFormats = c("%Y-%m-%d")),
    .before = date
  )

rivm.text <- "https://www.rivm.nl/monkeypox-apenpokken" %>%
  read_html() %>%
  html_nodes('.blockPadding') %>%
  html_text() %>%
  parse_number()


new.data <- data.frame("Besmettingen" = 0, "date" = as.Date(Sys.Date()), "Cumulatief" = rivm.text)

monkeypox.nl <- rbind(monkeypox.nl,new.data)

last.data <- last(monkeypox.nl$Cumulatief,2)
new.infections <- last.data[2] - last.data[1]


monkeypox.nl[nrow(monkeypox.nl),"Besmettingen"] <- new.infections
monkeypox.nl <- pad(monkeypox.nl)

monkeypox.nl <- monkeypox.nl %>%
  mutate(Cumulatief = tidyr::replace_na(Cumulatief, last.data[1])) %>%
  mutate(Besmettingen = tidyr::replace_na(Besmettingen, 0))



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
repo <- git2r::init()
add(repo, path = "*")
commit(repo, all = T, paste0("[", Sys.Date(), "] - Monkeypox update"))
push(repo, credentials = git.auth)
