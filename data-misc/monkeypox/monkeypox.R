require(rvest)
require(padr)
require(discordr)
require(git2r)
#setwd("C:/Users/Marino/Documents/covid-19")
#monkeypox.nl <- fread("data-misc/monkeypox/monkeypox_globaldothealth.csv")
source("workflow/twitter/token_mzelst.R")

## Loop for start ##

number.infections <- "https://www.rivm.nl/monkeypox-apenpokken" %>%
  read_html() %>%
  html_nodes('.card-outline-primary') %>%
  html_text() 
number.infections.old <- parse_number(number.infections[1])*1000

repeat {
  Sys.sleep(300)
  number.infections <- "https://www.rivm.nl/monkeypox-apenpokken" %>%
    read_html() %>%
    html_nodes('.card-outline-primary') %>%
    html_text() 
  number.infections.new <- parse_number(number.infections[1])*1000
  
  if (number.infections.old < number.infections.new){
    message <- "GO GO GO GO GO"
    break
  }
}

pull(repo)

monkeypox.nl <- fread("data-misc/monkeypox/monkeypox.csv")
monkeypox.nl <- monkeypox.nl %>%
  mutate(
    date = as.Date(date, tryFormats = c("%Y-%m-%d")),
    .before = date
  )

rivm.text <- "https://www.rivm.nl/monkeypox-apenpokken" %>%
  read_html() %>%
  html_nodes('.card-outline-primary') %>%
  html_text() %>%
  parse_number()
rivm.text <- rivm.text[1]*1000

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



## Plot based on DOO

#doo.monkeypox <- fread("C:/Users/marin/Downloads/meldingen-apenpokken.csv")

u <- "https://www.rivm.nl/monkeypox-apenpokken"
webpage <- read_html(u)


script <- webpage %>%
  html_nodes('script') %>%
  html_text()

dat.monkeypox <- script[3]
dat.monkeypox <- fromJSON(dat.monkeypox)
dat.monkeypox <- fromJSON(dat.monkeypox$easychart$`764271-0-field_par_chart`$config)

doo.monkeypox <- data.frame(dat.monkeypox$series$data)
doo.monkeypox <- doo.monkeypox[,c(1,2,4,6)]

colnames(doo.monkeypox) <- c("date","unknown","before_endjune","after_endjune")
doo.monkeypox$infections <- rowSums(doo.monkeypox[,c("unknown","before_endjune","after_endjune")],na.rm=T)


doo.monkeypox <- doo.monkeypox %>%
  mutate(
    date = as.Date(date, tryFormats = c("%d-%m-%Y")),
    .before = date
  ) %>%
  #mutate(infections = before_endjune+after_endjune) %>%
  mutate(infections_7d = frollmean(infections,7))


rows.monkeypox <- nrow(doo.monkeypox)

doo.monkeypox[(rows.monkeypox-9):rows.monkeypox,6] <- NA

doo.monkeypox.MA <- doo.monkeypox %>%
  drop_na(infections_7d) %>%
  last()

title.monkeypox.doo <- paste0("Monkeypox besmettingen op basis van eerste ziektedag")

monkeypox.plot.doo <- doo.monkeypox %>%
  dplyr::filter(date() >= "2022-05-17") %>%
  ggplot() + 
  geom_col(aes(x = date, y = infections)) +
  geom_line(aes(x = date, y = infections_7d), color = "red",lwd = 1.6) + 
  ggtitle(title.monkeypox.doo) +
  theme_bw() +
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
  labs(subtitle = paste0("Totale aantal besmettingen: ",last(monkeypox.nl$Cumulatief),"
  Zevendaagse gemiddelde: ", round(doo.monkeypox.MA$infections_7d,1),"
  Let op (!): Monkeypox kent een lange incubatietijd (~8.5 dagen, interval: 6.6 - 10.9 dagen) waardoor de gegevens voor de afgelopen twee weken nog incompleet zijn."),
       caption = paste0("Bron: RIVM | Plot: @mzelst | Datum: ",as.Date(Sys.Date()))) +
  ylim(0, max(doo.monkeypox$infections)+5) +
  xlim(min(doo.monkeypox$date)-2, Sys.Date())
monkeypox.plot.doo

ggsave(monkeypox.plot.doo, file = "plots/monkeypox_doo.png",width = 16, height = 8)


doo.monkeypox.growth <- doo.monkeypox %>%
  drop_na(infections_7d) %>%
  mutate(groei_monkeypox = infections_7d/(dplyr::lag(infections_7d,7)))

monkeypox.growth <- last(doo.monkeypox.growth$infections_7d,8)
growth.text <- ifelse(monkeypox.growth[8]/monkeypox.growth[1]>1,"toe","af")

tweet.monkeypox <- paste0("#Monkeypox

Er zijn ",new.infections," nieuwe monkeypox besmettingen met vastgesteld in de afgelopen week. Het zevendaags gemiddelde neemt ",growth.text," (",round(doo.monkeypox.MA$infections_7d,1)," per dag).

Tot nu toe zijn er in totaal ",last(monkeypox.nl$Cumulatief)," besmettingen vastgesteld. 

Voor meer informatie, zie de website van het RIVM: https://rivm.nl/monkeypox-apenpokken")

posted_tweet <- post_tweet (
  tweet.monkeypox,
  token = token.mzelst,
  media = "plots/monkeypox_doo.png")

posted_tweet <- fromJSON(rawToChar(posted_tweet$content))
tweet.main.id.monkeypox <- posted_tweet$id_str

## Region

rivm.monkeypox.table <- "https://www.rivm.nl/monkeypox-apenpokken" %>%
  read_html() %>%
  html_table()

monkeypox.region <- data.frame(do.call(rbind,rivm.monkeypox.table),"Datum" =as.character(Sys.Date()))


monkeypox.region.dat <- read.csv("data-misc/monkeypox/monkeypox_region.csv")[,1:3]
monkeypox.region.dat <- rbind(monkeypox.region.dat,monkeypox.region)

monkeypox.region.dat <- monkeypox.region.dat %>%
  mutate(
    Datum = as.Date(Datum, tryFormats = c("%Y-%m-%d"))) %>%
  arrange(Datum) %>%
  group_by(Regio) %>%
  mutate(toename_monkeypox = c(0,diff(Aantallen)))


fwrite(monkeypox.region.dat, file = "data-misc/monkeypox/monkeypox_region.csv")

monkeypox.region.dat.latest <- monkeypox.region.dat %>%
  dplyr::filter(Datum == Sys.Date())

tweet.monkeypox.regio <- paste0("#Monkeypox - Besmettingen per regio (totaal en toename)

",monkeypox.region.dat.latest[1,1],": ",monkeypox.region.dat.latest[1,2]," (+",monkeypox.region.dat.latest[1,4],")
",monkeypox.region.dat.latest[2,1],": ",monkeypox.region.dat.latest[2,2]," (+",monkeypox.region.dat.latest[2,4],")
",monkeypox.region.dat.latest[3,1],": ",monkeypox.region.dat.latest[3,2]," (+",monkeypox.region.dat.latest[3,4],")
",monkeypox.region.dat.latest[4,1],": ",monkeypox.region.dat.latest[4,2]," (+",monkeypox.region.dat.latest[4,4],")
",monkeypox.region.dat.latest[5,1],": ",monkeypox.region.dat.latest[5,2]," (+",monkeypox.region.dat.latest[5,4],")
",monkeypox.region.dat.latest[6,1],": ",monkeypox.region.dat.latest[6,2]," (+",monkeypox.region.dat.latest[6,4],")
",monkeypox.region.dat.latest[7,1],": ",monkeypox.region.dat.latest[7,2]," (+",monkeypox.region.dat.latest[7,4],")")

post_tweet (
  tweet.monkeypox.regio,
  token = token.mzelst,
  in_reply_to_status_id = tweet.main.id.monkeypox)

## Push to GIT

git.credentials <- read_lines("git_auth.txt")
git.auth <- cred_user_pass(git.credentials[1],git.credentials[2])

## Push to git
repo <- git2r::init()
add(repo, path = "*")
commit(repo, all = T, paste0("[", Sys.Date(), "] - Monkeypox update"))
push(repo, credentials = git.auth)
