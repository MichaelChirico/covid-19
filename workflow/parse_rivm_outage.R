u <- "https://www.rivm.nl/coronavirus-covid-19/actueel"

webshot(u, cliprect = c(10, 50, 950, 300), file = "plots/dagelijkse_storing.png")

webpage <- read_html(u)

webpage.text <- webpage %>%
  html_nodes("p") %>%
  html_text()


webpage.text <- webpage.text[4]
webpage.text <- gsub('[.]', '', webpage.text)
webpage.text.test <- parse_number(webpage.text)
matches <- regmatches(webpage.text, gregexpr("[[:digit:]]+", webpage.text))

days.since <- matches[[1]][2]
infections.missing <- matches[[1]][2]
dat.outage.today <- data.frame(as.Date(Sys.Date()),days.since,infections.missing)
colnames(dat.outage.today) <- c("date","days_since","infections_missed")
dat.outage.today$date <- as.Date(dat.outage.today$date)

dat.outage <- read.csv("data-misc/data_rivm_outages.csv")
dat.outage$date <- as.Date(dat.outage$date)
dat.outage <- rbind(dat.outage,dat.outage.today)

write.csv(dat.outage, file = "data-misc/data_rivm_outages.csv",row.names=F)


git.credentials <- read_lines("git_auth.txt")
git.auth <- cred_user_pass(git.credentials[1],git.credentials[2])

## Push to git
repo <- init()
add(repo, path = "data-misc/data_rivm_outages.csv")
commit(repo, all = T, paste0("[", last(dat.outage$date), "] Daily (automated) update RIVM data outage"))
push(repo, credentials = git.auth)