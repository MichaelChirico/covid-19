u <- "https://www.cbs.nl/nl-nl/reeksen/sterfte-per-week"

webpage <- read_html(u)
urls <- webpage %>%
  html_nodes("a") %>%
  html_attr(("href"))

urls <- data.frame(urls)
urls$number <- gsub("[^0-9.]", "",  urls$urls)
urls$number <- substr(urls$number, 3, 8)
urls$category <- grepl("provincie", urls$urls, fixed = TRUE)
urls <- urls %>%
  filter(category == "TRUE") %>%
  filter(number >= 0) %>%
  mutate(year = substr(number, 1, 4)) %>%
  mutate(week = substr(number, 5, 6)) %>%
  setorder(year, week)
rm(webpage,u)
write.csv(urls, file = "data-misc/excess_mortality/links_cbs_mortality.csv",row.names = F)

## Download RIVM mortality graph

u <- "https://www.rivm.nl/monitoring-sterftecijfers-nederland"
webpage <- read_html(u)
imgsrc <- read_html(u) %>%
  html_node(xpath = '//*[@id="top"]/article/div[2]/div[2]/div/div/div/div[2]/div/div/img') %>%
  html_attr('src')

download.file(paste0("https://www.rivm.nl/",imgsrc), destfile="data-misc/excess_mortality/plots_weekly_update/sterfte_perweek_rivm.jpeg", mode="wb")




## Download RIVM excess mortality data ##


rivm_excess_data <- read.csv("data-misc/excess_mortality/excess_mortality_rivm.csv")

jaar <- as.numeric(data.table::year(Sys.Date()))
week <- data.table::week(Sys.Date())-2


start.week <- as.Date(Sys.Date()-15)
eind.week <- as.Date(Sys.Date()-8)

url <- "https://www.rivm.nl/monitoring-sterftecijfers-nederland"
webpage <- read_html(url)

webpage.text <- webpage %>%
  html_nodes("p") %>%
  html_text()

webpage.text <- webpage.text[2]
webpage.text <- gsub('[.]', '', webpage.text)

webpage.text.test <- parse_number(webpage.text)


matches <- regmatches(webpage.text, gregexpr("[[:digit:]]+", webpage.text))
numbers <- as.numeric(unlist(matches))

sterfte <- numbers[6]
ondergrens <- numbers[7]
bovengrens <- numbers[8]
midpoint <- (ondergrens+bovengrens)/2
oversterfte <- sterfte-midpoint

df.test <- as.data.frame(cbind(jaar, week, start.week, eind.week, ondergrens,bovengrens,sterfte,oversterfte,midpoint))

rivm.data.merge <- rbind(rivm_excess_data,df.test)
write.csv(rivm.data.merge, file = "data-misc/excess_mortality/excess_mortality_rivm.csv", row.names = F)
