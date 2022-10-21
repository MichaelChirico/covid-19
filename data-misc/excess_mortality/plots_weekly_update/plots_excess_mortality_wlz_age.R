### WLZ PLOT ###

urls <- read.csv("data-misc/excess_mortality/links_cbs_mortality.csv")

cbs_url <- last(urls)
page <- read_html(cbs_url[1,1])
page <- page %>% html_nodes("a") %>% html_attr('href')
page <- data.frame(page)

page$category <- grepl(".xlsx", page$page, fixed = TRUE)
page <- page %>%
  dplyr::filter(category == "TRUE")
cbs_url <- page[1,1]

download.file(cbs_url,destfile = "cbs_deaths.xlsx", mode = "wb")


cbs_sterfte <- data.table(read_excel("cbs_deaths.xlsx", sheet = 6))[,c(1,2,5,10)]
cbs_sterfte <- cbs_sterfte[6:nrow(cbs_sterfte),]
#cbs_sterfte <- cbs_sterfte[c(1:4)]

## Select indices ##
test <- row_count(cbs_sterfte, count = NA)
rows.2020 <- which(test$rowcount == 4)[3]
rows.2021 <- which(test$rowcount == 4)[2]
rows.2022 <- which(test$rowcount == 4)[1]

sterfte.2022 <- cbs_sterfte[(rows.2022+1):(rows.2021-1),]
sterfte.2022 <- sterfte.2022[-((rows.2021-4):(rows.2021-3)),]
colnames(sterfte.2022) <- c("Jaar","Week","Sterfte_Wlz","Sterfte_Other")
sterfte.2022 <- sterfte.2022 %>%
  mutate(Week = (1:nrow(sterfte.2022))) %>%
  mutate(Jaar = 2022)
setDF(sterfte.2022)

sterfte.2021 <- cbs_sterfte[(rows.2021+1):(rows.2020-1),]
colnames(sterfte.2021) <- c("Jaar","Week","Sterfte_Wlz","Sterfte_Other")
sterfte.2021 <- sterfte.2021 %>%
  mutate(Week = parse_number(Week)) %>%
  mutate(Jaar = 2021)
setDF(sterfte.2021)
sterfte.2021["Week"][sterfte.2021["Week"] == 524] <- 52

sterfte.2020 <- cbs_sterfte[(rows.2020+2):(nrow(cbs_sterfte)-7),]
colnames(sterfte.2020) <- c("Jaar","Week","Sterfte_Wlz","Sterfte_Other")
sterfte.2020 <- sterfte.2020 %>%
  mutate(Week = parse_number(Week)) %>%
  mutate(Jaar = 2020)
setDF(sterfte.2020)
sterfte.2020["Week"][sterfte.2020["Week"] == 533] <- 53

sterfte_wlz_other <- rbind(sterfte.2020,sterfte.2021,sterfte.2022)


#### Sterfte verwacht ####
cbs_sterfte <- data.table(read_excel("cbs_deaths.xlsx", sheet = 7))[8:166,c(1,5:10)]
colnames(cbs_sterfte) <- c("week_year","wlz_verwacht","wlz_verwacht_lb","wlz_verwacht_ub","other_verwacht","other_verwacht_lb","other_verwacht_ub")

cbs_sterfte <- cbs_sterfte %>%
  mutate(Jaar = parse_number(substr(week_year, 1, 4))) %>%
  mutate(Week = parse_number(substr(week_year, 11, 12))) %>%
  mutate(week_year = NULL) %>%
  mutate_all(function(x) parse_number(as.character(x)))

setDF(cbs_sterfte)

cbs_deaths <- merge(sterfte_wlz_other, cbs_sterfte,by=c("Week","Jaar"), all.y=T)
wlz.table <- cbs_deaths %>%
  arrange(Jaar,Week) %>%
  mutate_all(function(x) parse_number(as.character(x))) %>%
  mutate(weekyear = ifelse(Week<10,
                           paste0(Jaar,"-",0,Week),
                           paste0(Jaar,"-",Week))) %>%
  mutate(date = ymd(paste0(Jaar,"-01-01")) + weeks(Week))

wlz.table <- wlz.table[11:(nrow(wlz.table) - 52 + last(sterfte_wlz_other$Week) + 6),]

colors <- c("WLZ_ci" = "lightgreen", "Other_ci" = "lightblue", "WLZ Zorggebruikers" = "darkgreen","Overige bevolking" = "blue","WLZ_Verwacht" = "black","Other_Verwacht" = "black")

week.cbs.plots <- isoweek(Sys.Date())-1

plot.wlz.other.sterfte <- wlz.table %>%
  ggplot(aes(x=date)) + 
  geom_ribbon(aes(ymin=wlz_verwacht_lb,ymax=wlz_verwacht_ub, fill="WLZ_ci"), alpha = 0.6) +
  geom_ribbon(aes(ymin=other_verwacht_lb,ymax=other_verwacht_ub, fill="Other_ci"), alpha = 0.6) +
  geom_line(aes(y = Sterfte_Wlz, color = "WLZ Zorggebruikers"), lwd=1.2) +
  geom_line(aes(y = wlz_verwacht, color = "WLZ_Verwacht"),lwd=1.0) +
  geom_line(aes(y = Sterfte_Other, color = "Overige bevolking"), lwd=1.2) +
  geom_line(aes(y = other_verwacht, color = "Other_Verwacht"),lwd=1.0) +
  ggtitle("Overledenen per week, naar Wlz-zorggebruik") + 
  theme_bw() + 
  scale_y_continuous(expand = c(0,0), limits = c(0, 3000), breaks = c(0,500,1000,1500,2000,2500,3000)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") + 
  theme(axis.title.x=element_blank(),
        #axis.title.y=element_blank(),
        axis.text.x.bottom = element_text(size=10, angle = 90),
        axis.text.y = element_text(size=10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        plot.title.position = "plot",
        plot.caption = element_text(size = 6),
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.text = element_text(size=10, color = "black"),
        legend.margin = margin(2,2,2,2)) +
  labs(x = "Datum",
       y = "Overledenen per week",
       subtitle = paste0("Data t/m week ",week.cbs.plots," - 2022")) +
  scale_color_manual(name = "Group",values = colors, labels = NULL, guide = "none") +
  scale_fill_manual(values = colors, breaks = c("WLZ Zorggebruikers","Overige bevolking"))

ggsave(plot.wlz.other.sterfte, file = "data-misc/excess_mortality/plots_weekly_update/overledenen-per-week-wlz.png", width = 12, height = 8)




### AGE PLOT ###

urls <- read.csv("data-misc/excess_mortality/links_cbs_mortality.csv")

cbs_url <- last(urls)
page <- read_html(cbs_url[1,1])
page <- page %>% html_nodes("a") %>% html_attr('href')
page <- data.frame(page)

page$category <- grepl(".xlsx", page$page, fixed = TRUE)
page <- page %>%
  dplyr::filter(category == "TRUE")
cbs_url <- page[1,1]

download.file(cbs_url,destfile = "cbs_deaths.xlsx", mode = "wb")


cbs_sterfte_leeftijd <- data.table(read_excel("cbs_deaths.xlsx", sheet = 6))[,c(1,2,6:8,11:13)]
cbs_sterfte_leeftijd <- cbs_sterfte_leeftijd[7:nrow(cbs_sterfte_leeftijd),]
colnames(cbs_sterfte_leeftijd) <- c("Jaar","Week","sterfte_wlz_0_65","sterfte_wlz_65_80","sterfte_wlz_80","sterfte_other_0_65","sterfte_other_65_80","sterfte_other_80")


cbs_sterfte_leeftijd <- cbs_sterfte_leeftijd %>%
  mutate_all(function(x) parse_number(as.character(x))) %>%
  mutate(sterfte_0_65 = sterfte_wlz_0_65 + sterfte_other_0_65) %>%
  mutate(sterfte_65_80 = sterfte_wlz_65_80 + sterfte_other_65_80) %>%
  mutate(sterfte_80 = sterfte_wlz_80 + sterfte_other_80) %>%
  dplyr::select(Jaar,Week,sterfte_0_65,sterfte_65_80,sterfte_80)
#cbs_sterfte <- cbs_sterfte[c(1:4)]

## Select indices ##
test <- row_count(cbs_sterfte_leeftijd, count = NA)
rows.2020.age <- which(test$rowcount == 5)[3]
rows.2021.age <- which(test$rowcount == 5)[2]
rows.2022.age <- which(test$rowcount == 4)[1]

sterfte.2022.leeftijd <- cbs_sterfte_leeftijd[1:(rows.2022.age-1),]
colnames(sterfte.2022.leeftijd) <- c("Jaar","Week","Sterfte_0_65","Sterfte_65_80","Sterfte_80")
sterfte.2022.leeftijd <- sterfte.2022.leeftijd %>%
  mutate(Week = (1:nrow(sterfte.2022.leeftijd))) %>%
  mutate(Jaar = 2022)
setDF(sterfte.2022.leeftijd)

sterfte.2021.leeftijd <- cbs_sterfte_leeftijd[(rows.2021.age+1):(rows.2020.age-1),]
colnames(sterfte.2021.leeftijd) <- c("Jaar","Week","Sterfte_0_65","Sterfte_65_80","Sterfte_80")
sterfte.2021.leeftijd <- sterfte.2021.leeftijd %>%
  mutate(Jaar = 2021)
setDF(sterfte.2021.leeftijd)
sterfte.2021.leeftijd["Week"][sterfte.2021.leeftijd["Week"] == 524] <- 52

sterfte.2020.leeftijd <- cbs_sterfte_leeftijd[(rows.2020.age+2):(nrow(cbs_sterfte_leeftijd)-7),]
colnames(sterfte.2020.leeftijd) <- c("Jaar","Week","Sterfte_0_65","Sterfte_65_80","Sterfte_80")
sterfte.2020.leeftijd <- sterfte.2020.leeftijd %>%
  mutate(Jaar = 2020)
setDF(sterfte.2020.leeftijd)
sterfte.2020.leeftijd["Week"][sterfte.2020.leeftijd["Week"] == 533] <- 53

sterfte_leeftijd <- rbind(sterfte.2020.leeftijd,sterfte.2021.leeftijd,sterfte.2022.leeftijd)

## Add current week from other CBS data source

table_mortality <- data.table(cbs_get_data("70895ned", 
                                           Geslacht = "1100",
                                           Perioden = has_substring("W") | has_substring("X")))

table_mortality$Jaar <- substr(table_mortality$Perioden, 1, 4)

table_mortality$Week <- parse_number(str_sub(table_mortality$Perioden, start = -2))

current.week.mort.age <- isoweek(Sys.Date())-1
current.jaar.mort.age <- isoyear(Sys.Date())

mortality.test <- table_mortality %>%
  dplyr::filter(Jaar == current.jaar.mort.age) %>%
  dplyr::filter(Week == current.week.mort.age) %>%
  dplyr::filter(LeeftijdOp31December != 10000) %>%
  dplyr::select(Jaar, Week, Overledenen_1)

sterfte_leeftijd_currentweek <- c(current.jaar.mort.age, current.week.mort.age, t(mortality.test$Overledenen_1))

sterfte_leeftijd <- rbind(sterfte_leeftijd,sterfte_leeftijd_currentweek)



#### Sterfte verwacht ####
cbs_sterfte_leeftijd_verwacht <- data.table(read_excel("cbs_deaths.xlsx", sheet = 7))[8:166,c(1,19:27)]
colnames(cbs_sterfte_leeftijd_verwacht) <- c("week_year","verwacht_0_65","verwacht_0_65_lb","verwacht_0_65_ub","verwacht_65_80","verwacht_65_80_lb","verwacht_65_80_ub",
                                             "verwacht_80","verwacht_80_lb","verwacht_80_ub")

cbs_sterfte_leeftijd_verwacht <- cbs_sterfte_leeftijd_verwacht %>%
  mutate(Jaar = parse_number(substr(week_year, 1, 4))) %>%
  mutate(Week = parse_number(substr(week_year, 11, 12))) %>%
  mutate(week_year = NULL) %>%
  mutate_all(function(x) parse_number(as.character(x)))

setDF(cbs_sterfte_leeftijd_verwacht)

age.table <- merge(sterfte_leeftijd, cbs_sterfte_leeftijd_verwacht,by=c("Week","Jaar"), all.y=T)
age.table <- age.table %>%
  arrange(Jaar,Week) %>%
  mutate_all(function(x) parse_number(as.character(x))) %>%
  mutate(weekyear = ifelse(Week<10,
                           paste0(Jaar,"-",0,Week),
                           paste0(Jaar,"-",Week))) %>%
  mutate(date = ymd(paste0(Jaar,"-01-01")) + weeks(Week))

age.table <- age.table[11:(nrow(age.table) - 52 + last(sterfte_leeftijd$Week) + 6),]


colors <- c("ci_65_80" = "lightgreen", "ci_0_65" = "lightblue", "65 tot 80" = "darkgreen","0 tot 65" = "blue",
            "ci_80" = "lightpink1","80+" = "palevioletred3","Verwacht_65_80" = "black","Verwacht_0_65" = "black","Verwacht_80" = "black")

plot.age.sterfte <- age.table %>%
  ggplot(aes(x=date)) + 
  geom_ribbon(aes(ymin=verwacht_80_lb,ymax=verwacht_80_ub, fill="ci_80"), alpha = 0.6) +
  geom_ribbon(aes(ymin=verwacht_65_80_lb,ymax=verwacht_65_80_ub, fill="ci_65_80"), alpha = 0.6) +
  geom_ribbon(aes(ymin=verwacht_0_65_lb,ymax=verwacht_0_65_ub, fill="ci_0_65"), alpha = 0.6) +
  geom_line(aes(y = Sterfte_80, color = "80+"), lwd=1.2) +
  geom_line(aes(y = verwacht_80 , color = "Verwacht_80"),lwd=1.0) +
  geom_line(aes(y = Sterfte_65_80, color = "65 tot 80"), lwd=1.2) +
  geom_line(aes(y = verwacht_65_80 , color = "Verwacht_65_80"),lwd=1.0) +
  geom_line(aes(y = Sterfte_0_65, color = "0 tot 65"), lwd=1.2) +
  geom_line(aes(y = verwacht_0_65, color = "Verwacht_0_65"),lwd=1.0) +
  ggtitle("Overledenen per week, naar leeftijd") + 
  theme_bw() + 
  scale_y_continuous(expand = c(0,0), limits = c(0, 3500), breaks = c(0,500,1000,1500,2000,2500,3000,3500)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") + 
  theme(axis.title.x=element_blank(),
        #axis.title.y=element_blank(),
        axis.text.x.bottom = element_text(size=10, angle = 90),
        axis.text.y = element_text(size=10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        plot.title.position = "plot",
        plot.caption = element_text(size = 6),
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.text = element_text(size=10, color = "black"),
        legend.margin = margin(2,2,2,2)) +
  labs(x = "Datum",
       y = "Overledenen per week",
       subtitle = paste0("Data t/m week ",week.cbs.plots," - 2022"),
       caption = paste("Bron data: CBS  | Plot: @mzelst | ",Sys.Date())) + 
  scale_color_manual(name = "Group",values = colors, labels = NULL, guide = "none") +
  scale_fill_manual(values = colors, breaks = c("80+","65 tot 80","0 tot 65"))

ggsave(plot.age.sterfte, file = "data-misc/excess_mortality/plots_weekly_update/overledenen-per-week.png", width = 12, height = 8)
## Match names in scale_fill_manual with dataframe


