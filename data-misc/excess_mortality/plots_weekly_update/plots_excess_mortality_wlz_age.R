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
       y = "Overledenen per week") +
  scale_color_manual(name = "Group",values = colors, labels = NULL, guide = "none") +
  scale_fill_manual(values = colors, breaks = c("WLZ Zorggebruikers","Overige bevolking"))

ggsave(plot.wlz.other.sterfte, file = "data-misc/excess_mortality/plots_weekly_update/overledenen-per-week-wlz.png", width = 12, height = 8)

