require(ggpubr)

zkh_new <- rjson::fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/zkh/new-intake/",simplify = TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill=TRUE) %>%
  slice(1,2,4) %>%
  t() %>% as.data.frame() %>%
  mutate_all(unlist) %>%
  rename(date = V1,new_hosp_proven = V2, new_hosp_suspected = V3) %>%
  mutate(week = isoweek(date)) %>%
  mutate(year = isoyear(date))

zkh_new <- aggregate(new_hosp_proven ~ week + year, data = zkh_new, FUN = sum)  
setDT(zkh_new)


reason_intake <- rjson::fromJSON(file = "https://www.stichting-nice.nl//covid-19/public/zkh/reason-for-intake-per-week/",simplify = TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill = TRUE) %>%
  slice(1,2,4,6) %>%
  t() %>% as.data.frame() %>%
  mutate_all(unlist) %>%
  mutate(year = parse_number(paste0("20",parse_number(V1)))) %>%
  rename(week = V1,covid_primair = V2, covid_secundair = V3, met_covid = V4) %>%
  mutate_at(vars(covid_primair,covid_secundair,met_covid), ~. / 100) %>%
  mutate(week = parse_number(str_sub(week, 8, 10)))

setDT(reason_intake)

nice.reason.intake <- merge(reason_intake, zkh_new, by = c("week","year"), all.x=T)

nice.reason.intake <- nice.reason.intake %>%
  mutate_at(vars(covid_primair,covid_secundair,met_covid), ~. * new_hosp_proven) %>%
  mutate(new_hosp_proven = NULL)

nice.reason.intake.long <- melt(nice.reason.intake, id.vars = c("week","year"),variable.name = "type")
reason.intake.perc <- melt(reason_intake, id.vars = c("week","year"),variable.name = "type")
reason.intake.perc$percentage <- paste0(round(reason.intake.perc$value*100,0),"%")
reason.intake.perc$value <- NULL

nice.reason.intake.long <- merge(nice.reason.intake.long, reason.intake.perc, by = c("week","year","type"))

nice.reason.intake.long <- nice.reason.intake.long %>%
  mutate(type = factor(type, rev(unique(type))))

nice.reason.intake.long$weekyear <- ifelse(nice.reason.intake.long$week<10,
                             paste0(nice.reason.intake.long$year,"-",0,nice.reason.intake.long$week),
                             paste0(nice.reason.intake.long$year,"-",nice.reason.intake.long$week))

week.data <- last(nice.reason.intake.long$week)

colors <- c("covid_primair" = "blue", "covid_secundair" = "steelblue","met_covid" = "lightblue")
plot.reason.hospital <- nice.reason.intake.long %>%
  arrange(value) %>%
  ggplot(aes(x = factor(weekyear), y = value, fill = type)) + 
  geom_bar(stat = 'identity', position = 'stack', alpha = 0.8) + 
  theme_bw() + 
  #scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") + 
  theme(#axis.title.x=element_blank(),
    #axis.title.y=element_blank(),
    axis.text.x.bottom = element_text(size=10),
    axis.text.y = element_text(size=10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    plot.title.position = "plot",
    plot.caption = element_text(size = 6),
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size=10, color = "black"),
    legend.margin = margin(2,2,2,2)) +
  labs(x = "Week",
       y = "Aantal opnames",
       caption = "") +
  #scale_x_continuous(n.breaks = nrow(nice.reason.intake.long)/2) +
  scale_fill_manual(labels = c("Door COVID","Met COVID - met ontregeling","Met COVID - zonder ontregeling"),
                    values = colors, guide = guide_legend(reverse = TRUE)) +
  geom_text(aes(label = percentage), position = position_stack(vjust = 0.5), fontface = "bold")

plot.reason.hospital


ic_new <- rjson::fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/new-intake/",simplify = TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill=TRUE) %>%
  slice(1,2,4) %>%
  t() %>% as.data.frame() %>%
  mutate_all(unlist) %>%
  rename(date = V1,new_ic_proven = V2, new_ic_suspected = V3) %>%
  mutate(week = isoweek(date)) %>%
  mutate(year = isoyear(date))

ic_new <- aggregate(new_ic_proven ~ week + year, data = ic_new, FUN = sum)  
setDT(ic_new)


reason_intake_ic <- rjson::fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/reason-for-intake-per-week/",simplify = TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill = TRUE) %>%
  slice(1,2,4,6) %>%
  t() %>% as.data.frame() %>%
  mutate_all(unlist) %>%
  mutate(year = parse_number(paste0("20",parse_number(V1)))) %>%
  rename(week = V1,covid_primair = V2, covid_secundair = V3, met_covid = V4) %>%
  mutate_at(vars(covid_primair,covid_secundair,met_covid), ~. / 100) %>%
  mutate(week = parse_number(str_sub(week, 8, 10)))
setDT(reason_intake_ic)

nice.reason.intake.ic <- merge(reason_intake_ic, ic_new, by = c("week","year"), all.x=T)


nice.reason.intake.ic <- nice.reason.intake.ic %>%
  mutate_at(vars(covid_primair,covid_secundair,met_covid), ~. * new_ic_proven) %>%
  mutate(new_ic_proven = NULL)

nice.reason.intake.ic.long <- melt(nice.reason.intake.ic, id.vars = c("week","year"),variable.name = "type")
reason.intake.ic.perc <- melt(reason_intake_ic, id.vars = c("week","year"),variable.name = "type")
reason.intake.ic.perc$percentage <- paste0(round(reason.intake.ic.perc$value*100,0),"%")
reason.intake.ic.perc$value <- NULL

nice.reason.intake.ic.long <- merge(nice.reason.intake.ic.long, reason.intake.ic.perc, by = c("week","year","type"))

nice.reason.intake.ic.long <- nice.reason.intake.ic.long %>%
  mutate(type = factor(type, rev(unique(type))))

nice.reason.intake.ic.long$weekyear <- ifelse(nice.reason.intake.ic.long$week<10,
                                           paste0(nice.reason.intake.ic.long$year,"-",0,nice.reason.intake.ic.long$week),
                                           paste0(nice.reason.intake.ic.long$year,"-",nice.reason.intake.ic.long$week))

plot.reason.ic <- nice.reason.intake.ic.long %>%
  arrange(value) %>%
  ggplot(aes(x = factor(weekyear), y = value, fill = type)) + 
  geom_bar(stat = 'identity', position = 'stack', alpha = 0.8) + 
  theme_bw() + 
  #scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") + 
  theme(#axis.title.x=element_blank(),
    #axis.title.y=element_blank(),
    axis.text.x.bottom = element_text(size=10),
    axis.text.y = element_text(size=10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    plot.title.position = "plot",
    plot.caption = element_text(size = 6),
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.position = "right",
    legend.text = element_text(size=10, color = "black"),
    legend.margin = margin(2,2,2,2)) +
  labs(x = "Week",
       y = "Aantal IC-opnames",
       caption = paste0("Bron: Stichting NICE | Plot: @mzelst | ",Sys.Date())) +
  #scale_x_continuous(n.breaks = nrow(nice.reason.intake.ic.long)/2) +
  scale_fill_manual(labels = c("Door COVID","Met COVID - met ontregeling","Met COVID - zonder ontregeling"),
                    values = colors, guide = guide_legend(reverse = TRUE)) +
  geom_text(aes(label = percentage), position = position_stack(vjust = 0.5), fontface = "bold")



reason.figure <- ggarrange(plot.reason.hospital,plot.reason.ic, labels = c("Kliniek","IC"),
                           ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom", label.x = c(0.4,0.5),label.y = c(0.98), font.label = list(size = 20))

reason.figure

#explainer.bottom <- expression(atop(
#  scriptstyle("Percentage is relatieve aandeel per \n opnamereden voor die week \n Let op: De data van de laatste twee weken zijn nog niet compleet")))

reason.intake.figure <- annotate_figure(reason.figure, top=text_grob("Reden van opname", size = 18, face = "bold"))#, bottom = text_grob(explainer.bottom,size = 16, hjust = 1.8))
reason.intake.figure
ggsave(reason.intake.figure, file ="plots/reden_van_opname.png",width = 12, height = 8)


