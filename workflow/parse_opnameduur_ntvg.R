dat <- fread("https://raw.githubusercontent.com/mzelst/covid-19/master/data-nice/nice-today.csv")

dat <- dat %>%
  mutate(incoming = Hospital_Intake_Proven + Hospital_Intake_Suspected) %>%
  mutate(running_average_incoming = frollmean(incoming,7)) %>%
  mutate(total_beds_currently = Hospital_Currently) %>%
  mutate(running_average_total_beds = frollmean(total_beds_currently,7))


temp = list.files(path = "data-nice/exit/Clinical_Beds/",pattern="*.csv", full.names = T) ## Pull names of all available datafiles
dat.hospital <- fread(last(temp))
dat.hospital <- dat.hospital %>%
  mutate(outgoing_hospital = Overleden_pdag + Ontslagen_pdag)


dat.merge <- merge(dat,dat.hospital,by = "date")

dat.merge <- dat.merge %>%
  mutate(death_perc = (Overleden_pdag / dplyr::lag(total_beds_currently,1)) * 100) %>%
  mutate(running_average_death_perc = frollmean(death_perc,7)) %>%
  mutate(daily_incidence = outgoing_hospital / (dplyr::lag(total_beds_currently,1) - (0.5*outgoing_hospital) + (0.5*incoming))) %>%
  mutate(running_average_daily_incidence = frollmean(daily_incidence,7)) %>%
  mutate(average_hosp_stay_perday = 1/running_average_daily_incidence) %>%
  mutate(running_average_total_beds_sec_axis = running_average_total_beds / 100)


dat.merge %>%
  dplyr::filter(date >= "2021-01-01") %>%
  ggplot() + 
  theme_bw() +
  geom_line(aes(x = date, y = average_hosp_stay_perday),colour = "black", size = 1) +
  geom_bar(stat='identity', mapping = aes(x = date, y = running_average_total_beds_sec_axis), fill = "#ED7D31", alpha = 0.4) +
  scale_y_continuous(name = "Opnameduur (dagen)",limits = c(0, 30), labels = label_number(big.mark = ".", decimal.mark = ","),
                   sec.axis = sec_axis(~ . * 100, name="Bezetting kliniek")) +
  scale_x_date(date_breaks = "1 month", 
               date_labels= format("%m-%Y"),
               limits = as.Date(c("2021-01-01", NA))) +
  labs(x = "Maand") +
  ggtitle("Opnameduur en druk op de zorg") +
  theme(
    axis.title.y = element_text(color = "black", size=13),
    axis.title.y.right = element_text(color = "#ED7D31", size=13),
    axis.text.y.right = element_text(color = "#ED7D31"),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))
ggsave("plots/opnameduur_ntvg.png", width = 22,height = 12)
