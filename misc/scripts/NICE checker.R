#### NICE CHECKER ####

source("workflow/parse_nice-data.R")

temp = tail(list.files(path = "data-nice/data-nice-json/",pattern="*.csv", full.names = T),4)
myfiles = lapply(temp, read.csv)

dat.today <- as.data.frame(myfiles[4])
dat.yesterday <- as.data.frame(myfiles[3])
dat.twodaysago <- as.data.frame(myfiles[2])
dat.threedaysago <- as.data.frame(myfiles[1])

sum(dat.today$Hospital_Intake_Proven) - sum(dat.yesterday$Hospital_Intake_Proven)
sum(dat.today$Hospital_Intake_Suspected) - sum(dat.yesterday$Hospital_Intake_Suspected)
last(dat.today$Hospital_Currently)
sum(dat.today$IC_Intake_Proven) - sum(dat.yesterday$IC_Intake_Proven)
sum(dat.today$IC_Intake_Suspected) - sum(dat.yesterday$IC_Intake_Suspected)
last(dat.today$IC_Current)

dat.today <- dat.today %>%
  mutate(Hospital_Intake_7d = round(frollmean(Hospital_Intake_Proven,7),0)) %>%
  mutate(IC_Intake_7d = round(frollmean(IC_Intake_Proven,7),0))

df <- merge(dat.today[,c("date","Hospital_Intake_Proven","Hospital_Intake_Suspected","IC_Intake_Proven","Hospital_Currently","IC_Current")], dat.yesterday[,c("date","Hospital_Intake_Proven","Hospital_Intake_Suspected","IC_Intake_Proven","Hospital_Currently","IC_Current")], by = "date", all.x=T)
df$diff.proven <- df$Hospital_Intake_Proven.x-df$Hospital_Intake_Proven.y
df$diff.suspec <- df$Hospital_Intake_Suspected.x-df$Hospital_Intake_Suspected.y
df$diff.proven.ic <- df$IC_Intake_Proven.x-df$IC_Intake_Proven.y
df$diff.current.hosp <- df$Hospital_Currently.x-df$Hospital_Currently.y
df$diff.current.ic <- df$IC_Current.x-df$IC_Current.y

#rm(myfiles,temp,dat.threedaysago,dat.twodaysago,dat.yesterday,dat.today,df,vaccine.data,dat)

dat.today$growth <- lead(dat.today$Hospital_Currently)/dat.today$Hospital_Currently
dat.today$growth_7d <- frollmean(dat.today$growth,7)

dat.today$growth_IC <- lead(dat.today$IC_Current)/dat.today$IC_Current
dat.today$growth_IC_7d <- frollmean(dat.today$growth_IC,7)


dat.today$date <- as.Date(dat.today$date)
today.date <- dat.today[nrow(dat.today)-4,"date"]

  
# Plot for positive tests per day
dat.today %>%
  filter(date <= today.date) %>%
  ggplot(aes(x=date, y=growth_7d)) + 
  geom_line(aes(y = growth_7d, color = "Groei in bezetting_7d"), lwd=1.2) +
  geom_line(aes(y = growth_IC_7d, color = "Groei in bezetting_IC_7d"), lwd=1.2) +
  #scale_x_date(expand = c(0, 2)) + 
  #scale_y_continuous(expand = c(0, 200), limits = c(0, NA)) +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x.bottom = element_text(size=12, hjust = 0.2),
        axis.text.y = element_text(size=12, face="bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        legend.pos = "none",
        legend.direction = "vertical",
        legend.title = element_blank()) +
  labs(x = "Datum",
       y = "Groei in bezetting kliniek (7d gemiddelde)",
       color = "Legend") +
  geom_hline(yintercept = 1, linetype = "dotted") +
  geom_hline(yintercept = 1.03, linetype = "dotted") +
  ggtitle("Meldingen van geconstateerde besmettingen")
