temp = tail(list.files(path = "data-rivm/municipal-datasets-per-day/",pattern="*.csv", full.names = T),2)
dat.today <- fread(temp[2])
dat.yesterday <- fread(temp[1])

# Positive tests
cases.today <- dat.today[, .(cases.today=sum(Total_reported)), by=Date_of_publication]
cases.yesterday <- dat.yesterday[, .(cases.yesterday=sum(Total_reported)), by=Date_of_publication]

df.cases.new <- merge(cases.today,cases.yesterday,by="Date_of_publication")
df.cases.new$diff <- df.cases.new$cases.today - df.cases.new$cases.yesterday

new.infection <- last(cases.today$cases.today)
corrections.cases <- sum(df.cases.new$diff)
net.infection <- new.infection+corrections.cases

## Hospitals
#hospital.today <- dat.today[, .(hospital.today=sum(Hospital_admission)), by=Date_of_publication]
#hospital.yesterday <- dat.yesterday[, .(hospital.yesterday=sum(Hospital_admission)), by=Date_of_publication]

#df.hospital.new <- merge(hospital.today,hospital.yesterday,by="Date_of_publication")
#df.hospital.new$diff <- df.hospital.new$hospital.today - df.hospital.new$hospital.yesterday

#new.hospitals <- last(hospital.today$hospital.today)
#corrections.hospitals <- sum(df.hospital.new$diff)
#net.hospitals <- new.hospitals + corrections.hospitals

## Deaths
death.today <- dat.today[, .(death.today=sum(Deceased)), by=Date_of_publication]
death.yesterday <- dat.yesterday[, .(death.yesterday=sum(Deceased)), by=Date_of_publication]

df.death.new <- merge(death.today,death.yesterday,by="Date_of_publication")
df.death.new$diff <- df.death.new$death.today - df.death.new$death.yesterday

new.deaths <- last(death.today$death.today)
corrections.deaths <- sum(df.death.new$diff)
net.deaths <- new.deaths+corrections.deaths


corrections.all <- as.data.frame(cbind(new.infection,corrections.cases, net.infection,0,0, 
                                       0,new.deaths,corrections.deaths,net.deaths))

corrections.all$date <- as.Date(Sys.Date())

filename <- paste0("corrections/corrections_per_day/corrections-",Sys.Date()-1,'.csv')
write.csv(corrections.all, file = filename, row.names=F)

temp = list.files(path = "corrections/corrections_per_day/",pattern="*.csv", full.names = T)
corrections.perday = rbindlist(lapply(temp, fread))
corrections.perday <- corrections.perday[order(corrections.perday$date),]


corrections.perday[,positive_7daverage := round(frollmean(new.infection,7),0)
                   ][,positive_14d := lag(positive_7daverage,7)
                     ][,growth_infections := round(positive_7daverage/positive_14d*100,1)]

fwrite(corrections.perday, file = "corrections/corrections_perday.csv", row.names = FALSE)

rm(list=ls())
