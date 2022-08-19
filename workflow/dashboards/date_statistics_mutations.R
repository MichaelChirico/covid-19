temp = last(list.files(path = "data-rivm/casus-datasets/",pattern="*.csv.gz", full.names = T),2) ## Pull names of all available datafiles
df.today <- fread(temp[2])
df.yesterday <- fread(temp[1])

date_file <- as.Date(parse_date_time(last(df.today$Date_file), "Ymd HMS"))

df.today <- df.today %>%
  mutate(value = 1) %>%
  mutate(date = date_file)

df_date_long <- dcast.data.table(df.today, Date_statistics_type + Date_statistics + date ~ value, fun.aggregate = sum)
rm(df.today)
gc()

date_file_yesterday <- as.Date(parse_date_time(last(df.yesterday$Date_file), "Ymd HMS"))

df.yesterday <- df.yesterday %>%
  mutate(value = 1) %>%
  mutate(date = date_file_yesterday)

df_date_long.yesterday <- dcast.data.table(df.yesterday, Date_statistics_type + Date_statistics + date ~ value, fun.aggregate = sum)
rm(df.yesterday)
gc()

df_date_long <- rbind(df_date_long, df_date_long.yesterday)

colnames(df_date_long) <- c("Type_Datum","Datum","Dag","x")
setorder(df_date_long, Dag ,Datum)


df_date_wide <- spread(df_date_long, key = Dag, value = x)
setDF(df_date_wide)

df_date_wide$Verschil <- df_date_wide[,ncol(df_date_wide)] - df_date_wide[,ncol(df_date_wide)-1]
df_date_wide <- df_date_wide[,c("Datum","Verschil","Type_Datum")]

df.final <- spread(df_date_wide, key = Type_Datum, value = Verschil, fill = 0)
colnames(df.final) <- c("Datum","DOO_diff","DPL_diff")
df.final$Datum <- as.Date(df.final$Datum)

dat.today <- fread(temp[2])
dat.today$value <- 1

date_type.df <- dcast.data.table(dat.today, Date_statistics + Date_statistics_type ~ value, fun.aggregate = sum)
date_type_wide <- spread(date_type.df, key = Date_statistics_type, value = `1`, fill = 0)
date_type_wide$Datum <- as.Date(date_type_wide$Date_statistics)
date_type_wide <- date_type_wide[,c("Date_statistics","DOO","DPL","Datum")]

dat_wide <- merge(date_type_wide, df.final, by = "Datum")
dat_wide <- dat_wide[,-c("Date_statistics")]

write.csv(dat_wide, file = "data-dashboards/date_statistics_mutations.csv")
repo <- git2r::init()
add(repo, path = "data-dashboards/date_statistics_mutations.csv")
commit(repo, all = T, paste0("Update mutations per date_statistics types ",Sys.Date()))
push(repo, credentials = git.auth)
