## Parse municipalities population data
require(geojsonio)
## Set month

set.month <- paste0("2021MM",0,month(Sys.Date())-2)

dat.mun <- cbs_get_data("37230ned",add_column_labels = FALSE,Perioden = has_substring(c(set.month)))
dat.mun <- dat.mun[,c("RegioS","BevolkingAanHetEindeVanDePeriode_15")]
colnames(dat.mun) <- c("statcode","populatie")

dat.mun <- dat.mun %>%
  dplyr::filter(!is.na(populatie)) %>%
  dplyr::filter(grepl("GM", statcode))

gemeentegrenzen <- geojson_read("misc/maps/Gemeentegrenzen2021RD.geojson", what = "sp")
gemeentes <- gemeentegrenzen@data[,c(2,4)]

gemeente.stats <- merge(gemeentes, dat.mun, by = "statcode")
colnames(gemeente.stats) <- c("Municipality_code","Municipality_name","population")
write.csv(gemeente.stats, file = "misc/municipalities-population.csv")

## Parse GGD population data
nl_dt <- fread("data-rivm/municipal-datasets-per-day/rivm_municipality_perday_2021-06-01.csv.gz")
nl_dt <- aggregate(Deceased ~ Municipal_health_service + Municipality_code, data = nl_dt, sum)

dat.mun <- cbs_get_data("37230ned",add_column_labels = FALSE,Perioden = has_substring(c(set.month)))
dat.mun <- dat.mun[,c("RegioS","BevolkingAanHetEindeVanDePeriode_15")]
colnames(dat.mun) <- c("statcode","populatie")

dat.mun <- dat.mun %>%
  dplyr::filter(!is.na(populatie)) %>%
  dplyr::filter(grepl("GM", statcode))

gemeentegrenzen <- geojson_read("misc/maps/Gemeentegrenzen2021RD.geojson", what = "sp")
gemeentes <- gemeentegrenzen@data[,c(2,4)]

gemeente.stats <- merge(gemeentes, dat.mun, by = "statcode")
colnames(gemeente.stats) <- c("Municipality_code","Municipality_name","population")

df <- merge(nl_dt, gemeente.stats, by = "Municipality_code")
df <- aggregate(population ~ Municipal_health_service, data = df, sum)

ggds_population <- read.csv("misc/ggds-population.csv")

df <- cbind(df, ggds_population[,c("population","ggd_code","ID")])
df <- df[,c(-3)]
colnames(df) <- c("statnaam","population","ggd_code","ID")
write.csv(df, file = "misc/ggds-population.csv", row.names = FALSE)

## Parse province data
dat.mun <- cbs_get_data("37230ned",add_column_labels = FALSE,Perioden = has_substring(c(set.month)), RegioS = has_substring(c("PV")))
dat.mun <- dat.mun[,c("RegioS","BevolkingAanHetBeginVanDePeriode_1")]
dat.mun$RegioS <- as.character(dat.mun$RegioS)
colnames(dat.mun) <- c("RegioS","population")
dat.mun$RegioS <- gsub(" ", "", dat.mun$RegioS, fixed = TRUE)

province.identifier <- cbs_get_data("70072ned",add_column_labels = FALSE,Perioden = has_substring(c("2021JJ00")))
province.identifier <- province.identifier[,c("Code_289","Naam_290")]
province.identifier <- province.identifier %>% distinct()
colnames(province.identifier) <- c("RegioS","Province")
province.identifier$RegioS <- gsub(" ", "", province.identifier$RegioS, fixed = TRUE)
province.identifier$Province <- gsub(" ", "", province.identifier$Province, fixed = TRUE)

df <- merge(province.identifier, dat.mun, by = c("RegioS"), all.y=T)

df <- df[order(df$Province),]
df$ID <- seq(1,12)

write.csv(df, file = "misc/provinces-population.csv")

## Parse safety region data

dat.safetyregion <- cbs_get_data("84929NED",add_column_labels = FALSE)[,c("Code_1","Naam_2","Code_14","Naam_15","Code_26","Naam_27","Code_44","Naam_45","Inwonertal_52")]
pop.safetyregion <- aggregate(Inwonertal_52 ~ Code_44 + Naam_45, data = dat.safetyregion, FUN = sum)
colnames(pop.safetyregion) <- c("Security_region_code","Security_region_name","population")

pop.safetyregion$Security_region_name <- trimws(pop.safetyregion$Security_region_name)
pop.safetyregion$Security_region_code <- trimws(pop.safetyregion$Security_region_code)

write.csv(pop.safetyregion, file = "misc/safetyregions-population.csv")


## Age population data
pop.age <- cbs_get_data("83482NED",add_column_labels = FALSE,Perioden = has_substring(c("2021MM05")), 
                        Migratieachtergrond = has_substring(c("T001040")),
                        Generatie = has_substring(c("T001040")),
                        Geslacht = has_substring(c("T001038")))


#leeftijdopbouw per gemeente ophalen
pop.age <- cbs_get_data("03759ned", Perioden = has_substring(c("2021JJ00")),RegioS = has_substring(c("GM")) ,Geslacht = has_substring(c("T001038")),
                                  BurgerlijkeStaat = has_substring(c("T001019")))

#Totalen eruit filteren + CBScode omzetten naar leeftijd
pop.age<-pop.age[!pop.age$Leeftijd=="10000"&!pop.age$Leeftijd=="22000",]
pop.age$Leeftijd2<-((as.numeric(pop.age$Leeftijd))-10000)/100

pop.age$Leeftijd3<-ifelse(pop.age$Leeftijd2<10,"0-9","-")
pop.age$Leeftijd3<-ifelse(pop.age$Leeftijd2>9&pop.age$Leeftijd2<20,"10-19",pop.age$Leeftijd3)
pop.age$Leeftijd3<-ifelse(pop.age$Leeftijd2>19&pop.age$Leeftijd2<30,"20-29",pop.age$Leeftijd3)
pop.age$Leeftijd3<-ifelse(pop.age$Leeftijd2>29&pop.age$Leeftijd2<40,"30-39",pop.age$Leeftijd3)
pop.age$Leeftijd3<-ifelse(pop.age$Leeftijd2>39&pop.age$Leeftijd2<50,"40-49",pop.age$Leeftijd3)
pop.age$Leeftijd3<-ifelse(pop.age$Leeftijd2>49&pop.age$Leeftijd2<60,"50-59",pop.age$Leeftijd3)
pop.age$Leeftijd3<-ifelse(pop.age$Leeftijd2>59&pop.age$Leeftijd2<70,"60-69",pop.age$Leeftijd3)
pop.age$Leeftijd3<-ifelse(pop.age$Leeftijd2>69&pop.age$Leeftijd2<80,"70-79",pop.age$Leeftijd3)
pop.age$Leeftijd3<-ifelse(pop.age$Leeftijd2>79&pop.age$Leeftijd2<90,"80-89",pop.age$Leeftijd3)
pop.age$Leeftijd3<-ifelse(pop.age$Leeftijd2>89,"90+",pop.age$Leeftijd3)

#Lege kolom eruit gooien + kolomnaam aanpassen + oude gemeentes eruit knikkeren
pop.age<-pop.age[c(4,6,9)]
colnames(pop.age)<-c("Municipality_code","Aantalinwoners","Leeftijdsgroep")
pop.age<-pop.age[!is.na(pop.age$Aantalinwoners),]

pop.age <- pop.age %>% 
  group_by(Leeftijdsgroep,Municipality_code) %>%
  summarise(Inwoners=sum(Aantalinwoners))

temp = tail(list.files(path = "data-rivm/municipal-datasets-per-day/",pattern="*.csv.gz", full.names = T),1)
myfiles = fread(temp)
myfiles <- myfiles %>%
  filter(Date_of_publication == last(myfiles$Date_of_publication))

df.pop <- merge(pop.age, myfiles[,c("Municipality_code","Municipality_name","Province","Security_region_code",
                                    "Security_region_name","Municipal_health_service","ROAZ_region")], by = "Municipality_code")

## Add code Municipal_health region
ggd_population <- read.csv("misc/ggds-population.csv")
colnames(ggd_population)[1]<-"Municipal_health_service"
df.pop <- merge(df.pop, ggd_population[,c("Municipal_health_service","ggd_code")])

## Add schoolregions

schoolregion <- read.csv("https://raw.githubusercontent.com/mzelst/covid-19/master/misc/gemeente-scholen-regio.csv")
colnames(schoolregion)[1]<-"Municipality_code"
df.pop <- merge(df.pop, schoolregion[,c("Municipality_code","SchoolRegio")], by = "Municipality_code")

colnames(df.pop) <- c("Municipality_code","GGD_name","Age_group","Population_2021","Municipality_name","Province","Security_region_code",
                      "Security_region_name","ROAZ_region","GGD_code","School_Region")

## Parse population - age - municipality data for 2020
#leeftijdopbouw per gemeente ophalen
pop.age <- cbs_get_data("03759ned", Perioden = has_substring(c("2020JJ00")),RegioS = has_substring(c("GM")) ,Geslacht = has_substring(c("T001038")),
                        BurgerlijkeStaat = has_substring(c("T001019")))

#Totalen eruit filteren + CBScode omzetten naar leeftijd
pop.age<-pop.age[!pop.age$Leeftijd=="10000"&!pop.age$Leeftijd=="22000",]
pop.age$Leeftijd2<-((as.numeric(pop.age$Leeftijd))-10000)/100

pop.age$Leeftijd3<-ifelse(pop.age$Leeftijd2<10,"0-9","-")
pop.age$Leeftijd3<-ifelse(pop.age$Leeftijd2>9&pop.age$Leeftijd2<20,"10-19",pop.age$Leeftijd3)
pop.age$Leeftijd3<-ifelse(pop.age$Leeftijd2>19&pop.age$Leeftijd2<30,"20-29",pop.age$Leeftijd3)
pop.age$Leeftijd3<-ifelse(pop.age$Leeftijd2>29&pop.age$Leeftijd2<40,"30-39",pop.age$Leeftijd3)
pop.age$Leeftijd3<-ifelse(pop.age$Leeftijd2>39&pop.age$Leeftijd2<50,"40-49",pop.age$Leeftijd3)
pop.age$Leeftijd3<-ifelse(pop.age$Leeftijd2>49&pop.age$Leeftijd2<60,"50-59",pop.age$Leeftijd3)
pop.age$Leeftijd3<-ifelse(pop.age$Leeftijd2>59&pop.age$Leeftijd2<70,"60-69",pop.age$Leeftijd3)
pop.age$Leeftijd3<-ifelse(pop.age$Leeftijd2>69&pop.age$Leeftijd2<80,"70-79",pop.age$Leeftijd3)
pop.age$Leeftijd3<-ifelse(pop.age$Leeftijd2>79&pop.age$Leeftijd2<90,"80-89",pop.age$Leeftijd3)
pop.age$Leeftijd3<-ifelse(pop.age$Leeftijd2>89,"90+",pop.age$Leeftijd3)

#Lege kolom eruit gooien + kolomnaam aanpassen + oude gemeentes eruit knikkeren
pop.age<-pop.age[c(4,6,9)]
colnames(pop.age)<-c("Municipality_code","Aantalinwoners","Age_group")
pop.age<-pop.age[!is.na(pop.age$Aantalinwoners),]

pop.age <- pop.age %>% 
  group_by(Age_group,Municipality_code) %>%
  summarise(Population_2020=sum(Aantalinwoners))

df.pop <- merge(df.pop,pop.age, by = c("Municipality_code","Age_group"))

write.csv(df.pop,file = "misc/population_masterfile.csv",row.names=F)

## Repo

git.credentials <- read_lines("git_auth.txt")
git.auth <- cred_user_pass(git.credentials[1],git.credentials[2])

##Push to git
repo <- init()
add(repo, path = "*")
commit(repo, all = T, paste0("Monthly update population data - ",set.month))
push(repo, credentials = git.auth)

rm(dat.mun, dat.safetyregion, df, df.pop, gemeente.stats, gemeentegrenzen, gemeentes, ggd_population, ggds_population,
   myfiles, nl_dt, pop.age, pop.safetyregion, province.identifier,schoolregion, token.mzelst, set.month,
   temp, webhook.discord)
