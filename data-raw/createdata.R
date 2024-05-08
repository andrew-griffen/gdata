library(MASS)
library(tidyverse)
# library(griffen)
library(haven)
library(magrittr)
library(zoo)
library(tidyr)
library(broom)

save_datasets = function(...){
  datasets_list <- lapply(eval(substitute(alist(...))),deparse)
  files = lapply(datasets_list,function(x) paste0("/home/andrew/Dropbox/R_packages/gdata/data/",x,".rda"))
  mapply(save, list = datasets_list, file = files)
  invisible(datasets_list)
}

left <- griffen::left

#example 1 expenditure data
n = 100
expenditure_data1 = tibble(id = 1:100, food = round(100*exp(rnorm(n))), clothing = round(100*exp(rnorm(n))), housing = round(200*exp(rnorm(n))), alcohol = round(20*exp(rnorm(n))))
save_datasets(expenditure_data1)

#example 2 expenditure data
n = 100
k = 200
exp_matrix = mvrnorm(n, mu = rep(0,k),Sigma = diag(k))
expenditure_data2 = as_tibble(round(100*exp(exp_matrix)))
names(expenditure_data2) = paste("item",1:k,sep="")
expenditure_data2 %<>% mutate(id = 1:n) %>% left(id)
save_datasets(expenditure_data2)

#oecd data
oecd <- read_csv("oecd.csv")
iso <- read_csv("iso.csv")
iso <- iso %>%
arrange(country) %>%
group_by(alpha3) %>% # the complete group of interest
mutate(duplicate = 1:n()) %>% # count number in each group
filter(duplicate==1) %>%
select(-duplicate) %>%
ungroup()
oecd <- oecd %>% left_join(select(iso, country, alpha3), by = "alpha3")
oecd <- oecd %>% select(country, year, sex, life_expectancy)

sp <- read_csv("sp.csv")
sp <- sp %>% as_tibble()
sp <- sp %>% mutate(returns = returns/100)
save_datasets(sp)

#oj
oj <- read_csv("oj.csv")
save_datasets(oj)

ak1991 = read_dta("NEW7080.dta")
ak1991 %<>% rename(age = v1)
ak1991 %<>% rename(education = v4)
ak1991 %<>% rename(log_wage = v9)
ak1991 %<>% rename(married = v10)
ak1991 %<>% rename(census = v16)
ak1991 %<>% rename(quarter_birth = v18)
ak1991 %<>% rename(race = v19)
ak1991 %<>% rename(birthyear = v27)
ak1991 %<>% select(log_wage,
              education,
              quarter_birth,
              birthyear,
              census,
              race)
ak1991 %<>% mutate(race = if_else(race == 1, "black", "white"))
ak1991 %<>% mutate(census = census + 1900)
ak1991 %<>% mutate(birthyear = if_else( census == 1980, birthyear + 1900, birthyear))
save_datasets(ak1991)


gettysburg <- "Four score and seven years ago our fathers brought forth on this continent a new nation, conceived in liberty, and dedicated to the proposition that all men are created equal. Now we are engaged in a great civil war, testing whether that nation, or any nation so conceived and so dedicated, can long endure. We are met on a great battlefield of that war. We have come to dedicate a portion of that field as a final resting place for those who here gave their lives that that nation might live. It is altogether fitting and proper that we should do this. But in a larger sense we cannot dedicate, we cannot consecrate, we cannot hallow this ground. The brave men, living and dead, who struggled here have consecrated it, far above our poor power to add or detract. The world will little note, nor long remember, what we say here, but it can never forget what they did here. It is for us the living, rather, to be dedicated here to the unfinished work which they who fought here have thus far so nobly advanced. It is rather for us to be here dedicated to the great task remaining before us,that from these honored dead we take increased devotion to that cause for which they gave the last full measure of devotion, that we here highly resolve that these dead shall not have died in vain, that this nation, under God, shall have a new birth of freedom, and that government of the people, by the people, for the people, shall not perish from the earth."
save_datasets(gettysburg)


#cps
cps <- read_csv("cps.csv")
cps <- cps[sample( 1:nrow(cps), .2 * nrow(cps)), ]
cps %<>% mutate(year = as.integer(year))
save_datasets(cps)





#credit scores
credit <- read_csv("credit.csv")
credit <- credit %>% mutate(default = factor(default))

#boston
boston <- read_csv("boston.csv")

#heights
heights <- read_csv("heights.csv")

x <- tibble(key = c(1, 2, 3), val_x = c("x1", "x2", "x3"))
y <- tibble(key = c(1, 2, 4), val_y = c("y1", "y2", "y3"))


whales <- read_csv("whales.csv")
comment(whales) <- "Sperm whales Gulf of California 2007-2008"
# whale <- read_csv("Sperm whales Gulf of California 2007-2008 - Argos data.csv")
whales <- whales %>% mutate(date = as.Date(timestamp)) %>% left()
whales <- whales %>% arrange(timestamp) %>% distinct(date,.keep_all=TRUE)
whales %<>% rename(long = `location-long`, lat = `location-lat`, time = timestamp) %>% select(time,long,lat)


clark = read_csv("clark.csv")
coges = read_csv("coges.csv")

tbl1 <- tibble(id = c("1","2","3"), andrew1 = c(1, 2, 3), andrew2 = c(4, 5, 6), andrew3 = c(7, 8, 9))
tbl2 <- tibble(id = c(1,2,3), a = c(1, 2, 3), b = c(4, 5, 6) , c = c(7, 8, 9))
tbl3 <- tibble(bla1l128a = c(1, 2, 3), id = c(1, 2, 3), ahfka1 = c(4, 5, 6), fdhsfka = c(7, 8, 9))
tbl4 <- tibble(id = c(1, 2, 3), contribution_round1 = c(100, 200, 300), contribution_round2 = c(300, 200, 200))

n <- 10
rounds <- 100
tbl5 <- matrix(100*round(runif(n*rounds,0,6)),n,rounds) %>% as.data.frame()
names(tbl5) <- paste(rep("contribution_round",rounds),1:rounds,sep="")
tbl5 <- as_tibble(tbl5)
tbl5 <- tbl5 %>% mutate(id = 1:nrow(tbl5)) %>% left()

state_population <- read_csv("state_population.csv")
state_population <- state_population %>% pivot_longer(-c("state","region"),names_to="year",values_to="population")
region <- state_population %>% select(state,region) %>% distinct()
state_population <- state_population %>% select(-region)
state_population <- state_population %>% mutate(year = as.integer(year))
state_population <- state_population %>% select(state) %>% distinct() %>% crossing(tibble(year=1970:2014)) %>% full_join(state_population) %>% arrange(state,year)
state_population <- state_population %>% dplyr::filter(year<=2010) %>% group_by(state) %>% mutate(population = na.approx(population,year))
state_population <- state_population %>% left_join(region)
state_population <- state_population |> ungroup()

n <- 3
form_df <- tibble(y = round(10*runif(3)), x1 = round(10*runif(3)), x2 = round(10*runif(3)), D = c("treated","control","treated") )
form_df <- form_df %>% mutate(D = factor(D))
form_df <- form_df %>% mutate(D = fct_relevel(D,"treated"))

save_datasets(x,y)
save_datasets(boston, credit, form_df, heights)
# save_datasets(post_bart, pre_bart
save_datasets(state_population)
save_datasets(tbl1, tbl2, tbl3, tbl4, tbl5)
save_datasets(oecd)
save_datasets(whales)
save_datasets(clark)
save_datasets(coges)
# save_datasets(japan_travel,japan_shp)




stocks <- read.csv("../../../Models/Taddy Data Science/MBAcourse-master/examples/stocks.csv")
stocks$RET <- as.numeric(as.character(stocks$RET))
stocks$date <- as.Date(as.character(stocks$date), format="%Y%m%d")
stocks <- stocks %>% filter(TICKER!="" & RET!="")
dups <- which(duplicated(stocks[,c("TICKER","date")]))
stocks <- stocks[-dups,]
stocks$month <- paste(format(stocks$date, "%Y-%m"),"-01",sep="")
stocks$month <- as.Date(stocks$month)
stocks %<>% as_tibble()
agg <- function(r) prod(1+r, na.rm=TRUE) - 1
stocks <- stocks %>%
  				group_by(TICKER, month) %>%
  				summarize(RET = agg(RET), SNP = agg(sprtrn), .groups="drop")
stocks %<>% ungroup
stocks %<>% rename(r = RET, r_sp = SNP, ticker = TICKER)

stocks_nested = stocks %>% group_by(ticker) %>% nest()
stocks_nested %<>% mutate(model = map(data,~lm(r ~ r_sp, .)))
stocks_nested %<>% ungroup
stocks_nested %<>% mutate(tidy_model = map(model,tidy))


capm <- stocks_nested %>% select(ticker,tidy_model) %>% unnest(cols = c(tidy_model))
capm %<>% mutate(term = case_when(term=="(Intercept)" ~ "alpha",
								  term=="r_sp" ~ "beta",
								  TRUE ~ NA_character_))
capm %<>% rename(parameter = term)
capm %<>% select(ticker,parameter,estimate)
save_datasets(capm)



description = c("Where the kid met the judge and burned down the tavern. Nacogdoches was the frontier gateway into Texas. A short rebellion against the Mexican government occured there in 1825, with some locals declaring the Republic of Fredonia.",
"Where the kid smashed out the eye of the Mexican barman with a broken glass. Just called Bexar in the book, this is actually the modern day city of San Antonio. It was the center of Spanish/Mexican influence in Texas.",
"No rivers flow out of this natural desert basin, which is the scene of the Comanche attack on the incompetent Captain White and his filibusters. The Comanche from the southern Great Plains would ride through here on their way to raid Mexicans.",
"The mud-walled Presidio and ancient mission chapel mentioned in the book still stand in this dusty town south of the border. Glanton killed the old Indian woman on the plaza. Geronimo used to visit Janos to trade.",
"The high pine forests of the Animas Peaks in the far south of New Mexico are the location of the scene where Black Jackson kills White Jackson.",
"The gang meets the marooned forty-niners with the snakebit mule here. The story told in the book about the mining town being abandoned due to Apache raids in the 1830s is true. Today Santa Rita is a huge open-pit copper mine.",
"The judge picking through the ruins and artifacts was inspired here to tell the parable of the highway robber. The cliff dwellings were built into the rocks by the Anasazi people, who mysteriously disappeared from history in the 1300s.",
"After the massacre of the Gileños and fighting a running battle all the way back to Chihuahua, the victorious gang spends a riotous night at the seat of government, which turns into a days-long bacchanal. Current structure was built in 1881.",
"A town on the Texas border, where the gang briefly stops as they hunt Apaches. We get an intimate glimpse into Glanton's heart as he takes a moment out in the desert by himself to think about his family somewhere in eastern Texas who he'll never see again.",
"Like an oasis rising out of the surrounding flat desert, these granite hills contain depressions which naturally collect rainwater, drawing in thirsty travellers passing through the area since the beginning of time. Every crevice is covered in petroglyphs.",
"The third scalping expedition that the Glanton Gang took from Chihuahua ended here in a massacre of the townsfolk. Perhaps included in Blood Meridian because of its association with the Crawford Battle of 1886.",
"Capital of Sonora at the time, Glanton got one more contract for Apache scalps before himself becoming the hunted by General Elias. Today it's a quiet town surrounded by beautiful agricultural fields. Mentioned in the historical records as far back as the 1530s.",
"The judge gave an extemporaneous lecture on the architecture of this mission, despite having never been there himself. The bodies of the scouts and the Vandiemenlander were found nearby, hanging upside down from a paloverde.",
"Beautiful mission, the oldest European structure in Arizona. It was mentioned that as the Glanton Gang camped here a green meteor appeared overhead. Extremely well-preserved and a very popular tourist attraction today just outside of Tucson.",
"Briefly occupied by the Mormon Batallion during the Mexican-American War, I actually can't find why McCarthy still has it occupied by American troops after the war but before the Gadsden Purchase. If anyone knows why, let me know.",
"This dramatic volcanic wasteland outside of Puerto Peñasco is the site of the judge's famous exposition on war and some of McCarthy's most eloquent landscape descriptions. Today it is a UNESCO World Heritage Site and protected Mexican biosphere.",
"This place on the California-Arizona-Mexico border where the Gila and Colorado Rivers meet has long been an important crossroads. Named after the Indian tribe that lived in the area. Glanton's luck finally ran out here in both the book and real life.",
"Finally settled in 1769 by Junipero Serra, and always isolated from the rest of Mexico, San Diego had recently been captured by America in 1850 when the kid visits. It would have been a tiny village of just 650 people.",
"The final harrowing scenes of Blood Meridian happen at this fort which had been built after the Civil War to protect the Texas frontier against Comanche attacks. It's a ghost town today, with a few old brick buildings still standing.")

location = c("Nacogdoches, Texas",
"San Antonio de Bexar",
"Bolson de Mapini, Chihuahua",
"Janos, Chihuahua",
"Animas Mountains, New Mexico",
"Santa Rita del Cobre Copper Mines, New Mexico",
"Gila Cliff Dwellings, New Mexico",
"Governor's Palace, Chihuahua",
"Presidio, Texas",
"Hueco Tanks, Texas",
"Nácori Chico, Sonora",
"Ures, Sonora",
"Mission San Jose de Tumacacori, Arizona",
"San Xavier del Bac, Arizona",
"Presidio San Agustín del Tucsón, Arizona",
"El Pinacate, Sonora",
"Yuma, Arizona",
"San Diego, California",
"Fort Griffin, Texas")

lng = c(-94.65557367273087, -98.49517555510062, -106.09891633023416, -108.19135834853053, -108.78329671794378, -108.07263760000137, -108.26978000978869, -106.07281639031231, -104.37213311934813, -106.04357364602406, -108.98090939774758, -110.38586610635605, -111.05048907858992, -111.00835084973995, -110.97273310612216, -113.50322932692085, -114.62424305509093, -117.15983896821592, -99.2265764826492)

lat = c(31.604300240355077, 29.425593314328324, 28.75574167065598, 30.88892315655848, 31.565926571592716, 32.80705141358657, 33.22733186773855, 28.63906832178077, 29.56109949920634, 31.917299394786763, 29.687837546889742, 29.426050217423537, 31.56871732642278, 32.10706374123074, 32.22437649877388, 31.792591062358998, 32.69760349453199, 32.71788652676264, 32.921484211161584)

blood_meridian = tibble(location, description, lng, lat)

save_datasets(blood_meridian)






