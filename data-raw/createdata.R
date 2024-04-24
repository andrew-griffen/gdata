library(MASS)
library(tidyverse)
# library(griffen)
library(haven)
library(magrittr)
library(zoo)

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
oecd <- oecd %>% left_join(select(iso,country,alpha3),by="alpha3")
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
ak1991 %<>% mutate(race = if_else(race==1,"black","white"))
ak1991 %<>% mutate(census = census + 1900)
ak1991 %<>% mutate(birthyear = if_else(census==1980,birthyear + 1900,birthyear))
save_datasets(ak1991)


gettysburg <- "Four score and seven years ago our fathers brought forth on this continent a new nation, conceived in liberty, and dedicated to the proposition that all men are created equal. Now we are engaged in a great civil war, testing whether that nation, or any nation so conceived and so dedicated, can long endure. We are met on a great battlefield of that war. We have come to dedicate a portion of that field as a final resting place for those who here gave their lives that that nation might live. It is altogether fitting and proper that we should do this. But in a larger sense we cannot dedicate, we cannot consecrate, we cannot hallow this ground. The brave men, living and dead, who struggled here have consecrated it, far above our poor power to add or detract. The world will little note, nor long remember, what we say here, but it can never forget what they did here. It is for us the living, rather, to be dedicated here to the unfinished work which they who fought here have thus far so nobly advanced. It is rather for us to be here dedicated to the great task remaining before us,that from these honored dead we take increased devotion to that cause for which they gave the last full measure of devotion, that we here highly resolve that these dead shall not have died in vain, that this nation, under God, shall have a new birth of freedom, and that government of the people, by the people, for the people, shall not perish from the earth."
save_datasets(gettysburg)


#cps
cps <- read_csv("cps.csv")
cps <- cps[sample(1:nrow(cps),.2*nrow(cps)),]
cps %<>% mutate(year = as.integer(year))
save_datasets(cps)





#credit scores
credit <- read_csv("credit.csv")
credit <- credit %>% mutate(default = factor(default))

#boston
boston <- read_csv("boston.csv")

#heights
heights <- read_csv("heights.csv")

x <- tibble(key = c(1,2,3), val_x = c("x1","x2","x3"))
y <- tibble(key = c(1,2,4), val_y = c("y1","y2","y3"))


whales <- read_csv("whales.csv")
comment(whales) <- "Sperm whales Gulf of California 2007-2008"
# whale <- read_csv("Sperm whales Gulf of California 2007-2008 - Argos data.csv")
whales <- whales %>% mutate(date = as.Date(timestamp)) %>% left()
whales <- whales %>% arrange(timestamp) %>% distinct(date,.keep_all=TRUE)
whales %<>% rename(long = `location-long`, lat = `location-lat`, time = timestamp) %>% select(time,long,lat)


clark = read_csv("clark.csv")
coges = read_csv("coges.csv")

tbl1 <- tibble(id = c("1","2","3"), drew1 = c(1,2,3), drew2 = c(4,5,6), drew3 = c(7,8,9))
tbl2 <- tibble(id = c(1,2,3), a = c(1,2,3), b = c(4,5,6) , c = c(7,8,9))
tbl3 <- tibble(bla1l128a = c(1,2,3), id = c(1,2,3), ahfka1 = c(4,5,6), fdhsfka = c(7,8,9))
tbl4 <- tibble(id = c(1,2,3), contribution_round1 = c(100,200,300), contribution_round2 = c(300,200,200))

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







