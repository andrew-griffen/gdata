library(MASS)
library(tidyverse)
library(griffen)
library(haven)
library(magrittr)

save_datasets = function(...){
  datasets_list <- lapply(eval(substitute(alist(...))),deparse)
  files = lapply(datasets_list,function(x) paste0("/home/andrew/Dropbox/R_packages/gdata/data/",x,".rda"))
  mapply(save, list = datasets_list, file = files)
  invisible(datasets_list)
}

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
oecd <- oecd %>% select(country,year,sex,life_expectancy)
save_datasets(oecd)

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