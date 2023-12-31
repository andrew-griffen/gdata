library(MASS)
library(tidyverse)
library(griffen)

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



