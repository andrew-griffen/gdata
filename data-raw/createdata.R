library(tidyverse)

save_datasets = function(...){
  datasets_list <- lapply(eval(substitute(alist(...))),deparse)
  files = lapply(datasets_list,function(x) paste0("/home/andrew/Dropbox/R_packages/gdata/data/",x,".rda"))
  mapply(save, list = datasets_list, file = files)
  invisible(datasets_list)
}

#example expenditure data
n = 100
expenditure_data = tibble(id = 1:100, food = round(100*exp(rnorm(n))), clothing = round(100*exp(rnorm(n))), housing = round(200*exp(rnorm(n))), alcohol = round(20*exp(rnorm(n))))
save_datasets(expenditure_data)



