rm(list=ls())
directory <- "workspace"
models <- list.files(path=directory, pattern= "*_model.RData",recursive=TRUE,full.names = TRUE)
ex<-c()
for(i in seq(1,length(models)))
{
  load(models[[i]])
  ex <- c(ex,is.na(model$performance_metric))
  
}