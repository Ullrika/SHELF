#load("probdensfromSHELF_mix.Rdata") #df1
#load("probdensfromSHELF_nonmix.Rdata")

get_env <- function(){
load("fitfromSHELF.Rdata")
fit <- params$fit

x = seq(-4,6,length.out = 10000)

pmat <- do.call('rbind',lapply(1:14,function(ex){
expertprobs(fit, x, d = "best", ex = ex)
}))

df_env_SHELF <- data.frame(x=x,upper=apply(pmat,2,'min'),
lower=apply(pmat,2,'max'))

save(df_env_SHELF,file="df_env_SHELF.Rdata")
}



