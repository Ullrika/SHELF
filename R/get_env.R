#load("probdensfromSHELF_mix.Rdata") #df1
#load("probdensfromSHELF_nonmix.Rdata")


load("fitfromSHELF.Rdata")
fit <- params$fit

x = seq(-2,4,length.out = 100)

pmat <- do.call('rbind',lapply(1:14,function(ex){
expertprobs(fit, x, d = "best", ex = 1)
}))

df_env_SHELF <- data.frame(x=x,lower=apply(pmat,2,'min'),
upper=apply(pmat,2,'max'))

save(df_env_SHELF,file="df_env_SHELF.Rdata")




