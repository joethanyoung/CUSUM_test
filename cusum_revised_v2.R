####分为模型建设与结论两个阶段。
###模型建设(R语言)
########initialize#############
##### coding by yang zx
##### import data and transform to time series
# clear & reset environment
rm(list = ls())
# import qcc
library(qcc)
# import data
library(readxl)
df <- read_excel("C:/Users/zhuoxun.yang001/Documents/fude/业务研究/预警模型/异常机构月度保费.xlsx", sheet = "陕西")
head(df)
#
dim(df)
# normalized the dataset
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
#
df_mm <- min_max_norm(df[2:11])
df_mf <- as.data.frame(df_mm)
#
head(df_mf)
#
dim(df_mf)
#
str(df_mf)
# Create matrix for easier processing and referencing
premium = matrix(df_mf[,1:ncol(df_mf)])
premium_vec <- as.vector(unlist(premium))
# check the shape of premium
dim(premium)
# check the structure of premium
str(premium)
# plot time series 
p_ts <- ts(premium_vec, start = 2019, frequency = 12)
plot(p_ts)
hist(p_ts)
# replicate avg_premium of all premium vector
avg_premium <- rep(0,nrow(premium))
sd_premium <- rep(0,nrow(premium))
avg_premium
sd_premium
# write loop to loop through all the data and assign value to avg & std
for (i in 1:nrow(premium)){
  avg_premium[i] <- mean(premium[[i]][1:36])
  sd_premium[i] <- sd(premium[[i]][1:36])
}
## print avg & std
avg_premium
sd_premium

######Applying CUSUM (change of detection)
#####
###
CUSUMmodels <- vector(mode="list", length=nrow(premium)) 
CUSUMviolations <- vector(mode="list", length=nrow(premium)) 
####

di <- 2

ss <- 1

for (i in 1:nrow(premium)){
  CUSUMmodels[[i]] <- cusum(premium[[i]], center=avg_premium[i], std.dev = sd_premium[i], decision.interval=di, se.shift=ss, plot = TRUE)
  CUSUMviolations[[i]] <- CUSUMmodels[[i]]$violations}
#####
### end
#####
