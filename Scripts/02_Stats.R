##### Examine observed removal rates ----------------------------------------------------------------------

# Total proportion of seeds removed after a given time
1 - mean(Data$t_6)/25; sd(Data$t_6)/sqrt(length(Data$t_6))/25    # 6h
1 - mean(Data$t_12)/25; sd(Data$t_12)/sqrt(length(Data$t_12))/25 # 12h
1 - mean(Data$t_24)/25; sd(Data$t_24)/sqrt(length(Data$t_24))/25 # 24h
1 - mean(Data$t_48)/25; sd(Data$t_48)/sqrt(length(Data$t_48))/25 # 48h

# Proportion of seeds removed after a given time for warmed CN E+ [highest]
1 - mean(Data_CN_YW_YE$t_6)/25; sd(Data_CN_YW_YE$t_6)/sqrt(length(Data_CN_YW_YE$t_6))/25    # 6h
1 - mean(Data_CN_YW_YE$t_12)/25; sd(Data_CN_YW_YE$t_12)/sqrt(length(Data_CN_YW_YE$t_12))/25 # 12h
1 - mean(Data_CN_YW_YE$t_24)/25; sd(Data_CN_YW_YE$t_24)/sqrt(length(Data_CN_YW_YE$t_24))/25 # 24h
1 - mean(Data_CN_YW_YE$t_48)/25; sd(Data_CN_YW_YE$t_48)/sqrt(length(Data_CN_YW_YE$t_48))/25 # 48h

# Proportion of seeds removed after 48h for unwarmed CN and CA E- [lowest]
1 - mean(Data_CN_NW_NE$t_48)/25; sd(Data_CN_NW_NE$t_48)/sqrt(length(Data_CN_NW_NE$t_48))/25  # 6
1 - mean(Data_CA_NW_NE$t_48)/25; sd(Data_CA_NW_NE$t_48)/sqrt(length(Data_CA_NW_NE$t_48))/25  # 6





##### Fit GLM to seed removal data (species separate) -----------------------------------------------------

# Use binomial error structure with logit link function
# Response is vector of "successes" (seeds removed) and "failures" (seeds not removed)
# Models for 48 hours not fit due to convergence issues

# Subset data by species
Data2_CN <- subset(Data2, Species == "CN")
Data2_CA <- subset(Data2, Species == "CA")

# Fit GLMs for seed removal at 6, 12, and 24 hours (CN)
Fit6_CN <- glmer(cbind(25 - Data2_CN$t_6, Data2_CN$t_6) ~ Warmed + Elaiosome + Warmed:Elaiosome + (1 | Block),
                 data = Data2_CN, family = "binomial")
Fit12_CN <- glmer(cbind(Data2_CN$t_6 - Data2_CN$t_12, Data2_CN$t_12) ~ Warmed + Elaiosome + Warmed:Elaiosome +
                    (1 | Block), data = Data2_CN, family = "binomial")
Fit24_CN <- glmer(cbind(Data2_CN$t_12 - Data2_CN$t_24, Data2_CN$t_24) ~ Warmed + Elaiosome + Warmed:Elaiosome +
                    (1 | Block), data = Data2_CN, family = "binomial")

# Model selection performed on 12 and 24 hour models to minimise AIC (CN)
#Fit12_CN <- glmer(cbind(Data2_CN$t_6 - Data2_CN$t_12, Data2_CN$t_12) ~ Elaiosome +
#                    (1 | Block), data = Data2_CN, family = "binomial")
#Fit24_CN <- glmer(cbind(Data2_CN$t_12 - Data2_CN$t_24, Data2_CN$t_24) ~ Warmed + Elaiosome +
#                    (1 | Block), data = Data2_CN, family = "binomial")

# Fit GLMs for seed removal at 6, 12, and 24 hours (CA)
# AIC already minimised on models, no need for step selection
Fit6_CA <- glmer(cbind(25 - Data2_CA$t_6, Data2_CA$t_6) ~ Warmed + Elaiosome + Warmed:Elaiosome + (1 | Block),
                 data = Data2_CA, family = "binomial")
Fit12_CA <- glmer(cbind(Data2_CA$t_6 - Data2_CA$t_12, Data2_CA$t_12) ~ Warmed + Elaiosome + Warmed:Elaiosome +
                    (1 | Block), data = Data2_CA, family = "binomial")
Fit24_CA <- glmer(cbind(Data2_CA$t_12 - Data2_CA$t_24, Data2_CA$t_24) ~ Warmed + Elaiosome + Warmed:Elaiosome +
                    (1 | Block), data = Data2_CA, family = "binomial")

# Check model summaries
summary(Fit6_CN)
summary(Fit12_CN)
summary(Fit24_CN)
summary(Fit6_CA)
summary(Fit12_CA)
summary(Fit24_CA)





##### Examine potential differences in seed mass due to warming -------------------------------------------

# Test for equal variance; fail to reject equality of variance between groups
bartlett.test(rbind(Data_SM_CN_W, Data_SM_CN_NW)$Mass,
              rbind(Data_SM_CN_W, Data_SM_CN_NW)$Warmed)
bartlett.test(rbind(Data_SM_CA_W, Data_SM_CA_NW)$Mass,
              rbind(Data_SM_CA_W, Data_SM_CA_NW)$Warmed)

# Perform t-test with equal variances
# Significant difference between warmed and unwarmed within each species
t.test(Data_SM_CN_W$Mass, Data_SM_CN_NW$Mass, alt = "two.sided", var.equal = TRUE)
t.test(Data_SM_CA_W$Mass, Data_SM_CA_NW$Mass, alt = "two.sided", var.equal = TRUE)

# Normality assumption holds for each treatment group
shapiro.test(Data_SM_CN_W$Mass)
qqnorm(Data_SM_CN_W$Mass)
qqline(Data_SM_CN_W$Mass)
shapiro.test(Data_SM_CN_NW$Mass)
qqnorm(Data_SM_CN_NW$Mass)
qqline(Data_SM_CN_NW$Mass)
shapiro.test(Data_SM_CA_W$Mass)
qqnorm(Data_SM_CA_W$Mass)
qqline(Data_SM_CA_W$Mass)
shapiro.test(Data_SM_CA_NW$Mass)
qqnorm(Data_SM_CA_NW$Mass)
qqline(Data_SM_CA_NW$Mass)

# Seed mass between species is obviously different
# Suggests seed mass might be worth looking into, but is probably not sole factor
bartlett.test(rbind(Data_SM_CN_W, Data_SM_CA_W)$Mass,
              rbind(Data_SM_CN_W, Data_SM_CA_W)$Species)
bartlett.test(rbind(Data_SM_CN_NW, Data_SM_CA_NW)$Mass,
              rbind(Data_SM_CN_NW, Data_SM_CA_NW)$Species)
t.test(Data_SM_CN_W$Mass, Data_SM_CA_W$Mass, alt = "two.sided", var.equal = FALSE)
t.test(Data_SM_CN_NW$Mass, Data_SM_CA_NW$Mass, alt = "two.sided", var.equal = FALSE)





##### Compare qualitative GLM results with survival analysis [WIP] ----------------------------------------

# If survival analysis is similar, note this in the paper
# GLM earlier is survival-style model where hazard functions are allowed to change over time

# Construct concise representation of original dataset
# ToD indicates time of death
# Cens indicates censor status (1 = dead, 0 = censored and survived until end)
for(i in 1:nrow(Data)){
  if(i == 1){
    df_main <- matrix(ncol = 7, nrow = 0)}
  row_sub <- Data[i, ]
  for(j in 7:ncol(Data)){
    if(j == 7){
      prevNum <- 25
      df_sub <- matrix(ncol = 7, nrow = 0)}
    curNum <- as.numeric(row_sub[j])
    if(curNum < prevNum){
      df_sub <- rbind(df_sub, matrix(c(as.character(row_sub[1:5]), str_remove(names(Data)[j], "t_"), 1),
                                     nrow = prevNum - curNum, ncol = 7, byrow = TRUE))}
    if(j == ncol(Data) & curNum > 0){
      df_sub <- rbind(df_sub, matrix(c(as.character(row_sub[1:5]), str_remove(names(Data)[j], "t_"), 0),
                                     nrow = curNum, ncol = 7, byrow = TRUE))}
    prevNum <- curNum}
  df_main <- rbind(df_main, df_sub)}
df_main <- data.frame(df_main, stringsAsFactors = FALSE)
names(df_main) <- c(names(Data)[1:5], "ToD", "Cens")
df_main$Depot <- as.numeric(df_main$Depot)
df_main$Block <- as.numeric(df_main$Block)
df_main$Warmed <- as.numeric(df_main$Warmed)
df_main$Elaiosome <- as.numeric(df_main$Elaiosome)
df_main$ToD <- as.numeric(df_main$ToD)
df_main$Cens <- as.numeric(df_main$Cens)
DataAlt <- df_main

# Remove temporary variables since they will no longer be used
remove(df_main, df_sub, row_sub, curNum, prevNum, i, j)

# Cox proportional hazard
# Global p-value low, PH probably not good
Surv1 <- coxph(Surv(ToD, Cens) ~ Warmed + Elaiosome + Species  + Warmed:Elaiosome + Warmed:Species +
                 Elaiosome:Species + frailty.gaussian(Block, sparse = FALSE), data = DataAlt)
summary(Surv1)
cox.zph(Surv1)
plot(cox.zph(Surv1))

# Accelerated failure/death - exponential hazard
Surv2 <- survreg(Surv(ToD, Cens) ~ Warmed + Elaiosome + Species + Warmed:Elaiosome + Warmed:Species +
                   Elaiosome:Species + frailty.gaussian(Block, sparse = FALSE), data = DataAlt, dist = "exponential")
summary(Surv2)
AIC(Surv2)
step(Surv2)

# Accelerated failure/death - Weibull hazard
Surv3 <- survreg(Surv(ToD, Cens) ~ Warmed + Elaiosome + Species + Warmed:Elaiosome + Warmed:Species +
                   Elaiosome:Species + frailty.gaussian(Block, sparse = FALSE), data = DataAlt)
summary(Surv3)
AIC(Surv3)
step(Surv3)

