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
1 - mean(Data_CN_NW_NE$t_48)/25; sd(Data_CN_NW_NE$t_48)/sqrt(length(Data_CN_NW_NE$t_48))/25
1 - mean(Data_CA_NW_NE$t_48)/25; sd(Data_CA_NW_NE$t_48)/sqrt(length(Data_CA_NW_NE$t_48))/25





##### Fit GLM to seed removal data (species separate) -----------------------------------------------------

# Use binomial error structure with logit link function
# Response is vector of "successes" (seeds removed) and "failures" (seeds not removed)
# Models for 48 hours not fit due to convergence issues

# Fit GLMs for seed removal at 6, 12, and 24 hours (CN)
GLM6_CN <- glmer(cbind(25 - Data_GLM_CN$t_6, Data_GLM_CN$t_6) ~ Warmed + Elaiosome + Warmed:Elaiosome +
                   (1 | Block), data = Data_GLM_CN, family = "binomial")
GLM12_CN <- glmer(cbind(Data_GLM_CN$t_6 - Data_GLM_CN$t_12, Data_GLM_CN$t_12) ~ Warmed + Elaiosome +
                    Warmed:Elaiosome + (1 | Block), data = Data_GLM_CN, family = "binomial")
GLM24_CN <- glmer(cbind(Data_GLM_CN$t_12 - Data_GLM_CN$t_24, Data_GLM_CN$t_24) ~ Warmed + Elaiosome + 
                    Warmed:Elaiosome + (1 | Block), data = Data_GLM_CN, family = "binomial")

# Model selection performed on 12 and 24 hour models to minimise AIC (CN)
#GLM12_CN <- glmer(cbind(Data_GLM_CN$t_6 - Data_GLM_CN$t_12, Data_GLM_CN$t_12) ~ Elaiosome +
#                    (1 | Block), data = Data_GLM_CN, family = "binomial")
#GLM24_CN <- glmer(cbind(Data_GLM_CN$t_12 - Data_GLM_CN$t_24, Data_GLM_CN$t_24) ~ Warmed + Elaiosome +
#                    (1 | Block), data = Data_GLM_CN, family = "binomial")

# Fit GLMs for seed removal at 6, 12, and 24 hours (CA)
# AIC already minimised on models, no need for step selection
GLM6_CA <- glmer(cbind(25 - Data_GLM_CA$t_6, Data_GLM_CA$t_6) ~ Warmed + Elaiosome + Warmed:Elaiosome +
                   (1 | Block), data = Data_GLM_CA, family = "binomial")
GLM12_CA <- glmer(cbind(Data_GLM_CA$t_6 - Data_GLM_CA$t_12, Data_GLM_CA$t_12) ~ Warmed + Elaiosome +
                    Warmed:Elaiosome + (1 | Block), data = Data_GLM_CA, family = "binomial")
GLM24_CA <- glmer(cbind(Data_GLM_CA$t_12 - Data_GLM_CA$t_24, Data_GLM_CA$t_24) ~ Warmed + Elaiosome +
                    Warmed:Elaiosome + (1 | Block), data = Data_GLM_CA, family = "binomial")

# Check model summaries
summary(GLM6_CN)
summary(GLM12_CN)
summary(GLM24_CN)
summary(GLM6_CA)
summary(GLM12_CA)
summary(GLM24_CA)





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





##### Qualitative compariosn of GLMs and survival regressions ---------------------------------------------

# Some sort of accelerated failure model (increasing hazard over time) makes most sense
# Will try exponential and Weibull

# Note: preferred method frailty() for random effect in survival model does NOT work in survreg
# Encoding random effect as (1|Block) also does not work; will thus encode as fixed effect
# This is perfectly valid, just does not let us make numeric predictions outside context of experiment

# CA, exponential hazard
Surv1_CA <- survreg(Surv(ToD, Cens) ~ Warmed + Elaiosome + Warmed:Elaiosome + as.factor(Block),
                    data = DataAlt_CA, dist = "exponential")
summary(Surv1_CA)

# CA, Weibull (default) hazard
Surv2_CA <- survreg(Surv(ToD, Cens) ~ Warmed + Elaiosome + Warmed:Elaiosome + as.factor(Block),
                    data = DataAlt_CA)
summary(Surv2_CA)

# CA results qualitatively similar to GLMs
# Warming and elaiosome presence (both significant) increase rates of removal
# Interaction effect (significant) dampens effects of both combined

# CN, exponential hazard
Surv1_CN <- survreg(Surv(ToD, Cens) ~ Warmed + Elaiosome + Warmed:Elaiosome + as.factor(Block),
                    data = DataAlt_CN, dist = "exponential")
summary(Surv1_CN)

# CN, Weibull (default) hazard
Surv2_CN <- survreg(Surv(ToD, Cens) ~ Warmed + Elaiosome + Warmed:Elaiosome + as.factor(Block),
                    data = DataAlt_CN)
summary(Surv2_CN)

# CN results qualitatively similar to GLMs
# Warming and elaiosome presence (both significant) increase rates of removal
# Interaction effect is not significant

