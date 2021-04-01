library(lme4)
library(lmerTest)
library(tidyverse)

# multilevel linear model aka linear mixed effects  -----------------------
# lm(Reaction ~ 1 + Days, data = sleepstudy)

M_7 <- lmer(Reaction ~ 1 + Days + (1 + Days|Subject), REML = F, data = sleepstudy)
ranef(M_7) # these are zeta
coef(M_7)  # these are beta

# This is identical to M_7
M_8 <- lmer(Reaction ~ Days + (Days|Subject), REML = F, data = sleepstudy)

# Random intercepts only
# So all slopes assumed to be identical across subjects
M_9 <- lmer(Reaction ~ 1 + Days + (1|Subject), REML = F, data = sleepstudy)
summary(M_9)

# Random slopes only
M_10 <- lmer(Reaction ~ 1 + Days + (0 + Days|Subject), REML = F, data = sleepstudy)
summary(M_10)

# Random slopes and random intercepts but no correlation
M_11 <- lmer(Reaction ~ Days + (Days||Subject),REML = F,  data = sleepstudy)

anova(M_11, M_7)

# Get the deviances of the models -----------------------------------------

deviance_7 <- logLik(M_7) * -2
deviance_9 <- logLik(M_9) * -2

deviance_delta <- deviance_9 - deviance_7 

pchisq(deviance_delta, df = 2, lower.tail = F)

anova(M_9, M_7)
anova(M_9, M_11)
anova(M_11, M_7)


M_12 <- lmer(Reaction ~ 1 + (Days|Subject),REML = F,  data = sleepstudy)
anova(M_12, M_7)


# Nested data -------------------------------------------------------------

classroom_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/immr04/master/data/classroom.csv")

ggplot(classroom_df,
       aes(x = ses, y = mathscore)
) + geom_point() + 
  facet_wrap(~schoolid) + 
  stat_smooth(method = 'lm', se = F)


M_13 <- lmer(mathscore ~ 1 + ses + (1 + ses|schoolid) + (1 + ses|classid),
             data = classroom_df)

summary(M_13)

M_14 <- lmer(mathscore ~ 1 + ses + (1 + ses|schoolid) + (1 + ses||classid),
             data = classroom_df)

M_15 <- lmer(mathscore ~ 1 + ses + (1 + ses|schoolid) + (1 |classid),
             data = classroom_df)

# does not work in the same way as M_13 does not work
M_16 <- lmer(mathscore ~ 1 + ses + (1 + ses|schoolid/classid2),
             data = classroom_df)

# for now, we will simplify to random intercepts only
# at BOTH schools and classes levels
# M_17 and M_18 and M_19 are all the same
M_17 <- lmer(mathscore ~ 1 + ses + (1|schoolid/classid2),
             data = classroom_df)
M_18 <- lmer(mathscore ~ 1 + ses + (1|schoolid) + (1|classid),
             data = classroom_df)
M_19 <- lmer(mathscore ~ 1 + ses + (1|schoolid) + (1|schoolid:classid2),
             data = classroom_df)

# we can use classid2 to make the M_15 model as follow
M_20 <- lmer(mathscore ~ 1 + ses + (1 + ses|schoolid) + (1 |schoolid:classid2),
             data = classroom_df)

# crossed structures ------------------------------------------------------

blp_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/immr04/master/data/blp-short2.csv")
blp_df %>% filter(spelling == 'herb')
blp_df %>% filter(participant == 18)

M_21 <- lmer(rt ~ 1 + (1|participant) + (1|spelling), data = blp_df)

# group level predictors --------------------------------------------------

mathach_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/immr04/master/data/mathachieve.csv")
mathach_school_df <- read_csv <- read_csv("https://raw.githubusercontent.com/mark-andrews/immr04/master/data/mathachieveschool.csv")

# in principle, just fine, but in practice not so much... convergence errors
M_22 <- lmer(mathach ~ ses + sex + minority + (ses|school) + (sex|school) + (minority|school),
             REML = F,
             data = mathach_df)

M_23 <- lmer(mathach ~ ses + sex + minority + (1 + ses|school), 
             REML = F,
             data = mathach_df)

mathach_join_df <- inner_join(mathach_df, mathach_school_df, by = 'school')

M_24 <- lmer(mathach ~ ses + himinty + (1 + ses|school), 
             REML = F,
             data = mathach_join_df)

M_25 <- lmer(mathach ~ ses + himinty + (1 + ses|school) + (himinty|school), 
             REML = F,
             data = mathach_join_df)

# does not work ...
# M_26 <- lmer(mathach ~ ses + pracad + (ses|school) + (pracad|school)) 
             

# Bayesian methods --------------------------------------------------------

library(brms)

M_27 <- lm(Reaction ~ Days, data = sleepstudy)  # linear regression
M_28 <- brm(Reaction ~ Days, data = sleepstudy) # Bayesian linear regression

summary(M_27)$coefficients
M_28

# Bayesian mixed effects model

M_29 <- brm(Reaction ~ Days + (Days|Subject), data = sleepstudy)
M_8 <- lmer(Reaction ~ Days + (Days|Subject), data = sleepstudy)

M_30 <- brm(Reaction ~ Days + (Days|Subject), 
            data = sleepstudy)

M_31 <- brm(Reaction ~ Days + (Days||Subject), 
            data = sleepstudy)

waic_30 <- waic(M_30)
waic_31 <- waic(M_31)
loo_compare(waic_30, waic_31)
