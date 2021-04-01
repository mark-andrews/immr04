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
