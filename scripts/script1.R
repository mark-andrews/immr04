library(tidyverse)

# Get rats data -----------------------------------------------------------

rats_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/immr04/master/data/rats.csv")
rats_df <- mutate(rats_df, batch = factor(batch))

# Look at batch 42 --------------------------------------------------------

rats_df_42 <- filter(rats_df, batch == "42")

M_1 <- glm(cbind(m, n-m) ~ 1, data = rats_df_42, family = binomial)
# estimated value of beta, i.e. the log odds of a tumour in batch 42
coef(M_1)
# estimated value of theta, i.e. the probability of a tumour in batch 42
plogis(coef(M_1))
# 95% confidence interval on the probability of a tumour in batch 42
plogis(confint.default(M_1))


# A single model of all batches -------------------------------------------

M_2 <- glm(cbind(m, n - m) ~ 0 + batch, data = rats_df, family = binomial) 
round(plogis(coef(M_2)), 2)



# Multilevel version of the multi batch model -----------------------------
library(lme4)
M_3 <- glmer(cbind(m, n-m) ~ 1 + (1|batch),
             data = rats_df,
             family = binomial)

summary(M_3)
fixef(M_3) + c(-1, 1) * 1.96 * 0.66
plogis(fixef(M_3) + c(-1, 1) * 1.96 * 0.66)

# random effect, or random differences, from the average log odds
ranef(M_3)

coef(M_3)

# normal random effects model ---------------------------------------------

alcohol_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/immr04/master/data/alcohol.csv")

M_4 <- lmer(alcohol ~ 1 + (1|country), data = alcohol_df)
summary(M_4)
# ranef gives the "zeta" values
ranef(M_4)

# the mu's themselves are from phi + zeta or coef(M_4)
# and phi is from fixef(M_4)
fixef(M_4)
coef(M_4)

# Intraclass correlation
vars <- (VarCorr(M_4) %>% as.data.frame())[,'vcov']
vars[1]/sum(vars)

# Linear mixed effects models ---------------------------------------------

head(sleepstudy)
ggplot(sleepstudy, 
       aes(x = Days, y = Reaction)
) + geom_point() + facet_wrap(~Subject)

ggplot(sleepstudy, 
       aes(x = Days, y = Reaction)
) + geom_point() + 
  facet_wrap(~Subject) + 
  stat_smooth(method = 'lm', se = F)
