#beta binomial models with brms (-> read abundance modelling)

setwd("C:/Users/coren/OneDrive/Bureau/Internship Vogelwarte/Data/R/data/glmms/non_binomial")
#OR


#packages:
library(brms)
library(bayesplot)
library(extraDistr)
library(ggplot2)
library(tidybayes)
library(dplyr)
#data input ----
df=read.csv("df_models.csv")
df=df[,-c(1,2)]

long=read.csv("long_new.csv")
long=long[,-c(1,2)]

df$prop<-df$anthropo_ra/df$total_ra

#first very simple model ----
mod<-brm(anthropo_ra | trials(total_ra)~sex_genetics, data=df, family = beta_binomial() )

#checking:
pp_check(mod) #model fit

conditional_effects(mod) #partial regressions

mcmc_trace(mod) #seems like its good

summary(mod)$fixed

summary(mod)$random

pp_check(mod, type = "bars") #model underestimate zeros and high counts

pp_check(mod, type = "stat", stat = "mean") #very nice

pp_check(mod, type = "stat", stat = "sd") #the model is slightly over-dispersed (may need a dispersion formula)

prop_zero <- function(x) mean(x == 0)

pp_check(mod, type = "stat", stat = "prop_zero") #model underestimate zeros

bf(
  anthropo_ra | trials(total_ra) ~ sex_genetics + cover_snow,
  phi ~ sex_genetics
)

summary(mod)$spec_pars # strong overdispersion ; expected
loo_mod <- loo(mod) #good, stable and reliable
plot(loo_mod)
bayes_R2(mod) # normal.

#mod2 ; cover_snow and sex_genetics ----
str(df$sex_genetics)
df$sex_genetics=as.factor(df$sex_genetics)
mod2 <- brm(
  anthropo_ra | trials(total_ra) ~ sex_genetics + cover_snow,
  data = df,
  family = beta_binomial()
)

#checking:
pp_check(mod2) #better
conditional_effects(
  mod2,
  effects = "cover_snow",
  conditions = list(sex_genetics = levels(df$sex_genetics))
) #partial regressions
levels(df$sex_genetics)

mcmc_trace(mod2) #
summary(mod)$fixed

summary(mod)$random

pp_check(mod, type = "bars") #

pp_check(mod, type = "stat", stat = "mean") #

pp_check(mod, type = "stat", stat = "sd") #

prop_zero <- function(x) mean(x == 0)

pp_check(mod, type = "stat", stat = "prop_zero") #

bf(
  anthropo_ra | trials(total_ra) ~ sex_genetics + cover_snow,
  phi ~ sex_genetics
)

summary(mod)$spec_pars # 
loo_mod <- loo(mod) #
plot(loo_mod)
bayes_R2(mod) # 

#mod3, interaction ----
mod3 <- brm(
  anthropo_ra | trials(total_ra) ~ sex_genetics * cover_snow,
  data = df,
  family = beta_binomial()
)
#checking:
pp_check(mod3) #better
conditional_effects(mod3, effects = "cover_snow:sex_genetics")
levels(df$sex_genetics)

mcmc_trace(mod3) #
summary(mod3)$fixed

summary(mod3)$random

pp_check(mod3, type = "bars") #

pp_check(mod3, type = "stat", stat = "mean") #

pp_check(mod3, type = "stat", stat = "sd") #

prop_zero <- function(x) mean(x == 0)

pp_check(mod3, type = "stat", stat = "prop_zero") #

bf(
  anthropo_ra | trials(total_ra) ~ sex_genetics * cover_snow,
  phi ~ sex_genetics
)

summary(mod3)$spec_pars # 
loo_mod <- loo(mod3) #
plot(loo_mod)
bayes_R2(mod3) 

#mod2 BIS ; cover_snow and sex_genetics ----
str(df$sex_genetics)
df$sex_genetics=as.factor(df$sex_genetics)
mod2b <- brm(
  anthropo_ra | trials(total_ra) ~ sex_genetics + cover_snow + (1 | month),
  data = df,
  family = beta_binomial(),
  control = list(adapt_delta = 0.99, max_treedepth = 15)
)
#increasing adapt delta??

#checking:
pp_check(mod2b) #better
conditional_effects(
  mod2b,
  effects = "cover_snow",
  conditions = list(sex_genetics = levels(df$sex_genetics))
) #partial regressions
levels(df$sex_genetics)

mcmc_trace(mod2b) #
summary(mod2b)$fixed

summary(mod2b)$random

pp_check(mod2b, type = "bars") #

pp_check(mod2b, type = "stat", stat = "mean") #

pp_check(mod2b, type = "stat", stat = "sd") #

prop_zero <- function(x) mean(x == 0)

pp_check(mod2b, type = "stat", stat = "prop_zero") #

bf(
  anthropo_ra | trials(total_ra) ~ sex_genetics + cover_snow + (1|month),
  phi ~ sex_genetics
)

summary(mod2b)$spec_pars # 
loo_mod <- loo(mod2b) #
plot(loo_mod)
bayes_R2(mod2b) # 

#mod3, interaction ----
mod3b <- brm(
  anthropo_ra | trials(total_ra) ~ sex_genetics * cover_snow + (1|month),
  data = df,
  family = beta_binomial()
)
#checking:
pp_check(mod3b) #better
conditional_effects(mod3b, effects = "cover_snow:sex_genetics")
levels(df$sex_genetics)

mcmc_trace(mod3b) #
summary(mod3b)$fixed

summary(mod3b)$random

pp_check(mod3, type = "bars") #

pp_check(mod3, type = "stat", stat = "mean") #

pp_check(mod3, type = "stat", stat = "sd") #

prop_zero <- function(x) mean(x == 0)

pp_check(mod3, type = "stat", stat = "prop_zero") #

bf(
  anthropo_ra | trials(total_ra) ~ sex_genetics * cover_snow,
  phi ~ sex_genetics
)

summary(mod3)$spec_pars # 
loo_mod <- loo(mod3) #
plot(loo_mod)
bayes_R2(mod3) 

#USING SITE as random effect. ----
df$sex_genetics <- factor(df$sex_genetics)
levels(df$sex_genetics)
mod2c <- brm(
  anthropo_ra | trials(total_ra) ~ sex_genetics + cover_snow + (1 | site),
  data = df,
  family = beta_binomial(),
  control = list(adapt_delta = 0.99, max_treedepth = 15) 
)

#increasing adapt delta -> go slower and explore more (default is 0.8 and 10)

#checking:
pp_check(mod2c) #better
conditional_effects(
  mod2c,
  effects = "cover_snow",
  conditions = list(sex_genetics = levels(df$sex_genetics))
) #partial regressions


mcmc_trace(mod2c) #
summary(mod2c)$fixed

summary(mod2c)$random

pp_check(mod2c, type = "bars") #

pp_check(mod2c, type = "stat", stat = "mean") #

pp_check(mod2c, type = "stat", stat = "sd") #

prop_zero <- function(x) mean(x == 0)

pp_check(mod2c, type = "stat", stat = "prop_zero") #

bf(
  anthropo_ra | trials(total_ra) ~ sex_genetics + cover_snow + (1|site),
  phi ~ sex_genetics
)

summary(mod2c)$spec_pars # 
loo_mod <- loo(mod2c) #
plot(loo_mod)
bayes_R2(mod2c) # 

# 1. Prediction grid
newdat <- tidyr::expand_grid(
  cover_snow = seq(min(df$cover_snow), max(df$cover_snow), length.out = 200),
  sex_genetics = levels(df$sex_genetics)
) %>%
  mutate(
    total_ra = 1,   # required for trials()
    site = NA       # ignored because re_formula = NA
  )

# 2. Posterior fitted values

pred <- add_epred_draws(
  mod2c,
  newdata = newdat,
  re_formula = NA
)

# 3. Summaries (median + 95% CI)
pred_sum <- pred %>%
  median_qi(.epred, .width = 0.95)

# 4. Observed proportions
df$obs_prop <- df$anthropo_ra / df$total_ra

# 5. Plot
df$obs_prop <- df$anthropo_ra / df$total_ra

ggplot() +
  geom_point(
    data = df,
    aes(x = cover_snow, y = obs_prop, color = sex_genetics),
    alpha = 0.4
  ) +
  geom_ribbon(
    data = pred_sum,
    aes(x = cover_snow, ymin = .lower, ymax = .upper, fill = sex_genetics),
    alpha = 0.25
  ) +
  geom_line(
    data = pred_sum,
    aes(x = cover_snow, y = .epred, color = sex_genetics),
    size = 1.2
  ) +
  scale_color_manual(values = c("f" = "#f9ada7", "m" = "#66d9dc")) +
  scale_fill_manual(values = c("f" = "#f9ada7", "m" = "#66d9dc")) +
  labs(
    x = "Snow cover",
    y = "Proportion of anthropogenic reads",
    color = "Sex",
    fill = "Sex",
    title = "anthropo_ra | trials(total_ra) ~ sex_genetics + cover_snow + ()"
  ) +
  theme_bw()

#mod3, interaction ----
mod3c <- brm(
  anthropo_ra | trials(total_ra) ~ sex_genetics * cover_snow + (1|site),
  data = df,
  family = beta_binomial()
)
#checking:
pp_check(mod3c) #better
conditional_effects(mod3c, effects = "cover_snow:sex_genetics")
levels(df$sex_genetics)

mcmc_trace(mod3c) #
summary(mod3c)$fixed

summary(mod3c)$random

pp_check(mod3c, type = "bars") #

pp_check(mod3c, type = "stat", stat = "mean") #

pp_check(mod3c, type = "stat", stat = "sd") #

prop_zero <- function(x) mean(x == 0)

pp_check(mod3c, type = "stat", stat = "prop_zero") #

bf(
  anthropo_ra | trials(total_ra) ~ sex_genetics * cover_snow + (1|site),
  phi ~ sex_genetics
)

summary(mod3c)$spec_pars # 
loo_mod <- loo(mod3c) #
plot(loo_mod)
bayes_R2(mod3c) 
sum(df$total_ra == 0)

#plotting mod3c with observations points
ce <- conditional_effects(mod3c, effects = "cover_snow:sex_genetics")
df_ce <- ce$`cover_snow:sex_genetics`
df$prop_obs <- df$anthropo_ra / df$total_ra

ggplot() +
  # raw observed points
  geom_point(
    data = df,
    aes(x = cover_snow, y = prop_obs, color = sex_genetics),
    alpha = 0.4, size = 2
  ) +
  
  # prediction ribbons
  geom_ribbon(
    data = df_ce,
    aes(x = cover_snow, ymin = lower__, ymax = upper__, fill = sex_genetics),
    alpha = 0.25, color = NA
  ) +
  
  # prediction lines
  geom_line(
    data = df_ce,
    aes(x = cover_snow, y = estimate__, color = sex_genetics),
    size = 1.2
  ) +
  
  scale_color_manual(values = c("f" = "#f9ada7", "m" = "#66d9dc")) +
  scale_fill_manual(values = c("f" = "#f9ada7", "m" = "#66d9dc")) +
  
  labs(
    x = "Snow cover",
    y = "Proportion of anthropogenic reads",
    color = "Sex",
    fill = "Sex",
    title = "anthropo_ra | trials(total_ra) ~ sex_genetics * cover_snow + (1|site)"
  ) +
  
  theme_classic(base_size = 10)

#with other weather variables ----
#the idea here is to compare all of these snow variables, to see with which it is best to model.

df$snow_height #this one maybe but difficult to say for missing values what they were (expect with weather variables)
df$fresh_snow #no not related to fresh snow!
df$precipitation
df$precipitation <- ifelse(is.na(df$precipitation), "no", df$precipitation)


#so comparing only snowcover and precipitation?
df$precipitation=as.factor(df$precipitation)
df$precipitation
m_precip <- brm(
  anthropo_ra | trials(total_ra) ~ sex_genetics + precipitation + (1 | site_month),
  data = df,
  family = beta_binomial()
)

#checking:
pp_check(m_precip) #better
conditional_effects(m_precip)
levels(df$sex_genetics)

mcmc_trace(m_precip) #
summary(m_precip)$fixed

summary(m_precip)$random

pp_check(m_precip, type = "bars") #

pp_check(m_precip, type = "stat", stat = "mean") #

pp_check(m_precip, type = "stat", stat = "sd") #

prop_zero <- function(x) mean(x == 0)

pp_check(m_precip, type = "stat", stat = "prop_zero") #

bf(
  anthropo_ra | trials(total_ra) ~ sex_genetics + precipitation + (1|site),
  phi ~ sex_genetics
)

summary(m_precip)$spec_pars # 
loo_mod <- loo(m_precip) #
plot(loo_mod)
bayes_R2(m_precip) 
sum(df$total_ra == 0)

#plotting m_precip with observations points
df$obs_prop <- df$anthropo_ra / df$total_ra
post_draws <- df %>%
  add_predicted_draws(mod1, re_formula = NA) %>%
  mutate(pred_prop = .prediction / total_ra)
post_draws$group <- interaction(
  post_draws$precipitation,
  post_draws$sex_genetics
)# Create combined factor

post_group <- post_draws %>%
  group_by(group, sex_genetics, precipitation, .draw) %>%
  summarise(pred = mean(pred_prop), .groups = "drop")

post_group$group <- interaction(post_group$precipitation, post_group$sex_genetics)

df$group <- interaction(df$precipitation, df$sex_genetics)

# Set desired order
desired_order <- c("no.f", "no.m", "snow.f", "snow.m")

post_group$group <- factor(post_group$group, levels = desired_order)
df$group <- factor(df$group, levels = desired_order)

library(ggplot2)

ggplot() +
  # observed data BEHIND
  geom_point(
    data = df,
    aes(x = group, y = obs_prop),
    position = position_jitter(width = 0.15),
    alpha = 0.25,
    color = "grey40"
  ) +
  # posterior predictive boxplots ON TOP
  geom_boxplot(
    data = post_group,
    aes(x = group, y = pred, fill = sex_genetics),
    alpha = 0.7,
    width = 0.6,
    outlier.shape = NA
  ) +
  scale_fill_manual(values = c("f" = "#f9ada7", "m" = "#66d9dc")) +
  labs(
    x = "Precipitation × Sex",
    y = "Proportion anthropogenic",
    title = "anthropo_ra | trials(total_ra) ~ sex_genetics + precipitation + (1|site_month)"
  ) +
  theme_bw()

## mod4 with fresh_snow ----
df$sex_genetics <- factor(df$sex_genetics)
levels(df$sex_genetics)
mod4 <- brm(
  anthropo_ra | trials(total_ra) ~ sex_genetics + fresh_snow + (1 | site),
  data = df,
  family = beta_binomial(),
  control = list(adapt_delta = 0.99, max_treedepth = 15) 
)


#checking:
pp_check(mod4) #better
conditional_effects(
  mod4,
  effects = "fresh_snow",
  conditions = list(sex_genetics = levels(df$sex_genetics))
) #partial regressions


mcmc_trace(mod4) #
summary(mod4)$fixed

summary(mod4)$random

pp_check(mod4, type = "bars") #

pp_check(mod4, type = "stat", stat = "mean") #

pp_check(mod4, type = "stat", stat = "sd") #

prop_zero <- function(x) mean(x == 0)

pp_check(mod4, type = "stat", stat = "prop_zero") #

bf(
  anthropo_ra | trials(total_ra) ~ sex_genetics + fresh_snow + (1|site),
  phi ~ sex_genetics
)

summary(mod4)$spec_pars # 
loo_mod <- loo(mod4) #
plot(loo_mod)
bayes_R2(mod4) # 

# 1. Prediction grid
newdat <- tidyr::expand_grid(
  fresh_snow = seq(min(df$fresh_snow), max(df$fresh_snow), length.out = 200),
  sex_genetics = levels(df$sex_genetics)
) %>%
  mutate(
    total_ra = 1,   # required for trials()
    site = NA       # ignored because re_formula = NA
  )

# 2. Posterior fitted values

pred <- add_epred_draws(
  mod4,
  newdata = newdat,
  re_formula = NA
)

# 3. Summaries (median + 95% CI)
pred_sum <- pred %>%
  median_qi(.epred, .width = 0.95)

# 4. Observed proportions
df$obs_prop <- df$anthropo_ra / df$total_ra

# 5. Plot
df$obs_prop <- df$anthropo_ra / df$total_ra

ggplot() +
  geom_point(
    data = df,
    aes(x = fresh_snow, y = obs_prop, color = sex_genetics),
    alpha = 0.4
  ) +
  geom_ribbon(
    data = pred_sum,
    aes(x = fresh_snow, ymin = .lower, ymax = .upper, fill = sex_genetics),
    alpha = 0.25
  ) +
  geom_line(
    data = pred_sum,
    aes(x = fresh_snow, y = .epred, color = sex_genetics),
    size = 1.2
  ) +
  scale_color_manual(values = c("f" = "#f9ada7", "m" = "#66d9dc")) +
  scale_fill_manual(values = c("f" = "#f9ada7", "m" = "#66d9dc")) +
  labs(
    x = "Fresh snow",
    y = "Proportion of anthropogenic reads",
    color = "Sex",
    fill = "Sex",
    title = "anthropo_ra | trials(total_ra) ~ sex_genetics + fresh_snow + (1|site)"
  ) +
  theme_bw()

#mod 5

df$nrecapture
df$recap_num

#site specific
mod <- brm(
  anthropo_ra | trials(total_ra) ~ site,
  data   = df,
  family = beta_binomial()
)

pp_check(mod)                     # general fit
conditional_effects(mod)          # partial effects
mcmc_trace(mod)                   # chain mixing
summary(mod)$fixed                # fixed effects
summary(mod)$random               # none here
pp_check(mod, type = "bars")      # zeros & high counts
pp_check(mod, type = "stat", stat = "mean")
pp_check(mod, type = "stat", stat = "sd")

prop_zero <- function(x) mean(x == 0)
pp_check(mod, type = "stat", stat = "prop_zero")

loo_mod <- loo(mod)
plot(loo_mod)

bR2 <- bayes_R2(mod)
bR2

df$site <- factor(df$site)

newdat <- data.frame(
  site = levels(df$site),
  total_ra = round(median(df$total_ra))   # required but irrelevant for p
)

# get linear predictor (logit scale)
pred_lin <- fitted(
  mod,
  newdata = newdat,
  scale = "linear",     # <-- THIS is the key
  summary = TRUE,
  probs = c(0.025, 0.975)
)

# convert to proportions
inv_logit <- function(x) plogis(x)

newdat$fit <- inv_logit(pred_lin[,"Estimate"])
newdat$lwr <- inv_logit(pred_lin[,"Q2.5"])
newdat$upr <- inv_logit(pred_lin[,"Q97.5"])
ggplot(newdat, aes(x = site, y = fit)) +
  
  geom_errorbar(
    aes(ymin = lwr, ymax = upr),
    width = 0.15,
    size  = 1.2,
    colour = "grey20"
  ) +
  
  geom_point(
    size  = 4,
    colour = "cyan4"
  ) +
  
  labs(
    x = "Site",
    y = "Posterior anthropogenic proportion",
    title = "Posterior mean ± 95% credible interval per site\n(beta-binomial brms model)"
  ) +
  
  theme_bw(base_size = 12)

df$obs <- df$anthropo_ra / df$total_ra
summary(df$obs)
range(df$obs)
newdat <- data.frame(
  site = levels(df$site),
  total_ra = round(median(df$total_ra))   # required but irrelevant for p
)

# get linear predictor (logit scale)
pred_lin <- fitted(
  mod,
  newdata = newdat,
  scale = "linear",     # gives logit(p)
  summary = TRUE,
  probs = c(0.025, 0.975)
)

# convert to proportions
newdat$fit <- plogis(pred_lin[,"Estimate"])
newdat$lwr <- plogis(pred_lin[,"Q2.5"])
newdat$upr <- plogis(pred_lin[,"Q97.5"])
ggplot() +
  
  # observed proportions (correct)
  geom_jitter(
    data = df,
    aes(x = site, y = obs),
    width = 0.15,
    alpha = 0.25,
    size  = 1.8,
    colour = "grey60"
  ) +
  
  # posterior 95% CrI
  geom_errorbar(
    data = newdat,
    aes(x = site, ymin = lwr, ymax = upr),
    width = 0.15,
    size  = 1,
    colour = "grey"
  ) +
  
  # posterior mean point
  geom_point(
    data = newdat,
    aes(x = site, y = fit),
    size  = 4,
    colour = "cyan4"
  ) +
  
  labs(
    x = "Site",
    y = "Anthropogenic proportion",
    title = "brm(anthropo_ra | trials(total_ra) ~ site"
  ) +
  
  theme_bw(base_size = 12) +
  theme(
    panel.grid.minor = element_blank()
  )
