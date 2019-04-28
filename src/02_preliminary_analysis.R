########################
# preliminary analysis
# RR2 project
# author: sebastian daza
########################

# library
library(brms)
library(data.table)
library(texreg)

source('src/utils.R')

# read data
df = readRDS('output/data_cohort_1.rds')

# results analysis
m1 = lm(post_score ~ treatment + pre_score, data = df)
m2 = lm(post_score ~ any_session + pre_score, data = df)
m3 = lm(post_score ~ sessions_11 + pre_score, data = df)
m4 = lm(post_score ~ treatment * nsessions + pre_score, data = df)

screenreg(list(m1, m2, m3, m4))

df[, diff := post_score - pre_score]
df[, mean(diff, na.rm=TRUE), treatment]

df[pre_post == 1, mean(pre_score, na.rm = TRUE), treatment]
df[pre_post == 1, mean(post_score, na.rm = TRUE), treatment]

# instrumental variable
f1 <- bf(any_session ~ treatment + treatment:pre_score, family('bernoulli'))
f2 <- bf(post_score ~ any_session + pre_score)
IV_brm <- brm(f1 + f2, data = df, cores = 4)

summary(IV_brm)

# examining autocorrelation of scores
df[, zpost_score := scale(post_score)]
df[, zpre_score := scale(pre_score)]

m5 = brm(zpost_score ~ (1|place) + (1|room), data = df, control = list(adapt_delta = .95))
m6 = brm(zpre_score ~ (1|place) + (1|room), data = df, control = list(adapt_delta = .95))

screenreg(list(m5, m6))
