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
m1 = lm(post_total ~ treatment + pre_total, data = df)
m2 = lm(post_total ~ any_session + pre_total, data = df)
m3 = lm(post_total ~ sessions_11 + pre_total, data = df)
m4 = lm(post_total ~ treatment * nsessions + pre_total, data = df)

screenreg(list(m1, m2, m3, m4))

df[, .(id, pre_total, post_total)]

df[, diff := post_total - pre_total]
df[, .(N = sum(!is.na(diff)), diff = mean(diff, na.rm=TRUE)), treatment]



# instrumental variable
f1 <- bf(any_session ~ treatment + treatment:pre_score, family('bernoulli'))
f2 <- bf(post_score ~ any_session + pre_score)
IV_brm <- brm(f1 + f2, data = df, cores = 4)

summary(IV_brm)

# clustering
m5 = brm(post_score ~ treatment + pre_score + (1|place) + (1|room),
         data = df)

# examining autocorrelation of scores
df[, zpost_score := scale(post_score)]
df[, zpre_score := scale(pre_score)]

m5 = brm(zpost_score ~ (1|place) + (1|room), data = df, control = list(adapt_delta = .95))
m6 = brm(zpre_score ~ (1|place) + (1|room), data = df, control = list(adapt_delta = .95))

screenreg(list(m5, m6))
